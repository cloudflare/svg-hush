//! Filters SVG files to make them safe to host anywhere.
//!
//! ```rust
//! use svg_hush::*;
//! let mut file = "<svg xmlns='http://www.w3.org/2000/svg' />".as_bytes();
//! let mut filter = Filter::new();
//! filter.set_data_url_filter(data_url_filter::allow_standard_images);
//! let mut out = Vec::new();
//! filter.filter(&mut file, &mut out)?;
//! # Ok::<_, FError>(())
//! ```

use crate::data_url_filter::DataUrl;
use crate::data_url_filter::DataUrlFilterResult;
use once_cell::sync::Lazy;
use quick_error::quick_error;
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::io;
use xml::attribute::{Attribute, OwnedAttribute};
use xml::EmitterConfig;
use xml::name::Name;
use xml::name::OwnedName;
use xml::ParserConfig;
use xml::reader::XmlEvent as REvent;
use xml::writer::XmlEvent as WEvent;


quick_error! {
    /// XML parsing error, I/O error, etc.
    #[derive(Debug)]
    pub enum FError {
        Reader(err: xml::reader::Error) {
            from()
            display("XML parsing error")
            source(err)
        }
        Writer(err: xml::writer::Error) {
            from()
            display("XML encoding error")
            source(err)
        }
    }
}

mod attrs;

/// Optionally, you can allow or modify `data:` URLs.
///
/// Please be careful with this, because these URLs can carry any file type.
pub mod data_url_filter;

enum Attr<'a> {
    /// Left unchanged
    Keep(Attribute<'a>),
    /// New attribute value
    Rewrite(OwnedAttribute),
    /// Attr is not allowed and must be discarded
    Drop,
}

enum ElementAction {
    Keep,
    FilterCSS,
    Drop,
}

#[derive(Copy, Clone)]
pub(crate) enum AttrType {
    /// Numbers, lengths, lists of numbers or keywords, and other non-URL non-script things
    AnyAscii,
    /// A single ASCII word
    Keyword,
    /// A floating-point number
    Number,
    /// Allows non-ASCII
    Text,
    /// Name of another attribute
    AttributeName,
    /// May be a URL (keywords are left alone, they're relative URLs at worst)
    Url,
    /// May contain "url(#)"
    UrlFunc,
    /// All kinds of dangers
    StyleSheet,
}

static ALLOWED_SVG_ELEMENTS: Lazy<HashSet<&'static str>> = Lazy::new(|| [
    "a", // but href is checked separately
    "altGlyph",
    "altGlyphDef",
    "altGlyphItem",
    "animate",
    "animateColor",
    "animateMotion",
    "animateTransform",
    "circle",
    "clipPath",
    "color-profile",
    "defs",
    "desc",
    "ellipse",
    "feBlend",
    "feColorMatrix",
    "feComponentTransfer",
    "feComposite",
    "feConvolveMatrix",
    "feDiffuseLighting",
    "feDisplacementMap",
    "feDistantLight",
    "feDropShadow",
    "feFlood",
    "feFuncA",
    "feFuncB",
    "feFuncG",
    "feFuncR",
    "feGaussianBlur",
    "feImage",
    "feMerge",
    "feMergeNode",
    "feMorphology",
    "feOffset",
    "fePointLight",
    "feSpecularLighting",
    "feSpotLight",
    "feTile",
    "feTurbulence",
    "filter",
    "flowDiv",
    "flowLine",
    "flowPara",
    "flowRegion",
    "flowRoot",
    "flowSpan",
    "flowTref",
    "font",
    "g",
    "glyph",
    "glyphRef",
    "hkern",
    "image",
    "line",
    "linearGradient",
    "marker",
    "mask",
    "meshgradient",
    "meshpatch",
    "meshrow",
    "mpath",
    "mPath", // not sure about the case, dtd and tests disagre
    "path",
    "pattern",
    "polygon",
    "polyline",
    "radialGradient",
    "rect",
    "set",
    "solidColor",
    "stop",
    "svg",
    "switch",
    "symbol",
    "text",
    "textPath",
    "title",
    "tref",
    "tspan",
    "use",
    "view",
    "vkern",
].into());

static ATTRIBUTE_TYPES: Lazy<HashMap<&'static str, AttrType>> = Lazy::new(|| {
    let attribute_types: HashMap<_, _> = attrs::ATTRS.iter().copied().collect();
    assert_eq!(attribute_types.len(), attrs::ATTRS.len());
    attribute_types
});

/// The main entry point. Call [`Filter::new`]
pub struct Filter {
    /// May improve compression
    sort_attributes: bool,
    /// We're not allowing non-SVG namespaces anyway
    strip_prefixes: bool,
    image_filter: Option<Box<dyn for<'a> Fn(&'a DataUrl<'a>) -> DataUrlFilterResult>>,
}

impl Filter {
    /// Create new filter instance. Call [`Filter::filter`] on it.
    pub fn new() -> Self {
        Self {
            sort_attributes: true,
            strip_prefixes: true,
            image_filter: None,
        }
    }

    fn is_allowed_element(&self, name: Name) -> ElementAction {
        if name.namespace == Some("http://www.w3.org/2000/svg") {
            if name.local_name == "style" {
                return ElementAction::FilterCSS;
            }
            if ALLOWED_SVG_ELEMENTS.get(&name.local_name).is_some() {
                return ElementAction::Keep;
            }
        }
        return ElementAction::Drop;
    }

    /// Read an SVG image from the `source` and write a filtered image to the `destination`.
    pub fn filter<Read: io::Read, Write: io::Write>(&self, source: Read, destination: Write) -> Result<(), FError> {
        let parser = ParserConfig::new()
            .cdata_to_characters(true)
            .ignore_comments(true)
            .coalesce_characters(false)
            .create_reader(source);

        let mut writer = EmitterConfig::new()
            .cdata_to_characters(true)
            .keep_element_names_stack(false)
            .autopad_comments(false)
            .perform_indent(true)
            .pad_self_closing(false)
            .create_writer(destination);

        let mut skipping = 0;
        let mut accumulating_css_text: Option<String> = None;
        let mut rewrite = Vec::new();
        let mut reached_document_end = false;
        let mut emitted_any_element = false;
        for e in parser {
            let mut e = e?;
            let w = match &mut e {
                REvent::StartElement {
                    name,
                    attributes,
                    namespace,
                } => {
                    if skipping > 0 {
                        skipping += 1;
                        continue;
                    }

                    match self.is_allowed_element(name.borrow()) {
                        ElementAction::Keep => {},
                        ElementAction::FilterCSS if accumulating_css_text.is_none() => {
                            accumulating_css_text = Some(String::new());
                        },
                        _ => {
                            skipping += 1;
                            continue;
                        }
                    }

                    let mut keep = Vec::with_capacity(attributes.len());
                    rewrite.clear();

                    // xlink to href conversion could cause a duplicate if the element had both
                    let mut seen_href = false;
                    for a in attributes {
                        if a.name.local_name == "href" {
                            if seen_href {
                                continue;
                            }
                            seen_href = true;
                        }
                        match self.filter_attribute((*a).borrow(), &name.local_name) {
                            Attr::Keep(a) => keep.push(a),
                            Attr::Rewrite(new_a) => rewrite.push(new_a),
                            Attr::Drop => {},
                        }
                    }
                    keep.extend(rewrite.iter().map(|a| a.borrow()));

                    if self.sort_attributes {
                        keep.sort_by(|a, b| a.name.local_name.cmp(&b.name.local_name));
                    }

                    // must be done in the end tag too
                    if self.strip_prefixes {
                        namespace.0.retain(|k, _| k.is_empty());
                        name.prefix = None;
                    }

                    emitted_any_element = true;
                    WEvent::StartElement {
                        name: (*name).borrow(),
                        attributes: keep.into(),
                        namespace: Cow::Borrowed(&*namespace),
                    }
                }
                REvent::EndElement { name } => {
                    if skipping > 0 {
                        skipping -= 1;
                        continue;
                    }
                    if self.strip_prefixes {
                        name.prefix = None;
                    }
                    if let Some(css) = accumulating_css_text.take() {
                        writer.write(WEvent::Characters(&self.filtered_url_func(&css)))?;
                    }
                    WEvent::EndElement {
                        name: Some((*name).borrow()),
                    }
                }
                REvent::StartDocument {
                    version,
                    encoding: _,
                    standalone: _,
                } => {
                    debug_assert_eq!(0, skipping);
                    if skipping != 0 { break; }
                    WEvent::StartDocument {
                        version: *version,
                        encoding: Some("utf-8"),
                        standalone: None,
                    }
                }
                REvent::EndDocument => {
                    debug_assert_eq!(0, skipping);
                    if skipping != 0 { break; }
                    reached_document_end = true;
                    break;
                }

                REvent::ProcessingInstruction { .. } => continue,
                REvent::CData(_) | REvent::Comment(_) => unreachable!(),
                REvent::Characters(c) | REvent::Whitespace(c) => {
                    if skipping > 0 {
                        continue;
                    }
                    if let Some(css) = &mut accumulating_css_text {
                        css.push_str(c);
                        WEvent::Characters("")
                    } else {
                        WEvent::Characters(c)
                    }
                }
            };
            writer.write(w)?;
        }
        if reached_document_end && emitted_any_element {
            Ok(())
        } else {
            return Err(xml::writer::Error::from(io::Error::new(io::ErrorKind::UnexpectedEof, "No acceptable SVG elements found")).into())
        }
    }

    /// This function will be called for every `data:` URL encountered in embedded images.
    ///
    /// This callback is not allowed to refer to its environment. Use `Arc<Mutex<_>>` to modify any shared state.
    pub fn set_data_url_filter(&mut self, filter: impl Fn(&DataUrl) -> DataUrlFilterResult + 'static) {
        self.image_filter = Some(Box::new(filter));
    }

    fn filter_attribute<'a>(&self, attr: Attribute<'a>, element_local_name: &str) -> Attr<'a> {
        // XML is weird in that attributes on a namespaced element are not in the element's namespace!
        // So all SVG attributes are in the default namespace.
        // We only allow the deprecated xlink:href, and only to downgrade it to a plain href.
        if let Some(ns) = attr.name.namespace {
            // TODO: support xml::space=preserve
            if ns != "http://www.w3.org/1999/xlink" || attr.name.local_name != "href" {
                return Attr::Drop;
            }
            debug_assert!(attr.name.prefix.is_some());
        }

        // Linking to same-origin images and gradients is fine (we'll filter the URL)
        if attr.name.local_name == "href" && !Self::may_use_href(element_local_name) {
            return Attr::Drop;
        }

        use AttrType::*;
        let attr_type = match ATTRIBUTE_TYPES.get(&attr.name.local_name).copied() {
            Some(Text) => return Attr::Keep(attr),
            Some(other) => other,
            // DOMPurify passes these through. ARIA is not supposed to have any scripting or remote resources.
            None if attr.name.local_name.starts_with("aria-") => AttrType::AnyAscii,
            None if attr.name.local_name.starts_with("data-") => return Attr::Keep(attr),
            None => return Attr::Drop,
        };

        // From here these are all attributes containing code, so they're supposed to be ASCII and tolerate normalized whitespace.
        // Historically there have been cases of filter bypasses due to browsers silently skipping "invalid" characters,
        // or due to overeager Unicode support treating various fancy whitespace characters, CJK full-width letters, or non-ASCII punctuation as part of the syntax.
        // This strips values to bare ASCII to ensure that what we parse is a simple case most likely to match what the browsers see.
        // The value is already XML-decoded, so entities are not an issue here.
        let trimmed_value = attr.value.trim();
        let attr_value = if trimmed_value.bytes().all(|c| c.is_ascii() && c != 0) {
            Cow::Borrowed(trimmed_value)
        } else {
            Cow::Owned(
                attr.value
                    .chars()
                    .map(|c| if !c.is_whitespace() { c } else { ' ' })
                    .filter(|&c| c.is_ascii() && c != '\0')
                    .collect(),
            )
        };

        /// makes a non-namespaced attribute
        fn no_ns_attr_with_value<'a>(attr: Attribute<'a>, value: Cow<str>) -> Attr<'a> {
            if attr.value == value && attr.name.namespace.is_none() && attr.name.prefix.is_none() {
                Attr::Keep(attr)
            } else {
                Attr::Rewrite(OwnedAttribute {
                    name: OwnedName { local_name: attr.name.local_name.to_owned(), namespace: None, prefix: None },
                    value: value.into_owned(),
                })
            }
        }

        match attr_type {
            // These checks aren't striclty necessary, just an integrity check
            AnyAscii | Keyword | Number | Text => no_ns_attr_with_value(attr, attr_value),
            // this is from deprecated SMIL. Don't let it create a banned attribute by "animating" to it.
            AttributeName if matches!(ATTRIBUTE_TYPES.get(attr.value.trim()), Some(AnyAscii | Keyword | Number | Text))
                => no_ns_attr_with_value(attr, attr_value),

            // Serious filtering starts here
            Url => self.filter_url(&attr_value).map(|val| no_ns_attr_with_value(attr, val.into())).unwrap_or(Attr::Drop),
            // The SVG spec says FuncIRI is a super simple `url(` token, but in browsers it's not. Browsers still support CSS-isms in url(),
            // so we'll just reuse CSS filter for them.
            UrlFunc => no_ns_attr_with_value(attr, self.filtered_url_func(&attr_value).into()),
            StyleSheet => no_ns_attr_with_value(attr, self.filtered_url_func(&attr_value).into()),
            _ => Attr::Drop,
        }
    }

    /// This list must not contain `<a>` and visible elements
    fn may_use_href(element_local_name: &str) -> bool {
        matches!(element_local_name, "radialGradient" | "linearGradient" | "image" | "use" | "pattern" | "feImage")
    }

    fn filter_data_url(&self, url_str: &str) -> Option<String> {
        let f = self.image_filter.as_ref()?;

        let url = DataUrl::process(url_str).ok()?;
        match f(&url) {
            DataUrlFilterResult::Drop => None,
            DataUrlFilterResult::Keep => Some(url_str.to_owned()),
            DataUrlFilterResult::Rewrite { mime_type, data } => {
                let mut out = String::with_capacity(mime_type.len() + data.len()*8/6 + 20);
                out.push_str("data:"); out.push_str(&mime_type); out.push_str(";base64,");
                base64::encode_config_buf(data, base64::Config::new(base64::CharacterSet::Standard, false), &mut out);
                Some(out)
            },
        }
    }

    /// Transforms URLs to relative (same-origin) URLs if possible. The host part is simply thrown away,
    /// since we're not concerned about causing 404s. Files that depend on cross-origin resources can still
    /// be made to work if the resources are copied to the same host as the file.
    fn filter_url(&self, url_str: &str) -> Option<String> {
        // The URL crate won't parse relative URLs directly, and `make_relative()` requires a base too
        let base_url = url::Url::parse("https://127.0.0.1/__relpath_prefix__/").expect("base");
        let url = base_url.join(url_str).ok()?;
        if url.scheme() == "data" {
            return self.filter_data_url(url_str);
        }
        if url.cannot_be_a_base() {
            return None;
        }

        let path = url.path();
        let mut relative = path
            .strip_prefix("/__relpath_prefix__/")
            .unwrap_or(path)
            .to_string();
        if relative.trim_start().starts_with("//") {
            // path became scheme-relative URL
            return None;
        }
        // defence in depth to prevent url being interpreted as having a scheme
        if relative.contains(':') {
            relative = relative.replace(':', "%3a");
        }
        if let Some(query) = url.query() {
            relative.push('?');
            relative.push_str(query);
        }
        // this is super unlikely to be an image
        if relative == "/" {
            return None;
        }
        if let Some(fragment) = url.fragment() {
            relative.push('#');
            relative.push_str(fragment);
        }
        // in case our type data was wrong and Url type was UrlFunc type, break the url() syntax
        if relative.contains('(') {
            relative = relative.replace('(', "%28");
        }
        Some(relative)
    }

    /// ## Why is this parser so hacky?
    ///
    /// Filtering properly would require precisely tokenizing the input,
    /// which depends on supporting syntax of every SVG attribute and CSS.
    /// Any mismatch between actual syntax and the parser would risk these two getting out of sync,
    /// and enabling filter bypass (e.g. if our tokenizer saw a comment but browsers didn't).
    /// Proper implementation needs browser-grade parsers, and that's a big ask for a small tool.
    ///
    /// So here's a brute approach instead: throwing out everything that isn't obviously trivial.
    /// We don't really need to support SVG files that rely on weird tricky syntax. We can mangle all
    /// the edge cases as long as we're erring on the side of safety.
    fn filtered_url_func(&self, css_or_svg: &str) -> String {
        // If we see something suspicious we'll try to remove it instead of throwing away the whole stylesheet/attribute.
        css_or_svg.split_inclusive([';', '{', '}', ','])
            .filter_map(|full_chunk| {
                // CSS allows escapes in identifiers! No escape schenanigans allowed, as this makes matching require a proper tokenizer.
                if full_chunk.contains('\\') {
                    // TODO: could perform unescaping instead
                    return None;
                }

                // without escapes it's reasonable to search for identifiers now (lowercase to make them case-insensitive)
                let chunk_lower = full_chunk.to_ascii_lowercase();
                // @rules don't allow extra whitespace, so apart from escapes, they can't be obfuscated
                if chunk_lower.contains("@import") {
                    return None;
                }

                // `url()` doesn't allow whitespace before '('
                if chunk_lower.contains("url(") {
                    let mut out = String::with_capacity(chunk_lower.len());
                    // this is like split(), but with the match done on lowercased string for case-insensitivity. Indices are used to get original case back.
                    // this works thanks to to_ascii_lowercase preserving offsets.
                    let mut last_idx = 0;
                    let mut url_chunks = chunk_lower.match_indices("url(").chain(Some((full_chunk.len(), "")))
                        .map(|(idx, frag)| {
                            let s = &full_chunk[last_idx..idx];
                            last_idx = idx+frag.len();
                            s
                        });
                    out.push_str(url_chunks.next().expect("first")); // text before `url(`
                    while let Some(url_chunk) = url_chunks.next() {
                        // Too bad if you use unescaped () in your URL
                        let mut urlfunc_parts = url_chunk.split(')');
                        let urlfunc = urlfunc_parts.next().expect("first");
                        let after_urlfunc = urlfunc_parts.next()?; // if it's none, it's `url(url(`, which we don't want
                        let url = urlfunc.trim()
                            // we've already established there are no escape chars in the chunk,
                            // so there can't be any tricky things inside the string.
                            // Sorry to everyone who uses unescaped quote chars at the end of their URLs.
                            .trim_start_matches(['"', '\'']).trim_end_matches(['"', '\''])
                            // the quoted value allows whitespace too!
                            .trim();
                        let url = self.filter_url(url)
                            // no tricky chars please. For SVG it's important to keep url() even if the URL inside it is bogus
                            // because otherwise properties like fill would default to black instead of transparent
                            .filter(|url| !url.contains(['(',')','\'','"','\\']));
                        let url = url.as_deref().unwrap_or("#");
                        out.push_str("url(");
                        out.push_str(url);
                        out.push(')');
                        out.push_str(after_urlfunc); // it's important to keep the terminator
                    }
                    return Some(Cow::Owned(out));
                }

                Some(Cow::Borrowed(full_chunk))
            })
            .collect()
    }
}

#[test]
fn urlfunc() {
    let f = Filter::new();
    assert_eq!(f.filtered_url_func("hello, url(world)"), "hello, url(world)");
    assert_eq!(f.filtered_url_func("hello, world"), "hello, world");
    assert_eq!(f.filtered_url_func("hello, url(http://evil.com)"), "hello, url(#)");
    assert_eq!(f.filtered_url_func("hello, url( http://evil.com )"), "hello, url(#)");
    assert_eq!(f.filtered_url_func("hello, url( //evil.com )"), "hello, url(#)");
    assert_eq!(f.filtered_url_func("url( /okay ), (), bye"), "url(/okay), (), bye");
    assert_eq!(f.filtered_url_func("hello, url( unclosed"), "hello,");
    assert_eq!(f.filtered_url_func("hello, url( ) url(    ) bork"), "hello, url() url() bork");
    assert_eq!(f.filtered_url_func("hello, url('1' )url(  2  ) bork"), "hello, url(1)url(2) bork");
    assert_eq!(f.filtered_url_func("hello, url('(' )"), "hello, url(%28)");
}

#[test]
fn css() {
    let f = Filter::new();
    assert_eq!(f.filtered_url_func("color: red; background: URL(X); huh"), "color: red; background: url(X); huh");
    assert_eq!(f.filtered_url_func("@import 'foo'; FONT-size: 1em;"), " FONT-size: 1em;");
    assert_eq!(f.filtered_url_func("u;url(data:x);rl(hack)"), "u;url(#);rl(hack)");
    assert_eq!(f.filtered_url_func("u;\\;rl(hack)"), "u;rl(hack)");
    assert_eq!(f.filtered_url_func("u;\\,rl(hack)"), "u;rl(hack)");
    assert_eq!(f.filtered_url_func("u;\\url(hack)"), "u;");
    assert_eq!(f.filtered_url_func("font-size: 1em; @Import 'foo';"), "font-size: 1em;");
    assert_eq!(f.filtered_url_func("@\\69MporT 'foo';"), "");
    assert_eq!(f.filtered_url_func("color: red; background: URL( url(); huh)"), "color: red; huh)");
    assert_eq!(f.filtered_url_func("color: red; background: URL(//x/rel);"), "color: red; background: url(/rel);");
    assert_eq!(f.filtered_url_func("color: red; background: URL(data:xx);"), "color: red; background: url(#);");
    assert_eq!(f.filtered_url_func("color: red; background: UR\\L(x); border: blue"), "color: red; border: blue");
    assert_eq!(f.filtered_url_func("prop: url (it is not);"), "prop: url (it is not);");
}

#[test]
fn data_url_filter() {
    let mut f = Filter::new();
    f.set_data_url_filter(|_data| {
        DataUrlFilterResult::Keep
    });
    assert!(f.filter_url("data://wat").is_none());
    assert_eq!(f.filter_url("data:text/plain,meh").unwrap(), "data:text/plain,meh");

    f.set_data_url_filter(|data| {
        assert_eq!(b"hello test"[..], data.decode_to_vec().unwrap().0);
        DataUrlFilterResult::Drop
    });
    assert!(f.filter_url("data:text/plain,hello%20test").is_none());

    f.set_data_url_filter(|data| {
        assert_eq!(b"meh"[..], data.decode_to_vec().unwrap().0);
        DataUrlFilterResult::Rewrite { mime_type: "text/html".into(), data: "hi".into() }
    });
    assert_eq!(f.filter_url("data:text/plain,meh").unwrap(), "data:text/html;base64,aGk");

    f.set_data_url_filter(|data| {
        assert_eq!(b"hi"[..], data.decode_to_vec().unwrap().0);
        DataUrlFilterResult::Keep
    });
    assert_eq!(f.filter_url("data:text/html;base64,aGk#frag").unwrap(), "data:text/html;base64,aGk#frag");
}

#[test]
fn url_filter() {
    let f = Filter::new();
    assert_eq!(f.filter_url("http://test.com/a.jpg").unwrap(), "/a.jpg");
    assert_eq!(
        f.filter_url("https://test.com:123/.././a/b/c.jpg").unwrap(),
        "/a/b/c.jpg"
    );
    assert_eq!(
        f.filter_url("/hello world.jpg").unwrap(),
        "/hello%20world.jpg"
    );
    assert_eq!(f.filter_url("b.jpg").unwrap(), "b.jpg");
    assert_eq!(f.filter_url("./x/").unwrap(), "x/");
    assert_eq!(f.filter_url("#hash").unwrap(), "#hash");
    assert_eq!(f.filter_url("?q s").unwrap(), "?q%20s");
    assert_eq!(f.filter_url("//host/PAth").unwrap(), "/PAth");
    assert_eq!(f.filter_url("//host/%2fpath").unwrap(), "/%2fpath");
    assert_eq!(f.filter_url("//host%%%/path"), None);
    assert_eq!(f.filter_url("//host///path"), None);
    assert_eq!(f.filter_url("data:text/html,xx"), None);
    assert_eq!(f.filter_url("blob:123"), None);
    assert_eq!(f.filter_url("javascript:alert(1)"), None);
    assert_eq!(f.filter_url("jAvascript: alert(1)"), None);
    assert_eq!(f.filter_url("  jAvascript: alert(1) //http://"), None);
}
