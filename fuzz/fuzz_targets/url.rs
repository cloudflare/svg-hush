#![no_main]
use libfuzzer_sys::{Corpus, fuzz_target};

fuzz_target!(|data: &[u8]| -> Corpus {
    if data.len() > 100 {
        return Corpus::Reject;
    }
    let mut hush = svg_hush::Filter::new();
    hush.set_data_url_filter(|_| svg_hush::data_url_filter::DataUrlFilterResult::Drop);

    let mut xml = Vec::new();
    xml.extend_from_slice(b"<style><![CDATA[");
    xml.extend_from_slice(String::from_utf8_lossy(data).as_bytes());
    xml.extend_from_slice(b"]]></style>");

    let mut out = Vec::<u8>::with_capacity(xml.len());
    if let Ok(()) = hush.filter(xml.as_slice(), &mut out) {
        out.make_ascii_lowercase();
        for (pos, _) in out.windows(4).enumerate().filter(|(_, frag)| frag == b"url(") {
            if let Some(frag) = out.get(pos..pos+6) {
               assert_eq!(frag, b"url(#)");
            }
        }
        Corpus::Keep
    } else {
        Corpus::Reject
    }
});
