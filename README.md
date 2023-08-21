# svg-hush

The goal of this tool is to make arbitrary SVG files as benign and safe to serve as images in other common Web file formats. SVG files aren't just images, they're documents with full access to all HTML and JavaScript features. This tool filters SVG files to remove use of any potentially risky features.

  * Removes scripting. Prevents SVG files from being used for cross-site scripting attacks. Although browsers don't allow scripts in `<img>`, they do allow scripting when SVG files are opened directly as a top-level document.

  * Removes hyperlinks to documents on other domains. Makes SVG files less attractive for SEO spam and phishing.

  * Removes references to cross-origin resources. Stops 3rd parties from tracking who is viewing the image.

This tool removes any elements and attributes that aren't in its allowlist and filters all URLs to be same-origin only (paths without a host name). It may break some SVG images. Please [file a bug](https://github.com/cloudflare/svg-hush/issues) when you find an image that is filtered too harshly!

This tool might make SVG files smaller by removing unnecessary junk from them, but it's not meant to be an SVG optimizer. It's safe to combine svg-hush with SVG optimization tools.

## Sanitization vs CSP

Regardless of this filtering, it's best to serve SVG images with a restrictive `Content-Security-Policy`. This tool is a defense-in-depth for cases where the CSP header may be unsupported, lost, or bypassed (e.g. due to server/CMS misconfiguration or a `ServiceWorker` that doesn't forward headers).

### Known limitations

* Legacy text encodings are not supported. UTF-8, UTF-16, and latin1 are supported.
* `DOCTYPE`s referencing external DTD files are not allowed. Internal subset is supported.
* The lax SVG-in-HTML syntax dialect is not supported. SVG documents must be well-formed XML and use the SVG namespace.
