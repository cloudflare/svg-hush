# svg-hush

The goal of this tool is to make arbitrary SVG files as benign and safe to serve as images in other common Web file formats. SVG files aren't just images, they're documents with full access to all HTML and JavaScript features. This tool filters SVG files to remove use of any potentially risky features.

  * Removes scripting. Prevents SVG files from being used for cross-site scripting attacks. Although browsers don't allow scripts in `<img>`, they do allow scripting when SVG files are opened directly as a top-level document.

  * Removes hyperlinks to other documents. Makes SVG files less attractive for SEO spam and phishing.

  * Removes references to cross-origin resources. Stops 3rd parties from tracking who is viewing the image.

This tool removes any elements and attributes that aren't in its allowlist and filters all URLs to be same-origin only (paths without a host name). It is likely to break some SVG images. Please file a bug when you find an image that is filtered too harshly!

This tool might make SVG files smaller by removing unnecessary junk from them, but it's not meant to be an SVG optimizer. It should be safe to optimize filtered SVG images with other tools.

## Sanitization vs CSP

Regardless of this filtering, it's best to serve SVG images with a restrictive `Content-Security-Policy`. This tool is a defense-in-depth for cases where the CSP header may be unsupported, lost, or bypassed (e.g. due to server/CMS misconfiguration or a `ServiceWorker` that doesn't forward headers).

## Early release warning

This is an early version of this tool. There could be some SVG features or syntax that it fails to filter out. Please try to break it, and report any holes you find!

### Known limitations

* Only UTF-8 encoding is supported.
* `DOCTYPE`s with custom entities are not supported.
* SVG-in-HTML dialect is not supported. SVG documents must be well-formed XML and use the SVG namespace.
* `data:` URLs are not allowed.
