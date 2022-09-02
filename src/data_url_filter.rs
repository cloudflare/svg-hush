
/// For the [`crate::Filter::set_data_url_filter`] callback.
pub use data_url::mime::Mime;

/// See <https://lib.rs/data-url>
pub use data_url::DataUrl;

/// Result of the callback of [`crate::Filter::set_data_url_filter`]
pub enum DataUrlFilterResult {
    /// The data URL will be kept exactly as-is.
    Keep,
    /// The URL will be removed entirely, and likely cause its attribute to be removed too
    Drop,
    /// A new `data:` URL will be encoded from this raw binary data
    Rewrite { mime_type: String, data: Vec<u8> }
}

/// An example preset for [`crate::Filter::set_data_url_filter`]
pub fn allow_standard_images(data: &DataUrl) -> DataUrlFilterResult {
    let mime = data.mime_type();
    // It's not safe to add uncommon formats to this list, because unrecognized data: URL types trigger downloads.
    // SVG could be filtered here recursively, PRs welcome.
    if mime.type_ == "image" && matches!(mime.subtype.as_str(), "jpeg" | "png" | "gif") {
        DataUrlFilterResult::Keep
    } else {
        DataUrlFilterResult::Drop
    }
}
