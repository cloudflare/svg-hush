#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    let _ = svg_hush::Filter::new().filter(data, std::io::sink());
});
