#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    let mut out = Vec::with_capacity(data.len());
    if let Ok(()) = svg_hush::Filter::new().filter(data, &mut out) {
        let mut out2 = Vec::with_capacity(out.len());
        svg_hush::Filter::new().filter(&*out, &mut out2).unwrap();
        assert_eq!(out, out2);
    }
});
