fuzz: ./target/debug/filter
	./target/debug/filter

test: ./target/debug/idempotent
	./target/debug/idempotent

fuzz-url: ./target/debug/url
	./target/debug/url

./target/debug/filter ./target/debug/idempotent ./target/debug/url: ../Cargo.toml Cargo.toml ./fuzz_targets
	cargo +nightly rustc --target-dir ./target --bin $(notdir $@) -- -C passes='sancov-module ' -C llvm-args='-sanitizer-coverage-level=3' -C llvm-args='-sanitizer-coverage-inline-8bit-counters' -Z sanitizer=address

.PHONY: fuzz test
