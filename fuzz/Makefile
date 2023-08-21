fuzz: ./target/debug/filter
	./target/debug/filter

test: ./target/debug/idempotent
	./target/debug/idempotent

./target/debug/filter ./target/debug/idempotent: ../Cargo.toml Cargo.toml
	cargo +nightly rustc --bin $(notdir $@) -- -C passes='sancov-module ' -C llvm-args='-sanitizer-coverage-level=3' -C llvm-args='-sanitizer-coverage-inline-8bit-counters' -Z sanitizer=address

.PHONY: fuzz test
