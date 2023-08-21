fuzz: ./target/debug/filter
	./target/debug/filter

./target/debug/filter: ../Cargo.toml Cargo.toml
	cargo +nightly rustc -- -C passes='sancov-module ' -C llvm-args='-sanitizer-coverage-level=3' -C llvm-args='-sanitizer-coverage-inline-8bit-counters' -Z sanitizer=address

.PHONY: fuzz
