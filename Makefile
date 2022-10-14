UNAME := $(shell uname)

ifeq ($(UNAME), win)
TARGET = sold.exe
else
TARGET = sold
endif

.deps-ready:
	bash script/toolchain.sh -s
	touch .deps-ready

clean:
	rm -fr release
	cargo clean

test: .deps-ready
	cargo test

fmt:
	cargo fmt

lint: .deps-ready
	cargo fmt --all -- --check
	cargo clippy --all-targets

qa: lint test

target/release/$(TARGET): .deps-ready
	cargo build --release

release: target/release/$(TARGET)
	mkdir -p release
	cp target/release/$(TARGET) release/

