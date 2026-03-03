.PHONY: all jit haskell clean test

all: jit haskell

jit:
	cd jit && cargo build --release

haskell: jit
	cabal build
	@# Sign the binary with JIT entitlements for macOS aarch64
	@BINARY=$$(cabal list-bin haskellboy 2>/dev/null | tail -1); \
	codesign --force --sign - --entitlements jit.entitlements "$$BINARY" 2>/dev/null || true

clean:
	cd jit && cargo clean
	cabal clean

test: all
	cabal test cpu-tests
	cabal test rom-tests

blargg: all
	cabal run blargg-tests

run: all
	cabal run haskellboy -- --jit --skip-boot $(ROM)
