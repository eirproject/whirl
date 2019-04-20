#!/bin/bash

set -e

rustup run nightly cargo rustc --target wasm32-unknown-unknown -- -Z print-link-args

echo "==== EXPORTED BY RUSTC ===="
wasm-nm -e ../target/wasm32-unknown-unknown/debug/runtime.wasm
EXPORTS=`wasm-nm -e ../target/wasm32-unknown-unknown/debug/runtime.wasm | sed "s/^e \(.*\)$/--export=\1 /" | tr -d "\n"`

clang-9 --target=wasm32 test.c -c -o test.a

#    ../whirl_cli/testing.core.s \

pushd ../whirl_cli
cargo run -- compile_core testing.core
llc-9 --mtriple=wasm32-unknown-unknown --filetype=obj -o testing.o testing.core.bc
popd

wasm-ld-9 --allow-undefined --no-entry \
    $EXPORTS \
    test.a \
    ../whirl_cli/testing.o \
    ../target/wasm32-unknown-unknown/debug/libruntime.a \
    -o linked.wasm

echo "==== EXPORTED MANUALLY ===="
wasm-nm -e linked.wasm

wasm-bindgen linked.wasm --out-dir out/

echo "==== EXPORTED MANUALLY AFTER BINDGEN ===="
wasm-nm -e out/linked_bg.wasm

#cp src/jslib.js out/

cd web
npm start
