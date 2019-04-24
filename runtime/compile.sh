#!/bin/bash

set -e

rustup run nightly cargo rustc --target wasm32-unknown-unknown -- -Z print-link-args #-C opt-level=3 -C lto -C link-args=-s

echo "==== EXPORTED BY RUSTC ===="
wasm-nm -e ../target/wasm32-unknown-unknown/debug/runtime.wasm
EXPORTS=`wasm-nm -e ../target/wasm32-unknown-unknown/debug/runtime.wasm | sed "s/^e \(.*\)$/--export=\1 /" | tr -d "\n"`

clang-9 --target=wasm32 test.c -c -o test.a

#    ../whirl_cli/testing.core.s \

pushd ../whirl_cli
erlc +to_core testing.erl
cargo run -- compile_core_all --core testing.core --out-dir dst/
llc-9 --mtriple=wasm32-unknown-unknown --filetype=obj -o dst/testing.o dst/testing.bc
llc-9 --mtriple=wasm32-unknown-unknown --filetype=obj -o dst/whirlc_init_module.o dst/whirlc_init_module.bc
popd

wasm-ld-9 --allow-undefined --no-entry --stack-first -z stack-size=4194304 --gc-sections --print-gc-sections \
    $EXPORTS \
    test.a \
    ../whirl_cli/dst/testing.o \
    ../whirl_cli/dst/whirlc_init_module.o \
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
