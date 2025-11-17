#!/usr/bin/env bash
set -e

wasm32-wasi-cabal build deltaq-live
wasm32-wasi-cabal list-bin exe:deltaq-live
EXE_WASM="$(wasm32-wasi-cabal list-bin exe:deltaq-live)"
cp -f "$EXE_WASM" "www/deltaq-live.wasm"

"$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs \
  --input "$EXE_WASM" --output "www/ghc_wasm_jsffi.js"
