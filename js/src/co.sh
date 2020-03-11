#!/bin/sh

# -O0 -g
#    -s STACK_OVERFLOW_CHECK=2 \

emcc -O3 \
    --memory-init-file 0 \
    -s NO_FILESYSTEM=1 \
    -s ALLOW_MEMORY_GROWTH=1 \
    -s MODULARIZE=1 \
    -s EXPORTED_FUNCTIONS='["_malloc", "_free", "_crypto_sign_ed25519_tweet1","_crypto_sign_ed25519_tweet_open1"]' \
    -s EXTRA_EXPORTED_RUNTIME_METHODS='["setValue", "getValue", "writeArrayToMemory", "stackAlloc", "stackSave", "stackRestore"]' \
    -o _tweetnacl.js \
    c_ext/tweetnacl.c
