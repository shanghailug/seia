let imports = {};
let wasm;

let cachegetUint8Memory0 = null;
function getUint8Memory0() {
    if (cachegetUint8Memory0 === null || cachegetUint8Memory0.buffer !== wasm.memory.buffer) {
        cachegetUint8Memory0 = new Uint8Array(wasm.memory.buffer);
    }
    return cachegetUint8Memory0;
}

let WASM_VECTOR_LEN = 0;

function passArray8ToWasm0(arg, malloc) {
    const ptr = malloc(arg.length * 1);
    getUint8Memory0().set(arg, ptr / 1);
    WASM_VECTOR_LEN = arg.length;
    return ptr;
}

let cachegetInt32Memory0 = null;
function getInt32Memory0() {
    if (cachegetInt32Memory0 === null || cachegetInt32Memory0.buffer !== wasm.memory.buffer) {
        cachegetInt32Memory0 = new Int32Array(wasm.memory.buffer);
    }
    return cachegetInt32Memory0;
}

function getArrayU8FromWasm0(ptr, len) {
    return getUint8Memory0().subarray(ptr / 1, ptr / 1 + len);
}
/**
* @param {Uint8Array} message
* @param {Uint8Array} secret_key
* @returns {Uint8Array}
*/
module.exports.sign = function(message, secret_key) {
    var ptr0 = passArray8ToWasm0(message, wasm.__wbindgen_malloc);
    var len0 = WASM_VECTOR_LEN;
    var ptr1 = passArray8ToWasm0(secret_key, wasm.__wbindgen_malloc);
    var len1 = WASM_VECTOR_LEN;
    wasm.sign(8, ptr0, len0, ptr1, len1);
    var r0 = getInt32Memory0()[8 / 4 + 0];
    var r1 = getInt32Memory0()[8 / 4 + 1];
    var v2 = getArrayU8FromWasm0(r0, r1).slice();
    wasm.__wbindgen_free(r0, r1 * 1);
    return v2;
};

/**
* @param {Uint8Array} message
* @param {Uint8Array} public_key
* @param {Uint8Array} signature
* @returns {boolean}
*/
module.exports.verify = function(message, public_key, signature) {
    var ptr0 = passArray8ToWasm0(message, wasm.__wbindgen_malloc);
    var len0 = WASM_VECTOR_LEN;
    var ptr1 = passArray8ToWasm0(public_key, wasm.__wbindgen_malloc);
    var len1 = WASM_VECTOR_LEN;
    var ptr2 = passArray8ToWasm0(signature, wasm.__wbindgen_malloc);
    var len2 = WASM_VECTOR_LEN;
    var ret = wasm.verify(ptr0, len0, ptr1, len1, ptr2, len2);
    return ret !== 0;
};

/**
* @param {Uint8Array} seed
* @returns {Uint8Array}
*/
module.exports.keypair = function(seed) {
    var ptr0 = passArray8ToWasm0(seed, wasm.__wbindgen_malloc);
    var len0 = WASM_VECTOR_LEN;
    wasm.keypair(8, ptr0, len0);
    var r0 = getInt32Memory0()[8 / 4 + 0];
    var r1 = getInt32Memory0()[8 / 4 + 1];
    var v1 = getArrayU8FromWasm0(r0, r1).slice();
    wasm.__wbindgen_free(r0, r1 * 1);
    return v1;
};

const path = require('path').join(__dirname, 'rust_crypto_ed25519_bg.wasm');
const bytes = require('fs').readFileSync(path);

const wasmModule = new WebAssembly.Module(bytes);
const wasmInstance = new WebAssembly.Instance(wasmModule, imports);
wasm = wasmInstance.exports;
module.exports.__wasm = wasm;

