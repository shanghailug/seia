var _nacl = require("./_tweetnacl")();

var SIG_LEN = 64

// uint8array, uint8array
function dsign(sk, dat) {
    var mlen = dat.length;

    var stack = _nacl.stackSave();

    var smlen_ptr = _nacl.stackAlloc(8);
    var sk_ptr    = _nacl.stackAlloc(SIG_LEN);

    var sm_ptr  = _nacl._malloc(mlen + SIG_LEN);
    var dat_ptr = _nacl._malloc(mlen);

    // copy
    _nacl.writeArrayToMemory(sk, sk_ptr);
    _nacl.writeArrayToMemory(dat, dat_ptr);

    //crypto_sign(sm,&smlen,m,mlen,sk);
    // '1' version swap mlen & sk
    _nacl._crypto_sign_ed25519_tweet1(sm_ptr, smlen_ptr, dat_ptr, sk_ptr, mlen);

    var res = new Uint8Array(new ArrayBuffer(SIG_LEN));

    _nacl.stackRestore(stack);

    for (i = 0; i < SIG_LEN; ++i) { res[i] = _nacl.getValue(sm_ptr + i); }

    _nacl._free(sm_ptr);
    _nacl._free(dat_ptr);

    return res;
}

function dverify(pk, sig, dat) {
    var res = -1;
    var i;
    var stack = _nacl.stackSave();

    var smlen = dat.length + SIG_LEN;
    var sm_ptr = _nacl._malloc(smlen);
    var m_ptr  = _nacl._malloc(smlen);

    var mlen_ptr = _nacl.stackAlloc(8);
    var pk_ptr   = _nacl.stackAlloc(SIG_LEN);

    for (i = 0; i < SIG_LEN; ++i) {
        _nacl.setValue(sm_ptr + i, sig[i]);
    }

    for (i = 0; i < dat.length; ++i) {
        _nacl.setValue(sm_ptr + i + SIG_LEN, dat[i]);
    }

    _nacl.writeArrayToMemory(pk, pk_ptr);

    //crypto_sign_open(m,&mlen,sm,smlen,pk);
    // '1' version swap smlen & pk
    //console.log("mlen_ptr",  mlen_ptr, "pk", pk_ptr);
    res = _nacl._crypto_sign_ed25519_tweet_open1(m_ptr, mlen_ptr, sm_ptr, pk_ptr, smlen);

    //console.log("res = ", res);

    _nacl._free(m_ptr);
    _nacl._free(sm_ptr);

    _nacl.stackRestore(stack);

    return (res == 0) ? true : false;
}

function onready(f) {
    _nacl.onRuntimeInitialized = f;
}

module.exports.onready = onready;
module.exports.dsign = dsign;
module.exports.dverify = dverify;
