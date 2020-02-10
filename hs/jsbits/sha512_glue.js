/*

struct sha512_ctx
{
  uint64_t sz;
  uint64_t sz_hi;
  uint8_t  buf[128];
  uint64_t h[8];
};


void hs_cryptohash_sha512_init (struct sha512_ctx *ctx);
void hs_cryptohash_sha512_update (struct sha512_ctx *ctx, const uint8_t *data, size_t len);
void hs_cryptohash_sha512_finalize (struct sha512_ctx *ctx, uint8_t *out);

*/

var sha512 = Module();

const CTX_SIZE = 208;
const OUT_SIZE = 64;

function h$hs_cryptohash_sha512_init(ctx_p, ctx_o) {
    // ctx is 208 byte
    var ctx = sha512.stackAlloc(CTX_SIZE);

    sha512._hs_cryptohash_sha512_init(ctx);

    for (var i = 0; i < CTX_SIZE; ++i) {
        ctx_p.u8[ctx_o + i] = sha512.getValue(ctx + i);
    }
}

function h$hs_cryptohash_sha512_update(ctx_p, ctx_o, dat_p, dat_o, len) {
    // ctx is 208 byte
    // dat is len byte
    var ctx = sha512.stackAlloc(CTX_SIZE);
    var dat = sha512._malloc(len);

    // copy to ctx
    for (var i = 0; i < CTX_SIZE; ++i) {
        sha512.setValue(ctx + i, ctx_p.u8[ctx_o + i]);
    }

    // copy to dat
    for (var i = 0; i < len; ++i) {
        sha512.setValue(dat + i, dat_p.u8[dat_o + i]);
    }

    sha512._hs_cryptohash_sha512_update(ctx, dat, len);

    // copy ctx back
    for (var i = 0; i < CTX_SIZE; ++i) {
        ctx_p.u8[ctx_o + i] = sha512.getValue(ctx + i);
    }

    sha512._free(dat);
}

function h$hs_cryptohash_sha512_finalize(ctx_p, ctx_o, out_p, out_o) {
    // ctx is 208 byte
    // output is 8 x 64 bit
    var ctx = sha512.stackAlloc(CTX_SIZE);
    var out = sha512.stackAlloc(OUT_SIZE);

    for (var i = 0; i < CTX_SIZE; ++i) {
        sha512.setValue(ctx + i, ctx_p.u8[ctx_o + i]);
    }

    for (var i = 0; i < OUT_SIZE; ++i) {
        sha512.setValue(out + i, out_p.u8[out_o + i]);
    }

    sha512._hs_cryptohash_sha512_finalize(ctx, out);

    // back
    for (var i = 0; i < CTX_SIZE; ++i) {
        ctx_p.u8[ctx_o + i] = sha512.getValue(ctx + i);
    }

    for (var i = 0; i < OUT_SIZE; ++i) {
        out_p.u8[out_o + i] = sha512.getValue(out + i);
    }
}


function h$c_init(obj, off) {
    console.log(obj);
    console.log(off);

    for (var i = 0; i < obj.len / 4; i += 1) {
        obj.i3[i] = 0xffffffff - i;
    }

    console.log(obj);
}
