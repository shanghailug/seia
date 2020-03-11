#include "tweetnacl.h"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef uint8_t u8;
typedef uint64_t u64;
typedef unsigned long long ull;

void randombytes(u8 *ptr, ull len)
{
    ull i;

    for (i = 0; i < len; ++i) {
        ptr[i] = rand();
    }
}

int main(int argc, char **argv)
{
    u8 sk[64] = {0}, pk[32] = {0};
    int i, j;
    u8 m[16+64];
    u8 sm[16+64] = {0};
    ull mlen, smlen;
    int res;

    // gen key
    crypto_sign_ed25519_tweet_keypair(pk, sk);

    for (i = 0; i < 32; i++) {
        printf("pk[%d] = %d\n", i, pk[i]);
    }

    for (i = 0; i < 64; i++) {
        printf("sk[%d] = %d\n", i, sk[i]);
    }

    mlen = 16;
    smlen = 0;

    for (i = 0; i < mlen; ++i) m[i] = i;

    for (j = 0; j < 1000; ++j) {
        crypto_sign_ed25519_tweet1(sm, &smlen, m, sk, mlen);

        //printf("smlen = %d\n", smlen);

        if (j == 0) {
            for (i = 0; i < smlen; ++i) {
                printf("%d: %d\n", i, sm[i]);
            }
        }

        res = crypto_sign_ed25519_tweet_open1(m, &mlen, sm, pk, smlen);
        if (j == 0) printf("open res = %d, len %lld\n", res, mlen);

        /*
          sm[1] = ~sm[1];
          res = crypto_sign_ed25519_tweet_open(m, &mlen, sm, smlen, pk);
          printf("open res = %d, len %d\n", res, mlen);
        */
    }

    return 0;
}
