/* crypto/aes/aes.h -*- mode:C; c-file-style: "eay" -*- */
/* ====================================================================
 * Copyright (c) 1998-2002 The OpenSSL Project.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer. 
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. All advertising materials mentioning features or use of this
 *    software must display the following acknowledgment:
 *    "This product includes software developed by the OpenSSL Project
 *    for use in the OpenSSL Toolkit. (http://www.openssl.org/)"
 *
 * 4. The names "OpenSSL Toolkit" and "OpenSSL Project" must not be used to
 *    endorse or promote products derived from this software without
 *    prior written permission. For written permission, please contact
 *    openssl-core@openssl.org.
 *
 * 5. Products derived from this software may not be called "OpenSSL"
 *    nor may "OpenSSL" appear in their names without prior written
 *    permission of the OpenSSL Project.
 *
 * 6. Redistributions of any form whatsoever must retain the following
 *    acknowledgment:
 *    "This product includes software developed by the OpenSSL Project
 *    for use in the OpenSSL Toolkit (http://www.openssl.org/)"
 *
 * THIS SOFTWARE IS PROVIDED BY THE OpenSSL PROJECT ``AS IS'' AND ANY
 * EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE OpenSSL PROJECT OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 * ====================================================================
 *
 */

#ifndef HEADER_AES_H
#define HEADER_AES_H

#ifdef OPENSSL_NO_AES
#error AES is disabled.
#endif

#define AES_ENCRYPT	1
#define AES_DECRYPT	0

/* Because array size can't be a const in C, the following two are macros.
   Both sizes are in bytes. */
#define AES_MAXNR 14
#define AES_BLOCK_SIZE 16

#ifdef  __cplusplus
extern "C" {
#endif

/* This should be a hidden type, but EVP requires that the size be known */
struct aes_key_st {
    uint32 rd_key[4 *(AES_MAXNR + 1)];
    int32 rounds;
};
typedef struct aes_key_st AES_KEY;

const int8 *AES_options(void);

int32 AES_set_encrypt_key(const uint8 *userKey, const int32 bits,
	AES_KEY *key);
int32 AES_set_decrypt_key(const uint8 *userKey, const int32 bits,
	AES_KEY *key);

void AES_encrypt(const uint8 *in, uint8 *out,
	const AES_KEY *key);
void AES_decrypt(const uint8 *in, uint8 *out,
	const AES_KEY *key);

void AES_ecb_encrypt(const uint8 *in, uint8 *out,
	const AES_KEY *key, const int32 enc);
void soft_AES_cbc_encrypt(const uint8 *in, uint8 *out,
	const uint32 length, const AES_KEY *key,
	uint8 *ivec, const int32 enc);
void AES_cfb128_encrypt(const uint8 *in, uint8 *out,
	const uint32 length, const AES_KEY *key,
	uint8 *ivec, int32 *num, const int32 enc);
void AES_ofb128_encrypt(const uint8 *in, uint8 *out,
	const uint32 length, const AES_KEY *key,
	uint8 *ivec, int32 *num);
void AES_ctr128_encrypt(const uint8 *in, uint8 *out,
	const uint32 length, const AES_KEY *key,
	uint8 counter[AES_BLOCK_SIZE],
	uint8 ecount_buf[AES_BLOCK_SIZE],
	uint32 *num);
void AES_key_gen(AES_KEY *key, uint8 *in, int32 keylen, int32 round);

#ifdef  __cplusplus
}
#endif

#endif /* !HEADER_AES_H */
