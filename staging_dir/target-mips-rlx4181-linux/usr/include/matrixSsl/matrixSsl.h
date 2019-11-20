/*
 *	matrixSsl.h
 *	Release $Name: MATRIXSSL_1_2_4_OPEN $
 *	
 *	Public header file for MatrixSSL
 *	Implementations interacting with the matrixssl library should
 *	only use the APIs and definitions used in this file.
 */
/*
 *	Copyright (c) PeerSec Networks, 2002-2005. All Rights Reserved.
 *	The latest version of this code is available at http://www.matrixssl.org
 *
 *	This software is open source; you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation; either version 2 of the License, or
 *	(at your option) any later version.
 *
 *	This General Public License does NOT permit incorporating this software 
 *	into proprietary programs.  If you are unable to comply with the GPL, a 
 *	commercial license for this software may be purchased from PeerSec Networks
 *	at http://www.peersec.com
 *	
 *	This program is distributed in WITHOUT ANY WARRANTY; without even the 
 *	implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 *	See the GNU General Public License for more details.
 *	
 *	You should have received a copy of the GNU General Public License
 *	along with this program; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *	http://www.gnu.org/copyleft/gpl.html
 */
/******************************************************************************/

#ifndef _h_MATRIXSSL
#define _h_MATRIXSSL

#ifdef __cplusplus
extern "C" {
#endif

/******************************************************************************/
/*
	Platform integer sizes.  Must match values in osLayer.h
*/
#ifndef MATRIX_INT32
#define MATRIX_INT32
typedef int int32;
#endif

#ifndef MATRIX_UINT32
#define MATRIX_UINT32
typedef unsigned int uint32;
#endif

/******************************************************************************/
/*
	Maximum SSL record size, per specification
*/
#define		SSL_MAX_PLAINTEXT_LEN	0x4000	/* 16KB */
#define		SSL_MAX_RECORD_LEN		SSL_MAX_PLAINTEXT_LEN + 2048
#define		SSL_MAX_BUF_SIZE		SSL_MAX_RECORD_LEN + 0x5

/*
	Return codes from public apis
	Not all apis return all codes.  See documentation for more details.
*/
#define		SSL_SUCCESS			0	/* Generic success */
#define		SSL_ERROR			-1	/* generic ssl error, see error code */
#define		SSL_FULL			-2	/* must call sslRead before decoding */
#define		SSL_PARTIAL			-3	/* more data reqired to parse full msg */
#define		SSL_SEND_RESPONSE	-4	/* decode produced output data */
#define		SSL_PROCESS_DATA	-5	/* succesfully decoded application data */
#define		SSL_ALERT			-6	/* we've decoded an alert */
#define		SSL_FILE_NOT_FOUND	-7	/* File not found */
#define		SSL_MEM_ERROR		-8	/* Memory allocation failure */

/*
	SSL Alert levels and descriptions
	This implementation treats all alerts as fatal
*/
#define SSL_ALERT_LEVEL_WARNING				1
#define SSL_ALERT_LEVEL_FATAL				2

#define SSL_ALERT_CLOSE_NOTIFY				0
#define SSL_ALERT_UNEXPECTED_MESSAGE		10
#define SSL_ALERT_BAD_RECORD_MAC			20
#define SSL_ALERT_DECOMPRESSION_FAILURE		30
#define SSL_ALERT_HANDSHAKE_FAILURE			40
#define SSL_ALERT_NO_CERTIFICATE			41
#define SSL_ALERT_BAD_CERTIFICATE			42
#define SSL_ALERT_UNSUPPORTED_CERTIFICATE	43
#define SSL_ALERT_CERTIFICATE_REVOKED		44
#define SSL_ALERT_CERTIFICATE_EXPIRED		45
#define SSL_ALERT_CERTIFICATE_UNKNOWN		46
#define SSL_ALERT_ILLEGAL_PARAMETER			47

/******************************************************************************/
/*
	Typdefs required for public apis.  From an end user perspective, the 
	sslBuf_t and sslCertInfo_t types have internal fields that are public,
	but ssl_t, sslKeys_t, and sslSessionId_t do not.  Defining those as 'int32'
	requires it to be treated as an opaque data type to be passed to public apis
*/
#ifndef _h_MATRIXINTERNAL
typedef struct {
	unsigned char	*buf;	/* Pointer to the start of the buffer */
	unsigned char	*start;	/* Pointer to start of valid data */
	unsigned char	*end;	/* Pointer to first byte of invalid data */
	int32			size;	/* Size of buffer in bytes */
} sslBuf_t;

/*
	Information provided to user callback for validating certificates.
	Register callback with call to matrixSslSetCertValidator
*/
typedef struct {
	char	*country;
	char	*state;
	char	*locality;
	char	*organization;
	char	*orgUnit;
	char	*commonName;
} sslDistinguishedName_t;

typedef struct {
	char	*dns;
	char	*uri;
	char	*email;
} sslSubjectAltName_t;

typedef struct sslCertInfo {
	int32					verified;
	unsigned char			*serialNumber;
	int32						serialNumberLen;
	char					*notBefore;
	char					*notAfter;
	char					*sigHash;
	int32					sigHashLen;
	sslSubjectAltName_t		subjectAltName;
	sslDistinguishedName_t	subject;
	sslDistinguishedName_t	issuer;
	struct sslCertInfo		*next;
} sslCertInfo_t;

typedef int32		ssl_t;
typedef int32		sslKeys_t;
typedef int32		sslSessionId_t;

/*
	Flag indicating server session in matrixSslNewSession
*/
#define	SSL_FLAGS_SERVER		0x1
#define SSL_FLAGS_CLIENT_AUTH	0x200

#define	SSL_OPTION_DELETE_SESSION		0


#define SSL_MD5_DIGEST_LENGTH 16
#define SSL_MD5_CONTEXT_DATA_SIZE 96

typedef struct {
	unsigned char data[SSL_MD5_CONTEXT_DATA_SIZE];
} sslMd5Context_t;


#define SSL_SHA1_DIGEST_LENGTH 20
#define SSL_SHA1_CONTEXT_DATA_SIZE 96

typedef struct {
	unsigned char data[SSL_SHA1_CONTEXT_DATA_SIZE];
} sslSha1Context_t;


#define SSL_CIPHER_CONTEXT_DATA_SIZE 784

typedef struct {
	unsigned char data[SSL_CIPHER_CONTEXT_DATA_SIZE];
} sslCipherContext_t;

/******************************************************************************/
/*
	Explicitly import these apis on Windows.  If we're being included from the
	internal header, we export them instead!
*/
#ifdef WIN32
#define SSLPUBLIC extern __declspec(dllimport)
#endif /* WIN */
#else /* _h_MATRIXINTERNAL */
#ifdef WIN32
#define SSLPUBLIC extern __declspec(dllexport)
#endif /* WIN */
#endif /* _h_MATRIXINTERNAL */
#ifndef WIN32
#define SSLPUBLIC extern
#endif /* !WIN */

/******************************************************************************/
/*
 *	Public API set
 */
SSLPUBLIC int32	matrixSslOpen();
SSLPUBLIC void	matrixSslClose();

SSLPUBLIC int32	matrixSslReadKeys(sslKeys_t **keys, char *certFile,
						char *privFile, char *privPass, char *trustedCAFile);
SSLPUBLIC void	matrixSslFreeKeys(sslKeys_t *keys);

SSLPUBLIC int32	matrixSslNewSession(ssl_t **ssl, sslKeys_t *keys,
						sslSessionId_t *session, int32 flags);
SSLPUBLIC void	matrixSslDeleteSession(ssl_t *ssl);

SSLPUBLIC int32	matrixSslDecode(ssl_t *ssl, sslBuf_t *in, sslBuf_t *out, 
						unsigned char *error, unsigned char *alertLevel,
						unsigned char *alertDescription);
SSLPUBLIC int32	matrixSslEncode(ssl_t *ssl, unsigned char *in, int32 inlen,
						sslBuf_t *out);
SSLPUBLIC int32	matrixSslEncodeClosureAlert(ssl_t *ssl, sslBuf_t *out);

SSLPUBLIC int32	matrixSslHandshakeIsComplete(ssl_t *ssl);

SSLPUBLIC void	matrixSslSetCertValidator(ssl_t *ssl,
					int32 (*certValidator)(sslCertInfo_t *, void *arg),
					void *arg);

SSLPUBLIC void	matrixSslSetSessionOption(ssl_t *ssl, int32 option, void *arg);

/*
	Client side APIs
*/
SSLPUBLIC int32	matrixSslEncodeClientHello(ssl_t *ssl, sslBuf_t *out,
						unsigned short cipherSpec);

SSLPUBLIC int32	matrixSslGetSessionId(ssl_t *ssl, sslSessionId_t **sessionId);
SSLPUBLIC void	matrixSslFreeSessionId(sslSessionId_t *sessionId);

/*
	Server side APIs
*/
SSLPUBLIC int32 matrixSslEncodeHelloRequest(ssl_t *ssl, sslBuf_t *out);


SSLPUBLIC int32 matrixSslReadKeysMem(sslKeys_t **keys, char *certBuf, int32 certLen, 
								char *privBuf, int32 privLen, char *privPass,
								char *trustedCABuf, int32 trustedCALen);


/*
	ARC4 (RC4) API
*/

SSLPUBLIC void	matrixArc4Init(sslCipherContext_t *ctx, unsigned char *key, 
						int keylen);

SSLPUBLIC int	matrixArc4(sslCipherContext_t *ctx, unsigned char *in,
						unsigned char *out, int len);

/*
	3DES (Triple-DES) API
*/

SSLPUBLIC int	matrix3desInit(sslCipherContext_t *ctx, unsigned char *IV,
						unsigned char *key, int keylen);

SSLPUBLIC int	matrix3desEncrypt(sslCipherContext_t *ctx, unsigned char *pt, 
						unsigned char *ct, int len);

SSLPUBLIC int	matrix3desDecrypt(sslCipherContext_t *ctx, unsigned char *ct,
						unsigned char *pt, int len);

/*
	AES API
*/

SSLPUBLIC int	matrixAesInit(sslCipherContext_t *ctx, unsigned char *IV,
						unsigned char *key, int keylen);

SSLPUBLIC int	matrixAesEncrypt(sslCipherContext_t *ctx, unsigned char *pt,
						unsigned char *ct, int len);

SSLPUBLIC int	matrixAesDecrypt(sslCipherContext_t *ctx, unsigned char *ct,
						unsigned char *pt, int len);

/*
	MD5 API
*/

SSLPUBLIC void	matrixMd5Init(sslMd5Context_t *ctx);

SSLPUBLIC void	matrixMd5Update(sslMd5Context_t *ctx, const unsigned char *buf, 
						unsigned long len);

SSLPUBLIC int	matrixMd5Final(sslMd5Context_t *ctx, unsigned char *hash);

SSLPUBLIC unsigned char *matrixMd5Digest(const unsigned char *buf, 
						unsigned long len, unsigned char *hash);

/*
	SHA1 API
*/

SSLPUBLIC void	matrixSha1Init(sslSha1Context_t *ctx);

SSLPUBLIC void	matrixSha1Update(sslSha1Context_t *ctx, const unsigned char *buf, 
						unsigned long len);

SSLPUBLIC int	matrixSha1Final(sslSha1Context_t *ctx, unsigned char *hash);

SSLPUBLIC unsigned char *matrixSha1Digest(const unsigned char *buf, 
						unsigned long len, unsigned char *hash);

/******************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* _h_MATRIXSSL */

/******************************************************************************/

