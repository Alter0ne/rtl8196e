/*
 * Copyright (C) 2010-2012 Free Software Foundation, Inc.
 *
 * Author: Nikos Mavrogiannopoulos
 *
 * This file is part of GnuTLS.
 *
 * The GnuTLS is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>
 *
 */

#ifndef __GNUTLS_PKCS11_H
#define __GNUTLS_PKCS11_H


#include <stdarg.h>
#include <gnutls/gnutls.h>
#include <gnutls/x509.h>

/* *INDENT-OFF* */
#ifdef __cplusplus
extern "C" {
#endif
/* *INDENT-ON* */

#define GNUTLS_PKCS11_MAX_PIN_LEN 32

/**
 * gnutls_pkcs11_token_callback_t:
 * @userdata: user-controlled data from gnutls_pkcs11_set_token_function().
 * @label: token label.
 * @retry: retry counter, initially 0.
 *
 * Token callback function. The callback will be used to ask the user
 * to re-insert the token with given (null terminated) label.  The
 * callback should return zero if token has been inserted by user and
 * a negative error code otherwise.  It might be called multiple times
 * if the token is not detected and the retry counter will be
 * increased.
 *
 * Returns: %GNUTLS_E_SUCCESS (0) on success or a negative error code
 * on error.
 *
 * Since: 2.12.0
 **/
typedef int (*gnutls_pkcs11_token_callback_t) (void *const
					       userdata,
					       const char *const
					       label, unsigned retry);


struct gnutls_pkcs11_obj_st;
typedef struct gnutls_pkcs11_obj_st *gnutls_pkcs11_obj_t;


#define GNUTLS_PKCS11_FLAG_MANUAL 0	/* Manual loading of libraries */
#define GNUTLS_PKCS11_FLAG_AUTO 1	/* Automatically load libraries by reading /etc/gnutls/pkcs11.conf */

/* pkcs11.conf format:
 * load = /lib/xxx-pkcs11.so
 * load = /lib/yyy-pkcs11.so
 */

int gnutls_pkcs11_init(unsigned int flags,
		       const char *deprecated_config_file);
int gnutls_pkcs11_reinit(void);
void gnutls_pkcs11_deinit(void);
void gnutls_pkcs11_set_token_function
    (gnutls_pkcs11_token_callback_t fn, void *userdata);

void gnutls_pkcs11_set_pin_function(gnutls_pin_callback_t fn,
				    void *userdata);

gnutls_pin_callback_t gnutls_pkcs11_get_pin_function(void
						     **userdata);

int gnutls_pkcs11_add_provider(const char *name, const char *params);
int gnutls_pkcs11_obj_init(gnutls_pkcs11_obj_t * obj);
void gnutls_pkcs11_obj_set_pin_function(gnutls_pkcs11_obj_t obj,
					gnutls_pin_callback_t fn,
					void *userdata);

/**
 * gnutls_pkcs11_obj_flags:
 * @GNUTLS_PKCS11_OBJ_FLAG_LOGIN: Force login in the token for the operation.
 * @GNUTLS_PKCS11_OBJ_FLAG_MARK_TRUSTED: object marked as trusted.
 * @GNUTLS_PKCS11_OBJ_FLAG_MARK_SENSITIVE: object marked as sensitive (unexportable).
 * @GNUTLS_PKCS11_OBJ_FLAG_LOGIN_SO: force login as a security officer in the token for the operation.
 * @GNUTLS_PKCS11_OBJ_FLAG_MARK_PRIVATE: marked as private (requires PIN to access).
 * @GNUTLS_PKCS11_OBJ_FLAG_MARK_NOT_PRIVATE: marked as not private.
 * @GNUTLS_PKCS11_OBJ_FLAG_RETRIEVE_ANY: When retrieving an object, do not set any requirements.
 * @GNUTLS_PKCS11_OBJ_FLAG_RETRIEVE_TRUSTED: When retrieving an object, only retrieve the marked as trusted.
 *   In gnutls_pkcs11_crt_is_known() it implies %GNUTLS_PKCS11_OBJ_FLAG_RETRIEVE_COMPARE if %GNUTLS_PKCS11_OBJ_FLAG_COMPARE_KEY is not given.
 * @GNUTLS_PKCS11_OBJ_FLAG_RETRIEVE_DISTRUSTED: When retrieving an object, only retrieve the marked as distrusted.
 * @GNUTLS_PKCS11_OBJ_FLAG_COMPARE: When checking an object's presence, fully compare it before returning any result.
 * @GNUTLS_PKCS11_OBJ_FLAG_COMPARE_KEY: When checking an object's presence, compare the key before returning any result.
 * @GNUTLS_PKCS11_OBJ_FLAG_PRESENT_IN_TRUSTED_MODULE: The object must be present in a marked as trusted module.
 * @GNUTLS_PKCS11_OBJ_FLAG_MARK_CA: Mark the object as a CA.
 * @GNUTLS_PKCS11_OBJ_FLAG_MARK_KEY_WRAP: Mark the generated key pair as wrapping and unwrapping keys.
 * @GNUTLS_PKCS11_OBJ_FLAG_OVERWRITE_TRUSTMOD_EXT: When an issuer is requested, override its extensions with the ones present in the trust module.
 *
 * Enumeration of different PKCS #11 object flags.
 */
typedef enum gnutls_pkcs11_obj_flags {
	GNUTLS_PKCS11_OBJ_FLAG_LOGIN = (1<<0),
	GNUTLS_PKCS11_OBJ_FLAG_MARK_TRUSTED = (1<<1),
	GNUTLS_PKCS11_OBJ_FLAG_MARK_SENSITIVE = (1<<2),
	GNUTLS_PKCS11_OBJ_FLAG_LOGIN_SO = (1<<3),
	GNUTLS_PKCS11_OBJ_FLAG_MARK_PRIVATE = (1<<4),
	GNUTLS_PKCS11_OBJ_FLAG_MARK_NOT_PRIVATE = (1<<5),
	GNUTLS_PKCS11_OBJ_FLAG_RETRIEVE_ANY = (1<<6),
	GNUTLS_PKCS11_OBJ_FLAG_RETRIEVE_TRUSTED = (1<<7),
	GNUTLS_PKCS11_OBJ_FLAG_RETRIEVE_DISTRUSTED = (1<<8),
	GNUTLS_PKCS11_OBJ_FLAG_COMPARE = (1<<9),
	GNUTLS_PKCS11_OBJ_FLAG_PRESENT_IN_TRUSTED_MODULE = (1<<10),
	GNUTLS_PKCS11_OBJ_FLAG_MARK_CA = (1<<11),
	GNUTLS_PKCS11_OBJ_FLAG_MARK_KEY_WRAP = (1<<12),
	GNUTLS_PKCS11_OBJ_FLAG_COMPARE_KEY = (1<<13),
	GNUTLS_PKCS11_OBJ_FLAG_OVERWRITE_TRUSTMOD_EXT = (1<<14)
	/* flags 1<<29 and later are reserved - see pkcs11_int.h */
} gnutls_pkcs11_obj_flags;

/**
 * gnutls_pkcs11_url_type_t:
 * @GNUTLS_PKCS11_URL_GENERIC: A generic-purpose URL.
 * @GNUTLS_PKCS11_URL_LIB: A URL that specifies the library used as well.
 * @GNUTLS_PKCS11_URL_LIB_VERSION: A URL that specifies the library and its version.
 *
 * Enumeration of different URL extraction flags.
 */
typedef enum {
	GNUTLS_PKCS11_URL_GENERIC,	/* URL specifies the object on token level */
	GNUTLS_PKCS11_URL_LIB,	/* URL specifies the object on module level */
	GNUTLS_PKCS11_URL_LIB_VERSION	/* URL specifies the object on module and version level */
} gnutls_pkcs11_url_type_t;

int gnutls_pkcs11_obj_import_url(gnutls_pkcs11_obj_t obj,
				 const char *url, unsigned int flags
				 /* GNUTLS_PKCS11_OBJ_FLAG_* */ );
int gnutls_pkcs11_obj_export_url(gnutls_pkcs11_obj_t obj,
				 gnutls_pkcs11_url_type_t detailed,
				 char **url);
void gnutls_pkcs11_obj_deinit(gnutls_pkcs11_obj_t obj);

int gnutls_pkcs11_obj_export(gnutls_pkcs11_obj_t obj,
			     void *output_data, size_t * output_data_size);
int gnutls_pkcs11_obj_export2(gnutls_pkcs11_obj_t obj,
			      gnutls_datum_t * out);

int gnutls_pkcs11_obj_export3(gnutls_pkcs11_obj_t obj, gnutls_x509_crt_fmt_t fmt,
			      gnutls_datum_t * out);

int gnutls_pkcs11_get_raw_issuer(const char *url, gnutls_x509_crt_t cert,
			     gnutls_datum_t * issuer,
			     gnutls_x509_crt_fmt_t fmt,
			     unsigned int flags);

int gnutls_pkcs11_crt_is_known(const char *url, gnutls_x509_crt_t cert,
			     unsigned int flags);

int gnutls_pkcs11_copy_x509_crt(const char *token_url,
				gnutls_x509_crt_t crt,
				const char *label, unsigned int flags
				/* GNUTLS_PKCS11_OBJ_FLAG_* */ );
int gnutls_pkcs11_copy_x509_privkey(const char *token_url,
				    gnutls_x509_privkey_t key,
				    const char *label,
				    unsigned int key_usage
				    /*GNUTLS_KEY_* */ ,
				    unsigned int flags
				    /* GNUTLS_PKCS11_OBJ_FLAG_* */
    );
int gnutls_pkcs11_delete_url(const char *object_url, unsigned int flags
			     /* GNUTLS_PKCS11_OBJ_FLAG_* */ );

int gnutls_pkcs11_copy_secret_key(const char *token_url,
				  gnutls_datum_t * key,
				  const char *label, unsigned int key_usage
				  /* GNUTLS_KEY_* */ ,
				  unsigned int flags
				  /* GNUTLS_PKCS11_OBJ_FLAG_* */ );

/**
 * gnutls_pkcs11_obj_info_t:
 * @GNUTLS_PKCS11_OBJ_ID_HEX: The object ID in hex.
 * @GNUTLS_PKCS11_OBJ_LABEL: The object label.
 * @GNUTLS_PKCS11_OBJ_TOKEN_LABEL: The token's label.
 * @GNUTLS_PKCS11_OBJ_TOKEN_SERIAL: The token's serial number.
 * @GNUTLS_PKCS11_OBJ_TOKEN_MANUFACTURER: The token's manufacturer.
 * @GNUTLS_PKCS11_OBJ_TOKEN_MODEL: The token's model.
 * @GNUTLS_PKCS11_OBJ_ID: The object ID.
 * @GNUTLS_PKCS11_OBJ_LIBRARY_VERSION: The library's used to access the object version.
 * @GNUTLS_PKCS11_OBJ_LIBRARY_DESCRIPTION: The library's used to access the object description (name).
 * @GNUTLS_PKCS11_OBJ_LIBRARY_MANUFACTURER: The library's used to access the object manufacturer name.
 *
 * Enumeration of several object information types.
 */
typedef enum {
	GNUTLS_PKCS11_OBJ_ID_HEX = 1,
	GNUTLS_PKCS11_OBJ_LABEL,
	GNUTLS_PKCS11_OBJ_TOKEN_LABEL,
	GNUTLS_PKCS11_OBJ_TOKEN_SERIAL,
	GNUTLS_PKCS11_OBJ_TOKEN_MANUFACTURER,
	GNUTLS_PKCS11_OBJ_TOKEN_MODEL,
	GNUTLS_PKCS11_OBJ_ID,
	/* the pkcs11 provider library info  */
	GNUTLS_PKCS11_OBJ_LIBRARY_VERSION,
	GNUTLS_PKCS11_OBJ_LIBRARY_DESCRIPTION,
	GNUTLS_PKCS11_OBJ_LIBRARY_MANUFACTURER
} gnutls_pkcs11_obj_info_t;

int gnutls_pkcs11_obj_get_info(gnutls_pkcs11_obj_t crt,
			       gnutls_pkcs11_obj_info_t itype,
			       void *output, size_t * output_size);

/**
 * gnutls_pkcs11_obj_attr_t:
 * @GNUTLS_PKCS11_OBJ_ATTR_CRT_ALL: Specify all certificates in the specified token.
 * @GNUTLS_PKCS11_OBJ_ATTR_CRT_TRUSTED: Specify all certificates marked as trusted in the specified token.
 * @GNUTLS_PKCS11_OBJ_ATTR_CRT_TRUSTED_CA: Specify all certificates marked as trusted and are CAs in the specified token.
 * @GNUTLS_PKCS11_OBJ_ATTR_CRT_WITH_PRIVKEY: Specify all certificates with a corresponding private key in the specified token.
 * @GNUTLS_PKCS11_OBJ_ATTR_PUBKEY: Specify all public keys in the specified token.
 * @GNUTLS_PKCS11_OBJ_ATTR_PRIVKEY: Specify all private keys in the specified token.
 * @GNUTLS_PKCS11_OBJ_ATTR_ALL: Specify all objects in the specified token.
 * @GNUTLS_PKCS11_OBJ_ATTR_MATCH: Only the objects that match the URL.
 *
 * Enumeration of several attributes for object enumeration.
 */
typedef enum {
	GNUTLS_PKCS11_OBJ_ATTR_CRT_ALL = 1,	/* all certificates */
	GNUTLS_PKCS11_OBJ_ATTR_CRT_TRUSTED,	/* certificates marked as trusted */
	GNUTLS_PKCS11_OBJ_ATTR_CRT_WITH_PRIVKEY,	/* certificates with corresponding private key */
	GNUTLS_PKCS11_OBJ_ATTR_PUBKEY,	/* public keys */
	GNUTLS_PKCS11_OBJ_ATTR_PRIVKEY,	/* private keys */
	GNUTLS_PKCS11_OBJ_ATTR_ALL,	/* everything! */
	GNUTLS_PKCS11_OBJ_ATTR_CRT_TRUSTED_CA,	/* CAs */
	GNUTLS_PKCS11_OBJ_ATTR_MATCH
} gnutls_pkcs11_obj_attr_t;

/**
 * gnutls_pkcs11_token_info_t:
 * @GNUTLS_PKCS11_TOKEN_LABEL: The token's label (string)
 * @GNUTLS_PKCS11_TOKEN_SERIAL: The token's serial number (string)
 * @GNUTLS_PKCS11_TOKEN_MANUFACTURER: The token's manufacturer (string)
 * @GNUTLS_PKCS11_TOKEN_MODEL: The token's model (string)
 * @GNUTLS_PKCS11_TOKEN_TRUSTED: Whether the token is marked as trusted in p11-kit (unsigned int)
 *
 * Enumeration of types for retrieving token information.
 */
typedef enum {
	GNUTLS_PKCS11_TOKEN_LABEL,
	GNUTLS_PKCS11_TOKEN_SERIAL,
	GNUTLS_PKCS11_TOKEN_MANUFACTURER,
	GNUTLS_PKCS11_TOKEN_MODEL
} gnutls_pkcs11_token_info_t;

/**
 * gnutls_pkcs11_obj_type_t:
 * @GNUTLS_PKCS11_OBJ_UNKNOWN: Unknown PKCS11 object.
 * @GNUTLS_PKCS11_OBJ_X509_CRT: X.509 certificate.
 * @GNUTLS_PKCS11_OBJ_PUBKEY: Public key.
 * @GNUTLS_PKCS11_OBJ_PRIVKEY: Private key.
 * @GNUTLS_PKCS11_OBJ_SECRET_KEY: Secret key.
 * @GNUTLS_PKCS11_OBJ_DATA: Data object.
 * @GNUTLS_PKCS11_OBJ_X509_CRT_EXTENSION: X.509 certificate extension (supported by p11-kit trust module only).
 *
 * Enumeration of object types.
 */
typedef enum {
	GNUTLS_PKCS11_OBJ_UNKNOWN,
	GNUTLS_PKCS11_OBJ_X509_CRT,
	GNUTLS_PKCS11_OBJ_PUBKEY,
	GNUTLS_PKCS11_OBJ_PRIVKEY,
	GNUTLS_PKCS11_OBJ_SECRET_KEY,
	GNUTLS_PKCS11_OBJ_DATA,
	GNUTLS_PKCS11_OBJ_X509_CRT_EXTENSION
} gnutls_pkcs11_obj_type_t;

int
gnutls_pkcs11_token_init(const char *token_url,
			 const char *so_pin, const char *label);

int
gnutls_pkcs11_token_get_mechanism(const char *url,
				  unsigned int idx,
				  unsigned long *mechanism);

int gnutls_pkcs11_token_set_pin(const char *token_url, const char *oldpin, const char *newpin, unsigned int flags	/*gnutls_pin_flag_t */);

int gnutls_pkcs11_token_get_url(unsigned int seq,
				gnutls_pkcs11_url_type_t detailed,
				char **url);
int gnutls_pkcs11_token_get_info(const char *url,
				 gnutls_pkcs11_token_info_t ttype,
				 void *output, size_t * output_size);

#define GNUTLS_PKCS11_TOKEN_HW 1
#define GNUTLS_PKCS11_TOKEN_TRUSTED (1<<1) /* p11-kit trusted */
int gnutls_pkcs11_token_get_flags(const char *url, unsigned int *flags);

int gnutls_pkcs11_obj_list_import_url(gnutls_pkcs11_obj_t * p_list,
				      unsigned int *const n_list,
				      const char *url,
				      gnutls_pkcs11_obj_attr_t
				      attrs, unsigned int flags
				      /* GNUTLS_PKCS11_OBJ_FLAG_* */
    );

int
gnutls_pkcs11_obj_list_import_url2(gnutls_pkcs11_obj_t ** p_list,
				   unsigned int *n_list,
				   const char *url,
				   gnutls_pkcs11_obj_attr_t attrs,
				   unsigned int flags
				   /* GNUTLS_PKCS11_OBJ_FLAG_* */
    );

int gnutls_x509_crt_import_pkcs11(gnutls_x509_crt_t crt,
				  gnutls_pkcs11_obj_t pkcs11_crt);
int gnutls_x509_crt_import_pkcs11_url(gnutls_x509_crt_t crt,
				      const char *url, unsigned int flags
				      /* GNUTLS_PKCS11_OBJ_FLAG_* */
    );

gnutls_pkcs11_obj_type_t
gnutls_pkcs11_obj_get_type(gnutls_pkcs11_obj_t obj);
const char *gnutls_pkcs11_type_get_name(gnutls_pkcs11_obj_type_t type);

int
gnutls_pkcs11_obj_get_exts(gnutls_pkcs11_obj_t obj,
			   struct gnutls_x509_ext_st **exts, unsigned int *exts_size,
			   unsigned int flags);

int
gnutls_pkcs11_obj_get_flags(gnutls_pkcs11_obj_t obj, unsigned int *oflags);
char *gnutls_pkcs11_obj_flags_get_str(unsigned int flags);

int gnutls_x509_crt_list_import_pkcs11(gnutls_x509_crt_t * certs,
				       unsigned int cert_max,
				       gnutls_pkcs11_obj_t *
				       const objs, unsigned int flags
				       /* must be zero */ );

/* private key functions...*/
int gnutls_pkcs11_privkey_init(gnutls_pkcs11_privkey_t * key);
void gnutls_pkcs11_privkey_set_pin_function(gnutls_pkcs11_privkey_t
					    key,
					    gnutls_pin_callback_t
					    fn, void *userdata);
void gnutls_pkcs11_privkey_deinit(gnutls_pkcs11_privkey_t key);
int gnutls_pkcs11_privkey_get_pk_algorithm(gnutls_pkcs11_privkey_t
					   key, unsigned int *bits);
int gnutls_pkcs11_privkey_get_info(gnutls_pkcs11_privkey_t pkey,
				   gnutls_pkcs11_obj_info_t itype,
				   void *output, size_t * output_size);

int gnutls_pkcs11_privkey_import_url(gnutls_pkcs11_privkey_t pkey,
				     const char *url, unsigned int flags);

int gnutls_pkcs11_privkey_export_url(gnutls_pkcs11_privkey_t key,
				     gnutls_pkcs11_url_type_t
				     detailed, char **url);
int gnutls_pkcs11_privkey_status(gnutls_pkcs11_privkey_t key);

int gnutls_pkcs11_privkey_generate(const char *url,
				   gnutls_pk_algorithm_t pk,
				   unsigned int bits,
				   const char *label, unsigned int flags);

int
gnutls_pkcs11_privkey_generate2(const char *url,
				gnutls_pk_algorithm_t pk,
				unsigned int bits,
				const char *label,
				gnutls_x509_crt_fmt_t fmt,
				gnutls_datum_t * pubkey,
				unsigned int flags);

int
gnutls_pkcs11_privkey_export_pubkey(gnutls_pkcs11_privkey_t pkey,
			      gnutls_x509_crt_fmt_t fmt,
			      gnutls_datum_t * pubkey,
			      unsigned int flags);

int
gnutls_pkcs11_token_get_random(const char *token_url,
			       void *data, size_t len);

/* *INDENT-OFF* */
#ifdef __cplusplus
}
#endif
/* *INDENT-ON* */
#endif
