/*
 * libdlna: reference DLNA standards implementation.
 * Copyright (C) 2007 Benjamin Zores <ben@geexbox.org>
 *
 * This file is part of libdlna.
 *
 * libdlna is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * libdlna is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with libdlna; if not, write to the Free Software
 * Foundation, Inc, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#ifndef _DLNA_H_
#define _DLNA_H_

/**
 * @file dlna.h
 * external api header.
 */

#ifdef __cplusplus
extern "C" {
#if 0 /* avoid EMACS indent */
}
#endif /* 0 */
#endif /* __cplusplus */

#define DLNA_STRINGIFY(s)         DLNA_TOSTRING(s)
#define DLNA_TOSTRING(s) #s

#define LIBDLNA_VERSION_INT  ((0<<16)+(2<<8)+3)
#define LIBDLNA_VERSION      0.2.4
#define LIBDLNA_BUILD        LIBDLNA_VERSION_INT

#define LIBDLNA_IDENT        "DLNA " DLNA_STRINGIFY(LIBDLNA_VERSION)

typedef enum {
  DLNA_PROTOCOL_INFO_TYPE_UNKNOWN,
  DLNA_PROTOCOL_INFO_TYPE_HTTP,
  DLNA_PROTOCOL_INFO_TYPE_RTP,
  DLNA_PROTOCOL_INFO_TYPE_ANY
} dlna_protocol_info_type_t;

/* DLNA.ORG_PS: play speed parameter (integer)
 *     0 invalid play speed
 *     1 normal play speed
 */
typedef enum {
  DLNA_ORG_PLAY_SPEED_INVALID = 0,
  DLNA_ORG_PLAY_SPEED_NORMAL = 1,
} dlna_org_play_speed_t;

/* DLNA.ORG_CI: conversion indicator parameter (integer)
 *     0 not transcoded
 *     1 transcoded
 */
typedef enum {
  DLNA_ORG_CONVERSION_NONE = 0,
  DLNA_ORG_CONVERSION_TRANSCODED = 1,
} dlna_org_conversion_t;

/* DLNA.ORG_OP: operations parameter (string)
 *     "00" (or "0") neither time seek range nor range supported
 *     "01" range supported
 *     "10" time seek range supported
 *     "11" both time seek range and range supported
 */
typedef enum {
  DLNA_ORG_OPERATION_NONE                  = 0x00,
  DLNA_ORG_OPERATION_RANGE                 = 0x01,
  DLNA_ORG_OPERATION_TIMESEEK              = 0x10,
} dlna_org_operation_t;

/* DLNA.ORG_FLAGS, padded with 24 trailing 0s
 *     80000000  31  senderPaced
 *     40000000  30  lsopTimeBasedSeekSupported
 *     20000000  29  lsopByteBasedSeekSupported
 *     10000000  28  playcontainerSupported
 *      8000000  27  s0IncreasingSupported
 *      4000000  26  sNIncreasingSupported
 *      2000000  25  rtspPauseSupported
 *      1000000  24  streamingTransferModeSupported
 *       800000  23  interactiveTransferModeSupported
 *       400000  22  backgroundTransferModeSupported
 *       200000  21  connectionStallingSupported
 *       100000  20  dlnaVersion15Supported
 *
 *     Example: (1 << 24) | (1 << 22) | (1 << 21) | (1 << 20)
 *       DLNA.ORG_FLAGS=01700000[000000000000000000000000] // [] show padding
 */
typedef enum {
  DLNA_ORG_FLAG_SENDER_PACED               = (1 << 31),
  DLNA_ORG_FLAG_TIME_BASED_SEEK            = (1 << 30),
  DLNA_ORG_FLAG_BYTE_BASED_SEEK            = (1 << 29),
  DLNA_ORG_FLAG_PLAY_CONTAINER             = (1 << 28),
  DLNA_ORG_FLAG_S0_INCREASE                = (1 << 27),
  DLNA_ORG_FLAG_SN_INCREASE                = (1 << 26),
  DLNA_ORG_FLAG_RTSP_PAUSE                 = (1 << 25),
  DLNA_ORG_FLAG_STREAMING_TRANSFER_MODE    = (1 << 24),
  DLNA_ORG_FLAG_INTERACTIVE_TRANSFERT_MODE = (1 << 23),
  DLNA_ORG_FLAG_BACKGROUND_TRANSFERT_MODE  = (1 << 22),
  DLNA_ORG_FLAG_CONNECTION_STALL           = (1 << 21),
  DLNA_ORG_FLAG_DLNA_V15                   = (1 << 20),
} dlna_org_flags_t;

typedef enum {
  DLNA_CLASS_UNKNOWN,
  DLNA_CLASS_IMAGE,
  DLNA_CLASS_AUDIO,
  DLNA_CLASS_AV,
  DLNA_CLASS_COLLECTION
} dlna_media_class_t;

typedef enum {
  /* Image Class */
  DLNA_PROFILE_IMAGE_JPEG,
  DLNA_PROFILE_IMAGE_PNG,
  /* Audio Class */
  DLNA_PROFILE_AUDIO_AC3,
  DLNA_PROFILE_AUDIO_AMR,
  DLNA_PROFILE_AUDIO_ATRAC3,
  DLNA_PROFILE_AUDIO_LPCM,
  DLNA_PROFILE_AUDIO_MP3,
  DLNA_PROFILE_AUDIO_MPEG4,
  DLNA_PROFILE_AUDIO_WMA,
  /* AV Class */
  DLNA_PROFILE_AV_MPEG1,
  DLNA_PROFILE_AV_MPEG2,
  DLNA_PROFILE_AV_MPEG4_PART2,
  DLNA_PROFILE_AV_MPEG4_PART10, /* a.k.a. MPEG-4 AVC */
  DLNA_PROFILE_AV_WMV9
} dlna_media_profile_t;

/**
 * DLNA profile.
 * This specifies the DLNA profile one file/stream is compatible with.
 */
typedef struct dlna_profile_s {
  /* Profile ID, part of DLNA.ORG_PN= string */
  const char *id;
  /* Profile MIME type */
  const char *mime;
  /* Profile Label */
  const char *label;
  /* Profile type: IMAGE / AUDIO / AV */
  dlna_media_class_t class;
} dlna_profile_t;

/**
 * DLNA Library's controller.
 * This controls the whole library.
 */
typedef struct dlna_s dlna_t;

/**
 * Initialization of library.
 *
 * @warning This function must be called before any libdlna function.
 * @return DLNA library's controller.
 */
dlna_t *dlna_init (void);

/**
 * Uninitialization of library.
 *
 * @param[in] dlna The DLNA library's controller.
 */
void dlna_uninit (dlna_t *dlna);

/**
 * Set library's verbosity level.
 *
 * @param[in] dlna  The DLNA library's controller.
 * @param[in] level Level of verbosity (0 to disable, 1 to enable).
 */
void dlna_set_verbosity (dlna_t *dlna, int level);

/**
 * Set library's check level on files extension.
 *
 * @param[in] dlna  The DLNA library's controller.
 * @param[in] level Level of check (0 for no check, 1 to enable checks).
 */
void dlna_set_extension_check (dlna_t *dlna, int level);

/**
 * Register all known/supported DLNA profiles.
 *
 * @param[in] dlna  The DLNA library's controller.
 */
void dlna_register_all_media_profiles (dlna_t *dlna);

/**
 * Register one specific known/supported DLNA profiles.
 *
 * @param[in] dlna     The DLNA library's controller.
 * @param[in] profile  The profile ID to be registered.
 */
void dlna_register_media_profile (dlna_t *dlna, dlna_media_profile_t profile);


/**
 * Guess which DLNA profile one input file/stream is compatible with.
 *
 * @warning This function returns a pointer, do _NOT_ free it.
 * @param[in] dlna     The DLNA library's controller.
 * @param[in] filename The file to be checked for compliance.
 * @return A pointer on file's DLNA profile if compatible, NULL otherwise.
 */
dlna_profile_t *dlna_guess_media_profile (dlna_t *dlna, const char *filename);

/**
 * Provides UPnP A/V ContentDirectory Object Item associated to profile.
 *
 * @warning This function returns a pointer, do _NOT_ free it.
 * @param[in] profile The DLNA profile that was targeted.
 * @return A pointer on CDS Object Item string.
 */
char *dlna_profile_upnp_object_item (dlna_profile_t *profile);

/**
 * Output the protocol information string that must be send by a DMS to a DMP
 * for the file to be played/recognized.
 *
 * @param[in] type    Streaming method.
 * @param[in] speed   DLNA.ORG_PS parameter.
 * @param[in] ci      DLNA.ORG_CI parameter.
 * @param[in] op      DLNA.ORG_OP parameter.
 * @param[in] flags   DLNA.ORG_FLAGS parameter.
 * @param[in] profile The DLNA's file profile that has been guessed.
 * @return            The protocol information string.
 */
char * dlna_write_protocol_info (dlna_protocol_info_type_t type,
                                 dlna_org_play_speed_t speed,
                                 dlna_org_conversion_t ci,
                                 dlna_org_operation_t op,
                                 dlna_org_flags_t flags,
                                 dlna_profile_t *p);

/**
 * Create a valid UPnP device description for Digital Media Server (DMS).
 *
 * @param[in] friendly_name      UPnP device friendly name.
 * @param[in] manufacturer       UPnP device manufacturer.
 * @param[in] manufacturer_url   UPnP device manufacturer URL.
 * @param[in] model_description  UPnP device model description.
 * @param[in] model_name         UPnP device model name.
 * @param[in] model_number       UPnP device model number.
 * @param[in] model_url          UPnP device model URL.
 * @param[in] serial_number      UPnP device serial number.
 * @param[in] uuid               UPnP device unique identifier.
 * @param[in] presentation_url   UPnP device web presentation page URL.
 * @param[in] cms_scpd_url       UPnP ConnectionManager service SCPD URL.
 * @param[in] cms_control_url    UPnP ConnectionManager service control URL.
 * @param[in] cms_event_url      UPnP ConnectionManager service event URL.
 * @param[in] cds_scpd_url       UPnP ContentDirectory service SCPD URL.
 * @param[in] cds_control_url    UPnP ContentDirectory service control URL.
 * @param[in] cds_event_url      UPnP ContentDirectory service event URL.
 * 
 * @return                       The DMS device description string.
 */
char *
dlna_dms_description_get (const char *friendly_name,
                          const char *manufacturer,
                          const char *manufacturer_url,
                          const char *model_description,
                          const char *model_name,
                          const char *model_number,
                          const char *model_url,
                          const char *serial_number,
                          const char *uuid,
                          const char *presentation_url,
                          const char *cms_scpd_url,
                          const char *cms_control_url,
                          const char *cms_event_url,
                          const char *cds_scpd_url,
                          const char *cds_control_url,
                          const char *cds_event_url);

#ifdef __cplusplus
#if 0 /* avoid EMACS indent */
{
#endif /* 0 */
}
#endif /* __cplusplus */

#endif /* _DLNA_H_ */
