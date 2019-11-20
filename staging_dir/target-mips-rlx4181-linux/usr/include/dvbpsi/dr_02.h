/*****************************************************************************
 * dr_02.h
 * (c)2001-2002 VideoLAN
 * $Id: dr_02.h 88 2004-02-24 14:31:18Z sam $
 *
 * Authors: Arnaud de Bossoreille de Ribou <bozo@via.ecp.fr>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 *****************************************************************************/

/*!
 * \file <dr_02.h>
 * \author Arnaud de Bossoreille de Ribou <bozo@via.ecp.fr>
 * \brief Application interface for the MPEG 2 "video stream" descriptor
 * decoder and generator.
 *
 * Application interface for the MPEG 2 "video stream" descriptor
 * decoder and generator. This descriptor's definition can be found in
 * ISO/IEC 13818-1 section 2.6.2.
 */

#ifndef _DVBPSI_DR_02_H_
#define _DVBPSI_DR_02_H_

#ifdef __cplusplus
extern "C" {
#endif


/*****************************************************************************
 * dvbpsi_vstream_dr_t
 *****************************************************************************/
/*!
 * \struct dvbpsi_vstream_dr_s
 * \brief "video stream" descriptor structure.
 *
 * This structure is used to store a decoded "video stream" descriptor.
 * (ISO/IEC 13818-1 section 2.6.2).
 */
/*!
 * \typedef struct dvbpsi_vstream_dr_s dvbpsi_vstream_dr_t
 * \brief dvbpsi_vstream_dr_t type definition.
 */
typedef struct dvbpsi_vstream_dr_s
{
  int       b_multiple_frame_rate;      /*!< multiple_frame_rate_flag */
  uint8_t   i_frame_rate_code;          /*!< frame_rate_code */
  int       b_mpeg2;                    /*!< MPEG_2_flag */
  int       b_constrained_parameter;    /*!< constrained_parameter_flag */
  int       b_still_picture;            /*!< still_picture_flag */

  /* used if b_mpeg2 is true */
  uint8_t   i_profile_level_indication; /*!< profile_and_level_indication */
  uint8_t   i_chroma_format;            /*!< chroma_format */
  int       b_frame_rate_extension;     /*!< frame_rate_extension_flag */

} dvbpsi_vstream_dr_t;


/*****************************************************************************
 * dvbpsi_DecodeVStreamDr
 *****************************************************************************/
/*!
 * \fn dvbpsi_vstream_dr_t * dvbpsi_DecodeVStreamDr(
                                        dvbpsi_descriptor_t * p_descriptor)
 * \brief "video stream" descriptor decoder.
 * \param p_descriptor pointer to the descriptor structure
 * \return a pointer to a new "video stream" descriptor structure which
 * contains the decoded data.
 */
dvbpsi_vstream_dr_t* dvbpsi_DecodeVStreamDr(dvbpsi_descriptor_t * p_descriptor);


/*****************************************************************************
 * dvbpsi_GenVStreamDr
 *****************************************************************************/
/*!
 * \fn dvbpsi_descriptor_t * dvbpsi_GenVStreamDr(
                        dvbpsi_vstream_dr_t * p_decoded, int b_duplicate)
 * \brief "video stream" descriptor generator.
 * \param p_decoded pointer to a decoded "video stream" descriptor structure
 * \param b_duplicate if non zero then duplicate the p_decoded structure into
 * the descriptor
 * \return a pointer to a new descriptor structure which contains encoded data.
 */
dvbpsi_descriptor_t * dvbpsi_GenVStreamDr(dvbpsi_vstream_dr_t * p_decoded,
                                          int b_duplicate);


#ifdef __cplusplus
};
#endif

#else
#error "Multiple inclusions of dr_02.h"
#endif

