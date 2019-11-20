/*****************************************************************************
 * dr_59.h
 * (c)2001-2002 VideoLAN
 * $Id: dr_59.h 93 2004-10-19 19:17:49Z massiot $
 *
 * Authors: Arnaud de Bossoreille de Ribou <bozo@via.ecp.fr>
 *          Tristan Leteurtre <tristan.leteurtre@anevia.com>
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
 * \file <dr_59.h>
 * \author Tristan Leteurtre <tristan.leteurtre@anevia.com>
 * \brief DVB subtitling descriptor parsing.
 *
 * DVB subtitling descriptor parsing, according to ETSI EN 300 468 
 * section 6.2.36.
 */

#ifndef _DVBPSI_DR_59_H_
#define _DVBPSI_DR_59_H_

#ifdef __cplusplus
extern "C" {
#endif

/*****************************************************************************
 * dvbpsi_subtitle_t
 *****************************************************************************/
/*!
 * \struct dvbpsi_subtitle_s
 * \brief  one subtitle structure.
 *
 * This structure is used since subtitling_descriptor will contain several
 * subtitles
 */
/*!
 * \typedef struct dvbpsi_subtitle_s dvbpsi_subtitle_t
 * \brief dvbpsi_subtitle_t type definition.
 */
typedef struct dvbpsi_subtitle_s
{
  uint8_t      i_iso6392_language_code[3];   
  uint8_t      i_subtitling_type;
  uint16_t      i_composition_page_id;
  uint16_t      i_ancillary_page_id;

} dvbpsi_subtitle_t;


/*****************************************************************************
 * dvbpsi_subtitling_dr_t
 *****************************************************************************/
/*!
 * \struct dvbpsi_subtitling_dr_s
 * \brief "subtitling" descriptor structure.
 *
 * This structure is used to store a decoded "subtitling"
 * descriptor. (ETSI EN 300 468 section 6.2.30).
 */
/*!
 * \typedef struct dvbpsi_subtitling_dr_s dvbpsi_subtitling_dr_t
 * \brief dvbpsi_subtitling_dr_t type definition.
 */
typedef struct dvbpsi_subtitling_dr_s
{
  uint8_t      i_subtitles_number;
  dvbpsi_subtitle_t p_subtitle[20];

} dvbpsi_subtitling_dr_t;


/*****************************************************************************
 * dvbpsi_DecodeSubtitlingDataDr
 *****************************************************************************/
/*!
 * \fn dvbpsi_subtitling_dr_t * dvbpsi_DecodeSubtitlingDr(
                                        dvbpsi_descriptor_t * p_descriptor)
 * \brief "subtitling" descriptor decoder.
 * \param p_descriptor pointer to the descriptor structure
 * \return a pointer to a new "subtitling" descriptor structure
 * which contains the decoded data.
 */
dvbpsi_subtitling_dr_t* dvbpsi_DecodeSubtitlingDr(
                                        dvbpsi_descriptor_t * p_descriptor);


/*****************************************************************************
 * dvbpsi_GenSubtitlingDataDr
 *****************************************************************************/
/*!
 * \fn dvbpsi_descriptor_t * dvbpsi_GenSubtitlingDr(
                        dvbpsi_subtitling_dr_t * p_decoded, int b_duplicate)
 * \brief "subtitling" descriptor generator.
 * \param p_decoded pointer to a decoded "subtitling" descriptor
 * structure
 * \param b_duplicate if non zero then duplicate the p_decoded structure into
 * the descriptor
 * \return a pointer to a new descriptor structure which contains encoded data.
 */
dvbpsi_descriptor_t * dvbpsi_GenSubtitlingDr(
                                        dvbpsi_subtitling_dr_t * p_decoded,
                                        int b_duplicate);


#ifdef __cplusplus
};
#endif

#else
#error "Multiple inclusions of dr_59.h"
#endif

