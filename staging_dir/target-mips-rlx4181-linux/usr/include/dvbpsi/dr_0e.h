/*****************************************************************************
 * dr_0e.h
 * (c)2001-2002 VideoLAN
 * $Id: dr_0e.h 88 2004-02-24 14:31:18Z sam $
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
 * \file <dr_0e.h>
 * \author Arnaud de Bossoreille de Ribou <bozo@via.ecp.fr>
 * \brief Application interface for the MPEG 2 "maximum bitrate" descriptor
 * decoder and generator.
 *
 * Application interface for the MPEG 2 "maximum bitrate" descriptor
 * decoder and generator. This descriptor's definition can be found in
 * ISO/IEC 13818-1 section 2.6.26.
 */

#ifndef _DVBPSI_DR_0E_H_
#define _DVBPSI_DR_0E_H_

#ifdef __cplusplus
extern "C" {
#endif


/*****************************************************************************
 * dvbpsi_max_bitrate_dr_t
 *****************************************************************************/
/*!
 * \struct dvbpsi_max_bitrate_dr_s
 * \brief "maximum bitrate" descriptor structure.
 *
 * This structure is used to store a decoded "maximum bitrate" descriptor.
 * (ISO/IEC 13818-1 section 2.6.26).
 */
/*!
 * \typedef struct dvbpsi_max_bitrate_dr_s dvbpsi_max_bitrate_dr_t
 * \brief dvbpsi_max_bitrate_dr_t type definition.
 */
typedef struct dvbpsi_max_bitrate_dr_s
{
  uint32_t      i_max_bitrate;          /*!< maximum_bitrate */

} dvbpsi_max_bitrate_dr_t;


/*****************************************************************************
 * dvbpsi_DecodeMaxBitrateDr
 *****************************************************************************/
/*!
 * \fn dvbpsi_max_bitrate_dr_t * dvbpsi_DecodeMaxBitrateDr(
                                        dvbpsi_descriptor_t * p_descriptor)
 * \brief "maximum bitrate" descriptor decoder.
 * \param p_descriptor pointer to the descriptor structure
 * \return a pointer to a new "maximum bitrate" descriptor structure which
 * contains the decoded data.
 */
dvbpsi_max_bitrate_dr_t* dvbpsi_DecodeMaxBitrateDr(
                                        dvbpsi_descriptor_t * p_descriptor);


/*****************************************************************************
 * dvbpsi_GenMaxBitrateDr
 *****************************************************************************/
/*!
 * \fn dvbpsi_descriptor_t * dvbpsi_GenMaxBitrateDr(
                        dvbpsi_max_bitrate_dr_t * p_decoded, int b_duplicate)
 * \brief "maximum bitrate" descriptor generator.
 * \param p_decoded pointer to a decoded "maximum bitrate" descriptor structure
 * \param b_duplicate if non zero then duplicate the p_decoded structure into
 * the descriptor
 * \return a pointer to a new descriptor structure which contains encoded data.
 */
dvbpsi_descriptor_t * dvbpsi_GenMaxBitrateDr(
                                        dvbpsi_max_bitrate_dr_t * p_decoded,
                                        int b_duplicate);


#ifdef __cplusplus
};
#endif

#else
#error "Multiple inclusions of dr_0e.h"
#endif

