/*****************************************************************************
 * dvbpsi.h
 * (c)2001-2002 VideoLAN
 * $Id: dvbpsi.h 138 2007-01-22 18:03:13Z massiot $
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
 * \file <dvbpsi.h>
 * \author Arnaud de Bossoreille de Ribou <bozo@via.ecp.fr>
 * \brief Application interface for all DVB/PSI decoders.
 *
 * Application interface for all DVB/PSI decoders. The generic decoder
 * structure is public so that external decoders are allowed.
 */

#ifndef _DVBPSI_DVBPSI_H_
#define _DVBPSI_DVBPSI_H_

#define DVBPSI_VERSION      0.1.6
#define DVBPSI_VERSION_INT  ((0<<16)+(1<<8)+6)

#ifdef __cplusplus
extern "C" {
#endif


/*****************************************************************************
 * dvbpsi_handle
 *****************************************************************************/
/*!
 * \typedef struct dvbpsi_decoder_s * dvbpsi_handle
 * \brief Decoder abstration.
 */
typedef struct dvbpsi_decoder_s * dvbpsi_handle;


/*****************************************************************************
 * dvbpsi_PushPacket
 *****************************************************************************/
/*!
 * \fn void dvbpsi_PushPacket(dvbpsi_handle h_dvbpsi, uint8_t* p_data)
 * \brief Injection of a TS packet into a PSI decoder.
 * \param h_dvbpsi handle to the decoder
 * \param p_data pointer to a 188 bytes playload of a TS packet
 * \return nothing.
 *
 * Injection of a TS packet into a PSI decoder.
 */
void dvbpsi_PushPacket(dvbpsi_handle h_dvbpsi, uint8_t* p_data);


/*****************************************************************************
 * The following definitions are just here to allow external decoders but
 * shouldn't be used for any other purpose.
 *****************************************************************************/

/*!
 * \typedef struct dvbpsi_psi_section_s dvbpsi_psi_section_t
 * \brief dvbpsi_psi_section_t type definition.
 */
typedef struct dvbpsi_psi_section_s dvbpsi_psi_section_t;


/*****************************************************************************
 * dvbpsi_callback
 *****************************************************************************/
/*!
 * \typedef void (* dvbpsi_callback)(dvbpsi_handle p_decoder,
                                     dvbpsi_psi_section_t* p_section)
 * \brief Callback type definition.
 */
typedef void (* dvbpsi_callback)(dvbpsi_handle p_decoder,
                                 dvbpsi_psi_section_t* p_section);


/*****************************************************************************
 * dvbpsi_decoder_t
 *****************************************************************************/
/*!
 * \struct dvbpsi_decoder_s
 * \brief PSI decoder structure.
 *
 * This structure shouldn't be used but if you want to write an external
 * decoder.
 */
/*!
 * \typedef struct dvbpsi_decoder_s dvbpsi_decoder_t
 * \brief dvbpsi_decoder_t type definition.
 */
typedef struct dvbpsi_decoder_s
{
  dvbpsi_callback               pf_callback;            /*!< PSI decoder's
                                                             callback */

  void *                        p_private_decoder;      /*!< specific
                                                             decoder */

  int                           i_section_max_size;     /*!< Max size of a
                                                             section for this
                                                             decoder */

  uint8_t                       i_continuity_counter;   /*!< Continuity
                                                             counter */
  int                           b_discontinuity;        /*!< Discontinuity
                                                             flag */

  dvbpsi_psi_section_t *        p_current_section;      /*!< Current section */
  int                           i_need;                 /*!< Bytes needed */
  int                           b_complete_header;      /*!< Flag for header
                                                             completion */

} dvbpsi_decoder_t;


#ifdef __cplusplus
};
#endif

#else
#error "Multiple inclusions of dvbpsi.h"
#endif

