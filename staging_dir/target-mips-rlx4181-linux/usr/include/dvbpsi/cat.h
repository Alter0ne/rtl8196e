/*****************************************************************************
 * cat.h
 * (c)2001-2007 VideoLAN
 * $Id: cat.h 145 2007-10-05 17:53:35Z jpsaman $
 *
 * Authors: Johann Hanne
 *          heavily based on pmt.h which was written by
 *          Arnaud de Bossoreille de Ribou <bozo@via.ecp.fr>
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
 * \file <cat.h>
 * \author Arnaud de Bossoreille de Ribou <bozo@via.ecp.fr>
 * \brief Application interface for the CAT decoder and the CAT generator.
 *
 * Application interface for the CAT decoder and the CAT generator. New
 * decoded CAT tables are sent by callback to the application. If a table
 * wasn't active (b_current_next == 0) and the next is the same but active
 * (b_current_next == 1) then the two lists are empty and should be
 * caught from the previous structure.
 */

#ifndef _DVBPSI_CAT_H_
#define _DVBPSI_CAT_H_

#ifdef __cplusplus
extern "C" {
#endif


/*****************************************************************************
 * dvbpsi_cat_t
 *****************************************************************************/
/*!
 * \struct dvbpsi_cat_s
 * \brief CAT structure.
 *
 * This structure is used to store a decoded CAT.
 * (ISO/IEC 13818-1 section 2.4.4.6).
 */
/*!
 * \typedef struct dvbpsi_cat_s dvbpsi_cat_t
 * \brief dvbpsi_cat_t type definition.
 */
typedef struct dvbpsi_cat_s
{
  uint8_t                   i_version;          /*!< version_number */
  int                       b_current_next;     /*!< current_next_indicator */

  dvbpsi_descriptor_t *     p_first_descriptor; /*!< descriptor list */

} dvbpsi_cat_t;


/*****************************************************************************
 * dvbpsi_cat_callback
 *****************************************************************************/
/*!
 * \typedef void (* dvbpsi_cat_callback)(void* p_cb_data,
                                         dvbpsi_cat_t* p_new_cat)
 * \brief Callback type definition.
 */
typedef void (* dvbpsi_cat_callback)(void* p_cb_data, dvbpsi_cat_t* p_new_cat);


/*****************************************************************************
 * dvbpsi_AttachCAT
 *****************************************************************************/
/*!
 * \fn dvbpsi_handle dvbpsi_AttachCAT(dvbpsi_cat_callback pf_callback,
                                      void* p_cb_data)
 * \brief Creation and initialization of a CAT decoder.
 * \param pf_callback function to call back on new CAT
 * \param p_cb_data private data given in argument to the callback
 * \return a pointer to the decoder for future calls.
 */
dvbpsi_handle dvbpsi_AttachCAT(dvbpsi_cat_callback pf_callback,
                               void* p_cb_data);


/*****************************************************************************
 * dvbpsi_DetachCAT
 *****************************************************************************/
/*!
 * \fn void dvbpsi_DetachCAT(dvbpsi_handle h_dvbpsi)
 * \brief Destroy a CAT decoder.
 * \param h_dvbpsi handle to the decoder
 * \return nothing.
 *
 * The handle isn't valid any more.
 */
void dvbpsi_DetachCAT(dvbpsi_handle h_dvbpsi);


/*****************************************************************************
 * dvbpsi_InitCAT/dvbpsi_NewCAT
 *****************************************************************************/
/*!
 * \fn void dvbpsi_InitCAT(dvbpsi_cat_t* p_cat,
                           uint8_t i_version, int b_current_next)
 * \brief Initialize a user-allocated dvbpsi_cat_t structure.
 * \param p_cat pointer to the CAT structure
 * \param i_version CAT version
 * \param b_current_next current next indicator
 * \return nothing.
 */
void dvbpsi_InitCAT(dvbpsi_cat_t* p_cat,
                    uint8_t i_version, int b_current_next);

/*!
 * \def dvbpsi_NewCAT(p_cat,
                      i_version, b_current_next)
 * \brief Allocate and initialize a new dvbpsi_cat_t structure.
 * \param p_cat pointer to the CAT structure
 * \param i_version CAT version
 * \param b_current_next current next indicator
 * \return nothing.
 */
#define dvbpsi_NewCAT(p_cat,                                            \
                      i_version, b_current_next)                        \
do {                                                                    \
  p_cat = (dvbpsi_cat_t*)malloc(sizeof(dvbpsi_cat_t));                  \
  if(p_cat != NULL)                                                     \
    dvbpsi_InitCAT(p_cat, i_version, b_current_next);                   \
} while(0);


/*****************************************************************************
 * dvbpsi_EmptyCAT/dvbpsi_DeleteCAT
 *****************************************************************************/
/*!
 * \fn void dvbpsi_EmptyCAT(dvbpsi_cat_t* p_cat)
 * \brief Clean a dvbpsi_cat_t structure.
 * \param p_cat pointer to the CAT structure
 * \return nothing.
 */
void dvbpsi_EmptyCAT(dvbpsi_cat_t* p_cat);

/*!
 * \def dvbpsi_DeleteCAT(p_cat)
 * \brief Clean and free a dvbpsi_cat_t structure.
 * \param p_cat pointer to the CAT structure
 * \return nothing.
 */
#define dvbpsi_DeleteCAT(p_cat)                                         \
do {                                                                    \
  dvbpsi_EmptyCAT(p_cat);                                               \
  free(p_cat);                                                          \
} while(0);


/*****************************************************************************
 * dvbpsi_CATAddDescriptor
 *****************************************************************************/
/*!
 * \fn dvbpsi_descriptor_t* dvbpsi_CATAddDescriptor(dvbpsi_cat_t* p_cat,
                                                    uint8_t i_tag,
                                                    uint8_t i_length,
                                                    uint8_t* p_data)
 * \brief Add a descriptor in the CAT.
 * \param p_cat pointer to the CAT structure
 * \param i_tag descriptor's tag
 * \param i_length descriptor's length
 * \param p_data descriptor's data
 * \return a pointer to the added descriptor.
 */
dvbpsi_descriptor_t* dvbpsi_CATAddDescriptor(dvbpsi_cat_t* p_cat,
                                             uint8_t i_tag, uint8_t i_length,
                                             uint8_t* p_data);


/*****************************************************************************
 * dvbpsi_GenCATSections
 *****************************************************************************/
/*!
 * \fn dvbpsi_psi_section_t* dvbpsi_GenCATSections(dvbpsi_cat_t* p_cat)
 * \brief CAT generator
 * \param p_cat CAT structure
 * \return a pointer to the list of generated PSI sections.
 *
 * Generate CAT sections based on the dvbpsi_cat_t structure.
 */
dvbpsi_psi_section_t* dvbpsi_GenCATSections(dvbpsi_cat_t* p_cat);


#ifdef __cplusplus
};
#endif

#else
#error "Multiple inclusions of cat.h"
#endif

