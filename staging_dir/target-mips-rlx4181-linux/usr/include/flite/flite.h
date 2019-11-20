/*************************************************************************/
/*                                                                       */
/*                  Language Technologies Institute                      */
/*                     Carnegie Mellon University                        */
/*                        Copyright (c) 1999                             */
/*                        All Rights Reserved.                           */
/*                                                                       */
/*  Permission is hereby granted, free of charge, to use and distribute  */
/*  this software and its documentation without restriction, including   */
/*  without limitation the rights to use, copy, modify, merge, publish,  */
/*  distribute, sublicense, and/or sell copies of this work, and to      */
/*  permit persons to whom this work is furnished to do so, subject to   */
/*  the following conditions:                                            */
/*   1. The code must retain the above copyright notice, this list of    */
/*      conditions and the following disclaimer.                         */
/*   2. Any modifications must be clearly marked as such.                */
/*   3. Original authors' names are not deleted.                         */
/*   4. The authors' names are not used to endorse or promote products   */
/*      derived from this software without specific prior written        */
/*      permission.                                                      */
/*                                                                       */
/*  CARNEGIE MELLON UNIVERSITY AND THE CONTRIBUTORS TO THIS WORK         */
/*  DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING      */
/*  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT   */
/*  SHALL CARNEGIE MELLON UNIVERSITY NOR THE CONTRIBUTORS BE LIABLE      */
/*  FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    */
/*  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   */
/*  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          */
/*  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       */
/*  THIS SOFTWARE.                                                       */
/*                                                                       */
/*************************************************************************/
/*             Author:  Alan W Black (awb@cs.cmu.edu)                    */
/*               Date:  December 1999                                    */
/*************************************************************************/
/*                                                                       */
/*  Light weight run-time speech synthesis system, public API            */
/*                                                                       */
/*************************************************************************/
#ifndef _FLITE_H__
#define _FLITE_H__

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include "cst_string.h"
#include "cst_regex.h"
#include "cst_val.h"
#include "cst_features.h"
#include "cst_item.h"
#include "cst_relation.h"
#include "cst_utterance.h"
#include "cst_wave.h"
#include "cst_track.h"

#include "cst_cart.h"
#include "cst_phoneset.h"
#include "cst_voice.h"
#include "cst_audio.h"

#include "cst_utt_utils.h"
#include "cst_lexicon.h"
#include "cst_synth.h"
#include "cst_units.h"
#include "cst_tokenstream.h"

/* Public functions */
int flite_init();

cst_wave *flite_text_to_wave(const char *text,cst_voice *voice);
float flite_file_to_speech(const char *filename, 
			   cst_voice *voice,
			   const char *outtype);
float flite_text_to_speech(const char *text, 
			   cst_voice *voice,
			   const char *outtype);
float flite_phones_to_speech(const char *text, 
			     cst_voice *voice,
			     const char *outtype);
float flite_tokens_to_speech(cst_utterance *u,
			     cst_voice *voice,
			     const char *outtype);

cst_utterance *flite_synth_text(const char *text,cst_voice *voice);
cst_utterance *flite_synth_phones(const char *phones,cst_voice *voice);

#ifdef __cplusplus
}; /* extern "C" */
#endif /* __cplusplus */


#endif
