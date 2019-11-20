/*
   (c) Copyright 2001-2009  The world wide DirectFB Open Source Community (directfb.org)
   (c) Copyright 2000-2004  Convergence (integrated media) GmbH

   All rights reserved.

   Written by Denis Oliver Kropp <dok@directfb.org>,
              Andreas Hundt <andi@fischlustig.de>,
              Sven Neumann <neo@directfb.org>,
              Ville Syrjälä <syrjala@sci.fi> and
              Claudio Ciccani <klan@users.sf.net>.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the
   Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.
*/

#ifndef __GENERIC_H__
#define __GENERIC_H__

#include <dfb_types.h>

#include <directfb.h>
#include <core/coretypes.h>
#include <core/gfxcard.h>

/* this order is required for Intel with MMX, how about bigendian? */

typedef union {
     struct {
          u16 b;
          u16 g;
          u16 r;
          u16 a;
     } RGB;
     struct {
          u16 u;
          u16 v;
          u16 y;
          u16 a;
     } YUV;
} GenefxAccumulator;


typedef struct _GenefxState GenefxState;

typedef void (*GenefxFunc)(GenefxState *gfxs);

/*
 * State of the virtual graphics processing unit "Genefx" (pron. 'genie facts').
 */
struct _GenefxState {
     GenefxFunc funcs[32];

     int length;    /* span length */
     int Slen;      /* span length (source) */
     int Dlen;      /* span length (destination) */

     /*
      * state values
      */
     void *dst_org[3];
     void *src_org[3];
     int dst_pitch;
     int src_pitch;

     int dst_bpp;
     int src_bpp;

     DFBSurfaceCapabilities dst_caps;
     DFBSurfaceCapabilities src_caps;

     DFBSurfacePixelFormat src_format;
     DFBSurfacePixelFormat dst_format;

     int dst_height;
     int src_height;

     int dst_field_offset;
     int src_field_offset;

     DFBColor color;

     /*
      * operands
      */
     void *Aop[3];
     void *Bop[3];
     u32   Cop;

     int   Astep;
     int   Bstep;

     u8 YCop;
     u8 CbCop;
     u8 CrCop;

     int Aop_field;
     int Bop_field;
     
     int AopY;
     int BopY;

     /*
      * color keys
      */
     u32 Dkey;
     u32 Skey;

     /*
      * color lookup tables
      */
     CorePalette *Alut;
     CorePalette *Blut;

     /*
      * accumulators
      */
     void              *ABstart;
     int                ABsize;
     GenefxAccumulator *Aacc;
     GenefxAccumulator *Bacc;
     GenefxAccumulator *Tacc; /* for simultaneous S+D blending */
     GenefxAccumulator  Cacc;
     GenefxAccumulator  SCacc;

     /*
      * dataflow control
      */
     GenefxAccumulator *Xacc; /* writing pointer for blending */
     GenefxAccumulator *Yacc; /* input pointer for blending */
     GenefxAccumulator *Dacc;
     GenefxAccumulator *Sacc;

     void        **Sop;
     CorePalette  *Slut;

     int Ostep; /* controls horizontal blitting direction */

     int SperD;     /* for scaled routines only */
     int Xphase;    /* initial value for fractional steps (zero if not clipped) */

     bool need_accumulator;

     int *trans;
     int  num_trans;
};


void gGetDriverInfo( GraphicsDriverInfo *info );
void gGetDeviceInfo( GraphicsDeviceInfo *info );

bool gAcquire  ( CardState *state, DFBAccelerationMask accel );
void gRelease ( CardState *state );

void gFillRectangle ( CardState *state, DFBRectangle *rect );
void gDrawLine      ( CardState *state, DFBRegion    *line );

void gBlit          ( CardState *state, DFBRectangle *rect, int dx, int dy );
void gStretchBlit   ( CardState *state, DFBRectangle *srect, DFBRectangle *drect );


#endif
