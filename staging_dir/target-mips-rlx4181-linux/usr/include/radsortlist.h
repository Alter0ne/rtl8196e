#ifndef INC_radsortlisth
#define INC_radsortlisth
#ifdef __cplusplus
extern "C" {
#endif
/*---------------------------------------------------------------------------
 
  FILENAME:
        radsortlist.h
 
  PURPOSE:
        Sorted (ascending) linked list class.
 
  REVISION HISTORY:
        Date            Engineer        Revision        Remarks
        11/14/98        MST             0               Original
        12/23/01        MST             1               Port to "C"
        02/08/07        MST             2               Add 64-bit support
 
  NOTES:
        See list.h for list usage.
 
  LICENSE:
        Copyright 2001-2005 Mark S. Teel. All rights reserved.

        Redistribution and use in source and binary forms, with or without 
        modification, are permitted provided that the following conditions 
        are met:

        1. Redistributions of source code must retain the above copyright 
           notice, this list of conditions and the following disclaimer.
        2. Redistributions in binary form must reproduce the above copyright 
           notice, this list of conditions and the following disclaimer in the 
           documentation and/or other materials provided with the distribution.

        THIS SOFTWARE IS PROVIDED BY Mark Teel ``AS IS'' AND ANY EXPRESS OR 
        IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
        WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
        DISCLAIMED. IN NO EVENT SHALL MARK TEEL OR CONTRIBUTORS BE LIABLE FOR 
        ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
        DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS 
        OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
        HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, 
        STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING 
        IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
        POSSIBILITY OF SUCH DAMAGE.
  
----------------------------------------------------------------------------*/

/*  ... includes
*/
#include <stdlib.h>
#include <stdio.h>

#include <radsysdefs.h>
#include <radlist.h>


/*  ... macro definitions
*/


/*  ... typedefs
*/


/*  ... consts
*/


typedef struct
{
    RADLIST     list;
    long        (*keyFunc) (void *data);
} *SORTLIST_ID;


/* ... API
*/

/* ... create a sorted list; getKey function returns the "ordinal" given
 ... a pointer to the list node (void *) - this is used to sort the list;
    ... if getKey is NULL, this will use the value of the NODE_PTR (not very
    ... useful!);
    ... returns SORTLIST_ID or NULL
*/
extern SORTLIST_ID radSortListInit (long (*getKey)(void *data));

/* ... this WILL free all nodes on the sorted list before destroying
 ... the list
*/
extern void radSortListExit (SORTLIST_ID id);

/* ... insert into the list; returns OK or ERROR
*/
extern int radSortListInsert (SORTLIST_ID id, NODE_PTR node);

/* ... find and remove a node given the NODE_PTR; returns OK or ERROR
*/
extern int radSortListRemove (SORTLIST_ID id, NODE_PTR node);

/* ... find a NODE_PTR given the "ordinal" or "key" of the node;
 ... returns NODE_PTR or NULL
*/
extern NODE_PTR radSortListFind (SORTLIST_ID id, long key);


#ifdef __cplusplus
}
#endif
#endif

