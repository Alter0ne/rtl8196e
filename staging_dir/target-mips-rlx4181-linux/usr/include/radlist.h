#ifndef INC_radlisth
#define INC_radlisth
#ifdef __cplusplus
extern "C" {
#endif
/*---------------------------------------------------------------------------
 
  FILENAME:
        radlist.h
 
  PURPOSE:
        Provide the doubly-linked list API.
 
  REVISION HISTORY:
        Date            Engineer        Revision        Remarks
        5/26/95         M.S. Teel       0               Original
        9/26/99         M.S. Teel       1               Port to pSOS
        3/22/01         M.S. Teel       2               Port to Linux
 
  NOTES:
        
 
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

/*  ...HIDDEN, don't use
*/

struct nodeTag
{
    struct nodeTag  *prevNode;
    struct nodeTag  *nextNode;
};

struct listTag
{
    struct nodeTag *firstNode;
    struct nodeTag *lastNode;
    struct nodeTag dummyFirst;
    struct nodeTag dummyLast;
    int             noNodes;
};

/*  ... END HIDDEN
*/



typedef struct listTag *RADLIST_ID;
typedef struct listTag RADLIST;
typedef struct nodeTag *NODE_PTR;
typedef struct nodeTag  NODE;


/*  ... first, to create, reset and delete a RADLIST_ID
*/
extern RADLIST_ID radListCreate
(
    void
);
extern RADLIST_ID radListReset
(
    RADLIST_ID  list
);
extern void radListDelete
(
    RADLIST_ID  list
);


/*  ... Insertion calls
*/
extern void radListInsertAfter
(
    RADLIST_ID  list,
    NODE_PTR    afterThisOne,  /* NULL inserts at front */
    NODE_PTR    node
);
extern void radListInsertBefore
(
    RADLIST_ID  list,
    NODE_PTR    beforeThisOne,  /* NULL inserts at front */
    NODE_PTR    node
);
extern void radListAddToFront
(
    RADLIST_ID  list,
    NODE_PTR    node
);
extern void radListAddToEnd
(
    RADLIST_ID  list,
    NODE_PTR    node
);


/*  ... Node Removal calls (all but listRemove return pointer to node being removed)
*/
extern void radListRemove
(
    RADLIST_ID  list,
    NODE_PTR    removeThisOne
);
extern NODE_PTR radListRemoveFirst
(
    RADLIST_ID  list
);
extern NODE_PTR radListRemoveLast
(
    RADLIST_ID  list
);


/*  ... return number of nodes on list
*/
extern int radListGetNumberOfNodes
(
    RADLIST_ID  list
);


/*  ... Node pointer requests, the node is not removed from the list
*/
extern NODE_PTR radListGetFirst
(
    RADLIST_ID  list
);
extern NODE_PTR radListGetLast
(
    RADLIST_ID  list
);
extern NODE_PTR radListGetNext
(
    RADLIST_ID  list,
    NODE_PTR    afterThisOne  /* NULL -> listGetFirst */
);
extern NODE_PTR radListGetPrevious
(
    RADLIST_ID  list,
    NODE_PTR    beforeThisOne  /* NULL -> listGetLast */
);

#ifdef __cplusplus
}
#endif
#endif

