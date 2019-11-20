#ifndef INC_radstackh
#define INC_radstackh
#ifdef __cplusplus
extern "C" {
#endif
/*---------------------------------------------------------------------------
 
  FILENAME:
        radstack.h
 
  PURPOSE:
        Provide a generic push/pop stack utility.
 
  REVISION HISTORY:
        Date            Engineer        Revision        Remarks
        11/16/98        MST             0               Original
        12/9/01         MST             1               Port to C
 
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


/* ... define the stack "nodes"
*/
typedef struct stackNodeTag
{
    struct stackNodeTag *next;
}
STACK_NODE;

/* ... define the ID for a stack
*/
typedef struct
{
    STACK_NODE  *head;
    int    count;
}
*STACK_ID;



/*  ... create a stack
*/
extern STACK_ID radStackInit (void);

/* ... delete a stack (frees all the nodes as well!)
*/
extern void radStackExit (STACK_ID id);


/*  ... Push data onto stack
*/
extern int radStackPush (STACK_ID id, STACK_NODE *newNode);

/*  ... Pop data off the stack
*/
extern STACK_NODE *radStackPop (STACK_ID id);

/*  ... get count of nodes
*/
extern int radStackCount (STACK_ID id);


#ifdef __cplusplus
}
#endif
#endif

