#ifndef INC_radtextsearchh
#define INC_radtextsearchh
#ifdef __cplusplus
extern "C" {
#endif
/*---------------------------------------------------------------------------
 
  FILENAME:
        radtextsearch.h
 
  PURPOSE:
        Provide a red-black tree based text matching utility.
 
  REVISION HISTORY:
        Date            Engineer        Revision        Remarks
        1/7/2009        MST             0               Original
 
  NOTES:
        The inspiration for these algorithms was taken from Julienne Walker.

        This utility provides fast text string search given a well-known set
        of text strings (the universe). An ordinal value is stored with each
        text string in a red-black binary tree. Once the tree is populated with
        the universe of possible text strings and corresponding ordinal values,
        the find method can be used to retrieve the corresponding ordinal if
        a matching text string is found.        
 
  LICENSE:
        Copyright 2001-2009 Mark S. Teel. All rights reserved.

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
#include <radsysdefs.h>


// Define constants:
#define SEARCH_TEXT_MAX                 128


// Define data:

// Define the node:
typedef struct _rbNode
{
    int             red;
    struct _rbNode* link[2];
    char            text[SEARCH_TEXT_MAX];
    int             ordinal;
} SEARCH_NODE;


// Define the ID for a text search object:
typedef struct _rbTree
{
    SEARCH_NODE*    root;
} *TEXT_SEARCH_ID;



// API:

// Create a text search object:
// Returns the new search ID or ERROR if the ID cannot be allocated:
extern TEXT_SEARCH_ID radtextsearchInit (void);

// Cleanup and delete a search object:
extern void radtextsearchExit (TEXT_SEARCH_ID id);

// Insert a member of the text string universe:
// Returns OK or ERROR if a new node cannot be allocated:
extern int radtextsearchInsert (TEXT_SEARCH_ID id, const char* text, int ordinal);

// Delete a member of the universe:
// Returns OK if found/removed, ERROR otherwise:
extern int radtextsearchRemove (TEXT_SEARCH_ID id, const char* text);

// Find a member of the universe and return the corresponding ordinal in
// "ordinalStore":
// Returns OK if found (and ordinalStore populated) or ERROR if not found:
extern int radtextsearchFind (TEXT_SEARCH_ID id, const char* text, int* ordinalStore);

// Provide a tree traversal debugger:
extern int radtextsearchDebug (SEARCH_NODE* root);


#ifdef __cplusplus
}
#endif
#endif

