#ifndef INC_radconffileh
#define INC_radconffileh
#ifdef __cplusplus
extern "C" {
#endif
/*---------------------------------------------------------------------------
 
  FILENAME:
        radconffile.h
 
  PURPOSE:
        This file contains the definitions of functions used to read
        and write data to and from configuration files.
 
  REVISION HISTORY:
        Date            Engineer        Revision        Remarks
        02/03/04        MS Teel         0               Initial creation
 
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

#include <stdio.h>
#include <string.h>

#include <radsysdefs.h>
#include <radlist.h>



#define MAX_ID_LENGTH                       64
#define MAX_INSTANCE_LENGTH                 32
#define MAX_LINE_LENGTH                     512
#define MAX_COMMENT_LENGTH                  128
#define MAX_FILENAME_LENGTH                 256
#define CF_STAMP_VALUE                      0x51267E28


/*  Hidden...do not use!  */
typedef enum
{
    CF_COMMENT = 0,
    CF_ENTRY
} CF_ENTRY_TYPE;

typedef struct
{
    NODE           node;
    CF_ENTRY_TYPE  type;
}
CF_FILE_DATA_HDR;

typedef struct
{
    CF_FILE_DATA_HDR  hdr;
    char              comment[MAX_LINE_LENGTH];
}
CF_COMMENT_DATA_TYPE;

typedef struct
{
    CF_FILE_DATA_HDR  hdr;
    char              id[MAX_ID_LENGTH];
    char              instance[MAX_INSTANCE_LENGTH];
    char              value[MAX_LINE_LENGTH];
    char              comment[MAX_COMMENT_LENGTH];
}
CF_ENTRY_DATA_TYPE;

typedef struct
{
    char        fileName[MAX_FILENAME_LENGTH];
    NODE_PTR    lastSearchNode;
    RADLIST_ID  fileData;
}
CF_INFO_TYPE;

/*  End of hidden data  */


typedef CF_INFO_TYPE *CF_ID;


/*  ... Raeds the given config file, creating local record keeping about 
    ... the file.  If the file does not already exist, a new file is created.  
    ... Returns the CF_ID or NULL indicating an error.
    ... NOTE: This function reads the entire file into a RAM copy for
    ... use by the rest of the configFile utilities.  Any changes
    ... made to the config file via these utils will NOT take effect until 
    ... the application calls radCfFlush! The file is NOT locked for
    ... this open operation.
*/
extern CF_ID radCfOpen (char *file);

/*  ... Clean up local information about the file.
*/
extern void radCfClose (CF_ID file);

/*  ... Flushes the file to disk.  Returns OK or ERROR
    ... NOTE: This call MUST be made for any changes to config files to be
    ... saved into the file itself. The file is locked during the write
    ... process.
*/
extern int radCfFlush (CF_ID file);

/*  ... Creates a comment line at the end of the config file.
    ... Returns OK or ERROR.
*/
extern int radCfPutComment (CF_ID file, char *text);


//  ... for all of the following methods which require the "instance"
//  ... parameter, a value of NULL can be given to ignore instance

/*  ... Inserts a comment before the given entry.  Returns OK or ERROR
*/
int radCfPutCommentBefore (CF_ID file, char *id, char *instance, char *commentText);

/*  ... Checks for a comment before the given entry.  Returns TRUE or FALSE
*/
int radCfIsCommentBefore (CF_ID file, char *id, char *instance, char *commentText);

/*  ... Checks for a comment after the given entry.  Returns TRUE or FALSE
*/
int radCfIsCommentAfter (CF_ID file, char *id, char *instance, char *commentText);

/*  ... Retrieves the first entry for a particular ID.  instance and value
    ... are both filled with the appropriate information when an entry is
    ... found.  Returns OK or ERROR.  
*/
extern int radCfGetFirstEntry (CF_ID file, char *id, char *instance, char *value);

/*  ... Gets the next entry for a particular ID.  instance and value are
    ... both filled with the appropriate information when a new entry is
    ... found.  Returns OK or ERROR.
*/
extern int radCfGetNextEntry (CF_ID file, char *id, char *instance, char *value);

/*  ... Retrieves a particular config file entry specified by the "id" and
    ... "instance" inputs.  Returns OK or ERROR.
*/
extern int radCfGetEntry (CF_ID file, char *id, char *instance, char *value);

/*  ... Creates/Updates a configuration entry in the file
    ... given the id, instance, and value.  New entries will
    ... be placed at the end of the file.  Returns OK or ERROR  
*/
extern int radCfPutEntry (CF_ID file, char *id, char *instance, char *value, char *comment);

#ifdef __cplusplus
}
#endif
#endif

