#ifndef INC_raddatabaseh
#define INC_raddatabaseh
#ifdef __cplusplus
extern "C" {
#endif
/*---------------------------------------------------------------------
 
 FILE NAME:
 raddatabase.h
 
 PURPOSE:
 Provide a generic database I/F utility.
 
 REVISION HISTORY:
    Date        Programmer  Revision    Function
    12/30/01    M.S. Teel   0           Original
 
 NOTES:
 The API is partitioned by data "dimension" (in reverse order):
        Field       - a datum with attributes
        Row         - a collection of Fields
        Table       - a collection of Rows
        Database    - a collection of Tables
        
    This API is database engine independent.
    See .../database/<db_engine>/xx_database.c for I/F specifics.
 
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

------------------------------------------------------------------------*/

/*  ... System include files
*/
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <string.h>
#include <signal.h>
#include <ctype.h>

/*  ... Library include files
*/
#include <radsysdefs.h>
#include <radlist.h>


#define DB_FIELD_NAME_MAX       64
#define DB_QUERY_LENGTH_MAX     1024



/*  ... define the database control ID;
    ... 'void' to make generic for multiple server plug-ins;
    ... WARNING: the side effect of being 'void' is that the compiler
    ... will not error out if you pass ANY pointer as the DATABASE_ID!
    ... Be careful in all functions requiring DATABASE_ID that it is the
    ... appropriate pointer!
*/
typedef void    *DATABASE_ID;


/*  ... define the field types
*/
enum DbFieldTypes
{
    FIELD_INT           = 0x00000001,
    FIELD_STRING        = 0x00000002,
    FIELD_BIGINT        = 0x00000004,
    FIELD_FLOAT         = 0x00000008,
    FIELD_DOUBLE        = 0x00000010,
    FIELD_DATETIME      = 0x00000020,
    FIELD_TYPE_CLEAR    = 0xFF000000,       /* used to clear field type only */
    FIELD_VALUE_IS_NULL = 0x80000000,       /* for queries & results only */
    FIELD_DISPLAY       = 0x40000000        /* for queries only */
};

/*  ... only populated for raddatabaseTableCreate calls
*/
enum DbFieldTraits
{
    FIELD_PRI_KEY       = 0x00010000,
    FIELD_UNIQUE_INDEX  = 0x00020000,
    FIELD_INDEX         = 0x00040000,
    FIELD_NOT_NULL      = 0x00080000
};


/*  ... define the field control data
*/
typedef struct
{
    NODE        node;
    char        name[DB_FIELD_NAME_MAX];
    UINT        type;           /* DbFieldTypes | DbFieldTraits */
    int         ivalue;
    long long   llvalue;
    float       fvalue;
    double      dvalue;
    char        *cvalue;
    int         cvalLength;
} *FIELD_ID;

/*  ... define the row control data
*/
typedef struct
{
    NODE        node;
    RADLIST     fields;         /* list of FIELD_IDs (columns) */
} *ROW_ID;

/*  ... define the result set control data
*/
typedef struct
{
    RADLIST     rows;           /* list of ROW_IDs (rows) */
    ROW_ID      current;
    char        query[DB_QUERY_LENGTH_MAX];
} *RESULT_SET_ID;


/*  ... methods
*/

/* !!!!!!!!!!!!!!!!!!!!! DB ENGINE SPECIFIC METHODS !!!!!!!!!!!!!!!!!!!!!!!
   !!!! SEE .../database/<db_engine>/xx_database.c for implementation !!!!!
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*/

/************************** Database - Level ******************************
 **************************************************************************
*/
/*  ... connect to a database server;
    ... if 'hostORfile' is NULL, localhost is used;
    ... if this is a file-based db (sqlite), then 'hostORfile' should be the 
    ... full path of the db filename;
    ... 'username' and 'password' should be populated and are the db 
    ... server username and password (unless a file-based db is selected);
    ... if 'dbName' is NULL, no specific database is opened and all
    ... tableNames in calls below MUST be of the form 'dbName.tableName'
    ... (except in the case of file-based dbs);
    ... returns the ID or NULL
*/
extern DATABASE_ID raddatabaseOpen
(
    const char  *hostORfile,
    const char  *username,
    const char  *password,
    const char  *dbName
);

/*  ... close a database server connection
*/
extern void raddatabaseClose (DATABASE_ID id);


/*  ... create a database on the db server;
    ... returns OK or ERROR
*/
extern int raddatabaseCreate
(
    DATABASE_ID     id,
    const char      *dbName
);


/*  ... destroy a database on the db server;
    ... returns OK or ERROR
*/
extern int raddatabaseDelete
(
    DATABASE_ID     id,
    const char      *dbName
);


/*  ... issue an SQL query to the db engine;
    ... 'createResults' should be set to TRUE if a result set should
    ... be created for retrieval with the raddatabaseTableGetResults function
    ... described below, otherwise set 'createResults' to FALSE;
    ... if results are generated it can be VERY slow and eat up a lot of 
    ... heap space; the good news is that once the RESULT_SET_ID is 
    ... retrieved, it can persist as long as the user needs to keep it 
    ... without adverse effects on the db server;
    ... returns OK or ERROR if there is a db server error, query error,
    ... or 'createResults' is set to TRUE and no result set is generated 
    ... by the 'query'
*/
extern int raddatabaseQuery
(
    DATABASE_ID     id,
    const char      *query,
    int             createResults
);


/*  ... retrieve the result set (if there is one);
    ... should be called immediately after raddatabaseQuery if the
    ... query was supposed to generate a result set (SELECT, SHOW, etc.);
    ... returns NULL if there is no result set available;
    ... RESULT_SET_ID should be released via raddatabaseTableResultsRelease
    ... once the user is finished with it;
    ... returns RESULT_SET_ID or NULL
*/
extern RESULT_SET_ID raddatabaseGetResults
(
    DATABASE_ID     id
);


/*  ... refresh a result set based on the original query;
    ... can be even SLOWER;
    ... RESULT_SET_ID should be released via raddatabaseTableResultsRelease
    ... once the user is finished with it;
    ... returns RESULT_SET_ID or NULL
*/
extern RESULT_SET_ID raddatabaseRefreshResults
(
    DATABASE_ID     id,
    RESULT_SET_ID   resSetId
);


/*  ... release a result set
*/
extern void raddatabaseReleaseResults
(
    DATABASE_ID     id,
    RESULT_SET_ID   resSet
);


/*************************** Table - Level ********************************
 **************************************************************************
*/

/*  ... does 'tableName' table exist?;
    ... returns TRUE or FALSE
*/
extern int raddatabaseTableIfExists
(
    DATABASE_ID     id,
    const char      *tableName
);

/*  ... table management; all return OK or ERROR
*/
extern int raddatabaseTableCreate
(
    DATABASE_ID     id,
    const char      *tableName,
    ROW_ID          rowDescription      /* see raddatabaseRowDescriptionCreate */
);

extern int raddatabaseTableDelete
(
    DATABASE_ID     id,
    const char      *tableName
);

extern int raddatabaseTableTruncate
(
    DATABASE_ID     id,
    const char      *tableName
);

/*  ... create a row description to use for row insertion/retrieval;
    ... must be deleted with raddatabaseRowDescriptionDelete after use;
    ... sets the FIELD_VALUE_IS_NULL bit in field->type for all fields -
    ... this means the user must clear this bit for field values to be 
    ... used in queries/insertions/deletions;
    ... returns ROW_ID or NULL
*/
extern ROW_ID raddatabaseTableDescriptionGet
(
    DATABASE_ID     id,
    const char      *tableName
);

/*  ... query a table to create a result set given a row description;
    ... columns to be included in the result set should have FIELD_DISPLAY
    ... set in the appropriate FIELD_ID.type within 'rowDescription';
    ... columns NOT to be included in the WHERE clause should have
    ... FIELD_VALUE_IS_NULL set in FIELD_ID.type;
    ... columns in the where clause are 'ANDed' together;
    ... can be VERY slow and eat up a lot of heap space; the good news 
    ... is that once the RESULT_SET_ID is retrieved, it can persist as 
    ... long as the user needs to keep it without adverse effects on 
    ... the db server;
    ... RESULT_SET_ID should be released via raddatabaseTableResultsRelease
    ... once the user is finished with it;
    ... only the populated fields in 'rowDescription' will be used to 
    ... match records in the table (i.e., multiple rows can be matched)
    ... returns RESULT_SET_ID or NULL
*/
extern RESULT_SET_ID raddatabaseTableQueryRow
(
    DATABASE_ID     id,
    const char      *tableName,
    ROW_ID          rowDescription
);

/*  ... insert a row into a table;
    ... 'rowId' was created with raddatabaseTableDescriptionGet then
    ... field values were populated with raddatabaseFieldGet, 
    ... raddatabaseFieldSetIntValue, etc. prior to this call;
    ... if a 'NOT NULL' field has a NULL value in rowId, it's an error;
    ... returns OK or ERROR
*/
extern int raddatabaseTableInsertRow
(
    DATABASE_ID     id,
    const char      *tableName,
    ROW_ID          rowId
);

/*  ... modify rows in a table matching 'matchId';
    ... only the non-NULL fields will be used to match records
    ... in the table (i.e., multiple rows can be matched)
    ... all fields in newData will be updated in the db rows - if a NULL
    ... value is given for a NOT NULL field it is an error;
    ... thus if you don't want to modify a column value, remove it from
    ... the newData row desription with raddatabaseRowDescriptionRemoveField;
    ... returns OK or ERROR
*/
extern int raddatabaseTableModifyRows
(
    DATABASE_ID     id,
    const char      *tableName,
    ROW_ID          matchId,
    ROW_ID          newData
);

/*  ... delete rows in a table matching 'matchId';
    ... only the non-NULL fields will be used to match records
    ... in the table (i.e., multiple rows can be matched)
    ... returns OK or ERROR
*/
extern int raddatabaseTableDeleteRows
(
    DATABASE_ID     id,
    const char      *tableName,
    ROW_ID          matchId
);


/* !!!!!!!!!!!!!!!!! END OF DB ENGINE SPECIFIC METHODS !!!!!!!!!!!!!!!!!!!!
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*/

/**************************** Row - Level *********************************
 **************************************************************************
*/
/*  ... traverse the result set row by row
*/
extern ROW_ID raddatabaseResultsGetFirst (RESULT_SET_ID id);

extern ROW_ID raddatabaseResultsGetNext (RESULT_SET_ID id);

extern ROW_ID raddatabaseResultsGetPrev (RESULT_SET_ID id);

extern ROW_ID raddatabaseResultsGetLast (RESULT_SET_ID id);

/*  ... create an empty row description to use when creating a new table;
    ... returns ROW_ID or NULL
*/
extern ROW_ID raddatabaseRowDescriptionCreate
(
    void
);

/*  ... returns OK or ERROR
*/
extern int raddatabaseRowDescriptionAddField
(
    ROW_ID          id,
    const char      *name,
    UINT            type,
    int             maxLength
);

extern int raddatabaseRowDescriptionRemoveField
(
    ROW_ID          id,
    const char      *name
);

extern void raddatabaseRowDescriptionDelete
(
    ROW_ID          id
);


/*************************** Field - Level ********************************
 **************************************************************************
*/

/*  ... get the field of interest; returns FIELD_ID or NULL
*/
extern FIELD_ID raddatabaseFieldGet
(
    ROW_ID          id,
    const char      *fieldName
);

/*  ... field extractions/insertions; if FIELD_ID is bogus,
    ... these will blow chunks!
*/
extern UINT raddatabaseFieldGetType
(
    FIELD_ID        id
);

extern int raddatabaseFieldGetIntValue
(
    FIELD_ID        id
);

extern long long raddatabaseFieldGetBigIntValue
(
    FIELD_ID          id
);

extern float raddatabaseFieldGetFloatValue
(
    FIELD_ID          id
);

extern double raddatabaseFieldGetDoubleValue
(
    FIELD_ID          id
);

extern char *raddatabaseFieldGetTimeDateValue
(
    FIELD_ID          id
);

extern char *raddatabaseFieldGetCharValue
(
    FIELD_ID        id
);

extern int raddatabaseFieldGetCharLength
(
    FIELD_ID        id
);

/*  ... does not overwrite traits flags
*/
extern int raddatabaseFieldSetTypeInt
(
    FIELD_ID        id
);

extern int raddatabaseFieldSetTypeBigInt
(
    FIELD_ID        id
);

extern int raddatabaseFieldSetTypeFloat
(
    FIELD_ID        id
);

extern int raddatabaseFieldSetTypeDouble
(
    FIELD_ID        id
);

extern int raddatabaseFieldSetTypeDateTime
(
    FIELD_ID        id
);

extern int raddatabaseFieldSetTypeChar
(
    FIELD_ID        id
);

extern int raddatabaseFieldSetToNull
(
    FIELD_ID        id
);

extern int raddatabaseFieldSetToNotNull
(
    FIELD_ID        id
);

extern int raddatabaseFieldSetIntValue
(
    FIELD_ID        id,
    int             value
);

extern int raddatabaseFieldSetBigIntValue
(
    FIELD_ID        id,
    long long       value
);

extern int raddatabaseFieldSetFloatValue
(
    FIELD_ID        id,
    float           value
);

extern int raddatabaseFieldSetDoubleValue
(
    FIELD_ID        id,
    double          value
);

/*  ... sets both value AND length (might re-allocate the string)
*/
extern int raddatabaseFieldSetDateTimeValue
(
    FIELD_ID        id,
    const char      *value,
    int             valueLength
);

/*  ... sets both value AND length (might re-allocate the string)
*/
extern int raddatabaseFieldSetCharValue
(
    FIELD_ID        id,
    const char      *value,
    int             valueLength
);

#ifdef __cplusplus
}
#endif
#endif
