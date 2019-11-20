#ifndef INC_radsqliteh
#define INC_radsqliteh
#ifdef __cplusplus
extern "C" {
#endif
/*---------------------------------------------------------------------
 
 FILE NAME:
 radsqlite.h
 
 PURPOSE:
 Provide a generic database I/F utility for sqlite.
 
 REVISION HISTORY:
    Date        Programmer  Revision    Function
    07/24/2008  M.S. Teel   0           Original
 
 NOTES:
 The API is partitioned by data "dimension" (in reverse order):
        Field       - a datum with attributes
        Row         - a collection of Fields
        Table       - a collection of Rows
        Database    - a collection of Tables
        
    This API is database engine independent.
    See .../database/<db_engine>/xx_database.c for I/F specifics.
 
 LICENSE:
        Copyright 2001-2008 Mark S. Teel. All rights reserved.

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
#include <sqlite3.h>
#include <radsysdefs.h>
#include <radlist.h>
#include <radmsgLog.h>


#define DB_SQLITE_FIELD_NAME_MAX        64
#define DB_SQLITE_QUERY_LENGTH_MAX      2048
#define SQLITE_MAX_QUERY_TRIES          5


/*  ... define the database control ID;
    ... 'void' to make generic for multiple server plug-ins;
    ... WARNING: the side effect of being 'void' is that the compiler
    ... will not error out if you pass ANY pointer as the SQLITE_DATABASE_ID!
    ... Be careful in all functions requiring SQLITE_DATABASE_ID that it is the
    ... appropriate pointer!
*/
typedef void            *SQLITE_DATABASE_ID;

typedef sqlite3_stmt    *SQLITE_DIRECT_ROW;


/*  ... define the field types
*/
enum SQLiteFieldTypes
{
    SQLITE_FIELD_STRING        = 0x00000001,
    SQLITE_FIELD_BIGINT        = 0x00000002,
    SQLITE_FIELD_DOUBLE        = 0x00000004,
    SQLITE_FIELD_TYPE_CLEAR    = 0xFF000000,       /* used to clear field type only */
    SQLITE_FIELD_VALUE_IS_NULL = 0x80000000,       /* for queries & results only */
    SQLITE_FIELD_DISPLAY       = 0x40000000        /* for queries only */
};

/*  ... only populated for radsqliteTableCreate calls
*/
enum SQLiteFieldTraits
{
    SQLITE_FIELD_PRI_KEY       = 0x00010000,
    SQLITE_FIELD_UNIQUE_INDEX  = 0x00020000,
    SQLITE_FIELD_INDEX         = 0x00040000,
    SQLITE_FIELD_NOT_NULL      = 0x00080000
};


/*  ... define the field control data
*/
typedef struct
{
    NODE        node;
    char        name[DB_SQLITE_FIELD_NAME_MAX];
    UINT        type;           /* DbFieldTypes | DbFieldTraits */
    long long   llvalue;
    double      dvalue;
    char        *cvalue;
    int         cvalLength;
} *SQLITE_FIELD_ID, SQLITE_FIELD;

/*  ... define the row control data
*/
typedef struct
{
    NODE            node;
    RADLIST         fields;         /* list of SQLITE_FIELD_IDs (columns) */
    SQLITE_FIELD_ID mallocBlock;
    
} *SQLITE_ROW_ID;

/*  ... define the result set control data
*/
typedef struct
{
    RADLIST         rows;           /* list of SQLITE_ROW_IDs (rows) */
    SQLITE_ROW_ID   current;
    char            query[DB_SQLITE_QUERY_LENGTH_MAX];
} *SQLITE_RESULT_SET_ID;


/*  ... methods
*/

/************************** Database - Level ******************************
 **************************************************************************
*/
/*  ... connect to a database server;
    ... 'dbFile' should be the full path of the db filename;
    ... returns the ID or NULL
*/
extern SQLITE_DATABASE_ID radsqliteOpen(const char  *dbFile);

/*  ... close a database server connection
*/
extern void radsqliteClose (SQLITE_DATABASE_ID id);

/*  ... issue an SQL query to the db engine;
    ... 'createResults' should be set to TRUE if a result set should
    ... be created for retrieval with the radsqliteGetResults function
    ... described below, otherwise set 'createResults' to FALSE;
    ... if results are generated it can be VERY slow and eat up a lot of 
    ... heap space; the good news is that once the SQLITE_RESULT_SET_ID is 
    ... retrieved, it can persist as long as the user needs to keep it 
    ... without adverse effects on the db server;
    ... returns OK or ERROR if there is a db server error, query error,
    ... or 'createResults' is set to TRUE and no result set is generated 
    ... by the 'query'
*/
extern int radsqliteQuery
(
    SQLITE_DATABASE_ID  id,
    const char          *query,
    int                 createResults
);


/*  ... retrieve the result set (if there is one);
    ... should be called immediately after radsqliteQuery if the
    ... query was supposed to generate a result set (SELECT, SHOW, etc.);
    ... returns NULL if there is no result set available;
    ... SQLITE_RESULT_SET_ID should be released via radsqliteTableResultsRelease
    ... once the user is finished with it;
    ... returns SQLITE_RESULT_SET_ID or NULL
*/
extern SQLITE_RESULT_SET_ID radsqliteGetResults
(
    SQLITE_DATABASE_ID  id
);


/*  ... refresh a result set based on the original query;
    ... can be even SLOWER;
    ... SQLITE_RESULT_SET_ID should be released via radsqliteTableResultsRelease
    ... once the user is finished with it;
    ... returns SQLITE_RESULT_SET_ID or NULL
*/
extern SQLITE_RESULT_SET_ID radsqliteRefreshResults
(
    SQLITE_DATABASE_ID      id,
    SQLITE_RESULT_SET_ID    resSetId
);


/*  ... release a result set
*/
extern void radsqliteReleaseResults
(
    SQLITE_DATABASE_ID      id,
    SQLITE_RESULT_SET_ID    resSet
);


/*************************** Table - Level ********************************
 **************************************************************************
*/

/*  ... does 'tableName' table exist?;
    ... returns TRUE or FALSE
*/
extern int radsqliteTableIfExists
(
    SQLITE_DATABASE_ID  id,
    const char          *tableName
);

/*  ... table management; all return OK or ERROR
*/
extern int radsqliteTableCreate
(
    SQLITE_DATABASE_ID  id,
    const char          *tableName,
    SQLITE_ROW_ID       rowDescription      /* see radsqliteRowDescriptionCreate */
);

extern int radsqliteTableDelete
(
    SQLITE_DATABASE_ID  id,
    const char          *tableName
);

extern int radsqliteTableTruncate
(
    SQLITE_DATABASE_ID  id,
    const char          *tableName
);

/*  ... create a row description to use for row insertion/retrieval;
    ... must be deleted with radsqliteRowDescriptionDelete after use;
    ... sets the SQLITE_FIELD_VALUE_IS_NULL bit in field->type for all fields -
    ... this means the user must clear this bit for field values to be 
    ... used in queries/insertions/deletions;
    ... returns SQLITE_ROW_ID or NULL
*/
extern SQLITE_ROW_ID radsqliteTableDescriptionGet
(
    SQLITE_DATABASE_ID  id,
    const char          *tableName
);

/*  ... query a table to create a result set given a row description;
    ... columns to be included in the result set should have SQLITE_FIELD_DISPLAY
    ... set in the appropriate SQLITE_FIELD_ID.type within 'rowDescription';
    ... columns NOT to be included in the WHERE clause should have
    ... SQLITE_FIELD_VALUE_IS_NULL set in SQLITE_FIELD_ID.type;
    ... columns in the where clause are 'ANDed' together;
    ... can be VERY slow and eat up a lot of heap space; the good news 
    ... is that once the SQLITE_RESULT_SET_ID is retrieved, it can persist as 
    ... long as the user needs to keep it without adverse effects on 
    ... the db server;
    ... SQLITE_RESULT_SET_ID should be released via radsqliteTableResultsRelease
    ... once the user is finished with it;
    ... only the populated fields in 'rowDescription' will be used to 
    ... match records in the table (i.e., multiple rows can be matched)
    ... returns SQLITE_RESULT_SET_ID or NULL
*/
extern SQLITE_RESULT_SET_ID radsqliteTableQueryRow
(
    SQLITE_DATABASE_ID  id,
    const char          *tableName,
    SQLITE_ROW_ID       rowDescription
);

/*  ... insert a row into a table;
    ... 'rowId' was created with radsqliteTableDescriptionGet then
    ... field values were populated with radsqliteFieldGet, 
    ... radsqliteFieldSetIntValue, etc. prior to this call;
    ... if a 'NOT NULL' field has a NULL value in rowId, it's an error;
    ... returns OK or ERROR
*/
extern int radsqliteTableInsertRow
(
    SQLITE_DATABASE_ID  id,
    const char          *tableName,
    SQLITE_ROW_ID       rowId
);

/*  ... modify rows in a table matching 'matchId';
    ... only the non-NULL fields will be used to match records
    ... in the table (i.e., multiple rows can be matched)
    ... all fields in newData will be updated in the db rows - if a NULL
    ... value is given for a NOT NULL field it is an error;
    ... thus if you don't want to modify a column value, remove it from
    ... the newData row desription with radsqliteRowDescriptionRemoveField;
    ... returns OK or ERROR
*/
extern int radsqliteTableModifyRows
(
    SQLITE_DATABASE_ID  id,
    const char          *tableName,
    SQLITE_ROW_ID       matchId,
    SQLITE_ROW_ID       newData
);

/*  ... delete rows in a table matching 'matchId';
    ... only the non-NULL fields will be used to match records
    ... in the table (i.e., multiple rows can be matched)
    ... returns OK or ERROR
*/
extern int radsqliteTableDeleteRows
(
    SQLITE_DATABASE_ID  id,
    const char          *tableName,
    SQLITE_ROW_ID       matchId
);


/* !!!!!!!!!!!!!!!!! END OF DB ENGINE SPECIFIC METHODS !!!!!!!!!!!!!!!!!!!!
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*/

/**************************** Row - Level *********************************
 **************************************************************************
*/
/*  ... traverse the result set row by row
*/
extern SQLITE_ROW_ID radsqliteResultsGetFirst (SQLITE_RESULT_SET_ID id);

extern SQLITE_ROW_ID radsqliteResultsGetNext (SQLITE_RESULT_SET_ID id);

extern SQLITE_ROW_ID radsqliteResultsGetPrev (SQLITE_RESULT_SET_ID id);

extern SQLITE_ROW_ID radsqliteResultsGetLast (SQLITE_RESULT_SET_ID id);

extern int radsqliteResultsGetRowCount (SQLITE_RESULT_SET_ID id);

/*  ... create an empty row description to use when creating a new table;
    ... returns SQLITE_ROW_ID or NULL
*/
extern SQLITE_ROW_ID radsqliteRowDescriptionCreate
(
    void
);

/*  ... returns OK or ERROR
*/
extern int radsqliteRowDescriptionAddField
(
    SQLITE_ROW_ID   id,
    const char      *name,
    UINT            type,
    int             maxLength
);

extern int radsqliteRowDescriptionRemoveField
(
    SQLITE_ROW_ID   id,
    const char      *name
);

extern void radsqliteRowDescriptionDelete
(
    SQLITE_ROW_ID   id
);


/*************************** Field - Level ********************************
 **************************************************************************
*/

/*  ... get the field of interest; returns SQLITE_FIELD_ID or NULL
*/
extern SQLITE_FIELD_ID radsqliteFieldGet
(
    SQLITE_ROW_ID   id,
    const char      *fieldName
);

/*  ... field extractions/insertions; if SQLITE_FIELD_ID is bogus,
    ... these will blow chunks!
*/
extern UINT radsqliteFieldGetType
(
    SQLITE_FIELD_ID id
);

extern long long radsqliteFieldGetBigIntValue
(
    SQLITE_FIELD_ID id
);

extern double radsqliteFieldGetDoubleValue
(
    SQLITE_FIELD_ID id
);

extern char *radsqliteFieldGetCharValue
(
    SQLITE_FIELD_ID id
);

extern int radsqliteFieldGetCharLength
(
    SQLITE_FIELD_ID id
);

/*  ... does not overwrite traits flags
*/
extern int radsqliteFieldSetTypeBigInt
(
    SQLITE_FIELD_ID id
);

extern int radsqliteFieldSetTypeDouble
(
    SQLITE_FIELD_ID id
);

extern int radsqliteFieldSetTypeChar
(
    SQLITE_FIELD_ID id
);

extern int radsqliteFieldSetToDisplay
(
    SQLITE_FIELD_ID id
);

extern int radsqliteFieldSetToNotDisplay
(
    SQLITE_FIELD_ID id
);

extern int radsqliteFieldSetToNull
(
    SQLITE_FIELD_ID id
);

extern int radsqliteFieldSetToNotNull
(
    SQLITE_FIELD_ID id
);

extern int radsqliteFieldSetBigIntValue
(
    SQLITE_FIELD_ID id,
    long long       value
);

extern int radsqliteFieldSetDoubleValue
(
    SQLITE_FIELD_ID id,
    double          value
);

/*  ... sets both value AND length (might re-allocate the string)
*/
extern int radsqliteFieldSetCharValue
(
    SQLITE_FIELD_ID id,
    const char      *value,
    int             valueLength
);


/******************** Direct SQLite Access Methods ************************
 **************************************************************************
*/
/*  ... issue an SQL query to the db engine;
    ... 'createResults' should be set to TRUE if a result set should
    ... be created for retrieval with the radsqliteGetResults function
    ... described below, otherwise set 'createResults' to FALSE;
    ... returns OK or ERROR if there is a db server error, query error,
    ... or 'createResults' is set to TRUE and no result set is generated 
    ... by the 'query'
*/
extern int radsqlitedirectQuery
(
    SQLITE_DATABASE_ID     id,
    const char      *query,
    int             createResults
);


/*  ... retrieve the next result row (if there is one);
    ... should be called immediately after radsqlitedirectQuery if the
    ... query was supposed to generate a result set (SELECT, SHOW, etc.);
    ... returns NULL if there is no result row available;
    ... SQLITE_DIRECT_ROW should be released via radsqliteTableResultsRelease
    ... once the user is finished with it;
    ... returns SQLITE_RESULT_SET_ID or NULL
*/
extern SQLITE_DIRECT_ROW radsqlitedirectGetRow
(
    SQLITE_DATABASE_ID     id
);


/*  ... release direct results
*/
extern void radsqlitedirectReleaseResults
(
    SQLITE_DATABASE_ID             id
);


/*  ... get the direct field of interest; returns SQLITE_FIELD_ID or NULL
*/
extern SQLITE_FIELD_ID radsqlitedirectFieldGet
(
    SQLITE_DIRECT_ROW   id,
    const char          *fieldName
);

/*  ... Issue a pragma command to the database (no results retrieved):
*/
extern int radsqlitePragma 
(
    SQLITE_DATABASE_ID  id,
    const char*         pragmaName,
    const char*         value
);


#ifdef __cplusplus
}
#endif
#endif

