/* A Bison parser, made by GNU Bison 3.4.1.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2019 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

#ifndef YY_YY_SCANNERPARSER_H_INCLUDED
# define YY_YY_SCANNERPARSER_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    IDENTIFIER = 258,
    TYPEDEF_NAME = 259,
    INTEGER = 260,
    FLOATING = 261,
    CHARACTER = 262,
    STRING = 263,
    ELLIPSIS = 264,
    ADDEQ = 265,
    SUBEQ = 266,
    MULEQ = 267,
    DIVEQ = 268,
    MODEQ = 269,
    XOREQ = 270,
    ANDEQ = 271,
    OREQ = 272,
    SL = 273,
    SR = 274,
    SLEQ = 275,
    SREQ = 276,
    EQ = 277,
    NOTEQ = 278,
    LTEQ = 279,
    GTEQ = 280,
    ANDAND = 281,
    OROR = 282,
    PLUSPLUS = 283,
    MINUSMINUS = 284,
    ARROW = 285,
    AUTO = 286,
    BOOL = 287,
    BREAK = 288,
    CASE = 289,
    CHAR = 290,
    CONST = 291,
    CONTINUE = 292,
    DEFAULT = 293,
    DO = 294,
    DOUBLE = 295,
    ELSE = 296,
    ENUM = 297,
    EXTERN = 298,
    FLOAT = 299,
    FOR = 300,
    GOTO = 301,
    IF = 302,
    INLINE = 303,
    INT = 304,
    LONG = 305,
    REGISTER = 306,
    RESTRICT = 307,
    RETURN = 308,
    SHORT = 309,
    SIGNED = 310,
    SIZEOF = 311,
    STATIC = 312,
    STRUCT = 313,
    SWITCH = 314,
    TYPEDEF = 315,
    UNION = 316,
    UNSIGNED = 317,
    VOID = 318,
    VOLATILE = 319,
    WHILE = 320,
    FUNCTION_MACRO = 321,
    OBJECT_MACRO = 322
  };
#endif
/* Tokens.  */
#define IDENTIFIER 258
#define TYPEDEF_NAME 259
#define INTEGER 260
#define FLOATING 261
#define CHARACTER 262
#define STRING 263
#define ELLIPSIS 264
#define ADDEQ 265
#define SUBEQ 266
#define MULEQ 267
#define DIVEQ 268
#define MODEQ 269
#define XOREQ 270
#define ANDEQ 271
#define OREQ 272
#define SL 273
#define SR 274
#define SLEQ 275
#define SREQ 276
#define EQ 277
#define NOTEQ 278
#define LTEQ 279
#define GTEQ 280
#define ANDAND 281
#define OROR 282
#define PLUSPLUS 283
#define MINUSMINUS 284
#define ARROW 285
#define AUTO 286
#define BOOL 287
#define BREAK 288
#define CASE 289
#define CHAR 290
#define CONST 291
#define CONTINUE 292
#define DEFAULT 293
#define DO 294
#define DOUBLE 295
#define ELSE 296
#define ENUM 297
#define EXTERN 298
#define FLOAT 299
#define FOR 300
#define GOTO 301
#define IF 302
#define INLINE 303
#define INT 304
#define LONG 305
#define REGISTER 306
#define RESTRICT 307
#define RETURN 308
#define SHORT 309
#define SIGNED 310
#define SIZEOF 311
#define STATIC 312
#define STRUCT 313
#define SWITCH 314
#define TYPEDEF 315
#define UNION 316
#define UNSIGNED 317
#define VOID 318
#define VOLATILE 319
#define WHILE 320
#define FUNCTION_MACRO 321
#define OBJECT_MACRO 322

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 190 "scannerparser.y"

  char *str;
  GList *list;
  CSymbol *symbol;
  CType *ctype;
  StorageClassSpecifier storage_class_specifier;
  TypeQualifier type_qualifier;
  FunctionSpecifier function_specifier;
  UnaryOperator unary_operator;

#line 202 "scannerparser.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (GIGenerator* igenerator);

#endif /* !YY_YY_SCANNERPARSER_H_INCLUDED  */
