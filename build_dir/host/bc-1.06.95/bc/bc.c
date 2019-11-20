/* A Bison parser, made by GNU Bison 2.1.  */

/* Skeleton parser for Yacc-like parsing with Bison,
   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* Written by Richard Stallman by simplifying the original so called
   ``semantic'' parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     ENDOFLINE = 258,
     AND = 259,
     OR = 260,
     NOT = 261,
     STRING = 262,
     NAME = 263,
     NUMBER = 264,
     ASSIGN_OP = 265,
     REL_OP = 266,
     INCR_DECR = 267,
     Define = 268,
     Break = 269,
     Quit = 270,
     Length = 271,
     Return = 272,
     For = 273,
     If = 274,
     While = 275,
     Sqrt = 276,
     Else = 277,
     Scale = 278,
     Ibase = 279,
     Obase = 280,
     Auto = 281,
     Read = 282,
     Random = 283,
     Warranty = 284,
     Halt = 285,
     Last = 286,
     Continue = 287,
     Print = 288,
     Limits = 289,
     UNARY_MINUS = 290,
     HistoryVar = 291,
     Void = 292
   };
#endif
/* Tokens.  */
#define ENDOFLINE 258
#define AND 259
#define OR 260
#define NOT 261
#define STRING 262
#define NAME 263
#define NUMBER 264
#define ASSIGN_OP 265
#define REL_OP 266
#define INCR_DECR 267
#define Define 268
#define Break 269
#define Quit 270
#define Length 271
#define Return 272
#define For 273
#define If 274
#define While 275
#define Sqrt 276
#define Else 277
#define Scale 278
#define Ibase 279
#define Obase 280
#define Auto 281
#define Read 282
#define Random 283
#define Warranty 284
#define Halt 285
#define Last 286
#define Continue 287
#define Print 288
#define Limits 289
#define UNARY_MINUS 290
#define HistoryVar 291
#define Void 292




/* Copy the first part of user declarations.  */
#line 33 "bc.y"


#include "bcdefs.h"
#include "global.h"
#include "proto.h"

/* current function number. */
int cur_func = -1;

/* Expression encoded information -- See comment at expression rules. */
#define EX_ASSGN 0 
#define EX_REG   1
#define EX_COMP  2
#define EX_PAREN 4
#define EX_VOID  8 
#define EX_EMPTY 16



/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

#if ! defined (YYSTYPE) && ! defined (YYSTYPE_IS_DECLARED)
#line 54 "bc.y"
typedef union YYSTYPE {
	char	 *s_value;
	char	  c_value;
	int	  i_value;
	arg_list *a_value;
       } YYSTYPE;
/* Line 196 of yacc.c.  */
#line 185 "bc.c"
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 219 of yacc.c.  */
#line 197 "bc.c"

#if ! defined (YYSIZE_T) && defined (__SIZE_TYPE__)
# define YYSIZE_T __SIZE_TYPE__
#endif
#if ! defined (YYSIZE_T) && defined (size_t)
# define YYSIZE_T size_t
#endif
#if ! defined (YYSIZE_T) && (defined (__STDC__) || defined (__cplusplus))
# include <stddef.h> /* INFRINGES ON USER NAME SPACE */
# define YYSIZE_T size_t
#endif
#if ! defined (YYSIZE_T)
# define YYSIZE_T unsigned int
#endif

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

#if ! defined (yyoverflow) || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if defined (__STDC__) || defined (__cplusplus)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     define YYINCLUDED_STDLIB_H
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning. */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2005 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM ((YYSIZE_T) -1)
#  endif
#  ifdef __cplusplus
extern "C" {
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if (! defined (malloc) && ! defined (YYINCLUDED_STDLIB_H) \
	&& (defined (__STDC__) || defined (__cplusplus)))
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if (! defined (free) && ! defined (YYINCLUDED_STDLIB_H) \
	&& (defined (__STDC__) || defined (__cplusplus)))
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifdef __cplusplus
}
#  endif
# endif
#endif /* ! defined (yyoverflow) || YYERROR_VERBOSE */


#if (! defined (yyoverflow) \
     && (! defined (__cplusplus) \
	 || (defined (YYSTYPE_IS_TRIVIAL) && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  short int yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (short int) + sizeof (YYSTYPE))			\
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined (__GNUC__) && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (0)
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (0)

#endif

#if defined (__STDC__) || defined (__cplusplus)
   typedef signed char yysigned_char;
#else
   typedef short int yysigned_char;
#endif

/* YYFINAL -- State number of the termination state. */
#define YYFINAL  2
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   719

/* YYNTOKENS -- Number of terminals. */
#define YYNTOKENS  53
/* YYNNTS -- Number of nonterminals. */
#define YYNNTS  36
/* YYNRULES -- Number of rules. */
#define YYNRULES  112
/* YYNRULES -- Number of states. */
#define YYNSTATES  198

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   292

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const unsigned char yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,    42,    52,     2,
      45,    46,    40,    38,    49,    39,     2,    41,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,    44,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    50,     2,    51,    43,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    47,     2,    48,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const unsigned short int yyprhs[] =
{
       0,     0,     3,     4,     7,    10,    12,    15,    16,    18,
      19,    21,    25,    28,    29,    31,    34,    38,    41,    45,
      47,    50,    52,    54,    56,    58,    60,    62,    64,    66,
      69,    70,    71,    72,    73,    88,    89,    98,    99,   100,
     109,   113,   114,   118,   120,   124,   126,   128,   129,   130,
     135,   136,   150,   151,   153,   154,   156,   157,   161,   165,
     167,   171,   176,   181,   185,   191,   198,   205,   206,   208,
     210,   214,   218,   224,   225,   227,   228,   230,   231,   236,
     237,   242,   243,   248,   251,   255,   259,   263,   267,   271,
     275,   279,   282,   284,   286,   290,   295,   298,   301,   306,
     311,   316,   320,   324,   326,   331,   333,   335,   337,   339,
     341,   342,   344
};

/* YYRHS -- A `-1'-separated list of the rules' RHS. */
static const yysigned_char yyrhs[] =
{
      54,     0,    -1,    -1,    54,    55,    -1,    57,     3,    -1,
      73,    -1,     1,     3,    -1,    -1,     3,    -1,    -1,    59,
      -1,    57,    44,    59,    -1,    57,    44,    -1,    -1,    59,
      -1,    58,     3,    -1,    58,     3,    59,    -1,    58,    44,
      -1,    58,    44,    60,    -1,    60,    -1,     1,    60,    -1,
      29,    -1,    34,    -1,    83,    -1,     7,    -1,    14,    -1,
      32,    -1,    15,    -1,    30,    -1,    17,    82,    -1,    -1,
      -1,    -1,    -1,    18,    61,    45,    81,    44,    62,    81,
      44,    63,    81,    46,    64,    56,    60,    -1,    -1,    19,
      45,    83,    46,    65,    56,    60,    71,    -1,    -1,    -1,
      20,    66,    45,    83,    67,    46,    56,    60,    -1,    47,
      58,    48,    -1,    -1,    33,    68,    69,    -1,    70,    -1,
      70,    49,    69,    -1,     7,    -1,    83,    -1,    -1,    -1,
      22,    72,    56,    60,    -1,    -1,    13,    75,     8,    45,
      76,    46,    56,    47,    88,    77,    74,    58,    48,    -1,
      -1,    37,    -1,    -1,    78,    -1,    -1,    26,    78,     3,
      -1,    26,    78,    44,    -1,     8,    -1,     8,    50,    51,
      -1,    40,     8,    50,    51,    -1,    52,     8,    50,    51,
      -1,    78,    49,     8,    -1,    78,    49,     8,    50,    51,
      -1,    78,    49,    40,     8,    50,    51,    -1,    78,    49,
      52,     8,    50,    51,    -1,    -1,    80,    -1,    83,    -1,
       8,    50,    51,    -1,    80,    49,    83,    -1,    80,    49,
       8,    50,    51,    -1,    -1,    83,    -1,    -1,    83,    -1,
      -1,    87,    10,    84,    83,    -1,    -1,    83,     4,    85,
      83,    -1,    -1,    83,     5,    86,    83,    -1,     6,    83,
      -1,    83,    11,    83,    -1,    83,    38,    83,    -1,    83,
      39,    83,    -1,    83,    40,    83,    -1,    83,    41,    83,
      -1,    83,    42,    83,    -1,    83,    43,    83,    -1,    39,
      83,    -1,    87,    -1,     9,    -1,    45,    83,    46,    -1,
       8,    45,    79,    46,    -1,    12,    87,    -1,    87,    12,
      -1,    16,    45,    83,    46,    -1,    21,    45,    83,    46,
      -1,    23,    45,    83,    46,    -1,    27,    45,    46,    -1,
      28,    45,    46,    -1,     8,    -1,     8,    50,    83,    51,
      -1,    24,    -1,    25,    -1,    23,    -1,    36,    -1,    31,
      -1,    -1,     3,    -1,    88,     3,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const unsigned short int yyrline[] =
{
       0,   124,   124,   132,   134,   136,   138,   144,   145,   149,
     150,   151,   152,   155,   156,   157,   158,   159,   160,   162,
     163,   166,   168,   170,   179,   186,   196,   207,   209,   211,
     214,   219,   231,   244,   213,   264,   263,   279,   285,   278,
     299,   302,   301,   305,   306,   308,   314,   321,   323,   322,
     334,   332,   359,   360,   367,   368,   371,   372,   374,   377,
     379,   381,   385,   389,   391,   393,   397,   403,   404,   406,
     414,   420,   428,   446,   450,   453,   459,   474,   473,   503,
     502,   518,   517,   535,   543,   573,   580,   587,   594,   601,
     608,   615,   622,   631,   647,   653,   672,   691,   714,   721,
     728,   735,   741,   748,   750,   758,   760,   762,   764,   768,
     775,   776,   777
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals. */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "ENDOFLINE", "AND", "OR", "NOT",
  "STRING", "NAME", "NUMBER", "ASSIGN_OP", "REL_OP", "INCR_DECR", "Define",
  "Break", "Quit", "Length", "Return", "For", "If", "While", "Sqrt",
  "Else", "Scale", "Ibase", "Obase", "Auto", "Read", "Random", "Warranty",
  "Halt", "Last", "Continue", "Print", "Limits", "UNARY_MINUS",
  "HistoryVar", "Void", "'+'", "'-'", "'*'", "'/'", "'%'", "'^'", "';'",
  "'('", "')'", "'{'", "'}'", "','", "'['", "']'", "'&'", "$accept",
  "program", "input_item", "opt_newline", "semicolon_list",
  "statement_list", "statement_or_error", "statement", "@1", "@2", "@3",
  "@4", "@5", "@6", "@7", "@8", "print_list", "print_element", "opt_else",
  "@9", "function", "@10", "opt_void", "opt_parameter_list",
  "opt_auto_define_list", "define_list", "opt_argument_list",
  "argument_list", "opt_expression", "return_expression", "expression",
  "@11", "@12", "@13", "named_expression", "required_eol", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const unsigned short int yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,    43,    45,
      42,    47,    37,    94,    59,    40,    41,   123,   125,    44,
      91,    93,    38
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const unsigned char yyr1[] =
{
       0,    53,    54,    54,    55,    55,    55,    56,    56,    57,
      57,    57,    57,    58,    58,    58,    58,    58,    58,    59,
      59,    60,    60,    60,    60,    60,    60,    60,    60,    60,
      61,    62,    63,    64,    60,    65,    60,    66,    67,    60,
      60,    68,    60,    69,    69,    70,    70,    71,    72,    71,
      74,    73,    75,    75,    76,    76,    77,    77,    77,    78,
      78,    78,    78,    78,    78,    78,    78,    79,    79,    80,
      80,    80,    80,    81,    81,    82,    82,    84,    83,    85,
      83,    86,    83,    83,    83,    83,    83,    83,    83,    83,
      83,    83,    83,    83,    83,    83,    83,    83,    83,    83,
      83,    83,    83,    87,    87,    87,    87,    87,    87,    87,
      88,    88,    88
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const unsigned char yyr2[] =
{
       0,     2,     0,     2,     2,     1,     2,     0,     1,     0,
       1,     3,     2,     0,     1,     2,     3,     2,     3,     1,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     2,
       0,     0,     0,     0,    14,     0,     8,     0,     0,     8,
       3,     0,     3,     1,     3,     1,     1,     0,     0,     4,
       0,    13,     0,     1,     0,     1,     0,     3,     3,     1,
       3,     4,     4,     3,     5,     6,     6,     0,     1,     1,
       3,     3,     5,     0,     1,     0,     1,     0,     4,     0,
       4,     0,     4,     2,     3,     3,     3,     3,     3,     3,
       3,     2,     1,     1,     3,     4,     2,     2,     4,     4,
       4,     3,     3,     1,     4,     1,     1,     1,     1,     1,
       0,     1,     2
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const unsigned char yydefact[] =
{
       2,     0,     1,     0,     0,    24,   103,    93,     0,    52,
      25,    27,     0,    75,    30,     0,    37,     0,   107,   105,
     106,     0,     0,    21,    28,   109,    26,    41,    22,   108,
       0,     0,     0,     3,     0,    10,    19,     5,    23,    92,
       6,    20,    83,    67,     0,   103,   107,    96,    53,     0,
       0,    29,    76,     0,     0,     0,     0,     0,     0,     0,
       0,    91,     0,     0,     0,    14,     4,     0,    79,    81,
       0,     0,     0,     0,     0,     0,     0,    77,    97,   103,
       0,    68,    69,     0,     0,     0,    73,     0,     0,     0,
       0,   101,   102,    45,    42,    43,    46,    94,     0,    17,
      40,    11,     0,     0,    84,    85,    86,    87,    88,    89,
      90,     0,     0,    95,     0,   104,    54,    98,     0,    74,
      35,    38,    99,   100,     0,    16,    18,    80,    82,    78,
      70,   103,    71,    59,     0,     0,     0,    55,    31,     7,
       0,    44,     0,     0,     0,     0,     7,     0,    73,     8,
       0,     7,    72,    60,     0,     0,     0,    63,     0,     0,
       0,    47,     0,    61,    62,   110,     0,     0,     0,    32,
      48,    36,    39,   111,    56,    64,     0,     0,    73,     7,
     112,     0,    50,    65,    66,     0,     0,     0,     0,    33,
      49,    57,    58,     0,     7,    51,     0,    34
};

/* YYDEFGOTO[NTERM-NUM]. */
static const short int yydefgoto[] =
{
      -1,     1,    33,   150,    34,    64,    65,    36,    53,   148,
     178,   194,   139,    55,   140,    60,    94,    95,   171,   179,
      37,   188,    49,   136,   182,   137,    80,    81,   118,    51,
      38,   111,   102,   103,    39,   174
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -144
static const short int yypact[] =
{
    -144,   188,  -144,   392,   595,  -144,   -36,  -144,   484,   -31,
    -144,  -144,   -32,   595,  -144,   -11,  -144,   -10,    -7,  -144,
    -144,    -6,    -5,  -144,  -144,  -144,  -144,  -144,  -144,  -144,
     595,   595,   222,  -144,     2,  -144,  -144,  -144,   676,    14,
    -144,  -144,   131,   621,   595,   -27,  -144,  -144,  -144,    54,
     595,  -144,   676,    20,   595,    21,   595,   595,    13,    37,
     569,  -144,   425,   535,     1,  -144,  -144,   318,  -144,  -144,
     595,   595,   595,   595,   595,   595,   595,  -144,  -144,   -23,
      41,    36,   676,    39,    43,   436,   595,   445,   595,   485,
     494,  -144,  -144,  -144,  -144,    40,   676,  -144,   270,   535,
    -144,  -144,   595,   595,   -22,    51,    51,     4,     4,     4,
       4,   595,   105,  -144,   647,  -144,    23,  -144,    53,   676,
    -144,   676,  -144,  -144,   569,  -144,  -144,   131,   123,   -22,
    -144,   -20,   676,    45,    91,    94,    57,    55,  -144,   102,
      60,  -144,   352,    56,    58,    65,   102,    24,   595,  -144,
     535,   102,  -144,  -144,    67,    68,    69,    70,   115,   116,
      81,   109,   535,  -144,  -144,   132,    86,    88,    89,  -144,
    -144,  -144,  -144,  -144,     7,  -144,    92,    97,   595,   102,
    -144,    23,  -144,  -144,  -144,    99,   535,    12,   222,  -144,
    -144,  -144,  -144,     9,   102,  -144,   535,  -144
};

/* YYPGOTO[NTERM-NUM].  */
static const short int yypgoto[] =
{
    -144,  -144,  -144,  -143,  -144,   -39,     0,    -3,  -144,  -144,
    -144,  -144,  -144,  -144,  -144,  -144,    27,  -144,  -144,  -144,
    -144,  -144,  -144,  -144,  -144,   -29,  -144,  -144,  -141,  -144,
      -2,  -144,  -144,  -144,   145,  -144
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -16
static const short int yytable[] =
{
      41,    35,    42,   156,    98,    66,    48,   160,   162,    43,
     180,    52,    98,    50,    44,   191,    71,    72,    73,    74,
      75,    76,    43,    44,    77,    43,    78,   112,    61,    62,
     142,   133,   157,   181,    54,    56,   186,   185,    57,    58,
      59,    82,    83,    68,    69,    99,    67,    76,    85,   100,
      70,   196,    87,    99,    89,    90,   192,   195,    96,    91,
      41,   147,    84,   134,   158,    86,    88,   101,   104,   105,
     106,   107,   108,   109,   110,   135,   159,    71,    72,    73,
      74,    75,    76,    92,   119,   114,   121,   113,   116,   124,
     115,    73,    74,    75,    76,   143,   126,   138,   125,   144,
     127,   128,   145,   146,   147,   149,   151,   153,   154,   129,
      83,     4,   132,     6,     7,   155,   165,     8,   163,   164,
     166,    12,    96,   167,   168,   169,    17,    68,    18,    19,
      20,   170,    21,    22,    70,   173,    25,   175,   176,   177,
      83,    29,    70,   183,    30,   189,   119,   161,   184,   193,
      31,   141,   187,    47,     0,     0,   130,     0,     0,   172,
       0,    71,    72,    73,    74,    75,    76,     0,     0,    71,
      72,    73,    74,    75,    76,     0,   119,     0,     0,     0,
       0,     0,     0,   190,     0,     0,     0,     0,     2,     3,
       0,    -9,     0,   197,     4,     5,     6,     7,     0,     0,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
       0,    18,    19,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    63,    29,   -13,     0,    30,     4,     5,
       6,     7,    -9,    31,     8,    32,    10,    11,    12,    13,
      14,    15,    16,    17,     0,    18,    19,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,     0,    29,     0,
       0,    30,     0,     0,     0,     0,   -13,    31,     0,    32,
     -13,    63,     0,   -15,     0,     0,     4,     5,     6,     7,
       0,     0,     8,     0,    10,    11,    12,    13,    14,    15,
      16,    17,     0,    18,    19,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,     0,    29,     0,     0,    30,
       0,     0,     0,     0,   -15,    31,     0,    32,   -15,    63,
       0,   -12,     0,     0,     4,     5,     6,     7,     0,     0,
       8,     0,    10,    11,    12,    13,    14,    15,    16,    17,
       0,    18,    19,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,     0,    29,     0,     0,    30,     4,     0,
       6,     7,   -12,    31,     8,    32,     0,     0,    12,     0,
       0,     0,     0,    17,     0,    18,    19,    20,     0,    21,
      22,     0,     0,    25,     0,     0,     0,     0,    29,     0,
       0,    30,     0,     0,     0,    40,     0,    31,     4,     5,
       6,     7,     0,   152,     8,     0,    10,    11,    12,    13,
      14,    15,    16,    17,     0,    18,    19,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,     0,    29,    68,
      69,    30,     0,     0,     0,     0,    70,    31,     0,    32,
      68,    69,     0,     0,     0,     0,     0,    70,     0,    68,
      69,     0,     0,     0,     0,     0,    70,     0,     0,     0,
       0,     0,     0,    71,    72,    73,    74,    75,    76,     0,
       0,    97,     0,     0,    71,    72,    73,    74,    75,    76,
       0,     0,   117,    71,    72,    73,    74,    75,    76,    68,
      69,   120,    45,     0,     0,     0,    70,     0,    68,    69,
       0,     0,     0,     0,     0,    70,     0,    46,    19,    20,
       0,     0,     0,     0,     0,    25,     0,     0,     0,     0,
      29,     0,     0,    71,    72,    73,    74,    75,    76,     0,
       0,   122,    71,    72,    73,    74,    75,    76,     0,     0,
     123,     4,     5,     6,     7,     0,     0,     8,     0,    10,
      11,    12,    13,    14,    15,    16,    17,     0,    18,    19,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
       0,    29,     0,     0,    30,     4,    93,     6,     7,     0,
      31,     8,    32,     0,     0,    12,     0,     0,     0,     0,
      17,     0,    18,    19,    20,     0,    21,    22,     0,     0,
      25,     4,     0,     6,     7,    29,     0,     8,    30,     0,
       0,    12,     0,     0,    31,     0,    17,     0,    18,    19,
      20,     0,    21,    22,     0,     0,    25,     4,     0,    79,
       7,    29,     0,     8,    30,     0,     0,    12,     0,     0,
      31,     0,    17,     0,    18,    19,    20,     0,    21,    22,
       0,     0,    25,     4,     0,   131,     7,    29,     0,     8,
      30,     0,     0,    12,     0,     0,    31,     0,    17,     0,
      18,    19,    20,     0,    21,    22,     0,     0,    25,     0,
      68,    69,     0,    29,     0,     0,    30,    70,     0,     0,
       0,     0,    31,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    71,    72,    73,    74,    75,    76
};

static const short int yycheck[] =
{
       3,     1,     4,   146,     3,     3,    37,   148,   151,    45,
       3,    13,     3,    45,    50,     3,    38,    39,    40,    41,
      42,    43,    45,    50,    10,    45,    12,    50,    30,    31,
      50,     8,     8,    26,    45,    45,   179,   178,    45,    45,
      45,    43,    44,     4,     5,    44,    44,    43,    50,    48,
      11,   194,    54,    44,    56,    57,    44,    48,    60,    46,
      63,    49,     8,    40,    40,    45,    45,    67,    70,    71,
      72,    73,    74,    75,    76,    52,    52,    38,    39,    40,
      41,    42,    43,    46,    86,    49,    88,    46,    45,    49,
      51,    40,    41,    42,    43,    50,    99,    44,    98,     8,
     102,   103,     8,    46,    49,     3,    46,    51,    50,   111,
     112,     6,   114,     8,     9,    50,    47,    12,    51,    51,
      50,    16,   124,     8,     8,    44,    21,     4,    23,    24,
      25,    22,    27,    28,    11,     3,    31,    51,    50,    50,
     142,    36,    11,    51,    39,    46,   148,   150,    51,   188,
      45,   124,   181,     8,    -1,    -1,    51,    -1,    -1,   162,
      -1,    38,    39,    40,    41,    42,    43,    -1,    -1,    38,
      39,    40,    41,    42,    43,    -1,   178,    -1,    -1,    -1,
      -1,    -1,    -1,   186,    -1,    -1,    -1,    -1,     0,     1,
      -1,     3,    -1,   196,     6,     7,     8,     9,    -1,    -1,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      -1,    23,    24,    25,    -1,    27,    28,    29,    30,    31,
      32,    33,    34,     1,    36,     3,    -1,    39,     6,     7,
       8,     9,    44,    45,    12,    47,    14,    15,    16,    17,
      18,    19,    20,    21,    -1,    23,    24,    25,    -1,    27,
      28,    29,    30,    31,    32,    33,    34,    -1,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    44,    45,    -1,    47,
      48,     1,    -1,     3,    -1,    -1,     6,     7,     8,     9,
      -1,    -1,    12,    -1,    14,    15,    16,    17,    18,    19,
      20,    21,    -1,    23,    24,    25,    -1,    27,    28,    29,
      30,    31,    32,    33,    34,    -1,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    44,    45,    -1,    47,    48,     1,
      -1,     3,    -1,    -1,     6,     7,     8,     9,    -1,    -1,
      12,    -1,    14,    15,    16,    17,    18,    19,    20,    21,
      -1,    23,    24,    25,    -1,    27,    28,    29,    30,    31,
      32,    33,    34,    -1,    36,    -1,    -1,    39,     6,    -1,
       8,     9,    44,    45,    12,    47,    -1,    -1,    16,    -1,
      -1,    -1,    -1,    21,    -1,    23,    24,    25,    -1,    27,
      28,    -1,    -1,    31,    -1,    -1,    -1,    -1,    36,    -1,
      -1,    39,    -1,    -1,    -1,     3,    -1,    45,     6,     7,
       8,     9,    -1,    51,    12,    -1,    14,    15,    16,    17,
      18,    19,    20,    21,    -1,    23,    24,    25,    -1,    27,
      28,    29,    30,    31,    32,    33,    34,    -1,    36,     4,
       5,    39,    -1,    -1,    -1,    -1,    11,    45,    -1,    47,
       4,     5,    -1,    -1,    -1,    -1,    -1,    11,    -1,     4,
       5,    -1,    -1,    -1,    -1,    -1,    11,    -1,    -1,    -1,
      -1,    -1,    -1,    38,    39,    40,    41,    42,    43,    -1,
      -1,    46,    -1,    -1,    38,    39,    40,    41,    42,    43,
      -1,    -1,    46,    38,    39,    40,    41,    42,    43,     4,
       5,    46,     8,    -1,    -1,    -1,    11,    -1,     4,     5,
      -1,    -1,    -1,    -1,    -1,    11,    -1,    23,    24,    25,
      -1,    -1,    -1,    -1,    -1,    31,    -1,    -1,    -1,    -1,
      36,    -1,    -1,    38,    39,    40,    41,    42,    43,    -1,
      -1,    46,    38,    39,    40,    41,    42,    43,    -1,    -1,
      46,     6,     7,     8,     9,    -1,    -1,    12,    -1,    14,
      15,    16,    17,    18,    19,    20,    21,    -1,    23,    24,
      25,    -1,    27,    28,    29,    30,    31,    32,    33,    34,
      -1,    36,    -1,    -1,    39,     6,     7,     8,     9,    -1,
      45,    12,    47,    -1,    -1,    16,    -1,    -1,    -1,    -1,
      21,    -1,    23,    24,    25,    -1,    27,    28,    -1,    -1,
      31,     6,    -1,     8,     9,    36,    -1,    12,    39,    -1,
      -1,    16,    -1,    -1,    45,    -1,    21,    -1,    23,    24,
      25,    -1,    27,    28,    -1,    -1,    31,     6,    -1,     8,
       9,    36,    -1,    12,    39,    -1,    -1,    16,    -1,    -1,
      45,    -1,    21,    -1,    23,    24,    25,    -1,    27,    28,
      -1,    -1,    31,     6,    -1,     8,     9,    36,    -1,    12,
      39,    -1,    -1,    16,    -1,    -1,    45,    -1,    21,    -1,
      23,    24,    25,    -1,    27,    28,    -1,    -1,    31,    -1,
       4,     5,    -1,    36,    -1,    -1,    39,    11,    -1,    -1,
      -1,    -1,    45,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    38,    39,    40,    41,    42,    43
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const unsigned char yystos[] =
{
       0,    54,     0,     1,     6,     7,     8,     9,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    23,    24,
      25,    27,    28,    29,    30,    31,    32,    33,    34,    36,
      39,    45,    47,    55,    57,    59,    60,    73,    83,    87,
       3,    60,    83,    45,    50,     8,    23,    87,    37,    75,
      45,    82,    83,    61,    45,    66,    45,    45,    45,    45,
      68,    83,    83,     1,    58,    59,     3,    44,     4,     5,
      11,    38,    39,    40,    41,    42,    43,    10,    12,     8,
      79,    80,    83,    83,     8,    83,    45,    83,    45,    83,
      83,    46,    46,     7,    69,    70,    83,    46,     3,    44,
      48,    59,    85,    86,    83,    83,    83,    83,    83,    83,
      83,    84,    50,    46,    49,    51,    45,    46,    81,    83,
      46,    83,    46,    46,    49,    59,    60,    83,    83,    83,
      51,     8,    83,     8,    40,    52,    76,    78,    44,    65,
      67,    69,    50,    50,     8,     8,    46,    49,    62,     3,
      56,    46,    51,    51,    50,    50,    56,     8,    40,    52,
      81,    60,    56,    51,    51,    47,    50,     8,     8,    44,
      22,    71,    60,     3,    88,    51,    50,    50,    63,    72,
       3,    26,    77,    51,    51,    81,    56,    78,    74,    46,
      60,     3,    44,    58,    64,    48,    56,    60
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (0)


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (N)								\
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (0)
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
              (Loc).first_line, (Loc).first_column,	\
              (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (0)

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)		\
do {								\
  if (yydebug)							\
    {								\
      YYFPRINTF (stderr, "%s ", Title);				\
      yysymprint (stderr,					\
                  Type, Value);	\
      YYFPRINTF (stderr, "\n");					\
    }								\
} while (0)

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_stack_print (short int *bottom, short int *top)
#else
static void
yy_stack_print (bottom, top)
    short int *bottom;
    short int *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (/* Nothing. */; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_reduce_print (int yyrule)
#else
static void
yy_reduce_print (yyrule)
    int yyrule;
#endif
{
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu), ",
             yyrule - 1, yylno);
  /* Print the symbols being reduced, and their result.  */
  for (yyi = yyprhs[yyrule]; 0 <= yyrhs[yyi]; yyi++)
    YYFPRINTF (stderr, "%s ", yytname[yyrhs[yyi]]);
  YYFPRINTF (stderr, "-> %s\n", yytname[yyr1[yyrule]]);
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (Rule);		\
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined (__GLIBC__) && defined (_STRING_H)
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
#   if defined (__STDC__) || defined (__cplusplus)
yystrlen (const char *yystr)
#   else
yystrlen (yystr)
     const char *yystr;
#   endif
{
  const char *yys = yystr;

  while (*yys++ != '\0')
    continue;

  return yys - yystr - 1;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined (__GLIBC__) && defined (_STRING_H) && defined (_GNU_SOURCE)
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
#   if defined (__STDC__) || defined (__cplusplus)
yystpcpy (char *yydest, const char *yysrc)
#   else
yystpcpy (yydest, yysrc)
     char *yydest;
     const char *yysrc;
#   endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      size_t yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

#endif /* YYERROR_VERBOSE */



#if YYDEBUG
/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yysymprint (FILE *yyoutput, int yytype, YYSTYPE *yyvaluep)
#else
static void
yysymprint (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);


# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  switch (yytype)
    {
      default:
        break;
    }
  YYFPRINTF (yyoutput, ")");
}

#endif /* ! YYDEBUG */
/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
        break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse (void *YYPARSE_PARAM);
# else
int yyparse ();
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse (void *YYPARSE_PARAM)
# else
int yyparse (YYPARSE_PARAM)
  void *YYPARSE_PARAM;
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
int
yyparse (void)
#else
int
yyparse ()
    ;
#endif
#endif
{
  
  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  short int yyssa[YYINITDEPTH];
  short int *yyss = yyssa;
  short int *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;



#define YYPOPSTACK   (yyvsp--, yyssp--)

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* When reducing, the number of symbols on the RHS of the reduced
     rule.  */
  int yylen;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed. so pushing a state here evens the stacks.
     */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack. Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	short int *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	short int *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

/* Do appropriate processing given the current state.  */
/* Read a look-ahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to look-ahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;


  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  yystate = yyn;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 124 "bc.y"
    {
			      (yyval.i_value) = 0;
			      if (interactive && !quiet)
				{
				  show_bc_version ();
				  welcome ();
				}
			    }
    break;

  case 4:
#line 135 "bc.y"
    { run_code (); }
    break;

  case 5:
#line 137 "bc.y"
    { run_code (); }
    break;

  case 6:
#line 139 "bc.y"
    {
			      yyerrok;
			      init_gen ();
			    }
    break;

  case 8:
#line 146 "bc.y"
    { warn ("newline not allowed"); }
    break;

  case 9:
#line 149 "bc.y"
    { (yyval.i_value) = 0; }
    break;

  case 13:
#line 155 "bc.y"
    { (yyval.i_value) = 0; }
    break;

  case 20:
#line 164 "bc.y"
    { (yyval.i_value) = (yyvsp[0].i_value); }
    break;

  case 21:
#line 167 "bc.y"
    { warranty (""); }
    break;

  case 22:
#line 169 "bc.y"
    { limits (); }
    break;

  case 23:
#line 171 "bc.y"
    {
			      if ((yyvsp[0].i_value) & EX_COMP)
				warn ("comparison in expression");
			      if ((yyvsp[0].i_value) & EX_REG)
				generate ("W");
			      else 
				generate ("p");
			    }
    break;

  case 24:
#line 180 "bc.y"
    {
			      (yyval.i_value) = 0;
			      generate ("w");
			      generate ((yyvsp[0].s_value));
			      free ((yyvsp[0].s_value));
			    }
    break;

  case 25:
#line 187 "bc.y"
    {
			      if (break_label == 0)
				yyerror ("Break outside a for/while");
			      else
				{
				  sprintf (genstr, "J%1d:", break_label);
				  generate (genstr);
				}
			    }
    break;

  case 26:
#line 197 "bc.y"
    {
			      warn ("Continue statement");
			      if (continue_label == 0)
				yyerror ("Continue outside a for");
			      else
				{
				  sprintf (genstr, "J%1d:", continue_label);
				  generate (genstr);
				}
			    }
    break;

  case 27:
#line 208 "bc.y"
    { exit (0); }
    break;

  case 28:
#line 210 "bc.y"
    { generate ("h"); }
    break;

  case 29:
#line 212 "bc.y"
    { generate ("R"); }
    break;

  case 30:
#line 214 "bc.y"
    {
			      (yyvsp[0].i_value) = break_label; 
			      break_label = next_label++;
			    }
    break;

  case 31:
#line 219 "bc.y"
    {
			      if ((yyvsp[-1].i_value) & EX_COMP)
				warn ("Comparison in first for expression");
			      if ((yyvsp[-1].i_value) & EX_VOID)
				yyerror ("first expression is void");
			      if (!((yyvsp[-1].i_value) & EX_EMPTY))
				generate ("p");
			      (yyvsp[-1].i_value) = next_label++;
			      sprintf (genstr, "N%1d:", (yyvsp[-1].i_value));
			      generate (genstr);
			    }
    break;

  case 32:
#line 231 "bc.y"
    {
			      if ((yyvsp[-1].i_value) & EX_VOID)
				yyerror ("second expression is void");
			      if ((yyvsp[-1].i_value) & EX_EMPTY ) generate ("1");
			      (yyvsp[-1].i_value) = next_label++;
			      sprintf (genstr, "B%1d:J%1d:", (yyvsp[-1].i_value), break_label);
			      generate (genstr);
			      (yyval.i_value) = continue_label;
			      continue_label = next_label++;
			      sprintf (genstr, "N%1d:", continue_label);
			      generate (genstr);
			    }
    break;

  case 33:
#line 244 "bc.y"
    {
			      if ((yyvsp[-1].i_value) & EX_COMP)
				warn ("Comparison in third for expression");
			      if ((yyvsp[-1].i_value) & EX_VOID)
				yyerror ("third expression is void");
			      if ((yyvsp[-1].i_value) & EX_EMPTY)
				sprintf (genstr, "J%1d:N%1d:", (yyvsp[-7].i_value), (yyvsp[-4].i_value));
			      else
				sprintf (genstr, "pJ%1d:N%1d:", (yyvsp[-7].i_value), (yyvsp[-4].i_value));
			      generate (genstr);
			    }
    break;

  case 34:
#line 256 "bc.y"
    {
			      sprintf (genstr, "J%1d:N%1d:",
				       continue_label, break_label);
			      generate (genstr);
			      break_label = (yyvsp[-13].i_value);
			      continue_label = (yyvsp[-5].i_value);
			    }
    break;

  case 35:
#line 264 "bc.y"
    {
			      if ((yyvsp[-1].i_value) & EX_VOID)
				yyerror ("void expression");
			      (yyvsp[-1].i_value) = if_label;
			      if_label = next_label++;
			      sprintf (genstr, "Z%1d:", if_label);
			      generate (genstr);
			    }
    break;

  case 36:
#line 273 "bc.y"
    {
			      sprintf (genstr, "N%1d:", if_label); 
			      generate (genstr);
			      if_label = (yyvsp[-5].i_value);
			    }
    break;

  case 37:
#line 279 "bc.y"
    {
			      (yyvsp[0].i_value) = next_label++;
			      sprintf (genstr, "N%1d:", (yyvsp[0].i_value));
			      generate (genstr);
			    }
    break;

  case 38:
#line 285 "bc.y"
    {
			      if ((yyvsp[0].i_value) & EX_VOID)
				yyerror ("void expression");
			      (yyvsp[0].i_value) = break_label; 
			      break_label = next_label++;
			      sprintf (genstr, "Z%1d:", break_label);
			      generate (genstr);
			    }
    break;

  case 39:
#line 294 "bc.y"
    {
			      sprintf (genstr, "J%1d:N%1d:", (yyvsp[-7].i_value), break_label);
			      generate (genstr);
			      break_label = (yyvsp[-4].i_value);
			    }
    break;

  case 40:
#line 300 "bc.y"
    { (yyval.i_value) = 0; }
    break;

  case 41:
#line 302 "bc.y"
    {  warn ("print statement"); }
    break;

  case 45:
#line 309 "bc.y"
    {
			      generate ("O");
			      generate ((yyvsp[0].s_value));
			      free ((yyvsp[0].s_value));
			    }
    break;

  case 46:
#line 315 "bc.y"
    {
			      if ((yyvsp[0].i_value) & EX_VOID)
				yyerror ("void expression in print");
			      generate ("P");
			    }
    break;

  case 48:
#line 323 "bc.y"
    {
			      warn ("else clause in if statement");
			      (yyvsp[0].i_value) = next_label++;
			      sprintf (genstr, "J%d:N%1d:", (yyvsp[0].i_value), if_label); 
			      generate (genstr);
			      if_label = (yyvsp[0].i_value);
			    }
    break;

  case 50:
#line 334 "bc.y"
    { char *params, *autos;
			      /* Check auto list against parameter list? */
			      check_params ((yyvsp[-5].a_value),(yyvsp[0].a_value));
			      params = arg_str ((yyvsp[-5].a_value));
			      autos  = arg_str ((yyvsp[0].a_value));
			      set_genstr_size (30 + strlen (params)
					       + strlen (autos));
			      cur_func = lookup((yyvsp[-7].s_value),FUNCTDEF);
			      sprintf (genstr, "F%d,%s.%s[", cur_func, params,
				       autos); 
			      generate (genstr);
			      functions[cur_func].f_void = (yyvsp[-8].i_value);
			      free_args ((yyvsp[-5].a_value));
			      free_args ((yyvsp[0].a_value));
			      (yyvsp[-9].i_value) = next_label;
			      next_label = 1;
			    }
    break;

  case 51:
#line 352 "bc.y"
    {
			      generate ("0R]");
			      next_label = (yyvsp[-12].i_value);
			      cur_func = -1;
			    }
    break;

  case 52:
#line 359 "bc.y"
    { (yyval.i_value) = 0; }
    break;

  case 53:
#line 361 "bc.y"
    {
			      (yyval.i_value) = 1;
			      warn ("void functions");
			    }
    break;

  case 54:
#line 367 "bc.y"
    { (yyval.a_value) = NULL; }
    break;

  case 56:
#line 371 "bc.y"
    { (yyval.a_value) = NULL; }
    break;

  case 57:
#line 373 "bc.y"
    { (yyval.a_value) = (yyvsp[-1].a_value); }
    break;

  case 58:
#line 375 "bc.y"
    { (yyval.a_value) = (yyvsp[-1].a_value); }
    break;

  case 59:
#line 378 "bc.y"
    { (yyval.a_value) = nextarg (NULL, lookup ((yyvsp[0].s_value),SIMPLE), FALSE);}
    break;

  case 60:
#line 380 "bc.y"
    { (yyval.a_value) = nextarg (NULL, lookup ((yyvsp[-2].s_value),ARRAY), FALSE); }
    break;

  case 61:
#line 382 "bc.y"
    { (yyval.a_value) = nextarg (NULL, lookup ((yyvsp[-2].s_value),ARRAY), TRUE);
			      warn ("Call by variable arrays");
			    }
    break;

  case 62:
#line 386 "bc.y"
    { (yyval.a_value) = nextarg (NULL, lookup ((yyvsp[-2].s_value),ARRAY), TRUE);
			      warn ("Call by variable arrays");
			    }
    break;

  case 63:
#line 390 "bc.y"
    { (yyval.a_value) = nextarg ((yyvsp[-2].a_value), lookup ((yyvsp[0].s_value),SIMPLE), FALSE); }
    break;

  case 64:
#line 392 "bc.y"
    { (yyval.a_value) = nextarg ((yyvsp[-4].a_value), lookup ((yyvsp[-2].s_value),ARRAY), FALSE); }
    break;

  case 65:
#line 394 "bc.y"
    { (yyval.a_value) = nextarg ((yyvsp[-5].a_value), lookup ((yyvsp[-2].s_value),ARRAY), TRUE);
			      warn ("Call by variable arrays");
			    }
    break;

  case 66:
#line 398 "bc.y"
    { (yyval.a_value) = nextarg ((yyvsp[-5].a_value), lookup ((yyvsp[-2].s_value),ARRAY), TRUE);
			      warn ("Call by variable arrays");
			    }
    break;

  case 67:
#line 403 "bc.y"
    { (yyval.a_value) = NULL; }
    break;

  case 69:
#line 407 "bc.y"
    {
			      if ((yyvsp[0].i_value) & EX_COMP)
				warn ("comparison in argument");
			      if ((yyvsp[0].i_value) & EX_VOID)
				yyerror ("void argument");
			      (yyval.a_value) = nextarg (NULL,0,FALSE);
			    }
    break;

  case 70:
#line 415 "bc.y"
    {
			      sprintf (genstr, "K%d:", -lookup ((yyvsp[-2].s_value),ARRAY));
			      generate (genstr);
			      (yyval.a_value) = nextarg (NULL,1,FALSE);
			    }
    break;

  case 71:
#line 421 "bc.y"
    {
			      if ((yyvsp[0].i_value) & EX_COMP)
				warn ("comparison in argument");
			      if ((yyvsp[0].i_value) & EX_VOID)
				yyerror ("void argument");
			      (yyval.a_value) = nextarg ((yyvsp[-2].a_value),0,FALSE);
			    }
    break;

  case 72:
#line 429 "bc.y"
    {
			      sprintf (genstr, "K%d:", -lookup ((yyvsp[-2].s_value),ARRAY));
			      generate (genstr);
			      (yyval.a_value) = nextarg ((yyvsp[-4].a_value),1,FALSE);
			    }
    break;

  case 73:
#line 446 "bc.y"
    {
			      (yyval.i_value) = EX_EMPTY;
			      warn ("Missing expression in for statement");
			    }
    break;

  case 75:
#line 453 "bc.y"
    {
			      (yyval.i_value) = 0;
			      generate ("0");
			      if (cur_func == -1)
				yyerror("Return outside of a function.");
			    }
    break;

  case 76:
#line 460 "bc.y"
    {
			      if ((yyvsp[0].i_value) & EX_COMP)
				warn ("comparison in return expresion");
			      if (!((yyvsp[0].i_value) & EX_PAREN))
				warn ("return expression requires parenthesis");
			      if ((yyvsp[0].i_value) & EX_VOID)
				yyerror("return requires non-void expression");
			      if (cur_func == -1)
				yyerror("Return outside of a function.");
			      else if (functions[cur_func].f_void)
				yyerror("Return expression in a void function.");
			    }
    break;

  case 77:
#line 474 "bc.y"
    {
			      if ((yyvsp[0].c_value) != '=')
				{
				  if ((yyvsp[-1].i_value) < 0)
				    sprintf (genstr, "DL%d:", -(yyvsp[-1].i_value));
				  else
				    sprintf (genstr, "l%d:", (yyvsp[-1].i_value));
				  generate (genstr);
				}
			    }
    break;

  case 78:
#line 485 "bc.y"
    {
			      if ((yyvsp[0].i_value) & EX_ASSGN)
				warn("comparison in assignment");
			      if ((yyvsp[0].i_value) & EX_VOID)
				yyerror("Assignment of a void expression");
			      if ((yyvsp[-2].c_value) != '=')
				{
				  sprintf (genstr, "%c", (yyvsp[-2].c_value));
				  generate (genstr);
				}
			      if ((yyvsp[-3].i_value) < 0)
				sprintf (genstr, "S%d:", -(yyvsp[-3].i_value));
			      else
				sprintf (genstr, "s%d:", (yyvsp[-3].i_value));
			      generate (genstr);
			      (yyval.i_value) = EX_ASSGN;
			    }
    break;

  case 79:
#line 503 "bc.y"
    {
			      warn("&& operator");
			      (yyvsp[0].i_value) = next_label++;
			      sprintf (genstr, "DZ%d:p", (yyvsp[0].i_value));
			      generate (genstr);
			    }
    break;

  case 80:
#line 510 "bc.y"
    {
			      if (((yyvsp[-3].i_value) & EX_VOID) || ((yyvsp[0].i_value) & EX_VOID))
				yyerror ("void expression with &&");
			      sprintf (genstr, "DZ%d:p1N%d:", (yyvsp[-2].i_value), (yyvsp[-2].i_value));
			      generate (genstr);
			      (yyval.i_value) = ((yyvsp[-3].i_value) | (yyvsp[0].i_value)) & ~EX_PAREN;
			    }
    break;

  case 81:
#line 518 "bc.y"
    {
			      warn("|| operator");
			      (yyvsp[0].i_value) = next_label++;
			      sprintf (genstr, "B%d:", (yyvsp[0].i_value));
			      generate (genstr);
			    }
    break;

  case 82:
#line 525 "bc.y"
    {
			      int tmplab;
			      if (((yyvsp[-3].i_value) & EX_VOID) || ((yyvsp[0].i_value) & EX_VOID))
				yyerror ("void expression with ||");
			      tmplab = next_label++;
			      sprintf (genstr, "B%d:0J%d:N%d:1N%d:",
				       (yyvsp[-2].i_value), tmplab, (yyvsp[-2].i_value), tmplab);
			      generate (genstr);
			      (yyval.i_value) = ((yyvsp[-3].i_value) | (yyvsp[0].i_value)) & ~EX_PAREN;
			    }
    break;

  case 83:
#line 536 "bc.y"
    {
			      if ((yyvsp[0].i_value) & EX_VOID)
				yyerror ("void expression with !");
			      (yyval.i_value) = (yyvsp[0].i_value) & ~EX_PAREN;
			      warn("! operator");
			      generate ("!");
			    }
    break;

  case 84:
#line 544 "bc.y"
    {
			      if (((yyvsp[-2].i_value) & EX_VOID) || ((yyvsp[0].i_value) & EX_VOID))
				yyerror ("void expression with comparison");
			      (yyval.i_value) = EX_REG | EX_COMP;
			      switch (*((yyvsp[-1].s_value)))
				{
				case '=':
				  generate ("=");
				  break;

				case '!':
				  generate ("#");
				  break;

				case '<':
				  if ((yyvsp[-1].s_value)[1] == '=')
				    generate ("{");
				  else
				    generate ("<");
				  break;

				case '>':
				  if ((yyvsp[-1].s_value)[1] == '=')
				    generate ("}");
				  else
				    generate (">");
				  break;
				}
			    }
    break;

  case 85:
#line 574 "bc.y"
    {
			      if (((yyvsp[-2].i_value) & EX_VOID) || ((yyvsp[0].i_value) & EX_VOID))
				yyerror ("void expression with +");
			      generate ("+");
			      (yyval.i_value) = ((yyvsp[-2].i_value) | (yyvsp[0].i_value)) & ~EX_PAREN;
			    }
    break;

  case 86:
#line 581 "bc.y"
    {
			      if (((yyvsp[-2].i_value) & EX_VOID) || ((yyvsp[0].i_value) & EX_VOID))
				yyerror ("void expression with -");
			      generate ("-");
			      (yyval.i_value) = ((yyvsp[-2].i_value) | (yyvsp[0].i_value)) & ~EX_PAREN;
			    }
    break;

  case 87:
#line 588 "bc.y"
    {
			      if (((yyvsp[-2].i_value) & EX_VOID) || ((yyvsp[0].i_value) & EX_VOID))
				yyerror ("void expression with *");
			      generate ("*");
			      (yyval.i_value) = ((yyvsp[-2].i_value) | (yyvsp[0].i_value)) & ~EX_PAREN;
			    }
    break;

  case 88:
#line 595 "bc.y"
    {
			      if (((yyvsp[-2].i_value) & EX_VOID) || ((yyvsp[0].i_value) & EX_VOID))
				yyerror ("void expression with /");
			      generate ("/");
			      (yyval.i_value) = ((yyvsp[-2].i_value) | (yyvsp[0].i_value)) & ~EX_PAREN;
			    }
    break;

  case 89:
#line 602 "bc.y"
    {
			      if (((yyvsp[-2].i_value) & EX_VOID) || ((yyvsp[0].i_value) & EX_VOID))
				yyerror ("void expression with %");
			      generate ("%");
			      (yyval.i_value) = ((yyvsp[-2].i_value) | (yyvsp[0].i_value)) & ~EX_PAREN;
			    }
    break;

  case 90:
#line 609 "bc.y"
    {
			      if (((yyvsp[-2].i_value) & EX_VOID) || ((yyvsp[0].i_value) & EX_VOID))
				yyerror ("void expression with ^");
			      generate ("^");
			      (yyval.i_value) = ((yyvsp[-2].i_value) | (yyvsp[0].i_value)) & ~EX_PAREN;
			    }
    break;

  case 91:
#line 616 "bc.y"
    {
			      if ((yyvsp[0].i_value) & EX_VOID)
				yyerror ("void expression with unary -");
			      generate ("n");
			      (yyval.i_value) = (yyvsp[0].i_value) & ~EX_PAREN;
			    }
    break;

  case 92:
#line 623 "bc.y"
    {
			      (yyval.i_value) = EX_REG;
			      if ((yyvsp[0].i_value) < 0)
				sprintf (genstr, "L%d:", -(yyvsp[0].i_value));
			      else
				sprintf (genstr, "l%d:", (yyvsp[0].i_value));
			      generate (genstr);
			    }
    break;

  case 93:
#line 632 "bc.y"
    {
			      int len = strlen((yyvsp[0].s_value));
			      (yyval.i_value) = EX_REG;
			      if (len == 1 && *(yyvsp[0].s_value) == '0')
				generate ("0");
			      else if (len == 1 && *(yyvsp[0].s_value) == '1')
				generate ("1");
			      else
				{
				  generate ("K");
				  generate ((yyvsp[0].s_value));
				  generate (":");
				}
			      free ((yyvsp[0].s_value));
			    }
    break;

  case 94:
#line 648 "bc.y"
    { 
			      if ((yyvsp[-1].i_value) & EX_VOID)
				yyerror ("void expression in parenthesis");
			      (yyval.i_value) = (yyvsp[-1].i_value) | EX_REG | EX_PAREN;
			    }
    break;

  case 95:
#line 654 "bc.y"
    { int fn;
			      fn = lookup ((yyvsp[-3].s_value),FUNCT);
			      if (functions[fn].f_void)
				(yyval.i_value) = EX_VOID;
			      else
				(yyval.i_value) = EX_REG;
			      if ((yyvsp[-1].a_value) != NULL)
				{ char *params = call_str ((yyvsp[-1].a_value));
				  set_genstr_size (20 + strlen (params));
				  sprintf (genstr, "C%d,%s:", fn, params);
				  free_args ((yyvsp[-1].a_value));
				}
			      else
				{
				  sprintf (genstr, "C%d:", fn);
				}
			      generate (genstr);
			    }
    break;

  case 96:
#line 673 "bc.y"
    {
			      (yyval.i_value) = EX_REG;
			      if ((yyvsp[0].i_value) < 0)
				{
				  if ((yyvsp[-1].c_value) == '+')
				    sprintf (genstr, "DA%d:L%d:", -(yyvsp[0].i_value), -(yyvsp[0].i_value));
				  else
				    sprintf (genstr, "DM%d:L%d:", -(yyvsp[0].i_value), -(yyvsp[0].i_value));
				}
			      else
				{
				  if ((yyvsp[-1].c_value) == '+')
				    sprintf (genstr, "i%d:l%d:", (yyvsp[0].i_value), (yyvsp[0].i_value));
				  else
				    sprintf (genstr, "d%d:l%d:", (yyvsp[0].i_value), (yyvsp[0].i_value));
				}
			      generate (genstr);
			    }
    break;

  case 97:
#line 692 "bc.y"
    {
			      (yyval.i_value) = EX_REG;
			      if ((yyvsp[-1].i_value) < 0)
				{
				  sprintf (genstr, "DL%d:x", -(yyvsp[-1].i_value));
				  generate (genstr); 
				  if ((yyvsp[0].c_value) == '+')
				    sprintf (genstr, "A%d:", -(yyvsp[-1].i_value));
				  else
				      sprintf (genstr, "M%d:", -(yyvsp[-1].i_value));
				}
			      else
				{
				  sprintf (genstr, "l%d:", (yyvsp[-1].i_value));
				  generate (genstr);
				  if ((yyvsp[0].c_value) == '+')
				    sprintf (genstr, "i%d:", (yyvsp[-1].i_value));
				  else
				    sprintf (genstr, "d%d:", (yyvsp[-1].i_value));
				}
			      generate (genstr);
			    }
    break;

  case 98:
#line 715 "bc.y"
    {
			      if ((yyvsp[-1].i_value) & EX_VOID)
				yyerror ("void expression in length()");
			      generate ("cL");
			      (yyval.i_value) = EX_REG;
			    }
    break;

  case 99:
#line 722 "bc.y"
    {
			      if ((yyvsp[-1].i_value) & EX_VOID)
				yyerror ("void expression in sqrt()");
			      generate ("cR");
			      (yyval.i_value) = EX_REG;
			    }
    break;

  case 100:
#line 729 "bc.y"
    {
			      if ((yyvsp[-1].i_value) & EX_VOID)
				yyerror ("void expression in scale()");
			      generate ("cS");
			      (yyval.i_value) = EX_REG;
			    }
    break;

  case 101:
#line 736 "bc.y"
    {
			      warn ("read function");
			      generate ("cI");
			      (yyval.i_value) = EX_REG;
			    }
    break;

  case 102:
#line 742 "bc.y"
    {
			      warn ("random function");
			      generate ("cX");
			      (yyval.i_value) = EX_REG;
			    }
    break;

  case 103:
#line 749 "bc.y"
    { (yyval.i_value) = lookup((yyvsp[0].s_value),SIMPLE); }
    break;

  case 104:
#line 751 "bc.y"
    {
			      if ((yyvsp[-1].i_value) & EX_VOID)
				yyerror("void expression as subscript");
			      if ((yyvsp[-1].i_value) & EX_COMP)
				warn("comparison in subscript");
			      (yyval.i_value) = lookup((yyvsp[-3].s_value),ARRAY);
			    }
    break;

  case 105:
#line 759 "bc.y"
    { (yyval.i_value) = 0; }
    break;

  case 106:
#line 761 "bc.y"
    { (yyval.i_value) = 1; }
    break;

  case 107:
#line 763 "bc.y"
    { (yyval.i_value) = 2; }
    break;

  case 108:
#line 765 "bc.y"
    { (yyval.i_value) = 3;
			      warn ("History variable");
			    }
    break;

  case 109:
#line 769 "bc.y"
    { (yyval.i_value) = 4;
			      warn ("Last variable");
			    }
    break;

  case 110:
#line 775 "bc.y"
    { warn ("End of line required"); }
    break;

  case 112:
#line 778 "bc.y"
    { warn ("Too many end of lines"); }
    break;


      default: break;
    }

/* Line 1126 of yacc.c.  */
#line 2350 "bc.c"

  yyvsp -= yylen;
  yyssp -= yylen;


  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (YYPACT_NINF < yyn && yyn < YYLAST)
	{
	  int yytype = YYTRANSLATE (yychar);
	  YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
	  YYSIZE_T yysize = yysize0;
	  YYSIZE_T yysize1;
	  int yysize_overflow = 0;
	  char *yymsg = 0;
#	  define YYERROR_VERBOSE_ARGS_MAXIMUM 5
	  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
	  int yyx;

#if 0
	  /* This is so xgettext sees the translatable formats that are
	     constructed on the fly.  */
	  YY_("syntax error, unexpected %s");
	  YY_("syntax error, unexpected %s, expecting %s");
	  YY_("syntax error, unexpected %s, expecting %s or %s");
	  YY_("syntax error, unexpected %s, expecting %s or %s or %s");
	  YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
#endif
	  char *yyfmt;
	  char const *yyf;
	  static char const yyunexpected[] = "syntax error, unexpected %s";
	  static char const yyexpecting[] = ", expecting %s";
	  static char const yyor[] = " or %s";
	  char yyformat[sizeof yyunexpected
			+ sizeof yyexpecting - 1
			+ ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
			   * (sizeof yyor - 1))];
	  char const *yyprefix = yyexpecting;

	  /* Start YYX at -YYN if negative to avoid negative indexes in
	     YYCHECK.  */
	  int yyxbegin = yyn < 0 ? -yyn : 0;

	  /* Stay within bounds of both yycheck and yytname.  */
	  int yychecklim = YYLAST - yyn;
	  int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
	  int yycount = 1;

	  yyarg[0] = yytname[yytype];
	  yyfmt = yystpcpy (yyformat, yyunexpected);

	  for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	    if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	      {
		if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
		  {
		    yycount = 1;
		    yysize = yysize0;
		    yyformat[sizeof yyunexpected - 1] = '\0';
		    break;
		  }
		yyarg[yycount++] = yytname[yyx];
		yysize1 = yysize + yytnamerr (0, yytname[yyx]);
		yysize_overflow |= yysize1 < yysize;
		yysize = yysize1;
		yyfmt = yystpcpy (yyfmt, yyprefix);
		yyprefix = yyor;
	      }

	  yyf = YY_(yyformat);
	  yysize1 = yysize + yystrlen (yyf);
	  yysize_overflow |= yysize1 < yysize;
	  yysize = yysize1;

	  if (!yysize_overflow && yysize <= YYSTACK_ALLOC_MAXIMUM)
	    yymsg = (char *) YYSTACK_ALLOC (yysize);
	  if (yymsg)
	    {
	      /* Avoid sprintf, as that infringes on the user's name space.
		 Don't have undefined behavior even if the translation
		 produced a string with the wrong number of "%s"s.  */
	      char *yyp = yymsg;
	      int yyi = 0;
	      while ((*yyp = *yyf))
		{
		  if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		    {
		      yyp += yytnamerr (yyp, yyarg[yyi++]);
		      yyf += 2;
		    }
		  else
		    {
		      yyp++;
		      yyf++;
		    }
		}
	      yyerror (yymsg);
	      YYSTACK_FREE (yymsg);
	    }
	  else
	    {
	      yyerror (YY_("syntax error"));
	      goto yyexhaustedlab;
	    }
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror (YY_("syntax error"));
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
        {
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
        }
      else
	{
	  yydestruct ("Error: discarding", yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (0)
     goto yyerrorlab;

yyvsp -= yylen;
  yyssp -= yylen;
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping", yystos[yystate], yyvsp);
      YYPOPSTACK;
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;


  /* Shift the error token. */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK;
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  return yyresult;
}


#line 781 "bc.y"



