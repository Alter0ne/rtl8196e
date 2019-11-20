#line 1548 "./doc/bison.texi"
/* Reverse polish notation calculator.  */


%{
  #include <stdio.h>
  #include <math.h>
  int yylex (void);
  void yyerror (char const *);
%}


%define api.value.type {double}
%token NUM

%% /* Grammar rules and actions follow.  */
#line 1603 "./doc/bison.texi"

input:
  %empty
| input line
;



line:
  '\n'
| exp '\n'      { printf ("%.10g\n", $1); }
;



exp:
  NUM           { $$ = $1;           }
| exp exp '+'   { $$ = $1 + $2;      }
| exp exp '-'   { $$ = $1 - $2;      }
| exp exp '*'   { $$ = $1 * $2;      }
| exp exp '/'   { $$ = $1 / $2;      }
| exp exp '^'   { $$ = pow ($1, $2); }  /* Exponentiation */
| exp 'n'       { $$ = -$1;          }  /* Unary minus    */
;

%%
#line 1819 "./doc/bison.texi"

/* The lexical analyzer returns a double floating point
   number on the stack and the token NUM, or the numeric code
   of the character read if not a number.  It skips all blanks
   and tabs, and returns 0 for end-of-input.  */

#include <ctype.h>



int
yylex (void)
{
  int c;

  /* Skip white space.  */
  while ((c = getchar ()) == ' ' || c == '\t')
    continue;


  /* Process numbers.  */
  if (c == '.' || isdigit (c))
    {
      ungetc (c, stdin);
      scanf ("%lf", &yylval);
      return NUM;
    }


  /* Return end-of-input.  */
  if (c == EOF)
    return 0;
  /* Return a single char.  */
  return c;
}

#line 1868 "./doc/bison.texi"

int
main (void)
{
  return yyparse ();
}

#line 1889 "./doc/bison.texi"
#include <stdio.h>


/* Called by yyparse on error.  */
void
yyerror (char const *s)
{
  fprintf (stderr, "%s\n", s);
}

