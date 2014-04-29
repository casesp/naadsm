%{
#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <glib.h>
#include "shell.h"

#if STDC_HEADERS
#  include <string.h>
#elif HAVE_STRINGS_H
#  include <strings.h>
#endif

/** @file prob_dist/test/scanner.c
 * A scanner for a simple shell to exercise libprob_dist.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date July 2003
 *
 * Copyright &copy; University of Guelph, 2003-2006
 * 
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * @todo Check for overflow in string-to-int and string-to-float conversions.
 *
 */
#define TABWIDTH 8
#define BUFFERSIZE 2048
#define ADJUST (tokenpos = charpos, charpos += yyleng)

int yyerror (char *s, int fatal);

char linebuf[BUFFERSIZE];
int charpos = 0, tokenpos = 0;
%}
%option case-insensitive
%option noyywrap
%x PASTFIRSTLINE
%%
.*	{
  /* Copy a line of input to a buffer, then "put it back" for processing by
   * lex.  This allows us to print pretty error messages showing the entire
   * current line of input. */
  strncpy (linebuf, yytext, BUFFERSIZE);
  /* If the input line didn't fit in the buffer, null-terminate it. */
  linebuf[BUFFERSIZE-1] = '\0';
  yyless(0);
  BEGIN(PASTFIRSTLINE);
}
<PASTFIRSTLINE>{
  \n.* {
    /* Copy a line of input to a buffer (see comment above). */
    strncpy (linebuf, yytext+1, BUFFERSIZE);
    /* If the input line didn't fit in the buffer, null-terminate it. */
    linebuf[BUFFERSIZE-1] = '\0';
    yyless(1);
    charpos = 0;
    tokenpos = 0;
  }
  \t {
    tokenpos = charpos;
    charpos += TABWIDTH;
  }
  ","                       { ADJUST; return COMMA; }
  "("                       { ADJUST; return LPAREN; }
  ")"                       { ADJUST; return RPAREN; }
  "point"                   { ADJUST; return POINT; }
  "uniform"                 { ADJUST; return UNIFORM; }
  "triangular"              { ADJUST; return TRIANGULAR; }
  "piecewise"               { ADJUST; return PIECEWISE; }
  histo(gram)?              { ADJUST; return HISTOGRAM; }
  "gaussian"                { ADJUST; return GAUSSIAN; }
  "poisson"                 { ADJUST; return POISSON; }
  "beta"                    { ADJUST; return BETA; }
  "betapert"                { ADJUST; return BETAPERT; }
  "gamma"                   { ADJUST; return GAMMA; }
  "weibull"                 { ADJUST; return WEIBULL; }
  "exponential"             { ADJUST; return EXPONENTIAL; }
  "logistic"                { ADJUST; return LOGISTIC; }
  "loglogistic"             { ADJUST; return LOGLOGISTIC; }
  "lognormal"               { ADJUST; return LOGNORMAL; }
  "pdf"                     { ADJUST; return PDF; }
  "cdf"                     { ADJUST; return CDF; }
  "invcdf"                  { ADJUST; return INVCDF; }
  "sample"                  { ADJUST; return SAMPLE; }
  [+-]?[[:digit:]]+(\.[[:digit:]]+)?([eE][+-][[:digit:]]+)? {
    ADJUST;
    yylval.fval = (double)atof(yytext);
    return NUM;
  }
  \n {
    charpos = 0;
    tokenpos = 0;
  }
  [[:space:]] { /* Ignore whitespace */ }
  . {
    /* Report an unrecognized token. */
    yyerror ("Unrecognized token", 1);
  }
}
%%
