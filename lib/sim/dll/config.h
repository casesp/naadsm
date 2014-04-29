/** @file dll/config.h
 *
 * @author Aaron Reeves <Aaron.Reeves@ucalgary.ca><br>
 *
 * Copyright &copy; 2005 - 2013 NAADSM Development Team
 *
 * This file has been modified by hand for Windows/MinGW
 * versions (exe and dll) of NAADSM.
 * Linux/Unix builds should use config.h as generated by configure.
 *
 * <http://www.naadsm.org>
 *
 * This file is based on the following sources:
 * config.h.  Generated by configure.
 * config.h.in.  Generated from configure.in by autoheader.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

/* Set to 1 to enable debugging output. */
#define DEBUG 0

/* Set to 1 to enable informational output. */
#define INFO 0

/* Define to 1 if you have the `atan2' function. */
#define HAVE_ATAN2 1

/* Define to 1 if you have the <ctype.h> header file. */
#define HAVE_CTYPE_H 1

/* Define to 1 if you have the <dlfcn.h> header file. */
#define HAVE_DLFCN_H 0

/* Define to 1 if you have the <errno.h> header file. */
#define HAVE_ERRNO_H 1

/* Define if you have the `getline' function. */
#undef HAVE_GETLINE

/* Define to 1 if you have the `getstr' function. */
/* #undef HAVE_GETSTR */

/* Define to 1 if you have the <gmp.h> header file. */
#define HAVE_GMP_H 1

/* Define to 1 if you have the `g_strsplit_regexp' function. */
/* #undef HAVE_G_STRSPLIT_REGEXP */

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the `gpcl' library (-lgpcl). */
#define HAVE_LIBGPCL 1

/* Define to 1 if you have the `m' library (-lm). */
#define HAVE_LIBM 1

/* Define to 1 if you have the `popt' library (-lpopt). */
#define HAVE_LIBPOPT 1

/* Define to 1 if you have the `rtree' library (-lrtree). */
#define HAVE_LIBRTREE 1

/* Define to 1 if you have <limits.h>. */
#define HAVE_LIMITS_H 1

/* Define to 1 if you have the <ltdl.h> header file. */
#define HAVE_LTDL_H 0

/* Define to 1 if you have the <math.h> header file. */
#define HAVE_MATH_H 1

/* Define to 1 if you have the `memchr' function. */
#define HAVE_MEMCHR 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define this if the link test succeeded */
/* #undef HAVE_MPI */

/* Define to 1 if you have the <regex.h> header file. */
#define HAVE_REGEX_H 1

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the `strcasecmp' function. */
#define HAVE_STRCASECMP 1

/* Define to 1 if you have the `strdup' function. */
#define HAVE_STRDUP 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the `strndup' function. */
#define HAVE_STRNDUP 1

/* Define to 1 if you have the `strnlen' function. */
#define HAVE_STRNLEN 1

/* Define to 1 if you have the `strstr' function. */
#define HAVE_STRSTR 1

/* Define to 1 if you have the `strtod' function. */
#define HAVE_STRTOD 1

/* Define to 1 if you have the `strtol' function. */
#define HAVE_STRTOL 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Name of package */
#define PACKAGE "NAADSM"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT ""

/* Define to the full name of this package. */
#define PACKAGE_NAME "naadsm"

/* Define to the full name and version of this package. */
/* ================== */
/* !!!! IMPORTANT !!! */
/* ================== */
/* DON'T FORGET: When updating this version number, also update the version 
   number in the file config.in for the Linux version */ 
#define PACKAGE_STRING "NAADSM 4.1.0"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "NAADSM"

/* Define to the version of this package. */
/* ================== */
/* !!!! IMPORTANT !!! */
/* ================== */
/* DON'T FORGET: When updating this version number, also update the version 
   number in the file config.in for the Linux version */ 
#define PACKAGE_VERSION "4.1.0"

/* Define to use the simple SPRNG interface. */
#define SIMPLE_SPRNG 1

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Define to 1 if using the GNU multiple precision library. */
#define USE_GMP 1

/* Version number of package */
/* ================== */
/* !!!! IMPORTANT !!! */
/* ================== */
/* DON'T FORGET: When updating this version number, also update the version 
   number in the file config.in for the Linux version */ 
#define VERSION "4.1.0"

/* Define this if the link test failed. */
/* #undef WITHOUT_SPRNG */

/* Define to 1 if you have the SPRNG random number generator. */
#define WITH_SPRNG 1

/* Define to 1 if `lex' declares `yytext' as a `char *' by default, not a
   `char[]'. */
#define YYTEXT_POINTER 1

/* Define to empty if `const' does not conform to ANSI C. */
/* #undef const */

/* Define to `unsigned' if <sys/types.h> does not define. */
/* #undef size_t */
