## Makefile.am -- Process this file with automake to produce Makefile.in

#AUTOMAKE_OPTIONS = dejagnu

#SUBDIRS = . test

INCLUDES = -I$(top_srcdir) @SPRNG_CFLAGS@ @GLIB_CFLAGS@ @GSL_CFLAGS@ -DG_LOG_DOMAIN=\"rng\"

lib_LTLIBRARIES = librng.la
include_HEADERS = rng.h

librng_la_SOURCES = rng.c rng.h
librng_la_LIBADD = @SPRNG_LIBS@ @GLIB_LIBS@ @GSL_LIBS@

#RUNTESTDEFAULTFLAGS = --tool librng --debug -v -v SHELL=$$srcdir/test/shell --srcdir $$srcdir/test

# The distclean-DEJAGNU rule, run as part of "make distclean", will delete
# site.exp, site.bak, and .sum and .log files with the top-level package name
# (sim).  Also delete the .sum and .log files for this library.

#DISTCLEANFILES = dbg.log librng.log librng.sum
