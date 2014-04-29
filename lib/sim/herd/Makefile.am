## Makefile.am -- Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = dejagnu

SUBDIRS = . test

INCLUDES = -I$(top_srcdir) -I$(top_srcdir)/rel_chart -I$(top_srcdir)/wml -I$(top_srcdir)/gis -I$(top_srcdir)/zone -I$(top_srcdir)/src @GLIB_CFLAGS@ @SCEW_CFLAGS@ @SPRNG_CFLAGS@ -DG_LOG_DOMAIN=\"herd\"
AM_LDFLAGS = -L$(top_srcdir)/rel_chart -L$(top_srcdir)/wml -L$(top_srcdir)/gis

lib_LTLIBRARIES = libherd.la
include_HEADERS = herd.h

libherd_la_SOURCES = herd.c herd.h 2dch.c 2dch.h
libherd_la_LIBADD = -lrel_chart -lwml -lgis $(top_builddir)/replace/libreplace.la @GLIB_LIBS@ @GSL_LIBS@ @SCEW_LIBS@ @SPRNG_LIBS@ -lm

RUNTESTDEFAULTFLAGS = --tool libherd --debug -v -v SHELL=$$srcdir/test/shell --srcdir $$srcdir/test

# The distclean-DEJAGNU rule, run as part of "make distclean", will delete
# site.exp, site.bak, and .sum and .log files with the top-level package name
# (sim).  Also delete the .sum and .log files for this library.

DISTCLEANFILES = dbg.log libherd.log libherd.sum
