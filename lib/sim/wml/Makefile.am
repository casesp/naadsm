## Makefile.am -- Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = dejagnu

SUBDIRS = . test

INCLUDES = @GLIB_CFLAGS@ -DG_LOG_DOMAIN=\"wml\"

lib_LTLIBRARIES = libwml.la
include_HEADERS = wml.h
nobase_include_HEADERS = wml/WmlConvexHull2.h wml/WmlContPointInPolygon2.h \
  wml/WmlVector2.h wml/WmlContMinBox2.h wml/WmlBox2.h

libwml_la_SOURCES = WmlConvexHull2.c WmlContPointInPolygon2.c WmlVector2.c \
  WmlContMinBox2.c WmlBox2.c
libwml_la_LIBADD = @GLIB_LIBS@ -lm

RUNTESTDEFAULTFLAGS = --tool libwml --debug -v -v SHELL=$$srcdir/test/shell --srcdir $$srcdir/test

# The distclean-DEJAGNU rule, run as part of "make distclean", will delete
# site.exp, site.bak, and .sum and .log files with the top-level package name
# (sim).  Also delete the .sum and .log files for this library.

DISTCLEANFILES = dbg.log libwml.log libwml.sum
