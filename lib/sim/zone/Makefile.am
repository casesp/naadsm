## Makefile.am -- Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = dejagnu

SUBDIRS = .
#SUBDIRS = . test

INCLUDES = -I$(top_srcdir) -I$(top_srcdir)/gis -I$(top_srcdir)/wml @GLIB_CFLAGS@ -DG_LOG_DOMAIN=\"zone\"
AM_LDFLAGS = -L$(top_srcdir)/gis -L$(top_srcdir)/wml

lib_LTLIBRARIES = libzone.la
include_HEADERS = zone.h

libzone_la_SOURCES = zone.c zone.h
libzone_la_LIBADD = -lgis -lwml $(top_builddir)/replace/libreplace.la @GLIB_LIBS@ -lm

# RUNTESTDEFAULTFLAGS = --tool libzone --debug -v -v SHELL=$$srcdir/test/shell --srcdir $$srcdir/test

# The distclean-DEJAGNU rule, run as part of "make distclean", will delete
# site.exp, site.bak, and .sum and .log files with the top-level package name
# (sim).  Also delete the .sum and .log files for this library.

DISTCLEANFILES = dbg.log libzone.log libzone.sum
