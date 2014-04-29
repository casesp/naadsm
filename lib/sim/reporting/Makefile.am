## Makefile.am -- Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = dejagnu

SUBDIRS = . test

INCLUDES = -I$(top_srcdir) @GLIB_CFLAGS@ -DG_LOG_DOMAIN=\"reporting\"

lib_LTLIBRARIES = libreporting.la
include_HEADERS = reporting.h

libreporting_la_SOURCES = reporting.c reporting.h
libreporting_la_LIBADD = @GLIB_LIBS@

RUNTESTDEFAULTFLAGS = --tool libreporting --debug -v -v SHELL=$$srcdir/test/shell --srcdir $$srcdir/test

# The distclean-DEJAGNU rule, run as part of "make distclean", will delete
# site.exp, site.bak, and .sum and .log files with the top-level package name
# (sim).  Also delete the .sum and .log files for this library.

DISTCLEANFILES = dbg.log libreporting.log libreporting.sum
