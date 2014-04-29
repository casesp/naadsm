## Makefile.am -- Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = dejagnu

SUBDIRS = . test

INCLUDES = -I$(top_srcdir) @GLIB_CFLAGS@ -DG_LOG_DOMAIN=\"rel_chart\"

lib_LTLIBRARIES = librel_chart.la
include_HEADERS = rel_chart.h

librel_chart_la_SOURCES = rel_chart.c rel_chart.h
librel_chart_la_LIBADD = @GLIB_LIBS@ @GSL_LIBS@

RUNTESTDEFAULTFLAGS = --tool librel_chart --debug -v -v SHELL=$$srcdir/test/shell --srcdir $$srcdir/test

# The distclean-DEJAGNU rule, run as part of "make distclean", will delete
# site.exp, site.bak, and .sum and .log files with the top-level package name
# (sim).  Also delete the .sum and .log files for this library.

DISTCLEANFILES = dbg.log librel_chart.log librel_chart.sum
