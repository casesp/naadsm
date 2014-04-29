## Makefile.am -- Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = dejagnu

SUBDIRS = . test

INCLUDES = @GLIB_CFLAGS@ @GSL_CFLAGS@ -DG_LOG_DOMAIN=\"gis\"

lib_LTLIBRARIES = libgis.la
include_HEADERS = gis.h

libgis_la_SOURCES = gis.c gis.h
libgis_la_LIBADD = -lgpcl @GLIB_LIBS@ @GSL_LIBS@ -lm

RUNTESTDEFAULTFLAGS = --tool libgis --debug -v -v SHELL=$$srcdir/test/shell --srcdir $$srcdir/test
