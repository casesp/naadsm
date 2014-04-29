## Makefile.am -- Process this file with automake to produce Makefile.in

## AUTOMAKE_OPTIONS = dejagnu

## SUBDIRS = . test

INCLUDES = -I$(top_srcdir) -I$(top_srcdir)/herd -I$(top_srcdir)/rel_chart @GLIB_CFLAGS@ -DG_LOG_DOMAIN=\"event\"

lib_LTLIBRARIES = libevent.la
include_HEADERS = event.h

libevent_la_SOURCES = event.c event.h
libevent_la_LIBADD = @GLIB_LIBS@

## RUNTESTDEFAULTFLAGS = --tool libevent --debug -v -v SHELL=$$srcdir/test/shell --srcdir $$srcdir/test
