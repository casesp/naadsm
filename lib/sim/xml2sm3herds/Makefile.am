## Makefile.am -- Process this file with automake to produce Makefile.in

INCLUDES = -I$(top_srcdir)/herd -I$(top_srcdir)/rel_chart
INCLUDES += @GLIB_CFLAGS@
AM_LDFLAGS = -L$(top_srcdir)/herd -L$(top_srcdir)/wml

bin_PROGRAMS = xml2sm3herds
xml2sm3herds_SOURCES = main.c
xml2sm3herds_LDADD = -lherd -lwml @GLIB_LIBS@
