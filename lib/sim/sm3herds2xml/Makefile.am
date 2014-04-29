## Makefile.am -- Process this file with automake to produce Makefile.in

INCLUDES = -I$(top_srcdir)/herd -I$(top_srcdir)/rel_chart
INCLUDES += @GLIB_CFLAGS@
AM_LDFLAGS = -L$(top_srcdir)/herd -L$(top_srcdir)/wml

AM_YFLAGS = -d -v

bin_PROGRAMS = sm3herds2xml
sm3herds2xml_SOURCES = main.c parser.y scanner.l
sm3herds2xml_LDADD = -lherd -lwml
sm3herds2xml_LDADD += @LEXLIB@
