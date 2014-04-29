## Makefile.am -- Process this file with automake to produce Makefile.in

INCLUDES = -I.. -I$(top_srcdir)/rel_chart @GLIB_CFLAGS@
AM_LDFLAGS = -L.. -L$(top_srcdir)/wml

AM_YFLAGS = -d -v

noinst_PROGRAMS = shell
shell_SOURCES = shell.y scanner.l
shell_LDADD = -lherd -lwml $(top_builddir)/replace/libreplace.la

# Include the DejaGnu test files in the distribution file.

EXTRA_DIST = config/unix.exp libherd.all/*.exp
