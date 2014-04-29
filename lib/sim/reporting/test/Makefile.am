## Makefile.am -- Process this file with automake to produce Makefile.in

INCLUDES = -I.. @GLIB_CFLAGS@
AM_LDFLAGS = -L..

AM_YFLAGS = -d -v

noinst_PROGRAMS = shell fcmp
shell_SOURCES = shell.y scanner.l
shell_LDADD = -lreporting $(top_builddir)/replace/libreplace.la

fcmp_SOURCES = fcmp.c
fcmp_LDADD = @GLIB_LIBS@ @GSL_LIBS@

# Include the DejaGnu test files in the distribution file.

EXTRA_DIST = config/unix.exp libreporting.all/*.exp
