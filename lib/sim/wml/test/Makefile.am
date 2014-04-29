## Makefile.am -- Process this file with automake to produce Makefile.in

INCLUDES = -I.. @GLIB_CFLAGS@
AM_LDFLAGS = -L..

AM_YFLAGS = -d -v

noinst_PROGRAMS = shell
shell_SOURCES = shell.y scanner.l
shell_LDADD = -lwml

# Include the DejaGnu test files in the distribution file.

EXTRA_DIST = config/unix.exp libwml.convexhull/*.exp libwml.pointinpolygon/*.exp
