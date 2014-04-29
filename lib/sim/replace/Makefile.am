## Makefile.am -- Process this file with automake to produce Makefile.in

## The functions in this directory are automatically built and included if
## AC_REPLACE_FUNCS (in configure.in) can't find them on the system.  See sec.
## 7.1.5 and 7.2.3 (9.1.5 and 9.2.3 in the online version) of "GNU Autoconf,
## Automake, and Libtool".

INCLUDES = -I$(top_builddir) -I$(top_srcdir) @GLIB_CFLAGS@

MAINTAINERCLEANFILES = Makefile.in

noinst_HEADERS = getstr.h g_strsplit_regexp.h
noinst_LTLIBRARIES = libreplace.la
libreplace_la_SOURCES =
libreplace_la_LIBADD = @LTLIBOBJS@
