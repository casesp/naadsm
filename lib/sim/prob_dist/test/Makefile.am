## Makefile.am -- Process this file with automake to produce Makefile.in

INCLUDES = -I.. -I$(top_srcdir)/rng @GLIB_CFLAGS@ @SPRNG_CFLAGS@
AM_LDFLAGS = -L.. -L$(top_srcdir)/rng

AM_YFLAGS = -d -v

noinst_PROGRAMS = shell fcmp
shell_SOURCES = shell.y scanner.l
shell_LDADD = -lprob_dist -lrng @SPRNG_LIBS@

fcmp_SOURCES = fcmp.c
fcmp_LDADD = @GLIB_LIBS@ @GSL_LIBS@

# Include the DejaGnu test files in the distribution file.

EXTRA_DIST = config/unix.exp libprob_dist.all/*.exp
