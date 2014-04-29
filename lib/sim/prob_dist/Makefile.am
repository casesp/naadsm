## Makefile.am -- Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = dejagnu

SUBDIRS = . test

INCLUDES = -I$(top_srcdir) -I$(top_srcdir)/rng @GLIB_CFLAGS@ @SPRNG_CFLAGS@ -DG_LOG_DOMAIN=\"prob_dist\"
AM_LDFLAGS = -L$(top_srcdir)/rng

lib_LTLIBRARIES = libprob_dist.la
include_HEADERS = prob_dist.h

libprob_dist_la_SOURCES = prob_dist.c prob_dist.h
libprob_dist_la_LIBADD = @GLIB_LIBS@ @GSL_LIBS@ -lrng @SPRNG_LIBS@

RUNTESTDEFAULTFLAGS = --tool libprob_dist --debug -v -v SHELL=$$srcdir/test/shell --srcdir $$srcdir/test

# The distclean-DEJAGNU rule, run as part of "make distclean", will delete
# site.exp, site.bak, and .sum and .log files with the top-level package name
# (sim).  Also delete the .sum and .log files for this library.

DISTCLEANFILES = dbg.log libprob_dist.log libprob_dist.sum
