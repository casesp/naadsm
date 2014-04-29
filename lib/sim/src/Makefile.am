## Makefile.am -- Process this file with automake to produce Makefile.in

INCLUDES = -I$(top_srcdir) \
  -I$(top_srcdir)/herd \
  -I$(top_srcdir)/event \
  -I$(top_srcdir)/prob_dist \
  -I$(top_srcdir)/rel_chart \
  -I$(top_srcdir)/gis \
  -I$(top_srcdir)/reporting \
  -I$(top_srcdir)/rng \
  -I$(top_srcdir)/zone \
  -I$(top_srcdir)/wml
INCLUDES += @INCLTDL@
INCLUDES += @GLIB_CFLAGS@ @GSL_CFLAGS@ @SCEW_CFLAGS@ @SPRNG_CFLAGS@
if HAVE_MPI
INCLUDES += @MPI_CFLAGS@
endif
AM_LDFLAGS = -L$(top_srcdir)/herd \
  -L$(top_srcdir)/event \
  -L$(top_srcdir)/prob_dist \
  -L$(top_srcdir)/rel_chart \
  -L$(top_srcdir)/gis \
  -L$(top_srcdir)/reporting \
  -L$(top_srcdir)/rng \
  -L$(top_srcdir)/zone \
  -L$(top_srcdir)/wml

lib_LTLIBRARIES = libparameter.la

bin_PROGRAMS = sim
sim_SOURCES = main.c model_loader.c model_loader.h model.h \
  event_manager.c event_manager.h herd_zone_updater.c herd_zone_updater.h \
  guilib.c guilib.h
if HAVE_MPI
sim_SOURCES += mpix.c mpix.h
endif
sim_LDADD = @LIBLTDL@ @DLPREOPEN@
sim_LDADD += -lherd -levent -lprob_dist -lrel_chart -lgis -lreporting -lzone -lwml -lrng
sim_LDADD += @LEXLIB@ @GLIB_LIBS@ @GSL_LIBS@ @SCEW_LIBS@ @SPRNG_LIBS@
if HAVE_MPI
sim_LDADD += @MPI_LIBS@
endif

libparameter_la_SOURCES = parameter.c parameter.h
libparameter_la_LIBADD = -lprob_dist -lrel_chart
