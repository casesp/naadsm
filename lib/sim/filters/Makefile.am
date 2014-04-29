## Makefile.am -- Process this file with automake to produce Makefile.in

INCLUDES = -I.. -I$(top_srcdir)/herd -I$(top_srcdir)/rel_chart -I$(top_srcdir)/gis -I$(top_srcdir)/event -I$(top_srcdir)/reporting @GLIB_CFLAGS@ @GSL_CFLAGS@ @SCEW_CFLAGS@
AM_LDFLAGS = -L.. -L$(top_srcdir)/herd -L$(top_srcdir)/gis -L$(top_srcdir)/wml -L$(top_srcdir)/reporting

AM_YFLAGS = -d -v

bin_PROGRAMS = \
  table_filter \
  full_table_filter \
  exposures_table_filter \
  apparent_events_table_filter \
  state_table_filter \
  progress_meter \
  xml2shp \
  weekly_gis_filter \
  summary_gis_filter \
  shp2png \
  shp2png2 \
  weekly_gis_zones_filter
table_filter_SOURCES = table.y scanner.l
table_filter_LDADD = -lreporting $(top_builddir)/replace/libreplace.la @GSL_LIBS@
full_table_filter_SOURCES = full_table.y scanner.l
full_table_filter_LDADD = -lreporting $(top_builddir)/replace/libreplace.la @GSL_LIBS@
exposures_table_filter_SOURCES = exposures_table.y scanner.l
exposures_table_filter_LDADD = -lherd -lwml -lreporting $(top_builddir)/replace/libreplace.la @GSL_LIBS@
apparent_events_table_filter_SOURCES = apparent_events_table.y scanner.l
apparent_events_table_filter_LDADD = -lherd -lwml -lreporting $(top_builddir)/replace/libreplace.la @GSL_LIBS@
state_table_filter_SOURCES = state_table.y scanner.l
state_table_filter_LDADD = $(top_builddir)/replace/libreplace.la @GLIB_LIBS@
progress_meter_SOURCES = progress_meter.c
progress_meter_LDADD = $(top_builddir)/replace/libreplace.la @GLIB_LIBS@ @SCEW_LIBS@
xml2shp_SOURCES = xml2shp.c
xml2shp_LDADD = -lherd -lshp
weekly_gis_filter_SOURCES = weekly_gis.y scanner.l
weekly_gis_filter_LDADD = -lherd -lshp @GLIB_LIBS@
summary_gis_filter_SOURCES = summary_gis.y scanner.l
summary_gis_filter_LDADD = -lherd -lshp -lgis @GLIB_LIBS@
shp2png_SOURCES = shp2png.c
shp2png_LDADD = -lshp -lgd @GLIB_LIBS@
shp2png2_SOURCES = shp2png2.c
shp2png2_LDADD = -lshp -lgd @GLIB_LIBS@
weekly_gis_zones_filter_SOURCES = weekly_gis_zones.y scanner.l
weekly_gis_zones_filter_LDADD = -lshp @GLIB_LIBS@

# Include the DejaGnu test files in the distribution file.

# EXTRA_DIST = config/unix.exp libherd.all/*.exp
