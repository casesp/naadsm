## Makefile.am -- Process this file with automake to produce Makefile.in

SUBDIRS = . \
  model.disease \
  model.direct-contact-spread \
  model.indirect-contact-spread \
  model.airborne-spread \
  model.detection \
  model.control-measures \
  model.ring-destruction \
  model.ring-vaccination \
  model.zones

INCLUDES = -I$(top_srcdir) \
  -I$(top_srcdir)/src \
  -I$(top_srcdir)/herd \
  -I$(top_srcdir)/event \
  -I$(top_srcdir)/prob_dist \
  -I$(top_srcdir)/rel_chart \
  -I$(top_srcdir)/gis \
  -I$(top_srcdir)/reporting \
  -I$(top_srcdir)/zone \
  -I$(top_srcdir)/rng \
  -I$(top_srcdir)/wml
INCLUDES += @INCLTDL@
INCLUDES += @GLIB_CFLAGS@ @GSL_CFLAGS@ @SCEW_CFLAGS@ @SPRNG_CFLAGS@
AM_LDFLAGS = -L$(top_srcdir)/herd \
  -L$(top_srcdir)/event \
  -L$(top_srcdir)/prob_dist \
  -L$(top_srcdir)/rel_chart \
  -L$(top_srcdir)/gis \
  -L$(top_srcdir)/reporting \
  -L$(top_srcdir)/zone \
  -L$(top_srcdir)/rng \
  -L$(top_srcdir)/wml

# Create a program just like the real simulator, but without MPI, for running
# small tests.

noinst_PROGRAMS = minisim shell shell2 fcmp

minisim_SOURCES = $(top_srcdir)/src/main.c \
  $(top_srcdir)/src/model_loader.c $(top_srcdir)/src/model_loader.h \
  $(top_srcdir)/src/model.h \
  $(top_srcdir)/src/event_manager.c $(top_srcdir)/src/event_manager.h \
  $(top_srcdir)/src/herd_zone_updater.c $(top_srcdir)/src/herd_zone_updater.h \
  $(top_srcdir)/src/guilib.c $(top_srcdir)/src/guilib.h
minisim_CPPFLAGS = -DCANCEL_MPI=1 $(AM_CFLAGS)
minisim_LDADD = @LIBLTDL@ @DLPREOPEN@
minisim_LDADD += -lherd -levent -lprob_dist -lrel_chart -lgis -lreporting -lzone -lwml -lrng
minisim_LDADD += @LEXLIB@ @GLIB_LIBS@ @GSL_LIBS@ @SCEW_LIBS@ @SPRNG_LIBS@

shell_SOURCES = shell.c
shell_LDADD = @GLIB_LIBS@ $(top_builddir)/replace/libreplace.la

shell2_SOURCES = shell2.c
shell2_LDADD = @GLIB_LIBS@ $(top_builddir)/replace/libreplace.la

fcmp_SOURCES = fcmp.c
fcmp_LDADD = @GLIB_LIBS@ @GSL_LIBS@

# Create a C file for Doxygen that contains documentation for the test cases.
# Include that in the distribution file, but not the files that generated it.

dist_noinst_DATA = testsuite.c
testsuite_c_SOURCES = tests_start_tag.xml \
  model.disease/all.xml \
  model.direct-contact-spread/all.xml \
  model.indirect-contact-spread/all.xml \
  model.airborne-spread/all.xml \
  model.detection/all.xml \
  model.control-measures/all.xml \
  model.ring-destruction/all.xml \
  model.ring-vaccination/all.xml \
  model.zones/all.xml \
  tests_end_tag.xml

testsuite.c: $(testsuite_c_SOURCES)
	cat $^ | python xml2dox.py > $@

dist_noinst_SCRIPTS = xml2dox.py xml2expect.py

# Include the DejaGnu test files in the distribution file.

EXTRA_DIST = config/unix.exp 1herd.xml 2herds.xml \
  2herds_pigs_cattle_pigs_infected.xml 2herds_pigs_cattle_all_infected.xml \
  2targets_same_dist_same_size.xml 2targets_same_dist_diff_size.xml \
  3herds.xml 3herds_2_at_same_loc.xml 3targets_same_dist_same_size.xml \
  3herds_pigs_2cattle_pigs_infected.xml \
  3herds_pigs_cattle_sheep_sheep_infected.xml \
  3herds_pigs_cattle_sheep_pigs_sheep_infected.xml \
  3herds_pigs_cattle_sheep_all_infected.xml \
  4herds_line.xml 4herds_all_infected.xml 4herds_line_2.xml 4herds_line_3.xml \
  4herds_line_4.xml 4herds_2.xml \
  5herds.xml 5herds_very_close.xml 6herds.xml 6herds_2.xml 7herds.xml \
  8herds_all_infected.xml 8herds_2rows.xml 8herds_2rows_2.xml \
  herds_circle.xml herds_circle2.xml 16herds_square.xml \
  1herd_1dayleft.xml 1herd_3daysleft.xml \
  1herd_start_infectious.xml 1herd_start_infectious_1dayleft.xml \
  1herd_start_subclinical.xml \
  2herds_1_start_nimmune.xml 2herds_1_start_nimmune_2daysleft.xml \
  2herds_1_start_nimmune_3daysleft.xml 2herds_1_start_vimmune.xml \
  2herds_1_start_vimmune_1dayleft.xml 2herds_1_start_vimmune_2daysleft.xml \
  2herds_1_start_destroyed.xml \
  no_rtree_lat0.xml rtree_lat0.xml no_rtree_lat45.xml rtree_lat45.xml \
  no_rtree_contact_spread.xml rtree_contact_spread.xml \
  2herds_ascii.xml 2herds_iso88591.xml 2herds_utf8.xml 2herds_utf8_no_bom.xml \
  2herds_utf16_big_endian.xml 2herds_utf16_big_endian_no_bom.xml \
  2herds_utf16_little_endian.xml 2herds_utf16_little_endian_no_bom.xml \
  $(testsuite_c_SOURCES)
