## Makefile.am -- Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = dejagnu

SUBDIRS = . test

INCLUDES = \
  -I$(top_srcdir) \
  -I$(top_srcdir)/src \
  -I$(top_srcdir)/herd \
  -I$(top_srcdir)/event \
  -I$(top_srcdir)/prob_dist \
  -I$(top_srcdir)/rel_chart \
  -I$(top_srcdir)/gis \
  -I$(top_srcdir)/reporting \
  -I$(top_srcdir)/zone \
  -I$(top_srcdir)/rng \
  -I$(top_srcdir)/libltdl \
  @GLIB_CFLAGS@ @SCEW_CFLAGS@ @SPRNG_CFLAGS@
AM_LDFLAGS = \
  -L$(top_srcdir)/herd \
  -L$(top_srcdir)/event \
  -L$(top_srcdir)/prob_dist \
  -L$(top_srcdir)/rel_chart \
  -L$(top_srcdir)/gis \
  -L$(top_srcdir)/reporting \
  -L$(top_srcdir)/zone \
  -L$(top_srcdir)/rng \
  -L$(top_srcdir)/src

modelsdir = $(exec_prefix)/models
models_LTLIBRARIES = \
  conflict-resolver.la \
  disease-model.la \
  contact-spread-model.la \
  airborne-spread-model.la \
  airborne-spread-exponential-model.la \
  detection-model.la \
  detection-on-given-day-model.la \
  vaccine-model.la \
  resources-and-implementation-of-controls-model.la \
  quarantine-model.la \
  basic-destruction-model.la \
  trace-back-destruction-model.la \
  ring-destruction-model.la \
  ring-vaccination-model.la \
  zone-model.la \
  basic-zone-focus-model.la \
  trace-back-zone-focus-model.la \
  economic-model.la \
  destruction-monitor.la \
  destruction-list-monitor.la \
  trace-back-monitor.la \
  vaccination-monitor.la \
  vaccination-list-monitor.la \
  detection-monitor.la \
  exposure-monitor.la \
  infection-monitor.la \
  zone-monitor.la

conflict_resolver_la_SOURCES = conflict-resolver.c conflict-resolver.h
conflict_resolver_la_LIBADD = $(AM_LDFLAGS) -lherd @GLIB_LIBS@ -lm
conflict_resolver_la_LDFLAGS = -module -no-undefined -avoid-version

disease_model_la_SOURCES = disease-model.c disease-model.h model_util.c model_util.h
disease_model_la_LIBADD = $(AM_LDFLAGS) -lherd -lparameter -lprob_dist @GLIB_LIBS@ @SCEW_LIBS@ -lm
disease_model_la_LDFLAGS = -module -no-undefined -avoid-version

contact_spread_model_la_SOURCES = contact-spread-model.c contact-spread-model.h model_util.c model_util.h
contact_spread_model_la_LIBADD = $(AM_LDFLAGS) -lherd -levent -lparameter -lprob_dist -lrel_chart -lgis @GLIB_LIBS@ @SCEW_LIBS@ -lm
contact_spread_model_la_LDFLAGS = -module -no-undefined -avoid-version

airborne_spread_model_la_SOURCES = airborne-spread-model.c airborne-spread-model.h model_util.c model_util.h
airborne_spread_model_la_LIBADD = $(AM_LDFLAGS) -lherd -levent -lparameter -lprob_dist -lgis @GLIB_LIBS@ @GSL_LIBS@ @SCEW_LIBS@ -lm
airborne_spread_model_la_LDFLAGS = -module -no-undefined -avoid-version

airborne_spread_exponential_model_la_SOURCES = airborne-spread-exponential-model.c airborne-spread-exponential-model.h model_util.c model_util.h
airborne_spread_exponential_model_la_LIBADD = $(AM_LDFLAGS) -lherd -levent -lparameter -lprob_dist -lgis @GLIB_LIBS@ @GSL_LIBS@ @SCEW_LIBS@ -lm
airborne_spread_exponential_model_la_LDFLAGS = -module -no-undefined -avoid-version

detection_model_la_SOURCES = detection-model.c detection-model.h model_util.c model_util.h
detection_model_la_LIBADD = $(AM_LDFLAGS) -lherd -levent -lparameter -lrel_chart -lreporting @GLIB_LIBS@ @SCEW_LIBS@ @SPRNG_LIBS@
detection_model_la_LDFLAGS = -module -no-undefined -avoid-version

detection_on_given_day_model_la_SOURCES = detection-on-given-day-model.c detection-on-given-day-model.h model_util.c model_util.h
detection_on_given_day_model_la_LIBADD = $(AM_LDFLAGS) -lherd -levent -lparameter @GLIB_LIBS@ @SCEW_LIBS@ @SPRNG_LIBS@ -lm
detection_on_given_day_model_la_LDFLAGS = -module -no-undefined -avoid-version

vaccine_model_la_SOURCES = vaccine-model.c vaccine-model.h model_util.c model_util.h
vaccine_model_la_LIBADD = $(AM_LDFLAGS) -lherd -lparameter -lprob_dist @GLIB_LIBS@ @SCEW_LIBS@ -lm
vaccine_model_la_LDFLAGS = -module -no-undefined -avoid-version

resources_and_implementation_of_controls_model_la_SOURCES = resources-and-implementation-of-controls-model.c resources-and-implementation-of-controls-model.h
resources_and_implementation_of_controls_model_la_LIBADD = $(AM_LDFLAGS) -lherd -levent -lparameter -lrel_chart -lreporting @GLIB_LIBS@ @SCEW_LIBS@ -lm
resources_and_implementation_of_controls_model_la_LDFLAGS = -module -no-undefined -avoid-version

quarantine_model_la_SOURCES = quarantine-model.c quarantine-model.h
quarantine_model_la_LIBADD = $(AM_LDFLAGS) -lherd -lparameter @GLIB_LIBS@ @SCEW_LIBS@
quarantine_model_la_LDFLAGS = -module -no-undefined -avoid-version

basic_destruction_model_la_SOURCES = basic-destruction-model.c basic-destruction-model.h model_util.c model_util.h
basic_destruction_model_la_LIBADD = $(AM_LDFLAGS) -lherd -levent -lparameter @GLIB_LIBS@ @SCEW_LIBS@ -lm
basic_destruction_model_la_LDFLAGS = -module -no-undefined -avoid-version

trace_back_destruction_model_la_SOURCES = trace-back-destruction-model.c trace-back-destruction-model.h model_util.c model_util.h
trace_back_destruction_model_la_LIBADD = $(AM_LDFLAGS) -lherd -levent -lparameter -lreporting @GLIB_LIBS@ @SCEW_LIBS@ @SPRNG_LIBS@ -lm
trace_back_destruction_model_la_LDFLAGS = -module -no-undefined -avoid-version

ring_destruction_model_la_SOURCES = ring-destruction-model.c ring-destruction-model.h model_util.c model_util.h
ring_destruction_model_la_LIBADD = $(AM_LDFLAGS) -lherd -levent -lparameter -lgis @GLIB_LIBS@ @SCEW_LIBS@ -lm
ring_destruction_model_la_LDFLAGS = -module -no-undefined -avoid-version

ring_vaccination_model_la_SOURCES = ring-vaccination-model.c ring-vaccination-model.h model_util.c model_util.h
ring_vaccination_model_la_LIBADD = $(AM_LDFLAGS) -lherd -levent -lparameter -lgis @GLIB_LIBS@ @SCEW_LIBS@ -lm
ring_vaccination_model_la_LDFLAGS = -module -no-undefined -avoid-version

zone_model_la_SOURCES = zone-model.c zone-model.h
zone_model_la_LIBADD = $(AM_LDFLAGS) -lparameter @GLIB_LIBS@
zone_model_la_LDFLAGS = -module -no-undefined -avoid-version

basic_zone_focus_model_la_SOURCES = basic-zone-focus-model.c basic-zone-focus-model.h model_util.c model_util.h
basic_zone_focus_model_la_LIBADD = $(AM_LDFLAGS) -lparameter @GLIB_LIBS@
basic_zone_focus_model_la_LDFLAGS = -module -no-undefined -avoid-version

trace_back_zone_focus_model_la_SOURCES = trace-back-zone-focus-model.c trace-back-zone-focus-model.h model_util.c model_util.h
trace_back_zone_focus_model_la_LIBADD = $(AM_LDFLAGS) -lparameter @GLIB_LIBS@
trace_back_zone_focus_model_la_LDFLAGS = -module -no-undefined -avoid-version

economic_model_la_SOURCES = economic-model.c economic-model.h
economic_model_la_LIBADD = $(AM_LDFLAGS) -lherd -lparameter -lrel_chart -lreporting @GLIB_LIBS@ @SCEW_LIBS@
economic_model_la_LDFLAGS = -module -no-undefined -avoid-version

destruction_monitor_la_SOURCES = destruction-monitor.c destruction-monitor.h
destruction_monitor_la_LIBADD = $(AM_LDFLAGS) -lherd -lreporting @GLIB_LIBS@
destruction_monitor_la_LDFLAGS = -module -no-undefined -avoid-version

destruction_list_monitor_la_SOURCES = destruction-list-monitor.c
destruction_list_monitor_la_LIBADD = $(AM_LDFLAGS) -lherd -lreporting @GLIB_LIBS@
destruction_list_monitor_la_LDFLAGS = -module -no-undefined -avoid-version

trace_back_monitor_la_SOURCES = trace-back-monitor.c trace-back-monitor.h
trace_back_monitor_la_LIBADD = $(AM_LDFLAGS) -lherd -lreporting @GLIB_LIBS@
trace_back_monitor_la_LDFLAGS = -module -no-undefined -avoid-version

vaccination_monitor_la_SOURCES = vaccination-monitor.c vaccination-monitor.h
vaccination_monitor_la_LIBADD = $(AM_LDFLAGS) -lherd -lreporting @GLIB_LIBS@
vaccination_monitor_la_LDFLAGS = -module -no-undefined -avoid-version

vaccination_list_monitor_la_SOURCES = vaccination-list-monitor.c
vaccination_list_monitor_la_LIBADD = $(AM_LDFLAGS) -lherd -lreporting @GLIB_LIBS@
vaccination_list_monitor_la_LDFLAGS = -module -no-undefined -avoid-version

detection_monitor_la_SOURCES = detection-monitor.c detection-monitor.h
detection_monitor_la_LIBADD = $(AM_LDFLAGS) -lreporting @GLIB_LIBS@ @SCEW_LIBS@
detection_monitor_la_LDFLAGS = -module -no-undefined -avoid-version

exposure_monitor_la_SOURCES = exposure-monitor.c exposure-monitor.h
exposure_monitor_la_LIBADD = $(AM_LDFLAGS) -lreporting @GLIB_LIBS@ @SCEW_LIBS@
exposure_monitor_la_LDFLAGS = -module -no-undefined -avoid-version

infection_monitor_la_SOURCES = infection-monitor.c infection-monitor.h
infection_monitor_la_LIBADD = $(AM_LDFLAGS) -lparameter -lreporting @GLIB_LIBS@ @SCEW_LIBS@ -lm
infection_monitor_la_LDFLAGS = -module -no-undefined -avoid-version

zone_monitor_la_SOURCES = zone-monitor.c zone-monitor.h
zone_monitor_la_LIBADD = $(AM_LDFLAGS) -lreporting @GLIB_LIBS@ @SCEW_LIBS@ -lm
zone_monitor_la_LDFLAGS = -module -no-undefined -avoid-version

RUNTESTDEFAULTFLAGS = --tool model --debug -v -v SHELL=$$srcdir/test/shell --srcdir $$srcdir/test

# The distclean-DEJAGNU rule, run as part of "make distclean", will delete
# site.exp, site.bak, and .sum and .log files with the top-level package name
# (sim).  Also delete the .sum and .log files for the models.

DISTCLEANFILES = dbg.log model.log model.sum
