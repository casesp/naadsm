# naadsm4.pro - Qt Creator project for the NAADSM core library, version 4.x
# -------------------------------------------------------------------------
# Begin: 2012/02/29
# Last revision: $Date: 2013-06-26 22:51:06 $ $Author: areeves $
# Version: $Revision: 1.2.2.2 $
# Project:NAADSM
# Website: http://www.naadsm.org
# Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
# --------------------------------------------------
# Copyright (C) 2012 - 2013 NAADSM Development Team

## Define one or the other here...
DEFINES += \
##  BUILD_APP
  BUILD_DLL

## These definitions apply to all Windows builds
DEFINES += \
  HAVE_CONFIG_H \
  WIN_DLL \
  NO_PRAGMA_COMMENT_WARNINGS  
  
## The following definitions are used to select experimental versions.
## For the base version of NAADSM, none of these symbols should be defined.
DEFINES += \
#  CHEYENNE
#  LARAMIE

## Change the name of the target, depending on the defined symbols.
contains( DEFINES, BUILD_APP ) {
  TEMPLATE = app
  contains( DEFINES, CHEYENNE ) {
    TARGET = cheyenne4sc
  } else {
    contains( DEFINES, LARAMIE ) {
      TARGET = laramie4sc
    } else {
      TARGET = naadsm4sc
    }
  }
} else {
  contains( DEFINES, BUILD_DLL ) {
    TEMPLATE = lib
    DEFINES += DLL_EXPORTS
    contains( DEFINES, CHEYENNE ) {
      TARGET = cheyenne4
    } else {
      contains( DEFINES, LARAMIE ) {
        TARGET = laramie4
      } else {
        TARGET = naadsm4
      }
    }
  }
}

QT       -= core gui

INCLUDEPATH += \
    C:/libs/C_libs/glib-2.22.2/include/glib-2.0 \
    C:/libs/C_libs/gpc-2.32/include \
    C:/libs/C_libs/gsl-1.8/include \
    C:/libs/C_libs/rtree/include \
    C:/libs/C_libs/scew-0.4.0/include \
    C:/libs/C_libs/sprng-2.0a_naadsm/include \
    C:/libs/C_libs/expat-1.95.8/include \
    C:/libs/C_libs/regex-0.12/include \
    C:/libs/C_libs/proj-4.6.1/include \
    ../../dll \
    ../../src \
    ../../herd \
    ../../gis \
    ../../wml \
    ../../event \
    ../../zone \
    ../../prob_dist \
    ../../rel_chart \
    ../../reporting \
    ../../models \
    ../../rng \
    ../../general \
    ../../spatial_search \
    ../../wml \
    ../../replace

LIBS += \
    C:/libs/C_libs/glib-2.22.2/lib/glib-2.0.lib \
    C:/libs/C_libs/gsl-1.8/lib/libgsl.a \
    C:/libs/C_libs/gsl-1.8/lib/libgslcblas.a \
    C:/libs/C_libs/sprng-2.0a_naadsm/lib/libsprng.lib \
    C:/libs/C_libs/rtree/lib/librtree.a \
    C:/libs/C_libs/gpc-2.32/lib/libgpcl.a \
    C:/libs/C_libs/regex-0.12/lib/libregex.a \
    C:/libs/C_libs/expat-1.95.8/lib/libexpat.a \
    C:/libs/C_libs/scew-0.4.0/lib/libscew.a \
    C:/libs/C_libs/proj-4.6.1/lib/proj.lib

SOURCES += \
    ../../models/zone-monitor.c \
    ../../models/zone-model.c \
    ../../models/vaccine-model.c \
    ../../models/trace-zone-focus-model.c \
    ../../models/trace-quarantine-model.c \
    ../../models/trace-model.c \
    ../../models/trace-exam-model.c \
    ../../models/trace-destruction-model.c \
    ../../models/test-model.c \
    ../../models/ring-vaccination-model.c \
    ../../models/ring-destruction-model.c \
    ../../models/resources-and-implementation-of-controls-model.c \
    ../../models/quarantine-model.c \
    ../../models/prophylactic-vaccination-model.c \
    ../../models/model_util.c \
    ../../models/local-area-spread-model.c \
    ../../models/disease-model.c \
    ../../models/detection-model.c \
    ../../models/contact-spread-model.c \
    ../../models/contact-recorder-model.c \
    ../../models/conflict-resolver.c \
    ../../models/basic-zone-focus-model.c \
    ../../models/basic-destruction-model.c \
    ../../models/basic-dcd-model.c \
    ../../models/airborne-spread-model.c \
    ../../herd/scorecard.c \
    ../../herd/herd-randomizer.c \
    ../../herd/herd.c \
    ../../event/event.c \
    ../../general/general.c \
    ../../gis/gis.c \
    ../../prob_dist/prob_dist.c \
    ../../rel_chart/rel_chart.c \
    ../../replace/replace.c \
    ../../reporting/reporting.c \
    ../../rng/rng.c \
    ../../spatial_search/spatial_search.c \
    ../../spatial_search/ch2d.c \
    ../../src/parameter.c \
    ../../src/naadsm.c \
    ../../src/model_loader.c \
    ../../src/main.c \
    ../../src/event_manager.c \
    ../../zone/zone.c \
    ../../wml/wml.c

contains( DEFINES, BUILD_APP ) {
  SOURCES += ../../src/cli.c
}    
    
HEADERS +=\
    ../../models/zone-model.h \
    ../../models/vaccine-model.h \
    ../../models/trace-zone-focus-model.h \
    ../../models/trace-quarantine-model.h \
    ../../models/trace-model.h \
    ../../models/trace-exam-model.h \
    ../../models/trace-destruction-model.h \
    ../../models/test-model.h \
    ../../models/ring-vaccination-model.h \
    ../../models/ring-destruction-model.h \
    ../../models/resources-and-implementation-of-controls-model.h \
    ../../models/quarantine-model.h \
    ../../models/prophylactic-vaccination-model.h \
    ../../models/model_util.h \
    ../../models/local-area-spread-model.h \
    ../../models/disease-model.h \
    ../../models/detection-model.h \
    ../../models/contact-spread-model.h \
    ../../models/contact-recorder-model.h \
    ../../models/conflict-resolver.h \
    ../../models/basic-zone-focus-model.h \
    ../../models/basic-destruction-model.h \
    ../../models/basic-dcd-model.h \
    ../../models/airborne-spread-model.h \
    ../../models/zone-monitor.h \
    ../../herd/herd-randomizer.h \
    ../../herd/herd.h \
    ../../herd/scorecard.h \
    ../../event/event.h \
    ../../general/general.h \
    ../../gis/gis.h \
    ../../prob_dist/prob_dist.h \
    ../../rel_chart/rel_chart.h \
    ../../replace/replace.h \
    ../../reporting/reporting.h \
    ../../rng/rng.h \
    ../../spatial_search/ch2d.h \
    ../../spatial_search/spatial_search.h \
    ../../src/naadsm.h \
    ../../src/model_loader.h \
    ../../src/model.h \
    ../../src/event_manager.h \
    ../../src/parameter.h \
    ../../zone/zone.h \
    ../config.h \
    ../../wml/wml.h

win32:RC_FILE = ../naadsm_private.rc

































