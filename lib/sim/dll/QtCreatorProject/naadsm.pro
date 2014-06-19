# naadsm.pro - Qt Creator project for the NAADSM core library, version 3.x
# ------------------------------------------------------------------------
# Begin: 2012/02/29
# Last revision: $Date: 2013-06-26 22:51:06 $ $Author: areeves $
# Version: $Revision: 1.2.2.2 $
# Project:NAADSM
# Website: http://www.naadsm.org
# Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
# --------------------------------------------------
# Copyright (C) 2012 - 2013 NAADSM Development Team
#
# This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
# Public License as published by the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.

## Disable warnings about comparison between signed and unsigned integers, unused parameters, and set but unused variables.
## Legacy NAADSM code does it EVERYHWERE, which is incredibly annoying.
CONFIG += warn_on
QMAKE_CFLAGS_WARN_ON += -Wno-sign-compare -Wno-unused-parameter -Wno-unused-but-set-variable


DEFINES += \
  HAVE_CONFIG_H \              ## Required, evidently.
  CPPOUTPUT \                  ## Optional.  Use if building a version with CPP/Linux/Qt/Database outputs.
  #DLL_EXPORTS \               ## Optional. Use if building a DLL on Windows.
  BUILD_FOR_WINDOWS \          ## Required on Windows. Removes nonessential code that causes errors when executed on Windows.
  NO_PRAGMA_COMMENT_WARNINGS   ## ???

  
## The following definitions are used to select experimental versions.
## A maximum of one should be defined at a time.
## For the base version of NAADSM, none of these symbols should be defined.
DEFINES += \
#  CHEYENNE
#  LARAMIE
#  RIVERTON


contains( DEFINES, CPPOUTPUT ) {
  QMAKE_CFLAGS_WARN_ON += -fpermissive -Wno-write-strings ## Let the compiler get by without explicitly casting everything everywhere.
  QMAKE_CC = g++  ## Treat C code as C++ to enable compiling.
  QT -= gui
  QT += core sql
  CONFIG  += console
} else {
  QT -= core gui
}

## Change the name of the target, depending on the defined symbols.
contains( DEFINES, CHEYENNE ) {
  TARGET = cheyenne
} else {
  contains( DEFINES, LARAMIE ) {
    TARGET = laramie
  } else {
    contains( DEFINES, RIVERTON ) {
      TARGET = riverton
    } else {
      TARGET = naadsm
    }
  }
}

contains( DEFINES, DLL_EXPORTS ) {
  TEMPLATE = lib
} else {
  TEMPLATE = app
}

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
    C:/libs/C_libs/popt-1.8.1/include \
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

contains( DEFINES, CPPOUTPUT ) {
  INCLUDEPATH += \
    ../../cppoutputs \
   ../../
}

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
    C:/libs/C_libs/proj-4.6.1/lib/proj.lib \
    C:/libs/C_libs/popt-1.8.1/lib/libpopt.lib

SOURCES += \
    ../../models/zone-monitor.c \
    ../../models/zone-model.c \
    ../../models/vaccine-model.c \
    ../../models/trace-zone-focus-model.c \
    ../../models/trace-quarantine-model.c \
    ../../models/trace-model.c \
    ../../models/trace-exam-model.c \
    ../../models/trace-destruction-model.c \
    ../../models/trace-back-destruction-model.c \
    ../../models/test-model.c \
    ../../models/ring-vaccination-model.c \
    ../../models/ring-destruction-model.c \
    ../../models/resources-and-implementation-of-controls-model.c \
    ../../models/quarantine-model.c \
    ../../models/model_util.c \
    ../../models/disease-model.c \
    ../../models/detection-model.c \
    ../../models/contact-spread-model.c \
    ../../models/contact-recorder-model.c \
    ../../models/conflict-resolver.c \
    ../../models/basic-zone-focus-model.c \
    ../../models/basic-destruction-model.c \
    ../../models/airborne-spread-model.c \
    ../../models/airborne-spread-exponential-model.c \    
    ../../models/destruction-list-monitor.c \
    ../../models/destruction-monitor.c \
    ../../models/detection-monitor.c \
    ../../models/economic-model.c \
    ../../models/exposure-monitor.c \
    ../../models/infection-monitor.c \
    ../../models/vaccination-list-monitor.c \
    ../../models/vaccination-monitor.c \
    ../../models/trace-back-zone-focus-model.c \
    ../../models/trace-back-monitor.c \
    ../../models/exam-monitor.c \
    ../../models/test-monitor.c \
    ../../models/trace-monitor.c \    
    ../../herd/herd.c \
    ../../herd/herd-randomizer.c \
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
    ../../src/herd_zone_updater.c \
    ../../zone/zone.c \
    ../../wml/wml.c \
    ../../cppoutputs/csmdatabase.cpp \
    ../../cppoutputs/csmsimulation.cpp \
    ../../cppoutputs/naadsmlibrary.cpp \
    ../../cppoutputs/czone.cpp \
    ../../cppoutputs/cprodtype.cpp \
    ../../cppoutputs/csmptoutput.cpp \
    ../../ar_general_purpose/cencryption.cpp \
    ../../ar_general_purpose/qcout.cpp \
    ../../ar_general_purpose/strutils.cpp \
    ../../ar_general_purpose/qmath.cpp \
    ../../ar_general_purpose/ccmdline.cpp

HEADERS +=\
    ../../models/zone-model.h \
    ../../models/vaccine-model.h \
    ../../models/trace-zone-focus-model.h \
    ../../models/trace-quarantine-model.h \
    ../../models/trace-model.h \
    ../../models/trace-exam-model.h \
    ../../models/trace-destruction-model.h \
    ../../models/trace-back-destruction-model.h \
    ../../models/test-model.h \
    ../../models/ring-vaccination-model.h \
    ../../models/ring-destruction-model.h \
    ../../models/resources-and-implementation-of-controls-model.h \
    ../../models/quarantine-model.h \
    ../../models/model_util.h \
    ../../models/disease-model.h \
    ../../models/detection-model.h \
    ../../models/contact-spread-model.h \
    ../../models/contact-recorder-model.h \
    ../../models/conflict-resolver.h \
    ../../models/basic-zone-focus-model.h \
    ../../models/basic-destruction-model.h \
    ../../models/airborne-spread-model.h \
    ../../models/airborne-spread-exponential-model.h \
    ../../models/zone-monitor.h \
    ../../models/destruction-list-monitor.h \
    ../../models/destruction-monitor.h \
    ../../models/detection-monitor.h \
    ../../models/economic-model.h \
    ../../models/exposure-monitor.h \
    ../../models/infection-monitor.h \
    ../../models/vaccination-list-monitor.h \
    ../../models/vaccination-monitor.h \
    ../../models/trace-back-zone-focus-model.h \
    ../../models/trace-back-monitor.h \
    ../../models/exam-monitor.h \
    ../../models/test-monitor.h \
    ../../models/trace-monitor.h \
    ../../herd/herd.h \
    ../../herd/herd-randomizer.h \
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
    ../../src/herd_zone_updater.h \
    ../../zone/zone.h \
    ../config.h \
    ../../wml/wml.h \
    ../../cppoutputs/csmdatabase.h \
    ../../cppoutputs/csmsimulation.h \
    ../../cppoutputs/naadsmlibrary.h \
    ../../cppoutputs/czone.h \
    ../../cppoutputs/cprodtype.h \
    ../../cppoutputs/csmptoutput.h \
    ../../ar_general_purpose/cencryption.h \
    ../../ar_general_purpose/qcout.h \
    ../../ar_general_purpose/strutils.h \
    ../../ar_general_purpose/qmath.h \
    ../../ar_general_purpose/ccmdline.h \
    ../../src/main.h

win32:RC_FILE = ../naadsm_private.rc

































