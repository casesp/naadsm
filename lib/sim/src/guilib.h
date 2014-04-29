/** @file guilib.h
 *
 * @author Aaron Reeves <Aaron.Reeves@colostate.edu><br>
 *   Animal Population Health Institute<br>
 *   College of Veterinary Medicine and Biomedical Sciences<br>
 *   Colorado State University<br>
 *   Fort Collins, CO 80523<br>
 *   USA
 * @version 0.1
 * @date June 2005
 *
 * Copyright &copy; 2005 - 2006 Animal Population Health Institute, 
 * Colorado State University
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#ifndef GUILIB_H
#define GUILIB_H

#if defined(DLL_EXPORTS)
# define DLL_API __declspec( dllexport )
#elif defined(DLL_IMPORTS)
# define DLL_API __declspec( dllimport )
#else
# define DLL_API
#endif

#include <glib.h>
#include "herd.h"
#include "zone.h"

/* Functions for version tracking */
/*--------------------------------*/
/** Returns the oldest version of the GUI application
that is compatible with this DLL */
DLL_API char *minimum_gui_version (void);

/** Returns the version of the model specification that
this application or DLL is intended to comply with */
DLL_API char *specification_version (void);


/* Function pointer types */
/*------------------------*/
typedef void (*TFnVoid_1_CharP) (char *);
typedef void (*TFnVoid_1_Int) (int);
typedef void (*TFnVoid_1_THRDUpdate) (HRD_update_t);
typedef void (*TFnVoid_1_THRDExpose) (HRD_expose_t);
typedef void (*TFnVoid_1_THRDZone) (HRD_zone_t);
typedef void (*TFnVoid_0) (void);
typedef int (*TFnInt_0) (void);
typedef void (*TFnVoid_1_THRDPerimeterList) (ZON_zone_list_t *);
typedef void (*TFnVoid_2_Int_Double) (int, double);
typedef void (*TFnVoid_5_Int_Int_Int_Int_Int) (int, int, int, int, int);

/* Function pointers */
/*-------------------*/
/* For the display of debugging information in the GUI */
TFnVoid_1_CharP guilib_printf;
TFnVoid_1_CharP guilib_debug;

/* For key simulation- and iteration-level events */
TFnVoid_0 guilib_sim_start;
TFnVoid_1_Int guilib_iteration_start;
TFnVoid_1_Int guilib_day_start;
TFnVoid_1_Int guilib_day_complete;
TFnVoid_1_Int guilib_disease_end;
TFnVoid_1_Int guilib_outbreak_end;
TFnVoid_1_Int guilib_iteration_complete;
TFnVoid_1_Int guilib_sim_complete;

/* Used to determine when iterations should end */
TFnInt_0 guilib_stop_on_disease_end;
TFnVoid_0 guilib_reset_detection_end;
TFnInt_0 guilib_stop_on_detection;

/* Used to update herd status and related events as an iteration runs */
TFnVoid_1_THRDUpdate guilib_change_herd_state;
TFnVoid_1_THRDUpdate guilib_infect_herd;
TFnVoid_1_THRDUpdate guilib_expose_herd;
TFnVoid_1_THRDUpdate guilib_detect_herd;
TFnVoid_1_THRDUpdate guilib_attempt_trace_herd;
/* TFnVoid_1_THRDUpdate guilib_trace_herd; *//* Not currently used */
TFnVoid_1_THRDUpdate guilib_destroy_herd;
TFnVoid_1_THRDUpdate guilib_vaccinate_herd;
TFnVoid_1_THRDExpose guilib_record_exposure;
TFnVoid_1_Int guilib_make_zone_focus;
TFnVoid_1_THRDZone guilib_record_zone_change;
TFnVoid_2_Int_Double guilib_record_zone_area;

/* Used by the GUI to access zone information during a running simulation */
TFnVoid_1_THRDPerimeterList guilib_set_zone_perimeters;

/* Used to write daily herd state output, when desired */
TFnVoid_1_CharP guilib_show_all_states;

/* Used to write daily herd prevalence output, when desired */
TFnVoid_1_CharP guilib_show_all_prevalences;

/* Used to write daily herd zone output, when desired */
/* This function will need to be re-implemented if it is ever needed again. */
/* TFnVoid_1_CharP guilib_show_all_zones; */

/* Used to determine whether the user wants to interrupt a running simulation */
TFnInt_0 guilib_simulation_stop;

TFnVoid_5_Int_Int_Int_Int_Int guilib_report_search_hits;


/* Functions used to set the function pointers */
/*---------------------------------------------*/
DLL_API void set_printf (TFnVoid_1_CharP fn);
DLL_API void set_debug (TFnVoid_1_CharP fn);

DLL_API void set_sim_start (TFnVoid_0 fn);
DLL_API void set_iteration_start (TFnVoid_1_Int fn);
DLL_API void set_day_start (TFnVoid_1_Int fn);
DLL_API void set_day_complete (TFnVoid_1_Int fn);
DLL_API void set_disease_end (TFnVoid_1_Int fn);
DLL_API void set_outbreak_end (TFnVoid_1_Int fn);
DLL_API void set_iteration_complete (TFnVoid_1_Int fn);
DLL_API void set_sim_complete (TFnVoid_1_Int fn);

DLL_API void set_stop_on_disease_end (TFnInt_0 fn);
DLL_API void set_reset_detection_end (TFnVoid_0 fn);
DLL_API void set_stop_on_detection (TFnInt_0 fn);

DLL_API void set_change_herd_state (TFnVoid_1_THRDUpdate fn);
DLL_API void set_infect_herd (TFnVoid_1_THRDUpdate fn);
DLL_API void set_expose_herd (TFnVoid_1_THRDUpdate fn);
DLL_API void set_detect_herd (TFnVoid_1_THRDUpdate fn);
DLL_API void set_attempt_trace_herd (TFnVoid_1_THRDUpdate fn);
/* DLL_API void set_trace_herd( TFnVoid_1_THRDUpdate fn ); *//* Not currently used */
DLL_API void set_destroy_herd (TFnVoid_1_THRDUpdate fn);
DLL_API void set_vaccinate_herd (TFnVoid_1_THRDUpdate fn);
DLL_API void set_record_exposure (TFnVoid_1_THRDExpose fn);
DLL_API void set_make_zone_focus( TFnVoid_1_Int fn );
DLL_API void set_record_zone_change (TFnVoid_1_THRDZone fn);
DLL_API void set_record_zone_area (TFnVoid_2_Int_Double fn);

DLL_API void set_set_zone_perimeters( TFnVoid_1_THRDPerimeterList fn);
DLL_API unsigned int get_zone_list_length( ZON_zone_list_t *zones );
DLL_API ZON_zone_t *get_zone_from_list( ZON_zone_list_t * zones, int i);

DLL_API void set_show_all_states (TFnVoid_1_CharP fn);
DLL_API void set_show_all_prevalences (TFnVoid_1_CharP fn);

/* This function will need to be re-implemented if it is ever needed again. */
/* DLL_API void set_show_all_zones (TFnVoid_1_CharP fn); */

DLL_API void set_simulation_stop (TFnInt_0 fn);

DLL_API void set_report_search_hits (TFnVoid_5_Int_Int_Int_Int_Int fn); 


/* Function pointer helpers */
/*--------------------------*/
void clear_guilib_fns (void);


/* Some other potentially useful functions */
/*-----------------------------------------*/
DLL_API double
calculate_distance (double lat1, double lon1, double lat2, double lon2);


#endif /* GUILIB_H */
