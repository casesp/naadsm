/** @file guilib.c
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
 * Copyright &copy; 2005 - 2008 Animal Population Health Institute, 
 * Colorado State University
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */


#include "guilib.h"
#include <glib.h>

#include "gis.h"
#include "rng.h"

#ifdef DLL_EXPORTS
#include <windows.h>

#if defined( CHEYENNE )
  #if defined( LARAMIE )
    /* This block will lead to compiler errors if symbols for multiple
     * experimental versions are defined. Note that these experimental
     * versions are currently mutually exlusive, and only one symbol
     * should be defined at a time.
    */
    #error "Only one experimental version may be defined at a time."
  #endif
#endif

BOOL APIENTRY
DllMain (HINSTANCE hInst /* Library instance handle. */ ,
         DWORD reason /* Reason this function is being called. */ ,
         LPVOID reserved /* Not used. */ )
{
  switch (reason)
    {
    case DLL_PROCESS_ATTACH:
      /* printf( "@@@ DLL LOADED @@@" ); */
      clear_guilib_fns ();
      clear_rng_fns ();
      break;
    case DLL_PROCESS_DETACH:
      break;
    case DLL_THREAD_ATTACH:
      break;
    case DLL_THREAD_DETACH:
      break;
    }

  /* Returns TRUE on success, FALSE on failure */
  return TRUE;
}

#endif /* DLL_EXPORTS */


/* Functions for version tracking */
/*--------------------------------*/
/** Returns the oldest version of the GUI application
that is compatible with this DLL */
DLL_API char *
minimum_gui_version (void)
{
  #if defined( CHEYENNE )
    return "3.1.18-CHEYENNE";
  #elif defined( LARAMIE )
    return "3.1.18-LARAMIE";
  #else
    return "3.1.18";
  #endif
}

/** Returns the version of the model specification that
this application or DLL is intended to comply with */
DLL_API char *
specification_version (void)
{
  return "1.1.0";
}



/* Functions used to set the function pointers */
/*---------------------------------------------*/

/* For the display of debugging information in the GUI */
DLL_API void
set_printf (TFnVoid_1_CharP fn)
{
  guilib_printf = fn;
}


DLL_API void
set_debug (TFnVoid_1_CharP fn)
{
  guilib_debug = fn;
}



/* For key simulation- and iteration-level events */
DLL_API void
set_sim_start (TFnVoid_0 fn)
{
  guilib_sim_start = fn;
}


DLL_API void
set_iteration_start (TFnVoid_1_Int fn)
{
  guilib_iteration_start = fn;
}


DLL_API void
set_day_start (TFnVoid_1_Int fn)
{
  guilib_day_start = fn;
}


DLL_API void
set_day_complete (TFnVoid_1_Int fn)
{
  guilib_day_complete = fn;
}


DLL_API void
set_disease_end (TFnVoid_1_Int fn)
{
  guilib_disease_end = fn;
}


DLL_API void
set_outbreak_end (TFnVoid_1_Int fn)
{
  guilib_outbreak_end = fn;
}


DLL_API void
set_iteration_complete (TFnVoid_1_Int fn)
{
  guilib_iteration_complete = fn;
}


DLL_API void
set_sim_complete (TFnVoid_1_Int fn)
{
  guilib_sim_complete = fn;
}



/* Used by the GUI to access zone information during a running simulation */
DLL_API void
set_stop_on_disease_end (TFnInt_0 fn)
{
  guilib_stop_on_disease_end = fn;
}


DLL_API void
set_reset_detection_end (TFnVoid_0 fn)
{
  guilib_reset_detection_end = fn;
}


DLL_API void
set_stop_on_detection (TFnInt_0 fn)
{
  guilib_stop_on_detection = fn;
}



/* Used to update herd status and related events as an iteration runs */
DLL_API void
set_change_herd_state (TFnVoid_1_THRDUpdate fn)
{
  guilib_change_herd_state = fn;
}


DLL_API void
set_infect_herd (TFnVoid_1_THRDUpdate fn)
{
  guilib_infect_herd = fn;
}


DLL_API void
set_expose_herd (TFnVoid_1_THRDUpdate fn)
{
  guilib_expose_herd = fn;
}


DLL_API void
set_detect_herd (TFnVoid_1_THRDUpdate fn)
{
  guilib_detect_herd = fn;
}


DLL_API void
set_attempt_trace_herd (TFnVoid_1_THRDUpdate fn)
{
  guilib_attempt_trace_herd = fn;
}


/*
DLL_API void set_trace_herd( TFnVoid_1_THRDUpdate fn ) {
  guilib_trace_herd = fn;
}
*/


DLL_API void
set_destroy_herd (TFnVoid_1_THRDUpdate fn)
{
  guilib_destroy_herd = fn;
}


DLL_API void
set_vaccinate_herd (TFnVoid_1_THRDUpdate fn)
{
  guilib_vaccinate_herd = fn;
}


DLL_API void
set_record_exposure (TFnVoid_1_THRDExpose fn)
{
  guilib_record_exposure = fn;
}


DLL_API void 
set_make_zone_focus( TFnVoid_1_Int fn )
{
  guilib_make_zone_focus = fn;  
}


DLL_API void 
set_record_zone_change (TFnVoid_1_THRDZone fn )
{
  guilib_record_zone_change = fn;  
}

DLL_API void 
set_record_zone_area (TFnVoid_2_Int_Double fn)
{
  guilib_record_zone_area = fn;  
}



/* Used by the GUI to access zone perimeters during a running simulation */
DLL_API void 
set_set_zone_perimeters( TFnVoid_1_THRDPerimeterList fn)
{
   guilib_set_zone_perimeters = fn;
}                       


DLL_API unsigned int 
get_zone_list_length( ZON_zone_list_t *zones )
{
  if (zones == NULL)
    return 0;
  else
    return ZON_zone_list_length (zones);
}


DLL_API ZON_zone_t *
get_zone_from_list( ZON_zone_list_t * zones, int i)
{
  if (zones == NULL)
    return NULL;
  else
    return ZON_zone_list_get (zones, i);                   
}                    


/* Used to write daily herd state output, when desired */
DLL_API void
set_show_all_states (TFnVoid_1_CharP fn)
{
  guilib_show_all_states = fn;
}


/* Used to write daily herd prevalence output, when desired */
DLL_API void
set_show_all_prevalences (TFnVoid_1_CharP fn)
{
  guilib_show_all_prevalences = fn;
}


/* Used to write daily herd zone output, when desired */
/* This function will need to be re-implemented if it is ever needed again. */
/*
DLL_API void
set_show_all_zones (TFnVoid_1_CharP fn)
{
  guilib_show_all_zones = fn;
}
*/

/* Used to determine whether the user wants to interrupt a running simulation */
DLL_API void
set_simulation_stop (TFnInt_0 fn)
{
  guilib_simulation_stop = fn;
}


DLL_API void 
set_report_search_hits (TFnVoid_5_Int_Int_Int_Int_Int fn)
{
  guilib_report_search_hits = fn; 
}


/* Function pointer helpers */
/*--------------------------*/
void
clear_guilib_fns (void)
{
  set_printf (NULL);
  set_debug (NULL);
  
  set_sim_start (NULL);
  set_iteration_start (NULL);
  set_day_start (NULL);
  set_day_complete (NULL);
  set_iteration_complete (NULL);
  set_disease_end (NULL);
  set_outbreak_end (NULL);  
  set_sim_complete (NULL);
  
  set_stop_on_disease_end (NULL);
  set_reset_detection_end (NULL);
  set_stop_on_detection (NULL);
  
  set_change_herd_state (NULL);
  set_infect_herd (NULL);
  set_expose_herd (NULL);
  set_detect_herd (NULL);
  set_attempt_trace_herd (NULL);
  /* set_trace_herd( NULL ); */
  set_destroy_herd (NULL);
  set_vaccinate_herd (NULL);
  set_record_exposure (NULL);
  set_make_zone_focus (NULL);
  set_record_zone_change (NULL);
  set_record_zone_area (NULL);

  set_set_zone_perimeters( NULL );
  
  set_show_all_states (NULL);
  set_show_all_prevalences (NULL);
  /* set_show_all_zones (NULL); */

  set_simulation_stop (NULL); 
  
  set_report_search_hits (NULL); 
}



/* Some other potentially useful functions */
/*-----------------------------------------*/
DLL_API double
calculate_distance (double lat1, double lon1, double lat2, double lon2)
{
  return GIS_great_circle_distance (lat1, lon1, lat2, lon2);
}
