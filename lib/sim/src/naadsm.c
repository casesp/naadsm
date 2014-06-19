#ifdef CPPOUTPUT
extern "C"
{
#endif //CPPOUTPUT

/** @file naadsm.c
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
 * Copyright &copy; 2005 - 2009 Animal Population Health Institute,
 * Colorado State University
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */


#include "naadsm.h"
#include <glib.h>

#include "gis.h"
#include "rng.h"

/* Function pointers */
/*-------------------*/
/* For the display of debugging information in the GUI */
TFnVoid_1_CharP naadsm_printf = NULL;
TFnVoid_1_CharP naadsm_debug = NULL;

/* For key simulation- and iteration-level events */
TFnVoid_2_THRDListP_TZONListP naadsm_cpp_initialize = NULL;
TFnVoid_0 naadsm_cpp_finalize = NULL;
TFNVoid_1_Rng naadsm_set_rng = NULL;
TFnVoid_0 naadsm_sim_start = NULL;
TFnVoid_1_Int naadsm_iteration_start = NULL;
TFnVoid_1_Int naadsm_day_start = NULL;
TFnVoid_1_Int naadsm_day_complete = NULL;
TFnVoid_1_Int naadsm_disease_end = NULL;
TFnVoid_1_Int naadsm_outbreak_end = NULL;
TFnVoid_1_Int naadsm_iteration_complete = NULL;
TFnVoid_1_Int naadsm_sim_complete = NULL;

/* Used to determine whether the user wants to interrupt a running simulation */
TFnInt_0 naadsm_simulation_stop = NULL;

/* Used to update herd status and related events as an iteration runs */
TFnVoid_1_THRDUpdate naadsm_change_herd_state = NULL;
TFnVoid_1_THRDInfect naadsm_infect_herd = NULL;
TFnVoid_1_THRDDetect naadsm_detect_herd = NULL;
TFnVoid_1_THRDExpose naadsm_expose_herd = NULL;
TFnVoid_1_THRDTrace naadsm_trace_herd = NULL;
TFnVoid_1_THRDExam naadsm_examine_herd = NULL;
TFnVoid_1_THRDTest naadsm_test_herd = NULL;
TFnVoid_1_Int naadsm_queue_herd_for_destruction = NULL;
TFnVoid_1_THRDControl naadsm_destroy_herd = NULL;
TFnVoid_1_Int naadsm_queue_herd_for_vaccination = NULL;
TFnVoid_1_THRDControl naadsm_vaccinate_herd = NULL;
TFnVoid_1_THRDControl naadsm_cancel_herd_vaccination = NULL;
TFnVoid_1_Int naadsm_make_zone_focus = NULL;
TFnVoid_1_THRDZone naadsm_record_zone_change = NULL;
TFnVoid_2_Int_Double naadsm_record_zone_area = NULL;
TFnVoid_2_Int_Double naadsm_record_zone_perimeter = NULL;

/* Used by the GUI to access zone information during a running simulation */
TFnVoid_1_THRDPerimeterList naadsm_set_zone_perimeters = NULL;

/* Used to write daily herd state output, when desired */
TFnVoid_1_CharP naadsm_show_all_states = NULL;

/* Used to write daily herd prevalence output, when desired */
TFnVoid_1_CharP naadsm_show_all_prevalences = NULL;

/* Used to display g_warnings, etc., in the GUI */
TFnVoid_1_CharP naadsm_display_g_message = NULL;

/* Used to write daily herd zone output, when desired */
/* This function will need to be re-implemented if it is ever needed again. */
/* TFnVoid_1_CharP naadsm_show_all_zones = NULL; */

TFnVoid_5_Int_Int_Int_Int_Int naadsm_report_search_hits = NULL;


/*-----------------------------------------------------------------------------
 * Required for the Windows DLL version of the NAADSM core library
 *
 * Other implementations should ignore this block.
 *---------------------------------------------------------------------------*/
#ifdef DLL_EXPORTS
#include <windows.h>

BOOL APIENTRY
DllMain (HINSTANCE hInst /* Library instance handle. */ ,
         DWORD reason /* Reason this function is being called. */ ,
         LPVOID reserved /* Not used. */ )
{
  if( ( 0 != hInst ) || ( 0 == reason ) || ( 0 == reserved ) ) {
    /* Avoid compiler warning */
  }

  switch (reason)
    {
    case DLL_PROCESS_ATTACH:
      /* printf( "@@@ DLL LOADED @@@" ); */
      clear_naadsm_fns ();
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
/*------------------------------------------------------------------------------*/



/*-----------------------------------------------------------------------------
 * Functions for version tracking
 *---------------------------------------------------------------------------*/
/** Returns the current version of this application. */
DLL_API char *
current_version (void)
{
  int i;
  char* ret_val;

  /* DON'T FORGET: When updating the string constants below, also change
   * PACKAGE_STRING, PACKAGE_VERSION, and VERSION in config.h. 
   * Also don't forget to update the version number in config.in */ 
  i = 0;

  #ifdef CHEYENNE
    ++i;
    ret_val = "3.3.2-CHEYENNE";
    #error "Double check all new modules for 'Cheyenne' rules."
  #endif

  #ifdef LARAMIE
    ++i;
    ret_val = "3.3.2-LARAMIE";
    #error "Double check all new modules for 'Laramie' rules."
  #endif

  #ifdef RIVERTON
    ++i;
    ret_val = "3.3.2-RIVERTON";
  #endif

  if( 0 == i ) {
    ret_val = "3.3.2";
  }
  else if( 1 == i ) {
    /* All is right with the world: do nothing. */
  }
  else {
   /* Someone screwed something up. */
   g_assert( FALSE );
   ret_val = "0.0.0";
  }

  return ret_val;
}


/** Returns the version of the model specification that
    this application or DLL is intended to comply with */
DLL_API char *
specification_version (void)
{
  return "1.2.2";
}
/*---------------------------------------------------------------------------*/



/*-----------------------------------------------------------------------------
 * Functions used to set the function pointers
 *---------------------------------------------------------------------------*/
/* For the display of debugging information in the GUI */
DLL_API void
set_printf (TFnVoid_1_CharP fn)
{
  naadsm_printf = fn;
}


DLL_API void
set_debug (TFnVoid_1_CharP fn)
{
  naadsm_debug = fn;
}


/* For key simulation- and iteration-level events */
DLL_API void
set_cpp_initialize( TFnVoid_2_THRDListP_TZONListP fn )
{
 naadsm_cpp_initialize = fn;
}

DLL_API void
set_cpp_finalize( TFnVoid_0 fn )
{
  naadsm_cpp_finalize = fn;
}

DLL_API void
set_set_rng( TFNVoid_1_Rng fn)
{
  naadsm_set_rng = fn;
}


DLL_API void
set_sim_start (TFnVoid_0 fn)
{
  naadsm_sim_start = fn;
}


DLL_API void
set_iteration_start (TFnVoid_1_Int fn)
{
  naadsm_iteration_start = fn;
}


DLL_API void
set_day_start (TFnVoid_1_Int fn)
{
  naadsm_day_start = fn;
}


DLL_API void
set_day_complete (TFnVoid_1_Int fn)
{
  naadsm_day_complete = fn;
}


DLL_API void
set_disease_end (TFnVoid_1_Int fn)
{
  naadsm_disease_end = fn;
}


DLL_API void
set_outbreak_end (TFnVoid_1_Int fn)
{
  naadsm_outbreak_end = fn;
}


DLL_API void
set_iteration_complete (TFnVoid_1_Int fn)
{
  naadsm_iteration_complete = fn;
}


DLL_API void
set_sim_complete (TFnVoid_1_Int fn)
{
  naadsm_sim_complete = fn;
}


/* Used to update herd status and related events as an iteration runs */
DLL_API void
set_change_herd_state (TFnVoid_1_THRDUpdate fn)
{
  naadsm_change_herd_state = fn;
}


DLL_API void
set_infect_herd (TFnVoid_1_THRDInfect fn)
{
  naadsm_infect_herd = fn;
}


DLL_API void
set_expose_herd (TFnVoid_1_THRDExpose fn)
{
  naadsm_expose_herd = fn;
}


DLL_API void
set_detect_herd (TFnVoid_1_THRDDetect fn)
{
  naadsm_detect_herd = fn;
}


DLL_API void
set_trace_herd (TFnVoid_1_THRDTrace fn)
{
  naadsm_trace_herd = fn;
}


DLL_API void
set_examine_herd (TFnVoid_1_THRDExam fn)
{
  naadsm_examine_herd = fn; 
}


DLL_API void 
set_test_herd (TFnVoid_1_THRDTest fn)
{
  naadsm_test_herd = fn;  
}


DLL_API void 
set_queue_herd_for_destruction (TFnVoid_1_Int fn)
{
  naadsm_queue_herd_for_destruction = fn; 
}


DLL_API void
set_destroy_herd (TFnVoid_1_THRDControl fn)
{
  naadsm_destroy_herd = fn;
}


DLL_API void 
set_queue_herd_for_vaccination (TFnVoid_1_Int fn)
{
  naadsm_queue_herd_for_vaccination = fn; 
}


DLL_API void
set_vaccinate_herd (TFnVoid_1_THRDControl fn)
{
  naadsm_vaccinate_herd = fn;
}


DLL_API void
set_cancel_herd_vaccination (TFnVoid_1_THRDControl fn)
{
  naadsm_cancel_herd_vaccination = fn;
}


DLL_API void
set_make_zone_focus( TFnVoid_1_Int fn )
{
  naadsm_make_zone_focus = fn;
}


DLL_API void
set_record_zone_change (TFnVoid_1_THRDZone fn )
{
  naadsm_record_zone_change = fn;
}


DLL_API void
set_record_zone_area (TFnVoid_2_Int_Double fn)
{
  naadsm_record_zone_area = fn;
}


DLL_API void
set_record_zone_perimeter (TFnVoid_2_Int_Double fn)
{
  naadsm_record_zone_perimeter = fn;
}


/* Used by the GUI to access zone perimeters during a running simulation */
DLL_API void
set_set_zone_perimeters( TFnVoid_1_THRDPerimeterList fn)
{
   naadsm_set_zone_perimeters = fn;
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
  naadsm_show_all_states = fn;
}


/* Used to write daily herd prevalence output, when desired */
DLL_API void
set_show_all_prevalences (TFnVoid_1_CharP fn)
{
  naadsm_show_all_prevalences = fn;
}


/* Used to write daily herd zone output, when desired */
/* This function will need to be re-implemented if it is ever needed again. */
/*
DLL_API void
set_show_all_zones (TFnVoid_1_CharP fn)
{
  naadsm_show_all_zones = fn;
}
*/

/* Used to determine whether the user wants to interrupt a running simulation */
DLL_API void
set_simulation_stop (TFnInt_0 fn)
{
  naadsm_simulation_stop = fn;
}


DLL_API void
set_display_g_message (TFnVoid_1_CharP fn)
{
  naadsm_display_g_message = fn;  
}


DLL_API void
set_report_search_hits (TFnVoid_5_Int_Int_Int_Int_Int fn)
{
  naadsm_report_search_hits = fn;
}
/*---------------------------------------------------------------------------*/



/*-----------------------------------------------------------------------------
 * Function pointer helpers
 *---------------------------------------------------------------------------*/
void
clear_naadsm_fns (void)
{
  set_printf (NULL);
  set_debug (NULL);

  set_cpp_initialize (NULL);
  set_cpp_finalize (NULL);

  set_set_rng (NULL);
  set_sim_start (NULL);
  set_iteration_start (NULL);
  set_day_start (NULL);
  set_day_complete (NULL);
  set_iteration_complete (NULL);
  set_disease_end (NULL);
  set_outbreak_end (NULL);
  set_sim_complete (NULL);

  set_change_herd_state (NULL);
  set_infect_herd (NULL);
  set_expose_herd (NULL);
  set_detect_herd (NULL);
  set_trace_herd (NULL);
  set_examine_herd (NULL);
  set_test_herd (NULL);
  set_queue_herd_for_destruction (NULL);
  set_destroy_herd (NULL);
  set_queue_herd_for_vaccination (NULL);
  set_vaccinate_herd (NULL);
  set_cancel_herd_vaccination (NULL);
  set_make_zone_focus (NULL);
  set_record_zone_change (NULL);
  set_record_zone_area (NULL);
  set_record_zone_perimeter (NULL);

  set_set_zone_perimeters( NULL );

  set_show_all_states (NULL);
  set_show_all_prevalences (NULL);
  /* set_show_all_zones (NULL); */

  set_simulation_stop (NULL);
  set_display_g_message (NULL);

  set_report_search_hits (NULL);
}

void
naadsm_log_handler(const gchar *log_domain, GLogLevelFlags log_level, const gchar *message, gpointer user_data)
{
  if( ( NULL != log_domain ) || ( 0 == log_level ) || ( NULL == user_data ) ) {
    /* Avoid compiler warning */
  }

  if( NULL != naadsm_display_g_message )
    naadsm_display_g_message( (gchar*) message ); 
}
/*---------------------------------------------------------------------------*/



/*-----------------------------------------------------------------------------
 * Fully spelled out and abbreviated names for enums.
 *---------------------------------------------------------------------------*/
const char *NAADSM_trace_direction_name[] = {
  "Trace Neither", "Trace Forward or Out", "Trace Back or In", NULL
};

const char *NAADSM_trace_direction_abbrev[] = {
  "Neither", "Fwd", "Back", NULL
};

const char *NAADSM_contact_type_name[] = {
  "Unknown", "Direct Contact", "Indirect Contact", "Airborne Spread",
  "Initially Infected", NULL
};

const char *NAADSM_contact_type_abbrev[] = {
  "Unkn", "Dir", "Ind", "Air", "Ini", NULL
};

const char *NAADSM_detection_reason_abbrev[] = {
  "Unkn", "Clin", "Test", NULL
};

const char *NAADSM_control_reason_name[] = {
  "Unspecified", "Ring", "Trace Forward Direct", "Trace Forward Indirect",
  "Trace Back Direct", "Trace Back Indirect", "Detection", "Initial State",
  NULL
};

const char *NAADSM_control_reason_abbrev[] = {
  "Unsp", "Ring", "DirFwd", "IndFwd", "DirBack", "IndBack", "Det", "Ini", NULL
};

/*---------------------------------------------------------------------------*/


#ifdef CPPOUTPUT
}
#endif //CPPOUTPUT

