unit NAADSMLibrary;

(*
NAADSMLibrary.pas
-----------------
Begin: 2004/08/20
Last revision: $Date: 2013-06-27 19:11:18 $ $Author: areeves $
Version: $Revision: 1.21.4.17 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2004 - 2011 Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)


{*
 Items in this unit are global, rather than encapsulated in a class,
 because I'm not sure that Delphi function pointers "of object" can be
 passed to a C DLL.  This warrants further investigation at some point,
 however:  I'd feel better without all of these very important functions,
 variables, and references sticking out where just anyone can get to them.
}


{$INCLUDE Defs.inc}

interface

  uses
    Windows, // Defines THandle

    I88n,
    FunctionPointers,
    ZipFunctions,

    {$IFNDEF CONSOLEAPP}
    DialogLongMessage,
    {$ENDIF}

    NAADSMLibraryTypes,
    SMDatabase,
    SMSimulationInput,
    EventsAndExposures,
    ZonePerimeter,
    Herd
  ;


  // Function pointer types
  //-----------------------
  // (Other function pointers used in this application are defined
  // for more general use in unit FunctionPointers.pas)
  type TCFnVoid_1_THRDUpdate = procedure( r: THRDUpdate ); cdecl;
  type TCFnVoid_1_THRDInfect = procedure( r: THRDInfect ); cdecl;
  type TCFnVoid_1_THRDDetect = procedure( d: THRDDetect ); cdecl;
  type TCFnVoid_1_THRDControl = procedure( c: THRDControl ); cdecl;
  type TCFnVoid_1_THRDTrace = procedure( t: THRDTrace ); cdecl;
  type TCFnVoid_1_THRDExam = procedure( e: THRDExam ); cdecl;
  type TCFnVoid_1_THRDTest = procedure( t: THRDTest ); cdecl;
  type TCFnVoid_1_THRDExpose = procedure( e: THRDExpose ); cdecl;
  type TCFnVoid_1_THRDZone = procedure( z: THRDZone ); cdecl;

  //Function to set the Perimeter lists.
  type TCFnVoid_1_THRDPerimeterList = procedure( p: THRD_PerimeterList ); cdecl;


  // Functions for launching the simulation from the DLL
  //-----------------------------------------------------
  // Validates objects passed as parameters, and launches DLL if things are OK
  function startSim(
    smSim: TSMSimulationInput;
    herds: THerdList;
    db: TSMDatabase;
    var errMsg: string
  ): integer;

  // Actually launches the DLL.  If an exception occurs, creates the error *.zip file.
  function launchSim( var errMsg: string ): integer;

  function writeLogFile( msg: string ): string;


  // Function declarations/pointers
  //-------------------------------
  procedure rng_read_seed( val: integer ); cdecl; // type TCFnVoid_1_Int

  // For the display of debugging information in the GUI
  procedure naadsm_printf( msg: pchar ); cdecl; // type TCFnVoid_1_CharP
  procedure naadsm_debug( msg: pchar ); cdecl;

  // For key simulation- and iteration-level events
  procedure naadsm_sim_start(); cdecl; // type TCFnVoid_0
  procedure naadsm_iteration_start( it: integer ); cdecl; // type TCFnVoid_1_Int
  procedure naadsm_day_start( day: integer ); cdecl; // type TCFnVoid_1_Int
  procedure naadsm_day_complete( day: integer ); cdecl; // type TCFnVoid_1_Int
  procedure naadsm_disease_end( val: integer ); cdecl; // type TCFnVoid_1_Int
  procedure naadsm_outbreak_end( val: integer ); cdecl; // type TCFnVoid_1_Int
  procedure naadsm_iteration_complete( it: integer ); cdecl; // type TCFnVoid_1_Int
  procedure naadsm_sim_complete( val: integer ); cdecl; // type TCFnVoid_1_Int

  // Used to update herd status and related events as an  iteration runs
  procedure naadsm_change_herd_state( r: THRDUpdate );cdecl; // type TCFnVoid_1_THRDUpdate
  procedure naadsm_infect_herd( r: THRDInfect ); cdecl; // type TCFnVoid_1_THRDInfect
  procedure naadsm_expose_herd( e: THRDExpose ); cdecl; // type TCFnVoid_1_THRDExpose
  procedure naadsm_detect_herd( d: THRDDetect ); cdecl; // type TCFnVoid_1_THRDDetect
  procedure naadsm_trace_herd( t: THRDTrace ); cdecl; // type TCFnVoid_1_THRDTrace
  procedure naadsm_examine_herd( e: THRDExam ); cdecl; // type TCFnVoid_1_THRDExam
  procedure naadsm_test_herd( t: THRDTest ); cdecl; // type TCFnVoid_1_THRDTest
  procedure naadsm_queue_herd_for_destruction( herdIndex: integer ); cdecl; // type TCFnVoid_1_Int
  procedure naadsm_destroy_herd( c: THRDControl );  cdecl; // type TCFnVoid_1_THRDControl
  procedure naadsm_queue_herd_for_vaccination( herdIndex: integer ); cdecl; // type TCFnVoid_1_Int
  procedure naadsm_vaccinate_herd( c: THRDControl );  cdecl; // type TCFnVoid_1_THRDControl
  procedure naadsm_cancel_herd_vaccination( c: THRDControl ); cdecl; // type TCFnVoid_1_THRDControl
  procedure naadsm_make_zone_focus( herdIndex: integer ); cdecl; // type TCFnVoid_1_Int
  procedure naadsm_record_zone_change( r: THRDZone ); cdecl; // type TCFnVoid_1_THRDZone
  procedure naadsm_record_zone_area( zoneLevel: integer; area: double ); cdecl; // type TCFnVoid_2_Int_Double
  procedure naadsm_record_zone_perimeter( zoneLevel: integer; perim: double ); cdecl; // type TCFnVoid_2_Int_Double

  // Used by the GUI to access zone information during a running simulation
  // This procedure is called by the dll to set zone perimeters each day of the sim.
  // This is actually a pointer into a complex C structure.  Use functions in ZonePerimeter.pas
  // to get more details about the data pointed to by this PerimeterList....
  procedure naadsm_set_zone_perimeters( p: THRD_PerimeterList ); cdecl;  // type TCFnVoid_1_THRDPerimeterList

  // Used to write daily herd state output, when desired
  procedure naadsm_show_all_states( statesMsg: pchar ); cdecl; // type TCFnVoid_1_CharP

  // Used to write daily herd prevalence output, when desired
  procedure naadsm_show_all_prevalences(  prevMsg: pchar ); cdecl; // type TCFnVoid_1_CharP

  // Used to determine whether the user wants to interrupt a running simulation
  function naadsm_simulation_stop(): integer; cdecl; // type TCFnInt_0

  // Alerts the user in case the DLL generates any warnings or errors
  procedure naadsm_display_g_message( msg: pchar ); cdecl; // type TCFnVoid_1_CharP

  procedure naadsm_report_search_hits( val1, val2, val3, val4, val5: integer ); cdecl; // type TCFnVoid_5_Int_Int_Int_Int_Int

  // Loading functions from the DLL
  //-------------------------------
  function naadsmLibLoadErrors(): string;

  // Hack needed to handle multiple detections in NAADSM 3.
  // This function will not exist in NAADSM 4!
  //--------------------------------------------------------
  procedure naadsm3ProcessDetections();

  var
    simFileName, herdFileName, logFileName: string;

    {$IFNDEF CONSOLEAPP}
      frmDllWarnings: TDialogLongMessage;
    {$ENDIF}

    // FIX ME: Move these to ZonePerimeter.
    get_zone_list_length: function( zones: Pointer ): integer; cdecl;
    get_zone_from_list: function( zones: Pointer; i: integer): ZON_zone_t_ptr; cdecl;

    // Variables used for the running simulation
    // (Declared in the implementation section, but
    // listed here for easy reference.)
    //---------------------------------------------
    (*
    _diseaseEndDay: integer;
    _outbreakEnd: integer;
    _simDay: integer;
    _simIteration: integer;
    _smSim: TSMSimulationInput;
    _herds: THerdList;
    _smdb: TSMDatabase;

    _rngSeed: integer;

    _eventCounter: integer;
    _eventList: TSMEventList;

    _exposureCounter: integer;
    _exposureList: TSMExposureOrTraceList;

    _badThingsList: TQStringList;

    _naadsMap: TNAADSMap;

    _herdsInZones: TQIntegerObjectMap;

    // See comment for naadsm_detect_herd.  This will not be used in NAADSM 4!
    _naadsm3Detections: TQIntegerObjectMap;
   *)

    // Variables used for DLL management
    //-----------------------------------
    naadsmLibLoaded: boolean;

  const
    // Constants used while running the simulation
    //---------------------------------------------
    ERRRUNSIMEXCEPTION = -3;
    ERRINVALIDSCENARIO = -2;
    ERRCANNOTWRITEFILES = -1;
    ERRNONE = 0;
    ERRNOTSET = 17;

    // Constants used to indicate the reason for stopping an iteration
    //----------------------------------------------------------------
    NO_STOP = 0;
    USER_STOP = -1;
    DETECTION_STOP = 1;

implementation

  uses
    Forms, // Defines the Application object
    SysUtils,
    StrUtils,

    MyStrUtils,
    DebugWindow,
    SqlClasses,
    WindowsUtils,
    MyDialogs,

    QIntegerMaps,
    QLists,

    AphiRng,

    StringConsts,
    StatusEnums,

    Zone,
    CustomOutputDefinitions,
    SMExceptionHandler,

    NAADSMap

    {$IFNDEF CONSOLEAPP}
    ,
    FormMain,
    FormMap
    {$ENDIF}
  ;

  var
    dllLoadErrors: string;

    //Function pointers, to be loaded from the DLL.
    //----------------------------------------------
    current_dll_version: function(): pchar; cdecl;

    run_sim_main: procedure(
      herdFile: pchar;
      paramFile: pchar;
      outputFile: pchar;
      fixedRngValue: double; // use -1 if values should be generated randomly
      verbosity: integer;
      seed: integer
    ); cdecl;

    set_rng_read_seed: procedure( fn: TCFnVoid_1_Int ); cdecl;

    set_printf: procedure( fn: TCFnVoid_1_CharP ); cdecl;
    set_debug: procedure( fn: TCFnVoid_1_CharP ); cdecl;

    set_sim_start: procedure( fn: TCFnVoid_0 ); cdecl;
    set_iteration_start: procedure( fn: TCFnVoid_1_Int ); cdecl;
    set_day_start: procedure( fn: TCFnVoid_1_Int ); cdecl;
    set_disease_end: procedure( fn: TCFnVoid_1_Int ); cdecl;
    set_outbreak_end: procedure( fn: TCFnVoid_1_Int ); cdecl;
    set_day_complete: procedure( fn: TCFnVoid_1_Int ); cdecl;
    set_iteration_complete: procedure( fn: TCFnVoid_1_Int ); cdecl;
    set_sim_complete: procedure( fn: TCFnVoid_1_Int ); cdecl;

    set_change_herd_state: procedure( fn: TCFnVoid_1_THRDUpdate ); cdecl;
    set_infect_herd: procedure( fn: TCFnVoid_1_THRDInfect ); cdecl;
    set_expose_herd: procedure( fn: TCFnVoid_1_THRDExpose ); cdecl;
    set_detect_herd: procedure( fn: TCFnVoid_1_THRDDetect ); cdecl;
    set_trace_herd: procedure( fn: TCFnVoid_1_THRDTrace ); cdecl;
    set_examine_herd: procedure( fn: TCFnVoid_1_THRDExam ); cdecl;
    set_test_herd: procedure( fn: TCFnVoid_1_THRDTest ); cdecl;
    set_queue_herd_for_destruction: procedure( fn: TCFnVoid_1_Int ); cdecl;
    set_queue_herd_for_vaccination: procedure( fn: TCFnVoid_1_Int ); cdecl;
    set_destroy_herd: procedure( fn: TCFnVoid_1_THRDControl ); cdecl;
    set_vaccinate_herd: procedure( fn: TCFnVoid_1_THRDControl ); cdecl;
    set_cancel_herd_vaccination: procedure( fn: TCFnVoid_1_THRDControl ); cdecl;
    set_make_zone_focus: procedure( fn: TCFnVoid_1_Int ); cdecl;
    set_record_zone_change: procedure( fn: TCFnVoid_1_THRDZone ); cdecl;
    set_record_zone_area: procedure( fn: TCFnVoid_2_Int_Double ); cdecl;
    set_record_zone_perimeter: procedure( fn: TCFnVoid_2_Int_Double ); cdecl;

    set_set_zone_perimeters: procedure( fn: TCFnVoid_1_THRDPerimeterList ); cdecl;

    set_show_all_states: procedure( fn: TCFnVoid_1_CharP ); cdecl;
    set_show_all_prevalences: procedure( fn: TCFnVoid_1_CharP ); cdecl;

    set_simulation_stop: procedure( fn: TCFnInt_0 ); cdecl;

    set_display_g_message: procedure(fn: TCFnVoid_1_CharP ); cdecl;

    set_report_search_hits: procedure( fn: TCFnVoid_5_Int_Int_Int_Int_Int ); cdecl;

    // Variables used for the running simulation
    //-------------------------------------------
    _diseaseEndDay: integer;
    _outbreakEnd: integer;
    _simDay: integer;
    _simIteration: integer;
    _smSim: TSMSimulationInput;
    _herds: THerdList;
    _smdb: TSMDatabase;

    _rngSeed: integer;

    _eventCounter: integer;
    _eventList: TSMEventList;

    _exposureCounter: integer;
    _exposureList: TSMExposureOrTraceList;

    _badThingsList: TQStringList;

    _dailyStatesFile: textfile;
    _dailyPrevalenceFile: textfile;
    _naadsMap: TNAADSMap;

    _herdsInZones: TQIntegerObjectMap;

    // See comment for naadsm_detect_herd.  This will not be used in NAADSM 4!
    _naadsm3Detections: TQIntegerObjectMap;

  const
    DBSHOWMSG: boolean = false; // set to true to enable debugging messages for this unit


  function naadsmLibLoadErrors(): string;
    begin
      result := dllLoadErrors;
    end
  ;


  function loadDynamicDll(): boolean;
    var
      minVersion: string;
      dllHandle: THandle; //Handle used to open the DLL.  Defined in unit Windows.
    begin
      dllLoadErrors := '';

      try
        dllHandle := loadLibrary( SIM_DLL_NAME );
        dbcout( 'NAADSM: loadLibrary successful', DBSHOWMSG );
      except
        dbcout( 'NAADSM: loadLibrary failed', DBSHOWMSG );
        result := false;
        exit;
      end;

      result := true;

      dllLoadErrors := dllLoadErrors + intToStr( dllHandle );

      if( dllHandle >= 32 ) then // library was successfully loaded.  Assign function pointers now.
        begin
          dbcout( 'NAADSM: Library was successfully loaded', DBSHOWMSG );

          dbcout( 'NAADSM: Attempting to set function pointers', DBSHOWMSG );

          // Check the version of the DLL
          //-----------------------------
          current_dll_version := GetProcAddress( dllHandle, 'current_version' ); // Yes, the function names don't match here.  That is intentional.
          if( nil = @current_dll_version ) then
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION current_version';
              result := false;
            end
          else
            begin
              minVersion := current_dll_version();
              if( MIN_COMPATIBLE_DLL_VERSION  <> minVersion ) then
                begin
                  dllLoadErrors := dllLoadErrors + endl + 'INCOMPATIBLE DLL VERSION: ' + minVersion;
                  result := false;
                end
              ;
            end
          ;


          // Load functions
          //---------------
          run_sim_main := GetProcAddress( dllHandle, 'run_sim_main' );
          if( nil = @run_sim_main ) then
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION run_sim_main';
              result := false;
            end
          ;

          // For retrieiving the RNG seed
          //-----------------------------
          set_rng_read_seed := GetProcAddress( dllHandle, 'set_rng_read_seed' );
          if( nil <> @set_rng_read_seed ) then
            set_rng_read_seed( @rng_read_seed )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_rng_read_seed';
              result := false;
            end
          ;

          // For the display of debugging information in the GUI
          //----------------------------------------------------
          set_printf := GetProcAddress( dllHandle, 'set_printf' );
          if( nil <> @set_printf ) then
            begin
              if( true (* DBSHOWMSG *) ) then
                set_printf( @naadsm_printf )
              else
                set_printf( nil )
              ;
            end
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_printf';
              result := false;
            end
          ;

          set_debug := GetProcAddress( dllHandle, 'set_debug' );
          if( nil <> @set_debug ) then
            begin
              if( true (* DBSHOWMSG *) ) then
                set_debug( @naadsm_debug )
              else
                set_debug( nil )
              ;
            end
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_debug';
              result := false;
            end
          ;
          
          
          // For key simulation- and iteration-level events
          //-----------------------------------------------
          set_sim_start := GetProcAddress( dllHandle, 'set_sim_start' );
          if( nil <> @set_sim_start ) then
            set_sim_start( @naadsm_sim_start )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_sim_start';
              result := false;
            end
          ;
          
          set_iteration_start := GetProcAddress( dllHandle, 'set_iteration_start' );
          if( nil <> @set_iteration_start ) then
            set_iteration_start( @naadsm_iteration_start )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_iteration_start';
              result := false;
            end
          ;
          
          set_day_start := GetProcAddress( dllHandle, 'set_day_start' );
          if( nil <> @set_day_start ) then
            set_day_start( @naadsm_day_start )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_day_start';
              result := false;
            end
          ; 
          
          set_day_complete := GetProcAddress( dllHandle, 'set_day_complete' );
          if( nil <> @set_day_complete ) then
            set_day_complete( @naadsm_day_complete )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_day_complete';
              result := false;
            end
          ;


          set_disease_end := GetProcAddress( dllHandle, 'set_disease_end' );
          if( nil <> @set_disease_end ) then
            set_disease_end( @naadsm_disease_end )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_disease_end';
              result := false;
            end
          ;

          set_outbreak_end := GetProcAddress( dllHandle, 'set_outbreak_end' );
          if( nil <> @set_outbreak_end ) then
            set_outbreak_end( @naadsm_outbreak_end )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_outbreak_end';
              result := false;
            end
          ;

          set_iteration_complete := getProcAddress( dllHandle, 'set_iteration_complete' );
          if( nil <> @set_iteration_complete ) then
            set_iteration_complete( @naadsm_iteration_complete )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_iteration_complete';
              result := false;
            end
          ;
          
          set_sim_complete := GetProcAddress( dllHandle, 'set_sim_complete' );
          if( nil <> @set_sim_complete ) then
            set_sim_complete( @naadsm_sim_complete )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_sim_complete';
              result := false;
            end
          ;
          

          // Used to update herd status and related events as an iteration runs
          //--------------------------------------------------------------------
          set_change_herd_state := GetProcAddress( dllHandle, 'set_change_herd_state' );
          if( nil <> @set_change_herd_state ) then
            set_change_herd_state( @naadsm_change_herd_state )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_change_herd_state';
              result := false;
            end
          ;

          set_infect_herd := GetProcAddress( dllHandle, 'set_infect_herd' );
          if( nil <> @set_infect_herd ) then
            set_infect_herd( @naadsm_infect_herd )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_infect_herd';
              result := false;
            end
          ;

          set_expose_herd := GetProcAddress( dllHandle, 'set_expose_herd' );
          if( nil <> @set_expose_herd ) then
            set_expose_herd( @naadsm_expose_herd )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_expose_herd';
              result := false;
            end
          ;

          set_detect_herd := GetProcAddress( dllHandle, 'set_detect_herd' );
          if( nil <> @set_detect_herd ) then
            set_detect_herd( @naadsm_detect_herd )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_detect_herd';
              result := false;
            end
          ;

          set_trace_herd := GetProcAddress( dllHandle, 'set_trace_herd' );
          if( nil <> @set_trace_herd ) then
            set_trace_herd( @naadsm_trace_herd )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_trace_herd';
              result := false;
            end
          ;

          set_examine_herd := GetProcAddress( dllHandle, 'set_examine_herd' );
          if( nil <> @set_examine_herd ) then
            set_examine_herd( @naadsm_examine_herd )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_examine_herd';
              result := false;
            end
          ;

          set_test_herd := GetProcAddress( dllHandle, 'set_test_herd' );
          if( nil <> @set_test_herd ) then
            set_test_herd( @naadsm_test_herd )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_test_herd';
              result := false;
            end
          ;

          set_queue_herd_for_destruction := GetProcAddress( dllHandle, 'set_queue_herd_for_destruction' );
          if( nil <> @set_queue_herd_for_destruction ) then
            set_queue_herd_for_destruction( @naadsm_queue_herd_for_destruction )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_queue_herd_for_destruction';
              result := false;
            end
          ;

          set_destroy_herd := GetProcAddress( dllHandle, 'set_destroy_herd' );
          if( nil <> @set_destroy_herd ) then
            set_destroy_herd( @naadsm_destroy_herd )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_destroy_herd';
              result := false;
            end
          ;

          set_queue_herd_for_vaccination := GetProcAddress( dllHandle, 'set_queue_herd_for_vaccination' );
          if( nil <> @set_queue_herd_for_vaccination ) then
            set_queue_herd_for_vaccination( @naadsm_queue_herd_for_vaccination )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_queue_herd_for_vaccination';
              result := false;
            end
          ;

          set_vaccinate_herd := GetProcAddress( dllHandle, 'set_vaccinate_herd' );
          if( nil <> @set_vaccinate_herd ) then
            set_vaccinate_herd( @naadsm_vaccinate_herd )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_vaccinate_herd';
              result := false;
            end
          ; 

          set_cancel_herd_vaccination := GetProcAddress( dllHandle, 'set_cancel_herd_vaccination' );
          if( nil <> @set_cancel_herd_vaccination ) then
            set_cancel_herd_vaccination( @naadsm_cancel_herd_vaccination )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_cancel_herd_vaccination';
              result := false;
            end
          ;

          set_make_zone_focus := GetProcAddress( dllHandle, 'set_make_zone_focus' );
          if( nil <> @set_make_zone_focus ) then
            set_make_zone_focus( @naadsm_make_zone_focus )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_make_zone_focus';
              result := false;
            end
          ;
          
          set_record_zone_change := GetProcAddress( dllHandle, 'set_record_zone_change' );
          if( nil <> @set_record_zone_change ) then
            set_record_zone_change( @naadsm_record_zone_change )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_record_zone_change';
              result := false;
            end
          ;

          set_record_zone_area := GetProcAddress( dllHandle, 'set_record_zone_area' );
          if( nil <> @set_record_zone_area ) then
            set_record_zone_area( @naadsm_record_zone_area )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_record_zone_area';
              result := false;
            end
          ;

          set_record_zone_perimeter := GetProcAddress( dllHandle, 'set_record_zone_perimeter' );
          if( nil <> @set_record_zone_perimeter ) then
            set_record_zone_perimeter( @naadsm_record_zone_perimeter )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_record_zone_perimeter';
              result := false;
            end
          ;

         
          // Used by the GUI to access zone information during a running simulation
          //-----------------------------------------------------------------------
          set_set_zone_perimeters := GetProcAddress( dllHandle, 'set_set_zone_perimeters' );
          if( nil = @set_set_zone_perimeters ) then
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_set_zone_perimeters';
              result := false;
            end
          else
            begin
              set_set_zone_perimeters( @naadsm_set_zone_perimeters );
            end;
          ;

          get_zone_list_length := GetProcAddress( dllHandle, 'get_zone_list_length' );
          if( nil = @get_zone_list_length ) then
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION get_zone_list_length';
              result := false;
            end
          ;

          get_zone_from_list := GetProcAddress( dllHandle, 'get_zone_from_list' );
          if( nil = @get_zone_from_list ) then
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION get_zone_from_list';
              result := false;
            end
          ;

          // Used to determine write daily herd state output, when desired
          //---------------------------------------------------------------
          set_show_all_states := GetProcAddress( dllHandle, 'set_show_all_states' );
          if( nil <> @set_show_all_states ) then
            set_show_all_states( @naadsm_show_all_states )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_show_all_states';
              result := false;
            end
          ;

          // Used to write daily herd prevalence output, when desired
          //---------------------------------------------------------
          set_show_all_prevalences := GetProcAddress( dllHandle, 'set_show_all_prevalences' );
          if( nil <> @set_show_all_prevalences ) then
            set_show_all_prevalences( @naadsm_show_all_prevalences )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_show_all_prevalences';
              result := false;
            end
          ;
          
          // Used to determine whether the user wants to interrupt a running simulation
          //---------------------------------------------------------------------------
          set_simulation_stop := GetProcAddress( dllHandle, 'set_simulation_stop' );
          if( nil <> @set_simulation_stop ) then
            set_simulation_stop( @naadsm_simulation_stop )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_simulation_stop';
              result := false;
            end
          ;

          // Alerts the user in case the DLL generates any warnings or errors
          //-----------------------------------------------------------------
          set_display_g_message := GetProcAddress( dllHandle, 'set_display_g_message' );
          if( nil <> @set_display_g_message ) then
            set_display_g_message( @naadsm_display_g_message )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_display_g_message';
              result := false;
            end
          ;


          set_report_search_hits := GetProcAddress( dllHandle, 'set_report_search_hits' );
          if( nil <> @set_report_search_hits ) then
            set_report_search_hits( @naadsm_report_search_hits )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_report_search_hits';
              result := false;
            end
          ;
        end
      else
        begin
          dllLoadErrors := dllLoadErrors + endl + 'The required dll is missing.';
          result := false;
        end
      ;

    end
  ;


  function launchSim( var errMsg: string ): integer;
    var
      logFileName: string;
      seed: integer;
    begin
      dbcout( 'NAADSM: ++ Start launchSim', DBSHOWMSG );

      if( _smSim.useFixedRandomSeed ) then
        seed := _smSim.randomSeed
      else
        seed := -1
      ;

      _badThingsList := TQStringList.create();

      // Launch the DLL with appropriate parameters
      //-------------------------------------------
      try
        // If DEBUG, exceptions will show up immediately.
        // Otherwise, an error log file will be created.
          try
            _eventList := TSMEventList.create();
            _exposureList := TSMExposureOrTraceList.create();

            {$IFNDEF CONSOLEAPP}
            frmDllWarnings.clear();
            //frmDllWarnings.disableFormClose(); // People didn't like that they couldn't close this window when a simulation was in progress.  If they screw up, it's now their fault!
            {$ENDIF}

            run_sim_main(
              pchar( herdFileName ),
              pchar( simFileName ),
              nil, // Output file name (typically nil for Windows)
              -1, // fixed_rng_value (if -1, values will be generated randomly)
              0,   // Verbosity (basically ignored)
              seed   // Specified seed for RNG (if -1, seed will be automatically generated)
            );
            result := ERRNONE;
          except
            on e: exception do
              begin
                {$IFDEF DEBUG}
                  dbcout( endl + endl + '!! 100 most recent messages from core library: ', true );
                  _badThingsList.debug();
                  dbcout( '!! Done.', true );
                  result := ERRRUNSIMEXCEPTION;
                  //raise e;
                {$ELSE}
                  dbcout( 'NAADSM: EXCEPTION: ' + e.message, true );
                  logFileName := writeLogFile( e.Message );
                  result := ERRRUNSIMEXCEPTION;
                  {$IFNDEF CONSOLE}
                    {$IFDEF EUREKALOG}
                      raise;
                    {$ELSE}
                      errMsg := buildErrorZip( simFileName, herdFileName, logFileName );
                    {$ENDIF}
                  {$ENDIF}
                {$ENDIF}
              end
            ;
          end;
      finally
        // clean up
        //---------
        deleteFile( simFileName );
        deleteFile( herdFileName );
        simFileName := '';
        herdFileName := '';
        
        if( 0 < length( logFileName ) ) then
          begin
            deleteFile( logFileName );
            logFileName := '';
          end
        ;

        _eventList.free();
        _exposureList.free();

        _badThingsList.free();
      end;

      dbcout( 'NAADSM: -- End launchSim', DBSHOWMSG );
    end
  ;


  function startSim(
        smSim: TSMSimulationInput;
        herds: THerdList;
        db: TSMDatabase;
        var errMsg: string
      ): integer;
    var
      sfn, hfn: string;
    begin
      dbcout( 'NAADSM: ++ Start startSim', DBSHOWMSG );

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        _smSim := smSim;
        _herds := herds;
        _smdb := db;

        if( not ( _smSim.isValid( false, @errMsg ) and _herds.isValid( @errMsg ) ) ) then
          begin
            result := ERRINVALIDSCENARIO;
            exit;
          end
        ;

        // Get temporary file names
        sfn := tempFileName( currentDir() );
        hfn := tempFileName( currentDir() );

        if( not (
          ( _smSim.writeXMLFile( sfn, false, ssStopReasonGuiDefined, -1, @errMsg ) )
        and
          ( _herds.writeXMLFile( hfn, true, @errMsg ) ) ) )
        then
          begin
            dbcout( 'NAADSM: ' + errMsg, DBSHOWMSG );
            deleteFile( sfn );
            deleteFile( hfn );
            result := ERRCANNOTWRITEFILES;
            exit;
          end
        ;

        dbcout( 'NAADSM: Launching simulation', DBSHOWMSG );
        simFileName := sfn;
        herdFileName := hfn;
        result := launchSim( errMsg );
      {$IFDEF DEBUG}
      except
        dbcout( 'NAADSM: Exception caught in startSim', true );
        result := ERRRUNSIMEXCEPTION;
      end;
      {$ENDIF}

      dbcout( 'NAADSM: -- End startSim', DBSHOWMSG );
    end
  ;



  {*
    Return name of the newly created log file
  }
  function writeLogFile( msg: string ): string;
    var
      logFile: textfile;
      fileName: string;
    begin
      dbcout( 'NAADSM: ++ Start writeLogFile', DBSHOWMSG );

      try
        fileName := tempFileName( currentDir() );
        assignFile( logFile, fileName );
        rewrite( logFile );
        writeln( logFile, APPNAME + ' VERSION ' + VERSIONNUMBER + ' ' + BRANCHNAME );
        writeln( logFile, 'RNG SEED: ' + intToStr( _rngSeed ) );
        writeln( logFile, 'ITERATION: ' + intToStr( _simIteration ) );
        writeln( logFile, 'DAY: ' + intToStr( _simDay ) );
        writeln( logFile, ' MESSAGE:' );
        writeln( logFile, msg );
        closeFile( logFile );
      except
        // Fail mostly silently
        fileName := '';
      end;

      result := fileName;

      dbcout( 'NAADSM: -- End writeLogFile', DBSHOWMSG );
    end
  ;




  procedure naadsm_printf( msg: pchar );
    begin
      dbcout( 'NAADSM DLL: ' + msg, true );
    end
  ;


  procedure naadsm_debug( msg: pchar );
    begin
      dbcout( 'NAADSM DLL: ' + msg, true (*DBSHOWMSG*) );
    end
  ;

//-----------------------------------------------------------------------------
// Obtaining zone information
//-----------------------------------------------------------------------------
  procedure naadsm_set_zone_perimeters( p: THRD_PerimeterList ); cdecl;
    begin
      // Unlike other zone functions, this one is called even if there are no zones.
      // It's harmless, though.
      
      {$IFDEF DEBUG}
        zonePerimeterDebug( p, DBSHOWMSG );
      {$ENDIF}

      {$IFNDEF CONSOLEAPP}
        if( assigned( frmMap ) ) then
          frmMap.drawZones( p )
        ;
      {$ENDIF}
    end
  ;
//-----------------------------------------------------------------------------

  procedure rng_read_seed( val: integer );
    begin
      dbcout( 'NAADSM: RNG SEED SET TO ' + intToStr( val ), DBSHOWMSG );
      _rngSeed := val;
      _smdb.setRngSeed( _rngSeed );
    end
  ;


  procedure naadsm_display_g_message( msg: pchar );
    {$IFDEF DEBUG}
      var
        s: string;
    {$ENDIF}
    begin
      {$IFDEF DEBUG}
        s := 'DLL g_message: ' + msg;
        dbcout( s, true );
        _badThingsList.append( s );
        if( 100 < _badThingsList.count ) then
          _badThingsList.removeAt( 0 )
        ;
        Application.ProcessMessages();
      {$ELSE}
        {$IFDEF CONSOLEAPP}
          cout( 'An error occurred while running this simulation: ' + msg );
        {$ELSE}
          if( nil = frmDllWarnings ) then
            dbcout( '*** frmDllWarnings is nil in naadsm_display_g_message()', true )
          else
            begin
              if( not( frmDllWarnings.Showing ) ) then
                frmDllWarnings.show( true )
              ;
              frmDllWarnings.appendMessage( msg );
            end
          ;
        {$ENDIF}
        Application.ProcessMessages();
      {$ENDIF}
    end
  ;


  function naadsm_simulation_stop(): integer;
    begin
      {$IFDEF DEBUG}
        dbcout( 'NAADSM: ++ Start naadsm_simulation_stop', DBSHOWMSG );
      {$ENDIF}

      Application.ProcessMessages();

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        {$IFNDEF CONSOLEAPP}
        if( assigned( frmMain ) ) then
          result := frmMain.userStop
        else
          result := 0
        ;
        {$ELSE}
          result := 0;
        {$ENDIF}
      {$IFDEF DEBUG}
      except
        dbcout( 'NAADSM: Exception caught in naadsm_simulation_stop', true );
        result := 0;
      end;
      {$ENDIF}

      {$IFDEF DEBUG}
        dbcout( 'NAADSM: -- End naadsm_simulation_stop', DBSHOWMSG );
      {$ENDIF}
    end
  ;


  procedure naadsm_sim_start();
    var
      prevFileName: string;
      fn: string;
    begin
      {$IFDEF DEBUG}
        dbcout( 'NAADSM: ++ Start naadsm_sim_start', DBSHOWMSG );
      {$ENDIF}

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        if( _smSim.useCustomOutputs ) then
          _smdb.initializeCustomOutputTables( _smSim.customOutputDefinitions )
        else
          _smdb.dropCustomOutputTables()
        ;

        if( _smSim.storeSelectDailyOutputs ) then
          _smdb.initializeSelectDailyOutputTables( _smSim.selectDailyOutputs )
        else
          _smdb.dropSelectDailyOutputTables()
        ;

        _smdb.initializeAllOutputRecords();
        _smdb.initializeRemoteDatabase();
        _smSim.initializeAllOutputRecords();
        _herds.initializeAllOutputRecords();

        _smdb.recordStartTime( VERSIONFOROUTPUT );

        _simIteration := -1;
        _simDay := -1;
        _diseaseEndDay := -1;
        _outbreakEnd := -1;

        _eventCounter := 1;
        _exposureCounter := 1;
        
        _herdsInZones := TQIntegerObjectMap.create();

       {$IFNDEF CONSOLEAPP}
        if( assigned( frmMap ) ) then
          frmMap.clearZones()
        ;
        {$ENDIF}

        // Don't make the DLL go to the trouble of creating these potentially long
        // strings if they're not going to be used for anything.
        if( not( _smSim.outputOptions.writeDailyStatesFile ) ) then
          begin
            set_show_all_states( nil );
            set_show_all_prevalences( nil );
          end
        else
          begin
            set_show_all_states( @naadsm_show_all_states );
            set_show_all_prevalences( @naadsm_show_all_prevalences );

            try
              fn := _smSim.outputOptions.dailyStatesFileName;
              assignFile( _dailyStatesFile, fn );
              rewrite( _dailyStatesFile );

              if( _smSim.useWithinHerdPrevalence ) then
                begin
                  if( '.txt' = ansiLowerCase( rightStr( fn, 4 ) ) ) then
                    prevFileName := leftStr(  fn, length( fn ) - 4 )
                  else
                    prevFileName := fn
                  ;
                  prevFileName := prevFileName + '-prev.txt';

                  assignFile( _dailyPrevalenceFile, prevFileName );
                  rewrite( _dailyPrevalenceFile );
                end
              ;
            except
              // fail silently... for now.
            end;
          end
        ;

        if( _smSim.outputOptions.writeNAADSMapOutput ) then
          begin
            _naadsMap := TNAADSMap.create( _smSim, _herds, _smdb );
            _naadsMap.simStart();
          end
        else
          _naadsMap := nil
        ;

        Application.ProcessMessages();
        {$IFNDEF CONSOLEAPP}
        if( assigned( frmMain ) ) then frmMain.simStart();
        {$ENDIF}
      {$IFDEF DEBUG}
      except
        dbcout( 'NAADSM: Exception caught in naadsm_sim_start', true );
      end;
      {$ENDIF}

      {$IFDEF DEBUG}
        dbcout( 'NAADSM: -- End naadsm_sim_start', DBSHOWMSG );
      {$ENDIF}
    end
  ;


  procedure naadsm_iteration_start( it: integer );
    begin
      {$IFDEF DEBUG}
        dbcout( 'NAADSM: ++ naadsm_iteration_start ' + intToStr( it + 1 ) + ': ' + intToStr( getCurrentProcessMemorySize() ), DBSHOWMSG );
      {$ENDIF}

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        {$IFDEF DEBUG}
          dbcout( 'NAADSM: Iteration ' + intToStr( it + 1 ) + ' is beginning', DBSHOWMSG );
        {$ENDIF}

        Application.ProcessMessages();
        _smdb.prepareForIteration( it + 1 );
        _smSim.prepareForIteration( it + 1 );
        _herds.prepareForIteration( it + 1 );

        _simIteration := it + 1;
        _simDay := -1;
        _diseaseEndDay := -1;
        _outbreakEnd := -1;

        _eventCounter := 1;
        _exposureCounter := 1;

        _herdsInZones.clear();

        if( _smSim.outputOptions.writeDailyStatesFile ) then
          begin
            try
              if( 0 = it ) then
                writeln( _dailyStatesFile, 'Iteration ' + intToStr( it + 1 ) )
              else
                writeln( _dailyStatesFile, endl + 'Iteration ' + intToStr( it + 1 ) )
              ;

              if( _smSim.useWithinHerdPrevalence ) then
                begin
                  if( 0 = it ) then
                    writeln( _dailyPrevalenceFile, 'Iteration ' + intToStr( it + 1 ) )
                  else
                    writeln( _dailyPrevalenceFile, endl + 'Iteration ' + intToStr( it + 1 ) )
                  ;
                  writeln( _dailyPrevalenceFile, 'Day, Herd ID, Status, Prevalence' );
                end
              ;
            except
              // fail silently
            end;
          end
        ;

        if( assigned( _naadsMap ) ) then
          _naadsMap.iterationStart( _simIteration )
        ;


        {$IFNDEF CONSOLEAPP}
        if( assigned( frmMain ) ) then frmMain.iterationStart( it );
        {$ENDIF}
      {$IFDEF DEBUG}
      except
        dbcout( 'NAADSM: Exception caught in naadsm_sim_start', true );
      end;
      {$ENDIF}

      {$IFDEF DEBUG}
        dbcout( 'NAADSM: -- End naadsm_iteration_start', DBSHOWMSG );
      {$ENDIF}
    end
  ;


  procedure naadsm_day_start( day: integer );
    begin
      {$IFDEF DEBUG}
        dbcout( 'NAADSM: ++ naadsm_day_start ' + intToStr( day ), DBSHOWMSG );
      {$ENDIF}

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        dbcout( 'NAADSM: Day ' + intToStr( day ) + ' starting.', DBSHOWMSG );

        _smSim.prepareForDay( day );
        // Right now, there is no herd-based daily preparation required.
        //_herds.prepareForDay( day );
        _simDay := day;
        _exposureCounter := 1;
        _eventCounter := 1;

        Application.ProcessMessages();

        {$IFNDEF CONSOLEAPP}
        if( assigned( frmMain ) ) then frmMain.dayStart( day );
        {$ENDIF}
      {$IFDEF DEBUG}
      except
        dbcout( 'NAADSM: Exception caught in naadsm_day_start', true );
      end;
      {$ENDIF}

      {$IFDEF DEBUG}
        dbcout( 'NAADSM: -- End naadsm_day_start', DBSHOWMSG );
      {$ENDIF}
    end
  ;


  procedure naadsm_change_herd_state( r: THRDUpdate );
    var
      h: THerd;
      evt: TSMEvent;
      zoneID: integer;
    begin
      {$IFDEF DEBUG}
        dbcout( 'NAADSM: ++ Start naadsm_change_herd_state', DBSHOWMSG );
      {$ENDIF}

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        // Do the important work
        //----------------------
        h := _herds[r.herdIndex];
        h.changeHerdState( TNAADSMDiseaseState( r.status ), _simDay );

        {$IFNDEF CONSOLEAPP}
        if( assigned( frmMain ) ) then frmMain.changeHerdState( h );
        {$ENDIF}

        if( assigned( _naadsMap ) ) then _naadsMap.herdEvent( h, EVTTransistionStateChange, _simDay );

        Application.ProcessMessages();


        // Deal with daily events
        //-----------------------
        if( _smSim.outputOptions.saveDailyEvents ) then
          begin
            if( nil <> _smSim.zoneList.findByLevel( h.zoneLevel ) ) then
              zoneID := _smSim.zoneList.findByLevel( h.zoneLevel ).id
            else
              zoneID := -1
            ;

            evt := TSMEvent.create( _eventCounter, _simIteration, _simDay, h.id, zoneID, EVTTransistionStateChange, h.diseaseStatus );
            _eventList.append( evt );
            inc( _eventCounter );

            {$IFDEF DEBUG}
              dbcout( 'NAADSM: Events in list: ' + intToStr( _eventList.Count ), DBSHOWMSG );
            {$ENDIF}

            // frmOutputEvents is not currently updated dynamically.  Some day it might be...
            //if( assigned( frmOutputEvents ) ) then frmOutputEvents.appendEvent( evt );
          end
        ;
      {$IFDEF DEBUG}
      except
        dbcout( 'NAADSM: Exception caught in naadsm_change_herd_state', true );
      end;
      {$ENDIF}

      {$IFDEF DEBUG}
        dbcout( 'NAADSM: -- End naadsm_change_herd_state', DBSHOWMSG );
      {$ENDIF}
    end
  ;


  procedure naadsm_expose_herd( e: THRDExpose );
    var
      exposingZoneID, exposedZoneID: integer;
    begin
      {$IFDEF DEBUG}
        dbcout( 'NAADSM: ++ Start naadsm_expose_herd', DBSHOWMSG );

        //dbcout( 'simDay: ' + intToStr( _simDay ), true );
        //debugHRDExpose( e );
        (*
        if( _simDay <> e.finalizedDay ) then
          begin
            dbcout( 'simDay: ' + intToStr( _simDay ), true );
            debugHRDExpose( e );
            raise exception.create( 'Days are out of sync in naadsm_expose_herd()' );
          end
        ;
        *)
      {$ENDIF}

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        {$IFDEF DEBUG}
          dbcout(
          'NAADSM: Herd with index ' + intToStr( e.destIndex )
            + ' was exposed by ' + naadsmContactStr( e.exposureMethod )
            + ' from herd ' + intToStr( e.srcIndex )
            + ' adequate: ' + naadsmSuccessStr( e.isAdequate )
          , DBSHOWMSG
          );
        {$ENDIF}

        // Do some error checking
        //-----------------------
        if
          ( not( _smSim.includeContactSpreadGlobal ) )
        and
          ( e.exposureMethod in [NAADSMDirectContact, NAADSMIndirectContact] )
        then
          raise exception.Create( 'Contact spread is being conducted when it should be disabled.' )
        ;

        if
          ( not( _smSim.includeAirborneSpreadGlobal ) )
        and
          ( NAADSMAirborneSpread = e.exposureMethod )
        then
          raise exception.Create( 'Airborne spread is being conducted when it should be disabled.' )
        ;

        // Process the event
        //------------------
        if( _smSim.outputOptions.saveDailyExposuresAndTraces ) then
          begin
            if( nil <> _smSim.zoneList.findByLevel( _herds[ e.srcIndex ].zoneLevel ) ) then
              exposingZoneID := _smSim.zoneList.findByLevel( _herds[ e.srcIndex ].zoneLevel ).id
            else
              exposingZoneID := -1
            ;

            if( nil <> _smSim.zoneList.findByLevel( _herds[ e.destIndex ].zoneLevel ) ) then
              exposedZoneID := _smSim.zoneList.findByLevel( _herds[ e.destIndex ].zoneLevel ).id
            else
              exposedZoneID := -1
            ;

            _exposureList.append(
              TSMExposureOrTrace.create(
                _exposureCounter,
                _simIteration,
                e,
                _herds[ e.srcIndex ].id,
                _herds[ e.destIndex ].id,
                exposingZoneID,
                exposedZoneID
              )
            );

            inc( _exposureCounter );

            // frmOutputExposures is not currently updated dynamically.  Some day it might be...
            //if( assigned( frmOutputExposures ) ) then frmOutputExposures.appendExposure( exp );
          end
        ;

        _herds[e.destIndex].expose( e );

        Application.ProcessMessages();

        // There isn't really anything to do on the main form.
        //if( assigned( frmMain ) ) then frmMain.exposeHerd( r );
      {$IFDEF DEBUG}
      except
        dbcout( 'NAADSM: Exception caught in naadsm_expose_herd', true );
      end;
      {$ENDIF}

      {$IFDEF DEBUG}
        dbcout( 'NAADSM: -- End naadsm_expose_herd', DBSHOWMSG );
      {$ENDIF}
    end
  ;


  procedure naadsm_trace_herd( t: THRDTrace );
    var
      identifiedHerd, originHerd: THerd;
      identifiedZoneID, originZoneID: integer;
    begin
      {$IFDEF DEBUG}
        dbcout( 'NAADSM: ++ Start naadsm_trace_herd', DBSHOWMSG );

        // Do some error checking
        //-----------------------
        if( not( _smSim.includeTracingGlobal ) ) then
          begin
            raise exception.Create( 'Tracing is being conducted when it should be disabled.' );
          end
        ;
      {$ENDIF}

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        {$IFDEF DEBUG}
          //dbcout( 'NAADSM: Trace attempted for herd with index ' + intToStr( t.identifiedIndex ) + ' for ' + naadsmContactStr( t.contactType ), DBSHOWMSG );
          //dbcout( '        Origin was herd with index ' + intToStr( t.originIndex ), DBSHOWMSG );
        {$ENDIF}

        // Do the important work
        //----------------------
        identifiedHerd := _herds[t.identifiedIndex];
        identifiedHerd.attemptTraceForReason( t );

        if( naadsmSuccessIsTrue( t.success ) ) then // Trace was successful.
          begin
            identifiedHerd.traceForReason( t );
            {$IFNDEF CONSOLEAPP}
              if( assigned( frmMain ) ) then frmMain.traceHerd( identifiedHerd );
            {$ENDIF}
          end
        ;

        originHerd := _herds[ t.originIndex ];
        originHerd.recordTraceOrigin( t );
        // Currently, the code does not record successful traces by origin,
        // nor does the map display them.


        Application.ProcessMessages();

        // Deal with daily traces
        //-----------------------
        if( _smSim.outputOptions.saveDailyExposuresAndTraces ) then
          begin
            if( nil <> _smSim.zoneList.findByLevel( identifiedHerd.zoneLevel ) ) then
              identifiedZoneID := _smSim.zoneList.findByLevel( identifiedHerd.zoneLevel ).id
            else
              identifiedZoneID := -1
            ;

            if( nil <> _smSim.zoneList.findByLevel( originHerd.zoneLevel ) ) then
              originZoneID := _smSim.zoneList.findByLevel( originHerd.zoneLevel ).id
            else
              originZoneID := -1
            ;

            _exposureList.append(
              TSMExposureOrTrace.create(
                _exposureCounter,
                _simIteration,
                t,
                _herds,
                originZoneID,
                identifiedZoneID
              )
            );

            inc( _exposureCounter );

            // frmOutputEvents is not currently updated dynamically.  Some day it might be...
            //if( assigned( frmOutputEvents ) ) then frmOutputEvents.appendEvent( evt );
          end
        ;
      {$IFDEF DEBUG}
      except
        dbcout( 'NAADSM: Exception caught in naadsm_trace_herd', true );
      end;
      {$ENDIF}

      {$IFDEF DEBUG}
        dbcout( 'NAADSM: -- End naadsm_trace_herd', DBSHOWMSG );
      {$ENDIF}
    end
  ;


  procedure naadsm_examine_herd( e: THRDExam );
   var
      h: THerd;
      evt: TSMEvent;
      zoneID: integer;
    begin
      {$IFDEF DEBUG}
        dbcout( 'NAADSM: ++ Start naadsm_examine_herd', DBSHOWMSG );
      {$ENDIF}

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        {$IFDEF DEBUG}
          dbcout( 'NAADSM: Exam for herd with index ' + intToStr( e.herdIndex ) + ' for ' + naadsmContactStr( e.contactType ) + ' on day ' + intToStr( _simDay ), DBSHOWMSG );
        {$ENDIF}

        h := _herds[e.herdIndex];
        h.conductHerdExam( e );

        if( _smSim.outputOptions.saveDailyEvents ) then
          begin
            if( nil <> _smSim.zoneList.findByLevel( h.zoneLevel ) ) then
              zoneID := _smSim.zoneList.findByLevel( h.zoneLevel ).id
            else
              zoneID := -1
            ;

            // We don't care why an exam took place for the events table.
            evt := TSMEvent.create( _eventCounter, _simIteration, _simDay, h.id, zoneID, EVTHerdExam );
            _eventList.append( evt );
            inc( _eventCounter );
          end
        ;

        Application.ProcessMessages();

        // There isn't currently anything to do on the main form
        //if( assigned( frmMain ) ) then frmMain.conductHerdExam( e );
      {$IFDEF DEBUG}
      except
        dbcout( 'NAADSM: Exception caught in naadsm_examine_herd', true );
      end;
      {$ENDIF}

      {$IFDEF DEBUG}
        dbcout( 'NAADSM: -- End naadsm_examine_herd', DBSHOWMSG );
      {$ENDIF}
    end
  ;


  procedure naadsm_test_herd( t: THRDTest );
   var
      h: THerd;
      evt: TSMEvent;
      zoneID: integer;
    begin
      {$IFDEF DEBUG}
        dbcout( 'NAADSM: ++ Start naadsm_test_herd', DBSHOWMSG );
      {$ENDIF}

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        {$IFDEF DEBUG}
          dbcout( 'NAADSM: Testing for herd with index ' + intToStr( t.herdIndex ), DBSHOWMSG );
        {$ENDIF}

        // Do some error checking
        //-----------------------
        if( not( _smSim.includeTracingTestingGlobal ) ) then
          raise exception.Create( 'Diagnostic testing is being conducted when it should be disabled.' )
        ;

        // Do the real work
        //-----------------
        h := _herds[t.herdIndex];
        h.conductDiagnosticTest( t );

        if( _smSim.outputOptions.saveDailyEvents ) then
          begin
            if( nil <> _smSim.zoneList.findByLevel( h.zoneLevel ) ) then
              zoneID := _smSim.zoneList.findByLevel( h.zoneLevel ).id
            else
              zoneID := -1
            ;

            // We don't care why an exam took place for the events table.
            evt := TSMEvent.create( _eventCounter, _simIteration, _simDay, h.id, zoneID, EVTDiagnosticTest, NAADSMStateUnspecified, NAADSMSuccessUnspecified, t.testResult );
            _eventList.append( evt );
            inc( _eventCounter );
          end
        ;

        Application.ProcessMessages();

        // There isn't currently anything to do on the main form
        //if( assigned( frmMain ) ) then frmMain.conductDiagnosticTest( t );
      {$IFDEF DEBUG}
      except
        dbcout( 'NAADSM: Exception caught in naadsm_test_herd', true );
      end;
      {$ENDIF}

      {$IFDEF DEBUG}
        dbcout( 'NAADSM: -- End naadsm_test_herd', DBSHOWMSG );
      {$ENDIF}
    end
  ;


  procedure naadsm_queue_herd_for_destruction( herdIndex: integer );
    var
      h: THerd;
    begin
      {$IFDEF DEBUG}
        dbcout( 'NAADSM: Queuing herd ' + intToStr( herdIndex ) + ' for destruction on day ' + intToStr( _simDay ), DBSHOWMSG );
      {$ENDIF}

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        // Do some error checking
        //-----------------------
        if( not( _smSim.includeDestructionGlobal ) and ( -1 <> _simDay ) ) then
          raise exception.Create( 'Destruction is being conducted when it should be disabled.' )
        ;

        // Do the important work
        //----------------------
        h := _herds[ herdIndex ];
        h.queueForDestruction( _simDay );
        {$IFNDEF CONSOLEAPP}
          if( assigned( frmMain ) ) then frmMain.quarantineHerd( h );
        {$ENDIF}

        Application.ProcessMessages();

        // There are currently no daily events to deal with
      {$IFDEF DEBUG}
      except
        dbcout( 'NAADSM: Exception caught in naadsm_queue_herd_for_destruction', true );
      end;
      {$ENDIF}
    end
  ;


  procedure naadsm_queue_herd_for_vaccination( herdIndex: integer );
    begin
      {$IFDEF DEBUG}
        dbcout( 'NAADSM: ++ Start naadsm_queue_herd_for_vaccination', DBSHOWMSG );
      {$ENDIF}

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        // Do some error checking
        //-----------------------
        if( not( _smSim.includeVaccinationGlobal ) and ( -1 <> _simDay ) ) then
          raise exception.Create( 'Vaccination is being conducted when it should be disabled' )
        ;

        // Do the important work
        //----------------------
        dbcout( 'NAADSM: Queuing herd ' + intToStr( _herds[ herdIndex ].id ) + ' for vaccination on day ' + intToStr( _simDay ), DBSHOWMSG );

        _herds[ herdIndex ].queueForVaccination( _simDay );

        // There is currently nothing to do on the main form

        Application.ProcessMessages();

        // There are currently no daily events to deal with
        
      {$IFDEF DEBUG}
      except
        dbcout( 'NAADSM: Exception caught in naadsm_queue_herd_for_vaccination', true );
      end;
      {$ENDIF}

      {$IFDEF DEBUG}
        dbcout( 'NAADSM: -- End naadsm_queue_herd_for_vaccination', DBSHOWMSG );
      {$ENDIF}
    end
  ;


  procedure naadsm_infect_herd( r: THRDInfect );
    var
      h: THerd;
      evt: TSMEvent;
      zoneID: integer;
    begin
      {$IFDEF DEBUG}
        dbcout( 'NAADSM: ++ Start naadsm_infect_herd', DBSHOWMSG );
      {$ENDIF}

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        {$IFDEF DEBUG}
          dbcout( 'NAADSM: Herd with index ' + intToStr( r.herdIndex ) + ' was infected by ' + naadsmContactStr( r.infectionSourceType ), DBSHOWMSG );
        {$ENDIF}

        // Do some error checking
        //-----------------------
        if( not( _smSim.includeContactSpreadGlobal ) ) then
          begin
            if( ( NAADSMDirectContact = r.infectionSourceType ) or ( NAADSMIndirectContact = r.infectionSourceType ) ) then
              raise exception.Create( 'Disease spread by contact is being conducted when it should be disabled.' )
            ;
          end
        ;

        if( not( _smSim.includeAirborneSpreadGlobal ) ) then
          begin
            if( NAADSMAirborneSpread = r.infectionSourceType ) then
              raise exception.Create( 'Disease spread by airborne transmission is being conducted when it should be disabled.' )
            ;
          end
        ;

        Application.ProcessMessages();
        h := _herds[r.herdIndex];
        h.infect( r, _simDay );

        if( assigned( _naadsMap ) ) then
          _naadsMap.herdEvent( h, EVTInfected, _simDay )
        ;

        if( ( _smSim.outputOptions.saveDailyEvents ) ) then
          begin
            if( nil <> _smSim.zoneList.findByLevel( h.zoneLevel ) ) then
              zoneID := _smSim.zoneList.findByLevel( h.zoneLevel ).id
            else
              zoneID := -1
            ;

            evt := TSMEvent.create( _eventCounter, _simIteration, _simDay, h.id, zoneID, EVTInfected );
            _eventList.append( evt );
            inc( _eventCounter );

            // frmOutputEvents is not currently updated dynamically.  Some day it might be...
            //if( assigned( frmOutputEvents ) ) then frmOutputEvents.appendEvent( evt );
          end
        ;

        // There isn't currently anything to do on the main form
        //if( assigned( frmMain ) ) then frmMain.infectHerd( h );
      {$IFDEF DEBUG}
      except
        dbcout( 'NAADSM: Exception caught in naadsm_infect_herd', true );
      end;
      {$ENDIF}

      {$IFDEF DEBUG}
        dbcout( 'NAADSM: -- End naadsm_infect_herd', DBSHOWMSG );
      {$ENDIF}
    end
  ;


  (*
    In NAADSM 3.x, when a unit is infected by multiple causes/exposures on the same day,
    only one cause/exposure is selected at random to be reported as "the" cause of
    infection. For infections, this process is handled by the core library, in the unit
    conflict-resolver: the UI doesn't need to do anything special in this situation.

    In keeping with this line of reasoning, when a unit is detected by multiple mechanisms
    on the same day in NAADSM 3.2, one cause will be selected at random and will be reported
    as "the" cause of detection.  [Note that this can (hopefully rarely) result in a truly
    clinical unit being reported as a false positive detection based on a diagnostic test,
    if the test was carried out before the unit was infected and the test result was delayed.]

    For detections, this process IS NOT handled by the core library: additional code is needed
    in the UI to deal with this situation.

    The approach used to track causes of infection and detection will be substantially revised
    in NAADSM 4.
  *)
  procedure naadsm_detect_herd( d: THRDDetect );
    var
      detections: TQObjectList;
      det: TNAADSM3Detect;
    begin
      {$IFDEF DEBUG}
        dbcout( 'NAADSM: ++ Start naadsm_detect_herd', DBSHOWMSG );
      {$ENDIF}

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        dbcout( 'NAADSM: Herd with index ' + intToStr( d.herdIndex ) + ' was detected on day ' + intToStr( _simDay ), DBSHOWMSG );

        // Do some error checking
        //-----------------------
        if( not( _smSim.includeDetectionGlobal ) ) then
          raise exception.Create( 'Detection is being conducted when it should be disabled.' )
        ;

        // Do the real work
        //-----------------
        // in NAADSM 3.2, all this function has to do is add the detection event
        // to the appropriate list.  Detections will be handled at the end of
        // each simulation day by naadsm3ProcessDetections().
        if( _naadsm3Detections.contains( d.herdIndex ) ) then
          detections := _naadsm3Detections.value( d.herdIndex ) as TQObjectList
        else
          begin
            detections := TQObjectList.create();
            _naadsm3Detections.insert( d.herdIndex , detections );
          end
        ;

        det := TNAADSM3Detect.create( d );
        detections.append( det );
      {$IFDEF DEBUG}
      except
        dbcout( 'NAADSM: Exception caught in naadsm_detect_herd', true );
      end;
      {$ENDIF}

      {$IFDEF DEBUG}
        dbcout( 'NAADSM: -- End naadsm_detect_herd', DBSHOWMSG );
      {$ENDIF}
    end
  ;


  procedure naadsm3ProcessDetections();
    var
      {$IFNDEF CONSOLEAPP}
      wasFirstEvent: boolean;
      {$ENDIF}
      h: THerd;
      evt: TSMEvent;
      zoneID: integer;

      i: integer;
      detections: TQObjectList;
      det: TNAADSM3Detect;
      d: THRDDetect;
      detIdx: integer;
    begin
      {$IFDEF DEBUG}
      try
      {$ENDIF}
        // For each detected herd...
        //--------------------------
        for i := 0 to _naadsm3Detections.count - 1 do
          begin
            // Did multiple detection events occur?  If so, choose one at random
            //------------------------------------------------------------------
            detections := _naadsm3Detections.itemAtIndex( i ) as TQObjectList;

            if( 1 = detections.count ) then
              detIdx := 0
            else
              detIdx := rngRandInt( 0, detections.count )
            ;

            det := detections.at( detIdx ) as TNAADSM3Detect;

            d.herdIndex := det.herdIndex;
            d.reason := det.reason;
            d.testResult := det.testResult;

            // Once a detection has been selected, free all of the list items.  The list itself will be freed below.
            detections.freeAllValues();

            // Do the important work
            //----------------------
            dbcout( 'NAADSM: Herd with index ' + intToStr( d.herdIndex ) + ' will really be detected', DBSHOWMSG );

            h := _herds[d.herdIndex];
            {$IFNDEF CONSOLEAPP}
              wasFirstEvent := h.detect( d, _simDay );
              if( assigned( frmMain ) ) then frmMain.detectHerd( h, wasFirstEvent, _simDay );
            {$ELSE}
              h.detect( d, _simDay );
            {$ENDIF}

            if( assigned( _naadsMap ) ) then _naadsMap.herdEvent( h, EVTDetected, _simDay );

            Application.ProcessMessages();

            // Deal with daily events
            //-----------------------
            if( _smSim.outputOptions.saveDailyEvents ) then
              begin
                if( nil <> _smSim.zoneList.findByLevel( h.zoneLevel ) ) then
                  zoneID := _smSim.zoneList.findByLevel( h.zoneLevel ).id
                else
                  zoneID := -1
                ;

                evt := TSMEvent.create( _eventCounter, _simIteration, _simDay, h.id, zoneID, EVTDetected );
                _eventList.append( evt );
                inc( _eventCounter );

                // frmOutputEvents is not currently updated dynamically.  Some day it might be...
                //if( assigned( frmOutputEvents ) ) then frmOutputEvents.appendEvent( evt );
              end
            ;
          end
        ;

        // Once all detections have been processed, clear out the map for later use
        //-------------------------------------------------------------------------
        _naadsm3Detections.deleteValues();

      {$IFDEF DEBUG}
      except
        dbcout( 'NAADSM: Exception caught in naadsm3ProcessDetections', true );
      end;
      {$ENDIF}
    end
  ;


  procedure naadsm_destroy_herd( c: THRDControl );
    var
      {$IFNDEF CONSOLEAPP}
      wasFirstEvent: boolean;
      {$ENDIF}
      h: THerd;
      evt: TSMEvent;
      zoneID: integer;
    begin
      {$IFDEF DEBUG}
        dbcout( 'NAADSM: ++ Start naadsm_destroy_herd', DBSHOWMSG );
        dbcout( 'NAADSM: Destroying herd ' + intToStr( _herds[ c.herdIndex ].id ) + ' on day ' + intToStr( _simDay ), DBSHOWMSG );
      {$ENDIF}

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        // Do some error checking
        //-----------------------
        if( not( _smSim.includeDestructionGlobal ) and ( -1 <> _simDay ) ) then
          raise exception.Create( 'Destruction is being conducted when it should be disabled.' )
        ;

        // Do the important work
        //----------------------
        h := _herds[c.herdIndex];

        {$IFNDEF CONSOLEAPP}
          wasFirstEvent := h.destroyForReason( c, _simDay );
          if( assigned( frmMain ) ) then frmMain.destroyHerd( h, wasFirstEvent, _simDay );
        {$ELSE}
          h.destroyForReason( c, _simDay );
        {$ENDIF}

        if( assigned( _naadsMap ) ) then
          _naadsMap.herdEvent( h, EVTDestroyed, _simDay )
        ;

        // If this herd was previously "enzoned", it can be taken out of the list now.
        if( _herdsInZones.contains( c.herdIndex ) ) then
          _herdsInZones.Remove( c.herdIndex )
        ;

        Application.ProcessMessages();


        // Deal with daily events
        //-----------------------
        if( _smSim.outputOptions.saveDailyEvents ) then
          begin
            if( nil <> _smSim.zoneList.findByLevel( h.zoneLevel ) ) then
              zoneID := _smSim.zoneList.findByLevel( h.zoneLevel ).id
            else
              zoneID := -1
            ;

            evt := TSMEvent.create( _eventCounter, _simIteration, _simDay, h.id, zoneID, EVTDestroyed );
            _eventList.append( evt );
            inc( _eventCounter );

            // frmOutputEvents is not currently updated dynamically.  Some day it might be...
            //if( assigned( frmOutputEvents ) ) then frmOutputEvents.appendEvent( evt );
          end
        ;
      {$IFDEF DEBUG}
      except
        dbcout( 'NAADSM: Exception caught in naadsm_destroy_herd', true );
      end;
      {$ENDIF}

      {$IFDEF DEBUG}
        dbcout( 'NAADSM: -- End naadsm_destroy_herd', DBSHOWMSG );
      {$ENDIF}
    end
  ;


  procedure naadsm_vaccinate_herd( c: THRDControl );
    var
      {$IFNDEF CONSOLEAPP}
      wasFirstEvent: boolean;
      {$ENDIF}
      h: THerd;
      evt: TSMEvent;
      zoneID: integer;
    begin
      {$IFDEF DEBUG}
        dbcout( 'NAADSM: ++ Start naadsm_vaccinate_herd', DBSHOWMSG );
      {$ENDIF}

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        // Do some error checking
        //-----------------------
        if( not( _smSim.includeVaccinationGlobal ) and ( -1 <> _simDay ) ) then
          raise exception.Create( 'Vaccination is being conducted when it should be disabled' )
        ;
        
        // Do the important work
        //----------------------
        h := _herds[c.herdIndex];

        dbcout( 'NAADSM: Vaccinating herd ' + intToStr( h.id ) + ' on day ' + intToStr( _simDay ), DBSHOWMSG );

        {$IFNDEF CONSOLEAPP}
          wasFirstEvent := h.vaccinateForReason( c, _simDay );
          if( assigned( frmMain ) ) then frmMain.vaccinateHerd( h, wasFirstEvent, _simDay );
        {$ELSE}
          h.vaccinateForReason( c, _simDay );
        {$ENDIF}

        if( assigned( _naadsMap ) ) then
          _naadsMap.herdEvent( h, EVTVaccinated, _simDay )
        ;

        Application.ProcessMessages();


        // Deal with daily events
        //-----------------------
        if( _smSim.outputOptions.saveDailyEvents ) then
          begin
            if( nil <> _smSim.zoneList.findByLevel( h.zoneLevel ) ) then
              zoneID := _smSim.zoneList.findByLevel( h.zoneLevel ).id
            else
              zoneID := -1
            ;

            evt := TSMEvent.create( _eventCounter, _simIteration, _simDay, h.id, zoneID, EVTVaccinated );
            _eventList.append( evt );
            inc( _eventCounter );

            // frmOutputEvents is not currently updated dynamically.  Some day it might be...
            //if( assigned( frmOutputEvents ) ) then frmOutputEvents.appendEvent( evt );
          end
        ;
      {$IFDEF DEBUG}
      except
        dbcout( 'NAADSM: Exception caught in naadsm_vaccinate_herd', true );
      end;
      {$ENDIF}

      {$IFDEF DEBUG}
        dbcout( 'NAADSM: -- End naadsm_vaccinate_herd', DBSHOWMSG );
      {$ENDIF}
    end
  ;


  procedure naadsm_cancel_herd_vaccination( c: THRDControl );
    begin
      {$IFDEF DEBUG}
        dbcout( 'NAADSM: ++ Start naadsm_cancel_herd_vaccination', DBSHOWMSG );
      {$ENDIF}

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        // Do some error checking
        //-----------------------
        if( not( _smSim.includeVaccinationGlobal ) and ( -1 <> _simDay ) ) then
          raise exception.Create( 'Vaccination is being canceled when it should be disabled' )
        ;

        // Do the important work
        //----------------------
        dbcout( 'NAADSM: Canceling vaccination for herd ' + intToStr( _herds[ c.herdIndex ].id ) + ' on day ' + intToStr( _simDay ), DBSHOWMSG );

        _herds[c.herdIndex].cancelVaccination( c, _simDay );

        // There is currently nothing to do on the main form

        Application.ProcessMessages();

        // There are currently no daily events to deal with
        
      {$IFDEF DEBUG}
      except
        dbcout( 'NAADSM: Exception caught in naadsm_cancel_herd_vaccination', true );
      end;
      {$ENDIF}


      {$IFDEF DEBUG}
        dbcout( 'NAADSM: -- End naadsm_cancel_herd_vaccination', DBSHOWMSG );
      {$ENDIF}
    end
  ;


  procedure naadsm_show_all_states( statesMsg: pchar );
    var
      stateStr: string;
    begin
      {$IFDEF DEBUG}
        dbcout( 'NAADSM: ++ Start naadsm_show_all_states', DBSHOWMSG );
      {$ENDIF}

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        if( _smSim.outputOptions.writeDailyStatesFile ) then
          begin
            stateStr := statesMsg;
            stateStr := AnsiReplaceStr( stateStr, '0', 'S' );
            stateStr := AnsiReplaceStr( stateStr, '1', 'L' );
            stateStr := AnsiReplaceStr( stateStr, '2', 'B' );
            stateStr := AnsiReplaceStr( stateStr, '3', 'C' );
            stateStr := AnsiReplaceStr( stateStr, '4', 'N' );
            stateStr := AnsiReplaceStr( stateStr, '5', 'V' );
            stateStr := AnsiReplaceStr( stateStr, '6', 'D' );

            try
              writeln( _dailyStatesFile, stateStr );
            except
              // fail silently
            end;
          end
        ;
      {$IFDEF DEBUG}
      except
        dbcout( 'NAADSM: Exception caught in naadsm_show_all_states', true );
      end;
      {$ENDIF}

      {$IFDEF DEBUG}
        dbcout( 'NAADSM: -- End naadsm_show_all_states', DBSHOWMSG );
      {$ENDIF}
    end
  ;


  procedure naadsm_show_all_prevalences(  prevMsg: pchar );
    var
      prevStr: string;
    begin
      if( _smSim.useWithinHerdPrevalence and _smSim.outputOptions.writeDailyStatesFile ) then
        begin
          try
            prevStr := prevMsg;
            prevStr := AnsiReplaceStr( prevStr, 's0s', 'S' );
            prevStr := AnsiReplaceStr( prevStr, 's1s', 'L' );
            prevStr := AnsiReplaceStr( prevStr, 's2s', 'B' );
            prevStr := AnsiReplaceStr( prevStr, 's3s', 'C' );
            prevStr := AnsiReplaceStr( prevStr, 's4s', 'N' );
            prevStr := AnsiReplaceStr( prevStr, 's5s', 'V' );
            prevStr := AnsiReplaceStr( prevStr, 's6s', 'D' );

            writeln( _dailyPrevalenceFile, prevStr );
          except
            // fail silently
          end;
        end
      ;
    end
  ;


  procedure naadsm_disease_end( val: integer );
    begin
      {$IFDEF DEBUG}
        dbcout( 'NAADSM: ++ Start naadsm_disease_end', DBSHOWMSG );
      {$ENDIF}

      {$IFDEF DEBUG}
      try
      {$ENDIF}

        {$IFDEF DEBUG}
          dbcout( 'NAADSM: Disease ended on day ' + intToStr( val ), DBSHOWMSG );
        {$ENDIF}
        _diseaseEndDay := val;

        Application.ProcessMessages();
      {$IFDEF DEBUG}
      except
        dbcout( 'NAADSM: Exception caught in naadsm_disease_end', true );
      end;
      {$ENDIF}

      {$IFDEF DEBUG}
        dbcout( 'NAADSM: -- End naadsm_disease_end', DBSHOWMSG );
      {$ENDIF}
    end
  ;


  procedure naadsm_outbreak_end( val: integer );
    begin
      {$IFDEF DEBUG}
        dbcout( 'NAADSM: ++ Start naadsm_outbreak_end', DBSHOWMSG );
      {$ENDIF}

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        {$IFDEF DEBUG}
          dbcout( 'NAADSM: Outbreak ended on day ' + intToStr( val ), DBSHOWMSG );
        {$ENDIF}

        _outbreakEnd := val;

        Application.ProcessMessages();

        {$IFNDEF CONSOLEAPP}
        if( assigned( frmMain ) ) then frmMain.outbreakEnd( val );
        {$ENDIF}
      {$IFDEF DEBUG}
      except
        dbcout( 'NAADSM: Exception caught in naadsm_outbreak_end', true );
      end;
      {$ENDIF}

      {$IFDEF DEBUG}
        dbcout( 'NAADSM: -- End naadsm_outbreak_end', DBSHOWMSG );
      {$ENDIF}
    end
  ;


  procedure naadsm_day_complete( day: integer );
    var
      i: integer;
      h: THerd;
      z: TZone;
    begin
      {$IFDEF DEBUG}
        dbcout( 'NAADSM: ++ Start naadsm_day_complete', DBSHOWMSG );
      {$ENDIF}

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        dbcout( 'NAADSM: Sim day ' + intToStr( day ) + ' is complete.', DBSHOWMSG );

        // See note in naadsm_detect_herd.  This function will not exist in NAADSM 4!
        naadsm3ProcessDetections();

        // Save daily events and exposures, if necessary
        if( 0 = day mod 5 ) then
          begin
            if( _smSim.outputOptions.saveDailyEvents ) then
              begin
                _eventList.populateDatabase( _smdb );
                _eventList.Clear();
              end
            ;

            if( _smSim.outputOptions.saveDailyExposuresAndTraces ) then
              begin
                _exposureList.populateDatabase( _smdb );
                _exposureList.Clear();
              end
            ;
          end
        ;

        // Update the number of herds and animals of each production type in each zone
        for i := 0 to _herdsInZones.count - 1 do
          begin
            h := _herdsInZones.itemAtIndex( i ) as THerd;
            z := _smSim.zoneList.findByLevel( h.zoneLevel );
            z.addToZoneTotals( h.prodTypeID, h.initialSize, _simDay );
          end
        ;

        //_herds.processDailyRecords(); // There aren't currently any end-of-day actions for the herd list
        {$IFNDEF TESTMEMLEAK}
        _smSim.processDailyRecords( _smdb, day );
        {$ENDIF}

        Application.ProcessMessages();

      {$IFNDEF CONSOLEAPP}
        if( assigned( frmMain ) ) then frmMain.dayComplete( day );
      {$ELSE}
        {$IFNDEF TESTMEMLEAK}
        cout( '  Day ' + intToStr( day ) + ' complete.' );
        {$ENDIF}
      {$ENDIF}

      {$IFDEF DEBUG}
      except
        dbcout( 'NAADSM: Exception caught in naadsm_day_complete', true );
      end;
      {$ENDIF}

      {$IFDEF DEBUG}
        dbcout( 'NAADSM: -- End naadsm_day_complete', DBSHOWMSG );
      {$ENDIF}
    end
  ;



  procedure naadsm_iteration_complete( it: integer );
    var
      stopReason: integer;
    begin
      {$IFDEF DEBUG}
        dbcout( 'NAADSM: -- Start naadsm_iteration_complete ' + intToStr( it + 1 ), DBSHOWMSG );
        //dbcout( 'NAADSM: ++ naadsm_iteration_complete ' + intToStr( it + 1 ) + ': ' + intToStr( getCurrentProcessMemorySize() ), DBSHOWMSG );
      {$ENDIF}

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        dbcout( 'NAADSM: Iteration ' + intToStr( it + 1 ) + ' is complete.', DBSHOWMSG );

        // Process any remaining events and exposures
        // (Most events and exposures are processed at the end of each day,
        // but there may be a few that haven't been yet.)
        //------------------------------------------------------------------
        if( _smSim.outputOptions.saveDailyEvents ) then
          begin
            _eventList.populateDatabase( _smdb );
            _eventList.Clear();
          end
        ;

        if( _smSim.outputOptions.saveDailyExposuresAndTraces ) then
          begin
            _exposureList.populateDatabase( _smdb );
            _exposureList.Clear();
          end
        ;

        Application.ProcessMessages();

        if( assigned( _naadsMap ) ) then
          _naadsMap.iterationEnd( it + 1 )
        ;

        // See if the user wants to interrupt the simulation in progress
        //--------------------------------------------------------------
        {$IFNDEF CONSOLEAPP}
          if( assigned( frmMain ) ) then
            stopReason := frmMain.userStop
          else
            stopReason := NO_STOP
          ;
        {$ELSE}
          stopReason := NO_STOP;
        {$ENDIF}

        if( USER_STOP = stopReason ) then
          begin
            // User stopped the simulation before the current iteration completed.
            // Don't record it.
          end
        else
          begin
            // Record the iteration as complete
            //---------------------------------
            // Iterations should be 1-indexed in the database.
            {$IFNDEF TESTMEMLEAK}
            _smdb.processIterationRecords(
              it + 1,
              _simDay,
              ( -1 <> _outbreakEnd ),
              _diseaseEndDay,
              ( -1 <> _diseaseEndDay ),
              _smSim.zoneList.focusCreated
            );

            _smSim.processIterationRecords( _smdb, it + 1 );
            _herds.processIterationRecords( _smdb, it + 1 );

            // Process custom outputs
            //------------------------
            if( _smSim.useCustomOutputs ) then
              begin
                if( _smSim.customOutputDefinitions.hasOutputs( OFIteration ) ) then
                  _smdb.processCustomIterationRecords( it + 1, _smSim.customOutputDefinitions )
                ;

                if( _smSim.customOutputDefinitions.hasOutputs( OFProductionTypeIteration ) ) then
                  _smdb.processCustomProductionTypeRecords( it + 1, _smSim.customOutputDefinitions, _smSim.ptList )
                ;

                if( _smSim.customOutputDefinitions.hasOutputs( OFZoneIteration ) ) then
                  _smdb.processCustomZoneRecords( it + 1, _smSim.customOutputDefinitions, _smSim.zoneList )
                ;

                if( _smSim.customOutputDefinitions.hasOutputs( OFZoneProductionTypeIteration ) ) then
                  _smdb.processCustomZonePTRecords( it + 1, _smSim.customOutputDefinitions, _smSim.zoneList, _smSim.ptList )
                ;
              end
            ;
            {$ENDIF}

            // Update the GUI
            //---------------
            {$IFNDEF CONSOLEAPP}
              if( assigned( frmMain ) ) then frmMain.iterationComplete( it );
            {$ELSE}
              cout( 'Iteration ' + intToStr( it + 1 ) + ' complete.' );
            {$ENDIF}
          end
        ;

      {$IFDEF DEBUG}
      except
        on e: exception do
          begin
            dbcout( 'NAADSM: Exception caught in naadsm_iteration_complete: ' + e.Message , true );
            raise e;
          end
        ;
      end;
      {$ENDIF}

      {$IFDEF DEBUG}
        dbcout( 'NAADSM: -- End naadsm_iteration_complete', DBSHOWMSG );
      {$ENDIF}
    end
  ;


  procedure naadsm_sim_complete( val: integer );
    begin
      {$IFDEF DEBUG}
        dbcout( 'NAADSM: ++ Start naadsm_sim_complete', DBSHOWMSG );
      {$ENDIF}

      {$IFNDEF CONSOLEAPP}
      // frmDllWarnings.enableFormClose(); // People didn't like that they couldn't close this window when a simulation was in progress.  If they screw up, it's now their fault!
      {$ENDIF}

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        if( _smSim.outputOptions.writeDailyStatesFile ) then
          begin
            try
              closeFile( _dailyStatesFile );

              if( _smSim.useWithinHerdPrevalence ) then
                closeFile( _dailyPrevalenceFile )
              ;
            except
              // fail silently
            end;
          end
        ;

        if( 0 <> val ) then
          dbcout( 'NAADSM: Sim is successfully completed.', DBSHOWMSG )
        else
          dbcout( 'NAADSM: Sim did not complete successfully.', DBSHOWMSG )
        ;

        if( 0 <> val ) then
          _smdb.recordEndTime()
        ;

        if( assigned( _naadsMap ) ) then
          begin
            _naadsMap.simEnd( (0 <> val ) );
            freeAndNil( _naadsMap );
          end
        ;

        _herdsInZones.clear();
        // DO NOT delete the items in _herdsInZones: it does not own them.
        _herdsInZones.Free();

        _smSim.simComplete();
        _smdb.simComplete();

        {$IFNDEF CONSOLEAPP}
        if( assigned( frmMap ) ) then
          frmMap.copyZones()
        ;

        if( assigned( frmMain ) ) then frmMain.simComplete( 0 <> val );
        {$ENDIF}

        Application.ProcessMessages();

      {$IFDEF DEBUG}
      except
        dbcout( 'NAADSM: Exception caught in naadsm_sim_complete', true );
      end;
      {$ENDIF}

      {$IFDEF DEBUG}
        dbcout( 'NAADSM: -- End naadsm_sim_complete', DBSHOWMSG );
      {$ENDIF}
    end
  ;


  // The GUI could determine which detections and traces lead to zone foci without
  // relying on the core library.  Using the library, though, acts as an extra sanity check.
  procedure naadsm_make_zone_focus( herdIndex: integer );
    var
      h: THerd;
      evt: TSMEvent;
      zoneID: integer;
    begin
      {$IFDEF DEBUG}
        if( not( _smSim.includeZonesGlobal ) ) then
          raise exception.create( 'Zones should not be used, but naadsm_make_zone_focus was called.' )
        ;
      {$ENDIF}

      Application.ProcessMessages();

      _smSim.zoneList.setFocusCreatedOnDay( _simDay );

      h := _herds[herdIndex];

      h.prodType.addZoneFocusEvent( _simDay );

      if( assigned( _naadsMap ) ) then
        _naadsMap.herdEvent( h, EVTZoneFocus, _simDay )
      ;

      if( _smSim.outputOptions.saveDailyEvents ) then
        begin
          if( nil <> _smSim.zoneList.findByLevel( h.zoneLevel ) ) then
            zoneID := _smSim.zoneList.findByLevel( h.zoneLevel ).id
          else
            zoneID := -1
          ;

          evt := TSMEvent.create( _eventCounter, _simIteration, _simDay, h.id, zoneID, EVTZoneFocus );
          _eventList.append( evt );
          inc( _eventCounter );

          // frmOutputEvents is not currently updated dynamically.  Some day it might be...
          //if( assigned( frmOutputEvents ) ) then frmOutputEvents.appendEvent( evt );
        end
      ;
    end
  ;


  procedure naadsm_record_zone_change( r: THRDZone );
    var
      h: THerd;
      zoneID: integer;
      evt: TSMEvent;
    begin
      {$IFDEF DEBUG}
        dbcout( 'NAADSM: ++ Start naadsm_record_zone_change', DBSHOWMSG );
      {$ENDIF}

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        // Do some error checking
        //-----------------------
        if( not( _smSim.includeZonesGlobal ) ) then
          raise exception.create( 'Zones should not be used, but naadsm_record_zone_change was called.' )
        ;

        // Do the real work
        //-----------------
        Application.ProcessMessages();

        h := _herds[r.herdIndex];

        // Add the herd to the list of those "enzoned".
        // At the end of the day, outputs will be recorded.
        h.zoneLevel := r.zoneLevel;

        if( not( _herdsInZones.contains( r.herdIndex ) ) ) then
          _herdsInZones.insert( r.herdIndex, h )
        ;

        if( ( _smSim.outputOptions.saveDailyEvents ) ) then
          begin
            if( nil <> _smSim.zoneList.findByLevel( h.zoneLevel ) ) then
              zoneID := _smSim.zoneList.findByLevel( h.zoneLevel ).id
            else
              raise exception.Create( 'Zone could not be found in naadsm_record_zone_change()' )
            ;

            evt := TSMEvent.create( _eventCounter, _simIteration, _simDay, h.id, zoneID, EVTZoneChanged );
            _eventList.append( evt );
            inc( _eventCounter );

            // frmOutputEvents is not currently updated dynamically.  Some day it might be...
            //if( assigned( frmOutputEvents ) ) then frmOutputEvents.appendEvent( evt );
          end
        ;

      {$IFDEF DEBUG}
      except
        dbcout( 'NAADSM: Exception caught in naadsm_record_zone_change', true );
      end;
      {$ENDIF}

      {$IFDEF DEBUG}
        dbcout( 'NAADSM: -- End naadsm_record_zone_change', DBSHOWMSG );
      {$ENDIF}
    end
  ;


  procedure naadsm_record_zone_area( zoneLevel: integer; area: double );
    var
      z: TZone;
    begin
      // Do some error checking
      //-----------------------
      if( not( _smSim.includeZonesGlobal ) ) then
        raise exception.create( 'Zones should not be used, but naadsm_record_zone_area was called.' )
      ;

      // Do the real work
      //-----------------
      z := _smSim.zoneList.findByLevel( zoneLevel );

      if( nil <> z ) then
        z.setArea( area, _simDay )
      else
        raise exception.Create( 'Unrecognized zone (level ' + intToStr( zoneLevel ) + ') in naadsm_record_zone_area' )
      ;
    end
  ;


  procedure naadsm_record_zone_perimeter( zoneLevel: integer; perim: double );
    var
      z: TZone;
    begin
      // Do some error checking
      //-----------------------
      if( not( _smSim.includeZonesGlobal ) ) then
        raise exception.create( 'Zones should not be used, but naadsm_record_zone_perimeter was called.' )
      ;

      // Do the real work
      //-----------------
      z := _smSim.zoneList.findByLevel( zoneLevel );

      if( nil <> z ) then
        z.setPerimeter( perim, _simDay )
      else
        raise exception.Create( 'Unrecognized zone (level ' + intToStr( zoneLevel ) + ') in naadsm_record_zone_perimeter' )
      ;
    end
  ;


  // This function is never called by the core model.
  // It might be useful again some day for testing purposes.
  procedure naadsm_report_search_hits(  val1, val2, val3, val4, val5: integer );
    var
      str: string;
    begin
      {$IFDEF DEBUG}
      try
      {$ENDIF}
        if( _smSim.includeZonesGlobal and _smSim.outputOptions.writeDailyStatesFile ) then
          begin
            try
              str :=
                intToStr( val1 )
                + ', ' + intToStr( val2 )
                + ', ' + intToStr( val3 )
                + ', ' + intToStr( val4 )
                + ', ' + intToStr( val5 )
              ;
              //writeln( _searchHitsFile, str );
            except
              // fail silently
            end;
          end
        ;
      {$IFDEF DEBUG}
      except
        dbcout( 'NAADSM: Exception caught in naadsm_report_search_hits', true );
      end;
      {$ENDIF}
    end
  ;


initialization
  simFileName := '';
  herdFileName := '';
  logFileName := '';

  _smSim := nil;
  _herds := nil;
  _smdb := nil;

  _rngSeed := 0;

  _eventCounter := 0;
  _eventList := nil;

  _exposureCounter := 0;
  _exposureList := nil;

  _naadsMap := nil;

  // This will not used in NAADSM 4!
  _naadsm3Detections := TQIntegerObjectMap.create();

  dllLoadErrors := '';
  naadsmLibLoaded := loadDynamicDll();

  {$IFNDEF CONSOLEAPP}
    (*
    frmDllWarnings := TDialogLongMessage.Create(
      nil,
      tr( 'Messages generated during simulation run' ),
      tr( 'The following messages were generated by the running simulation.' )
          + ' ' + tr( 'These may simply indicate warnings to the user.' )
          + ' ' + tr( 'It is also possible that they indicate unanticipated error conditions, which might affect the validity of simulation output.' )
          + ' ' + tr( 'If you believe that these messages represent errors, please copy the information below and send it to the NAADSM Development Team.' )
          + ' ' + tr( 'Thank you for your assistance!' )
    );
    //frmDllWarnings.BorderIcons := [ biSystemMenu, biMaximize ];
    frmDllWarnings.hide();
    *)
  {$ENDIF}


finalization
  // This will not used in NAADSM 4!
  _naadsm3Detections.free();

  {$IFNDEF CONSOLEAPP}
    //frmDllWarnings.release();
  {$ENDIF}

end.
