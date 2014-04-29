unit ModelImplementation;

(*
ModelImplementation.pas
-----------------------
Begin: 2004/08/20
Last revision: $Date: 2008/10/21 23:34:43 $ $Author: areeves $
Version: $Revision: 1.73 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2004 - 2008 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)


{*
 Items in this unit are global, rather than encapsulated in a class,
 because I'm not sure that Delphi function pointers "of object" can be
 passed to a C DLL.  This warrants further investigation at some point,
 however, because I'd feel better without all of these very important
 functions, variables, and references sticking out where just anyone
 can get to them.
}


{$INCLUDE Defs.inc}

interface

	uses
  	Windows, // Defines THandle

    FunctionPointers,
    ZipFunctions,

    Herd,
    SMDatabase,
    SMSimulationInput,
    EventsAndExposures,
    ZonePerimeter
  ;

  // Function pointer types
  //-----------------------
  // (All other function pointers used in this application are defined
  // for more general use in unit FunctionPointers.pas)
  type TCFnVoid_1_THRDUpdate = procedure( r: THRDUpdate ); cdecl;
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
  function launchSim( simFileName, herdFileName: string; var errMsg: string ): integer;

  function writeLogFile( msg: string ): string;


  // Function declarations/pointers
  //-------------------------------
  procedure rng_read_seed( val: integer ); cdecl; // type TCFnVoid_1_Int

  // For the display of debugging information in the GUI
  procedure guilib_printf( msg: pchar ); cdecl; // type TCFnVoid_1_CharP
  procedure guilib_debug( msg: pchar ); cdecl;

  // For key simulation- and iteration-level events
  procedure guilib_sim_start(); cdecl; // type TCFnVoid_0
	procedure guilib_iteration_start( it: integer ); cdecl; // type TCFnVoid_1_Int
  procedure guilib_day_start( day: integer ); cdecl; // type TCFnVoid_1_Int
  procedure guilib_day_complete( day: integer ); cdecl; // type TCFnVoid_1_Int
  procedure guilib_disease_end( val: integer ); cdecl; // type TCFnVoid_1_Int
  procedure guilib_outbreak_end( val: integer ); cdecl; // type TCFnVoid_1_Int
  procedure guilib_iteration_complete( it: integer ); cdecl; // type TCFnVoid_1_Int
  procedure guilib_sim_complete( val: integer ); cdecl; // type TCFnVoid_1_Int

  // Used to determine when iterations should end
  function guilib_stop_on_disease_end(): integer; cdecl; // type TFnInt_0
  procedure guilib_reset_detection_end(); cdecl; // type TCFnVoid_0
  function guilib_stop_on_detection(): integer; cdecl; // type TCFnInt_0

  // Used to update herd status and related events as an  iteration runs
  procedure guilib_change_herd_state( r: THRDUpdate );cdecl; // type TCFnVoid_1_THRDUpdate
  procedure guilib_infect_herd( r: THRDUpdate ); cdecl; // type TCFnVoid_1_THRDUpdate
  procedure guilib_expose_herd( r: THRDUpdate ); cdecl; // type TCFnVoid_1_THRDUpdate
  procedure guilib_detect_herd( r: THRDUpdate ); cdecl; // type TCFnVoid_1_THRDUpdate
  procedure guilib_attempt_trace_herd( r: THRDUpdate ); cdecl; // type TCFnVoid_1_THRDUpdate
  procedure guilib_destroy_herd( r: THRDUpdate );  cdecl; // type TCFnVoid_1_THRDUpdate
  procedure guilib_vaccinate_herd( r: THRDUpdate );  cdecl; // type TCFnVoid_1_THRDUpdate
  procedure guilib_record_exposure( e: THRDExpose ); cdecl; // type TCFnVoid_1_THRDExpose
  procedure guilib_make_zone_focus( herdIndex: integer ); cdecl; // type TCFnVoid_1_Int
  procedure guilib_record_zone_change( r: THRDZone ); cdecl; // type TCFnVoid_1_THRDZone
  procedure guilib_record_zone_area( zoneLevel: integer; area: double ); cdecl; // type TCFnVoic_2_Int_Double

  // Used by the GUI to access zone information during a running simulation
  // This procedure is called by the dll to set zone perimeters each day of the sim.
  // This is actually a pointer into a complex C structure.  Use functions in ZonePerimeter.pas
  // to get more details about the data pointed to by this PerimeterList....
  procedure guilib_set_zone_perimeters( p: THRD_PerimeterList ); cdecl;  // type TCFnVoid_1_THRDPerimeterList

  // Used to write daily herd state output, when desired
  procedure guilib_show_all_states( statesMsg: pchar ); cdecl; // type TCFnVoid_1_CharP

  // Used to write daily herd prevalence output, when desired
  procedure guilib_show_all_prevalences(  prevMsg: pchar ); cdecl; // type TCFnVoid_1_CharP

  // Used to determine whether the user wants to interrupt a running simulation
  function guilib_simulation_stop(): integer; cdecl; // type TCFnInt_0

  procedure guilib_report_search_hits( val1, val2, val3, val4, val5: integer ); cdecl; // type TCFnVoid_5_Int_Int_Int_Int_Int

  // Loading functions from the DLL
  //-------------------------------
	function loadDynamicDll(): boolean;
  function ssSimLoadErrors(): string;

  var
    dllLoadErrors: string;

    //Function pointers, to be loaded from the DLL.
    //----------------------------------------------
    minimum_gui_version: function(): pchar; cdecl;

    run_sim_main: procedure(
      herdFile: pchar;
      paramFile: pchar;
      outputFile: pchar;
      modelDir: pchar;
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

    set_reset_detection_end: procedure( fn: TCFnVoid_0 ); cdecl;
    set_stop_on_detection: procedure( fn: TCFnInt_0 ); cdecl;
    set_stop_on_disease_end: procedure( fn: TCFnInt_0 ); cdecl;

    set_change_herd_state: procedure( fn: TCFnVoid_1_THRDUpdate ); cdecl;
    set_infect_herd: procedure( fn: TCFnVoid_1_THRDUpdate ); cdecl;
    set_expose_herd: procedure( fn: TCFnVoid_1_THRDUpdate ); cdecl;
    set_detect_herd: procedure( fn: TCFnVoid_1_THRDUpdate ); cdecl;
    set_attempt_trace_herd: procedure( fn: TCFnVoid_1_THRDUpdate ); cdecl;
    set_destroy_herd: procedure( fn: TCFnVoid_1_THRDUpdate ); cdecl;
    set_vaccinate_herd: procedure( fn: TCFnVoid_1_THRDUpdate ); cdecl;
    set_record_exposure: procedure( fn: TCFnVoid_1_THRDExpose ); cdecl;
    set_make_zone_focus: procedure( fn: TCFnVoid_1_Int ); cdecl;
    set_record_zone_change: procedure( fn: TCFnVoid_1_THRDZone ); cdecl;
    set_record_zone_area: procedure( fn: TCFnVoid_2_Int_Double ); cdecl;

    set_set_zone_perimeters: procedure( fn: TCFnVoid_1_THRDPerimeterList ); cdecl;
    get_zone_list_length: function( zones: Pointer ): integer; cdecl;
    get_zone_from_list: function( zones: Pointer; i: integer): ZON_zone_t_ptr; cdecl;

    set_show_all_states: procedure( fn: TCFnVoid_1_CharP ); cdecl;
    set_show_all_prevalences: procedure( fn: TCFnVoid_1_CharP ); cdecl;

    set_simulation_stop: procedure( fn: TCFnInt_0 ); cdecl;

    set_report_search_hits: procedure( fn: TCFnVoid_5_Int_Int_Int_Int_Int ); cdecl;

    // Other useful functions
    calculate_distance: function( lat1, lon1, lat2, lon2: double ): double; cdecl;

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

    _stopReason: TStopReason;
    _detectionStop: integer;

    _eventCounter: integer;
    _eventList: TSMEventList;

    _exposureCounter: integer;
    _exposureList: TSMExposureList;

    _naadsMap: TNAADSMap;
   *)

    // Variables used for DLL management
    //-----------------------------------
    sssimLoaded: boolean;

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

    DBMODELIMPLEMENTATION = false; // Set to true to enable debugging messages for this unit.
    DBFN = false; // Another debugging constant.
    DBLIBLOAD = false; // Ditto.
    DBDLL = false; // set to true to enable debugging messages from the DLL.
    
implementation

  uses
    Forms, // Defines the Application object
    SysUtils,
    StrUtils,

    MyStrUtils,
    GuiStrUtils,
    DebugWindow,
    SqlClasses,
    WindowsUtils,
    QIntegerMaps,
    MyDialogs,
    USStrUtils,

    StringConsts,
    StatusEnums,

    Zone,
    CustomOutputDefinitions,
    
    NAADSMap

    {$IFNDEF CONSOLEAPP}
    ,
    FormMain,
    FormMap
    {$ENDIF}
  ;

  var
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

    _stopReason: TStopReason;
    _detectionStop: integer;

    _eventCounter: integer;
    _eventList: TSMEventList;

    _exposureCounter: integer;
    _exposureList: TSMExposureList;

    _dailyStatesFile: textfile;
    _dailyPrevalenceFile: textfile;
    _naadsMap: TNAADSMap;

    _herdsInZones: TQIntegerObjectMap;

  	_dllHandle: THandle; //Handle used to open the DLL.  Defined in unit Windows.


  function ssSimLoadErrors(): string;
    begin
      result := dllLoadErrors;
    end
  ;


  function loadDynamicDll(): boolean;
    var
      minVersion: string;
  	begin
      dllLoadErrors := '';

      try
        {$IF Defined( CHEYENNE ) }
          _dllHandle := loadLibrary( 'cheyenne.dll' );
        {$ELSEIF Defined( LARAMIE ) }
          _dllHandle := loadLibrary( 'laramie.dll' );
        {$ELSE}
          _dllHandle := loadLibrary( 'sssim.dll' );
        {$IFEND}

        dbcout( 'loadLibrary successful', DBLIBLOAD );
      except
        dbcout( 'loadLibrary failed', DBLIBLOAD );
        result := false;
        exit;
      end;

      result := true;

      if( _dllHandle >= 32 ) then // library was successfully loaded.  Assign function pointers now.
        begin
          dbcout( 'Library was successfully loaded', DBLIBLOAD );

          dbcout( 'Attempting to set function pointers', DBLIBLOAD );

          // Check the version of the DLL
          //-----------------------------
          minimum_gui_version := GetProcAddress( _dllHandle, 'minimum_gui_version' );
          if( nil = @minimum_gui_version ) then
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION minimum_gui_version';
              result := false;
            end
          else
            begin
              minVersion := minimum_gui_version();
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
          run_sim_main := GetProcAddress( _dllHandle, 'run_sim_main' );
          if( nil = @run_sim_main ) then
          	begin
          		dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION run_sim_main';
              result := false;
            end
          ;

          // For retrieiving the RNG seed
          //-----------------------------
          set_rng_read_seed := GetProcAddress( _dllHandle, 'set_rng_read_seed' );
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
          set_printf := GetProcAddress( _dllHandle, 'set_printf' );
          if( nil <> @set_printf ) then
          	begin
              if( DBDLL ) then
          		  set_printf( @guilib_printf )
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

          set_debug := GetProcAddress( _dllHandle, 'set_debug' );
          if( nil <> @set_debug ) then
          	begin
              if( DBDLL ) then
          		  set_debug( @guilib_debug )
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
          set_sim_start := GetProcAddress( _dllHandle, 'set_sim_start' );
          if( nil <> @set_sim_start ) then
            set_sim_start( @guilib_sim_start )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_sim_start';
              result := false;
            end
          ;
          
          set_iteration_start := GetProcAddress( _dllHandle, 'set_iteration_start' );
          if( nil <> @set_iteration_start ) then
            set_iteration_start( @guilib_iteration_start )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_iteration_start';
              result := false;
            end
          ;
          
          set_day_start := GetProcAddress( _dllHandle, 'set_day_start' );
          if( nil <> @set_day_start ) then
            set_day_start( @guilib_day_start )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_day_start';
              result := false;
            end
          ; 
          
          set_day_complete := GetProcAddress( _dllHandle, 'set_day_complete' );
          if( nil <> @set_day_complete ) then
            set_day_complete( @guilib_day_complete )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_day_complete';
              result := false;
            end
          ;


          set_disease_end := GetProcAddress( _dllHandle, 'set_disease_end' );
          if( nil <> @set_disease_end ) then
            set_disease_end( @guilib_disease_end )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_disease_end';
              result := false;
            end
          ;

          set_outbreak_end := GetProcAddress( _dllHandle, 'set_outbreak_end' );
          if( nil <> @set_outbreak_end ) then
            set_outbreak_end( @guilib_outbreak_end )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_outbreak_end';
              result := false;
            end
          ;

          set_iteration_complete := getProcAddress( _dllHandle, 'set_iteration_complete' );
          if( nil <> @set_iteration_complete ) then
            set_iteration_complete( @guilib_iteration_complete )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_iteration_complete';
              result := false;
            end
          ;
          
          set_sim_complete := GetProcAddress( _dllHandle, 'set_sim_complete' );
          if( nil <> @set_sim_complete ) then
            set_sim_complete( @guilib_sim_complete )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_sim_complete';
              result := false;
            end
          ;
  
  
          // Used to determine when iterations should end  
          //---------------------------------------------
          set_stop_on_disease_end := GetProcAddress( _dllHandle, 'set_stop_on_disease_end' );
          if( nil <> @set_stop_on_disease_end ) then
            set_stop_on_disease_end( @guilib_stop_on_disease_end )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_stop_on_disease_end';
              result := false;
            end
          ;
          
          set_reset_detection_end := GetProcAddress( _dllHandle, 'set_reset_detection_end' );
          if( nil <> @set_reset_detection_end ) then
            set_reset_detection_end( @guilib_reset_detection_end )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_reset_detection_end';
              result := false;
            end
          ;

          set_stop_on_detection := GetProcAddress( _dllHandle, 'set_stop_on_detection' );
          if( nil <> @set_stop_on_detection ) then
            set_stop_on_detection( @guilib_stop_on_detection )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_stop_on_detection';
              result := false;
            end
          ;          
          
          
          // Used to update herd status and related events as an iteration runs
          //--------------------------------------------------------------------
          set_change_herd_state := GetProcAddress( _dllHandle, 'set_change_herd_state' );
          if( nil <> @set_change_herd_state ) then
            set_change_herd_state( @guilib_change_herd_state )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_change_herd_state';
              result := false;
            end
          ;

          set_infect_herd := GetProcAddress( _dllHandle, 'set_infect_herd' );
          if( nil <> @set_infect_herd ) then
            set_infect_herd( @guilib_infect_herd )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_infect_herd';
              result := false;
            end
          ;

          set_expose_herd := GetProcAddress( _dllHandle, 'set_expose_herd' );
          if( nil <> @set_expose_herd ) then
            set_expose_herd( @guilib_expose_herd )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_expose_herd';
              result := false;
            end
          ;

          set_detect_herd := GetProcAddress( _dllHandle, 'set_detect_herd' );
          if( nil <> @set_detect_herd ) then
            set_detect_herd( @guilib_detect_herd )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_detect_herd';
              result := false;
            end
          ;

          set_attempt_trace_herd := GetProcAddress( _dllHandle, 'set_attempt_trace_herd' );
          if( nil <> @set_attempt_trace_herd ) then
            set_attempt_trace_herd( @guilib_attempt_trace_herd )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_trace_herd';
              result := false;
            end
          ;

          set_destroy_herd := GetProcAddress( _dllHandle, 'set_destroy_herd' );
          if( nil <> @set_destroy_herd ) then
            set_destroy_herd( @guilib_destroy_herd )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_destroy_herd';
              result := false;
            end
          ;

          set_vaccinate_herd := GetProcAddress( _dllHandle, 'set_vaccinate_herd' );
          if( nil <> @set_vaccinate_herd ) then
            set_vaccinate_herd( @guilib_vaccinate_herd )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_vaccinate_herd';
              result := false;
            end
          ; 
          
          set_record_exposure := GetProcAddress( _dllHandle, 'set_record_exposure' );
          if( nil <> @set_record_exposure ) then
            set_record_exposure( @guilib_record_exposure )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_record_exposure';
              result := false;
            end
          ;

          set_make_zone_focus := GetProcAddress( _dllHandle, 'set_make_zone_focus' );
          if( nil <> @set_make_zone_focus ) then
            set_make_zone_focus( @guilib_make_zone_focus )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_make_zone_focus';
              result := false;
            end
          ;
          
          set_record_zone_change := GetProcAddress( _dllHandle, 'set_record_zone_change' );
          if( nil <> @set_record_zone_change ) then
            set_record_zone_change( @guilib_record_zone_change )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_record_zone_change';
              result := false;
            end
          ;

          set_record_zone_area := GetProcAddress( _dllHandle, 'set_record_zone_area' );
          if( nil <> @set_record_zone_area ) then
            set_record_zone_area( @guilib_record_zone_area )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_record_zone_area';
              result := false;
            end
          ;

         
          // Used by the GUI to access zone information during a running simulation
          //-----------------------------------------------------------------------
          set_set_zone_perimeters := GetProcAddress( _dllHandle, 'set_set_zone_perimeters' );
          if( nil = @set_set_zone_perimeters ) then
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_set_zone_perimeters';
              result := false;
            end
          else
            begin
              set_set_zone_perimeters( @guilib_set_zone_perimeters );
            end;
          ;

          get_zone_list_length := GetProcAddress( _dllHandle, 'get_zone_list_length' );
          if( nil = @get_zone_list_length ) then
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION get_zone_list_length';
              result := false;
            end
          ;

          get_zone_from_list := GetProcAddress( _dllHandle, 'get_zone_from_list' );
          if( nil = @get_zone_from_list ) then
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION get_zone_from_list';
              result := false;
            end
          ;

          // Used to determine write daily herd state output, when desired
          //---------------------------------------------------------------
          set_show_all_states := GetProcAddress( _dllHandle, 'set_show_all_states' );
          if( nil <> @set_show_all_states ) then
            set_show_all_states( @guilib_show_all_states )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_show_all_states';
              result := false;
            end
          ;

          // Used to write daily herd prevalence output, when desired
          //---------------------------------------------------------
          set_show_all_prevalences := GetProcAddress( _dllHandle, 'set_show_all_prevalences' );
          if( nil <> @set_show_all_prevalences ) then
            set_show_all_prevalences( @guilib_show_all_prevalences )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_show_all_prevalences';
              result := false;
            end
          ;
          
          // Used to determine whether the user wants to interrupt a running simulation
          //---------------------------------------------------------------------------
          set_simulation_stop := GetProcAddress( _dllHandle, 'set_simulation_stop' );
          if( nil <> @set_simulation_stop ) then
            set_simulation_stop( @guilib_simulation_stop )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_simulation_stop';
              result := false;
            end
          ;

          set_report_search_hits := GetProcAddress( _dllHandle, 'set_report_search_hits' );
          if( nil <> @set_report_search_hits ) then
            set_report_search_hits( @guilib_report_search_hits )
          else
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION set_report_search_hits';
              result := false;
            end
          ;

          // Other functions
          //----------------
          // This function is not really used for the simulation,
          // but is potentially useful for other purposes.
          calculate_distance := GetProcAddress( _dllHandle, 'calculate_distance' );
          if( nil = @calculate_distance ) then
            begin
              dllLoadErrors := dllLoadErrors + endl + 'MISSING FUNCTION calculate_distance';
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


  function launchSim( simFileName, herdFileName: string; var errMsg: string ): integer;
    var
      logFileName: string;
      seed: integer;
    begin
      dbcout( '++ Start launchSim', DBFN );

      if( _smSim.useFixedRandomSeed ) then
        seed := _smSim.randomSeed
      else
        seed := -1
      ;

      // Launch the DLL with appropriate parameters
      //-------------------------------------------
      try
        // If DEBUG, exceptions will show up immediately.
        // Otherwise, an error log file will be created.
        {$IFNDEF DEBUG}
          try
        {$ENDIF}
            _eventList := TSMEventList.create();
            _exposureList := TSMExposureList.create();

            run_sim_main(
              pchar( herdFileName ),
              pchar( simFileName ),
              nil, // Output file name (typically nil for Windows)
              nil, // Directory containing submodels (always nil for the Windows version: there are no submodels)
              -1, // fixed_rng_value (if -1, values will be generated randomly)
              0,   // Verbosity (basically ignored)
              seed   // Specified seed for RNG (if -1, seed will be automatically generated)
            );
            result := 0;
        {$IFNDEF DEBUG}
          except
            on e: exception do
              begin
                dbcout( 'EXCEPTION: ' + e.message, true );

                logFileName := writeLogFile( e.Message );
                {$IFNDEF CONSOLE}
                  errMsg := buildErrorZip( simFileName, herdFileName, logFileName );
                {$ENDIF}
                result := ERRRUNSIMEXCEPTION;
              end
            ;
          end;
        {$ENDIF}
      finally
        // clean up
        //---------
        deleteFile( simFileName );
        deleteFile( herdFileName );
        
        if( 0 < length( logFileName ) ) then
          deleteFile( logFileName )
        ;

        _eventList.free();
        _exposureList.free();
      end;

      dbcout( '-- End launchSim', DBFN );
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
      dbcout( '++ Start startSim', DBFN );

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        _smSim := smSim;
        _herds := herds;
        _smdb := db;
        _stopReason := _smSim.simStopReason;

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
          ( _smSim.writeXMLFile( sfn, false, @errMsg ) )
        and
          ( _herds.writeXMLFile( hfn, @errMsg ) ) ) )
        then
          begin
            dbcout( errMsg, DBMODELIMPLEMENTATION );
            deleteFile( sfn );
            deleteFile( hfn );
            result := ERRCANNOTWRITEFILES;
            exit;
          end
        ;

        dbcout( 'Launching simulation', DBMODELIMPLEMENTATION );
        result := launchSim( sfn, hfn, errMsg );
      {$IFDEF DEBUG}
      except
        dbcout( 'Exception caught in startSim', true );
        result := ERRRUNSIMEXCEPTION;
      end;
      {$ENDIF}

      dbcout( '-- End startSim', DBFN );
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
      dbcout( '++ Start writeLogFile', DBFN );

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

      dbcout( '-- End writeLogFile', DBFN );
    end
  ;




  procedure guilib_printf( msg: pchar );
  	begin
  		dbcout( 'LIB: ' + msg, DBDLL );
    end
  ;


  procedure guilib_debug( msg: pchar );
    begin
      dbcout( 'LIB: ' + msg, DBDLL );
    end
  ;


  function guilib_stop_on_disease_end(): integer;
    begin
      if( ssStartAndStopAtDiseaseEnd = _smSim.simStopReason ) then
        result := -1 // "true"
      else
        result := 0 // "false"
      ;
    end
  ;


  procedure guilib_reset_detection_end();
    begin
      _detectionStop := 0;
    end
  ;


  function guilib_stop_on_detection(): integer;
    begin
      result := _detectionStop;
    end
  ;


//-----------------------------------------------------------------------------
// Obtaining zone information
//-----------------------------------------------------------------------------
  procedure guilib_set_zone_perimeters( p: THRD_PerimeterList ); cdecl;
    begin
      // Unlike other zone functions, this one is called even if there are no zones.
      // It's harmless, though.
      
      {$IFDEF DEBUG}
        //zonePerimeterDebug( p );
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
      dbcout2( 'RNG SEED SET TO ' + intToStr( val ) );
      _rngSeed := val;
      _smdb.setRngSeed( _rngSeed );
    end
  ;


  function guilib_simulation_stop(): integer;
  	begin
      dbcout( '++ Start guilib_simulation_stop', DBFN );

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
        dbcout( 'Exception caught in guilib_simulation_stop', true );
        result := 0;
      end;
      {$ENDIF}

      dbcout( '-- End guilib_simulation_stop', DBFN );
    end
  ;


  procedure guilib_sim_start();
    var
      prevFileName: string;
      fn: string;
  	begin
      dbcout( '++ Start guilib_sim_start', DBFN );

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        dbcout( 'Simulation is starting', DBMODELIMPLEMENTATION );

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

        _smdb.setStartTime();

        _simIteration := -1;
        _simDay := -1;
        _diseaseEndDay := -1;
        _outbreakEnd := -1;

        _eventCounter := 1;

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
            set_show_all_states( @guilib_show_all_states );
            set_show_all_prevalences( @guilib_show_all_prevalences );

            try
              fn := _smSim.outputOptions.dailyStatesFileName;
              assignFile( _dailyStatesFile, fn );
              rewrite( _dailyStatesFile );

              if( _smSim.useWithinHerdPrevalence ) then
                begin
                  if( '.txt' = lowerCase( rightStr( fn, 4 ) ) ) then
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
        dbcout( 'Exception caught in guilib_sim_start', true );
      end;
      {$ENDIF}

      dbcout( '-- End guilib_sim_start', DBFN );
    end
  ;


  procedure guilib_iteration_start( it: integer );
  	begin
      dbcout( '++++++++++ guilib_iteration_start ' + intToStr( it + 1 ), DBFN );

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        dbcout( 'Iteration ' + intToStr( it + 1 ) + ' is beginning', DBMODELIMPLEMENTATION );

        Application.ProcessMessages();
        _smdb.prepareForIteration( it + 1 );
        _smSim.prepareForIteration( it + 1 );
        _herds.prepareForIteration( it + 1 );

        _simIteration := it + 1;
        _simDay := -1;
        _diseaseEndDay := -1;
        _outbreakEnd := -1;

        _eventCounter := 1;

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
        dbcout( 'Exception caught in guilib_sim_start', true );
      end;
      {$ENDIF}

      dbcout( '-- End guilib_iteration_start', DBFN );
    end
  ;


  procedure guilib_day_start( day: integer );
  	begin
      dbcout( '+++++ guilib_day_start', DBFN );

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        dbcout( 'Day ' + intToStr( day ) + ' starting.', DBMODELIMPLEMENTATION );

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
        dbcout( 'Exception caught in guilib_day_start', true );
      end;
      {$ENDIF}

      dbcout( '-- End guilib_day_start', DBFN );
    end
  ;


  procedure guilib_change_herd_state( r: THRDUpdate );
    var
      h: THerd;
      evt: TSMEvent;
      zoneID: integer;
  	begin
      dbcout( '++ Start guilib_change_herd_state', DBFN );

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        dbcout(
          'Herd with index ' + intToStr( r.index )
            + ' now has status ' + transitionStateString( TTransitionState( r.status ) )
          , DBMODELIMPLEMENTATION
        );

        h := _herds[r.index];
        h.setSimulatedStatus( TTransitionState( r.status ), true );

        if( _smSim.outputOptions.saveDailyEvents ) then
          begin
            if( nil <> _smSim.zoneList.findByLevel( h.zoneLevel ) ) then
              zoneID := _smSim.zoneList.findByLevel( h.zoneLevel ).id
            else
              zoneID := -1
            ;

            evt := TSMEvent.create(
              _eventCounter,
              _simIteration,
              _simDay,
              h.id,
              zoneID,
              EVT_TRANSITION_STATE_CHANGE,
              h.simulatedStatus
            );

            _eventList.append( evt );
            inc( _eventCounter );
            dbcout( 'Events in list: ' + intToStr( _eventList.Count ), DBMODELIMPLEMENTATION );

            // frmOutputEvents is not currently updated dynamically.  Some day it might be...
            //if( assigned( frmOutputEvents ) ) then frmOutputEvents.appendEvent( evt );
          end
        ;

        if( assigned( _naadsMap ) ) then
          _naadsMap.herdEvent( h, EVT_TRANSITION_STATE_CHANGE, _simDay )
        ;

        Application.ProcessMessages();

        {$IFNDEF CONSOLEAPP}
        if( assigned( frmMain ) ) then frmMain.changeHerdState( h );
        {$ENDIF}
      {$IFDEF DEBUG}
      except
        dbcout( 'Exception caught in guilib_change_herd_state', true );
      end;
      {$ENDIF}

      dbcout( '-- End guilib_change_herd_state', DBFN );
    end
  ;


  procedure guilib_expose_herd( r: THRDUpdate );
  	begin
      dbcout( '++ Start guilib_expose_herd', DBFN );

      if( not( _smSim.includeContactSpreadGlobal ) ) then
        begin
          raise exception.Create( 'Contact spread is being conducted when it should be disabled.' );
        end
      ;

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        dbcout( 'Herd with index ' + intToStr( r.index ) + ' was exposed by ' + r.msg, DBMODELIMPLEMENTATION );

        _herds[r.index].exposeByMechanism( r.msg );
        Application.ProcessMessages();

        // There isn't really anything to do on the main form.
        //if( assigned( frmMain ) ) then frmMain.exposeHerd( r );
      {$IFDEF DEBUG}
      except
        dbcout( 'Exception caught in guilib_expose_herd', true );
      end;
      {$ENDIF}

      dbcout( '-- End guilib_expose_herd', DBFN );
    end
  ;


  procedure guilib_attempt_trace_herd( r: THRDUpdate );
    var
      mechanism: string;
      h: THerd;
      evt: TSMEvent;
      zoneID: integer;
  	begin
      dbcout( '++ Start guilib_attempt_trace_herd', DBFN );

      if( not( _smSim.includeTracingGlobal ) ) then
        begin
          raise exception.Create( 'Tracing is being conducted when it should be disabled.' );
        end
      ;

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        dbcout( '+++ Trace attempted for herd with index ' + intToStr( r.index ) + ' for ' + r.msg, DBMODELIMPLEMENTATION );

        h := _herds[r.index];
        h.attemptTraceForReason( r.msg );

        if( -1 = r.success ) then // Trace was successful.
          h.traceForReason( r.msg )
        ;

        if( _smSim.outputOptions.saveDailyEvents ) then
          begin
            mechanism := fixup( r.msg );

            if( nil <> _smSim.zoneList.findByLevel( h.zoneLevel ) ) then
              zoneID := _smSim.zoneList.findByLevel( h.zoneLevel ).id
            else
              zoneID := -1
            ;

            if( 'direct contact' = mechanism ) then
              begin
                evt := TSMEvent.create(
                  _eventCounter,
                  _simIteration,
                  _simDay,
                  h.id,
                  zoneID,
                  EVT_TRACED_DIRECT,
                  h.simulatedStatus,
                  intToBool( r.success )
                );
                _eventList.append( evt );

                // frmOutputEvents is not currently updated dynamically.  Some day it might be...
                //if( assigned( frmOutputEvents ) ) then frmOutputEvents.appendEvent( evt );
              end
            else if( 'indirect contact' = mechanism ) then
              begin
                evt := TSMEvent.create(
                  _eventCounter,
                  _simIteration,
                  _simDay,
                  h.id,
                  zoneID,
                  EVT_TRACED_INDIRECT,
                  h.simulatedStatus,
                  intToBool( r.success )
                );
                _eventList.append( evt );

                // frmOutputEvents is not currently updated dynamically.  Some day it might be...
                //if( assigned( frmOutputEvents ) ) then frmOutputEvents.appendEvent( evt );
              end
            else
              raise exception.Create( 'Unrecognized trace reason in guilib_attempt_trace_herd' )
            ;

            inc( _eventCounter );

            dbcout( '-- End guilib_attempt_trace_herd', DBFN );
          end
        ;

        Application.ProcessMessages();

        // There isn't currently anything to do on the main form
        //if( assigned( frmMain ) ) then frmMain.attemptTraceHerd( r );
      {$IFDEF DEBUG}
      except
        dbcout( 'Exception caught in guilib_attempt_trace_herd', true );
      end;
      {$ENDIF}

      dbcout( '-- End guilib_attempt_trace_herd', DBFN );
    end
  ;


  procedure guilib_infect_herd( r: THRDUpdate );
    var
      h: THerd;
      evt: TSMEvent;
      zoneID: integer;
      mechanism: string;
  	begin
      dbcout( '++ Start guilib_infect_herd', DBFN );

      if( not( _smSim.includeContactSpreadGlobal ) and not( _smSim.includeAirborneSpreadGlobal ) ) then
        begin
          mechanism := fixup( r.msg );

          if( ( 'direct contact' = mechanism ) or ( 'indirect contact' = mechanism ) ) then
            raise exception.Create( 'Disease spread by contact is being conducted when it should be disabled.' )
          else if( 'airborne spread' = mechanism ) then
            raise exception.Create( 'Disease spread by airborne transmission is being conducted when it should be disabled.' )
          ;
        end
      ;

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        dbcout( 'Herd with index ' + intToStr( r.index ) + ' was infected by ' + r.msg, DBMODELIMPLEMENTATION );

        Application.ProcessMessages();
        h := _herds[r.index];
        h.infectByMechanism( r.msg, _simDay );

        if( assigned( _naadsMap ) ) then
          _naadsMap.herdEvent( h, EVT_INFECTED, _simDay )
        ;

        if( ( _smSim.outputOptions.saveDailyEvents ) ) then
          begin
            if( nil <> _smSim.zoneList.findByLevel( h.zoneLevel ) ) then
              zoneID := _smSim.zoneList.findByLevel( h.zoneLevel ).id
            else
              zoneID := -1
            ;

            evt := TSMEvent.create(
              _eventCounter,
              _simIteration,
              _simDay,
              h.id,
              zoneID,
              EVT_INFECTED
            );
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
        dbcout( 'Exception caught in guilib_infect_herd', true );
      end;
      {$ENDIF}

      dbcout( '-- End guilib_infect_herd', DBFN );
    end
  ;


  procedure guilib_detect_herd( r: THRDUpdate );
    var
      {$IFNDEF CONSOLEAPP}
      wasFirstEvent: boolean;
      {$ENDIF}
      h: THerd;
      evt: TSMEvent;
      zoneID: integer;
  	begin
      dbcout( '++ Start guilib_detect_herd', DBFN );

      if( not( _smSim.includeDetectionGlobal ) ) then
        begin
          raise exception.Create( 'Detection is being conducted when it should be disabled.' );
        end
      ;

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        dbcout( 'Herd with index ' + intToStr( r.index ) + ' was detected', DBMODELIMPLEMENTATION );

        Application.ProcessMessages();
        h := _herds[r.index];
        {$IFNDEF CONSOLEAPP}
          wasFirstEvent := h.detect( _simDay );
        {$ELSE}
          h.detect( _simDay );
        {$ENDIF}

        if( assigned( _naadsMap ) ) then
          _naadsMap.herdEvent( h, EVT_DETECTED, _simDay )
        ;

        if( _smSim.outputOptions.saveDailyEvents ) then
          begin
            if( nil <> _smSim.zoneList.findByLevel( h.zoneLevel ) ) then
              zoneID := _smSim.zoneList.findByLevel( h.zoneLevel ).id
            else
              zoneID := -1
            ;

            evt := TSMEvent.create(
              _eventCounter,
              _simIteration,
              _simDay,
              h.id,
              zoneID,
              EVT_DETECTED
            );
            _eventList.append( evt );
            inc( _eventCounter );

            // frmOutputEvents is not currently updated dynamically.  Some day it might be...
            //if( assigned( frmOutputEvents ) ) then frmOutputEvents.appendEvent( evt );
          end
        ;

        {$IFNDEF CONSOLEAPP}
        if( assigned( frmMain ) ) then frmMain.detectHerd( h, wasFirstEvent, _simDay );
        {$ENDIF}

        // If stopping at first detection, then end simulation here
        if( _stopReason in [ssStartAndStopAtFirstDetection] ) then
          _detectionStop := -1
        ;
      {$IFDEF DEBUG}
      except
        dbcout( 'Exception caught in guilib_detect_herd', true );
      end;
      {$ENDIF}

      dbcout( '-- End guilib_detect_herd', DBFN );
    end
  ;


  procedure guilib_destroy_herd( r: THRDUpdate );
    var
      {$IFNDEF CONSOLEAPP}
      wasFirstEvent: boolean;
      {$ENDIF}
      h: THerd;
      evt: TSMEvent;
      zoneID: integer;
  	begin
      dbcout( '++ Start guilib_destroy_herd', DBFN );

      if( not( _smSim.includeDestructionGlobal ) ) then
        begin
          raise exception.Create( 'Destruction is being conducted when it should be disabled.' );
        end
      ;

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        dbcout( intToStr( r.index ) + ' herd with index was destroyed for ' + r.msg, DBMODELIMPLEMENTATION );

        h := _herds[r.index];
        {$IFNDEF CONSOLEAPP}
          wasFirstEvent := h.destroyForReason( r.msg, _simDay );
        {$ELSE}
          h.destroyForReason( r.msg, _simDay );
        {$ENDIF}


        Application.ProcessMessages();

        if( assigned( _naadsMap ) ) then
          _naadsMap.herdEvent( h, EVT_DESTROYED, _simDay )
        ;

        if( _smSim.outputOptions.saveDailyEvents ) then
          begin
            if( nil <> _smSim.zoneList.findByLevel( h.zoneLevel ) ) then
              zoneID := _smSim.zoneList.findByLevel( h.zoneLevel ).id
            else
              zoneID := -1
            ;

            evt := TSMEvent.create(
              _eventCounter,
              _simIteration,
              _simDay,
              h.id,
              zoneID,
              EVT_DESTROYED
            );

            _eventList.append( evt );
            inc( _eventCounter );

            // frmOutputEvents is not currently updated dynamically.  Some day it might be...
            //if( assigned( frmOutputEvents ) ) then frmOutputEvents.appendEvent( evt );
          end
        ;

        // If this herd was previously "enzoned", it can be taken out of the list now.
        if( _herdsInZones.contains( r.index ) ) then
          _herdsInZones.Remove( r.index )
        ;

        {$IFNDEF CONSOLEAPP}
        if( assigned( frmMain ) ) then frmMain.destroyHerd( h, wasFirstEvent, _simDay );
        {$ENDIF}
      {$IFDEF DEBUG}
      except
        dbcout( 'Exception caught in guilib_destroy_herd', true );
      end;
      {$ENDIF}

      dbcout( '-- End guilib_destroy_herd', DBFN );
    end
  ;


  procedure guilib_vaccinate_herd( r: THRDUpdate );
    var
      {$IFNDEF CONSOLEAPP}
      wasFirstEvent: boolean;
      {$ENDIF}
      h: THerd;
      evt: TSMEvent;
      zoneID: integer;
  	begin
      dbcout( '++ Start guilib_vaccinate_herd', DBFN );

      if( not( _smSim.includeVaccinationGlobal ) ) then
        raise exception.Create( 'Vaccination is being conducted when it should be disabled' )
      ;

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        dbcout( intToStr( r.index ) + ' herd with index was vaccinated for ' + r.msg, DBMODELIMPLEMENTATION );

        h := _herds[r.index];
        {$IFNDEF CONSOLEAPP}
          wasFirstEvent := h.vaccinateForReason( r.msg, _simDay );
        {$ELSE}
          h.vaccinateForReason( r.msg, _simDay );
        {$ENDIF}


        if( assigned( _naadsMap ) ) then
          _naadsMap.herdEvent( h, EVT_VACCINATED, _simDay )
        ;

        if( _smSim.outputOptions.saveDailyEvents ) then
          begin
            if( nil <> _smSim.zoneList.findByLevel( h.zoneLevel ) ) then
              zoneID := _smSim.zoneList.findByLevel( h.zoneLevel ).id
            else
              zoneID := -1
            ;

            evt := TSMEvent.create(
              _eventCounter,
              _simIteration,
              _simDay,
              h.id,
              zoneID,
              EVT_VACCINATED
            );
            _eventList.append( evt );
            inc( _eventCounter );

            // frmOutputEvents is not currently updated dynamically.  Some day it might be...
            //if( assigned( frmOutputEvents ) ) then frmOutputEvents.appendEvent( evt );
          end
        ;

        Application.ProcessMessages();

        {$IFNDEF CONSOLEAPP}
        if( assigned( frmMain ) ) then frmMain.vaccinateHerd( h, wasFirstEvent, _simDay );
        {$ENDIF}
      {$IFDEF DEBUG}
      except
        dbcout( 'Exception caught in guilib_vaccinate_herd', true );
      end;
      {$ENDIF}

      dbcout( '-- End guilib_vaccinate_herd', DBFN );
    end
  ;


  procedure guilib_show_all_states( statesMsg: pchar );
    var
      stateStr: string;
    begin
      dbcout( '++ Start guilib_show_all_states', DBFN );

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
        dbcout( 'Exception caught in guilib_show_all_states', true );
      end;
      {$ENDIF}

      dbcout( '-- End guilib_show_all_states', DBFN );
    end
  ;


  procedure guilib_show_all_prevalences(  prevMsg: pchar );
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


  procedure guilib_disease_end( val: integer );
    begin
      dbcout( '++ Start guilib_disease_end', DBFN );

      {$IFDEF DEBUG}
      try
      {$ENDIF}

        // This check shouldn't be necessary, but we'll make it just in case.
        if( -1 = _diseaseEndDay ) then
          begin
            dbcout( 'Disease ended on day ' + intToStr( val ), DBMODELIMPLEMENTATION );
            _diseaseEndDay := val;
          end
        ;
        Application.ProcessMessages();
      {$IFDEF DEBUG}
      except
        dbcout( 'Exception caught in guilib_disease_end', true );
      end;
      {$ENDIF}

      dbcout( '-- End guilib_disease_end', DBFN );
    end
  ;


  procedure guilib_outbreak_end( val: integer );
  	begin
      dbcout( '++ Start guilib_outbreak_end', DBFN );

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        dbcout( 'Outbreak ended on day ' + intToStr( val ), DBMODELIMPLEMENTATION );

        _outbreakEnd := val;

        Application.ProcessMessages();

        {$IFNDEF CONSOLEAPP}
        if( assigned( frmMain ) ) then frmMain.outbreakEnd( val );
        {$ENDIF}
      {$IFDEF DEBUG}
      except
        dbcout( 'Exception caught in guilib_outbreak_end', true );
      end;
      {$ENDIF}

      dbcout( '-- End guilib_outbreak_end', DBFN );
    end
  ;


  procedure guilib_day_complete( day: integer );
    var
      i: integer;
      h: THerd;
      z: TZone;
  	begin
      dbcout( '----- guilib_day_complete', DBFN );

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        dbcout( 'Sim day ' + intToStr( day ) + ' is complete.', DBMODELIMPLEMENTATION );

        // Save daily events and exposures, if necessary
        if( 0 = day mod 5 ) then
          begin
            if( _smSim.outputOptions.saveDailyEvents ) then
              begin
                _eventList.populateDatabase( _smdb );
                _eventList.Clear();
              end
            ;

            if( _smSim.outputOptions.saveDailyExposures ) then
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
        _smSim.processDailyRecords( _smdb, day );

        Application.ProcessMessages();

      {$IFNDEF CONSOLEAPP}
        if( assigned( frmMain ) ) then frmMain.dayComplete( day );
      {$ELSE}
        cout( '  Day ' + intToStr( day ) + ' complete.' );
      {$ENDIF}

      {$IFDEF DEBUG}
      except
        dbcout( 'Exception caught in guilib_day_complete', true );
      end;
      {$ENDIF}

      dbcout( '-- End guilib_day_complete', DBFN );
    end
  ;



  procedure guilib_iteration_complete( it: integer );
    var
      stopReason: integer;
  	begin
      dbcout( '---------- guilib_iteration_complete ' + intToStr( it + 1 ), DBFN );

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        dbcout( 'Iteration ' + intToStr( it + 1 ) + ' is complete.', DBMODELIMPLEMENTATION );

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

        if( _smSim.outputOptions.saveDailyExposures ) then
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
            _smSim.processIterationRecords( _smdb, it + 1 );
            _herds.processIterationRecords( _smdb, it + 1 );

            _smdb.processIterationRecords(
              it + 1,
              _simDay,
              ( -1 <> _outbreakEnd ),
              _diseaseEndDay,
              ( -1 <> _diseaseEndDay ),
              _smSim.zoneList.focusCreated
            );

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
            dbcout( 'Exception caught in guilib_iteration_complete: ' + e.Message , true );
            raise e;
          end
        ;
      end;
      {$ENDIF}

      dbcout( '-- End guilib_iteration_complete', DBFN );
    end
  ;


  procedure guilib_sim_complete( val: integer );
  	begin
      dbcout( '++ Start guilib_sim_complete', DBFN );

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
          dbcout( 'Sim is successfully completed.', DBMODELIMPLEMENTATION )
        else
          dbcout( 'Sim did not complete successfully.', DBMODELIMPLEMENTATION )
        ;

        if( 0 <> val ) then _smdb.setEndTime();

        if( assigned( _naadsMap ) ) then
          begin
            _naadsMap.simEnd( (0 <> val ) );
            freeAndNil( _naadsMap );
          end
        ;

        _herdsInZones.clear();
        // DO NOT delete the items in _herdsInZones: it does not own the items.
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
        dbcout( 'Exception caught in guilib_sim_complete', true );
      end;
      {$ENDIF}

      dbcout( '-- End guilib_sim_complete', DBFN );
    end
  ;


  procedure guilib_record_exposure( e: THRDExpose );
    var
      exposingZoneID, exposedZoneID: integer;
    begin
      dbcout( '++ Start guilib_record_exposure', DBFN );

      {$IFDEF DEBUG}
      try
      {$ENDIF}
        dbcout(
          'Herd at index ' + intToStr( e.destIndex )
            + ' exposed by method ' + e.exposureMethod
            + ' by herd at index ' + intToStr( e.srcIndex )
            + ' with result ' + usBoolToText( bool( e.success ) )
          ,DBMODELIMPLEMENTATION
        );


        dbcout(
          'Source herd is ' + intToStr( e.srcStatus ) + ' (' +  transitionStateString( TTransitionState( e.srcStatus ) ) + ')'
            + ', recipient herd is ' + intToStr( e.destStatus ) + ' (' +  transitionStateString( TTransitionState( e.destStatus ) ) + ')'
          ,DBMODELIMPLEMENTATION
        );

        if( ( 0 > e.srcStatus ) or ( 6 < e.srcStatus ) ) then
          dbcout( 'SOURCE STATUS OUT OF RANGE: ' + intToStr( e.srcStatus ), DBMODELIMPLEMENTATION )
        ;

        if( ( 0 > e.destStatus ) or ( 6 < e.destStatus ) ) then
          dbcout( 'DEST STATUS OUT OF RANGE: ' + intToStr( e.destStatus ), DBMODELIMPLEMENTATION )
        ;


        if( _smSim.outputOptions.saveDailyExposures ) then
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
              TSMExposure.create(
                _exposureCounter,
                _simIteration,
                _simDay,
                _herds[ e.srcIndex ].id,
                TTransitionState( e.srcStatus ),
                exposingZoneID,
                _herds[ e.destIndex ].id,
                TTransitionState( e.destStatus ),
                exposedZoneID,
                e.exposureMethod,
                intToBool( e.success )
              )
            );

            inc( _exposureCounter );
          end
        ;
      {$IFDEF DEBUG}
      except
        dbcout( 'Exception caught in guilib_record_exposure', true );
      end;
      {$ENDIF}

      dbcout( '-- End guilib_record_exposure', DBFN );
    end
  ;


  // The GUI could determine which detections and traces lead to zone foci without
  // relying on the core library.  Using the library, though, acts as an extra sanity check.
  procedure guilib_make_zone_focus( herdIndex: integer );
    var
      h: THerd;
      evt: TSMEvent;
      zoneID: integer;
    begin
      if( not( _smSim.includeZonesGlobal ) ) then
        raise exception.create( 'Zones should not be used, but guilib_make_zone_focus was called.' )
      ;

      Application.ProcessMessages();

      _smSim.zoneList.setFocusCreatedOnDay( _simDay );

      h := _herds[herdIndex];

      h.prodType.addZoneFocus( _simDay );

      if( assigned( _naadsMap ) ) then
        _naadsMap.herdEvent( h, EVT_ZONE_FOCUS, _simDay )
      ;

      if( _smSim.outputOptions.saveDailyEvents ) then
        begin
          if( nil <> _smSim.zoneList.findByLevel( h.zoneLevel ) ) then
            zoneID := _smSim.zoneList.findByLevel( h.zoneLevel ).id
          else
            zoneID := -1
          ;

          evt := TSMEvent.create(
            _eventCounter,
            _simIteration,
            _simDay,
            h.id,
            zoneID,
            EVT_ZONE_FOCUS
          );
          _eventList.append( evt );
          inc( _eventCounter );

          // frmOutputEvents is not currently updated dynamically.  Some day it might be...
          //if( assigned( frmOutputEvents ) ) then frmOutputEvents.appendEvent( evt );
        end
      ;
    end
  ;


  procedure guilib_record_zone_change( r: THRDZone );
    var
      h: THerd;
      zoneID: integer;
      evt: TSMEvent;
    begin
      dbcout( '++ Start guilib_record_zone_change', DBFN );

      if( not( _smSim.includeZonesGlobal ) ) then
        raise exception.create( 'Zones should not be used, but guilib_record_zone_change was called.' )
      ;

      {$IFDEF DEBUG}
      try
      {$ENDIF}
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
              raise exception.Create( 'Zone could not be found in guilib_record_zone_change()' )
            ;

            evt := TSMEvent.create(
              _eventCounter,
              _simIteration,
              _simDay,
              h.id,
              zoneID,
              EVT_ZONE_CHANGED
            );
            _eventList.append( evt );
            inc( _eventCounter );

            // frmOutputEvents is not currently updated dynamically.  Some day it might be...
            //if( assigned( frmOutputEvents ) ) then frmOutputEvents.appendEvent( evt );
          end
        ;

      {$IFDEF DEBUG}
      except
        dbcout( 'Exception caught in guilib_record_zone_change', true );
      end;
      {$ENDIF}

      dbcout( '-- End guilib_record_zone_change', DBFN );
    end
  ;


  procedure guilib_record_zone_area( zoneLevel: integer; area: double );
    var
      z: TZone;
    begin
      if( not( _smSim.includeZonesGlobal ) ) then
        raise exception.create( 'Zones should not be used, but guilib_record_zone_area was called.' )
      ;

      z := _smSim.zoneList.findByLevel( zoneLevel );

      if( nil <> z ) then
        z.setArea( area, _simDay )
      else
        raise exception.Create( 'Unrecognized zone (level ' + intToStr( zoneLevel ) + ') in guilib_record_zone_area' )
      ;
    end
  ;


  // This function is never called by the core model.
  // It may be useful again some day for testing purposes.
  procedure guilib_report_search_hits(  val1, val2, val3, val4, val5: integer );
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
        dbcout( 'Exception caught in guilib_report_search_hits', true );
      end;
      {$ENDIF}
    end
  ;


initialization

  _smSim := nil;
  _herds := nil;
  _smdb := nil;

  _rngSeed := 0;
    
  _eventCounter := 0;
  _eventList := nil;

  _exposureCounter := 0;
  _exposureList := nil;

  _naadsMap := nil;

  dllLoadErrors := '';
  sssimLoaded := loadDynamicDll();

end.
