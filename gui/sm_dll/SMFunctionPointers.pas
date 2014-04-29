unit SMFunctionPointers;

(*
SMFunctionPointers.pas
----------------------
Begin: 2005/05/25
Last revision: $Date: 2009-08-25 01:46:42 $ $Author: areeves $
Version number: $Revision: 1.15 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2005 - 2006 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

{$INCLUDE ../Defs.inc}

interface

	uses
  	Windows, // Defines THandle

    FunctionPointers,
    ZipFunctions,

    Herd,
    SMDatabase,
    SMSimulationInput,
    HerdList
  ;


  // Function pointer types
  //-----------------------
  // (All other function pointers used in this application are defined
  // for more general use in unit FunctionPointers.pas)
  type TCFnVoid_1_THRDUpdate = procedure( r: THRDUpdate ); cdecl;



  // Functions for launching the simulation from the DLL
  //-----------------------------------------------------
  // Creates objects from database, and launches DLL if things are OK
  (*
  function startSim(
    db: TSMDatabase;
    StopReason: byte;
    stopOnDay: word;
    var errMsg: string;
    isAddingIterations: boolean = false;
    isFinishingIteration: boolean = false;
    nrOutputIterationsLastRun: longint = 0
  ): integer; overload;
  *)
  
  // Validates objects passed as parameters, and launches DLL if things are OK
  function startSim(
    smSim: TSMSimulationInput;
    herds: THerdList;
    db: TSMDatabase;
    StopReason: byte;
    stopOnDay: word;
    var errMsg: string;
    isAddingIterations: boolean = false;
    isFinishingIteration: boolean = false;
    nrOutputIterationsLastRun: longint = 0
  ): integer; overload;

  // Actually launches the DLL.  If an exception occurs, creates the error *.zip file.
  function launchSim( simFileName, herdFileName: string; var errMsg: string ): integer;

  function writeLogFile( msg: string ): string;



  // Function declarations/pointers
  //-------------------------------
  procedure guilib_printf( msg: pchar ); cdecl; // type TCFnVoid_1_CharP

  procedure guilib_sim_start(); cdecl; // type TCFnVoid_0
	procedure guilib_iteration_start( val: integer ); cdecl; // type TCFnVoid_1_Int
  procedure guilib_day_start( val: integer ); cdecl; // type TCFnVoid_1_Int

  procedure guilib_change_herd_state( r: THRDUpdate );cdecl; // type TCFnVoid_1_THRDUpdate
  procedure guilib_infect_herd( r: THRDUpdate ); cdecl; // type TCFnVoid_1_THRDUpdate
  procedure guilib_expose_herd( r: THRDUpdate ); cdecl; // type TCFnVoid_1_THRDUpdate
  procedure guilib_detect_herd( r: THRDUpdate ); cdecl; // type TCFnVoid_1_THRDUpdate
  procedure guilib_trace_herd( r: THRDUpdate );  cdecl; // type TCFnVoid_1_THRDUpdate
  procedure guilib_attempt_trace_herd( r: THRDUpdate ); cdecl; // type TCFnVoid_1_THRDUpdate
  procedure guilib_destroy_herd( r: THRDUpdate );  cdecl; // type TCFnVoid_1_THRDUpdate
  procedure guilib_vaccinate_herd( r: THRDUpdate );  cdecl; // type TCFnVoid_1_THRDUpdate

  procedure guilib_outbreak_end( val: integer ); cdecl; // type TCFnVoid_1_Int

  procedure guilib_simday_complete( val: integer ); cdecl; // type TCFnVoid_1_Int
  procedure guilib_iteration_complete( val: integer ); cdecl; // type TCFnVoid_1_Int
  procedure guilib_sim_complete( val: boolean ); cdecl; // type TCFnVoid_1_Bool

  function guilib_user_stop(): integer; cdecl; // type TCFnBool_0

	function loadDynamicDll(): boolean;

  // Used for debugging
  //------------
  function statusToString( val: integer ): string;


  var
    //Function pointers, to be loaded from the DLL.
    //----------------------------------------------
    run_sim_main: function(
      herdFile: pchar;
      paramFile: pchar;
      outputFile: pchar;
      modelDir: pchar;
      verbosity: integer
    ): integer; cdecl;

    set_printf: procedure( fn: TCFnVoid_1_CharP ); cdecl;
    set_sim_complete: procedure( fn: TCFnVoid_1_Bool ); cdecl;
    set_simday_complete: procedure( fn: TCFnVoid_1_Int ); cdecl;
    set_iteration_complete: procedure( fn: TCFnVoid_1_Int ); cdecl;
    set_change_herd_state: procedure( fn: TCFnVoid_1_THRDUpdate ); cdecl;
    set_infect_herd: procedure( fn: TCFnVoid_1_THRDUpdate ); cdecl;
    set_expose_herd: procedure( fn: TCFnVoid_1_THRDUpdate ); cdecl;
    set_detect_herd: procedure( fn: TCFnVoid_1_THRDUpdate ); cdecl;
    set_trace_herd: procedure( fn: TCFnVoid_1_THRDUpdate ); cdecl;
    set_attempt_trace_herd: procedure( fn: TCFnVoid_1_THRDUpdate ); cdecl;
    set_destroy_herd: procedure( fn: TCFnVoid_1_THRDUpdate ); cdecl;
    set_vaccinate_herd: procedure( fn: TCFnVoid_1_THRDUpdate ); cdecl;
    set_day_start: procedure( fn: TCFnVoid_1_Int ); cdecl;
    set_iteration_start: procedure( fn: TCFnVoid_1_Int ); cdecl;
    set_outbreak_end: procedure( fn: TCFnVoid_1_Int ); cdecl;
    set_sim_start: procedure( fn: TCFnVoid_0 ); cdecl;
    set_user_stop: procedure( fn: TCFnInt_0 ); cdecl;


    // Variables used for the running simulation
    //------------------------------------------
    _outbreakEnd: integer;
    _simDay: integer;
    _simIteration: integer;
    _smSim: TSMSimulationInput;
    _herds: THerdList;
    _db: TSMDatabase;

    // Variables used for DLL management
    //-----------------------------------
    libLoaded: boolean;
  	dllHandle: THandle; //Handle used to open the DLL.  Defined in unit Windows.


  const
    // Constants used while running the simulation
    //---------------------------------------------
  	ERRRUNSIMEXCEPTION = -3;
    ERRINVALIDSCENARIO = -2;
  	ERRCANNOTWRITEFILES = -1;
    ERRNONE = 0;
    ERRNOTSET = 17;

    // Debugging
    DBSMFUNCTIONPOINTERS: boolean = false; // set to true to enable debugging messages for this unit.



implementation

  uses
  	Forms, // Defines the Application object
  	SysUtils,

    MyStrUtils,
    SqlClasses,
    WindowsUtils,

    StatusEnums,
    MyDialogs,
    FormMain
  ;

  function statusToString( val: integer ): string;
  	var
    	state: TTransitionState;
  	begin
      state := TTransitionState( val );

      case state of
        tsSusceptible: result := 'susceptible';
        NAADSMStateLatent: result := 'latent';
        NAADSMStateSubclinical: result := 'subclinical';
        NAADSMStateClinical: result := 'clinical';
        NAADSMStateNaturallyImmune: result := 'naturally immune';
        NAADSMStateVaccineImmune: result := 'vaccine immune';
        NAADSMStateDestroyed: result := 'destroyed';
      else
      	result := 'undefined';
      end;

    end
  ;


	function loadDynamicDll(): boolean;
  	begin
      dllHandle := loadLibrary( 'sssim.dll' );

      result := true;

      if( dllHandle >= 32 ) then // library was successfully loaded.  Assign function pointers now.
        begin
        	dbcout( 'DLL was loaded' );

          run_sim_main := GetProcAddress( dllHandle, 'run_sim_main' );
          if( nil = @run_sim_main ) then
          	begin
          		dbcout( 'MISSING FUNCTION run_sim_main', DBSMFUNCTIONPOINTERS );
              result := false;
            end
          ;

          set_printf := GetProcAddress( dllHandle, 'set_printf' );
          if( nil <> @set_printf ) then
          	begin
          		set_printf( @guilib_printf );
              dbcout( 'guilib_printf is set', DBSMFUNCTIONPOINTERS );
            end
          else
          	begin
          		dbcout( 'MISSING FUNCTION set_printf', DBSMFUNCTIONPOINTERS );
              result := false;
            end
          ;

          set_sim_complete := GetProcAddress( dllHandle, 'set_sim_complete' );
          if( nil <> @set_sim_complete ) then
          	set_sim_complete( @guilib_sim_complete )
          else
          	begin
          		dbcout( 'MISSING FUNCTION set_sim_complete', DBSMFUNCTIONPOINTERS );
              result := false;
            end
          ;

          set_simday_complete := GetProcAddress( dllHandle, 'set_simday_complete' );
          if( nil <> @set_simday_complete ) then
          	set_simday_complete( @guilib_simday_complete )
          else
          	begin
          		dbcout( 'MISSING FUNCTION set_simday_complete', DBSMFUNCTIONPOINTERS );
              result := false;
            end
          ;

          set_iteration_complete := getProcAddress( dllHandle, 'set_iteration_complete' );
          if( nil <> @set_iteration_complete ) then
          	set_iteration_complete( @guilib_iteration_complete )
          else
          	begin
           		dbcout( 'MISSING FUNCTION set_iteration_complete', DBSMFUNCTIONPOINTERS );
              result := false;
            end
          ;

          set_change_herd_state := GetProcAddress( dllHandle, 'set_change_herd_state' );
          if( nil <> @set_change_herd_state ) then
          	set_change_herd_state( @guilib_change_herd_state )
          else
          	begin
          		dbcout( 'MISSING FUNCTION set_change_herd_state', DBSMFUNCTIONPOINTERS );
              result := false;
            end
          ;

          set_infect_herd := GetProcAddress( dllHandle, 'set_infect_herd' );
          if( nil <> @set_infect_herd ) then
          	set_infect_herd( @guilib_infect_herd )
          else
          	begin
              dbcout( 'MISSING FUNCTION set_infect_herd', DBSMFUNCTIONPOINTERS );
            	result := false;
            end
          ;

          set_expose_herd := GetProcAddress( dllHandle, 'set_expose_herd' );
          if( nil <> @set_expose_herd ) then
          	set_expose_herd( @guilib_expose_herd )
          else
          	begin
              dbcout( 'MISSING FUNCTION set_expose_herd', DBSMFUNCTIONPOINTERS );
            	result := false;
            end
          ;

          set_detect_herd := GetProcAddress( dllHandle, 'set_detect_herd' );
          if( nil <> @set_detect_herd ) then
          	set_detect_herd( @guilib_detect_herd )
          else
          	begin
              dbcout( 'MISSING FUNCTION set_detect_herd', DBSMFUNCTIONPOINTERS );
            	result := false;
            end
          ;

          set_trace_herd := GetProcAddress( dllHandle, 'set_trace_herd' );
          if( nil <> @set_trace_herd ) then
          	set_trace_herd( @guilib_trace_herd )
          else
          	begin
              dbcout( 'MISSING FUNCTION set_trace_herd', DBSMFUNCTIONPOINTERS );
            	result := false;
            end
          ;

          set_attempt_trace_herd := GetProcAddress( dllHandle, 'set_attempt_trace_herd' );
          if( nil <> @set_attempt_trace_herd ) then
          	set_attempt_trace_herd( @guilib_attempt_trace_herd )
          else
          	begin
              dbcout( 'MISSING FUNCTION set_trace_herd', DBSMFUNCTIONPOINTERS );
            	result := false;
            end
          ;

          set_destroy_herd := GetProcAddress( dllHandle, 'set_destroy_herd' );
          if( nil <> @set_destroy_herd ) then
          	set_destroy_herd( @guilib_destroy_herd )
          else
          	begin
              dbcout( 'MISSING FUNCTION set_destroy_herd', DBSMFUNCTIONPOINTERS );
            	result := false;
            end
          ;

          set_vaccinate_herd := GetProcAddress( dllHandle, 'set_vaccinate_herd' );
          if( nil <> @set_vaccinate_herd ) then
          	set_vaccinate_herd( @guilib_vaccinate_herd )
          else
          	begin
              dbcout( 'MISSING FUNCTION set_vaccinate_herd', DBSMFUNCTIONPOINTERS );
            	result := false;
            end
          ;

          set_day_start := GetProcAddress( dllHandle, 'set_day_start' );
          if( nil <> @set_day_start ) then
          	set_day_start( @guilib_day_start )
          else
          	begin
          		dbcout( 'MISSING FUNCTION set_day_start', DBSMFUNCTIONPOINTERS );
              result := false;
            end
          ;

          set_outbreak_end := GetProcAddress( dllHandle, 'set_outbreak_end' );
          if( nil <> @set_outbreak_end ) then
          	set_outbreak_end( @guilib_outbreak_end )
          else
          	begin
          		dbcout( 'MISSING FUNCTION set_outbreak_end', DBSMFUNCTIONPOINTERS );
              result := false;
            end
          ;

          set_iteration_start := GetProcAddress( dllHandle, 'set_iteration_start' );
          if( nil <> @set_iteration_start ) then
          	set_iteration_start( @guilib_iteration_start )
          else
          	begin
          		dbcout( 'MISSING FUNCTION set_iteration_start', DBSMFUNCTIONPOINTERS );
              result := false;
            end
          ;

          set_sim_start := GetProcAddress( dllHandle, 'set_sim_start' );
          if( nil <> @set_sim_start ) then
          	set_sim_start( @guilib_sim_start )
          else
          	begin
          		dbcout( 'MISSING FUNCTION set_sim_start', DBSMFUNCTIONPOINTERS );
              result := false;
            end
          ;

          set_user_stop := GetProcAddress( dllHandle, 'set_user_stop' );
          if( nil <> @set_user_stop ) then
          	set_user_stop( @guilib_user_stop )
          else
          	begin
            	dbcout( 'MISSING FUNCTION set_user_stop', DBSMFUNCTIONPOINTERS );
              result := false;
            end
          ;

        end
      else
        begin
          dbcout( 'The required dll is missing.', DBSMFUNCTIONPOINTERS );
          result := false;
        end
      ;

    end
  ;




  function launchSim( simFileName, herdFileName: string; var errMsg: string ): integer;
    var
      logFileName: string;
    begin
      // Launch the DLL with appropriate parameters
      //-------------------------------------------
      try
        try
          result := run_sim_main( pchar( herdFileName ), pchar( simFileName ), nil, nil, 2 );
        except
          on e: exception do
            begin
            	logFileName := writeLogFile( e.Message );
              errMsg := buildErrorZip( simFileName, herdFileName, logFileName );
              result := ERRRUNSIMEXCEPTION;
            end
          ;
        end;
      finally
        // clean up
        //---------
        deleteFile( simFileName );
        deleteFile( herdFileName );
        if( 0 < length( logFileName ) ) then deleteFile( logFileName );
      end;

    end
  ;


  function startSim(
        smSim: TSMSimulationInput;
        herds: THerdList;
        db: TSMDatabase;
        StopReason: byte;
        stopOnDay: word;
        var errMsg: string;
        isAddingIterations: boolean = false;
        isFinishingIteration: boolean = false;
        nrOutputIterationsLastRun: longint = 0
      ): integer;
    var
      sfn, hfn: string;
    begin
      _smSim := smSim;
      _herds := herds;
      _db := db;

      if( not ( _smSim.isValid( @errMsg ) ) and ( _herds.isValid( @errMsg ) ) ) then
        begin
          result := ERRINVALIDSCENARIO;
          exit;
        end
      ;

      // Get temporary file names
      sfn := tempFileName( currentDir() );
      hfn := tempFileName( currentDir() );

      if( not (
        ( _smSim.writeXMLFile( sfn, @errMsg ) )
      and
        ( _herds.writeXMLFile( hfn, @errMsg ) ) ) )
      then
        begin
          dbcout( errMsg );
          deleteFile( sfn );
          deleteFile( hfn );
          result := ERRCANNOTWRITEFILES;
          exit;
        end
      ;

      dbcout( 'Launching simulation' );
      result := launchSim( sfn, hfn, errMsg );
    end
  ;


  (*
  function startSim(
        db: TSMDatabase;
        StopReason: byte;
        stopOnDay: word;
        var errMsg: string;
        isAddingIterations: boolean = false;
        isFinishingIteration: boolean = false;
        nrOutputIterationsLastRun: longint = 0
      ): integer;
    var
      sfn, hfn: string;
      logFileName: string;
      cmd, workDir: string;
      cmdReturn: string;
    begin
      dbcout( 'RUNNING SIM' );

      // Export XML files
      //-----------------
      errMsg := '';

      // Check that the scenario is valid and ready to run
      if( not( db.scenarioIsValid( @errMsg ) ) ) then
        begin
          dbcout( 'The following errors were reported: ' );
          dbcout( errMsg );
          result := ERRINVALIDSCENARIO;
          exit;
        end
      ;

      errMsg := '';

      // Get temporary file names
      sfn := tempFileName( currentDir() );
      hfn := tempFileName( currentDir() );

      // Write text files
      if( not( db.writeXMLFiles( sfn, hfn, @errMsg ) ) ) then
        begin
          // FIX ME: Show the message, somehow
          dbcout( errMsg );
          deleteFile( sfn );
          deleteFile( hfn );
          result := ERRCANNOTWRITEFILES;
          exit;
        end
      ;

      result := launchSim( sfn, hfn, errMsg );

    end
  ;
  *)

  {*
  	Return name of the newly created log file
  }
  function writeLogFile( msg: string ): string;
  	var
    	logFile: textfile;
      fileName: string;
  	begin
      try
      	fileName := tempFileName( currentDir() );
        assignFile( logFile, fileName );
        rewrite( logFile );
        writeln( logFile, msg );
        closeFile( logFile );
      except
      	// Fail mostly silently
        fileName := '';
      end;

      result := fileName;
    end
  ;




  procedure guilib_printf( msg: pchar );
  	begin
  		dbcout( msg, true );
    end
  ;


  function guilib_user_stop(): integer;
  	begin
    	{$IF Defined(AR_SPREADMODEL)}
        if( assigned( frmMain ) ) then
      	  result := frmMain.userStop
        else
          result := 0
        ;
      {$ELSE}
      	result := 0;
      {$IFEND}
    end
  ;


  procedure guilib_sim_start();
  	begin
      dbcout( 'Simulation is starting', DBSMFUNCTIONPOINTERS );

      {$IF Defined(AR_SPREADMODEL)}
        _db.initializeAllOutputRecords();
        _smSim.initializeAllOutputRecords();
        _herds.initializeAllOutputRecords();
        _db.setStartTime();

        _simIteration := -1;
        _simDay := -1;
        _outbreakEnd := -1;
      	Application.ProcessMessages();
      	if( assigned( frmMain ) ) then frmMain.simStart();
      {$IFEND}
    end
  ;


  procedure guilib_iteration_start( val: integer );
  	begin
      dbcout( 'Iteration ' + intToStr( val + 1 ) + ' is beginning', DBSMFUNCTIONPOINTERS );

      {$IF Defined(AR_SPREADMODEL)}
      	Application.ProcessMessages();
        _db.prepareForIteration( val + 1 );
        _smSim.prepareForIteration( val + 1 );
        _herds.prepareForIteration( val + 1 );

        _simIteration := val + 1;
        _simDay := -1;
        _outbreakEnd := -1;

      	if( assigned( frmMain ) ) then frmMain.iterationStart( val );
      {$IFEND}
    end
  ;


  procedure guilib_day_start( val: integer );
  	begin
      dbcout( 'Day ' + intToStr( val ) + ' starting.', DBSMFUNCTIONPOINTERS );

      {$IF Defined(AR_SPREADMODEL)}
        _smSim.prepareForDay( val );
        // Right now, there is no herd-based daily preparation required.
        //_herds.prepareForDay( val );
        _simDay := val;
        Application.ProcessMessages();

      	if( assigned( frmMain ) ) then frmMain.dayStart( val );
      {$IFEND}
    end
  ;


  procedure guilib_change_herd_state( r: THRDUpdate );
    var
      h: THerd;
  	begin
      dbcout( 'Herd with index ' + intToStr( r.index ) + ' now has status ' + statusToString( r.status ), DBSMFUNCTIONPOINTERS );

      {$IF Defined(AR_SPREADMODEL)}
        h := _herds[r.index];
        h.setSimulatedStatus( TTransitionState( r.status ), true );

        Application.ProcessMessages();
      	if( assigned( frmMain ) ) then frmMain.changeHerdState( h );
      {$IFEND}
    end
  ;


  procedure guilib_expose_herd( r: THRDUpdate );
  	begin
      dbcout( 'Herd with index ' + intToStr( r.index ) + ' was exposed by ' + r.msg, DBSMFUNCTIONPOINTERS );

      {$IF Defined(AR_SPREADMODEL)}
        _herds[r.index].exposeByMechanism( r.msg );
        Application.ProcessMessages();

        // There isn't really anything to do on the main form.
        //if( assigned( frmMain ) ) then frmMain.exposeHerd( r );
      {$IFEND}
    end
  ;


  procedure guilib_attempt_trace_herd( r: THRDUpdate );
  	begin
      dbcout( 'Trace attempted for herd with index ' + intToStr( r.index ) + ' for ' + r.msg, DBSMFUNCTIONPOINTERS );

      {$IF Defined(AR_SPREADMODEL)}
        _herds[r.index].attemptTraceForReason( r.msg );

        Application.ProcessMessages();

        // There isn't currently anything to do on the main form
        //if( assigned( frmMain ) ) then frmMain.attemptTraceHerd( r );
      {$IFEND}
    end
  ;


  procedure guilib_trace_herd( r: THRDUpdate );
  	begin
      dbcout( 'Herd with index ' + intToStr( r.index ) + ' was traced after ' + r.msg, DBSMFUNCTIONPOINTERS );

      {$IF Defined(AR_SPREADMODEL)}
        _herds[r.index].traceForReason( r.msg );

        Application.ProcessMessages();

        // There isn't currently anything to do on the main form
        //if( assigned( frmMain ) ) then frmMain.traceHerd( r );
      {$IFEND}
    end
  ;


  procedure guilib_infect_herd( r: THRDUpdate );
  	begin
      dbcout( 'Herd with index ' + intToStr( r.index ) + ' was infected by ' + r.msg, DBSMFUNCTIONPOINTERS );

      {$IF Defined(AR_SPREADMODEL)}
        Application.ProcessMessages();
        _herds[r.index].infectByMechanism( r.msg, _simDay );

        // There isn't currently anything to do on the main form
        //if( assigned( frmMain ) ) then frmMain.infectHerd( h );
      {$IFEND}
    end
  ;


  procedure guilib_detect_herd( r: THRDUpdate );
    var
      wasFirstEvent: boolean;
      h: THerd;
  	begin
      dbcout( 'Herd with index ' + intToStr( r.index ) + ' was detected', DBSMFUNCTIONPOINTERS );

      {$IF Defined(AR_SPREADMODEL)}
        Application.ProcessMessages();
        h := _herds[r.index];
        wasFirstEvent := h.detect( _simDay );

        if( assigned( frmMain ) ) then frmMain.detectHerd( h, wasFirstEvent, _simDay );
      {$IFEND}
    end
  ;


  procedure guilib_destroy_herd( r: THRDUpdate );
    var
      wasFirstEvent: boolean;
      h: THerd;
  	begin
      dbcout( 'Herd with index ' + intToStr( r.index ) + ' was destroyed for ' + r.msg, DBSMFUNCTIONPOINTERS );

      {$IF Defined(AR_SPREADMODEL)}
        h := _herds[r.index];
        wasFirstEvent := h.destroyForReason( r.msg, _simDay );

        Application.ProcessMessages();

        if( assigned( frmMain ) ) then frmMain.destroyHerd( h, wasFirstEvent, _simDay );
      {$IFEND}
    end
  ;


  procedure guilib_vaccinate_herd( r: THRDUpdate );
    var
      wasFirstEvent: boolean;
      h: THerd;
  	begin
      dbcout( 'Herd with index ' + intToStr( r.index ) + ' was vaccinated for ' + r.msg, DBSMFUNCTIONPOINTERS );

      {$IF Defined(AR_SPREADMODEL)}
        h := _herds[r.index];
        wasFirstEvent := h.vaccinateForReason( r.msg, _simDay );

        Application.ProcessMessages();

        if( assigned( frmMain ) ) then frmMain.vaccinateHerd( h, wasFirstEvent, _simDay );
      {$IFEND}
    end
  ;


  procedure guilib_outbreak_end( val: integer );
  	begin
      dbcout( 'Outbreak ended on day ' + intToStr( val ), DBSMFUNCTIONPOINTERS );
      
      {$IF Defined(AR_SPREADMODEL)}
        _outbreakEnd := val;

        Application.ProcessMessages();

      	if( assigned( frmMain ) ) then frmMain.outbreakEnd( val );
      {$IFEND}
    end
  ;


  procedure guilib_simday_complete( val: integer );
  	begin
      dbcout( 'Sim day ' + intToStr( val ) + ' is complete.', DBSMFUNCTIONPOINTERS );

      {$IF Defined(AR_SPREADMODEL)}
        //_herds.processDailyRecords(); // There aren't currently any end-of-day actions for the herd list
        _smsim.processDailyRecords( _db, val );

        Application.ProcessMessages();

      	if( assigned( frmMain ) ) then frmMain.dayComplete( val );
      {$IFEND}
    end
  ;



  procedure guilib_iteration_complete( val: integer );
    var
      stopReason: integer;
  	begin
      dbcout( 'Sim iteration ' + intToStr( val ) + ' is complete.', DBSMFUNCTIONPOINTERS );

      {$IF Defined(AR_SPREADMODEL)}
      	Application.ProcessMessages();

        if( assigned( frmMain ) ) then
          stopReason := frmMain.userStop
        else
          stopReason := 0
        ;

        case stopReason of
          -1:
            // User stopped the simulation before the current iteration completed.
            // Don't record it.
          ;
          0: // iteration ended of its own accord.
            begin
              // Iterations should be 1-indexed in the database.
              dbcout( 'Iteration ' + intToStr( val + 1 ) + ' is complete', true );
              _smSim.processIterationRecords( _db, val + 1 );
              _herds.processIterationRecords( _db, val + 1 );

              _db.processIterationRecords(
                val + 1,
                _simDay,
                ( -1 <> _outbreakEnd )
              );

              if( assigned( frmMain ) ) then frmMain.iterationComplete( val );
            end
          ;
        end;
      {$IFEND}
    end
  ;


  procedure guilib_sim_complete( val: boolean );
  	begin
      if( val ) then
        dbcout( 'Sim is successfully completed.', DBSMFUNCTIONPOINTERS )
      else
        dbcout( 'Sim did not complete successfully.', DBSMFUNCTIONPOINTERS )
      ;

      {$IF Defined(AR_SPREADMODEL)}
        if( val ) then _db.setEndTime();

        Application.ProcessMessages();

      	if( assigned( frmMain ) ) then frmMain.simComplete( val );
      {$IFEND}
    end
  ;


initialization

    _smSim := nil;
    _herds := nil;
    _db := nil;

end.
