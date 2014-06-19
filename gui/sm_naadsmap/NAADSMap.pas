unit NAADSMap;

(*
NAADSMap.pas
-----------------
Begin: 2007/02/01
Last revision: $Date: 2013-06-27 19:11:36 $ $Author: areeves $
Version: $Revision: 1.14.4.1 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <aaron.reeves@naadsm.org>
--------------------------------------------------
Copyright (C) 2007 - 2009 Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

{$INCLUDE Defs.inc}

interface

  uses
    QVectors,
    QLists,

    SMSimulationInput,
    Herd,
    SMDatabase,
    StatusEnums,
    EventsAndExposures
  ;

  type TMapOutput = (MAPStart, MAPEnd);

  type TNAADSMap = class
    protected
      { References (pointers) to simulation objects }
      _smSim: TSMSimulationInput;
      _herds: THerdList;
      _smdb: TSMDatabase;

      { The top-level directory where NAADSMap output will be created }
      _outputDir: string;

      { Can _outputDir be used for output? }
      _dirReady: boolean;

      { The directory where output for the current iteration is being written }
      _iterationOutputDir: string;

      { This file gets status changes.  It is, for now, the only major NAADSMap file. }
      _mainFile: textFile;

      { Can _mainFile be written? }
      _writingMainFile: boolean;

      { Used to Keep track of the size (in lines) of the main file }
      _iterationLineCount: integer;

      { Used to keep track of days recorded in the main file }
      _statDayVector: TQIntegerVector;
      _lastStatDay: integer;

      { Used to keep track of all statuses that appear in the main file }
      _statusList: TQStringList;

      { Create a name like 'Iteration007' with enough leading zeroes that directories will always sort in the proper order. }
      function iterationDirName( const iteration: integer ): string;

      { Write the state of every herd to the main file at the start and end of each iteration }
      procedure writeAllStates( const mapDay: TMapOutput );

      { Write all of the small files that must be generated for every iteration }
      procedure writeAncillaryFiles();

      { These functions write the individual ancillary files }
      procedure writeDaysFile();
      procedure writeProdTypeFile();
      procedure writeStatusFile();
      procedure writeMaxFile();

    public
      constructor create( smSim: TSMSimulationInput; herds: THerdList; smdb: TSMDatabase );
      destructor destroy(); override;

      { Take the appropriate action when these events occur in the simulation }
      procedure simStart();
      procedure iterationStart( const iteration: integer );
      procedure herdEvent( const h: THerd; const evt: TEventCode; const simDay: integer );
      procedure iterationEnd( const iteration: integer );
      procedure simEnd( const simCompleted: boolean );

    end
  ;


implementation

  uses
    SysUtils,
    StrUtils,

    MyStrUtils,
    I88n,

    ProductionType,
    ProductionTypeList,
    NAADSMLibraryTypes
  ;

//-----------------------------------------------------------------------------
// Construction/destruction
//-----------------------------------------------------------------------------
  constructor TNAADSMap.create( smSim: TSMSimulationInput; herds: THerdList; smdb: TSMDatabase );
    begin
      inherited create();

      _smSim := smSim;
      _herds := herds;
      _smdb := smdb;

      _writingMainFile := false;

      _statDayVector := TQIntegerVector.create();
      _statusList := TQStringList.create();
    end
  ;


  destructor TNAADSMap.destroy();
    begin
      freeAndNil( _statDayVector );
      freeAndNil( _statusList );
      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Directory names
//-----------------------------------------------------------------------------
  function TNAADSMap.iterationDirName( const iteration: integer ): string;
    var
      i: integer;
      itStrLen: integer;
    begin
      result := '';

      itStrLen := length( intToStr( _smSim.simIterations ) );

      for i := length( intToStr( iteration ) ) to ( itStrLen - 1 ) do
        result := result + '0'
      ;

      result := 'Iteration' + result + intToStr( iteration );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Ancillary files
//-----------------------------------------------------------------------------
  procedure TNAADSMap.writeAncillaryFiles();
    begin
      writeDaysFile();
      writeProdTypeFile();
      writeStatusFile();
      writeMaxFile();
    end
  ;

  procedure TNAADSMap.writeDaysFile();
    var
      f: textFile;
      i: integer;
    begin
      // Write Days.txt
      //---------------
      try
        try
          assignFile( f, _iterationOutputDir + '\Days.txt' );
          rewrite( f );
          writeln( f, '"DAYS"' );

          for i := 0 to _statDayVector.count - 1 do
            writeLn( f, intToStr( _statDayVector[i] ) )
          ;

          writeln( f, '9999' );
        except
          // fail silently, for now...
        end;
      finally
        closeFile( f );
      end;
    end
  ;

  procedure TNAADSMap.writeProdTypeFile();
    var
      f: textFile;
      it: TProductionTypeListIterator;
    begin
      it := nil;

      // Write Prod.txt
      //---------------
      try
        try
          assignFile( f, _iterationOutputDir + '\Prod.txt' );
          rewrite( f );
          writeLn( f, csvQuote( 'PRODTYPE' ) );

          it := TProductionTypeListIterator.create( _smSim.ptList );
          it.toFirst();

          while( nil <> it.current() ) do
            begin
              writeln( f, csvQuote( it.current().productionTypeDescr ) );
              it.incr();
            end
          ;
        except
          // fail silently, for now...
        end;
      finally
        freeAndNil( it );
        closeFile( f );
      end;
    end
  ;


  procedure TNAADSMap.writeStatusFile();
    var
      f: textFile;
      i: integer;
    begin
      // Write Stat.txt
      //---------------
      try
        try
          assignFile( f, _iterationOutputDir + '\Stat.txt' );
          rewrite( f );
          writeLn( f, csvQuote( 'STATUS' ) );

          for i := 0 to _statusList.count - 1 do
            writeLn( f, csvQuote( _statusList.at(i) ) )
          ;
        except
          // fail silently, for now...
        end;
      finally
        closeFile( f );
      end;
    end
  ;

  procedure TNAADSMap.writeMaxFile();
    var
      f: textFile;
      str: string;
    begin
      // Write Max.txt
      //--------------
      try
        try
          assignFile( f, _iterationOutputDir + '\Max.txt' );
          rewrite( f );

          writeLn( f, '"NUMDAYS","NUMSTAT","NUMPREM","NUMPROD","NUMDAYPREMSTATUS"' );

          // NUMDAYS is the size of _statDayVector, plus one for day "9999"
          str := intToStr( _statDayVector.count + 1 ) + ',';

          // NUMSTAT is the size of _statusList
          str := str + intToStr( _statusList.count ) + ',';

          // NUMPREM is the size of the herd list
          str := str + intToStr( _herds.Count ) + ',';

          // NUMPROD is the size of the production type list
          str := str + intToStr( _smSim.ptList.Count ) + ',';

          // NUMDAYPREMSTATUS is the number of lines (excluding the header) written to DayPremStat.txt
          str := str + intToStr( _iterationLineCount );

          writeLn( f, str );
        except
          // fail silently, for now...
        end;
      finally
        closeFile( f );
      end;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Writing the main file
//-----------------------------------------------------------------------------
  procedure TNAADSMap.writeAllStates( const mapDay: TMapOutput );
    var
      it: THerdListIterator;
      h: THerd;
      line: string;
      dayStr: string;
    begin
      it := THerdListIterator.create( _herds );

      case mapDay of
        MAPStart: dayStr := '0';
        MAPEnd: dayStr := '9999';
      end;

      if( MAPStart = mapDay ) then
        begin
          line := '"DAY","PREM","STATUS","Latitude","Longitude","ProdDesc","Size"';
          writeLn( _mainFile, line );
        end
      ;

      while( nil <> it.current() ) do
        begin
          h := it.current();
          // Note the space before the herd ID.  It always appears in sample files, but I don't know if it's necessary.
          line := dayStr + csvListSep
            + csvQuote( ' ' + intToStr( h.id ) ) + csvListSep
            + csvQuote( naadsmDiseaseStateStr( h.diseaseStatus ) ) + csvListSep
            + csvFloatToStr( h.lat ) + csvListSep
            + csvFloatToStr( h.lon ) + csvListSep
            + csvQuote( h.prodTypeName ) + csvListSep
            + intToStr( h.initialSize )
          ;
          writeLn( _mainFile, line );
          inc( _iterationLineCount );
          it.incr();
        end
      ;

      it.Free();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Responding to sim events
//-----------------------------------------------------------------------------
  procedure TNAADSMap.simStart();
    var
      stamp: string;
    begin
      // Set up the root directory for output
      //-------------------------------------
      _outputDir := _smSim.outputOptions.NAADSMapOutputDirectory;

      if( not( isAbsolutePath( _outputDir ) ) ) then
        _outputDir := directory( _smdb.permanentDBFileName ) + _outputDir
      ;

      if( ( '\' <> rightStr( _outputDir, 1 ) ) and ( '/' <> rightStr( _outputDir, 1 ) ) ) then
        _outputDir := _outputDir + '\'
      ;

      // Tack on a subdirectory with a timestamp, to ensure that no existing data will be overwritten
      DateTimeToString( stamp, 'yymmdd_hhnnss', now() );
      _outputDir := _outputDir + 'NAADSMap-' + stripExtension( shortFileName( _smdb.permanentDBFileName ) ) + '-'+ stamp + '\';

      _dirReady := forceDirectories( _outputDir );
    end
  ;


  procedure TNAADSMap.iterationStart( const iteration: integer );
    begin
      // Set up the iteration subdirectory
      //----------------------------------
      if( _dirReady ) then
        begin
          _iterationOutputDir := _outputDir + iterationDirName( iteration );
          _dirReady := createDir( _iterationOutputDir );
        end
      ;

      if( _dirReady ) then
        begin
          // Create the main file
          //---------------------
          try
            _writingMainFile := true;
            assignFile( _mainFile, _iterationOutputDir + '\DayPremStat.txt' );
            rewrite( _mainFile );
          except
            // fail silently... for now.
            _writingMainFile := false;
          end;


          // Set up the variables used to keep track of everything
          //------------------------------------------------------
          _statDayVector.clear();
          _statDayVector.append( 0 );
          _lastStatDay := 0;

          _statusList.clear();
          _statusList.append( naadsmDiseaseStateStr( NAADSMStateSusceptible ) );

          _iterationLineCount := 0;


          // Write the starting population to the main file
          //-----------------------------------------------
          if( _writingMainFile ) then
            writeAllStates( MAPStart )
          ;
        end
      else
        _writingMainFile := false
      ;
    end
  ;


  procedure TNAADSMap.herdEvent( const h: THerd; const evt: TEventCode; const simDay: integer );
    var
      str: string;
      statusStr: string;
    begin
      // Write status changes to the main (and currently only) file
      //-----------------------------------------------------------
      // Sample format for state changes:
      //18," 16102","Destroyed",31.418804,-81.915954,"Commercial Layer",79000
      if( ( EVTTransistionStateChange = evt ) and ( _writingMainFile ) ) then
        begin
          statusStr := naadsmDiseaseStateStr( h.diseaseStatus );

          // Note the space before the herd ID.  It always appears in sample files, but I don't know if it's necessary.
          str := intToStr( simDay ) + csvListSep
            + csvQuote( ' ' + intToStr( h.id ) ) + csvListSep
            + csvQuote( statusStr ) + csvListSep
            + csvFloatToStr( h.lat ) + csvListSep
            + csvFloatToStr( h.lon ) + csvListSep
            + csvQuote( h.prodTypeName ) + csvListSep
            + intToStr( h.initialSize )
          ;

          writeln( _mainFile, str );
          inc( _iterationLineCount );

          if( simDay <> _lastStatDay ) then
            begin
              _lastStatDay := simDay;
              _statDayVector.append( simDay );
            end
          ;

          if( not( _statusList.contains( statusStr ) ) ) then
            _statusList.append( statusStr )
          ;
        end
      ;

      (*
      // These events are sent by NAADSMLibrary, but not yet recorded for NAADSMap:
      EVTDestroyed
      EVTVaccinated
      EVTInfected
      EVTDetected
      EVTZoneFocus
      EVTZoneChanged

      // These events are NOT sent by NAADSMLibrary to NAADSMap.  They probably aren't needed.
      EVTTraceForwardDirect
      EVTTraceForwardIndirect

      // These events are new since NAADSMap was written.  I don't know if they are needed or not.
      EVTTraceBackDirect
      EVTTraceBackIndirect
      EVTHerdExam
      EVTDiagnosticTest
      *)
    end
  ;


  procedure TNAADSMap.iterationEnd( const iteration: integer );
    begin
      if( _writingMainFile ) then
        begin
          try
            writeAllStates( MAPEnd );
            closeFile( _mainFile );

            writeAncillaryFiles();
          except
            // fail silently, for now.
          end;
        end
      ;
    end
  ;


  procedure TNAADSMap.simEnd( const simCompleted: boolean );
    begin
      // Do nothing
    end
  ;
//-----------------------------------------------------------------------------

end.