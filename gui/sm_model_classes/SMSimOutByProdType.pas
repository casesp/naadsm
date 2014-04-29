unit SMSimOutByProdType;

(*
SMSimOutByProdType.pas
-----------------------
Begin: 2005/07/06
Last revision: $Date: 2008/10/17 19:20:47 $ $Author: areeves $
Version number: $Revision: 1.28 $
Project: NAADSM and related applications
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2005 - 2008 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

	uses
  	SMDatabase,
    StatusEnums
  ;

	type TDailyRecordType = (
  	DRTUnspecified,
    DRTDaily,
    DRTIteration
  );


  type TSMIterationOutput = class
    protected
      // Properties
      _prodTypeID: integer;

      // For internal use
      procedure initialize();
      procedure clearRunningTotals();

      // Properties
      procedure setProdTypeID( val: integer );
      function getProdTypeID(): integer;

      function getExpcUTotal(): longint;
      function getExpcATotal(): longint;
      function getInfcUTotal(): longint;
      function getInfcATotal(): longint;

      function getDescUTotal(): longint;
      function getDescATotal(): longint;
      function getVaccUTotal(): longint;
      function getVaccATotal(): longint;

    public
      // Output values
      //---------------
      firstDetection: integer;
      firstVaccination: integer;
      firstDestruction: integer;

      // Running totals for each disease state
      tscUSusc: longint;
      tscASusc: longint;
      tscULat: longint;
      tscALat: longint;
      tscUSubc: longint;
      tscASubc: longint;
      tscUClin: longint;
      tscAClin: longint;
      tscUNImm: longint;
      tscANImm: longint;
      tscUVImm: longint;
      tscAVImm: longint;
      tscUDest: longint;
      tscADest: longint;

      // Running totals for cause of infection
      infcUIni: longint;
      infcAIni: longint;
      infcUAir: longint;
      infcAAir: longint;
      infcUDir: longint;
      infcADir: longint;
      infcUInd: longint;
      infcAInd: longint;

      // Running totals for exposures
      expcUDir: longint;
      expcADir: longint;
      expcUInd: longint;
      expcAInd: longint;

      // Running totals for traces
      trcUDir: longint;
      trcADir: longint;
      trcUInd: longint;
      trcAInd: longint;
      trcUDirp: longint;
      trcADirp: longint;
      trcUIndp: longint;
      trcAIndp: longint;

      // Running totals for detection
      detcUClin: longint;
      detcAClin: longint;

      // Running totals for destruction
      descUIni: longint;
      descAIni: longint;
      descUDet: longint;
      descADet: longint;
      descUDir: longint;
      descADir: longint;
      descUInd: longint;
      descAInd: longint;
      descURing: longint;
      descARing: longint;

      // Running totals for vaccination
      vaccUIni: longint;
      vaccAIni: longint;
      vaccURing: longint;
      vaccARing: longint;

      // Running totals for zone foci
      zoncFoci: longint;

      constructor create(); virtual;
      destructor destroy(); override;
      procedure clear(); virtual;

      procedure addCumulRecordsFrom( const src: TSMIterationOutput );
      procedure setCumulRecordsFrom( const src: TSMIterationOutput );

      // Properties
      //-----------
      property prodTypeID: integer read getProdTypeID write setProdTypeID;

      property expcUTotal: longint read getExpcUTotal;
      property expcATotal: longint read getExpcATotal;
      property infcUTotal: longint read getInfcUTotal;
      property infcATotal: longint read getInfcATotal;

      property descUTotal: longint read getDescUTotal;
      property descATotal: longint read getDescATotal;
      property vaccUTotal: longint read getVaccUTotal;
      property vaccATotal: longint read getVaccATotal;
    end
  ;


	type TSMSimOutByProdType = class( TSMIterationOutput )
    protected
      // For internal use
      //------------------
      procedure initialize();
      procedure clearDailyTotals();

    public
      // Properties
      //------------
      // Daily numbers for each disease state
      tsdUSusc: longint;
      tsdASusc: longint;
      tsdULat: longint;
      tsdALat: longint;
      tsdUSubc: longint;
      tsdASubc: longint;
      tsdUClin: longint;
      tsdAClin: longint;
      tsdUNImm: longint;
      tsdANImm: longint;
      tsdUVImm: longint;
      tsdAVImm: longint;
      tsdUDest: longint;
      tsdADest: longint;

      // New daily counts for cause of infection
      infnUAir: longint;
      infnAAir: longint;
      infnUDir: longint;
      infnADir: longint;
      infnUInd: longint;
      infnAInd: longint;

      // New daily counts for detection
      detnUClin: longint;
      detnAClin: longint;

      // New daily counts for tracing
      trnUDir: longint;
      trnADir: longint;
      trnUInd: longint;
      trnAInd: longint;

      // New daily counts for destruction for any reason
      desnUAll: longint;
      desnAAll: longint;

      // New daily counts for vaccination for any reason
      vaccnUAll: longint;
      vaccnAAll: longint;

      // New daily count for zone foci
      zonnFoci: longint;

      // Number of apparently infectious units on a particular day
      appUInfectious: longint;

      // New daily counts of units/animals infected for all causes
      function getInfUNew(): longint;
      function getInfANew(): longint;

    	constructor create(); override;
      destructor destroy(); override;

      procedure clear(); override;
      procedure clearNewDailyCounts();
      procedure decrementDailyCounts( herdAnimalCount: longint; oldState: TTransitionState );

      procedure setAllRecordsFrom( const src: TSMSimOutByProdType );

      procedure setDailyRecordsFrom( const src: TSMSimOutByProdType );
      procedure addDailyRecordsFrom( const src: TSMSimOutByProdType );

      procedure insertDatabaseOutputs(
        drt: TDailyRecordType;
      	db: TSMDatabase;
        ptiD: integer;
        iteration: integer;
        day: integer = 0
      );

      procedure debug();
      procedure debugSel();

      // New daily infections for all causes
      property infUNew: longint read getInfUNew;
      property infANew: longint read getInfANew;
  	end
  ;


implementation

	uses
  	SysUtils,
  	SqlClasses,
    MyStrUtils,
    USStrUtils,
    DebugWindow,
    RemoteDatabaseParams
  ;


  const
  	DBSMSIMOUTBYPRODTYPE: boolean = false; // set to true to display debugging messages for this unit.

//-----------------------------------------------------------------------------
// construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TSMIterationOutput.create();
    begin
   		// Yes, I know that this is unnecessary.
      // I'm doing it anyway.
      inherited create();
      initialize();
    end
  ;

  
  destructor TSMIterationOutput.destroy();
  	begin
    	// There is nothing to do here.
      inherited destroy();
    end
  ;


  procedure TSMIterationOutput.clearRunningTotals();
    begin
      // Running totals for each disease state
      tscUSusc := 0;
      tscASusc := 0;
      tscULat := 0;
      tscALat := 0;
      tscUSubc := 0;
      tscASubc := 0;
      tscUClin := 0;
      tscAClin := 0;
      tscUNImm := 0;
      tscANImm := 0;
      tscUVImm := 0;
      tscAVImm := 0;
      tscUDest := 0;
      tscADest := 0;
      // Running totals for cause of infection
      infcUIni := 0;
      infcAIni := 0;
      infcUAir := 0;
      infcAAir := 0;
      infcUDir := 0;
      infcADir := 0;
      infcUInd := 0;
      infcAInd := 0;
      // Running totals for exposures
      expcUDir := 0;
      expcADir := 0;
      expcUInd := 0;
      expcAInd := 0;
      // Running totals for traces
      trcUDir := 0;
      trcADir := 0;
      trcUInd := 0;
      trcAInd := 0;
      trcUDirp := 0;
      trcADirp := 0;
      trcUIndp := 0;
      trcAIndp := 0;
      // Running totals for detection
      detcUClin := 0;
      detcAClin := 0;
      // Running totals for destruction
      descUIni := 0;
      descAIni := 0;
      descUDet := 0;
      descADet := 0;
      descUDir := 0;
      descADir := 0;
      descUInd := 0;
      descAInd := 0;
      descURing := 0;
      descARing := 0;
      // Running totals for vaccination
      vaccUIni := 0;
      vaccAIni := 0;
      vaccURing := 0;
      vaccARing := 0;
      // Running totals for zone foci
      zoncFoci := 0;
    end
  ;


  procedure TSMIterationOutput.setCumulRecordsFrom( const src: TSMIterationOutput );
    begin
      clear();
      addCumulRecordsFrom( src );
    end
  ;

  
  procedure TSMIterationOutput.addCumulRecordsFrom( const src: TSMIterationOutput );
    begin
      // Running totals for each disease state
      inc( self.tscUSusc, src.tscUSusc );
      inc( self.tscASusc, src.tscASusc );
      inc( self.tscULat, src.tscULat );
      inc( self.tscALat, src.tscALat );
      inc( self.tscUSubc, src.tscUSubc );
      inc( self.tscASubc, src.tscASubc );
      inc( self.tscUClin, src.tscUClin );
      inc( self.tscAClin, src.tscAClin );
      inc( self.tscUNImm, src.tscUNImm );
      inc( self.tscANImm, src.tscANImm );
      inc( self.tscUVImm, src.tscUVImm );
      inc( self.tscAVImm, src.tscAVImm );
      inc( self.tscUDest, src.tscUDest );
      inc( self.tscADest, src.tscADest );

      // Running totals for cause of infection
      inc( self.infcUIni, src.infcUIni );
      inc( self.infcAIni, src.infcAIni );
      inc( self.infcUAir, src.infcUAir );
      inc( self.infcAAir, src.infcAAir );
      inc( self.infcUDir, src.infcUDir );
      inc( self.infcADir, src.infcADir );
      inc( self.infcUInd, src.infcUInd );
      inc( self.infcAInd, src.infcAInd );

      // Running totals for exposures
      inc( self.expcUDir, src.expcUDir );
      inc( self.expcADir, src.expcADir );
      inc( self.expcUInd, src.expcUInd );
      inc( self.expcAInd, src.expcAInd );

      // Running totals for traces
      inc( self.trcUDir, src.trcUDir );
      inc( self.trcADir, src.trcADir );
      inc( self.trcUInd, src.trcUInd );
      inc( self.trcAInd, src.trcAInd );
      inc( self.trcUDirp, src.trcUDirp );
      inc( self.trcADirp, src.trcADirp );
      inc( self.trcUIndp, src.trcUIndp );
      inc( self.trcAIndp, src.trcAIndp );

      // Running totals for detection
      inc( self.detcUClin, src.detcUClin );
      inc( self.detcAClin, src.detcAClin );

      // Running totals for destruction
      inc( self.descUIni, src.descUIni );
      inc( self.descAIni, src.descAIni );
      inc( self.descUDet, src.descUDet );
      inc( self.descADet, src.descADet );
      inc( self.descUDir, src.descUDir );
      inc( self.descADir, src.descADir );
      inc( self.descUInd, src.descUInd );
      inc( self.descAInd, src.descAInd );
      inc( self.descURing, src.descURing );
      inc( self.descARing, src.descARing );

      // Running totals for vaccination
      inc( self.vaccUIni, src.vaccUIni );
      inc( self.vaccAIni, src.vaccAIni );
      inc( self.vaccURing, src.vaccURing );
      inc( self.vaccARing, src.vaccARing );

      // Running totals for zone foci
      inc( self.zoncFoci, src.zoncFoci );
    end
  ;



  procedure TSMIterationOutput.initialize();
    begin
      // FIX ME: if a herd's initial state is destroyed or vaccinated, one or more of these should be 0
      _prodTypeID := -1;
      clear();
    end
  ;


  procedure TSMIterationOutput.clear();
    begin
      firstDetection := -1;
      firstDestruction := -1;
      firstVaccination := -1;

      clearRunningTotals();
    end
  ;


  procedure TSMIterationOutput.setProdTypeID( val: integer ); begin _prodTypeID := val; end;
  function TSMIterationOutput.getProdTypeID(): integer; begin result := _prodTypeID; end;


  function TSMIterationOutput.getExpcUTotal(): longint;
    begin
      result := expcUDir + expcUInd;
    end
  ;


  function TSMIterationOutput.getExpcATotal(): longint;
    begin
      result := expcADir + expcAInd;
    end
  ;


  function TSMIterationOutput.getInfcUTotal(): longint;
    begin
      // infcUTotal intentionally does NOT include units that were infected at the beginning of the simulation.
      result := infcUAir + infcUDir + infcUInd;
    end
  ;


  function TSMIterationOutput.getInfcATotal(): longint;
    begin
      // infcATotal intentionally does NOT include animals that were infected at the beginning of the simulation.
      result := infcAAir + infcADir + infcAInd;
    end
  ;


  function TSMIterationOutput.getDescUTotal(): longint;
    begin
      // descUTotal intentionally does NOT include units that were destroyed at the beginning of the simulation.
      result := descUDet + descUDir + descUInd + descURing;
    end
  ;


  function TSMIterationOutput.getDescATotal(): longint;
    begin
      // descATotal intentionally does NOT include animals that were destroyed at the beginning of the simulation.
      result := descADet + descADir + descAInd + descARing;
    end
  ;


  function TSMIterationOutput.getVaccUTotal(): longint;
    begin
      // vaccUTotal intentionally does NOT include units that were vaccine immune at the beginning of the simulation.
      result := vaccURing;
    end
  ;


  function TSMIterationOutput.getVaccATotal(): longint;
    begin
      // vaccATotal intentionally does NOT include animals that were vaccine immune at the beginning of the simulation.
      result := vaccARing;
    end
  ;


//-----------------------------------------------------------------------------
// construction/initialization/destruction
//-----------------------------------------------------------------------------
	constructor TSMSimOutByProdType.create();
  	begin
   		// Yes, I know that this is unnecessary.
      // I'm doing it anyway.
      inherited create();
      initialize();
    end
  ;


  procedure TSMSimOutByProdType.initialize();
  	begin
      inherited initialize();
      clear();
    end
  ;


  destructor TSMSimOutByProdType.destroy();
  	begin
    	// There is nothing to do here.
      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Data handling
//-----------------------------------------------------------------------------
  procedure TSMSimOutByProdType.setDailyRecordsFrom( const src: TSMSimOutByProdType );
    begin
      clear();
      addDailyRecordsFrom( src );
    end
  ;


  procedure TSMSimOutByProdType.setAllRecordsFrom( const src: TSMSimOutByProdType );
    begin
      clear();
      addDailyRecordsFrom( src );
      addCumulRecordsFrom( src );
    end
  ;


  procedure TSMSimOutByProdType.addDailyRecordsFrom( const src: TSMSimOutByProdType );
    begin
      // Daily numbers for each disease state
      inc( self.tsdUSusc, src.tsdUSusc );
      inc( self.tsdASusc, src.tsdASusc );
      inc( self.tsdULat, src.tsdULat );
      inc( self.tsdALat, src.tsdALat );
      inc( self.tsdUSubc, src.tsdUSubc );
      inc( self.tsdASubc, src.tsdASubc );
      inc( self.tsdUClin, src.tsdUClin );
      inc( self.tsdAClin, src.tsdAClin );
      inc( self.tsdUNImm, src.tsdUNImm );
      inc( self.tsdANImm, src.tsdANImm );
      inc( self.tsdUVImm, src.tsdUVImm );
      inc( self.tsdAVImm, src.tsdAVImm );
      inc( self.tsdUDest, src.tsdUDest );
      inc( self.tsdADest, src.tsdADest );

      // New daily counts for cause of infection
      inc( self.infnUAir, src.infnUAir );
      inc( self.infnAAir, src.infnAAir );
      inc( self.infnUDir, src.infnUDir );
      inc( self.infnADir, src.infnADir );
      inc( self.infnUInd, src.infnUInd );
      inc( self.infnAInd, src.infnAInd );

      // New daily counts for detection
      inc( self.detnUClin, src.detnUClin );
      inc( self.detnAClin, src.detnAClin );

      // New daily counts for tracing
      inc( self.trnUDir, src.trnUDir );
      inc( self.trnADir, src.trnADir );
      inc( self.trnUInd, src.trnUInd );
      inc( self.trnAInd, src.trnAInd );

      // New daily counts for destruction for any reason
      inc( self.desnUAll, src.desnUAll );
      inc( self.desnAAll, src.desnAAll );

      // New daily counts for vaccination for any reason
      inc( self.vaccnUAll, src.vaccnUAll );
      inc( self.vaccnAAll, src.vaccnAAll );

      // New daily counts for zone foci
      inc( self.zonnFoci, src.zonnFoci );

      // Daily number of apparently infectious units
      inc( self.appUInfectious, src.appUInfectious );
    end
  ;



  procedure TSMSimOutByProdType.clearDailyTotals();
    begin
      // Daily numbers for each disease state
      tsdUSusc := 0;
      tsdASusc := 0;
      tsdULat := 0;
      tsdALat := 0;
      tsdUSubc := 0;
      tsdASubc := 0;
      tsdUClin := 0;
      tsdAClin := 0;
      tsdUNImm := 0;
      tsdANImm := 0;
      tsdUVImm := 0;
      tsdAVImm := 0;
      tsdUDest := 0;
      tsdADest := 0;

      // Number of apparently infectious units
      appUInfectious := 0;
    end
  ;


  procedure TSMSimOutByProdType.clearNewDailyCounts();
    begin
      // New daily counts for cause of infection
      infnUAir := 0;
      infnAAir := 0;
      infnUDir := 0;
      infnADir := 0;
      infnUInd := 0;
      infnAInd := 0;

      // New daily counts for detection
      detnUClin := 0;
      detnAClin := 0;

      // New daily counts for tracing
      trnUDir := 0;
      trnADir := 0;
      trnUInd := 0;
      trnAInd := 0;

      // New daily counts for destruction for any reason
      desnUAll := 0;
      desnAAll := 0;

      // New daily counts for vaccination for any reason
      vaccnUAll := 0;
      vaccnAAll := 0;

      // New daily counts for zone foci
      zonnFoci := 0;
    end
  ;


  function TSMSimOutByProdType.getInfUNew(): longint;
    begin
      result := infnUAir + infnUDir + infnUInd;
    end
  ;


  function TSMSimOutByProdType.getInfANew(): longint;
    begin
      result := infnAAir + infnADir + infnAInd;
    end
  ;


  procedure TSMSimOutByProdType.clear();
    begin
      clearDailyTotals();
      clearNewDailyCounts();
      inherited clear();
    end
  ;


  procedure TSMSimOutByProdType.decrementDailyCounts( herdAnimalCount: longint; oldState: TTransitionState );
    begin
      case oldState of
        tsSusceptible:
          begin
            dec( tsdUSusc );
            dec( tsdASusc, herdAnimalCount );
          end
        ;
        tsLatent:
          begin
            dec( tsdULat );
            dec( tsdALat, herdAnimalCount );
          end
        ;
        tsSubclinical:
          begin
            dec( tsdUSubc );
            dec( tsdASubc, herdAnimalCount );
          end
        ;
        tsClinical:
          begin
            dec( tsdUClin );
            dec( tsdAClin, herdAnimalCount );
          end
        ;
        tsNaturalImmune:
          begin
            dec( tsdUNImm );
            dec( tsdANImm, herdAnimalCount );
          end
        ;
        tsvaccineImmune:
          begin
            dec( tsdUVImm );
            dec( tsdAVImm, herdAnimalCount );
          end
        ;
        tsDestroyed:
          raise exception.Create( 'Number of destroyed herd is going down!  This can''t happen!' )
        ;
        else
          raise exception.Create( 'Unrecognized disease state in TProductionType.makeDailyRecord' )
        ;
      end;

      if
        ( 0 > tsdUSusc )
      or
        ( 0 > tsdASusc )
      or
        ( 0 > tsdULat )
      or
        ( 0 > tsdALat )
      or
        ( 0 > tsdUSubc )
      or
        ( 0 > tsdASubc )
      or
        ( 0 > tsdUClin )
      or
        ( 0 > tsdAClin )
      or
        ( 0 > tsdUNImm )
      or
        ( 0 > tsdANImm )
      or
        ( 0 > tsdUVImm )
      or
        ( 0 > tsdAVImm )
      or
        ( 0 > tsdUDest )
      or
        ( 0 > tsdADest )
      then
        begin
          dbcout( 'tsdUClin:' + intToStr( tsdUClin ), DBSMSIMOUTBYPRODTYPE );
          dbcout( 'tsdAClin:' + intToStr( tsdAClin ), DBSMSIMOUTBYPRODTYPE );

          dbcout( 'tsdUDest:' + intToStr( tsdUDest ), DBSMSIMOUTBYPRODTYPE );
          dbcout( 'tsdADest:' + intToStr( tsdADest ), DBSMSIMOUTBYPRODTYPE );

          dbcout( 'tsdULat:' + intToStr( tsdULat ), DBSMSIMOUTBYPRODTYPE );
          dbcout( 'tsdALat:' + intToStr( tsdALat ), DBSMSIMOUTBYPRODTYPE );

          dbcout( 'tsdUNImm:' + intToStr( tsdUNImm ), DBSMSIMOUTBYPRODTYPE );
          dbcout( 'tsdANImm:' + intToStr( tsdANImm ), DBSMSIMOUTBYPRODTYPE );

          dbcout( 'tsdUSubc:' + intToStr( tsdUSubc ), DBSMSIMOUTBYPRODTYPE );
          dbcout( 'tsdASubc:' + intToStr( tsdASubc ), DBSMSIMOUTBYPRODTYPE );

          dbcout( 'tsdUSusc:' + intToStr( tsdUSusc ), DBSMSIMOUTBYPRODTYPE );
          dbcout( 'tsdASusc:' + intToStr( tsdASusc ), DBSMSIMOUTBYPRODTYPE );

          dbcout( 'tsdUVImm:' + intToStr( tsdUVImm ), DBSMSIMOUTBYPRODTYPE );
          dbcout( 'tsdAVImm:' + intToStr( tsdAVImm ), DBSMSIMOUTBYPRODTYPE );

          raise exception.create( 'Number of units or animals in transition state is less than 0' );
        end
      ;

    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// SpreadModel-specific functions
//-----------------------------------------------------------------------------
  procedure TSMSimOutByProdType.insertDatabaseOutputs(
        drt: TDailyRecordType;
      	db: TSMDatabase;
        ptID: integer;
        iteration: integer;
        day: integer = 0
      );
    var
      qDict: TQueryDictionary;
      table: string;
      q: string;
    begin
      qDict := TQueryDictionary.create();

      if( DRTDaily = drt ) then
        begin
          table := 'outDailyByProductionType';

          // These fields are only in the daily output table
          //-------------------------------------------------
          qDict['day'] := intToStr( day );

          // Daily numbers for each disease state
          qDict['tsdUSusc'] := intToStr( tsdUSusc );
          qDict['tsdASusc'] := intToStr( tsdASusc );
          qDict['tsdULat']  := intToStr( tsdULat );
          qDict['tsdALat']  := intToStr( tsdALat );
          qDict['tsdUSubc'] := intToStr( tsdUSubc );
          qDict['tsdASubc'] := intToStr( tsdASubc );
          qDict['tsdUClin'] := intToStr( tsdUClin );
          qDict['tsdAClin'] := intToStr( tsdAClin );
          qDict['tsdUNImm'] := intToStr( tsdUNImm );
          qDict['tsdANImm'] := intToStr( tsdANImm );
          qDict['tsdUVImm'] := intToStr( tsdUVImm );
          qDict['tsdAVImm'] := intToStr( tsdAVImm );
          qDict['tsdUDest'] := intToStr( tsdUDest );
          qDict['tsdADest'] := intToStr( tsdADest );

          // New daily counts for cause of infection
          qDict['infnUAir'] := intToStr( infnUAir );
          qDict['infnAAir'] := intToStr( infnAAir );
          qDict['infnUDir'] := intToStr( infnUDir );
          qDict['infnADir'] := intToStr( infnADir );
          qDict['infnUInd'] := intToStr( infnUInd );
          qDict['infnAInd'] := intToStr( infnAInd );

          // New daily counts for detection
          qDict['detnUClin'] := intToStr( detnUClin );
          qDict['detnAClin'] := intToStr( detnAClin );

          // New daily counts for tracing
          qDict['trnUDir'] := intToStr( trnUDir );
          qDict['trnADir'] := intToStr( trnADir );
          qDict['trnUInd'] := intToStr( trnUInd );
          qDict['trnAInd'] := intToStr( trnAInd );

          // New daily counts for destruction for any reason
          qDict['desnUAll'] := intToStr( desnUAll );
          qDict['desnAAll'] := intToStr( desnAAll );

          // New daily counts for vaccination for any reason
          qDict['vaccnUAll'] := intToStr( vaccnUAll );
          qDict['vaccnAAll'] := intToStr( vaccnAAll );

          // New daily counts for zone foci
          qDict['zonnFoci'] := intToStr( zonnFoci );

          // Number of apparently infectious units on this day
          qDict['appUInfectious'] := intToStr( appUInfectious );
        end
      else if( DRTIteration = drt ) then
        begin
          table := 'outIterationByProductionType';

          // These fields are only in the iteration output table
          //----------------------------------------------------
          if( -1 <> firstDetection ) then qDict['firstDetection']     := intToStr( firstDetection );
          if( -1 <> firstDestruction ) then qDict['firstDestruction'] := intToStr( firstDestruction );
          if( -1 <> firstVaccination ) then qDict['firstvaccination'] := intToStr( firstVaccination );
        end
      else
        raise exception.Create( 'Unrecognized TDailyRecordType in TSMSimOutByProdType.updateDatabase' )
      ;

      // These fields are in both the daily and iteration output tables
      //----------------------------------------------------------------
      qDict['iteration'] := intToStr( iteration ); // Remember that iterations are 1-indexed in the database
      qDict['productionTypeID'] := intToStr( ptID );

      // Running totals for each disease state
      qDict['tscUSusc'] := intToStr( tscUSusc );
      qDict['tscASusc'] := intToStr( tscASusc );
      qDict['tscULat']  := intToStr( tscULat );
      qDict['tscALat']  := intToStr( tscALat );
      qDict['tscUSubc'] := intToStr( tscUSubc );
      qDict['tscASubc'] := intToStr( tscASubc );
      qDict['tscUClin'] := intToStr( tscUClin );
      qDict['tscAClin'] := intToStr( tscAClin );
      qDict['tscUNImm'] := intToStr( tscUNImm );
      qDict['tscANImm'] := intToStr( tscANImm );
      qDict['tscUVImm'] := intToStr( tscUVImm );
      qDict['tscAVImm'] := intToStr( tscAVImm );
      qDict['tscUDest'] := intToStr( tscUDest );
      qDict['tscADest'] := intToStr( tscADest );

      // Running totals for cause of infection
      qDict['infcUIni'] := intToStr( infcUIni );
      qDict['infcAIni'] := intToStr( infcAIni );
      qDict['infcUAir'] := intToStr( infcUAir );
      qDict['infcAAir'] := intToStr( infcAAir );
      qDict['infcUDir'] := intToStr( infcUDir );
      qDict['infcADir'] := intToStr( infcADir );
      qDict['infcUInd'] := intToStr( infcUInd );
      qDict['infcAInd'] := intToStr( infcAInd );

      // Running totals for exposures
      qDict['expcUDir'] := intToStr( expcUDir );
      qDict['expcADir'] := intToStr( expcADir );
      qDict['expcUInd'] := intToStr( expcUInd );
      qDict['expcAInd'] := intToStr( expcAInd );

      // Running totals for traces
      qDict['trcUDir']  := intToStr( trcUDir );
      qDict['trcADir']  := intToStr( trcADir );
      qDict['trcUInd']  := intToStr( trcUInd );
      qDict['trcAInd']  := intToStr( trcAInd );
      qDict['trcUDirp'] := intToStr( trcUDirp );
      qDict['trcADirp'] := intToStr( trcADirp );
      qDict['trcUIndp'] := intToStr( trcUIndp );
      qDict['trcAIndp'] := intToStr( trcAIndp );

      // Running totals for detection
      qDict['detcUClin'] := intToStr( detcUClin );
      qDict['detcAClin'] := intToStr( detcAClin );

      // Running totals for destruction
      qDict['descUIni']  := intToStr( descUIni );
      qDict['descAIni']  := intToStr( descAIni );
      qDict['descUDet']  := intToStr( descUDet );
      qDict['descADet']  := intToStr( descADet );
      qDict['descUDir']  := intToStr( descUDir );
      qDict['descADir']  := intToStr( descADir );
      qDict['descUInd']  := intToStr( descUInd );
      qDict['descAInd']  := intToStr( descAInd );
      qDict['descURing'] := intToStr( descURing );
      qDict['descARing'] := intToStr( descARing );

      // Running totals for vaccination
      qDict['vaccUIni'] := intToStr( vaccUIni );
      qDict['vaccAIni'] := intToStr( vaccAIni );
      qDict['vaccURing'] := intToStr( vaccURing );
      qDict['vaccARing'] := intToStr( vaccARing );

      // Running totals for zone foci
      qDict['zoncFoci'] := intToStr( zoncFoci );

      // Outputs need to be stored at least temporarily in the local database,
      // even if the user wants to record outputs in a remote database.
      // The query has to be written twice: once for each database.
      q := sqlclasses.writeQuery( table, QInsert, qDict );

      db.execute( q );

      if
        ( ( DRTDaily = drt ) and ( db.saveAllDailyOutputs() ) )
      or
        ( DRTIteration = drt )
      then
        begin
          if( remoteDBParams.useRemoteDatabase ) then
            begin
              qDict['jobID'] := intToStr( remoteDBParams.jobID );
              q := sqlclasses.writeQuery( table, QInsert, qDict );
              db.remoteExecute( q );
            end
          ;
        end
      ;

      qDict.free();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Debugging
//-----------------------------------------------------------------------------
	procedure TSMSimOutByProdType.debug();
  	begin
    	dbcout( '----------------begin TSMSimOutByProdType', true );

      // Daily numbers for each disease state
      dbcout( 'tsdUSusc: ' + intToStr( tsdUSusc ), true  );
      dbcout( 'tsdASusc: ' + intToStr( tsdASusc ), true  );
      dbcout( 'tsdULat: ' + intToStr( tsdULat ), true  );
      dbcout( 'tsdALat: ' + intToStr( tsdALat ), true  );
      dbcout( 'tsdUSubc: ' + intToStr( tsdUSubc ), true  );
      dbcout( 'tsdASubc: ' + intToStr( tsdASubc ), true  );
      dbcout( 'tsdUClin: ' + intToStr( tsdUClin ), true  );
      dbcout( 'tsdAClin: ' + intToStr( tsdAClin ), true  );
      dbcout( 'tsdUNImm: ' + intToStr( tsdUNImm ), true  );
      dbcout( 'tsdANImm: ' + intToStr( tsdANImm ), true  );
      dbcout( 'tsdUVImm: ' + intToStr( tsdUVImm ), true  );
      dbcout( 'tsdAVImm: ' + intToStr( tsdAVImm ), true  );
      dbcout( 'tsdUDest: ' + intToStr( tsdUDest ), true  );
      dbcout( 'tsdADest: ' + intToStr( tsdADest ), true  );

      // Running totals for each disease state
      dbcout( 'tscUSusc: ' + intToStr( tscUSusc ), true  );
      dbcout( 'tscASusc: ' + intToStr( tscASusc ), true  );
      dbcout( 'tscULat: ' + intToStr( tscULat ), true  );
      dbcout( 'tscALat: ' + intToStr( tscALat ), true  );
      dbcout( 'tscUSubc: ' + intToStr( tscUSubc ), true  );
      dbcout( 'tscASubc: ' + intToStr( tscASubc ), true  );
      dbcout( 'tscUClin: ' + intToStr( tscUClin ), true  );
      dbcout( 'tscAClin: ' + intToStr( tscAClin ), true  );
      dbcout( 'tscUNImm: ' + intToStr( tscUNImm ), true  );
      dbcout( 'tscANImm: ' + intToStr( tscANImm ), true  );
      dbcout( 'tscUVImm: ' + intToStr( tscUVImm ), true  );
      dbcout( 'tscAVImm: ' + intToStr( tscAVImm ), true  );
      dbcout( 'tscUDest: ' + intToStr( tscUDest ), true  );
      dbcout( 'tscADest: ' + intToStr( tscADest ), true  );

      // Running totals for cause of infection
      dbcout( 'infcUIni: ' + intToStr( infcUIni ), true  );
      dbcout( 'infcAIni: ' + intToStr( infcAIni ), true  );
      dbcout( 'infcUAir: ' + intToStr( infcUAir ), true  );
      dbcout( 'infcAAir: ' + intToStr( infcAAir ), true  );
      dbcout( 'infcUDir: ' + intToStr( infcUDir ), true  );
      dbcout( 'infcADir: ' + intToStr( infcADir ), true  );
      dbcout( 'infcUInd: ' + intToStr( infcUInd ), true  );
      dbcout( 'infcAInd: ' + intToStr( infcAInd ), true  );

      // New daily counts for cause of infection
      dbcout( 'infnUAir: ' + intToStr( infnUAir ), true  );
      dbcout( 'infnAAir: ' + intToStr( infnAAir ), true  );
      dbcout( 'infnUDir: ' + intToStr( infnUDir ), true  );
      dbcout( 'infnADir: ' + intToStr( infnADir ), true  );
      dbcout( 'infnUInd: ' + intToStr( infnUInd ), true  );
      dbcout( 'infnAInd: ' + intToStr( infnAInd ), true  );

      // Running totals for exposures
      dbcout( 'expcUDir: ' + intToStr( expcUDir ), true  );
      dbcout( 'expcADir: ' + intToStr( expcADir ), true  );
      dbcout( 'expcUInd: ' + intToStr( expcUInd ), true  );
      dbcout( 'expcAInd: ' + intToStr( expcAInd ), true  );

      // Running totals for traces
      dbcout( 'trcUDir: ' + intToStr( trcUDir ), true  );
      dbcout( 'trcADir: ' + intToStr( trcADir ), true  );
      dbcout( 'trcUInd: ' + intToStr( trcUInd ), true  );
      dbcout( 'trcAInd: ' + intToStr( trcAInd ), true  );
      dbcout( 'trcUDirp: ' + intToStr( trcUDirp ), true  );
      dbcout( 'trcADirp: ' + intToStr( trcADirp ), true  );
      dbcout( 'trcUIndp: ' + intToStr( trcUIndp ), true  );
      dbcout( 'trcAIndp: ' + intToStr( trcAIndp ), true  );

      // New daily counts for detection
      dbcout( 'detnUClin: ' + intToStr( detnUClin ), true  );
      dbcout( 'detnAClin: ' + intToStr( detnAClin ), true  );

      // New daily counts for tracing
      dbcout( 'trnUDir: ' + intToStr( trnUDir ), true  );
      dbcout( 'trnADir: ' + intToStr( trnADir ), true  );
      dbcout( 'trnUInd: ' + intToStr( trnUInd ), true  );
      dbcout( 'trnAInd: ' + intToStr( trnAInd ), true  );

      // New daily counts for destruction for any reason
      dbcout( 'desnUAll: ' + intToStr( desnUAll ), true  );
      dbcout( 'desnAAll: ' + intToStr( desnAAll ), true  );

      // New daily counts for vaccination for any reason
      dbcout( 'vaccnUAll: ' + intToStr( vaccnUAll ), true  );
      dbcout( 'vaccnAAll: ' + intToStr( vaccnAAll ), true  );

      // Running totals for detection
      dbcout( 'detcUClin: ' + intToStr( detcUClin ), true  );
      dbcout( 'detcAClin: ' + intToStr( detcAClin ), true  );

      // Running totals for destruction
      dbcout( 'descUIni: ' + intToStr( descUIni ), true  );
      dbcout( 'descAIni: ' + intToStr( descAIni ), true  );
      dbcout( 'descUDet: ' + intToStr( descUDet ), true  );
      dbcout( 'descADet: ' + intToStr( descADet ), true  );
      dbcout( 'descUDir: ' + intToStr( descUDir ), true  );
      dbcout( 'descADir: ' + intToStr( descADir ), true  );
      dbcout( 'descUInd: ' + intToStr( descUInd ), true  );
      dbcout( 'descAInd: ' + intToStr( descAInd ), true  );
      dbcout( 'descURing: ' + intToStr( descURing ), true  );
      dbcout( 'descARing: ' + intToStr( descARing ), true  );

      // Running totals for vaccination
      dbcout( 'vaccUIni: ' + intToStr( vaccUIni ), true );
      dbcout( 'vaccAIni: ' + intToStr( vaccAIni ), true );
      dbcout( 'vaccURing: ' + intToStr( vaccURing ), true  );
      dbcout( 'vaccARing: ' + intToStr( vaccARing ), true  );

      // Running totals for zone foci
      dbcout( 'zoncFoci: ' + intTOStr( zoncFoci ), true );

      dbcout( 'firstDetection: ' + intToStr( firstDetection ), true );
      dbcout( 'firstDestruction: ' + intToStr( firstDestruction ), true );
      dbcout( 'firstVaccination: ' + intToStr( firstVaccination ), true );

			dbcout( '----------------end TSMSimOutByProdType' + endl, true );
    end
  ;


	procedure TSMSimOutByProdType.debugSel();
  	begin
    	dbcout( endl + '----------------begin TSMSimOutByProdType', true );
      dbcout( 'firstDetection: ' + intToStr( firstDetection ), true );
      dbcout( 'firstDestruction: ' + intToStr( firstDestruction ), true );
      dbcout( 'firstVaccination: ' + intToStr( firstVaccination ), true );
			dbcout( '----------------end TSMSimOutByProdType' + endl, true );
    end
  ;
//-----------------------------------------------------------------------------


end.
