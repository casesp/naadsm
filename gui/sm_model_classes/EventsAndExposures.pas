unit EventsAndExposures;

(*
EventsAndExposures.pas
----------------------
Begin: 2005/09/01
Last revision: $Date: 2013-06-27 19:11:34 $ $Author: areeves $
Version number: $Revision: 1.24.4.6 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2005 - 2013 NAADSM Development Team

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
    Contnrs,

    StatusEnums,
    NAADSMLibraryTypes,
    SMDatabase,

    Herd
  ;

  type TEventCode = (
    EVTUnspecified,
    EVTTransistionStateChange,
    EVTTraceForwardDirect,
    EVTTraceForwardIndirect,
    EVTTraceBackDirect,
    EVTTraceBackIndirect,
    EVTDestroyed,
    EVTVaccinated,
    EVTInfected,
    EVTDetected,
    EVTZoneFocus,
    EVTZoneChanged,
    EVTHerdExam,
    EVTDiagnosticTest
  );

  // Helper functions for event codes
  //---------------------------------
  function firstEventCode(): TEventCode;
  function lastEventCode(): TEventCode;
  function eventCodeChar( const code: TEventCode ): char;
  function eventCodeString( const code: TEventCode ): string;
  function eventFromCode( const c: char ): TEventCode;

  function eventIsTrace( const code: TEventCode ): boolean;
  function eventIsTest( const code: TEventCode ): boolean;
  function eventIsStateChange( const code: TEventCode ): boolean;


  type TSMEvent = class
    protected
      _eventID: integer;
      _iteration: integer;
      _day: integer;
      _herdID: integer;
      _zoneID: integer;
      _eventCode: TEventCode;
      _newStatus: TNAADSMDiseaseState;
      _traceSuccess: TNAADSMSuccess;
      _testResult: TNAADSMTestResult;

      function getEventCodeString(): string;

    public
      constructor create(
        event: integer;
        iteration: integer;
        day: integer;
        herdID: integer;
        zOneID: integer;
        eventCode: TEventCode;
        newStatus: TNAADSMDiseaseState = NAADSMStateUnspecified;
        traceSuccess: TNAADSMSuccess = NAADSMSuccessUnspecified;
        testResult: TNAADSMTestResult = NAADSMTestUnspecified
      );

      procedure debug();

      property eventID: integer read _eventID;
      property iteration: integer read _iteration;
      property day: integer read _day;
      property herdID: integer read _herdID;
      property zoneID: integer read _zoneID;
      property eventCode: TEventCode read _eventCode;
      property eventCodeString: string read getEventCodeString;
      property newStatus: TNAADSMDiseaseState read _newStatus;
      property traceSuccess: TNAADSMSuccess read _traceSuccess;
      property testResult: TNAADSMTestResult read _testResult;
    end
  ;


  type TSMExposureOrTrace = class
    protected
      _isExposure: boolean;
      _exposureID: integer;
      _iteration: integer;
      _day: integer;
      _initiatedDay: integer;
      _exposingHerdID: integer;
      _exposingHerdStatus: TNAADSMDiseaseState;
      _exposedHerdID: integer;
      _exposedHerdStatus: TNAADSMDiseaseState;
      _exposingZoneID: integer;
      _exposedZoneID: integer;
      _method: string;
      _isAdequate: boolean;

      function getIsTrace(): boolean;
      procedure setIsTrace( val: boolean );

    public
      constructor create(
        const exposureID: integer;
        const iteration: integer;
        const e: THRDExpose;
        const exposingHerdID: integer;
        const exposedHerdID: integer;
        const exposingZoneID: integer;
        const exposedZoneID: integer
      ); overload;

      constructor create(
        const exposureID: integer;
        const iteration: integer;
        const t: THRDTrace;
        const hList: THerdList;
        const originZoneID: integer;
        const identifiedZoneID: integer
      ); overload;

      procedure debug();

      class function getExposureCodeString( const code: string ): string;

      property isExposure: boolean read _isExposure write _isExposure;
      property isTrace: boolean read getIsTrace write setIsTrace;

      property exposureID: integer read _exposureID;
      property iteration: integer read _iteration;
      property day: integer read _day;
      property initiatedDay: integer read _initiatedDay;
      property exposingHerdID: integer read _exposingHerdID;
      property exposingHerdStatus: TNAADSMDiseaseState read _exposingHerdStatus;
      property exposedZoneID: integer read _exposedZoneID;
      property exposedHerdID: integer read _exposedHerdID;
      property exposedHerdStatus: TNAADSMDiseaseState read _exposedHerdStatus;
      property exposingZoneID: integer read _exposingZoneID;
      property method: string read _method;
      property isAdequate: boolean read _isAdequate;
    end
  ;



  type TSMEventList = class( TObjectList )
    protected
      _currentIndex: integer;

      procedure setObject( index: integer; item: TSMEvent );
      function getObject( index: integer ): TSMEvent;

      function getCurrentIndex(): integer;

    public
    	constructor create(); overload;

      destructor destroy(); override;

      procedure populateDatabase( db: TSMDatabase );

      function first(): TSMEvent;
      function last(): TSMEvent;
      function next(): TSMEvent;
      function current(): TSMEvent;
      function at( val: integer ): TSMEvent;

      function append( dm: TSMEvent ): integer;
      procedure insert( index: integer; dm: TSMEvent );
      property objects[ index: integer]: TSMEvent read getObject write setObject; default;
      property currentPosition: integer read getCurrentIndex;
    end
  ;


  type TSMExposureOrTraceList = class( TObjectList )
    protected
      _currentIndex: integer;

      procedure setObject( index: integer; item: TSMExposureOrTrace );
      function getObject( index: integer ): TSMExposureOrTrace;

      function getCurrentIndex(): integer;

    public
    	constructor create(); overload;

      destructor destroy(); override;

      procedure populateDatabase( db: TSMDatabase );

      function first(): TSMExposureOrTrace;
      function last(): TSMExposureOrTrace;
      function next(): TSMExposureOrTrace;
      function current(): TSMExposureOrTrace;
      function at( val: integer ): TSMExposureOrTrace;

      function append( dm: TSMExposureOrTrace ): integer;
      procedure insert( index: integer; dm: TSMExposureOrTrace );
      property objects[ index: integer]: TSMExposureOrTrace read getObject write setObject; default;
      property currentPosition: integer read getCurrentIndex;
    end
  ;

implementation

  uses
    SysUtils,
    Forms, // for Application object
    Variants,

    MyStrUtils,
    DebugWindow,
    QStringMaps,
    I88n
  ;


//-----------------------------------------------------------------------------
// Helper functions for event codes
//-----------------------------------------------------------------------------
  function firstEventCode(): TEventCode;
    begin
      result := EVTTransistionStateChange;
    end
  ;


  function lastEventCode(): TEventCode;
    begin
      result := EVTDiagnosticTest;
    end
  ;


  function eventIsTrace( const code: TEventCode ): boolean;
    begin
      result := ( code in [ EVTTraceForwardDirect, EVTTraceForwardIndirect, EVTTraceBackDirect, EVTTraceBackIndirect ] );
    end
  ;


  function eventIsTest( const code: TEventCode ): boolean;
    begin
      result := ( code = EVTDiagnosticTest );
    end
  ;

  function eventIsStateChange( const code: TEventCode ): boolean;
    begin
      result := ( code = EVTTransistionStateChange );
    end
  ;

  function eventCodeChar( const code: TEventCode ): char;
    begin
      case code of
        EVTUnspecified: result := char( 0 );
        EVTTransistionStateChange: result := 'R';
        EVTTraceForwardDirect: result := 'T';
        EVTTraceForwardIndirect: result := 'I';
        EVTTraceBackDirect: result := 'K';
        EVTTraceBackIndirect: result := 'J';
        EVTDestroyed: result := 'D';
        EVTVaccinated: result := 'V';
        EVTInfected: result := 'F';
        EVTDetected: result := 'E';
        EVTZoneFocus: result := 'Z';
        EVTZoneChanged: result := 'C';
        EVTHerdExam: result := 'M';
        EVTDiagnosticTest: result := 'S';
        else
          begin
            raise exception.Create( 'Unrecognized code in eventCodeChar' );
            result := char( 0 );
          end
        ;
      end;
    end
  ;

  function eventFromCode( const c: char ): TEventCode;
    begin
      if( 'R' = c ) then result := EVTTransistionStateChange
      else if( 'T' = c ) then result := EVTTraceForwardDirect
      else if( 'I' = c ) then result := EVTTraceForwardIndirect
      else if( 'K' = c ) then result := EVTTraceBackDirect
      else if( 'J' = c ) then result := EVTTraceBackIndirect
      else if( 'D' = c ) then result := EVTDestroyed
      else if( 'V' = c ) then result := EVTVaccinated
      else if( 'F' = c ) then result := EVTInfected
      else if( 'E' = c ) then result := EVTDetected
      else if( 'Z' = c ) then result := EVTZoneFocus
      else if( 'C' = c ) then result := EVTZoneChanged
      else if( 'M' = c ) then result := EVTHerdExam
      else if( 'S' = c ) then result := EVTDiagnosticTest
      else
        begin
          raise exception.Create( 'Unrecognized code (' + c + ') in eventFromCode' );
          result := EVTUnspecified;
        end
      ;
    end
  ;

  function eventCodeString( const code: TEventCode ): string;
    begin
      case code of
        EVTTransistionStateChange: result := tr( 'State change' );
        EVTTraceForwardDirect: result := tr( 'Trace forward of direct contact' );
        EVTTraceForwardIndirect: result := tr( 'Trace forward of indirect contact' );
        EVTTraceBackDirect: result := tr( 'Trace back of direct contact' );
        EVTTraceBackIndirect: result := tr( 'Trace back of indirect contact' );
        EVTDestroyed: result := tr( 'Destruction' );
        EVTVaccinated: result :=  tr( 'Vaccination' );
        EVTInfected: result := tr( 'Infection' );
        EVTDetected: result := tr( 'Detection' );
        EVTZoneFocus: result := tr( 'Creation of zone focus' );
        EVTZoneChanged: result := tr( 'Zone change' );
        EVTHerdExam: result := tr( 'Herd exam' );
        EVTDiagnosticTest: result := tr( 'Diagnostic test' );
        else
          begin
            raise exception.create( 'Unrecognized event code (' + intToStr( integer( code ) ) + ') in TSMEvent.getEventCodeString()' );
            result := '';
          end
        ;
      end;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TSMEvent: construction and debugging
//-----------------------------------------------------------------------------
  constructor TSMEvent.create(
        event: integer;
        iteration: integer;
        day: integer;
        herdID: integer;
        zoneID: integer;
        eventCode: TEventCode;
        newStatus: TNAADSMDiseaseState = NAADSMStateUnspecified;
        traceSuccess: TNAADSMSuccess = NAADSMSuccessUnspecified;
        testResult: TNAADSMTestResult = NAADSMTestUnspecified
      );
    var
      str: string;
    begin
      inherited create();

      str := '';

      _eventID := event;
      _iteration := iteration;
      _day := day;
      _herdID := herdID;
      _zoneID := zoneID;
      _eventCode := eventCode;

      if( eventIsStateChange( eventCode ) ) then
        begin
          if( NAADSMStateUnspecified = newStatus ) then
            raise exception.create( 'Unspecified disease state in TSMEvent.create()' )
          ;
          _newStatus := newStatus;
          str := 'New status: ' + naadsmDiseaseStateStr( newStatus );
        end
      ;

      if( eventIsTrace( eventCode ) ) then
        begin
          if( NAADSMSuccessUnspecified = traceSuccess ) then
            raise exception.Create( 'Unspecified trace success in TSMEvent.create()' )
          ;
          _traceSuccess := traceSuccess;
          str := 'Trace success: ' + naadsmSuccessStr( traceSuccess );
        end
      ;

      if( eventIsTest( eventCode ) ) then
        begin
          if( NAADSMTestUnspecified = testResult ) then
            raise exception.Create( 'Unspecified test result in TSMEvent.create()' )
          ;
          _testResult := testResult;
          str := 'Test result: ' + naadsmTestResultStr( testResult );
        end
      ;

      (*
      dbcout2( endl + '---Event created: ID ' + intToStr( _eventID ) + ', code: ' + EventsAndExposures.eventCodeString( _eventCode ) );
      dbcout2( 'Iteration: ' + intToStr( _iteration ) + ', day: ' + intToStr( _day ) );
      dbcout2( 'Herd ID: ' + intToStr( _herdID ) + ', zone ID: ' + intToStr( _zoneID ) );
      if( 0 < length( str ) ) then
        dbcout2( str )
      ;
      dbcout2( '---end event' );
      *)
    end
  ;


  procedure TSMEvent.debug();
    var
      str: string;
    begin
      str := '';

      if( eventIsStateChange( _eventCode ) ) then
        str := 'New status: ' + naadsmDiseaseStateStr( _newStatus )
      ;

      if( eventIsTrace( _eventCode ) ) then
        str := 'Trace success: ' + naadsmSuccessStr( _traceSuccess )
      ;

      if( eventIsTest( eventCode ) ) then
        str := 'Test result: ' + naadsmTestResultStr( _testResult )
      ;

      dbcout( endl + '---Event created: ID ' + intToStr( _eventID ) + ', code: ' + EventsAndExposures.eventCodeString( _eventCode ), true );
      dbcout( 'Iteration: ' + intToStr( _iteration ) + ', day: ' + intToStr( _day ), true );
      dbcout( 'Herd ID: ' + intToStr( _herdID ) + ', zone ID: ' + intToStr( _zoneID ), true );
      if( 0 < length( str ) ) then
        dbcout( str, true )
      ;
      dbcout( '---end event', true );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TSMEvent: conversions
//-----------------------------------------------------------------------------
  function TSMEvent.getEventCodeString(): string;
    begin
      result := EventsAndExposures.eventCodeString( _eventCode );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TSMExposure: construction
//-----------------------------------------------------------------------------
  constructor TSMExposureOrTrace.create(
        const exposureID: integer;
        const iteration: integer;
        const e: THRDExpose;
        const exposingHerdID: integer;
        const exposedHerdID: integer;
        const exposingZoneID: integer;
        const exposedZoneID: integer
      );
    begin
      dbcout2( endl + '### Exposure' + endl );

      isExposure := true;
      _exposureID := exposureID;
      _iteration := iteration;
      _day := e.finalizedDay;
      _initiatedDay := e.initiatedDay;
      _exposingHerdStatus := e.srcStatus;
      _exposedHerdStatus := e.destStatus;
      _exposingHerdID := exposingHerdID;
      _exposedHerdID := exposedHerdID;
      _exposingZoneID := exposingZoneID;
      _exposedZoneID := exposedZoneID;

      case e.exposureMethod of
        NAADSMDirectContact: _method := 'D';
        NAADSMIndirectContact: _method := 'I';
        NAADSMAirborneSpread: _method := 'A';
        NAADSMInitiallyInfected: _method := 'N';
        else raise exception.Create( 'Unrecognized exposure method in TSMExposure.create()' );
      end;

      _isAdequate := naadsmSuccessIsTrue( e.isAdequate );
    end
  ;


  constructor TSMExposureOrTrace.create(
        const exposureID: integer;
        const iteration: integer;
        const t: THRDTrace;
        const hList: THerdList;
        const originZoneID: integer;
        const identifiedZoneID: integer
      );
    begin
      dbcout2( endl + '### Trace' + endl );

      isTrace := true;
      _exposureID := exposureID;
      _iteration := iteration;
      _day := t.day;
      _initiatedDay := t.day;

      _exposingHerdID := hList.at( t.originIndex ).id;
      _exposingHerdStatus := hList.at( t.originIndex ).diseaseStatus;

      _exposedHerdID := hList.at( t.identifiedIndex ).id;
      _exposedHerdStatus := hList.at( t.identifiedIndex ).diseaseStatus;

      _exposingZoneID := originZoneID;
      _exposedZoneID := identifiedZoneID;

      case t.contactType of
        NAADSMDirectContact:
          begin
            case t.traceType of
              NAADSMTraceForwardOrOut: _method := 'TFD';
              NAADSMTraceBackOrIn: _method := 'TBD';
              else raise exception( 'Unsupported trace type (' + intToStr( ord( t.contactType ) ) + ') in TSMExposureOrTrace.create()' );
            end;
          end
        ;
        NAADSMIndirectContact:
          begin
            case t.traceType of
              NAADSMTraceForwardOrOut: _method := 'TFI';
              NAADSMTraceBackOrIn: _method := 'TBI';
              else raise exception( 'Unsupported trace type (' + intToStr( ord( t.contactType ) ) + ') in TSMExposureOrTrace.create()' );
            end;
          end
        ;
        else
          raise exception( 'Unsupported contact type (' + intToStr( ord( t.contactType ) ) + ') in TSMExposureOrTrace.create()' )
        ;
      end;

      _isAdequate := naadsmSuccessIsTrue( t.success );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TSMExposureOrTrace: debugging
//-----------------------------------------------------------------------------
  procedure TSMExposureOrTrace.debug();
    var
      s: string;
    begin
      s := 'TSMExposureOrTrace #' + intToStr( _exposureID ) + ' on day ' + intToStr( _day ) + ' of iteration ' + intToStr( _iteration ) + ':';
      if( _isExposure ) then
        begin
          s := s + ' (exposure) ' + _method;

          if( 0 = day ) then
            s := s + ' source ' + intToStr( _exposingHerdID ) + ' (no dz state) zone ' + intToStr( _exposingZoneID )
          else
            s := s + ' source ' + intToStr( _exposingHerdID ) + ' (' + naadsmDiseaseStateCode( _exposingHerdStatus ) + ') zone ' + intToStr( _exposingZoneID )
          ;
          s := s + ' dest ' + intToStr( _exposedHerdID ) + ' (' + naadsmDiseaseStateCode( _exposedHerdStatus ) + ') zone ' + intToStr( _exposedZoneID );
        end
      else
        begin
          if( _isAdequate ) then
            s := s + ' (successful trace) ' + _method
          else
            s := s + ' (unsuccessful trace) ' + _method
          ;
          s := s + ' origin ' + intToStr( _exposingHerdID ) + ' (' + naadsmDiseaseStateCode( _exposingHerdStatus ) + ') zone ' + intToStr( _exposingZoneID );
          s := s + ' identified ' + intToStr( _exposedHerdID ) + ' (' + naadsmDiseaseStateCode( _exposedHerdStatus ) + ') zone ' + intToStr( _exposedZoneID );
        end
      ;

      dbcout( s, true );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TSMExposureOrTrace: properties
//-----------------------------------------------------------------------------
  function TSMExposureOrTrace.getIsTrace(): boolean;
    begin
      result := not( _isExposure );
    end
  ;


  procedure TSMExposureOrTrace.setIsTrace( val: boolean );
    begin
      _isExposure := not( val );
    end
  ;


  class function TSMExposureOrTrace.getExposureCodeString( const code: string ): string;
    begin
      if( 'D' = code ) then result := tr( 'Direct contact' )
      else if( 'A' = code ) then result := tr( 'Airborne exposure' )
      else if( 'I' = code ) then result := tr( 'Indirect contact' )
      else if( 'TFD' = code ) then result := tr( 'Trace forward, direct' )
      else if( 'TFI' = code ) then result := tr( 'Trace forward, indirect' )
      else if( 'TBD' = code ) then result := tr( 'Trace back, direct' )
      else if( 'TBI' = code ) then result := tr( 'Trace back, indirect' )
      else
        begin
          raise exception.create( 'Unrecognized exposure code (' + code + ') in TSMExposureOrTrace.getExposureCodeString' );
          result := tr( 'Unknown' );
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TSMEventList: construction/destruction
//-----------------------------------------------------------------------------
	constructor TSMEventList.create();
		begin
      // This list owns its objects, and will automatically delete them.
   		inherited create( true );
    end
  ;


  destructor TSMEventList.destroy();
    begin
      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TSMEventList: database population
//-----------------------------------------------------------------------------
  procedure TSMEventList.populateDatabase( db: TSMDatabase );
    var
      vDict: TQStringVariantMap;
      i: integer;
      e: TSMEvent;
  	begin
      vDict := TQStringVariantMap.Create();

      db.startQuickInsert( 'outDailyEvents' );

      for i := 0 to self.count - 1 do
        begin
          e := self.at( i );

          vDict.clear();
          vDict['iteration'] := e.iteration;
          vDict['day'] := e.day;
          vDict['event'] := e.eventID;
          vDict['herdID'] := e.herdID;

          if( 0 < e.zoneID ) then
            vDict['zoneID'] := e.zoneID
          else
            vDict['zoneID'] := null
          ;

          vDict['eventCode'] := eventCodeChar( e.eventCode );

          if( eventIsStateChange( e.eventCode ) ) then
            vDict['newStateCode'] := naadsmDiseaseStateCode( e.newStatus )
          ;

          if( eventIsTrace( e.eventCode ) ) then
            vDict['traceSuccess'] := naadsmSuccessIsTrue( e.traceSuccess )
          ;

          if( eventIsTest( e.eventCode ) ) then
            vDict['testResultCode'] := naadsmTestResultCode( e.testResult )
          ;

          db.quickInsert( vDict );

          Application.ProcessMessages();
        end
      ;

      db.endQuickInsert();

      vDict.free();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TSMEventList: typical list functions
//-----------------------------------------------------------------------------
  function TSMEventList.getCurrentIndex(): integer;
  	begin
   		result := _currentIndex;
    end
  ;


  function TSMEventList.append( dm: TSMEvent ): integer;
    begin
      result := inherited Add( dm );
    end
  ;


  procedure TSMEventList.setObject( index: integer; item: TSMEvent );
    begin
      inherited SetItem( index, item );
    end
  ;


  function TSMEventList.getObject( index: integer ): TSMEvent;
    begin
      result := inherited GetItem( index ) as TSMEvent;
    end
  ;


  function TSMEventList.at( val: integer ):TSMEvent;
    begin
      result := inherited getItem( val ) as TSMEvent;
    end
  ;


  procedure TSMEventList.insert(index: integer; dm: TSMEvent);
    begin
      inherited Insert(index, dm);
    end
  ;


  function TSMEventList.first() : TSMEvent;
    begin
      _currentIndex := 0;
      if( self.Count = 0 ) then
      	result := nil
      else
      	result := getObject( _currentIndex )
      ;
    end
  ;


  function TSMEventList.last() : TSMEvent;
    begin
      if( self.Count = 0 ) then result := nil
      else
        begin
          _currentIndex := self.Count - 1;
          result := getObject( _currentIndex );
        end
      ;
    end
  ;


  function TSMEventList.next() : TSMEvent;
    begin
      _currentIndex := _currentIndex + 1;
      if( _currentIndex > (self.Count - 1) ) then
      	result := nil
      else
      	result := getObject( _currentIndex )
      ;
    end
  ;


  function TSMEventList.current() : TSMEvent;
    begin
      if( _currentIndex > (self.Count - 1) ) then
      	result := nil
      else
      	result := getObject( _currentIndex )
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TSMExposureOrTraceList: construction/destruction
//-----------------------------------------------------------------------------
	constructor TSMExposureOrTraceList.create();
		begin
      // This list owns its objects, and will automatically delete them.
   		inherited create( true );
    end
  ;


  destructor TSMExposureOrTraceList.destroy();
    begin
      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TSMExposureOrTraceList: database population
//-----------------------------------------------------------------------------
  procedure TSMExposureOrTraceList.populateDatabase( db: TSMDatabase );
    var
      vDict: TQStringVariantMap;
      i: integer;
      e: TSMExposureOrTrace;
  	begin
      vDict := TQStringVariantMap.Create();

      db.startQuickInsert( 'outDailyExposures' );

      for i := 0 to self.count - 1 do
        begin
          e := self.at( i );

          vDict.clear();

          vDict['iteration'] := e.iteration;
          vDict['day'] := e.day;

          if( -1 <> e.initiatedDay ) then
            vDict['initiatedDay'] := e.initiatedDay
          else
            vDict['initiatedDay'] := null
          ;

          vDict['exposure'] := e.exposureID;
          vDict['exposingHerdID'] := e.exposingHerdID;
          vDict['exposingHerdStatusCode'] := naadsmDiseaseStateCode( e.exposingHerdStatus );

          if( 0 < e.exposingZoneID ) then
            vDict['exposingZoneID'] := e.exposingZoneID
          else
            vDict['exposingZoneID'] := null
          ;

          vDict['exposedHerdID'] := e.exposedHerdID;
          vDict['exposedHerdStatusCode'] := naadsmDiseaseStateCode( e.exposedHerdStatus );

          if( 0 < e.exposedZoneID ) then
            vDict['exposedZoneID'] := e.exposedZoneID
          else
            vDict['exposedZoneID'] := null
          ;

          vDict['spreadMethodCode'] := e.method;
          vDict['isAdequate'] := e.isAdequate;

          db.quickInsert( vDict );

          Application.ProcessMessages();
        end
      ;

      db.endQuickInsert();

      vDict.free();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TSMExposureOrTraceList: typical list functions
//-----------------------------------------------------------------------------
  function TSMExposureOrTraceList.getCurrentIndex(): integer;
  	begin
   		result := _currentIndex;
    end
  ;


  function TSMExposureOrTraceList.append( dm: TSMExposureOrTrace ): integer;
    begin
      result := inherited Add( dm );
    end
  ;


  procedure TSMExposureOrTraceList.setObject( index: integer; item: TSMExposureOrTrace );
    begin
      inherited SetItem( index, item );
    end
  ;


  function TSMExposureOrTraceList.getObject( index: integer ): TSMExposureOrTrace;
    begin
      result := inherited GetItem( index ) as TSMExposureOrTrace;
    end
  ;


  function TSMExposureOrTraceList.at( val: integer ): TSMExposureOrTrace;
    begin
      result := inherited getItem( val ) as TSMExposureOrTrace;
    end
  ;

  procedure TSMExposureOrTraceList.insert(index: integer; dm: TSMExposureOrTrace);
    begin
      inherited Insert(index, dm);
    end
  ;


  function TSMExposureOrTraceList.first() : TSMExposureOrTrace;
    begin
      _currentIndex := 0;
      if( self.Count = 0 ) then
      	result := nil
      else
      	result := getObject( _currentIndex )
      ;
    end
  ;


  function TSMExposureOrTraceList.last() : TSMExposureOrTrace;
    begin
      if( self.Count = 0 ) then result := nil
      else
        begin
          _currentIndex := self.Count - 1;
          result := getObject( _currentIndex );
        end
      ;
    end
  ;


  function TSMExposureOrTraceList.next() : TSMExposureOrTrace;
    begin
      _currentIndex := _currentIndex + 1;
      if( _currentIndex > (self.Count - 1) ) then
      	result := nil
      else
      	result := getObject( _currentIndex )
      ;
    end
  ;


  function TSMExposureOrTraceList.current() : TSMExposureOrTrace;
    begin
      if( _currentIndex > (self.Count - 1) ) then
      	result := nil
      else
      	result := getObject( _currentIndex )
      ;
    end
  ;
//-----------------------------------------------------------------------------


end.
