unit EventsAndExposures;

(*
EventsAndExposures.pas
----------------------
Begin: 2005/09/01
Last revision: $Date: 2008/04/18 20:35:18 $ $Author: areeves $
Version number: $Revision: 1.18 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2005 - 2007 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
    Contnrs,

    StatusEnums,
    SMDatabase
  ;


  // FIX ME: Why are these types defined here?
  
  {*
  	Record type used with DLL, whenever something happens to a herd.
  }
  type THRDUpdate = record
      index: integer;
      status: integer;
      success: integer;
      msg: pchar;
    end
  ;


  {*
    Record type used with DLL, when a herd is exposed to infection
  }
  type THRDExpose = record
      srcIndex: integer;
      srcStatus: integer;
      destIndex: integer;
      destStatus: integer;
      success: integer;
      exposureMethod: pchar;
    end
  ;

  
  {*
    Record type used with DLL, when a herd's zone assignment changes
  }
  type THRDZone = record
      herdIndex: integer;
      zoneLevel: integer;
    end
  ;

  
  type TSMEvent = class
    protected
      _eventID: integer;
      _iteration: integer;
      _day: integer;
      _herdID: integer;
      _zoneID: integer;
      _eventCode: string;
      _newStatus: TTransitionState;
      _traceSuccess: boolean;

      procedure setEvent( val: integer );
      procedure setIteration( val: integer );
      procedure setDay( val: integer );
      procedure setHerdID( val: integer );
      procedure setZoneID( val: integer );
      procedure setEventCode( val: string );
      procedure setNewStatus( val: TTransitionState );
      procedure setTraceSuccess( val: boolean );

      function getEvent(): integer;
      function getIteration(): integer;
      function getDay(): integer;
      function getHerdID(): integer;
      function getZoneID(): integer;
      function getEventCode(): string;
      function getNewStatus(): TTransitionState;
      function getTraceSuccess(): boolean;
      function getEventCodeString2(): string;

    public
      constructor create(
        xEvent: integer;
        xIteration, xDay: integer;
        xHerdID: integer;
        xZOneID: integer;
        xEventCode: string;
        xNewStatus: TTransitionState = tsUnspecified;
        xTraceSuccess: boolean = false
      );

      class function getEventCodeString( const code: string ): string;

      property event: integer read getEvent write setEvent;
      property iteration: integer read getIteration write setIteration;
      property day: integer read getDay write setDay;
      property herdID: integer read getHerdID write setHerdID;
      property zoneID: integer read getZoneID write setZoneID;
      property eventCode: string read getEventCode write setEventCode;
      property eventCodeString: string read getEventCodeString2;
      property newStatus: TTransitionState read getNewStatus write setNewStatus;
      property traceSuccess: boolean read getTraceSuccess write setTraceSuccess;
    end
  ;


  type TSMExposure = class
    protected
      _exposureID: integer;
      _iteration: integer;
      _day: integer;
      _exposingHerdID: integer;
      _exposingHerdStatus: TTransitionState;
      _exposedHerdID: integer;
      _exposedHerdStatus: TTransitionState;
      _exposingZoneID: integer;
      _exposedZoneID: integer;
      _method: string;
      _success: boolean;

      procedure setExposureID( val: integer );
      procedure setIteration( val: integer );
      procedure setDay( val: integer );
      procedure setExposingHerdID( val: integer );
      procedure setExposingHerdStatus( val: TTransitionState );
      procedure setExposingZoneID( val: integer );
      procedure setExposedHerdID( val: integer );
      procedure setExposedHerdStatus( val: TTransitionState );
      procedure setExposedZoneID( val: integer );
      procedure setMethod( val: string );
      procedure setSuccess( val: boolean );

      function getExposureID(): integer;
      function getIteration(): integer;
      function getDay(): integer;
      function getExposingHerdID(): integer;
      function getExposingHerdStatus(): TTransitionState;
      function getExposingZoneID(): integer;
      function getExposedHerdID(): integer;
      function getExposedHerdStatus(): TTransitionState;
      function getExposedZoneID(): integer;
      function getMethod(): string;
      function getSuccess(): boolean;

    public
      constructor create(
        xExposureID: integer;
        xIteration: integer;
        xDay: integer;
        xExposingHerdID: integer;
        xExposingHerdStatus: TTransitionState;
        xExposingZoneID: integer;
        xExposedHerdID: integer;
        xExposedHerdStatus: TTransitionState;
        xExposedZoneID: integer;
        xMethod: string;
        xSuccess: boolean
      );

      class function getExposureCodeString( const code: string ): string;

      property exposureID: integer read getExposureID write setExposureID;
      property iteration: integer read getIteration write setIteration;
      property day: integer read getDay write setDay;
      property exposingHerdID: integer read getExposingHerdID write setExposingHerdID;
      property exposingHerdStatus: TTransitionState read getExposingHerdStatus write setExposingHerdStatus;
      property exposedZoneID: integer read getExposedZoneID write setExposedZoneID;
      property exposedHerdID: integer read getExposedHerdID write setExposedHerdID;
      property exposedHerdStatus: TTransitionState read getExposedHerdStatus write setExposedHerdStatus;
      property exposingZoneID: integer read getExposingZoneID write setExposingZoneID;
      property method: string read getMethod write setMethod;
      property success: boolean read getSuccess write setSuccess;
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


  type TSMExposureList = class( TObjectList )
    protected
      _currentIndex: integer;

      procedure setObject( index: integer; item: TSMExposure );
      function getObject( index: integer ): TSMExposure;

      function getCurrentIndex(): integer;

    public
    	constructor create(); overload;

      destructor destroy(); override;

      procedure populateDatabase( db: TSMDatabase );

      function first(): TSMExposure;
      function last(): TSMExposure;
      function next(): TSMExposure;
      function current(): TSMExposure;
      function at( val: integer ): TSMExposure;

      function append( dm: TSMExposure ): integer;
      procedure insert( index: integer; dm: TSMExposure );
      property objects[ index: integer]: TSMExposure read getObject write setObject; default;
      property currentPosition: integer read getCurrentIndex;
    end
  ;

implementation

  uses
    SysUtils,
    Forms, // for Application object
    Variants,

    QStringMaps,
    I88n
  ;

//-----------------------------------------------------------------------------
// TSMEvent: construction
//-----------------------------------------------------------------------------
  constructor TSMEvent.create(
        xEvent: integer;
        xIteration, xDay: integer;
        xHerdID: integer;
        xZoneID: integer;
        xEventCode: string;
        xNewStatus: TTransitionState = tsUnspecified;
        xTraceSuccess: boolean = false
      );
    begin
      setEvent( xEvent );
      setIteration( xIteration );
      setDay( xDay );
      setHerdID( xHerdID );
      setZoneID( xZoneID );
      setEventCode( xEventCode );
      setNewStatus( xNewStatus );
      setTraceSuccess( xTraceSuccess );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TSMEvent: conversions
//-----------------------------------------------------------------------------
  function TSMEvent.getEventCodeString2(): string;
    begin
      result := getEventCodeString( eventCode );
    end
  ;


  class function TSMEvent.getEventCodeString( const code: string ): string;
    begin
      if( code = EVT_TRANSITION_STATE_CHANGE ) then result := tr( 'State change' )
      else if( code = EVT_TRACED_DIRECT ) then result := tr( 'Trace-direct' )
      else if( code = EVT_TRACED_INDIRECT ) then result := tr( 'Trace-indirect' )
      else if( code = EVT_DESTROYED ) then result := tr( 'Destruction' )
      else if( code = EVT_VACCINATED ) then result :=  tr( 'Vaccination' )
      else if( code = EVT_INFECTED ) then result := tr( 'Infection' )
      else if( code = EVT_DETECTED ) then result := tr( 'Detection' )
      else if( code = EVT_ZONE_FOCUS ) then result := tr( 'Zone focus created' )
      else if( code = EVT_ZONE_CHANGED ) then result := tr( 'Zone change' )
      else
        begin
          raise exception.create( 'Unrecognized event code (' + code + ') in TSMEvent.getEventCodeString' );
          result := '';
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TSMEvent: properties
//-----------------------------------------------------------------------------
  procedure TSMEvent.setEvent( val: integer ); begin _eventID := val; end;
  procedure TSMEvent.setIteration( val: integer ); begin _iteration := val; end;
  procedure TSMEvent.setDay( val: integer ); begin _day := val; end;
  procedure TSMEvent.setHerdID( val: integer ); begin _herdID := val; end;
  procedure TSMEvent.setZoneID( val: integer ); begin _zoneID := val; end;
  procedure TSMEvent.setEventCode( val: string ); begin _eventCode := val; end;
  procedure TSMEvent.setNewStatus( val: TTransitionState ); begin _newStatus := val; end;
  procedure TSMEvent.setTraceSuccess( val: boolean ); begin _traceSuccess := val; end;

  function TSMEvent.getEvent(): integer; begin result := _eventID; end;
  function TSMEvent.getIteration(): integer; begin result := _iteration; end;
  function TSMEvent.getDay(): integer; begin result := _day; end;
  function TSMEvent.getHerdID(): integer; begin result := _herdID; end;
  function TSMEvent.getZoneID(): integer; begin result := _zoneID; end;
  function TSMEvent.getEventCode(): string; begin result := _eventCode; end;
  function TSMEvent.getNewStatus(): TTransitionState; begin result := _newStatus; end;
  function TSMEvent.getTraceSuccess(): boolean; begin result := _traceSuccess; end;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TSMExposure: construction
//-----------------------------------------------------------------------------
  constructor TSMExposure.create(
        xExposureID: integer;
        xIteration: integer;
        xDay: integer;
        xExposingHerdID: integer;
        xExposingHerdStatus: TTransitionState;
        xExposingZoneID: integer;
        xExposedHerdID: integer;
        xExposedHerdStatus: TTransitionState;
        xExposedZoneID: integer;
        xMethod: string;
        xSuccess: boolean
      );
    begin
      _exposureID := xExposureID;
      _iteration := xIteration;
      _day := xDay;
      _exposingHerdID := xExposingHerdID;
      _exposedHerdID := xExposedHerdID;
      _exposingHerdStatus := xExposingHerdStatus;
      _exposedHerdStatus := xExposedHerdStatus;
      _exposingZoneID := xExposingZoneID;
      _exposedZoneID := xExposedZoneID;
      _method := xMethod;
      _success := xSuccess;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TSMExposure: properties
//-----------------------------------------------------------------------------
  class function TSMExposure.getExposureCodeString( const code: string ): string;
    begin
      if( 'D' = code ) then result := tr( 'Direct contact' )
      else if( 'A' = code ) then result := tr( 'Airborne' )
      else if( 'I' = code ) then result := tr( 'Indirect contact' )
      else
        begin
          raise exception.create( 'Unrecognized exposure code (' + code + ') in TSMExposure.getExposureCodeString' );
          result := tr( 'Unknown' );
        end
      ;
    end
  ;


  procedure TSMExposure.setExposureID( val: integer ); begin _exposureID := val; end;
  procedure TSMExposure.setIteration( val: integer ); begin _iteration := val; end;
  procedure TSMExposure.setDay( val: integer ); begin _day := val; end;
  procedure TSMExposure.setExposingHerdID( val: integer ); begin _exposingHerdID := val; end;
  procedure TSMExposure.setExposingHerdStatus( val: TTransitionState ); begin _exposingHerdStatus := val; end;
  procedure TSMExposure.setExposingZoneID( val: integer ); begin _exposingZoneID := val; end;
  procedure TSMExposure.setExposedHerdID( val: integer ); begin _exposedHerdID := val; end;
  procedure TSMExposure.setExposedHerdStatus( val: TTransitionState ); begin _exposedHerdStatus := val; end;
  procedure TSMExposure.setExposedZoneID( val: integer ); begin _exposedZoneID := val; end;
  procedure TSMExposure.setMethod( val: string ); begin _method := val; end;
  procedure TSMExposure.setSuccess( val: boolean ); begin _success := val; end;

  function TSMExposure.getExposureID(): integer; begin result := _exposureID; end;
  function TSMExposure.getIteration(): integer; begin result := _iteration; end;
  function TSMExposure.getDay(): integer; begin result := _day; end;
  function TSMExposure.getExposingHerdID(): integer; begin result := _exposingHerdID; end;
  function TSMExposure.getExposingHerdStatus: TTransitionState; begin result := _exposingHerdStatus; end;
  function TSMExposure.getExposingZoneID(): integer; begin result := _exposingZoneID; end;
  function TSMExposure.getExposedHerdID(): integer; begin result := _exposedHerdID; end;
  function TSMExposure.getExposedHerdStatus: TTransitionState; begin result := _exposedHerdStatus; end;
  function TSMExposure.getExposedZoneID(): integer; begin result := _exposedZoneID; end;
  function TSMExposure.getMethod(): string; begin result := _method; end;
  function TSMExposure.getSuccess(): boolean; begin result := _success; end;
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
          vDict['event'] := e.event;
          vDict['herdID'] := e.herdID;

          if( 0 < e.zoneID ) then
            vDict['zoneID'] := e.zoneID
          else
            vDict['zoneID'] := null
          ;

          vDict['eventCode'] := e.eventCode;

          if( 'R' = e.eventCode ) then
            vDict['newStateCode'] := transitionStateCode( e.newStatus )
          ;

          if( ( 'T' = e.eventCode ) or ( 'I' = e.eventCode ) ) then
            vDict['traceSuccess'] := e.traceSuccess
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
// TSMExposureList: construction/destruction
//-----------------------------------------------------------------------------
	constructor TSMExposureList.create();
		begin
      // This list owns its objects, and will automatically delete them.
   		inherited create( true );
    end
  ;


  destructor TSMExposureList.destroy();
    begin
      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TSMEventList: database population
//-----------------------------------------------------------------------------
  procedure TSMExposureList.populateDatabase( db: TSMDatabase );
    var
      vDict: TQStringVariantMap;
      i: integer;
      e: TSMExposure;
  	begin
      vDict := TQStringVariantMap.Create();

      db.startQuickInsert( 'outDailyExposures' );

      for i := 0 to self.count - 1 do
        begin
          e := self.at( i );

          vDict.clear();
          vDict['iteration'] := e.iteration;
          vDict['day'] := e.day;
          vDict['exposure'] := e.exposureID;
          vDict['exposingHerdID'] := e.exposingHerdID;
          vDict['exposingHerdStatusCode'] := transitionStateCode( e.exposingHerdStatus );

          if( 0 < e.exposingZoneID ) then
            vDict['exposingZoneID'] := e.exposingZoneID
          else
            vDict['exposingZoneID'] := null
          ;

          vDict['exposedHerdID'] := e.exposedHerdID;
          vDict['exposedHerdStatusCode'] := transitionStateCode( e.exposedHerdStatus );

          if( 0 < e.exposedZoneID ) then
            vDict['exposedZoneID'] := e.exposedZoneID
          else
            vDict['exposedZoneID'] := null
          ;

          vDict['spreadMethodCode'] := e.method;
          vDict['success'] := e.success;

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
  function TSMExposureList.getCurrentIndex(): integer;
  	begin
   		result := _currentIndex;
    end
  ;


  function TSMExposureList.append( dm: TSMExposure ): integer;
    begin
      result := inherited Add( dm );
    end
  ;


  procedure TSMExposureList.setObject( index: integer; item: TSMExposure );
    begin
      inherited SetItem( index, item );
    end
  ;


  function TSMExposureList.getObject( index: integer ): TSMExposure;
    begin
      result := inherited GetItem( index ) as TSMExposure;
    end
  ;


  function TSMExposureList.at( val: integer ): TSMExposure;
    begin
      result := inherited getItem( val ) as TSMExposure;
    end
  ;

  procedure TSMExposureList.insert(index: integer; dm: TSMExposure);
    begin
      inherited Insert(index, dm);
    end
  ;


  function TSMExposureList.first() : TSMExposure;
    begin
      _currentIndex := 0;
      if( self.Count = 0 ) then
      	result := nil
      else
      	result := getObject( _currentIndex )
      ;
    end
  ;


  function TSMExposureList.last() : TSMExposure;
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


  function TSMExposureList.next() : TSMExposure;
    begin
      _currentIndex := _currentIndex + 1;
      if( _currentIndex > (self.Count - 1) ) then
      	result := nil
      else
      	result := getObject( _currentIndex )
      ;
    end
  ;


  function TSMExposureList.current() : TSMExposure;
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
