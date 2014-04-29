unit Zone;

(*
Zone.pas
--------
Begin: 2006/12/19
Last revision: $Date: 2011-08-26 17:12:16 $ $Author: areeves $
Version number: $Revision: 1.25.4.3 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2006 - 2011 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)


{$INCLUDE ../Defs.inc}


interface

  uses
    Contnrs,

    QIntegerMaps,
    QLists,

    Sdew,

    Models,
    ModelDatabase,

    SMDatabase
  ;

  type TZone = class( TModel )
    protected
      // Basic properties
      _id: integer;
      _descr: string;
      _radius: double;

      _level: integer;

      // Outputs
      //--------
      // These structures will contain running totals for the number of herds and
      // number of animals in each zone.  The map key is a production type ID, the
      // map value is the number of herds/animals.
      // Because these are QMaps, iterating over them will always give records
      // in order of production type ID.  This is a good thing...
      _herdDays: TQIntegerIntegerMap;
      _animalDays: TQIntegerIntegerMap;

      // These structures contain the number of herds and animals in each zone
      // on any given day (i.e., not running totals).  They work the same way
      // as the two QMaps shown above.
      _herdCount: TQIntegerIntegerMap;
      _animalCount: TQIntegerIntegerMap;

      _area: double;
      _maxArea: double;
      _maxAreaDay: integer;

      _perimeter: double;
      _maxPerimeter: double;
      _maxPerimeterDay: integer;

      // Housekeeping properties
      _removed: boolean;

      // Functions for internal use
      procedure initialize();

      // Basic properties
      procedure setId( val: integer );
      procedure setDescr( val: string );
      procedure setRadius( val: double );

      function getId(): integer;
      function getDescr(): string;
      function getRadius(): double;
      function getUnitCount(): integer;
      function getAnimalCount(): integer;
      function getUnitDays(): integer;
      function getAnimalDays(): integer;

      // Properties overridden from base class
      function getUpdated(): boolean; override;

      // Housekeeping properties
      procedure setRemoved( val: boolean );
      function getRemoved(): boolean;


      // Outputs
      procedure clearAllRecords( db: TSMDatabase ); // Called at simulation start
      procedure resetIterationRecords(); // Called at iteration start
      procedure prepareForDay( const simDay: integer ); // Called at the start of each day
      procedure processDailyRecords( db: TSMDatabase; const iteration: integer; const day: integer ); // Called at the end of every day, after zone totals have been calculated
      procedure processIterationRecords(
        db: TSMDatabase;
        iteration: integer;
        const includeCosts: boolean;
        const focusCreated: boolean
      ); // Called at the end of every complete iteration
      procedure simComplete(); // Called at the end of a simulation

    public
      constructor create(); overload;
      constructor create( const id: integer; const descr: string; const radius: double; sim: TObject ); overload;
      constructor create( const src: TZone; sim: TObject ); overload;

      destructor destroy(); override;

      procedure debug(); override;

      function validate( err: pstring = nil ): boolean; override;

      function ssXml( const zoneLevel: integer ): string; reintroduce;

      function populateDatabase( db: TSMDatabase; const updateAction: TDBUpdateActionType ): integer; reintroduce;

      function getUnitCountById( prodTypeId: integer ): integer;
      function getAnimalCountById( prodTypeId: integer ): integer;
      function getUnitDaysById( prodTypeId: integer ): integer;
      function getAnimalDaysById( prodTypeId: integer ): integer;

      // Outputs
      procedure addToZoneTotals( const prodTypeID: integer; const herdAnimalCount: integer; const day: integer ); // Called at the end of every day
      procedure setArea( const val: double; const simDay: integer );
      procedure setPerimeter( const val: double; const simDay: integer );

      // Basic properties
      property id: integer read getId write setId;
      property zoneID: integer read getId write setID;
      property descr: string read getDescr write setDescr;
      property radius: double read getRadius write setRadius;
      property level: integer read _level;
      property area: double read _area;
      property unitCount: integer read getUnitCount;
      property animalCount: integer read getAnimalCount;
      property unitDays: integer read getUnitDays;
      property animalDays: integer read getAnimalDays;
      property perimeter: double read _perimeter;

      // Housekeeping properties
      property removed: boolean read getRemoved write setRemoved;
    end
  ;

  
  type TZoneList = class( TModelList )
    protected
      _xmlModelList: TQStringList;
      _currentIndex: integer;

      _firstZoneFocusDay: integer;
      _focusCreated: boolean;

      procedure setObject( index: integer; item: TZone );
      function getObject( index: integer ): TZone;

      procedure initialize( sim: TObject );

      function getXmlModelList(): TQStringList;
      procedure importXml( model: pointer; sdew: TSdew; const nextZoneID: integer; errMsg: pstring = nil );

      function getUpdated(): boolean;

    public
      constructor create( sim: TObject ); overload;
      constructor create( db: TSMDatabase; sim: TObject ); overload;
      constructor create( db: TSMDatabase; sim: TObject; sdew: TSdew; models: pointer; errMsg: pstring = nil ); overload;
      constructor create( const src: TZoneList; sim: TObject ); overload;

      destructor destroy(); override;

      procedure populateDatabase( db: TSMDatabase; ptList: TObject; const updateAction: TDBUpdateActionType );

      function ssXml(): string;

      function validate( err: PString = nil ): boolean; override;
      function functionsAreValid(): boolean; override;
      
      procedure debug(); override;

      procedure sortByRadius();

      function findByLevel( const level: integer ): TZone;

      function equals( const otherList: TZoneList ): boolean;

      // Outputs
      //--------
      procedure clearAllRecords( db: TSMDatabase ); // Called at simulation start
      procedure resetIterationRecords(); // Called at iteration start
      procedure prepareForDay( const simDay: integer ); // Called at the start of each day
      procedure processDailyRecords( db: TSMDatabase; const iteration: integer; const day: integer ); // Called at the end of each day
      procedure processIterationRecords( db: TSMDatabase; const iteration: integer ); // Called at the end of every complete iteration
      procedure simComplete(); // Called at the end of a simulation

      procedure setFocusCreatedOnDay( const day: integer );

      // Typical list functions
      //-----------------------
      function find( const zoneID: integer ): TZone; overload;
      function find( const zoneName: string ): TZone; overload;
      function contains( const zoneName: string ): boolean;

      function append( dm: TZone ): integer; reintroduce;
      procedure insert( index: integer; dm: TZone );
      property objects[ index: integer]: TZone read getObject write setObject; default;
      function first(): TZone;
      function last(): TZone;
      function next(): TZone;
      function at( i: word ): TZone;
      function current(): TZone;

      // Outputs
      //--------
      property focusCreated: boolean read _focusCreated;

      // Useful properties
      //------------------
      property xmlModelList: TQStringList read getXmlModelList;
      property updated: boolean read getUpdated;
    end
  ;


  type TZoneListIterator = class( TModelListIterator )
    public
      function toFirst(): TZone;
      function toLast(): TZone;
      function current(): TZone;
  	end
  ;
  
  
implementation

  uses
    Forms, // For the application object
    StrUtils,
    SysUtils,
    Math,

    DebugWindow,
    MyStrUtils,
    sqlClasses,
    RemoteDatabaseParams,
    I88n,

    SMSimulationInput,
    ProductionType,
    ProductionTypeList
  ;

  const DBSHOWMSG = false; // set to true to enable debugging messages for this unit

//-----------------------------------------------------------------------------
// TZone construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TZone.create();
    begin
    	inherited create();
      initialize();
    end
  ;


  constructor TZone.create( const id: integer; const descr: string; const radius: double; sim: TObject );
    begin
      inherited create();
      initialize();
      
      _sim := sim;
      _id := id;
      _descr := descr;
      _radius := radius;

      // Level will be set appropriately when a simulation is launched.

      _updated := true;
    end
  ;


  constructor TZone.create( const src: TZone; sim: TObject );
    begin
      inherited create( src );
      _sim := sim;

      _id := src._id;
      _descr := src._descr;
      _radius := src._radius;

      // Level will actually be set appropriately when a simulation is launched.
      _level := src._level;
      _area := src._area;
      _maxArea := src._maxArea;
      _maxAreaDay := src._maxAreaDay;

      _perimeter := src._perimeter;
      _maxPerimeter := src._maxPerimeter;
      _maxPerimeterDay := src._maxPerimeterDay;

      _removed := src._removed;
      _updated := src._updated;
    end
  ;


  destructor TZone.destroy();
    begin
      inherited destroy();
    end
  ;


  procedure TZone.initialize();
    begin
      _id := -1;
      _descr := '';
      _radius := -1.0;
      _level := -1;

      _area := 0.0;
      _maxArea := 0.0;
      _maxAreaDay := 0;

      _perimeter := 0.0;
      _maxPerimeter := 0.0;
      _maxAreaDay := 0;

      _removed := false;
      _updated := false;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TZone: Validation and debugging
//-----------------------------------------------------------------------------
  procedure TZone.debug();
    begin
      dbcout( '=========== TZONE DEBUG', true );
      dbcout( 'ID: ' + intToStr( _id ), true );
      dbcout( 'Descr: ' + _descr, true );
      dbcout( 'Radius: ' + usFloatToStr( _radius ), true );
      dbcout( 'Updated: ' + usBoolToText( _updated ), true );
      dbcout( '=========== END TZONE', true );
    end
  ;


  function TZone.validate( err: pstring = nil ): boolean;
    var
    	msg: string;
    begin
      msg := '';
      result := true;

      if( 0 > _id ) then
        begin
          result := false;
          if( nil <> err ) then msg := msg + '  ' + tr( 'Zone has an invalid ID number.' );
        end
      ;

      if( 0 >= _radius ) then
        begin
          result := false;
          if( nil <> err ) then msg := msg + '  ' + tr( 'Radius must be greater than 0 km.' );
        end
      ;

      if( 0 = length( trim( _descr ) ) ) then
        begin
          result := false;
          if( nil <> err ) then msg := msg + '  ' + tr( 'Zone is unnamed.' );
        end
      ;

      // Don't validate based on level.

      if( ( result = false ) and ( err <> nil ) ) then
      	begin
					msg := endl + tr( 'Zone' ) + ' ' + _descr + ':' + endl + msg;
          err^ := err^ + msg;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TZone: Unit and animal counts
//-----------------------------------------------------------------------------
  function TZone.getUnitCountById( prodTypeId: integer ): integer;
    var
      total:integer;
    begin
      total := 0;

      if _herdCount.contains( prodTypeId ) then
        total := _herdCount.value( prodTypeId )
      ;

      result := total;
    end
  ;


  function TZone.getUnitCount(): integer;
    var
      i:integer;
      sum:integer;
    begin
      sum := 0;
      for i := 0 to (_herdCount.count -1 ) do
        sum := sum + _herdCount.itemAtIndex( i )
      ;

      result := sum;
    end
  ;


  function TZone.getAnimalCountById( prodTypeId: integer ): integer;
    var
      total:integer;
    begin
      total := 0;

      if _animalCount.contains( prodTypeId ) then
        total := _animalCount.value( prodTypeId )
      ;

      result := total;
    end
  ;

  
  function TZone.getAnimalCount(): integer;
    var
      i:integer;
      sum:integer;
    begin
      sum := 0;
      for i := 0 to (_animalCount.count -1 ) do
        sum := sum + _animalCount.itemAtIndex( i )
      ;

      result := sum;
    end
  ;


  function TZone.getAnimalDaysById( prodTypeId: integer ): integer;
    var
      total:integer;
    begin
      total := 0;

      if _animalDays.contains( prodTypeId ) then
        total := _animalDays.value( prodTypeId )
      ;

      result := total;
    end
  ;


  function TZone.getAnimalDays(): integer;
    var
      i:integer;
      sum:integer;
    begin
      sum := 0;
      for i := 0 to (_animalDays.count -1 ) do
        begin
          sum := sum + _animalDays.itemAtIndex( i );
        end;

      result := sum;
    end
  ;


  function TZone.getUnitDaysById( prodTypeId: integer ): integer;
    var
      total:integer;
    begin
      total := 0;

      if _herdDays.contains( prodTypeId ) then
        total := _herdDays.value( prodTypeId )
      ;

      result := total;
    end
  ;


  function TZone.getUnitDays(): integer;
    var
      i:integer;
      sum:integer;
    begin
      sum := 0;
      for i := 0 to (_herdDays.count -1 ) do
        sum := sum + _herdDays.itemAtIndex( i )
      ;

      result := sum;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TZone XML output
//-----------------------------------------------------------------------------
  function TZone.ssXml( const zoneLevel: integer ): string;
    begin
      _level := zoneLevel;

      result :=
          '  <zone-model zone-id="' + intToStr( self.zoneID ) + '">' + endl
        + '    <name>' + encodeXml( self.descr ) + '</name>' + endl
        + '    <level>' + intToStr( self.level ) + '</level>' + endl
        + '    <radius>' + endl
        + '      <value>' + usFloatToStr( self.radius ) + '</value>' + endl
        + '      <units><xdf:unit>km</xdf:unit></units>' + endl
        + '    </radius>' + endl
        + '  </zone-model>' + endl + endl
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TZone database population
//-----------------------------------------------------------------------------
  function TZone.populateDatabase( db: TSMDatabase; const updateAction: TDBUpdateActionType ): integer;
  	var
    	q: string;
      dict: TQueryDictionary;
    begin
      dict := TQueryDictionary.create();

      dict['descr'] := db.sqlQuote( trim( _descr ) );
      dict['radius'] := usFloatToStr( _radius );

      // Level isn't included in the database.  It is set as needed when a simulation is launched.

      if( ( 0 < _id ) and ( MDBAForceInsert <> updateAction ) ) then
        begin
          q := writeQuery( 'inZone', QUpdate, dict, 'WHERE `zoneID` = ' + intToStr( self.id ) );
          db.execute( q );
        end
      else
        begin
          if( 0 < _id ) then
            dict['zoneID'] := intToStr( _id )
          ;

          q := writeQuery( 'inZone', QInsert, dict );
          db.execute( q );
          _id := db.lastInsertID();
        end
      ;

      dict.clear();
      dict.free();

      _updated := false;

      result := _id;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TZone outputs
//-----------------------------------------------------------------------------
  procedure TZone.setArea( const val: double; const simDay: integer );
    begin
      _area := val;
      
      if( val > _maxArea ) then
        begin
          _maxArea := val;
          _maxAreaDay := simDay;
        end
      ;
    end
  ;


  procedure TZone.setPerimeter( const val: double; const simDay: integer );
    begin
      _perimeter := val;

      if( val > _maxPerimeter ) then
        begin
          _maxPerimeter := val;
          _maxPerimeterDay := simDay;
        end
      ;
    end
  ;


  procedure TZone.clearAllRecords( db: TSMDatabase ); // At the beginning of the simulation
    begin
      // Check for existing objects, just in case.
      // (They should never exist at this point.)
      if( nil <> _herdDays ) then
        begin
          _herdDays.clear();
          freeAndNil( _herdDays );
        end
      ;
      if( nil <> _animalDays ) then
        begin
          _animalDays.clear();
          freeAndNil( _animalDays );
        end
      ;

      if( nil <> _herdCount ) then
        begin
          _herdCount.clear();
          freeAndNil( _herdCount );
        end
      ;
      if( nil <> _animalCount ) then
        begin
          _animalCount.clear();
          freeAndNil( _animalCount );
        end
      ;

      _herdDays := TQIntegerIntegerMap.create();
      _animalDays := TQIntegerIntegerMap.create();

      _herdCount := TQIntegerIntegerMap.create();
      _animalCount := TQIntegerIntegerMap.create();

      _area := 0.0;

      // This function is called whenever a simulation is launched.
      // When this happens, the remote database needs to know about the zones
      // in the scenario.  That's what the following block of code is for (even
      // though it seems a little out of place here).
      if( remoteDBParams.useRemoteDatabase ) then
        db.remoteExecute(
          'INSERT INTO `inZone` ( `zoneID`, `descr`, `scenarioID` ) VALUES('
          + ' ' + intToStr( id )
          + ', ''' + descr + ''''
          + ', ' + intToStr( remoteDBParams.scenarioID )
          + ' )'
        )
      ;
    end
  ;


  procedure TZone.resetIterationRecords(); // At the beginning of an iteration
    begin
      _herdDays.clear();
      _animalDays.clear();

      _area := 0.0;
      _maxArea := 0.0;
      _maxAreaDay := 0;

      _perimeter := 0.0;
      _maxPerimeter := 0.0;
      _maxPerimeterDay := 0;
    end
  ;


  procedure TZone.prepareForDay( const simDay: integer ); // At the beginning of each day
    begin
      _herdCount.clear();
      _animalCount.clear();
    end
  ;


  procedure TZone.addToZoneTotals( const prodTypeID: integer; const herdAnimalCount: integer; const day: integer );  // On each sim day
    begin
      // Update the running totals
      //--------------------------
      if( _herdDays.contains( prodTypeID ) ) then
        _herdDays.insert( prodTypeID, _herdDays.value( prodTypeID ) + 1 )
      else
        _herdDays.insert( prodTypeID, 1 )
      ;

      if( _animalDays.contains( prodTypeID ) ) then
        _animalDays.insert( prodTypeID, _animalDays.value( prodTypeID ) + herdAnimalCount )
      else
        _animalDays.insert( prodTypeID, herdAnimalCount )
      ;

      // Update the daily counts
      //------------------------
      if( _herdCount.contains( prodTypeID ) ) then
        _herdCount.insert( prodTypeID, _herdCount.value( prodTypeID ) + 1 )
      else
        _herdCount.insert( prodTypeID, 1 )
      ;

      if( _animalCount.contains( prodTypeID ) ) then
        _animalCount.insert( prodTypeID, _animalCount.value( prodTypeID ) + herdAnimalCount )
      else
        _animalCount.insert( prodTypeID, herdAnimalCount )
      ;
    end
  ;


  procedure TZone.processDailyRecords( db: TSMDatabase; const iteration: integer; const day: integer ); // At the end of each sim day
    var
      qDict: TQueryDictionary;
      q: string;
      prodTypeID: integer;
      pList:TProductionTypeList;
      i:integer;
    begin
      qDict := TQueryDictionary.create();

      pList := (_sim as TSMSimulationInput).ptList;
      for i := 0 to (pList.Count - 1) do
        begin
          qDict.clear();

          prodTypeID := pList[i].productionTypeID;

          qDict['iteration'] := intToStr( iteration );
          qDict['day'] := intToStr( day );
          qDict['zoneID'] := intToStr( self.id );
          qDict['productionTypeID'] := intToStr( prodTypeID );

          if( _herdDays.contains( prodTypeID ) ) then
            begin
              qDict['unitDaysInZone'] := intToStr( _herdDays.value( prodTypeID ) );
              qDict['animalDaysInZone'] := intToStr( _animalDays.value( prodTypeID ) );
              qDict['unitsInZone'] := intToStr( _herdCount.value( prodTypeID ) );
              qDict['animalsInZone'] := intToStr( _animalCount.value( prodTypeID ) );
            end
          else
            begin
              qDict['unitDaysInZone'] := '0';
              qDict['animalDaysInZone'] := '0';
              qDict['unitsInZone'] := '0';
              qDict['animalsInZone'] := '0';
            end
          ;

          q := sqlclasses.writeQuery(
            'outDailyByZoneAndProductionType',
            QInsert,
            qDict
          );

          // Always save in the local database...
          if not( db.execute( q ) ) then
            raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
          ;

          /// ...and save in the remote database if all daily outputs were requested.
          if( db.saveAllDailyOutputs and remoteDBParams.useRemoteDatabase ) then
            begin
              qDict['jobID'] := intToStr( remoteDBParams.jobID );
              q := sqlclasses.writeQuery(
                'outDailyByZoneAndProductionType',
                QInsert,
                qDict
              );
              db.remoteExecute( q );
            end
          ;
        end
      ;

      q := 'INSERT INTO `outDailyByZone` ( `iteration`, `day`, `zoneID`, `zoneArea`, `zonePerimeter` )'
        + ' VALUES ( '
        + intToStr( iteration )
        + ', ' + intToStr( day )
        + ', ' + intToStr( self.id )
        + ', ' + usFloatToStr( _area )
        + ',' + usFloatToStr( _perimeter )
        + ' )'
      ;

      if not( db.execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;

      if( db.saveAllDailyOutputs and remoteDBParams.useRemoteDatabase ) then
        begin
          q := 'INSERT INTO `outDailyByZone` ( `jobID`, `iteration`, `day`, `zoneID`, `zoneArea`, `zonePerimeter` )'
            + ' VALUES ( '
            + intToStr( remoteDBParams.jobID ) + ', '
            + intToStr( iteration ) + ', '
            + intToStr( day ) + ', '
            + intToStr( self.id ) + ', '
            + usFloatToStr( _area ) + ','
            + usFloatToStr( _perimeter )
            + ' )'
          ;

          db.remoteExecute( q );
        end
      ;

      qDict.free();
    end
  ;


  // At the end of each iteration
  procedure TZone.processIterationRecords(
        db: TSMDatabase;
        iteration: integer;
        const includeCosts: boolean;
        const focusCreated: boolean
      );
    var
      qDict: TQueryDictionary;
      q: string;
      ptList:TProductionTypeList;
      prodTypeID: integer;
      survCostPerDay: double;
      i: integer;
    begin
      qDict := TQueryDictionary.create();

      ptList := (_sim as TSMSimulationInput).ptList;
      for i := 0 to (ptList.Count - 1) do
        begin
          qDict.clear();

          prodTypeID := ptList[i].productionTypeID;

          qDict['iteration'] := intToStr( iteration );
          qDict['zoneID'] := intToStr( self.id );

          qDict['productionTypeID'] := intToStr( prodTypeID );

          if( _herdDays.contains( prodTypeID ) ) then
            begin
              qDict['unitDaysInZone'] := intToStr( _herdDays.value( prodTypeID ) );
              qDict['animalDaysInZone'] := intToStr( _animalDays.value( prodTypeID ) );

              if( includeCosts ) then
                begin
                  survCostPerDay := ptList.byID( prodTypeID ).zoneParams.zonePtParamsList.paramsForZone( self.id ).costSurvPerAnimalDay;
                  qDict['costSurveillance'] := usFloatToStr( survCostPerDay * _animalDays.value( prodTypeID ), 2, true );
                end
              else
                qDict['costSurveillance'] := DATABASE_NULL_VALUE
              ;
            end
          else
            begin
              qDict['unitDaysInZone'] := '0';
              qDict['animalDaysInZone'] := '0';

              if( includeCosts ) then
                qDict['costSurveillance'] := usFloatToStr( 0.00, 2, true )
              else
                qDict['costSurveillance'] := DATABASE_NULL_VALUE
              ;
            end
          ;

          if( remoteDBParams.useRemoteDatabase ) then
            qDict['jobID'] := intToStr( remoteDBParams.jobID )
          ;

          q := sqlclasses.writeQuery(
            'outIterationByZoneAndProductionType',
            QInsert,
            qDict
          );

          if( remoteDBParams.useRemoteDatabase ) then
           db.remoteExecute( q )
          else
            begin
              if not( db.execute( q ) ) then
                raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
              ;
            end
          ;
        end
      ;

      qDict.clear();

      qDict['iteration'] := intToStr( iteration );
      qDict['zoneID'] := intToStr( self.id );

      if( focusCreated ) then
        begin
          qDict['finalZoneArea'] := usFloatToStr( _area );
          qDict['maxZoneArea'] := usFloatToStr( _maxArea );
          qDict['maxZoneAreaDay'] := intToStr( _maxAreaDay );
          qDict['finalZonePerimeter'] := usFloatToStr( _perimeter );
          qDict['maxZonePerimeter'] := usFloatToStr( _maxPerimeter );
          qDict['maxZonePerimeterDay'] := intToStr( _maxPerimeterDay );
        end
      else
        begin
          qDict['finalZoneArea'] := DATABASE_NULL_VALUE;
          qDict['maxZoneArea'] := DATABASE_NULL_VALUE;
          qDict['maxZoneAreaDay'] := DATABASE_NULL_VALUE;
          qDict['finalZonePerimeter'] := DATABASE_NULL_VALUE;
          qDict['maxZonePerimeter'] := DATABASE_NULL_VALUE;
          qDict['maxZonePerimeterDay'] := DATABASE_NULL_VALUE;
        end
      ;

      if( remoteDBParams.useRemoteDatabase ) then
        qDict['jobID'] := intToStr( remoteDBParams.jobID )
      ;

      q := sqlclasses.writeQuery( 'outIterationByZone', QInsert, qDict );

      if( remoteDBParams.useRemoteDatabase ) then
        db.remoteExecute( q )
      else
        db.execute( q )
      ;

      qDict.free();
    end
  ;


  procedure TZone.simComplete(); // At the end of each iteration
    begin
      if( nil <> _herdDays ) then
        begin
          _herdDays.clear();
          freeAndNil( _herdDays );
        end
      ;
      if( nil <> _animalDays ) then
        begin
          _animalDays.clear();
          freeAndNil( _animalDays );
        end
      ;

      if( nil <> _herdCount ) then
        begin
          _herdCount.clear();
          freeAndNil( _herdCount );
        end
      ;
      if( nil <> _animalCount ) then
        begin
          _animalCount.clear();
          freeAndNil( _animalCount );
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TZone properties
//-----------------------------------------------------------------------------
  procedure TZone.setId( val: integer ); begin _id := val; _updated := true; end;
  procedure TZone.setDescr( val: string ); begin _descr := val; _updated := true; end;
  procedure TZone.setRadius( val: double ); begin _radius := val; _updated := true; end;

  function TZone.getId(): integer; begin result := _id; end;
  function TZone.getDescr(): string; begin result := _descr; end;
  function TZone.getRadius(): double; begin result := _radius; end;

  function TZone.getUpdated(): boolean; begin result := _updated; end;

  procedure TZone.setRemoved( val: boolean ); begin _removed := val; _updated := true; end;
  function TZone.getRemoved(): boolean; begin result := _removed; end;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TZoneList: Helper functions
//-----------------------------------------------------------------------------
  function compareZonesByRadius( Item1, Item2: Pointer ): Integer;
    begin
      result := compareValue( TZone(Item1).radius, TZone(Item2).radius );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TZoneList: construction/destruction
//-----------------------------------------------------------------------------
  constructor TZoneList.create( sim: TObject );
    begin
      inherited create( true );
      initialize( sim );
    end
  ;


  constructor TZoneList.create( const src: TZoneList; sim: TObject );
    var
      srcZ: TZone;
      newZ: TZone;
    begin
      inherited create( src );
      initialize( sim );

      if ( ( Assigned( src ) ) and ( Assigned( sim ) ) ) then
        begin
          srcZ := src.first();
          while( nil <> srcZ ) do
            begin
              newZ := TZone.create( srcZ, _sim );
              self.append( newZ );
              srcZ := src.next();
            end
          ;
        end
      ;

      sortByRadius();
    end
  ;


  constructor TZoneList.create( db: TSMDatabase; sim: TObject );
    var
      db2: TSqlDatabase;
      res: TSqlResult;
      row: TSqlRow;

      q: string;
      
      z: TZone;
    begin
  		inherited create( true );
      initialize( sim );

      if ( ( Assigned( db ) ) and ( Assigned( sim ) ) ) then
        begin
          db2 := db as TSqlDatabase;

          q := 'SELECT'
            + ' zoneID, descr, radius '
            + ' FROM inZone'
          ;

          res := TSqlResult.create(q, db2 );

          row := res.fetchArrayFirst();

          while( nil <> row ) do
            begin
              Application.ProcessMessages();

              z := TZone.Create(
                integer( row.field('zoneID') ),
                string( row.field('descr') ),
                double( row.field('radius') ),
                sim
              );

              z._updated := false;
              self.append( z );

              row := res.fetchArrayNext();
            end
          ;

          freeAndNil( res );

          sortByRadius();
        end
      ;

      _focusCreated := false;
      _firstZoneFocusDay := -1;
    end
  ;


  constructor TZoneList.create( db: TSMDatabase; sim: TObject; sdew: TSdew; models: pointer; errMsg: pstring = nil );
    var
      i: integer;
      model: pointer;
      nModels: integer;
      modelName: string;
      nextZoneID: integer;
    begin
      inherited create( true );
      initialize( sim );

      nModels := sdew.GetElementCount( models );

      nextZoneID := 1;
      for i := 0 to nModels - 1 do
        begin
          model := sdew.GetElementByIndex( models, i );
          modelName := sdew.GetElementName( model );

          if( xmlModelList.contains( modelName ) ) then
            begin
              importXml( model, sdew, nextZoneID, errMsg );
              inc( nextZoneID );
            end
          ;
        end
      ;
    end
  ;


  procedure TZoneList.initialize( sim: TObject );
    begin
      _currentIndex := 0;
      if ( Assigned( sim ) ) then
        _sim := sim
      else
        _sim := nil
      ;

      _focusCreated := false;
      _firstZoneFocusDay := -1;

      _xmlModelList := nil;
    end
  ;


	destructor TZoneList.destroy();
  	begin
      freeAndNil( _xmlModelList );
    	inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TZoneList: Database handling
//-----------------------------------------------------------------------------
  procedure TZoneList.populateDatabase( db: TSMDatabase; ptList: TObject; const updateAction: TDBUpdateActionType );
  	var
    	zone: TZone;
      tempID: integer;
      ptList2: TProductionTypeList;
  	begin
      if( nil <> ptList ) then
        ptList2 := ptList as TProductionTypeList
      else
        ptList2 := nil
      ;
      
      zone := self.first();

      while( nil <> zone ) do
        begin
          if( zone.removed ) then
            begin
              if( 0 < zone.id ) then
                begin
                  dbcout( '######## zone will be removed: ' + intToStr( zone.id ), DBSHOWMSG );

                  // Remove this zone from the production type list in memory
                  if( nil <> ptList2 ) then
                    ptList2.removeZone( zone.id )
                  ;

                  // Remove this zone from the database.
                  db.removeZone( zone.id );
                end
              ;

              // Remove this zone from memory 
              remove( zone ); // Remember that TObjectList.remove() also frees the object.
              zone := self.current();
            end
          else
            begin
              if( zone.updated ) then
                begin
                  tempID := zone.id;
                  zone.populateDatabase( db, updateAction );
                  
                  if( ( 0 > tempID ) and ( nil <> ptList2 ) ) then // This was a new zone.  Make sure that the production types know about it.
                    ptList2.addZone( zone.id )
                  ;
                end
              ;

              zone := self.next();
            end
          ;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TZone XML output
//-----------------------------------------------------------------------------
  function TZoneList.ssXml(): string;
  	var
      i: integer;
    	z: TZone;
    begin
      self.sortByRadius();

      // Write all of the zones.
      for i := 0 to self.Count - 1 do
        begin
          z := self.at( i );
        	result := result + z.ssXml( i + 1 );
        end
      ;

      // Add the background zone.
      result := result +
          '  <zone-model>' + endl
        + '    <name>Background</name>' + endl
        + '    <level>' + intToStr( self.Count + 1 ) + '</level>' + endl
        + '    <radius>' + endl
        + '      <value>0</value>' + endl
        + '      <units><xdf:unit>km</xdf:unit></units>' + endl
        + '    </radius>' + endl
        + '  </zone-model>' + endl + endl
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TZoneList: sorting aond comparison
//-----------------------------------------------------------------------------
  procedure TZoneList.sortByRadius();
    begin
      self.Sort( compareZonesByRadius );
    end
  ;


  function TZoneList.equals( const otherList: TZoneList ): boolean;
    var
      i: integer;
      Z, otherZ: TZone;
    begin
      result := true; // until proven otherwise.

      // Are the lists the same size?
      if( self.Count <> otherList.Count ) then
        begin
          result := false;
          exit;
        end
      ;

      // Do all zones match?
      for i := 0 to self.Count - 1 do
        begin
          z := self.at( i );
          otherZ := otherList.at( i );

          if( z.id <> otherZ.id ) then
            begin
              result := false;
              break;
            end
          ;

          if( z.descr <> otherZ.descr ) then
            begin
              result := false;
              break;
            end
          ;

          // DON'T compare zone level: the 'other' zone level won't be set when the scenario file is loaded
          // Should zone radii be compared?
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TZoneList: debugging and validation
//-----------------------------------------------------------------------------
	procedure TZoneList.debug();
  	var
    	z: TZone;
  	begin
    	z := self.first();
      while( z <> nil ) do
      	begin
        	z.debug();
          z := self.next();
        end
      ;
    end
  ;

  
  function TZoneList.validate( err: PString = nil ): boolean;
  	var
    	dm: TZone;

      i: integer;
      it: TZoneListIterator;
    begin
    	result := true;

      // See if there are any zones.
      if( 0 = self.Count ) then
        begin
          result := false;
          if( nil <> err ) then err^ := err^ + tr( 'Use of zones is indicated, but no zones are defined.' );
          exit;
        end
      ;

      // See if each zone is completely specified.
      dm := self.first();

      while( nil <> dm ) do
        begin
        	if( not( dm.validate( err ) ) ) then
            begin
          	  result := false;
              break;
            end
          ;

          dm := self.next();
        end
      ;

      // Also make sure that each zone has a unique name and radius.
      if( result ) then
        begin
          it := TZoneListIterator.create( self );
          
          while( nil <> it.current() ) do
            begin
              for i := 0 to self.Count - 1 do
                begin
                  dm := self.at(i);

                  // Check the radius
                  //-----------------
                  if
                    ( not( dm = it.current() ) )
                  and
                    ( dm.radius = it.current().radius )
                  then
                    begin
                      if( nil <> err ) then err^ := err^ + tr( 'Each zone must have a unique radius.' );
                      result := false;
                      break;
                    end
                  ;

                  // Check the name
                  //---------------
                  if
                    ( not( dm = it.current() ) )
                  and
                    ( dm.descr = it.current().descr )
                  then
                    begin
                      if( nil <> err ) then err^ := err^ + tr( 'Each zone must have a unique name.' );
                      result := false;
                      break;
                    end
                  ;
                end
              ;

              if( false = result ) then
                break
              ;

              it.incr();
            end
          ;

          it.Free();
        end
      ;
    end
  ;


  function TZoneList.functionsAreValid(): boolean;
    begin
      // There are currently no functions to validate to TZoneList.
      result := true;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TZoneList: Outputs
//-----------------------------------------------------------------------------
	procedure TZoneList.clearAllRecords( db: TSMDatabase );
  	var
    	it: TZoneListIterator;
    begin
    	 it := TZoneListIterator.create( self );

       while( nil <> it.current() ) do
       	begin
          it.current().clearAllRecords( db );
          it.incr();
        end
       ;

       it.Free();
    end
  ;


  procedure TZoneList.resetIterationRecords();
  	var
    	it: TZoneListIterator;
    begin
       _focusCreated := false;
       _firstZoneFocusDay := -1;

    	 it := TZoneListIterator.create( self );

       while( nil <> it.current() ) do
       	begin
          it.current().resetIterationRecords();
          it.incr();
        end
       ;

       it.Free();
    end
  ;


  procedure TZoneList.setFocusCreatedOnDay( const day: integer );
    begin
      _focusCreated := true;

      if( -1 = _firstZoneFocusDay ) then
        _firstZoneFocusDay := day
      ;
    end
  ;


  procedure TZoneList.prepareForDay( const simDay: integer );
  	var
    	it: TZoneListIterator;
    begin
    	 it := TZoneListIterator.create( self );

       while( nil <> it.current() ) do
       	begin
          it.current().prepareForDay( simDay );
          it.incr();
        end
       ;

       it.Free();
    end
  ;

  procedure TZoneList.processDailyRecords( db: TSMDatabase; const iteration: integer; const day: integer );
  	var
    	it: TZoneListIterator;
    begin
      // DO check _focusCreated here.
      // Daily records should be stored only when zones have been created.
      if( false = _focusCreated ) then
        exit
      ;

      // Was the first zone focus just created?
      // If so, don't record zone outputs: there won't really be any until the next day.
      if( day = _firstZoneFocusDay ) then
        exit
      ;

      it := TZoneListIterator.create( self );

      while( nil <> it.current() ) do
      begin
        it.current().processDailyRecords( db, iteration, day );
        it.incr();
      end
      ;

      it.Free();
    end
  ;


  procedure TZoneList.processIterationRecords( db: TSMDatabase; const iteration: integer );
  	var
    	it: TZoneListIterator;
      includeCosts: boolean;
    begin
      // DO NOT check _focusCreated here.
      // Iteration records should have zone outputs, whether zones were created or not.

      it := TZoneListIterator.create( self );

      includeCosts := (_sim as TSMSimulationInput).costTrackZoneSurveillance;

      while( nil <> it.current() ) do
      begin
        it.current().processIterationRecords( db, iteration, includeCosts, focusCreated );
        it.incr();
      end
      ;

      it.Free();
    end
  ;


  procedure TZoneList.simComplete();
  	var
    	it: TZoneListIterator;
    begin
      it := TZoneListIterator.create( self );

      while( nil <> it.current() ) do
      begin
        it.current().simComplete();
        it.incr();
      end
      ;

      it.Free();
    end
  ;
//-----------------------------------------------------------------------------




//-----------------------------------------------------------------------------
// TZoneList: Typical list functions
//-----------------------------------------------------------------------------
	function TZoneList.find( const zoneID: integer ): TZone;
  	var
     it: TZoneListIterator;
  	begin
    	result := nil;

      it := TZoneListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
      	begin
       		if( zoneID = it.current().id ) then
          	begin
           		result := it.current();
              break;
            end
          ;

          it.incr();
        end
      ;

      it.Free();
    end
  ;


  function TZoneList.find( const zoneName: string ): TZone;
  	var
      it: TZoneListIterator;
  	begin
    	result := nil;

      it := TZoneListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
      	begin
       		if( zoneName = it.current().descr ) then
          	begin
           		result := it.current();
              break;
            end
          ;
          it.incr();
        end
      ;

      it.Free();
    end
  ;


  function TZoneList.contains( const zoneName: string ): boolean;
  	var
      it: TZoneListIterator;
  	begin
    	result := false;

      it := TZoneListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
      	begin
       		if( zoneName = it.current().descr ) then
          	begin
           		result := true;
              break;
            end
          ;
          it.incr();
        end
      ;

      it.Free();
    end
  ;


  function TZoneList.findByLevel( const level: integer ): TZone;
    var
      it: TZoneListIterator;
    begin
      result := nil;

      it := TZoneListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          if( level = it.current().level ) then
            begin
              result := it.current();
              break;
            end
          ;
          it.incr();
        end
      ;

      it.free();
    end
  ;


  function TZoneList.append( dm: TZone ): integer;
    begin
      result := inherited Add( dm );
    end
  ;


  procedure TZoneList.setObject( index: integer; item: TZone );
    begin
      inherited SetItem( index, item );
    end
  ;


  function TZoneList.getObject( index: integer ): TZone;
    begin
      result := inherited GetItem( index ) as TZone;
    end
  ;


  procedure TZoneList.insert(index: integer; dm: TZone);
    begin
      inherited Insert(index, dm);
    end
  ;


  function TZoneList.first() : TZone;
    begin
      _currentIndex := 0;
      if( self.Count = 0 ) then
      	result := nil
      else
      	result := getObject( _currentIndex )
      ;
    end
  ;


  function TZoneList.last() : TZone;
    begin
      if( self.Count = 0 ) then
      	result := nil
      else
        begin
          _currentIndex := self.Count - 1;
          result := getObject( _currentIndex );
        end
      ;
    end
  ;


  function TZoneList.next() : TZone;
    begin
      _currentIndex := _currentIndex + 1;
      if( _currentIndex > (self.Count - 1) ) then
      	result := nil
      else
      	result := getObject( _currentIndex )
      ;
    end
  ;


  function TZoneList.current() : TZone;
    begin
      if( _currentIndex > (self.Count - 1) ) then
      	result := nil
      else
      	result := getObject( _currentIndex )
      ;
    end
  ;


  function TZoneList.at( i: word ): TZone;
  	begin
      if( i > self.Count-1 ) then
      	raise exception.Create( 'Index out of bounds in TZoneList' )
      else
      	result := getObject( i )
      ;  
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TZoneList: properties
//-----------------------------------------------------------------------------
  function TZoneList.getUpdated(): boolean;
  	var
    	dm: TZone;
  	begin
      result := false;
      dm := self.first();

      while( nil <> dm ) do
        begin
          if( dm.updated ) then
            begin
              result := true;
              break;
            end
          ;
          dm := self.next();
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TZoneList: XML import
//-----------------------------------------------------------------------------
  function TZoneList.getXmlModelList(): TQStringList;
    begin
      if( nil = _xmlModelList ) then
        begin
          _xmlModelList := TQStringList.create();
          _xmlModelList.append( 'zone-model' );
        end
      ;

      result := _xmlModelList;
    end
  ;


  procedure TZoneList.importXml( model: pointer; sdew: TSdew; const nextZoneID: integer; errMsg: pstring = nil );
    var
      zone: TZone;

      zoneName: string;
      radius: double;
    begin
      // NOTES:
      // 1) Since the units used in the zone-model remain constant, they
      //    are not read in here.  If this changes in the future, remember to
      //    add the code here to read them in.

      // 2) The Level stored in the XML is not stored in the current database
      //    schema.  Consequently, it, also, is not read in here.  If this changes
      //    then be sure to remember to add the code to read the level here.


      // Ignore any zone ID number that might be in the XML file.  It will screw things up later on.
      //zoneID := myStrToInt( sdew.GetElementAttribute( model, 'zone-id' ), -1 );
      radius := -1.0;
      zoneName := '';

      if( nil <> sdew.GetElementByName( model, 'name' ) ) then
        zoneName := sdew.GetElementContents( sdew.GetElementByName( model, 'name' ) )
      ;

      if( nil <> sdew.GetElementByName( model, 'radius') ) then
        begin
          if ( nil <> sdew.GetElementByName( Sdew.GetElementByName( model, 'radius'), 'value' ) ) then
            radius := usStrToFloat( Sdew.GetElementContents( Sdew.GetElementByName( Sdew.GetElementByName( model, 'radius'), 'value')), -1.0 )
          ;
        end
      ;

      if( self.contains( zoneName ) ) then
        appendToPstring( errMsg, tr( 'XML contains duplicate zone names.' ) )
      else if( strIsEmpty( zoneName ) ) then
        appendToPstring( errMsg, tr( 'XML contains an unnamed zone.' ) )
      else if( 0.0 > radius ) then
        appendToPstring( errMsg, tr( 'XML contains a zone with an invalid radius.' ) )
      else if( 'Background' = zoneName ) then
        // Don't import the background zone.  It will be automatically created later.
      else
        begin
          zone := TZone.create( nextZoneID, zoneName, radius, sim );
          zone.updated := true;
          self.append( zone );
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TZoneListIterator: Basic functions
//-----------------------------------------------------------------------------
	function TZoneListIterator.toFirst(): TZone;
  	begin
      result := _toFirst() as TZone;
    end
  ;

	function TZoneListIterator.toLast(): TZone;
  	begin
      result := _toLast() as TZone;
    end
  ;

  function TZoneListIterator.current(): TZone;
  	begin
      result := _current() as TZone;
    end
  ;
//-----------------------------------------------------------------------------

end.