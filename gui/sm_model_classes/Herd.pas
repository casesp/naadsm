unit Herd;

(*
Herd.pas
---------
Begin: 2005/01/21
Last revision: $Date: 2008/10/15 16:23:43 $ $Author: areeves $
Version number: $Revision: 1.62 $
Project: NAADSM
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
  	Contnrs,
    SysUtils,

    SqlClasses,
    FunctionPointers,

    SMDatabase,
    ProductionType,
    StatusEnums,
    Graphics
  ;

  type TColorArray = array of TColor;
  type THerdList = class; // forward declaration, for the benefit of THerd

	{type} THerd = class( TObject )
      protected
        // Housekeeping
        _updated: boolean;
        _isInDatabase: boolean;
        _removeFromDatabase: boolean;
        _myList: THerdList;

        // Simple properties
        _id: integer;
        _prodTypeName: string;
        _xmlProdTypeName: string;
        _initialSize: integer;
				_lat: double;
				_lon: double;
        _daysLeftInInitialState: integer;
        _initialStatus: TTransitionState;
        _simulatedStatus: TTransitionState;
        _apparentStatus: TApparentStatus;
        _zoneLevel: integer;

        // Advanced properties
      	_sim: TObject;
        _pt: TProductionType;
        _prodTypeID: integer;

        // Outputs
        _itInfections: integer;
        _itDetections: integer;
        _itDestructions: integer;
        _itVaccinations: integer;

        _simVaccinations: integer;
        _simInfections: integer;
        _simDetections: integer;
        _simDestructions: integer;

        // Useful protected functions
        procedure initialize();

        // Housekeeping properties
        function getOutputUpdated(): boolean;

        function getUpdated(): boolean;
        procedure setUpdated( val: boolean );

        function getIsInDatabase(): boolean;
        procedure setIsInDatabase( val: boolean );

        function getRemoveFromDatabase(): boolean;
        procedure setRemoveFromDatabase( val: boolean );

        // Simple properties
        function getID(): integer;
        procedure setID( val: integer );

        procedure setProdTypeName( val: string );
        function getProdTypeName(): string;

        procedure setInitialSize( val: integer );
        function getInitialSize(): integer;

        procedure setDaysLeftInInitialState( val: integer );
        function getDaysLeftInInitialState(): integer;

				function getLat(): double;
				function getLon(): double;

				procedure setLat( val: double );
				procedure setLon( val: double );

        procedure setInitialStatus( val: TTransitionState );
        function getInitialStatus(): TTransitionState;

        function getSimulatedStatus(): TTransitionState;

        procedure setApparentStatus( val: TApparentStatus );
        function getApparentStatus(): TApparentStatus;

        procedure setZoneLevel( val: integer );
        function getZoneLevel(): integer;

        // Advanced properties
        procedure setSimParams( val: TObject );

        procedure setProdTypeID( val: integer );
        function getProdTypeID(): integer;

        //procedure setProdType( pt: TProductionType );  // This function is public: see below.
        function getProdType(): TProductionType;

        // Outputs
        function getCumVaccinations(): integer;
        function getCumInfections(): integer;
        function getCumDetections(): integer;
        function getCumDestructions(): integer;

        procedure setCumVaccinations( val: integer );
        procedure setCumInfections( val: integer );
        procedure setCumDetections( val: integer );
        procedure setCumDestructions( val: integer );

    	public
        // Construction/destruction
        //--------------------------
      	constructor create( list: THerdList ); overload;
        constructor create( const src: THerd; const list: THerdList = nil; const resetToInitialState: boolean = false ); overload;

        destructor destroy(); override;

        procedure assign( const src: THerd; const list: THerdList = nil; const resetToInitialState: boolean = false );

        // Text export
        //------------
        function ssXml(): string;
        function csvText( const separator: string; writeProdTypeName: boolean; writeInitialStateChar: boolean ): string;

        // GIS "properties"
        //----------------
        function isBoundedBy( const latNW, lonNW, latSE, lonSE: double ): boolean;
        function isInCircle( const latCenter, lonCenter, radius: double ): boolean;

        // Validation and debugging, and related functions
        //------------------------------------------------
        function briefInfo(): string;
        function isValid( err: PString = nil ): boolean;
        procedure debug();

        // Functions called when a simulation is in progress
        //--------------------------------------------------
        procedure initializeAllOutputRecords(); // at sim start
        procedure prepareForIteration(); // at iteration start
        procedure prepareForDay(); // at day start

        procedure infectByMechanism( infMech: string; day: integer );
        procedure exposeByMechanism( mechanism: string );
        procedure attemptTraceForReason( mechanism: string );
        procedure traceForReason( mechanism: string );
        function detect( day: integer ): boolean;
        function destroyForReason( reason: string; day:integer ): boolean;
        function vaccinateForReason( reason: string; day: integer ): boolean;

        //procedure setDailyOutput();

        procedure setSimulatedStatus(
          val: TTransitionState;
          updatePTOutputCounts: boolean
        );

        procedure processIterationRecords( iterationJustCompleted: integer );

        // Properties
        //------------
        // Housekeeping properties
        property updated: boolean read getUpdated;
        property isInDatabase: boolean read getIsInDatabase write setIsInDatabase;
        property removeFromDatabase: boolean read getRemoveFromDatabase;
        property outputUpdated: boolean read getOutputUpdated;

        // Simple properties
        property id: integer read getID write setID;

        property prodTypeName: string read getProdTypeName write setProdTypeName;
        property xmlProdTypeName: string read _xmlProdTypeName;

        property actualProdTypeID: integer read _prodTypeID;
        property actualProdTypeName: string read _prodTypeName;

        property initialSize: integer read getInitialSize write setInitialSize;
        property daysLeftInInitialState: integer read getDaysLeftInInitialState write setDaysLeftInInitialState;

				property lat: double read getLat write setLat;
				property lon: double read getLon write setLon;

        property initialStatus: TTransitionState read getInitialStatus write setInitialStatus;
        property simulatedStatus: TTransitionState read getSimulatedStatus;
        property apparentStatus: TApparentStatus read getApparentStatus write setApparentStatus;

        property zoneLevel: integer read getZoneLevel write setZoneLevel;

        // Advanced properties
       	property simParams: TObject write setSimParams;
        procedure setProdType( pt: TProductionType );
        property prodTypeID: integer read getProdTypeID;
        property prodType: TProductionType read getProdType;

        // Outputs
        property cumVaccinations: integer read getCumVaccinations;
        property cumInfections: integer read getCumInfections;
        property cumDetections: integer read getCumDetections;
        property cumDestructions: integer read getCumDestructions;
    end
  ;


  {type} THerdList = class( TObjectList )
    protected
      // Basic list operations
      _currentIndex: integer;

      // Housekeeping
      _updated: boolean;
      _removedCount: integer;

      _smdb: TSMDatabase;

      // Simple properties
      _minLat: double;
      _maxLat: double;
      _minLon: double;
      _maxLon: double;
      _eastLon: double;
      _westLon: double;

      // Basic list operations
      procedure setObject( index: integer; item: THerd );
      function getObject( index: integer ): THerd;

      function getCurrentIndex(): integer;

      // Useful protected functions
      procedure initialize();

      // Housekeeping
      function getUpdated(): boolean;
      procedure setUpdated( val: boolean );

      // Simple properties
      function getMinLat(): double;
      function getMaxLat(): double;
      function getMinLon(): double;
      function getMaxLon(): double;

      function getEastLon(): double;
      function getWestLon(): double;

      procedure setMinMaxLL(); // This function should only rarely be used.

      // Most of the time, the min and max will be set directly from the database.
      procedure setMinMaxLLFromDB();

    public
      // Construction/destruction
      //--------------------------
    	constructor create(); overload;
      constructor create( db: TSMDatabase; sim: TObject; fn: TObjFnBool1Int = nil ); overload;
      constructor create( const src: THerdList; const resetHerdsToInitalState: boolean = false ); overload;

      destructor destroy(); override;

      procedure assign( const src: THerdList; const resetHerdsToInitalState: boolean = false );

      // Basic list operations
      //----------------------
      function first(): THerd;
      function last(): THerd;
      function next(): THerd;
      function current(): THerd;
      function at( index: integer ): THerd;

      function append( dm: THerd ): integer;
      procedure insert( index: integer; dm: THerd );

      property objects[ index: integer]: THerd read getObject write setObject; default;
      property currentPosition: integer read getCurrentIndex;

      // Text export
      //------------
      //function ssXml(): string;  // This function produces a potentially huge string, and is currently unused.  If a file is to be written, writeXMLFile() is more efficient.
      function writeXMLFile( fileName: string; errMsg: PString = nil ): boolean;

      function writeCSVFile(
        fileName: string;
        writeProdTypeName: boolean;
        writeInitialStateChar: boolean;
        errMsg: PString = nil
      ): boolean;

      // Text import
      //-------------
      function importFromFile(
        fileName: string;
        fileFormat: integer;
        fnPrimaryProgress: TObjFnBool1Int = nil;
        fnSecondaryProgress: TObjFnBool1Int = nil;
        fnProgressMessage: TObjFnVoid1String = nil;
        progressStepPercent: integer = 0
      ): boolean;

      function importCSV(
        fileName: string;
        fnPrimaryProgress: TObjFnBool1Int = nil;
        fnSecondaryProgress: TObjFnBool1Int = nil;
        fnProgressMessage: TObjFnVoid1String = nil;
        progressStepPercent: integer = 0
      ): boolean;

      function importXML( const herdFileName: string ): boolean;

      // Database population
      //--------------------
      procedure populateDatabase( db: TSMDatabase; fnProgress: TObjFnBool1Int = nil );

      // Validation and debugging
      //--------------------------
      function isValid( err: PString = nil ): boolean;
      procedure debug();

      // Housekeeping
      //-------------
      procedure resetSim( sim: TObject; recordUpdate: boolean = true );

      procedure removeProductionType( ptID: integer );

      procedure setDB( db: TSMDatabase );

      { Indicated herd will be removed from list the next time the database is populated. }
      procedure scheduleHerdRemoval( herdIndex: integer );

      // For simulation outputs
      //-----------------------
      procedure initializeAllOutputRecords(); // upon sim start
      procedure prepareForIteration( iteration: integer ); // upon iteration start
      procedure prepareForDay( day: integer ); // upon day start
      //procedure processDailyRecords(); // Upon day end
      procedure processIterationRecords( db: TSMDatabase; iterationJustCompleted: integer ); // upon iteration end

      // Housekeeping properties
      //------------------------
      property updated: boolean read getUpdated write setUpdated;
      property removedCount: integer read _removedCount;

      // Simple properties
      //------------------
      property minLat: double read getMinLat;
      property maxLat: double read getMaxLat;
      property minLon: double read getMinLon;
      property maxLon: double read getMaxLon;

      property northLat: double read getMaxLat;
      property southLat: double read getMinLat;

      property westLon: double read getWestLon;
      property eastLon: double read getEastLon;
    end
  ;


  // FIX ME: iterator class has no error checking.
  {type} THerdListIterator = class
  	protected
    	_list: THerdList;
      _currentIndex: integer;

      function getCount(): word;
      function getIsEmpty(): boolean;

    public
    	constructor create( list: THerdList );
      destructor destroy(); override;

      function toFirst(): THerd;
      function toLast(): THerd;
      function current(): THerd;

      procedure incr();
      procedure decr();

      property count: word read getCount;
      property isEmpty: boolean read getIsEmpty;
  	end
  ;


  const
    UNKNOWN_FILE_FORMAT = 0;
    CSV_FILE_FORMAT = 1;
    XML_FILE_FORMAT = 2;

    DBHERD: boolean = false; // set to true to show debug messages for this unit.
  	DBHERDLIST: boolean = false; // set to true to show debug messages for this unit.



implementation

	uses
    StrUtils,
  	Classes,
    Variants,
    Forms, // for Application.ProcessMessages()
    Math,

    MyStrUtils,
    USStrUtils,
    MyDialogs,
    DebugWindow,
    WindowsUtils,
    QStringMaps,
    CStringList,
    CsvParser,
    BasicGIS,
    I88n,
    UnicodeDev,

    ReadXMLInput,
    SMScenario,
    SMSimulationInput
  ;

//*****************************************************************************
// CLASS THerd
//*****************************************************************************
  // --------------------------------------------------------------------------
  // THerd: Creation/initialization/destruction
  // --------------------------------------------------------------------------
    constructor THerd.create( list: THerdList );
      begin
        inherited create();
        _myList := list;
        dbcout( 'Creating empty herd', DBHERD );
        initialize();
      end
    ;


    constructor THerd.create( const src: THerd; const list: THerdList = nil; const resetToInitialState: boolean = false );
      begin
        inherited create();
        dbcout( 'Creating copy of herd' , DBHERD );
        assign( src, list, resetToInitialState );
      end
    ;


    destructor THerd.destroy();
      begin
        inherited destroy();
      end
    ;

    procedure THerd.initialize();
      begin
        // Simple properties
        setID( -1 );
        setProdTypeName( '' );
        setProdTypeID( -1 );
        setInitialSize( -1 );
        _lat := LAT_LON_UNDEFINED;
        _lon := LAT_LON_UNDEFINED;
        _initialStatus := tsUnspecified;
        setDaysLeftInInitialState( -1 );
        _apparentStatus := asUnspecified;
        _zoneLevel := -1;

        // Advanced properties
        _sim := nil;
        _pt := nil;

        // Outputs
        _itInfections := 0;
        _itDetections := 0;
        _itDestructions := 0;
        _itVaccinations := 0;

        _simVaccinations := 0;
        _simInfections := 0;
        _simDetections := 0;
        _simDestructions := 0;

        // Housekeeping
        setIsInDatabase( false );
        _removeFromDatabase := false;
        _updated := false;
      end
    ;


    procedure THerd.assign( const src: THerd; const list: THerdList = nil; const resetToInitialState: boolean = false );
      begin
        // Advanced properties
        _myList := list;

        // Simple properties
        _id := src._id;
        _initialSize := src._initialSize;
        _lat := src._lat;
        _lon := src._lon;

        _initialStatus := src._initialStatus;
        _daysLeftInInitialState := src._daysLeftInInitialState;

        if( resetToInitialState ) then
          begin
            prepareForIteration();

            _simVaccinations := 0;
            _simInfections := 0;
            _simDetections := 0;
            _simDestructions := 0;
          end
        else
          begin
            _simulatedStatus := src._simulatedStatus;
            _apparentStatus := src._apparentStatus;
            _zoneLevel := src._zoneLevel;

            _itInfections := src._itInfections;
            _itDetections := src._itDetections;
            _itDestructions := src._itDestructions;
            _itVaccinations := src._itVaccinations;

            _simVaccinations := src._simVaccinations;
            _simInfections := src._simInfections;
            _simDetections := src._simDetections;
            _simDestructions := src._simDestructions;
          end
        ;

        // Some more advanced properties
        if( nil = src.prodType ) then
          dbcout( 'src._pt is nil!', DBHERD )
        ;

        _sim := src._sim;

        setProdType( src.prodType );
        //setProdTypeName( src._prodTypeName ); // automatically taken care of by setProdType
        //setProdTypeID( src._prodTypeID ); // automatically taken care of by setProdType


        // Housekeeping properties
        _updated := src._updated;
        _isInDatabase := src._isInDatabase;
        _removeFromDatabase := src._removeFromDatabase;
      end
    ;
  //---------------------------------------------------------------------------



  //---------------------------------------------------------------------------
  // THerd: Text export
  //---------------------------------------------------------------------------
    function THerd.ssXML(): string;
      begin
        result := '';
        result := result + '  <herd>' + endl;
        result := result + '    <id>' + intToStr( id ) + '</id>' + endl;
        result := result + '    <production-type>' + xmlProdTypeName + '</production-type>' + endl;
        result := result + '    <size>' + intToStr( initialSize ) + '</size>' + endl;
        result := result + '    <location>' + endl;
        result := result + '      <latitude>' + usFloatToStr( lat, LAT_LON_PRECISION ) + '</latitude>' + endl;
        result := result + '      <longitude>' + usFloatToStr( lon, LAT_LON_PRECISION ) + '</longitude>' + endl;
        result := result + '    </location>' + endl;
        result := result + '    <status>' + xmlTransitionStateString( self.initialStatus ) + '</status>' + endl;

        if( -1 < daysLeftInInitialState ) then
          result := result + '    <days-left-in-status>' + intToStr( daysLeftInInitialState ) + '</days-left-in-status>' + endl
        ;

        result := result + '  </herd>' + endl;
      end
    ;


    function THerd.csvText( const separator: string; writeProdTypeName: boolean; writeInitialStateChar: boolean ): string;
      begin
        result := '';
        result := result + intToStr( self.id ) + separator;

        if( writeProdTypeName ) then
          result := result + '"' + self.prodTypeName + '"' + separator
        else
          result := result + intToStr( self.prodTypeID )  + separator
        ;

        result := result + intToStr( self.initialSize ) + separator;

        result := result + usFloatToStr( self.lat ) + separator;
        result := result + usFloatToStr( self.lon ) + separator;

        if( writeInitialStateChar ) then
          result := result + transitionStateCode( self._initialStatus ) + separator
        else
          result := result + intToStr( ord( self._initialStatus ) ) + separator
        ;

        result := result + intToStr( self.daysLeftInInitialState );
      end
    ;
  //-----------------------------------------------------------------------------



  //-----------------------------------------------------------------------------
  // GIS "properties"
  //-----------------------------------------------------------------------------
    function THerd.isBoundedBy( const latNW, lonNW, latSE, lonSE: double ): boolean;
      begin
        result := gisLLBoundedBy( self.lat, self.lon, latNW, lonNW, latSE, lonSE );
      end
    ;


    function THerd.isInCircle( const latCenter, lonCenter, radius: double ): boolean;
      var
        rSq: double;
      begin
        rSq := power( radius, 2 );
        result := ( rSq >= gisLocalDistanceSquared( latCenter, lonCenter, self.lat, self.lon ) );
      end
    ;
  //-----------------------------------------------------------------------------



  //-----------------------------------------------------------------------------
  // THerd: Validation and debugging, and related functions
  //-----------------------------------------------------------------------------
    function THerd.isValid( err: PString = nil ): boolean;
      var
        msg: string;
        sim: TSMSimulationInput;
      begin
        if( _sim = nil ) then
          raise exception.Create( 'THerd: Scenario is not set.')
        else
          sim := _sim as TSMSimulationInput
        ;

        result := true;

        if( ( 90 < lat ) or ( -90 > lat ) ) then
          begin
            if( err <> nil ) then msg := msg + '  ' + ansiReplaceStr( tr( 'Latitude (xyz) is invalid.' ), 'xyz', usFloatToStr( lat ) ) + endl;
            result := false;
          end
        ;

        if( ( -180 > lon ) or ( 180 < lon ) ) then
          begin
            if( err <> nil ) then msg := msg + '  ' + ansiReplaceStr( tr( 'Longitude (xyz) is invalid.' ), 'xyz', usFloatToStr( lon ) ) + endl;
            result := false;
          end
        ;

        if( 0 > initialSize ) then
          begin
            if( err <> nil ) then msg := msg + '  ' + tr( 'Initial unit size is not set.' ) + endl;
            result := false;
          end
        ;

        if( 0 = initialSize ) then
          begin
            if( err <> nil ) then msg := msg + '  ' + tr( 'Initial unit size is 0.' ) + endl;
            result := false;
          end
        ;

        if( 0 > prodTypeID ) then
          begin
            if( err <> nil ) then msg := msg + '  ' + tr( 'Production type is not identified.' ) + endl;
            result := false;
          end
        ;


        if( tsUnspecified = initialStatus ) then
          begin
            if( err <> nil ) then msg := msg + '  ' + tr( 'Initial disease status is not set.' ) + endl;
            result := false;
          end
        ;

        // Check that all herds have a recognized production type ID
        if( not( sim.prodTypeIDExists( prodTypeID ) ) ) then
          begin
            if( err <> nil ) then msg := msg + '  ' + ansiReplaceStr( tr( 'Production type ID xyz is unrecognized.' ), 'xyz', intToStr( prodTypeID ) ) + endl;
            result := false;
          end
        ;

        // Strange things happen when a herd has an initial disease state other than susceptible
        // if the herd's production type is not set to undergo disease state transitions.
        if
          ( tsSusceptible <> initialStatus )
        and
          ( not( self.prodType.simulateTransition ) )
        then
          begin
            if( err <> nil ) then msg := msg + '  ' + tr( 'Disease transitions are not simulated for units of this type, but an initial disease status is specified.' );
            result := false;
          end
        ;

        if( ( false = result ) and ( nil <> err ) ) then
          begin
            msg := endl +  ansiReplaceStr( tr( 'Unit xyz is not valid:' ), 'xyz', intToStr( id ) ) + endl + msg;
            err^ := err^ + msg;
          end
        ;
      end
    ;


    procedure THerd.debug();
      begin
        dbcout( 'Herd ID: ' + intToStr( id ), true );
        try
          dbcout( 'ProductionTypeName: ' + self._prodTypeName + ' (' + intToStr( self._prodTypeID ) + ')', true );
        except
          dbcout( 'ProductionTypeName (actual): ' + _prodTypeName + ' (' + intToStr( _prodTypeID ) + ')', true );
        end;

        dbcout( 'Size: ' + intToStr(self.initialSize), true );
        dbcout( 'Location: ' + usFloatToStr( lat ) + ' latitude, ' + usFloatToStr( lon ) + ' longitude', true );
        dbcout( 'Status: ' + transitionStateString( self.initialStatus ) + ' (' + intToStr( ord( initialStatus ) ) + ')', true );

        if( -1 = daysLeftInInitialState ) then
          dbcout( 'Days left in initial state: (unspecified)', true )
        else
          dbcout( 'Days left in initial state: ' + intToStr( daysLeftInInitialState ), true )
        ;

        dbcout( 'Updated: ' + boolToStr( updated ), true );
        dbcout( endl, true );
      end
    ;


    function THerd.briefInfo(): string;
      begin
        result :=
          'Unit ' + intToStr( id ) + ': '
          + 'Type: ' + self.prodTypeName + '  '
          + 'Location: Lat ' + usFloatToStr( lat ) + ', Lon ' + usFloatToStr( lon ) + endl
          + 'Size: ' + intToStr( initialSize ) + '  '
          + 'Disease state: ' + transitionStateString( self.simulatedStatus ) + '  '
          + 'Apparent status: ' + apparentStatusString( self.apparentStatus )
        ;
      end
    ;
  //---------------------------------------------------------------------------
    


  //---------------------------------------------------------------------------
  // THerd: Functions for simulation in progress
  //---------------------------------------------------------------------------
    procedure THerd.initializeAllOutputRecords(); // at start of simulation
      begin
        self.prodType.setInitialDailyRecords( initialSize, initialStatus );
        _simInfections := 0;
        _simDetections := 0;
        _simDestructions := 0;
        _simVaccinations := 0;
      end
    ;


    procedure THerd.prepareForIteration(); // at start of each iteration
      begin
        _simulatedStatus := self.initialStatus;
        _zoneLevel := -1;

        _itInfections := 0;
        _itDetections := 0;
        _itDestructions := 0;
        _itVaccinations := 0;

        case self.initialStatus of
          tsDestroyed: self.apparentStatus := asDestroyed;
          tsVaccineImmune: self.apparentStatus := asVaccinated;
          else self.apparentStatus := asUnknown;
        end;
      end
    ;


    procedure THerd.prepareForDay(); // at start of each day
      begin
        // Nothing to do here...
      end
    ;


    procedure THerd.setSimulatedStatus(
          val: TTransitionState;
          updatePTOutputCounts: boolean
        );
      begin
        if( updatePTOutputCounts ) then
          begin
            self.prodType.updateDailyRecordsProdType(
              initialSize,
              _simulatedStatus,
              val
            );
          end
        ;

        _simulatedStatus := val;
      end
    ;

  
    procedure THerd.infectByMechanism( infMech: string; day: integer );
      begin
        // Since initial infection occurs before day 0, remember to
        // treat initial infection differently...
        if( 'initially infected' = infMech ) then
          inc( _simInfections )
        else
          inc( _itInfections )
        ;

        self.prodType.addInfectedByMechanism( initialSize, infMech, day );

        //----------------------------
        // Debugging code
        //----------------------------
        //if( 20 <= self.id ) and ( 69 >= self.id ) then dbcout( 'Herd ' + intToStr( self.id ) + ' was infected' );
        //----------------------------
      end
    ;


    procedure THerd.exposeByMechanism( mechanism: string );
      begin
        self.prodType.addExposedByMechanism( initialSize, mechanism );
      end
    ;


    function THerd.detect( day: integer ): boolean;
      begin
        _apparentStatus := asDetected;
        inc( _itDetections );
        result := self.prodType.addDetection( initialSize, day );
      end
    ;


    function THerd.destroyForReason( reason: string; day: integer ): boolean;
      begin
        dbcout( '--- Destroying herd ' + intToStr( self.id) + ' for ' + reason, DBHERD );

        if( asDetected = _apparentStatus ) then
          self.prodType.decrementApparentInfectiousUnits()
        ;

        _apparentStatus := asDestroyed;
        inc( _itDestructions );

        if( 1 < _itDestructions ) then
          raise exception.Create( 'Herd ' + intToStr( self.id) + ' has been destroyed multiple times during an iteration.' )
        ;

        result := self.prodType.addDestructionEvent( initialSize, reason, day );
      end
    ;


    function THerd.vaccinateForReason( reason: string; day: integer ): boolean;
      begin
        _apparentStatus := asVaccinated;
        inc( _itVaccinations );
        result := self.prodType.addVaccinationEvent( initialSize, reason, day );
      end
    ;


    procedure THerd.attemptTraceForReason( mechanism: string );
      begin
        self.prodType.addAttemptedTraceEvent( initialSize, mechanism );
      end
    ;


    procedure THerd.traceForReason( mechanism: string );
      begin
        mechanism := fixup( mechanism );

        if( asDestroyed = _apparentStatus ) then
          // don't change it
        else
          begin
            if( 'direct contact' = mechanism ) then
              _apparentStatus := asTracedDirect
            else if( 'indirect contact' = mechanism ) then
              _apparentStatus := asTracedIndirect
            else
              raise exception.Create( 'Unrecognized trace reason in THerd.traceForReason' )
            ;
          end
        ;

        self.prodType.addTraceEvent( initialSize, mechanism );
      end
    ;

    
    (*
    procedure THerd.setDailyOutput();
      begin
        self.prodType.updateDailyRecords( initialSize, simulatedStatus );
      end
    ;
    *)


    procedure THerd.processIterationRecords( iterationJustCompleted: integer );
      begin
        inc( _simInfections, _itInfections );
        inc( _simDetections, _itDetections );
        inc( _simVaccinations, _itVaccinations );
        inc( _simDestructions, _itDestructions );
      end
    ;
  //---------------------------------------------------------------------------



  //---------------------------------------------------------------------------
  // THerd: Advanced properties
  //---------------------------------------------------------------------------
    procedure THerd.setSimParams( val: TObject );
      begin
        _sim := val;

        // Force a new search for this herd's production type.
        _pt := nil;
        getProdType();

        // Set prod type params.
        self.setProdTypeName( _pt.productionTypeDescr );
        self.setProdTypeID( _pt.productionTypeID );

        //setUpdated( true );  // Handled by functions above.
      end
    ;


    procedure THerd.setProdType( pt: TProductionType );
      begin
        _pt := pt;
        if( nil <> _pt ) then
          begin
            _prodTypeName := _pt.productionTypeDescr;
            _xmlProdTypeName := encodeXml( _prodTypeName );
            _prodTypeID := _pt.productionTypeID;
          end
        ;

        setUpdated( true );
      end
    ;


    function THerd.getProdType(): TProductionType;
      var
        sim: TSMSimulationInput;
      begin
        dbcout( 'Getting THerd.prodType', DBHERD );

        if( nil <> _pt ) then
          begin
            dbcout( '_pt is not nil!', DBHERD );
            result := _pt;
            exit;
          end
        ;

        dbcout( '_pt was nil.  See if it can be found...', DBHERD );

        // Otherwise, do it the hard way...
        if( nil = _sim ) then
          begin
            raise exception.Create( 'THerd parent is not set' );
            result := nil;
          end
        else
          begin
            sim := _sim as TSMSimulationInput;

            if( -1 <> _prodTypeID ) then
              _pt := sim.ptList.findProdType( _prodTypeID )
            else if( 0 <> length( trim( _prodTypeName ) ) ) then
              _pt := sim.ptList.findProdType( prodTypeName )
            else
              _pt := nil
            ;

            if( nil = _pt ) then
              dbcout( '_pt is nil even after doing a search for ' + _prodTypeName + ' (' + intToStr(_prodTypeID) + ')!', DBHERD )
            ;

            result := _pt;
          end
        ;
      end
    ;

  
    function THerd.getProdTypeID(): integer;
      var
        val: integer;
        sim: TSMSimulationInput;
      begin
        if( PRODTYPEUNASSIGNED <> _prodTypeID ) then
          val := _prodTypeID
        else
          begin
            if( _sim = nil ) then
              begin
                raise Exception.Create( 'THerd parent is not set' );
                val := PRODTYPEUNASSIGNED;
              end
            else
              begin
                sim := _sim as TSMSimulationInput;
                val := sim.getProdTypeID( _prodTypeName );
                _prodTypeID := val;
              end
            ;
          end
        ;

        if( PRODTYPEIDNOTFOUND = val ) then
          raise Exception.Create( 'Cannot find ptID for "' + _prodTypeName +  '" in THerd' )
        ;

        Result := val;
      end
    ;


    procedure THerd.setProdTypeID( val: integer ); begin _prodTypeID := val; setUpdated( true ); end;
  //---------------------------------------------------------------------------



  //---------------------------------------------------------------------------
  // THerd: simple properties
  //---------------------------------------------------------------------------
    procedure THerd.setID( val: integer ); begin _id := val; setUpdated( true ); end;
    function THerd.getID(): integer; begin Result := _id; end;


    procedure THerd.setLat( val: double );
      begin
        if( -90.0 > val ) then
          _lat := -90.0
        else if( 90.0 < val ) then
          _lat := 90.0
        else
          _lat := val
        ;

        setUpdated( true );
      end
    ;


    procedure THerd.setLon( val: double );
      begin
        while( -180.0 > val ) do
          val := val + 360.0
        ;
        while( 180.0 < val ) do
          val := val - 360.0
        ;
        _lon := val;

        setUpdated( true );
      end
    ;


    function THerd.getLat(): double; begin assert( gisValidLat( _lat ) ); Result := _lat; end;
    function THerd.getLon(): double; begin assert( gisValidLon( _lon ) ); Result := _lon; end;

    procedure THerd.setProdTypeName( val: string ); begin _prodTypeName := val; _xmlProdTypeName := encodeXml( val ); setUpdated( true ); end;
    function THerd.getProdTypeName(): string; begin Result := _prodTypeName; end;

    procedure THerd.setInitialSize( val: integer ); begin _initialSize := val; setUpdated( true ); end;
    function THerd.getInitialSize(): integer; begin Result := _initialSize; end;

    procedure THerd.setInitialStatus( val: TTransitionState ); begin _initialStatus := val; setUpdated( true ); end;
    procedure THerd.setApparentStatus( val: TApparentStatus ); begin _apparentStatus := val; setUpdated( true ); end;

    function THerd.getInitialStatus(): TTransitionState; begin Result := _initialStatus; end;
    function THerd.getSimulatedStatus(): TTransitionState; begin result := _simulatedStatus; end;
    function THerd.getApparentStatus(): TApparentStatus; begin result := _apparentStatus; end;

    procedure THerd.setDaysLeftInInitialState( val: integer ); begin _daysLeftInInitialState := val; setUpdated( true ); end;
    function THerd.getDaysLeftInInitialState(): integer; begin result := _daysLeftInInitialState; end;

    procedure THerd.setZoneLevel( val: integer ); begin _zoneLevel := val; {DO NOT set updated flag!} end;
    function THerd.getZoneLevel(): integer; begin result := _zoneLevel; end;
  //---------------------------------------------------------------------------


  //---------------------------------------------------------------------------
  // THerd: Housekeeping properties
  //---------------------------------------------------------------------------
    function THerd.getUpdated(): boolean; begin result := _updated; end;


    procedure THerd.setUpdated( val: boolean );
      begin
        _updated := val;

        if( val and ( nil <> _myList ) ) then
          _myList.setUpdated( true )
        else if( nil = _myList ) then
          dbcout( '_myList is nil!', DBHERD )
        ;
      end
    ;


    function THerd.getIsInDatabase(): boolean; begin  result := _isInDatabase; end;
    procedure THerd.setIsInDatabase( val: boolean ); begin _isInDatabase := val; end;


    function THerd.getRemoveFromDatabase(): boolean; begin result := _removeFromDatabase; end;
    procedure THerd.setRemoveFromDatabase( val: boolean ); begin _removeFromDatabase := val; end;


    function THerd.getOutputUpdated(): boolean;
      begin
        result :=
             ( 0 <> _itInfections )
          or ( 0 <> _itDetections )
          or ( 0 <> _itDestructions )
          or ( 0 <> _itVaccinations )
          or ( _simulatedStatus <> _initialStatus )
          or ( _apparentStatus <> asUnknown )
        ;
      end
    ;
  //---------------------------------------------------------------------------



  //---------------------------------------------------------------------------
  // THerd: output properties
  //---------------------------------------------------------------------------
    procedure THerd.setCumVaccinations( val: integer ); begin _simVaccinations := val; end;
    procedure THerd.setCumInfections( val: integer ); begin _simInfections := val; end;
    procedure THerd.setCumDetections( val: integer ); begin _simDetections := val; end;
    procedure THerd.setCumDestructions( val: integer ); begin _simDestructions := val; end;

    function THerd.getCumVaccinations(): integer; begin result := _simVaccinations; end;
    function THerd.getCumInfections(): integer; begin result := _simInfections; end;
    function THerd.getCumDetections(): integer; begin result := _simDetections; end;
    function THerd.getCumDestructions(): integer; begin result := _simDestructions; end;
  //---------------------------------------------------------------------------
//*****************************************************************************




//*****************************************************************************
// CLASS THerdList
//*****************************************************************************
  //---------------------------------------------------------------------------
  // THerdList: Construction/initialization/destruction
  //---------------------------------------------------------------------------
    constructor THerdList.create();
      begin
        inherited create( true );
        initialize();
      end
    ;


    constructor THerdList.create( db: TSMDatabase; sim: TObject; fn: TObjFnBool1Int = nil );
      var
        db2: TSqlDatabase;
        res: TSqlResult;
        row: TSqlRow;
        q: string;

        h: THerd;

        herdCounter: integer;
        percentComplete: extended;
        percentCompleteInt: integer;
      begin
        _smdb := db;

        dbcout( 'Starting to create herd list', DBHERDLIST );
        inherited create( true );
        initialize();

        // Create the basic herd list first
        //----------------------------------
        db2 := db as TSqlDatabase;

        q := 'SELECT'
            + ' inProductionType.descr AS typeDescr,'
            + ' dynHerd.productionTypeID AS typeID,'
            + ' dynHerd.herdID AS herdID,'
            + ' dynHerd.latitude AS latitude,'
            + ' dynHerd.longitude AS longitude,'
            + ' dynHerd.initialStateCode AS initialStateCode,'
            + ' dynHerd.daysLeftInInitialState AS daysLeftInInitialState,'
            + ' dynHerd.initialSize AS initialSize,'
            + ' dynHerd.finalStateCode AS finalStateCode,'
            + ' dynHerd.finalApparentStateCode AS finalApparentStateCode,'
            + ' dynHerd.cumInfected AS cumInfected,'
            + ' dynHerd.cumDetected AS cumDetected,'
            + ' dynHerd.cumDestroyed AS cumDestroyed,'
            + ' dynHerd.cumVaccinated AS cumVaccinated'
          + ' FROM'
            + ' dynHerd'
          + ' LEFT JOIN'
            + ' inProductionType'
          + ' ON'
            + ' dynHerd.productionTypeID = inProductionType.productionTypeID  ORDER by herdID'
        ;

        res := TSqlResult.create( q, db2 );

        herdCounter := 0;

        row := res.fetchArrayFirst();
        while( nil <> row ) do
          begin
            Application.ProcessMessages();

            h := THerd.create( self );
            h.isInDatabase := true;

            h.setProdTypeName( row.field('typeDescr') );
            h.setProdTypeID( row.field('typeID') );
            h.simParams := sim;
            h.initialSize := row.field('initialSize');
            h.id := row.field('herdID');
            h.lat := row.field('latitude');
            h.lon := row.field('longitude');

            h.initialStatus :=  transitionStateFromCode( row.field('initialStateCode') );

            if( null <> row.field( 'finalStateCode' ) ) then
              h.setSimulatedStatus( transitionStateFromCode( row.field('finalStateCode' ) ), false )
            else
              h.setSimulatedStatus( h.initialStatus, false )
            ;

            if( null <> row.field('daysLeftInInitialState') ) then
              h.daysLeftInInitialState := integer( row.field('daysLeftInInitialState') )
            else
              h.daysLeftInInitialState := -1
            ;

            if( null <> row.field('finalApparentStateCode') ) then
              h.apparentStatus := apparentStatusFromCode( charAt( row.field('finalApparentStateCode'), 0 ) )
            else
              h.apparentStatus := asUnknown
            ;

            if( null <> row.field('cumInfected') ) then h.setCumInfections( row.field('cumInfected') );
            if( null <> row.field('cumDetected') ) then h.setCumDetections( row.field('cumDetected') );
            if( null <> row.field('cumDestroyed') ) then h.setCumDestructions( row.field('cumDestroyed') );
            if( null <> row.field('cumVaccinated') ) then h.setCumVaccinations( row.field('cumVaccinated') );

            h.setUpdated( false );

            // Use Add here instead of append, because append does
            // some other things that are unnecessary here.
            inherited Add( h );

            inc( herdCounter );

            if( nil <> @fn ) then
              begin
                percentComplete := 100 * ( herdCounter/res.numRows );
                if( int( percentComplete ) = percentComplete ) then
                  begin
                    percentCompleteInt := (100*herdCounter) div res.numRows;
                    if( 98 < percentCompleteInt ) then
                      fn( 98 )  // Don't go to 100% until the rest of the function finishes
                    else
                      fn( percentCompleteInt )
                    ;
                    dbcout( percentCompleteInt, DBHERDLIST );
                  end
                ;
                Application.ProcessMessages();
              end
            ;

            row := res.fetchArrayNext();
          end
        ;

        // Set min and max lat/lon from the database
        //-------------------------------------------
        setMinMaxLLFromDB();

        // Clean up
        //----------
        res.Free();

        _updated := false;

        dbcout( 'Herd list created from database updated: ' + boolToStr( updated ), DBHERDLIST );
        dbcout( 'Done creating herd list', DBHERDLIST );

        (sim as TSMSimulationInput).ptList.recountUnits( self );

        if( nil <> @fn ) then fn( 100 );
      end
    ;


    constructor THerdList.create( const src: THerdList; const resetHerdsToInitalState: boolean = false );
      begin
        inherited create( true );
        assign( src, resetHerdsToInitalState );

        dbcout( 'Herd list created from previous list updated: ' + usBoolToText( updated ), DBHERDLIST );
      end
    ;


    procedure THerdList.initialize();
      begin
        _minLat := LAT_LON_UNDEFINED;
        _minLon := LAT_LON_UNDEFINED;
        _maxLat := LAT_LON_UNDEFINED;
        _maxLon := LAT_LON_UNDEFINED;
        _westLon := LAT_LON_UNDEFINED;
        _eastLon := LAT_LON_UNDEFINED;

        _updated := false;
        _removedCount := 0;
      end
    ;


    destructor THerdList.destroy();
      begin
        inherited destroy();
      end
    ;


    procedure THerdList.assign( const src: THerdList; const resetHerdsToInitalState: boolean = false );
      var
        srcH, newH: THerd;
        srcIt: THerdListIterator;
      begin
        _smdb := src._smdb;

        srcIt := THerdListIterator.create( src );

        _removedCount := 0;

        while( nil <> srcIt.current() ) do
          begin
            srcH := srcIt.current();

            newH := THerd.create( srcH, self, resetHerdsToInitalState );
            self.append( newH );

            if( srcH.updated ) then
              _updated := true
            ;
            if( srcH.removeFromDatabase ) then inc( _removedCount );

            srcIt.incr();
          end
        ;

        _minLat := src._minLat;
        _maxLat := src._maxLat;
        _minLon := src._minLon;
        _maxLon := src._maxLon;

        _updated := src._updated;

        srcIt.Free();
      end
    ;
  //---------------------------------------------------------------------------



  //---------------------------------------------------------------------------
  // THerdList: Housekeeping
  //---------------------------------------------------------------------------
    // FIX ME: could this process be made more efficient?
    procedure THerdList.resetSim( sim: TObject; recordUpdate: boolean = true );
      var
        h: THerd;
        it: THerdListIterator;
      begin
        it := THerdListIterator.create( self );

        while( nil <> it.current() ) do
          begin
            h := it.current();
            h.simParams := sim;

            if( not( recordUpdate ) ) then h.setUpdated( false );

            it.incr();
          end
        ;

        if( not( recordUpdate ) ) then _updated := false;

        it.Free();
      end
    ;


    procedure THerdList.removeProductionType( ptID: integer );
      var
        h: THerd;
        i: integer;
        start: integer;
      begin
        start := 0;

        repeat
          for i := start to self.count - 1 do
            begin
              h := self.at(i);
              if( ptID = h.prodTypeID ) then
                begin
                  start := i;
                  dbcout( 'Removing herd at position ' + intToStr( i ), DBHERDLIST );
                  remove( h );
                  break;
                end
            end
          ;
          dbcout( 'i = ' + intToStr( i ), DBHERDLIST );
          dbcout( 'self.count = ' + intToStr( self.Count ), DBHERDLIST );
        until self.Count = i;
      end
    ;


    procedure THerdList.setDB( db: TSMDatabase );
      begin
        _smdb := db;
      end
    ;


    function THerdList.getUpdated(): boolean;
      begin
        result := _updated;
      end
    ;


    procedure THerdList.setUpdated( val: boolean );
      begin
        //dbcout( 'Setting THerdList.updated to ' + boolToStr( val ) );
        _updated := val;
      end
    ;


    {*
      Indicated herd will be removed from list the next time the database is populated.
    }
    procedure THerdList.scheduleHerdRemoval( herdIndex: integer );
      begin
        self.at( herdIndex ).setRemoveFromDatabase( true );
        inc( _removedCount );
        _updated := true;
      end
    ;
  //---------------------------------------------------------------------------



  //---------------------------------------------------------------------------
  // THerdList: Text import
  //---------------------------------------------------------------------------
    function THerdList.importFromFile(
          fileName: string;
          fileFormat: integer;
          fnPrimaryProgress: TObjFnBool1Int = nil;
          fnSecondaryProgress: TObjFnBool1Int = nil;
          fnProgressMessage: TObjFnVoid1String = nil;
          progressStepPercent: integer = 0
        ): boolean;
      begin
        case fileFormat of
          XML_FILE_FORMAT:
            result := importXML( fileName )
          ;
          CSV_FILE_FORMAT:
            result := importCSV(
              fileName,
              fnPrimaryProgress,
              fnSecondaryProgress,
              fnProgressMessage,
              progressStepPercent
            )
          ;

          else
            raise exception.Create( 'Unsupported file format in THerdList.importFromFile()' )
          ;
        end;
      end
    ;


    function THerdList.importCSV(
          fileName: string;
          fnPrimaryProgress: TObjFnBool1Int = nil;
          fnSecondaryProgress: TObjFnBool1Int = nil;
          fnProgressMessage: TObjFnVoid1String = nil;
          progressStepPercent: integer = 0
        ): boolean;
      var
        h: THerd;
        csv: TCSVContents;
        colDict: TQStringLongIntMap;
        i: integer;
        tmp: string;

        total: integer;

        lastCompleteRow: integer;
      begin
        // First step: read and parse the file
        //-------------------------------------
        if( nil <> @fnProgressMessage ) then fnProgressMessage( 'Reading file contents...' );

        csv := TCSVContents.createFromFile( fileName, true );

        if( not( csv.parseSuccess ) ) then
          begin
            if( nil <> @fnPrimaryProgress ) then fnPrimaryProgress( 100 );
            if( nil <> @fnSecondaryProgress ) then fnSecondaryProgress( 100 );
            csv.Free();
            result := false;
            exit;
          end
        else
          dbcout( 'CSV was successfully parsed', DBHERDLIST )
        ;

        if( nil <> @fnSecondaryProgress ) then fnSecondaryProgress( 25 );


        // Second step: check header row
        //-------------------------------
        if( nil <> @fnProgressMessage ) then fnProgressMessage( 'Checking file format...' );

//        colDict := TIntegerDictionary.create();
        colDict := TQStringLongIntMap.create();

        // FIX ME: this search could be a bit more robust
        // (e.g. don't allow duplicate column names)
        for i := 0 to csv.columnCount - 1 do
          begin
            dbcout( csv.header(i), DBHERDLIST );
            tmp := fixup( csv.header(i) );
            if
              ( 'id' = tmp )
            or
              ( 'herdid' = tmp )
            or
              ( '''id' = tmp )
            or
              ( 'herd id' = tmp )
            or
              ( 'unitid' = tmp )
            or
              ( 'unit id' = tmp )
            then
              colDict['id'] := i
            ;

            if
              ( 'productiontype' = tmp )
            or
              ( 'productiontypeid' = tmp )
            or
              ( 'production type' = tmp )
            or
              ( 'production type id' = tmp )
            then
              colDict['productionType'] := i
            ;

            if
              ( 'herdsize' = tmp )
            or
              ( 'initialsize' = tmp )
            or
              ( 'herd size' = tmp )
            or
              ( 'initial size' = tmp )
            or
              ( 'size' = tmp )
            or
              ( 'unitsize' = tmp )
            or
              ( 'unit size' = tmp )
            then
              colDict['herdSize'] := i
            ;

            if( ( 'lat' = tmp ) or ( 'latitude' = tmp ) ) then colDict['lat'] := i;

            if
              ( 'lon' = tmp )
            or
              ( 'longitude' = tmp )
            or
              ( 'long' = tmp )
            then
              colDict['lon'] := i
            ;

            if( ( 'status' = tmp ) or ( 'initialstatecode' = tmp ) ) then colDict['status'] := i;
            if( ( 'daysleftinstatus' = tmp ) or ( 'daysleftininitialstate' = tmp ) ) then colDict['daysLeftInStatus'] := i;
          end
        ;

        // Check that all required columns are present
        // (daysLeftInStatus is optional)
        if not(
          colDict.HasKey('id')
        and
          colDict.HasKey('productionType')
        and
          colDict.HasKey('herdSize')
        and
          colDict.HasKey('lat')
        and
          colDict.HasKey('lon')
        and
          colDict.HasKey('status') )
        then
          begin
            if( nil <> @fnPrimaryProgress ) then fnPrimaryProgress( 100 );
            if( nil <> @fnSecondaryProgress ) then fnSecondaryProgress( 100 );
            dbcout( 'Field is missing', DBHERDLIST );
            csv.Free();
            colDict.Free();
            result := false;
            exit;
          end
        else
          dbcout( 'Fields are OK', DBHERDLIST )
        ;

        if( nil <> @fnSecondaryProgress ) then fnSecondaryProgress( 50 );


        // Third step: Read the individual rows
        //--------------------------------------
        if( nil <> @fnProgressMessage ) then fnProgressMessage( 'Creating units from file...' );

        total := csv.rowCount;

        lastCompleteRow := 0;

        try
          try
            for i := 0 to csv.rowCount - 1 do
              begin
                Application.ProcessMessages();

                lastCompleteRow := i;

                if( nil <> @fnPrimaryProgress ) then
                  begin
                    if( 0 = ( i mod 100 ) ) then fnPrimaryProgress( round( ( i/total) * 99 ) );
                  end
                ;
                  
                if( not( csv.rowIsEmpty( i ) ) ) then
                  begin
                    h := THerd.create( self );
                    h.id := myStrToInt( csv.value( colDict['id'], i ) );

                    if( -1 = myStrToInt( csv.value( colDict['productionType'], i ), -1, false ) ) then
                      h.setProdTypeName( trim( csv.value( colDict['productionType'], i ) ) )
                    else
                      h.setProdTypeID( myStrToInt( csv.value( colDict['productionType'], i ) ) )
                    ;

                    h.initialSize := myStrToInt( csv.value( colDict['herdSize'], i ) );

                    h.lat := usStrToFloat( csv.value( colDict['lat'], i ) );
                    h.lon := usStrToFloat( csv.value( colDict['lon'], i ) );

                    if( -1 = myStrToInt( csv.value( colDict['status'], i ), -1, false ) ) then
                      h.initialStatus := transitionStateFromCode( trim( csv.value( colDict['status'], i ) ) )
                    else
                      h.initialStatus := TTransitionState( myStrToInt( csv.value( colDict['status'], i ) ) )
                    ;

                    if( colDict.HasKey('daysLeftInStatus') ) then
                      begin
                        if( '' <> csv.value( colDict['daysLeftInStatus'], i ) ) then
                          h.daysLeftInInitialState := myStrtoInt( csv.value( colDict['daysLeftInStatus'], i ) )
                        else
                          h.daysLeftInInitialState := -1
                        ;
                      end
                    else
                      h.daysLeftInInitialState := -1
                    ;

                    self.append( h );
                  end
                ;
              end
            ;

            if( nil <> @fnPrimaryProgress ) then fnPrimaryProgress( 100 );
            if( nil <> @fnSecondaryProgress ) then fnSecondaryProgress( 75 );

            dbcout( 'Rows were successfully processed', DBHERDLIST );
            result := true;
          except
            dbcout( 'Exception occurred while processing rows, at or around row ' + intToStr( lastCompleteRow ), DBHERDLIST );
            if( nil <> @fnPrimaryProgress ) then fnPrimaryProgress( 100 );
            if( nil <> @fnSecondaryProgress ) then fnSecondaryProgress( 100 );
            result := false;
          end;
        finally
          colDict.Free();
          csv.Free();
        end;

      end
    ;


    function THerdList.importXML( const herdFileName: string ): boolean;
      var
        Converter: TXMLConvert;
        ret_val: boolean;
      begin
        ret_val := false;

        if ( length( herdFileName ) > 0 ) then
          begin
            Converter := TXMLConvert.create(herdFileName, '', nil );
            ret_val := Converter.ReadHerdXML( self );
            Converter.Destroy();
          end;

        result := ret_val;
      end
    ;
  //---------------------------------------------------------------------------



  //---------------------------------------------------------------------------
  // THerdList: Text export
  //---------------------------------------------------------------------------
  (*
    function THerdList.ssXml(): string;
      var
        h: THerd;
        it: THerdListIterator;
      begin
        result := '<?xml version="1.0" encoding="UTF-16" ?>' + endl;
        result := result + '<herds>' + endl + endl;

        it := THerdListIterator.create( self );

        while( nil <> it.current() ) do
          begin
            h := it.current();

            if( not( h.removeFromDatabase ) ) then
              result := result + h.ssXml() + endl
            ;

            it.incr();
          end
        ;

        result := result +'</herds>' + endl;

        it.Free();
      end
    ;
  *)

    {*
    WARNING: this function will attempt to overwrite an existing file without notice.
    FIX ME: no error checking is done.
    }
    function THerdList.writeXMLFile( fileName: string; errMsg: PString = nil ): boolean;
      var
        xmlFile: TextFile;
        h: THerd;
        it: THerdListIterator;
      begin
        try
          assignUnicode( xmlFile, fileName );
          rewrite( xmlFile );

          writeln( xmlFile, '<?xml version="1.0" encoding="UTF-16" ?>' + endl );
          writeln( xmlFile,'<herds>' + endl );

          it := THerdListIterator.create( self );

          while( nil <> it.current() ) do
            begin
              h := it.current();

              if( not( h.removeFromDatabase ) ) then
                writeln( xmlFile, h.ssXML() );
              ;

              it.incr();
            end
          ;

          writeln( xmlFile, '</herds>' );

          closeFile( xmlFile );
          result := true;

          it.Free();
        except
          result := false;
        end;
      end
    ;


    function THerdList.writeCSVFile(
          fileName: string;
          writeProdTypeName: boolean;
          writeInitialStateChar: boolean;
          errMsg: PString = nil
        ): boolean;
      var
        csvFile: TextFile;
        h: THerd;
        sep: string;
      begin
        sep := ',';

        try
          assignFile( csvFile, fileName );
          rewrite( csvFile );

          writeln( csvFile, 'UnitID' + sep + 'ProductionType' + sep + 'UnitSize' + sep + 'Lat' + sep + 'Lon' + sep + 'Status' + sep + 'DaysLeftInStatus' );

          h := self.first();
          while( nil <> h ) do
            begin
              if( not( h.removeFromDatabase ) ) then
                writeln( csvFile, h.csvText( sep, writeProdTypeName, writeInitialStateChar ) )
              ;
              h := self.next();
            end
          ;

          closeFile( csvFile );
          result := true;
        except
          result := false;
        end;

      end
    ;
  //---------------------------------------------------------------------------



  //---------------------------------------------------------------------------
  // THerdList: Database population
  //---------------------------------------------------------------------------
    procedure THerdList.populateDatabase( db: TSMDatabase; fnProgress: TObjFnBool1Int = nil );
      var
        h: THerd;
        q: string;
        ptErrors: TCStringList;
//        ptIDs: TIntegerDictionary;
        ptIDs: TQStringLongIntMap;
        res: TSqlResult;
        row: TSqlRow;
        db2: TSqlDatabase;
        success, failure: integer;
        ptID: string;

        taskTotal: integer;
        taskCounter: integer;

        vDict: TQStringVariantMap;

        idCounter: integer;
      begin
        vDict := TQStringVariantMap.Create();
//        ptIDs := TIntegerDictionary.Create();
        ptIDs := TQStringLongIntMap.create();
        ptErrors := TCStringList.create();
        db2 := db as TSqlDatabase;

        success := 0;
        failure := 0;

        dbcout( intToStr( count ) + ' herds will be put into the database', DBHERDLIST );

        // Get the production type IDs that correspond to the production type names
        res := TSqlResult.create( 'SELECT [descr], [productionTypeID] FROM [inProductionType]', db2 );

        row := res.fetchArrayFirst();
        while row <> nil do
          begin
            ptIds[intToStr(row.field(1) )] := row.field(1);
            row := res.fetchArrayNext();
          end
        ;

        res.Free();

        // We iterate over the list 4(!) times in this function, but only 3 take any time.
        taskTotal := self.Count * 3;
        taskCounter := 0;

        // Iterate over the list removing any items that should be removed.
        // Remember to remove them from the list as well as the database.
        // Doing this first might help to prevent ID clashes later on.
        //------------------------------------------------------------------
        db.startQuickUpdate( 'dynHerd', 'dynHerd_PK' );

        h := self.first();
        while( h <> nil ) do
          begin
            Application.processMessages();
            inc( taskCounter );

            // FIX ME: explore the possibility of a "QuickDelete"-type of method.
            if( h.removeFromDatabase ) then
              begin
                db.quickDelete( 'herdID', h.id );

                // Don't forget to remove the herd from the list itself.
                delete( currentPosition );
                dec( _removedCount );

                h := self.current(); // Remember that everything in the list cascades up after deletion.
              end
            else
              h := self.next()
            ;

            if( 0 = ( taskCounter mod 25 ) ) then
              if( nil <> @fnProgress ) then fnProgress( (100*taskCounter) div taskTotal )
            ;
          end
        ;

        assert( ( 0 = _removedCount ), '_removedCount is screwed up in THerdList.populateDatabase' );

        // Iterate over the herd list, looking for herds that are already in the database.
        // Check to be sure that the specified production type is present in the database.
        // If so, UPDATE THE EXISTING RECORD in the database. If not, record a failure.
        //--------------------------------------------------------------------------------
        h := self.first();
        while( h <> nil ) do
          begin
            Application.processMessages();
            inc( taskCounter );

            if( h.isInDatabase and h.updated and not( h.removeFromDatabase ) ) then
              begin
                dbcout( '--- Herd is in database and needs updating: ' + intToStr( h.id ), DBHERDLIST );

                ptID := intToStr( h.prodType.productionTypeID );

                if( ptIDs.HasKey( ptID ) ) then
                  begin
                    db.quickUpdate( h.id, 'productionTypeID', h.prodType.productionTypeID );
                    db.quickUpdate( 'latitude', h.lat );
                    db.quickUpdate( 'longitude', h.lon );
                    db.quickUpdate( 'initialStateCode', transitionStateCode( h.initialStatus ) );
                    db.quickUpdate( 'daysLeftInInitialState', h.daysLeftInInitialState );
                    db.quickUpdate( 'initialSize', h.initialSize );

                    h.setUpdated( false );

                    inc( success );
                  end
                else
                  begin
                    dbcout( 'ptID is missing', DBHERDLIST );
                    if( ptErrors.indexOf( ptID ) = -1 ) then ptErrors.append( ptID );
                    inc( failure );
                  end
                ;

              end
            ;
            h := self.next();

            if( 0 = ( taskCounter mod 25 ) ) then
              if( nil <> @fnProgress ) then fnProgress( (100*taskCounter) div taskTotal )
            ;
          end
        ;

        db.endQuickUpdate();

        // FIX ME: add the 4th progress check here.
        idCounter := db.lastHerdID();
        if( 0 = idCounter ) then
          begin
            h := self.first();
            while( nil <> h ) do
              begin
                if( h.id > idCounter ) then idCounter := h.id;
                h := self.next();
              end
            ;
          end
        ;


        // Iterate over the list again, looking for herds that are not in the database.
        // Check to be sure that the specified production type is present in the database.
        // If so, ADD A NEW RECORD to the database. If not, record a failure.
        //--------------------------------------------------------------------------------
        db.makeTemporaryHerdTable();

        db.startQuickInsert( 'dynHerd2' );

        h := self.first();
        while( h <> nil ) do
          begin
            Application.processMessages();
            inc( taskCounter );

            if( not( h.isInDatabase ) and not( h.removeFromDatabase ) ) then
              begin
                dbcout( 'Herd is not in database: ' + intToStr( h.id ), DBHERDLIST );

                ptID := intToStr( h.prodTypeID );
                if( ptIDs.HasKey( ptID ) ) then
                  begin
                    vDict.Clear();

                    if( 0 < h.id ) then
                      vDict['herdID'] := h.id
                    else
                      begin
                        inc( idCounter );
                        vDict['herdID'] := idCounter;
                      end
                    ;

                    vdict['productionTypeID'] := ptID;
                    vdict['latitude'] := h.lat;
                    vdict['longitude'] := h.lon;
                    vdict['initialStateCode'] := transitionStateCode( h.initialStatus );
                    vdict['initialSize'] := h.initialSize;

                    h.setID( (strToInt( db.quickInsert( vDict, 'herdID' ) ) ) );
                    h.isInDatabase := true;
                    h.setUpdated( false );
                      
                    inc( success );
                  end
                else
                  begin
                    dbcout( 'ptID is missing', DBHERDLIST );
                    if( ptErrors.indexOf( ptID ) = -1 ) then ptErrors.append( ptID );
                    inc( failure );
                  end
                ;
              end
            ;

            h := self.next();

            if( 0 = ( taskCounter mod 25 ) ) then
              if( nil <> @fnProgress ) then fnProgress( (100*taskCounter) div taskTotal )
            ;
          end
        ;

        db.endQuickInsert();

        db.mergeHerdTables();


        // Alert the user to any failures.
        //--------------------------------
        if( failure > 0 ) then
          begin
            q := intToStr( failure ) + ' herd records out of ' + intToStr( failure + success ) + ' could not be imported.';

            if( ptErrors.Count > 0 ) then
              q  := q + endl
              + 'The following production types are not present in the scenario file:'
              + endl
              + ptErrors.Text
            ;
          end
        else
          q := intToStr( success ) + ' herd records successfully imported.';
        ;

        _updated := false;

        setMinMaxLLFromDB();

        if( nil <> @fnProgress ) then fnProgress( 100 );
        freeAndNil( ptErrors );
        freeAndNil( ptIDs );
        freeAndNil( vDict );
      end
    ;
  //---------------------------------------------------------------------------



  //---------------------------------------------------------------------------
  // THerdList: Validation and debugging
  //---------------------------------------------------------------------------
    function THerdList.isValid( err: PString = nil ): boolean;
      var
        h: THerd;
        diseaseFound: boolean;
      begin
        result := true;
        diseaseFound := false;

        if( self.Count < 1 ) then
          begin
            if( nil <> err ) then err^ := err^ + tr( 'There are no units in this scenario.' ) + endl;
            result := false;
          end
        else
          begin
            h := self.first();
            while( h <> nil ) do
              begin
                if( not( h.isValid( err ) ) ) then result := false;
                if( h.initialStatus in [tsLatent, tsSubClinical, tsClinical] ) then diseaseFound := true;
                h := self.next();
              end
            ;

            if( not( diseaseFound ) ) then
              begin
                if( nil <> err ) then
                  err^ := err^ + tr( 'None of the current units is capable of transmitting disease: no disease spread can occur.' ) + endl
                ;
                result := false;
              end
            ;
          end
        ;
      end
    ;


    procedure THerdList.debug();
      var
        h: THerd;
      begin
        dbcout( '=========BEGIN HERDS: ' + intToStr( self.Count ) + endl, true );

        h := self.first();

        while( h <> nil ) do
          begin
            h.debug();
            h := self.next();
          end
        ;

        dbcout( '=========END HERDS', true );
      end
    ;
  //---------------------------------------------------------------------------



  //---------------------------------------------------------------------------
  // THerdList: Simulation outputs
  //---------------------------------------------------------------------------
    procedure THerdList.prepareForIteration( iteration: integer );
      var
        it: THerdListIterator;
      begin
        it := THerdListIterator.create( self );

        while( nil <> it.current() ) do
          begin
            it.current().prepareForIteration();
            it.incr();
          end
        ;

        it.Free();
      end
    ;


    procedure THerdList.initializeAllOutputRecords();
      var
        it: THerdListIterator;
      begin
        it := THerdListIterator.create( self );

        while( nil <> it.current() ) do
          begin
            it.current().initializeAllOutputRecords();
            it.incr();
          end
        ;

        it.Free();
      end
    ;


    procedure THerdList.prepareForDay( day: integer );
      //var
        //h: THerd;
      begin
        // There's not really anything to do in here at the moment...
        (*
        h := self.first();
        while( nil <> h ) do
          begin
            h.prepareForDay();
            h := self.next();
          end
        ;
        *)
      end
    ;


    (*
    // FIX ME: use a list iterator here, as soon as there is one...
    procedure THerdList.processDailyRecords();
      var
        h: THerd;
      begin
        h := self.first();
        while( nil <> h ) do
          begin
            h.setDailyOutput();
            h := self.next();
          end
        ;
      end
    ;
    *)


    procedure THerdList.processIterationRecords( db: TSMDatabase; iterationJustCompleted: integer );
      var
        h: THerd;
        it: THerdListIterator;
      begin
        _smdb.clearExistingFinalHerdStates();

        _smdb.startQuickUpdate( 'dynHerd', 'dynHerd_PK' );

        it := THerdListIterator.create( self );

        while( nil <> it.current() ) do
          begin
            h := it.current();

            h.processIterationRecords( iterationJustCompleted );

            if( h.outputUpdated ) then
              begin
                _smdb.quickUpdate( h.id, 'finalStateCode', transitionStateCode( h.simulatedStatus ) );
                _smdb.quickUpdate( 'finalApparentStateCode', apparentStatusCode( h.apparentStatus ) );
                _smdb.quickUpdate( 'cumInfected', h.cumInfections );
                _smdb.quickUpdate( 'cumDetected', h.cumDetections );
                _smdb.quickUpdate( 'cumDestroyed', h.cumDestructions );
                _smdb.quickUpdate( 'cumVaccinated', h.cumVaccinations );
              end
            ;

            it.incr();
          end
        ;

        _smdb.endQuickUpdate();

        it.Free();
      end
    ;
  //-----------------------------------------------------------------------------



  //---------------------------------------------------------------------------
  // THerdList: Simple properties
  //---------------------------------------------------------------------------
    procedure THerdList.setMinMaxLLFromDB();
      var
        db2: TSqlDatabase;
        res: TSqlResult;
        row: TSqlRow;
        q: string;
      begin
        _maxLat := LAT_LON_UNDEFINED;
        _minLat := LAT_LON_UNDEFINED;
        _maxLon := LAT_LON_UNDEFINED;
        _minLon := LAT_LON_UNDEFINED;
        _eastLon := LAT_LON_UNDEFINED;
        _westLon := LAT_LON_UNDEFINED;

        db2 := _smdb as TSqlDatabase;
        res := TSqlResult.create( db2 );

        // Set min and max lat/lon from the database
        //-------------------------------------------
        q := 'SELECT'
          + '   MAX( latitude ) AS maxLat,'
          + '   MIN( latitude ) AS minLat,'
          + '   MAX( longitude ) AS maxLon,'
          + '   MIN( longitude ) AS minLon'
          + ' FROM dynHerd'
        ;

        res.runQuery( q );
        row := res.fetchArrayFirst();
        
        if( null <> row.field('maxLat') ) then _maxLat := row.field('maxLat');
        if( null <> row.field('minLat') ) then _minLat := row.field('minLat');
        if( null <> row.field('maxLon') ) then _maxLon := row.field('maxLon');
        if( null <> row.field('minLon') ) then _minLon := row.field('minLon');

        res.Free();
      end
    ;


    function THerdList.getMinLat(): double;
      begin
        if( LAT_LON_UNDEFINED = _minLat ) then setMinMaxLL();
        assert( LAT_LON_UNDEFINED <> _minLat );
        result := _minLat;
      end
    ;


    function THerdList.getMaxLat(): double;
      begin
        if( LAT_LON_UNDEFINED = _maxLat ) then setMinMaxLL();
        assert( LAT_LON_UNDEFINED <> _maxLat );
        result := _maxLat;
      end
    ;


    function THerdList.getMinLon(): double;
      begin
        if( LAT_LON_UNDEFINED = _minLon ) then setMinMaxLL();
        assert( LAT_LON_UNDEFINED <> _minLon );
        result := _minLon;
      end
    ;


    function THerdList.getMaxLon(): double;
      begin
        if( LAT_LON_UNDEFINED = _maxLon ) then setMinMaxLL();
        assert( LAT_LON_UNDEFINED <> _maxLon );
        result := _maxLon;
      end
    ;


    procedure THerdList.setMinMaxLL();
      var
        h: THerd;
        it: THerdListIterator;

        adjMinLon, adjMaxLon: double;
        presLonW, presLonE: double;

        testLon, testLat: double;
      begin
        dbcout( '*** THerdList.setMinMaxLL!', DBHERDLIST );

        it := THerdListIterator.create( self );

        // First, set min's and max's if necessary.
        //-----------------------------------------
        if( 0 = self.Count ) then
          begin
            _maxLat := 1.0;
            _minLat := 0.0;
            _maxLon := 1.0;
            _minLon := 0.0;
          end
        else
          begin
            if
              ( LAT_LON_UNDEFINED = _maxLat )
            or
              ( LAT_LON_UNDEFINED = _maxLon )
            or
              ( LAT_LON_UNDEFINED = _minLat )
            or
              ( LAT_LON_UNDEFINED = _minLon )
            then
              begin
                _maxLat := -1 * LAT_LON_UNDEFINED;
                _maxLon := -1 * LAT_LON_UNDEFINED;

                _minLat := LAT_LON_UNDEFINED;
                _minLon := LAT_LON_UNDEFINED;

                while( nil <> it.current() ) do
                  begin
                    h := it.current();

                    if( h.lat > _maxLat ) then _maxLat := h.lat;
                    if( h.lat < _minLat ) then _minLat := h.lat;
                    if( h.lon > _maxLon ) then _maxLon := h.lon;
                    if( h.lon < _minLon ) then _minLon := h.lon;

                    it.incr();
                  end
                ;

                assert( LAT_LON_UNDEFINED <> _maxLat );
                assert( LAT_LON_UNDEFINED <> _maxLon );
                assert( LAT_LON_UNDEFINED <> _minLat );
                assert( LAT_LON_UNDEFINED <> _minLon );
              end
            ;
          end
        ;

        // Next, try to figure out which lon is "east" and which is "west"
        //-----------------------------------------------------------------

        // Start by making an educated guess about the presumptive west and east lons.

        // The logic here is easier if lon goes from 0 to 360 instead of -180 to 180.
        adjMinLon := minLon;
        adjMaxLon := maxLon;

        if( 0.0 > adjMinLon ) then adjMinLon := adjMinLon + 360;
        if( 0.0 > adjMaxLon ) then adjMaxLon := adjMaxLon + 360;

        // With this transformation applied,
        // the western lon will be the the smaller one, except in the
        // special case where the region of interest spans the
        // prime meridian but not the international date line.
        // We'll deal with this case in a minute.
        if( adjMinLon < adjMaxLon ) then
          begin
            presLonW := minLon;
            presLonE := maxLon;
          end
        else
          begin
            presLonW := maxLon;
            presLonE := minLon;
          end
        ;

        // Now we have an idea about which is "east" and which is "west", but we're not done yet.
        //
        // Our two longitudes split the globe into two pieces.
        // We know that our study area is in one of these pieces, but we don't know which one.
        //
        // Most likely, our study area is within a piece encompassing less than 180 degrees of longitude.
        // If that's the case, our guess so far regarding "east" and "west" are OK.
        //
        // Unfortunately, the study area could be in the "inverse" area,
        // which encompasses more than 180 degrees longitude.
        // If that's the case, we need to reverse our definitions of "east" and "west."
        //
        // There's also the case where the study area encompasses exactly 180 degrees longitude:
        // in this particular instance, "east" and "west" would be completely arbitrary.
        //
        // How do we know which is actually the case?
        //
        // If we have some other point that we know is within the area of interest
        // (say, the location of another herd somewhere between the min and max lons)
        // we could tell for sure what's happening.
        //
        // So let's see if our test point is between our "western" and "eastern" longitudes,
        // as we've defined them.  If it is, we got it right.  If it isn't, we just need to
        // reverse "east" and "west".
        testLat := ( _minLat + _maxLat ) / 2;
        testLon := presLonW;

        // Find a suitable test point...
        it.toFirst();
        while( nil <> it.current() ) do
          begin
            h := it.current();

            if( ( maxLon <> h.lon ) and (minLon <> h.lon ) ) then
              begin
                testLon := h.lon;
                break;
              end
            ;

            it.incr();
          end
        ;

        if( testLon <> presLonW ) then // We've found a suitable test point.
          begin
            // Test the test point.
            if( gisLLBoundedBy( testLat, testLon, northLat, presLonW, southLat, presLonE ) ) then
              begin
                // We guessed right!
                _westLon := presLonW;
                _eastLon := presLonE;
              end
            else
              begin
                // Switch them!
                _westLon := presLonE;
                _eastLon := presLonW;
              end
            ;
          end
        else // There are no points between the min and max lons.  We can't do any more.
          begin
            _westLon := presLonW;
            _eastLon := presLonE;
          end
        ;

        it.Free();
      end
    ;


    function THerdList.getEastLon(): double;
      begin
        if( LAT_LON_UNDEFINED = _eastLon ) then setMinMaxLL();
        assert( LAT_LON_UNDEFINED <> _eastLon );
        result := _eastLon;
      end
    ;


    function THerdList.getWestLon(): double;
      begin
        if( LAT_LON_UNDEFINED = _westLon ) then setMinMaxLL();
        assert( LAT_LON_UNDEFINED <> _westLon );
        result := _westLon;
      end
    ;
  //---------------------------------------------------------------------------



  //---------------------------------------------------------------------------
  // THerdList: Typical list functions
  //---------------------------------------------------------------------------
    function THerdList.getCurrentIndex(): integer;
      begin
        result := _currentIndex;
      end
    ;


    function THerdList.append( dm: THerd ): integer;
      begin
        result := inherited Add( dm );

        if( dm.lat > maxLat ) then _maxLat := dm.lat;
        if( dm.lat < minLat ) then _minLat := dm.lat;

        if( dm.lon > maxLon ) then _maxLon := dm.lon;
        if( dm.lon < minLon ) then _minLon := dm.lon;

        _westLon := LAT_LON_UNDEFINED;
        _eastLon := LAT_LON_UNDEFINED;
      end
    ;


    procedure THerdList.setObject( index: integer; item: THerd );
      begin
        inherited SetItem( index, item );
      end
    ;


    function THerdList.getObject( index: integer ): THerd;
      begin
        result := inherited GetItem( index ) as THerd;
      end
    ;


    function THerdList.at( index: integer ): THerd;
      begin
        result := inherited GetItem( index ) as THerd;
      end
    ;


    procedure THerdList.insert(index: integer; dm: THerd);
      begin
        inherited Insert(index, dm);
      end
    ;


    function THerdList.first() : THerd;
      begin
        _currentIndex := 0;
        if( self.Count = 0 ) then
          result := nil
        else
          result := getObject( _currentIndex )
        ;
      end
    ;


    function THerdList.last() : THerd;
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


    function THerdList.next() : THerd;
      begin
        _currentIndex := _currentIndex + 1;
        if( _currentIndex > (self.Count - 1) ) then
          result := nil
        else
          result := getObject( _currentIndex )
        ;
      end
    ;


    function THerdList.current() : THerd;
      begin
        if( _currentIndex > (self.Count - 1) ) then
          result := nil
        else
          result := getObject( _currentIndex )
        ;
      end
    ;
  //---------------------------------------------------------------------------
//*****************************************************************************



//*****************************************************************************
// CLASS THerdListIterator
//*****************************************************************************
  //---------------------------------------------------------------------------
  // THerdListIterator: Construction/destruction
  //---------------------------------------------------------------------------
    constructor THerdListIterator.create( list: THerdList );
      begin
        _list := list;
        _currentIndex := 0;
      end
    ;

    destructor THerdListIterator.destroy();
      begin
        // Nothing to do in here
        inherited destroy();
      end
    ;
  //---------------------------------------------------------------------------



  //---------------------------------------------------------------------------
  // TProductionTypeListIterator: Basic functions
  //---------------------------------------------------------------------------
    function THerdListIterator.toFirst(): THerd;
      begin
        if( 0 = _list.Count ) then
          begin
            _currentIndex := -1;
            result := nil;
          end
        else
          begin
            _currentIndex := 0;
            result := _list.first();
          end
        ;
      end
    ;

    function THerdListIterator.toLast(): THerd;
      begin
        if( 0 = _list.Count ) then
          begin
            _currentIndex := -1;
            result := nil;
          end
        else
          begin
            _currentIndex := _list.count-1;
            result := _list.last();
          end
        ;
      end
    ;

    function THerdListIterator.current(): THerd;
      begin
        if( _currentIndex < 0 ) or ( _currentIndex > _list.Count -1 ) then
          result := nil
        else
          result := _list.at( _currentIndex )
        ;
      end
    ;

    procedure THerdListIterator.incr();
      begin
        inc( _currentIndex );
      end
    ;

    procedure THerdListIterator.decr();
      begin
        dec( _currentIndex );
      end
    ;

    function THerdListIterator.getCount(): word;
      begin
        result := _list.count;
      end
    ;

    function THerdListIterator.getIsEmpty(): boolean;
      begin
        result := (_list.count = 0 );
      end
    ;
  //---------------------------------------------------------------------------
//*****************************************************************************

end.
