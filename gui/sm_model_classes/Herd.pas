unit Herd;

(*
Herd.pas
---------
Begin: 2005/01/21
Last revision: $Date: 2012-08-14 19:02:12 $ $Author: areeves $
Version number: $Revision: 1.83.4.29 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2005 - 2012 Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

{$INCLUDE Defs.inc}

interface

	uses
    // Standard Delphi units
  	Contnrs,
    SysUtils,
    Graphics,

    // QClasses for Delphi
    QStringMaps,
    QVectors,

    // APHI General Purpose Delphi library
    SqlClasses,
    FunctionPointers,

    // Proj.4 Delphi wrapper
    Proj4,

    // Simple Delphi Expat Wrapper
    Sdew,

    // Application-specific units
    NAADSMLibraryTypes,
    SMDatabase,
    SMSimulationInput,
    ProductionType,
    StatusEnums,
    HerdControlActivities
  ;

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
        _y: double; // y coordinate (latitude) in the projected coordinate system
        _x: double; // x coordinate( longitude) in the projected coordinate system

        _daysInInitialState: integer;
        _daysLeftInInitialState: integer;
        _initialStatus: TNAADSMDiseaseState;
        _diseaseStatus: TNAADSMDiseaseState;
        _controlStatus: TControlStatus; // Don't set this directly except during initalization!
        _detectionStatus: TDetectionStatus; // Don't set this directly except during initialization!
        _previouslyDetected: boolean; // Don't set this directly except during initialization!
        _zoneLevel: integer;

        // Advanced properties
      	_sim: TObject;
        _pt: TProductionType;
        _prodTypeID: integer;

        // Output properties
        _ctrlActivities: THerdControlActivities;

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

        procedure setDaysInInitialState( val: integer );
        function getDaysInInitialState(): integer;

        procedure setDaysLeftInInitialState( val: integer );
        function getDaysLeftInInitialState(): integer;

				function getLat(): double;
				function getLon(): double;

        function getY(): double;
        function getX(): double;

        procedure setInitialStatus( val: TNAADSMDiseaseState );
        function getInitialStatus(): TNAADSMDiseaseState;

        function getDiseaseStatus(): TNAADSMDiseaseState;

        procedure setControlStatus( val: TControlStatus );
        function getControlStatus(): TControlStatus;

        procedure setDetectionStatus( const val: TDetectionStatus );
        function getDetectionStatus(): TDetectionStatus;

        procedure setZoneLevel( val: integer );
        function getZoneLevel(): integer;

        // Advanced properties
        procedure setSimParams( val: TObject );

        procedure setProdTypeID( val: integer );
        function getProdTypeID(): integer;

        //procedure setProdType( pt: TProductionType );  // This function is public: see below.
        function getProdType(): TProductionType;

    	public
        // Construction/destruction
        //--------------------------
      	constructor create( list: THerdList ); overload;
        constructor create( const src: THerd; const list: THerdList = nil; const resetToInitialState: boolean = false ); overload;
        constructor create( list: THerdList; sim: TObject; sdew: TSdew; element: pointer; ptDict: TQStringObjectMap; errMsg: pstring = nil; conductInvProj: boolean = false ); overload;

        destructor destroy(); override;

        procedure assign( const src: THerd; const list: THerdList = nil; const resetToInitialState: boolean = false );

        // Cartographic projection
        //------------------------
        { Projects all herd lat/lon coordinates to the specified projection system. }
        function project(): boolean;

        // Text export
        //------------
        function ssXml( const useProjection: boolean ): string;
        function csvText( const writeProdTypeName, writeInitialStateChar, useProjection: boolean ): string;

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

        procedure changeHerdState( const val: TNAADSMDiseaseState; const day: integer );
        procedure infect( const r: THRDInfect; day: integer );
        procedure expose( const e: THRDExpose );
        procedure attemptTraceForReason( const t: THRDTrace );
        procedure traceForReason( const t: THRDTrace );
        procedure recordTraceOrigin( const t: THRDTrace );
        procedure conductHerdExam( const e: THRDExam );
        procedure conductDiagnosticTest( const t: THRDTest );
        function detect( const d: THRDDetect; day: integer ): boolean;
        procedure queueForDestruction( day: integer );
        procedure queueForVaccination( day: integer );
        function destroyForReason( const r: THRDControl; day:integer ): boolean;
        function vaccinateForReason( const r: THRDControl; day: integer ): boolean;
        procedure cancelVaccination( const r: THRDControl; day: integer );

        procedure processIterationRecords( iterationJustCompleted: integer );

        // Housekeeping properties
        //------------------------
        property updated: boolean read getUpdated;
        property isInDatabase: boolean read getIsInDatabase write setIsInDatabase;
        property removeFromDatabase: boolean read getRemoveFromDatabase;
        property outputUpdated: boolean read getOutputUpdated;

        // Simple properties
        //------------------
        property id: integer read getID write setID;

        // lat/lon are set as a pair to allow proper cartographic projection.
        procedure setLatLon( lat, lon: double );
				property lat: double read getLat;
				property lon: double read getLon;
        property y: double read getY;
        property x: double read getX;

        property prodTypeName: string read getProdTypeName write setProdTypeName;
        property xmlProdTypeName: string read _xmlProdTypeName;

        property actualProdTypeID: integer read _prodTypeID;
        property actualProdTypeName: string read _prodTypeName;

        property initialSize: integer read getInitialSize write setInitialSize;
        property daysInInitialState: integer read getDaysInInitialState write setDaysInInitialState;
        property daysLeftInInitialState: integer read getDaysLeftInInitialState write setDaysLeftInInitialState;

        property initialStatus: TNAADSMDiseaseState read getInitialStatus write setInitialStatus;
        property diseaseStatus: TNAADSMDiseaseState read getDiseaseStatus;
        property controlStatus: TControlStatus read getControlStatus;
        property detectionStatus: TDetectionStatus read getDetectionStatus;

        property zoneLevel: integer read getZoneLevel write setZoneLevel;

        // Advanced properties
        //--------------------
       	property simParams: TObject write setSimParams;
        procedure setProdType( pt: TProductionType );
        property prodTypeID: integer read getProdTypeID;
        property prodType: TProductionType read getProdType;

        property ctrlActivities: THerdControlActivities read _ctrlActivities;
    end
  ;


  {type} THerdList = class( TObjectList )
    protected
      // Basic list operations
      _currentIndex: integer;

      // Housekeeping
      _proj: TProj4;
      _updated: boolean;
      _removedCount: integer;
      _projected: boolean;

      _smdb: TSMDatabase;
      _sim: TSMSimulationInput;

      // Simple properties
      _minLat, _maxLat: double;
      _minLon, _maxLon: double;
      _eastLon, _westLon: double;
      _minX, _maxX: double;
      _minY, _maxY: double;

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

      function getMinX(): double;
      function getMaxX(): double;
      function getMinY(): double;
      function getMaxY(): double;

      function getEastLon(): double;
      function getWestLon(): double;

      { Projects all herd lat/lon coordinates to the specified projection system. }
      function project(): boolean;

      procedure setMinMaxLL(); // This function should only rarely be used.

      // Most of the time, the min and max will be set directly from the database.
      procedure setMinMaxLLFromDB();

      // Calculates min and max projected coordinates based on provided min/max lat/lons.
      procedure setMinMaxXY();

      // Determines which extreme longitude is "east" and which is "west"
      procedure setEastAndWest();

    public
      // Construction/destruction
      //--------------------------
    	constructor create(); overload;
      constructor create( db: TSMDatabase; sim: TObject; fn: TObjFnBool1Int = nil ); overload;
      constructor create( const src: THerdList; const resetHerdsToInitalState: boolean = false ); overload;
      constructor create(
        const xmlFileName: string;
        db: TSMDatabase;
        sim: TObject;
        errMsg: pstring = nil;
        fnPrimaryProgress: TObjFnBool1Int = nil;
        fnProgressMessage: TObjFnVoid1String = nil
      ); overload;

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

      // Cartographic projection
      //------------------------
      { Sets up the projection specified by the string for cartographic projection. }
      function setProjection( const projParams: string; projectHerds: boolean = true ): boolean;

      { Sets up the default system to be used for cartographic projection. }
      class function defaultProjection( const minLat, minLon, maxLat, maxLon: double ): string;

      { Projects all herd lat/lon coordinates to the specified projection system after resetting min and max lat/lons to current values. }
      function reproject(): boolean;

      { Writes projected coordinates to the debug window }
      procedure debugProjected();

      // Text export
      //------------
      // This function produces a potentially huge string, and is currently unused.
      // If a file is to be written, writeXMLFile() is more efficient.
      function ssXml( const useProjection: boolean ): string;

      function writeXMLFile( fileName: string; const useProjection: boolean; errMsg: PString = nil ): boolean;

      function writeCSVFile(
        fileName: string;
        const writeProdTypeName: boolean;
        const writeInitialStateChar: boolean;
        const useProjection: boolean;
        errMsg: PString = nil
      ): boolean;

      // Text import
      //-------------
      function importFromFile(
        fileName: string;
        fileFormat: integer;
        errMsg: pstring = nil;
        fnPrimaryProgress: TObjFnBool1Int = nil;
        fnSecondaryProgress: TObjFnBool1Int = nil;
        fnProgressMessage: TObjFnVoid1String = nil;
        populateDB: boolean = true;
        ids: TQIntegerVector = nil
      ): boolean;

      function importCSV(
        fileName: string;
        errMsg: pstring = nil;
        fnPrimaryProgress: TObjFnBool1Int = nil;
        fnSecondaryProgress: TObjFnBool1Int = nil;
        fnProgressMessage: TObjFnVoid1String = nil;
        ids: TQIntegerVector = nil
      ): boolean;

      function importXML(
        const herdFileName: string;
        errMsg: pstring = nil;
        fnPrimaryProgress: TObjFnBool1Int = nil;
        fnProgressMessage: TObjFnVoid1String = nil;
        populateDB: boolean = true;
        ids: TQIntegerVector = nil
      ): boolean;

      // Database population
      //--------------------
      // Returns true on success, otherwise false.
      function populateDatabase( db: TSMDatabase; fnProgress: TObjFnBool1Int = nil; errMsg: pstring = nil ): boolean;

      // Validation and debugging
      //--------------------------
      function isValid( err: PString = nil ): boolean;
      procedure debug();
      procedure invProjectDebug(); /// Used to double-check that inverse cartographic projection is OK.

      // Housekeeping
      //-------------
      procedure resetSim( sim: TObject; recordUpdate: boolean = true );

      procedure removeProductionType( ptID: integer );

      procedure setDB( db: TSMDatabase );

      //This is not the same as resetSim.
     //  It only sets the herdlist's internal storage of the sim object for use by the list only!
      procedure setSim( sim: TSMSimulationInput );


      { Indicated herd will be removed from list the next time the database is populated. }
      procedure scheduleHerdRemoval( herdIndex: integer );

      // For simulation outputs
      //-----------------------
      procedure initializeAllOutputRecords(); // upon sim start
      procedure prepareForIteration( iteration: integer ); // upon iteration start
      procedure prepareForDay( day: integer ); // upon day start.  There is nothing to do on day end.
      procedure processIterationRecords( db: TSMDatabase; iterationJustCompleted: integer ); // upon iteration end

      // Cartographic projection
      //------------------------
      { Sets up the default system to be used for cartographic projection. }
      //class function defaultProjection( const minLat, minLon, maxLat, maxLon: double ): string;

      // Housekeeping properties
      //------------------------
      property updated: boolean read getUpdated write setUpdated;
      property removedCount: integer read _removedCount;
      property projection: TProj4 read _proj;
      property isProjected: boolean read _projected; /// Did cartographic projection occur properly?

      // Simple properties
      //------------------
      property minLat: double read getMinLat;
      property maxLat: double read getMaxLat;
      property minLon: double read getMinLon;
      property maxLon: double read getMaxLon;

      property minX: double read getMinX;
      property maxX: double read getMaxX;
      property minY: double read getMinY;
      property maxY: double read getMaxY;

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
    // Standard Delphi units
    StrUtils,
  	Classes,
    Variants,
    Forms, // for Application.ProcessMessages()
    Math,

    // APHI General Purpose Delphi library
    MyStrUtils,
    MyDialogs,
    DebugWindow,
    WindowsUtils,
    QLists,
    CStringList,
    CsvParser,
    BasicGIS,
    I88n,
    UnicodeDev,
    Points,

    // Application-specific units
    SMScenario
  ;

//*****************************************************************************
// CLASS THerd
//*****************************************************************************
  // --------------------------------------------------------------------------
  // THerd: Creation/initialization/destruction
  // --------------------------------------------------------------------------
    constructor THerd.create( list: THerdList );
      begin
        dbcout( 'Creating empty herd', DBHERD );
        inherited create();
        _myList := list;
        initialize();
      end
    ;


    constructor THerd.create( const src: THerd; const list: THerdList = nil; const resetToInitialState: boolean = false );
      begin
        dbcout( 'Creating copy of herd' , DBHERD );
        inherited create();
        _ctrlActivities := THerdControlActivities.create();
        assign( src, list, resetToInitialState );
      end
    ;

    constructor THerd.create(
      list: THerdList;
      sim: TObject;
      sdew: TSdew;
      element: pointer;
      ptDict: TQStringObjectMap;
      errMsg: pstring = nil;
      conductInvProj: boolean = false
    );
      var
        e, ee: pointer;
        lat, lon, x, y: double;
        prodTypeName: string;
        pt: TProductionType;
        llr: RLatLon;
      begin

        inherited create();
        _myList := list;
        initialize();

        self.isInDatabase := false;
        lat := LAT_LON_UNDEFINED;
        lon := LAT_LON_UNDEFINED;
        x := NaN;
        y := NaN;

        // Don't use the property here: it has unpleasant side effects.
        // FIX ME: the getSim/setSim mechanism should be reviewed.  It's not very efficient, and may not even be necessary any more.
        _sim := sim;
        //self.simParams := sim;


        e := sdew.GetElementByName( element, 'id' );
        if( nil <> e ) then
          id := myStrToInt( sdew.GetElementContents( e ), -1 )
        ;

        // Herd spatial coordinates
        //----------------------------------------------------------------------
        e := sdew.GetElementByName( element, 'location' );
        if( nil <> e ) then
          begin
            if ( not conductInvProj ) then // typical case, the coordinates are geographic
              begin
                ee := sdew.GetElementByName( e, 'latitude' );
                if( nil <> ee ) then
                  begin
                    lat := usStrToFloat( sdew.GetElementContents( ee ), LAT_LON_UNDEFINED );
                    if not gisValidLat(lat) then
                      begin
                        appendToPString( errMsg, ansiReplaceStr( tr( 'Unit list XML Herd ID '  + intToStr(id)
                        + ' contains an invalid latitude: "xyz"' ), 'xyz', sdew.GetElementContents( ee )));
                        lat := LAT_LON_UNDEFINED;
                      end
                    ;
                  end
                ;

                ee := sdew.GetElementByName( e, 'longitude' );
                if( nil <> ee ) then
                  begin
                    lon := usStrToFloat( sdew.GetElementContents( ee ), LAT_LON_UNDEFINED );
                    if not gisValidLon(lon) then
                      begin
                        appendToPString( errMsg, ansiReplaceStr( tr( 'Unit list XML Herd ID '  + intToStr(id)
                        + ' contains an invalid longitude: "xyz"' ), 'xyz', sdew.GetElementContents( ee )));
                        lon := LAT_LON_UNDEFINED;
                      end
                    ;
                  end
                ;
              end
            else  // coordinates are projected (such as when a error log is generated)
              begin
                ee := sdew.GetElementByName( e, 'x' );
                if( nil <> ee ) then
                  x := usStrToFloat( sdew.GetElementContents( ee ), NaN )
                ;
                
                ee := sdew.GetElementByName( e, 'y' );
                if( nil <> ee ) then
                  y := usStrToFloat( sdew.GetElementContents( ee ), NaN )
                ;

                if( nil <> _myList ) then
                  begin
                    if( nil <> _myList.projection ) then
                      begin
                        { Projection object instantiated and params set in THerdList.importXML.
                          Inverse the projected coordinates to obtain geographic coordinates,
                          the imported x and y (projected) values are not retained. }
                        try
                          llr := _myList.projection.pjInv( x, y );
                          lon := double(llr.lon); // inverse of x
                          lat := double(llr.lat); // inverse of y
                        except
                          lon := LAT_LON_UNDEFINED;
                          lat := LAT_LON_UNDEFINED;
                          appendToPString( errMsg, tr( 'Unit list XML Herd ID '  + intToStr(id) + ' contains invalid x,y coordinates'));
                        end;
                      end
                    ;
                  end
                ;
              end
            ;

            self.setLatLon( lat, lon );
          end
        ;
        //----------------------------------------------------------------------

        e := sdew.GetElementByName( element, 'production-type' );
        if( nil <> e ) then
          begin
            prodTypeName := sdew.GetElementContents( e );
            pt := ptDict.value( prodTypeName ) as TProductionType;
            if( nil = pt ) then
              appendToPString( errMsg, ansiReplaceStr( tr( 'Unit list XML contains an unrecognized production type: "xyz"' ), 'xyz', prodTypeName ) )
            else
              setProdType( pt )
            ;
          end
        ;

        e := sdew.GetElementByName( element, 'size' );
        if( nil <> e ) then
          initialSize := myStrToInt( sdew.GetElementContents( e ), -1 )
        ;
        if (0 >= initialSize ) then
          appendToPString( errMsg, ansiReplaceStr( tr( 'Unit list XML Herd ID '  + intToStr(id)
          + ' contains an invalid size: "xyz"' ), 'xyz', intToStr( initialSize )))
        ;

        e := sdew.GetElementByName( element, 'status' );
        if( nil <> e ) then
          initialStatus := naadsmDiseaseStateFromString( sdew.GetElementContents( e ) )
        ;
        if ( initialStatus = NAADSMStateUnspecified ) then
          appendToPString( errMsg, ansiReplaceStr( tr( 'Unit list XML Herd ID '  + intToStr(id)
          + ' contains an invalid status code: "xyz"' ), 'xyz', sdew.GetElementContents( e )))
        ;

        e := sdew.GetElementByName( element, 'days-in-status' );
        if( nil <> e ) then
          daysInInitialState := myStrToInt( Sdew.GetElementContents( e ), -1 )  // -1 is a valid default
        ;

        e := Sdew.GetElementByName( element, 'days-left-in-status' );
        if( nil <> e ) then
          daysLeftInInitialState := myStrToInt( sdew.GetElementContents( e ), -1 ) // -1 is a valid default
        ;
      end
    ;


    destructor THerd.destroy();
      begin
        _ctrlActivities.free();
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
        _y := NaN;
        _x := NaN;
        _initialStatus := NAADSMStateUnspecified;
        setDaysInInitialState( -1 );
        setDaysLeftInInitialState( -1 );
        _diseaseStatus := NAADSMStateUnspecified;
        _controlStatus := asUnspecified;
        _detectionStatus := dsNoStatus;
        _previouslyDetected := false;
        _zoneLevel := -1;

        // Advanced properties
        _sim := nil;
        _pt := nil;

        // Output
        _ctrlActivities := THerdControlActivities.create();

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

        if( nil = src.prodType ) then
          dbcout( 'src._pt is nil in THerd.assign()', DBHERD )
        ;

        if( nil = src._sim ) then
          dbcout( 'src_sim is nil in THerd.assign()', DBHERD )
        ;

        _sim := src._sim;

        setProdType( src.prodType );
        //setProdTypeName( src._prodTypeName ); // automatically taken care of by setProdType
        //setProdTypeID( src._prodTypeID ); // automatically taken care of by setProdType

        // Simple properties
        _id := src._id;
        _initialSize := src._initialSize;
        _lat := src._lat;
        _lon := src._lon;
        _y := src._y;
        _x := src._x;

        _initialStatus := src._initialStatus;
        _daysInInitialState := src._daysInInitialState;
        _daysLeftInInitialState := src._daysLeftInInitialState;

        if( resetToInitialState ) then
          initializeAllOutputRecords()
        else
          begin
            _diseaseStatus := src._diseaseStatus;
            _controlStatus := src._controlStatus;
            _detectionStatus := src._detectionStatus;
            _previouslyDetected := src._previouslyDetected;
            _zoneLevel := src._zoneLevel;

            _ctrlActivities.assign( src._ctrlActivities );
          end
        ;

        // Housekeeping properties
        _updated := src._updated;
        _isInDatabase := src._isInDatabase;
        _removeFromDatabase := src._removeFromDatabase;
      end
    ;
  //---------------------------------------------------------------------------


  //---------------------------------------------------------------------------
  // THerd: Cartographic projection
  //---------------------------------------------------------------------------
    {*
      Projects all herd lat/lon coordinates to the specified projection system.

      @return true if projection was successful.
    }
    function THerd.project(): boolean;
      var
        p: RPoint;
      begin
        result := false;

        if( nil <> _myList ) then
          begin
            if( nil <> _myList.projection ) then
              begin
                p := _myList.projection.pjFwd( self.lon, self.lat );

                if( not( _myList.projection.error ) ) then
                  begin
                    _x := p.x;
                    _y := p.y;

                    _myList._minX := min( _myList._minX, _x );
                    _myList._maxX := max( _myList._maxX, _x );
                    _myList._minY := min( _myList._minY, _y );
                    _myList._maxY := max( _myList._maxY, _y );

                    result := true;
                  end
                ;
              end
            ;
          end
        ;

        if( false = result ) then
          begin
            _x := NaN;
            _y := NaN;
          end
        ;
      end
    ;


    procedure THerd.setLatLon( lat, lon: double );
      begin
        // FIX ME: Should we be this generous about allowing invalid lat and lon values?
        if( -90.0 > lat ) then
          _lat := -90.0
        else if( 90.0 < lat ) then
          _lat := 90.0
        else
          _lat := lat
        ;

        while( -180.0 > lon ) do
          lon := lon + 360.0
        ;
        while( 180.0 < lon ) do
          lon := lon - 360.0
        ;
        _lon := lon;

        project();
        setUpdated( true );
      end
    ;
  //---------------------------------------------------------------------------



  //---------------------------------------------------------------------------
  // THerd: Text export
  //---------------------------------------------------------------------------
    function THerd.ssXML( const useProjection: boolean ): string;
      begin
        result := '';
        result := result + '  <herd>' + endl;
        result := result + '    <id>' + intToStr( id ) + '</id>' + endl;
        result := result + '    <production-type>' + xmlProdTypeName + '</production-type>' + endl;
        result := result + '    <size>' + intToStr( initialSize ) + '</size>' + endl;
        result := result + '    <location>' + endl;

        if( useProjection ) then
          begin
            result := result + '      <x>' + usFloatToStr( x, LAT_LON_PRECISION ) + '</x>' + endl;
            result := result + '      <y>' + usFloatToStr( y, LAT_LON_PRECISION ) + '</y>' + endl;
          end
        else
          begin
            result := result + '      <latitude>' + usFloatToStr( lat, LAT_LON_PRECISION ) + '</latitude>' + endl;
            result := result + '      <longitude>' + usFloatToStr( lon, LAT_LON_PRECISION ) + '</longitude>' + endl;
          end
        ;
        
        result := result + '    </location>' + endl;
        result := result + '    <status>' + naadsmDiseaseStateXml( self.initialStatus ) + '</status>' + endl;

        if( -1 < daysInInitialState ) then
          result := result + '    <days-in-status>' + intToStr( daysInInitialState ) + '</days-in-status>' + endl
        ;

        if( -1 < daysLeftInInitialState ) then
          result := result + '    <days-left-in-status>' + intToStr( daysLeftInInitialState ) + '</days-left-in-status>' + endl
        ;

        result := result + '  </herd>' + endl;
      end
    ;


    function THerd.csvText( const writeProdTypeName, writeInitialStateChar, useProjection: boolean ): string;
      begin
        result := '';
        result := result + intToStr( self.id ) + csvListSep;

        if( writeProdTypeName ) then
          result := result + '"' + self.prodTypeName + '"' + csvListSep
        else
          result := result + intToStr( self.prodTypeID )  + csvListSep
        ;

        result := result + intToStr( self.initialSize ) + csvListSep;

        if( useProjection ) then
          begin
            result := result + csvFloatToStr( self.x ) + csvListSep;
            result := result + csvFloatToStr( self.y ) + csvListSep;
          end
        else
          begin
            result := result + csvFloatToStr( self.lat ) + csvListSep;
            result := result + csvFloatToStr( self.lon ) + csvListSep;
          end
        ;

        if( writeInitialStateChar ) then
          result := result + naadsmDiseaseStateCode( self._initialStatus ) + csvListSep
        else
          result := result + intToStr( ord( self._initialStatus ) ) + csvListSep
        ;

        result := result + intToStr( self.daysInInitialState ) + csvListSep;
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


        if( NAADSMStateUnspecified = initialStatus ) then
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
          ( NAADSMStateSusceptible <> initialStatus )
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
        dbcout( 'Projected: ' + usFloatToStr( y ) + ' y, ' + usFloatToStr( x ) + ' x', true );
        dbcout( 'Status: ' + naadsmDiseaseStateStr( self.initialStatus ) + ' (' + intToStr( ord( initialStatus ) ) + ')', true );

        if( -1 = daysInInitialState ) then
          dbcout( 'Days in initial state: (unspecified)', true )
        else
          dbcout( 'Days in initial state: ' + intToStr( daysInInitialState ), true )
        ;

        if( -1 = daysLeftInInitialState ) then
          dbcout( 'Days left in initial state: (unspecified)', true )
        else
          dbcout( 'Days left in initial state: ' + intToStr( daysLeftInInitialState ), true )
        ;

        dbcout( 'Updated: ' + usBoolToText( updated ), true );
        dbcout( endl, true );
      end
    ;


    function THerd.briefInfo(): string;
      begin
        result :=
          tr( 'Unit' ) + ' ' + intToStr( id ) + ': '
          + tr( 'Type:' ) + ' ' + self.prodTypeName + '  '
          + tr( 'Location:' ) + ' Lat ' + usFloatToStr( lat ) + ', Lon ' + usFloatToStr( lon ) + endl
          + tr( 'Size:' ) + ' ' + intToStr( initialSize ) + '  '
          + tr( 'Disease state:' ) + ' ' + naadsmDiseaseStateStr( self.diseaseStatus ) + '  '
          + tr( 'Control status:' ) + ' ' + controlStatusString( self.controlStatus ) + '  '
          + tr( 'Detection status:' ) + ' ' + detectionStatusString( self.detectionStatus )
        ;
      end
    ;
  //---------------------------------------------------------------------------
    


  //---------------------------------------------------------------------------
  // THerd: Functions for simulation in progress
  //---------------------------------------------------------------------------
    procedure THerd.initializeAllOutputRecords(); // at start of simulation
      begin
        // For both Torrington and Wheatland, set initial state to susceptible.
        // The core library will randomize the location of the initially infected herd(s)
        // while the simulation is in progress.

        {$IF Defined( TORRINGTON ) }
          self.prodType.setInitialDailyRecords( initialSize, NAADSMStateSusceptible );
        {$ELSEIF Defined( WHEATLAND ) }
          self.prodType.setInitialDailyRecords( initialSize, NAADSMStateSusceptible );
        {$ELSE}
          self.prodType.setInitialDailyRecords( initialSize, initialStatus );
        {$IFEND}

        _ctrlActivities.initializeAllOutputRecords();

        prepareForIteration();
      end
    ;


    procedure THerd.prepareForIteration(); // at start of each iteration
      begin
        // For both Torrington and Wheatland, set initial state to susceptible.
        // The core library will randomize the location of the initially infected herd(s)
        // while the simulation is in progress.

        {$IF Defined( TORRINGTON ) }
           _diseaseStatus := NAADSMStateSusceptible;
        {$ELSEIF Defined( WHEATLAND ) }
           _diseaseStatus := NAADSMStateSusceptible;
        {$ELSE}
           _diseaseStatus := self.initialStatus;
        {$IFEND}

        _zoneLevel := -1;
        _ctrlActivities.prepareForIteration();

        case self.initialStatus of
          NAADSMStateDestroyed: _controlStatus := asDestroyed;
          NAADSMStateVaccineImmune: _controlStatus := asVaccinated;
          else _controlStatus := asNoControl;
        end;

        _detectionStatus := dsNoStatus;
        _previouslyDetected := false;
      end
    ;


    procedure THerd.prepareForDay(); // at start of each day
      begin
        // Nothing to do here...
      end
    ;


    procedure THerd.changeHerdState( const val: TNAADSMDiseaseState; const day: integer );
      begin
        if( 1 = day ) then
          begin
            //dbcout( 'Herd ' + intToStr( self.id ) + ' has change state on day ' + intToStr( day ) + ': state is now ' + naadsmDiseaseStateStr( val ) );
            exit;
          end
        ;

        self.prodType.updateDailyRecordsProdType( initialSize, _diseaseStatus, val, day );

        // _controlStatus does not change
        _diseaseStatus := val;

        // When a herd is infected, it counts as undetected until it is something happens to detect it.
        // It's theoretically possible for a herd to become naturally immune immediately upon infection,
        // so that possibility needs to be accounted for here.
        if( naadsmIsInfectedState( val ) or ( NAADSMStateNaturallyImmune = val ) ) then
          setDetectionStatus( dsInfectedUndetected )
        else if( NAADSMStateDestroyed = val ) then
          setDetectionStatus( dsDestroyed )
        else
          // Don't change _detectionStatus for any other reason
        ;
      end
    ;


    procedure THerd.infect( const r: THRDInfect; day: integer );
      var
        isInitialEvent: boolean;
      begin
        // _diseaseStatus will be updated by a call to changeHerdState from the NAADSM library
        // _controlStatus does not change
        // _detectionStatus will be updated by a call to changeHerdState from the NAADSM library

        // Since initial infection occurs before day 0, remember to
        // treat initial infection differently...
        isInitialEvent := ( NAADSMInitiallyInfected = r.infectionSourceType );
        _ctrlActivities.infectHerd( isInitialEvent );

        self.prodType.addInfectionEvent( initialSize, r, day );
      end
    ;


    procedure THerd.expose( const e: THRDExpose );
      begin
        self.prodType.addExposureEvent( initialSize, e );
      end
    ;


    function THerd.detect( const d: THRDDetect; day: integer ): boolean;
      {$IFDEF DEBUG}
        var
          msg: string;
      {$ENDIF}
      begin
        {$IFDEF DEBUG}
          msg := '-- Herd ' + intToStr( self.id ) + ' detected by ';
          case d.reason of
            NAADSMDetectionClinicalSigns:
              msg := msg + 'clinical signs'
            ;
            NAADSMDetectionDiagnosticTest:
              begin
                case d.testResult of
                  NAADSMTestTruePositive: msg := msg + 'true positive diag test';
                  NAADSMTestFalsePositive: msg := msg + 'false positive diag test';
                end;
              end
            ;
          end;
          msg := msg + ' on day ' + intToStr( day );
          //dbcout2( msg );
        {$ENDIF}

        // _diseaseStatus does not change

        case self.controlStatus of
          asDetected:
            begin
              // If a unit has already been detected (either by
              // clinical signs or diagnostic testing), nothing else
              // can happen.
              result := false;
              exit;
            end
          ;
          asDestroyed:
            begin
              // A destroyed unit can be detected by a delayed diagnostic test.  If that happens, count it.

              // A destroyed unit can be detected by clinical signs on the same day that detection occurred.
              // This should be counted too.
              // Note: there is currently no way for the GUI to verify that, if this occurs, it happens on the same day.
              // For the time being, at least, we trust the core library.
              if( _previouslyDetected ) then
                result := false
              else
                begin
                  // _detectionStatus changes according to the rules below:
                  case d.reason of
                    NAADSMDetectionClinicalSigns:
                      begin
                        // Note that this should have no effect: dsDestroyed has higher priority than dsDetectedClinical.
                        setDetectionStatus( dsDetectedClinical );
                      end
                    ;
                    NAADSMDetectionDiagnosticTest:
                      begin
                        case d.testResult of
                          NAADSMTestTruePositive: setDetectionStatus( dsTestTruePos );
                          NAADSMTestFalsePositive: setDetectionStatus( dsTestFalsePos );
                          else
                            begin
                              // Negative test results are taken care of elsewhere
                              raise exception.create( 'Unrecognized or unexpected test result (' + intToStr( ord( d.testResult ) ) + ') in THerd.detect()' );
                            end
                          ;
                        end;
                      end
                    ;
                    else
                      raise exception.create( 'Unrecognized detection reason in THerd.detect()' )
                    ;
                  end;

                  // The control status of a destroyed unit cannot change.

                  _ctrlActivities.detectHerd();
                  result := self.prodType.addDetectionEvent( initialSize, d, day );

                  _previouslyDetected := true;
                end
              ;
            end
          ;
          else
            begin
              // For all other situations, this counts as a detection.
              // _detectionStatus changes according to the rules below:
              case d.reason of
                NAADSMDetectionClinicalSigns:
                  setDetectionStatus( dsDetectedClinical )
                ;
                NAADSMDetectionDiagnosticTest:
                  begin
                    case d.testResult of
                      NAADSMTestTruePositive: setDetectionStatus( dsTestTruePos );
                      NAADSMTestFalsePositive: setDetectionStatus( dsTestFalsePos );
                      else
                        begin
                          // Negative test results are taken care of elsewhere
                          raise exception.create( 'Unrecognized or unexpected test result (' + intToStr( ord( d.testResult ) ) + ') in THerd.detect()' );
                        end
                      ;
                    end;
                  end
                ;
                else
                  raise exception.create( 'Unrecognized detection reason in THerd.detect()' )
                ;
              end;

              _ctrlActivities.detectHerd();
              result := self.prodType.addDetectionEvent( initialSize, d, day );

              // Update the control status the unit
              setControlStatus( asDetected );
              _previouslyDetected := true;
            end
          ;
        end;
      end
    ;


    procedure THerd.queueForDestruction( day: integer );
      begin
        // _diseaseStatus does not change
        setControlStatus( asInDestructionQueue );
        // _detectionStatus does not change

        _ctrlActivities.queueForDestruction( day );
        self.prodType.addDestructionQueueEvent( self, day );
      end
    ;


    function THerd.destroyForReason( const r: THRDControl; day: integer ): boolean;
      begin
        if( asDetected = _controlStatus ) then
          self.prodType.decrementApparentInfectiousUnits()
        ;

        // _diseaseStatus will be set by a direct call to changeHerdState from the NAADSM library
        setControlStatus( asDestroyed );
        setDetectionStatus( dsDestroyed );

        // Initially destroyed herds should not count in the queue outputs or in the cumulative herd outputs,
        // but they should count in the appropriate production type outputs.
        if( NAADSMControlInitialState <> r.reason ) then
          _ctrlActivities.destroyHerd( day, r.dayCommitmentMade )
        ;
        result := self.prodType.addDestructionEvent( self, r, day );

        (*
        dbcout(
          '--- Herd at index ' + intToStr( r.herdIndex ) + ' with ID ' + intToStr( self.id )
            + ' was destroyed for ' + naadsmControlActivityStr( r.reason )
            + ' on day ' + intToStr( day )
            + ' after ' + intToStr( day - r.dayCommitmentMade ) + ' days in queue'
          , DBHERD
        );
        *)
      end
    ;


    procedure THerd.queueForVaccination( day: integer );
      begin
        // Disease status does not change
        // Control status does not change
        // Detection status does not change
        
        _ctrlActivities.queueForVaccination( day );
        self.prodType.addVaccinationQueueEvent( self, day );
      end
    ;


    function THerd.vaccinateForReason( const r: THRDControl; day: integer ): boolean;
      begin
        // _diseaseStatus does not change
        setControlStatus( asVaccinated );
        // _detectionStatus does not change

        // Initially vaccinated herds should not count in the queue outputs or in the cumulative herd outputs,
        // but they should count in the appropriate production type outputs.
        if( NAADSMControlInitialState <> r.reason ) then
          _ctrlActivities.vaccinateHerd( day, r.dayCommitmentMade )
        ;
        result := self.prodType.addVaccinationEvent( self, r, day );
      end
    ;


    procedure THerd.cancelVaccination( const r: THRDControl; day: integer );
      begin
        // Disease status does not change
        // Control status does not change
        // Detection status does not change

        _ctrlActivities.cancelHerdVaccination( r.dayCommitmentMade );
        self.prodType.subtractVaccinationQueueEvent( self );
      end
    ;


    procedure THerd.attemptTraceForReason( const t: THRDTrace );
      begin
        self.prodType.addAttemptedTraceEvent( initialSize, t );
      end
    ;


    procedure THerd.conductHerdExam( const e: THRDExam );
      begin
        // _diseaseStatus does not change
        // _controlStatus does not change

        // Don't override a previous clinical detection
        if( self.detectionStatus <> dsDetectedClinical ) then
          setDetectionStatus( dsExamined )
        ;

        if( NAADSMStateDestroyed = self.diseaseStatus ) then
          raise exception.create( 'Attempting to examine a destroyed herd in THerd.conductHerdExam' )
        else
          self.prodType.addHerdExamEvent( initialSize, e )
        ;
      end
    ;


    procedure THerd.conductDiagnosticTest( const t: THRDTest );
      begin
        // _diseaseStatus does not change
        // _controlStatus does not change
        // If the test was negative, change _detectionStatus here.
        // (Positive tests are reported by THerd.detect())

        //dbcout2( '--- THerd.conductDiagnosticTest() with result ' + naadsmTestResultStr( t.testResult ) );

        if( self.detectionStatus in [ dsDetectedClinical, dsDestroyed ] ) then
          // Do nothing: a test result will not override these states
        else
          begin
            case t.testResult of
              NAADSMTestTrueNegative: setDetectionStatus( dsTestTrueNeg );
              NAADSMTestFalseNegative: setDetectionStatus( dsTestFalseNeg );
            end;
          end
        ;

        // Count the test for tracking purposes, regardless of the herd's detection status.
        self.prodType.addDiagnosticTestEvent( initialSize, t );
      end
    ;


    procedure THerd.traceForReason( const t: THRDTrace );
      begin
        // _diseaseStatus does not change
        // _detectionStatus does not change
        // _controlStatus changes according to the rules below

        if( self.controlStatus in [ asDetected, asDestroyed ] ) then
          // don't change it
        else
          begin
            if( NAADSMDirectContact = t.contactType ) then
              begin
                case t.traceType of
                  NAADSMTraceForwardOrOut: setControlStatus( asTracedDirectFwd );
                  NAADSMTraceBackOrIn: setControlStatus( asTracedDirectBack );
                  else raise exception.Create( 'Unrecognized trace type (' + intToStr( cardinal( t.traceType ) ) + ') in THerd.traceForReason' );
                end;
              end
            else if( NAADSMIndirectContact = t.contactType ) then
              begin
                case t.traceType of
                  NAADSMTraceForwardOrOut: setControlStatus( asTracedIndirectFwd );
                  NAADSMTraceBackOrIn: setControlStatus( asTracedIndirectBack );
                  else raise exception.Create( 'Unrecognized trace type (' + intToStr( cardinal( t.traceType ) ) + ') in THerd.traceForReason' );
                end;
              end
            else
              raise exception.Create( 'Unrecognized contact type (' + intToStr( cardinal( t.contactType ) ) + ') in THerd.traceForReason' )
            ;
          end
        ;

        self.prodType.addTraceEvent( initialSize, t );
      end
    ;


    procedure THerd.recordTraceOrigin( const t: THRDTrace );
      begin
        // _diseaseStatus does not change
        // _detectionStatus does not change
        // _controlStatus does not change

        self.prodType.addTraceOrigin( t );
      end
    ;


    procedure THerd.processIterationRecords( iterationJustCompleted: integer );
      begin
        _ctrlActivities.processIterationRecords();
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

    function THerd.getLat(): double; begin assert( gisValidLat( _lat ) ); Result := _lat; end;
    function THerd.getLon(): double; begin assert( gisValidLon( _lon ) ); Result := _lon; end;

    function THerd.getY(): double;
      begin
        if( isNaN( _y ) ) then
          project()
        ;
        result := _y;
      end
    ;


    function THerd.getX(): double;
      begin
        if( isNaN( _x ) ) then
          project()
        ;
        result := _x;
      end
    ;

    procedure THerd.setProdTypeName( val: string ); begin _prodTypeName := val; _xmlProdTypeName := encodeXml( val ); setUpdated( true ); end;
    function THerd.getProdTypeName(): string; begin Result := _prodTypeName; end;

    procedure THerd.setInitialSize( val: integer ); begin _initialSize := val; setUpdated( true ); end;
    function THerd.getInitialSize(): integer; begin Result := _initialSize; end;

    procedure THerd.setInitialStatus( val: TNAADSMDiseaseState ); begin _initialStatus := val; setUpdated( true ); end;
    procedure THerd.setControlStatus( val: TControlStatus ); begin _controlStatus := val; setUpdated( true ); end;

    function THerd.getInitialStatus(): TNAADSMDiseaseState; begin Result := _initialStatus; end;
    function THerd.getDiseaseStatus(): TNAADSMDiseaseState; begin result := _diseaseStatus; end;
    function THerd.getControlStatus(): TControlStatus; begin result := _controlStatus; end;
    function THerd.getDetectionStatus(): TDetectionStatus; begin result := _detectionStatus; end;

    procedure THerd.setDetectionStatus( const val: TDetectionStatus );
      begin
        if( val > self.detectionStatus ) then
          begin
            _detectionStatus := val;
            setUpdated( true );
          end
        ;
      end
    ;

    procedure THerd.setDaysLeftInInitialState( val: integer ); begin _daysLeftInInitialState := val; setUpdated( true ); end;
    function THerd.getDaysLeftInInitialState(): integer; begin result := _daysLeftInInitialState; end;

    procedure THerd.setDaysInInitialState( val: integer ); begin _daysInInitialState := val; setUpdated( true ); end;
    function THerd.getDaysInInitialState(): integer; begin result := _daysInInitialState; end;

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
        result := _ctrlActivities.updated
          or ( _diseaseStatus <> _initialStatus )
          or ( _controlStatus <> asNoControl )
          or ( _detectionStatus <> dsNoStatus )
        ;
      end
    ;
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

        // This is no projection system and nothing to project.
        // So don't bother trying.
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
        dbcout( 'Starting to create herd list', DBHERDLIST );
        inherited create( true );
        initialize();

        _smdb := db;
        _sim := sim as TSMSimulationInput;

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
            + ' dynHerd.daysInInitialState AS daysInInitialState,'
            + ' dynHerd.daysLeftInInitialState AS daysLeftInInitialState,'
            + ' dynHerd.initialSize AS initialSize,'
            + ' dynHerd.finalStateCode AS finalStateCode,'
            + ' dynHerd.finalControlStateCode AS finalControlStateCode,'
            + ' dynHerd.finalDetectionStateCode AS finalDetectionStateCode,'
            + ' dynHerd.cumulInfected AS cumulInfected,'
            + ' dynHerd.cumulDetected AS cumulDetected,'
            + ' dynHerd.cumulDestroyed AS cumulDestroyed,'
            + ' dynHerd.cumulVaccinated AS cumulVaccinated'
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
            h.setLatLon( row.field('latitude'), row.field('longitude') );

            h.initialStatus :=  naadsmDiseaseStateFromCode( charAt( string( row.field('initialStateCode') ), 0 ) );

            if( null <> row.field( 'finalStateCode' ) ) then
              h._diseaseStatus := naadsmDiseaseStateFromCode( charAt( string( row.field('finalStateCode' ) ), 0 ) )
            else
              {$IF Defined( TORRINGTON ) }
                 h._diseaseStatus := NAADSMStateSusceptible
              {$ELSEIF Defined( WHEATLAND ) }
                 h._diseaseStatus := NAADSMStateSusceptible
              {$ELSE}
                 h._diseaseStatus := h.initialStatus
              {$IFEND}
            ;

            if( NAADSMStateSusceptible = h.initialStatus ) then
              begin
                h.daysInInitialState := -1;
                h.daysLeftInInitialState := -1;
              end
            else
              begin
                if( null <> row.field('daysInInitialState') ) then
                  h.daysInInitialState := integer( row.field('daysInInitialState') )
                else
                  h.daysInInitialState := -1
                ;

                if( null <> row.field('daysLeftInInitialState') ) then
                  h.daysLeftInInitialState := integer( row.field('daysLeftInInitialState') )
                else
                  h.daysLeftInInitialState := -1
                ;
              end
            ;

            if( null <> row.field('finalControlStateCode') ) then
              h._controlStatus := controlStatusFromCode( charAt( row.field('finalControlStateCode'), 0 ) )
            else
              h._controlStatus := asNoControl
            ;

            if( null <> row.field('finalDetectionStateCode') ) then
              h._detectionStatus := detectionStatusFromCode( charAt( row.field('finalDetectionStateCode'), 0 ) )
            else
              h._detectionStatus := dsNoStatus
            ;

            if( null <> row.field('cumulInfected') ) then h.ctrlActivities.cumulInfections := row.field('cumulInfected');
            if( null <> row.field('cumulDetected') ) then h.ctrlActivities.cumulDetections := row.field('cumulDetected');
            if( null <> row.field('cumulDestroyed') ) then h.ctrlActivities.cumulDestructions := row.field('cumulDestroyed');
            if( null <> row.field('cumulVaccinated') ) then h.ctrlActivities.cumulVaccinations := row.field('cumulVaccinated');

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

        // Project herd lat/lons
        //----------------------
        //  NOTE:  setProjection() calls project(), if the projection set was valid.
        //         no valid reason to call it twice...
        setProjection( defaultProjection( minLat, minLon, maxLat, maxLon ) );
        //  project();

        // Clean up
        //----------
        res.Free();

        _updated := false;

        dbcout( 'Herd list created from database updated: ' + usBoolToText( updated ), DBHERDLIST );
        dbcout( 'Done creating herd list', DBHERDLIST );

        (sim as TSMSimulationInput).ptList.recountUnits( self );

        if( nil <> @fn ) then fn( 100 );
      end
    ;


    constructor THerdList.create( const src: THerdList; const resetHerdsToInitalState: boolean = false );
      begin
        inherited create( true );
        initialize();
        
        assign( src, resetHerdsToInitalState );

        // It isn't necessary to project herd locations here:
        // x/y coordinates and the projection system are handled by
        // assign() and by THerd's copy constructor.

        dbcout( 'Herd list created from previous list updated: ' + usBoolToText( updated ), DBHERDLIST );
      end
    ;


    constructor THerdList.create(
            const xmlFileName: string;
            db: TSMDatabase;
            sim: TObject;
            errMsg: pstring = nil;
            fnPrimaryProgress: TObjFnBool1Int = nil;
            fnProgressMessage: TObjFnVoid1String = nil
          );
      begin
        inherited create( true );
        initialize();

        _smdb := db;
        _sim := sim as TSMSimulationInput;

        importXml( xmlFileName, errMsg, fnPrimaryProgress, fnProgressMessage );
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

        _minX := MaxDouble;
        _maxX := MinDouble;
        _minY := MaxDouble;
        _maxY := MinDouble;

        _proj := nil;
        _projected := false;

        _updated := false;
        _removedCount := 0;
        _sim := nil;
      end
    ;


    destructor THerdList.destroy();
      begin
        freeAndNil( _proj );
        
        inherited destroy();
      end
    ;


    procedure THerdList.assign( const src: THerdList; const resetHerdsToInitalState: boolean = false );
      var
        srcH, newH: THerd;
        srcIt: THerdListIterator;
      begin
        _smdb := src._smdb;

        // NOTE:  Projection should happen after the herds have been reloaded into
        //        the new list, not before...this code change handles and fixes
        //        a seg-fault that will occurr if the src herdlist had no proj
        //        set, as is always the case when a new herd list is imported via
        //        the FormHerdListEditor....

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


        if ( nil <> src._proj ) then
          begin
            _proj := TProj4.create( src._proj );
            _minLat := src._minLat;
            _maxLat := src._maxLat;
            _minLon := src._minLon;
            _maxLon := src._maxLon;

            _minX := src._minX;
            _maxX := src._maxX;
            _minY := src._minY;
            _maxY := src._maxY;
          end
        else
          begin
            //  NOTE: setProjection() calls project();
            setProjection( defaultProjection( _minLat, _minLon, _maxLat, _maxLon ) );
          end
        ;
        
        _updated := src._updated;

        srcIt.Free();
      end
    ;
  //---------------------------------------------------------------------------



  //---------------------------------------------------------------------------
  // THerdList: Cartographic projection
  //---------------------------------------------------------------------------
    {*
      Sets up the default system to be used for cartographic projection,
      which is based on the coordinates of the included herds.

      In the future, the user might be able to specify his own coordinate system:
      see function setProjection().
    }
    class function THerdList.defaultProjection( const minLat, minLon, maxLat, maxLon: double ): string;
      var
        centerLon: double;
        latRange: double;
        sp1, sp2: double;
        projStr: string;
      begin
        // The approach used here is identical to that used in
        // the core library: see function default_projection() in main.c

        centerLon := ( minLon + maxLon ) / 2.0;

        // If the latitude range is very close to the equator or contains the
        // equator, use a cylindrical equal area projection.  Otherwise, use
        // the Albers equal area projection.
        //
        // (The Albers equal area conic projection becomes the cylindrical equal
        // area when its parallels are at the equator.)
        if
          ( ( minLat > -1.0 ) and ( maxLat < 1.0 ) ) // "very close to the equator"
        or
          ( 0.0 > minLat * maxLat ) // One of the coordinates is negative, so the equator is included
        then
          begin
            projStr := ''
              + '+ellps=WGS84'
              + ' +units=km'
              + ' +lon_0=' + usFloatToStr( centerLon )
              + ' +proj=cea'
            ;
          end
        else // use Albers equal area projection
          begin
            latRange := maxLat - minLat;
            sp1 := minLat + latRange / 6.0;
            sp2 := maxLat - latRange / 6.0;

            projStr := ''
              + '+ellps=WGS84'
              + ' +units=km'
              + ' +lon_0=' + usFloatToStr( centerLon )
              + ' +proj=aea'
              + ' +lat_1=' + usFloatToStr( sp1 )
              + ' +lat_2=' + usFloatToStr( sp2 )
            ;
          end
        ;

        dbcout2( projStr );

        result := projStr;
      end
    ;


    {*
      Sets the projection system for the herd list based on the specified string.
      If the projection system is valid, then herd coordinates are optionally projected.
      Otherwise, nothing is changed.

      Some day, this function might be used to let users specify their own
      projection systems.

      @param projParams Proj4 library projection parameter name and value pairs delimited by '+' characters.
      @param projectHerds if true the coordinates of each herd are projected.
      The default value true preserves how this function worked before the option to only set projection parameters was added.
      @return true on success
    }
    function THerdList.setProjection( const projParams: string; projectHerds: boolean = true ): boolean;
      var
        newProj: TProj4;
      begin
        newProj := TProj4.create( projParams );

        if( newProj.isValid ) then
          begin
            if( nil <> _proj ) then
              freeAndNil( _proj )
            ;
            _proj := newProj;

            {The "if" is necessary to use this function for importing  both
            geographic and projected coordinates. When the coordinates are
            already projected we do not want to iterate through the herd list
            now because the list still has to be built. In this case we just want to
            initialize proj4 with the imported projection parameters so conversion
            to lat lon can be done in the herd constructor. }
            if projectHerds then project();

            result := true;
          end
        else
          begin
            freeAndNil( newProj );
            result := false;
          end
        ;
      end
    ;

    function THerdList.reproject(): boolean;
      begin
        setProjection( defaultProjection( minLat, minLon, maxLat, maxLon ) );

        result := project();
      end
    ;


    {*
      Projects all herd lat/lon coordinates to the specified projection system.

      @return true if every herd's coordinates were successfully projected, false if any one of them failed
    }
    function THerdList.project(): boolean;
      var
        it: THerdListIterator;
      begin
        result := false;
        
        _minX := MaxDouble;
        _maxX := MinDouble;
        _minY := MaxDouble;
        _maxY := MinDouble;

        if( nil <> _proj ) then
          begin
            result := true;
            it := THerdListIterator.create( self );

            { Resetting to the projected value of the first herd so that finding
              the max and mins and mapping will occur correctly in either hemisphere.
            }
            if ( nil <> it.current() ) then
              begin
                result := it.current().project();
                _maxX := it.current()._x;
                _maxY := it.current()._y;
                _minX := it.current()._x;
                _minY := it.current()._y;

                it.incr();
              end
            ;

            while( nil <> it.current() ) do
              begin
                // When the herd is projected, it also updates the list's min and max x,y values
                if( false = it.current().project() ) then
                  result := false
                ;
                it.incr();
              end
            ;

            it.Free();
          end
        ;

        _projected := result;

        if( not result ) then
          begin
            _minX := MaxDouble;
            _maxX := MinDouble;
            _minY := MaxDouble;
            _maxY := MinDouble;
          end
        ;
      end
    ;


    procedure THerdList.debugProjected();
      var
        it: THerdListIterator;
        s: string;
      begin
        it := THerdListIterator.create( self );

        dbcout( 'Current projection:', true );
        dbcout( self.projection.paramString, true );
        dbcout( 'id, x, y, size', true );

        while( nil <> it.current() ) do
          begin
            s := intToStr( it.current().id ) + ', ' + usFloatToStr( it.current().x ) + ', ' + usFloatToStr( it.current().y ) + ', ' + intToStr( it.current().initialSize );
            dbcout( s, true );
            it.incr();
          end
        ;

        it.Free();
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

    procedure THerdList.setSim( sim: TSMSimulationInput );
      begin
        _sim := sim;
      end
    ;

    function THerdList.getUpdated(): boolean;
      begin
        result := _updated;
      end
    ;


    procedure THerdList.setUpdated( val: boolean );
      begin
        //dbcout( 'Setting THerdList.updated to ' + usBoolToText( val ) );
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
          errMsg: pstring = nil;
          fnPrimaryProgress: TObjFnBool1Int = nil;
          fnSecondaryProgress: TObjFnBool1Int = nil;
          fnProgressMessage: TObjFnVoid1String = nil;
          populateDB: boolean = true;
          ids: TQIntegerVector = nil
        ): boolean;
      begin
        // populateDB is only used by the XML file importer at this time.
        
        case fileFormat of
          XML_FILE_FORMAT:
            begin
              result := importXML(
                fileName,
                errMsg,
                fnPrimaryProgress,
                fnProgressMessage,
                populateDB,
                ids
              );
              if( nil <> @fnSecondaryProgress ) then
                fnSecondaryProgress( 100 )
              ;
            end
          ;
          CSV_FILE_FORMAT:
            result := importCSV(
              fileName,
              errMsg,
              fnPrimaryProgress,
              fnSecondaryProgress,
              fnProgressMessage,
              ids
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
          errMsg: pstring = nil;
          fnPrimaryProgress: TObjFnBool1Int = nil;
          fnSecondaryProgress: TObjFnBool1Int = nil;
          fnProgressMessage: TObjFnVoid1String = nil;
          ids: TQIntegerVector = nil
        ): boolean;
      var
        h: THerd;
        csv: TCSVContents;
        colDict: TQStringLongIntMap;
        i: integer;
        tmp: string;

        idColNames: TQStringList;
        ptColNames: TQStringList;
        sizeColNames: TQStringList;
        latColNames: TQStringList;
        lonColNames: TQStringList;
        stateColNames: TQStringList;
        daysInStateColNames: TQStringList;
        daysLeftColNames: TQStringList;

        total: integer;

        lastCompleteRow: integer;

        success: boolean;
      begin
        if( nil <> errMsg ) then
          errMsg^ := ''
        ;

        // First step: read and parse the file
        //-------------------------------------
        if( nil <> @fnProgressMessage ) then fnProgressMessage( 'Reading file contents...' );

        csv := TCSVContents.createFromFile( fileName, csvListSep(), true );

        if( not( csv.parseSuccess ) ) then
          begin
            if( nil <> @fnPrimaryProgress ) then fnPrimaryProgress( 100 );
            if( nil <> @fnSecondaryProgress ) then fnSecondaryProgress( 100 );
            csv.Free();
            if( nil <> errMsg ) then
              errMsg^ := errMsg^ + tr( 'The file does not seem to be a CSV file, and cannot be parsed.' )
            ;
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

        colDict := TQStringLongIntMap.create();

        idColNames := TQStringList.create( 'id | herdid | ''id | herd id | unitid | unit id'
          + ' | iddelhato | iddelaunidad | id de la unidad'
          , '|'
        );

        ptColNames := TQStringList.create(
          'productiontype | productiontypeid | production type | production type id'
            + ' | tipodeproducción | tipo de producción | tipodeproduccion | tipo de produccion'
          , '|'
        );

        sizeColNames := TQStringList.create(
          'herdsize | initialsize | herd size | initial size | size | unitsize | unit size'
            + ' | tamañodelhato | tamaño del hato | tamanodelhato | tamano del hato'
          , '|'
        );

        latColNames := TQStringList.create( 'lat | latitude | latitud', '|' );

        lonColNames := TQStringList.create( 'lon | longitude | long | longitud', '|' );

        stateColNames := TQStringList.create( 'status | initialstatecode | estado', '|' );

        // FIX ME: Double-check Spanish fo "initial state"
        daysInStateColNames := TQStringList.create(
          'daysinstate | daysinstatus | daysininitialstate | days in state | days in initial state | days in status | days in initial status'
            + ' | díaseneseestado | días en ese estado | diaseneseestado | dias en ese estado'
          , '|'
        );

        daysLeftColNames := TQStringList.create(
          'daysleftininitialstate | days left in initial state | daysleftinstatus | days left in status | daysleftinstate | days left in state'
            + ' | díasapermanacereneseestado | díasapermanacereneseestado | diasapermanacereneseestado | dias a permanacer en ese estado'
          , '|'
        );

        // FIX ME: this search could be a bit more robust
        // (e.g. don't allow duplicate column names)
        for i := 0 to csv.columnCount - 1 do
          begin
            dbcout( csv.header(i), DBHERDLIST );
            tmp := fixup( csv.header(i) );

            if( idColNames.contains( tmp ) ) then colDict['id'] := i;
            if( ptColNames.contains( tmp ) ) then colDict['productionType'] := i;
            if( sizeColNames.contains( tmp ) ) then colDict['herdSize'] := i;
            if( latColNames.contains( tmp ) ) then colDict['lat'] := i;
            if( lonColNames.contains( tmp ) ) then colDict['lon'] := i;
            if( stateColNames.contains( tmp ) ) then colDict['status'] := i;
            if( daysInStateColNames.contains( tmp ) ) then colDict['daysInState'] := i;
            if( daysLeftColNames.contains( tmp ) ) then colDict['daysLeftInState'] := i;
          end
        ;

        idColNames.Free();
        ptColNames.Free();
        sizeColNames.Free();
        latColNames.Free();
        lonColNames.Free();
        stateColNames.Free();
        daysInStateColNames.Free();
        daysLeftColNames.Free();

        // Check that all required columns are present
        // (daysLeftInState is optional)
        success := true; // until shown otherwise.
        
        if( not( colDict.HasKey('id') ) ) then
          begin
            success := false;
            if( nil <> errMsg ) then
              errMsg^ := errMsg^ + tr( 'Column "unitID" is missing.' ) + endl
            ;
          end
        ;
        if( not( colDict.HasKey('productionType') ) ) then
          begin
            success := false;
            if( nil <> errMsg ) then
              errMsg^ := errMsg^ + tr( 'Column "productionType" is missing.' ) + endl
            ;
          end
        ;
        if( not( colDict.HasKey('herdSize') ) ) then
          begin
            success := false;
            if( nil <> errMsg ) then
              errMsg^ := errMsg^ + tr( 'Column "unitSize" is missing.' ) + endl
            ;
          end
        ;
        if( not( colDict.HasKey('lat') ) ) then
          begin
            success := false;
            if( nil <> errMsg ) then
              errMsg^ := errMsg^ + tr( 'Column "lat" is missing.' ) + endl
            ;
          end
        ;
        if( not( colDict.HasKey('lon') ) ) then
          begin
            success := false;
            if( nil <> errMsg ) then
              errMsg^ := errMsg^ + tr( 'Column "lon" is missing.' ) + endl
            ;
          end
        ;
        if( not( colDict.HasKey('status') ) ) then
          begin
            success := false;
            if( nil <> errMsg ) then
              errMsg^ := errMsg^ + tr( 'Column "status" is missing.' ) + endl
            ;
          end
        ;

        if( not( success ) ) then
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
                    if( ( nil <> ids ) and ( -1 <> h.id ) ) then
                      ids.append( h.id )
                    ;

                    if( -1 = myStrToInt( csv.value( colDict['productionType'], i ), -1, false ) ) then
                      h.setProdTypeName( trim( csv.value( colDict['productionType'], i ) ) )
                    else
                      h.setProdTypeID( myStrToInt( csv.value( colDict['productionType'], i ) ) )
                    ;

                    h.initialSize := myStrToInt( csv.value( colDict['herdSize'], i ) );

                    h.setLatLon(
                      csvStrToFloat( csv.value( colDict['lat'], i ) ),
                      csvStrToFloat( csv.value( colDict['lon'], i ) )
                    );

                    if( -1 = myStrToInt( csv.value( colDict['status'], i ), -1, false ) ) then
                      h.initialStatus := naadsmDiseaseStateFromCode( charAt( trim( csv.value( colDict['status'], i ) ), 0 ) )
                    else
                      h.initialStatus := TNAADSMDiseaseState( myStrToInt( csv.value( colDict['status'], i ) ) )
                    ;

                    if( colDict.HasKey('daysInState') ) then
                      begin
                        if( '' <> csv.value( colDict['daysInState'], i ) ) then
                          h.daysInInitialState := myStrtoInt( csv.value( colDict['daysInState'], i ) )
                        else
                          h.daysInInitialState := -1
                        ;
                      end
                    else
                      h.daysInInitialState := -1
                    ;

                    if( colDict.HasKey('daysLeftInState') ) then
                      begin
                        if( '' <> csv.value( colDict['daysLeftInState'], i ) ) then
                          h.daysLeftInInitialState := myStrtoInt( csv.value( colDict['daysLeftInState'], i ) )
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
            if( nil <> errMsg ) then
              errMsg^ := errMsg^ + ansiReplaceStr( tr( 'An error occurred at or near row xyz.' ), 'xyz', intToStr( lastCompleteRow + 1 ) ) + endl
            ;
            dbcout( 'Exception occurred while processing rows, at or around row ' + intToStr( lastCompleteRow + 1 ), DBHERDLIST );
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


    function THerdList.importXML(
          const herdFileName: string;
          errMsg: pstring = nil;
          fnPrimaryProgress: TObjFnBool1Int = nil;
          fnProgressMessage: TObjFnVoid1String = nil;
          populateDB: boolean = true;
          ids: TQIntegerVector = nil
        ): boolean;
      var
        sdew: TSdew;
        root: pointer;
        i, n: integer;
        e: pointer;
        h: THerd;
        ptDict: TQStringObjectMap;
        msg: string;
        nSteps, nStepsCompleted: integer;
        subElement: Pointer;
        proj4Params: string;
        doInverseProjection: boolean;
      begin
        if( strIsEmpty( herdFileName ) ) then
          begin
            result := false;
            exit;
          end
        ;

        result := true; // Until shown otherwise

        sdew := TSdew.createFromFile( pAnsiChar( herdFileName ) );
        root := sdew.GetRootTree();
        n := sdew.GetElementCount( root );

        // This can occur if the XML character encoding is incorrect or the XMLis not well formed
        if 0 > n then
          begin
            appendToPString( errMsg, tr( 'The XML herd import file could not be parsed.' ) );
            sdew.Free();
            result := false;
            exit;
          end
        ;
        nSteps := ceil( n / 100 ) + 6;
        nStepsCompleted := 0;

        // Set up dictionary for production types
        //---------------------------------------
        // ptDict is used to quickly set the production type for each herd.
        // It will go away when we're done with it.
        if( nil <> @fnProgressMessage ) then
          fnProgressMessage( tr( 'Checking production types...' ) )
        ;
        ptDict := TQStringObjectMap.create();
        for i := 0 to _sim.ptList.Count - 1 do
          ptDict.insert( _sim.ptList.objects[i].productionTypeDescr, _sim.ptList.objects[i] )
        ;


        // Determine if herd coordinates are already projected and inverse to geographic if so
        //------------------------------------------------------------------------------------
        proj4Params := '';
        doInverseProjection := false;
        e := Sdew.GetElementByName( root, 'spatial_reference' );
        if ( nil <> e ) then // the coordinates are not geographic
          begin
            subElement := Sdew.GetElementByName( e, 'PROJ4' ); // projection parameters
            if ( subElement <> nil ) then 
              begin
                proj4Params := Sdew.GetElementContents( subElement );
                // initialize _proj with imported projection parameters so herds (on create) can inverse project to lat lon
                doInverseProjection := setProjection(proj4Params, false);
                // the next setProjection call below re-initializes _proj for one of the default projections used by the model

                if not doInverseProjection then
                  begin
                    result := false;
                    appendToPString( errMsg, tr( 'Projection parameters in the herd import file are invalid so premises are not located properly.' ) ); 
                  end
                ;
              end
            ;
          end
        ;


        // Read herds
        //-----------
        inc( nStepsCompleted );
        if( nil <> @fnPrimaryProgress ) then
          fnPrimaryProgress( trunc( 100 * nStepsCompleted / nSteps ) )
        ;
        if( nil <> @fnProgressMessage ) then
          fnProgressMessage( tr( 'Reading units...' ) )
        ;

        for i := 0 to n - 1 do
          begin
            e := sdew.GetElementByIndex( root, i );
            if( 'herd' = sdew.GetElementName( e ) ) then
              begin
                h := THerd.create( self, _sim, sdew, e, ptDict, @msg, doInverseProjection );

                if( strIsEmpty( msg ) ) then
                  begin
                    self.append( h );
                    if( ( nil <> ids ) and ( -1 <> h.id ) ) then
                      ids.append( h.id )
                    ;
                  end
                else
                  begin
                    h.Free();
                    appendToPString( errMsg, msg );
                    msg := '';   //otherwise initial error msg is printed for every herd
                    result := false;
                  end
                ;

                if( 0 = ( i mod 100 ) ) then
                  begin
                    inc( nStepsCompleted );
                    if( nil <> @fnPrimaryProgress ) then
                      fnPrimaryProgress( trunc( 100 * nStepsCompleted / nSteps ) )
                    ;
                  end
                ;
              end
            ;
          end
        ;

        (*
          Note: populateDB controls whether to replace the contents of dynHerd
          now or simply rebuild the herd list and whether to project herd
          coordinates. populateDB is typically true when this function is called
          from FormImport and some of the herd list constructors but false when
          called from FormHerdListEditor. The default value of true preserves
          what happened in the function before this input parameter was added.
        *)

        // Deal with database issues
        //--------------------------
        inc( nStepsCompleted );
        if( nil <> @fnPrimaryProgress ) then
          fnPrimaryProgress( trunc( 100 * nStepsCompleted / nSteps ) )
        ;

        if ( populateDB ) then
          begin
            if( nil <> @fnProgressMessage ) then
              fnProgressMessage( tr( 'Populating scenario database...' ) )
            ;
            populateDatabase( _smdb );
            setMinMaxLLFromDB();
          end
        else
          begin
            setMinMaxLL;
          end
        ;

        // Project herd lat/lons
        //----------------------
        inc( nStepsCompleted );
        if( nil <> @fnPrimaryProgress ) then
          fnPrimaryProgress( trunc( 100 * nStepsCompleted / nSteps ) )
        ;

        if ( populateDB ) then
          begin
            if( nil <> @fnProgressMessage ) then
              fnProgressMessage( tr( 'Performing geographic projection...' ) )
            ;

            //  NOTE:  setProjection() calls project(), if the projection set was valid.
            //         no valid reason to call it twice...
            setProjection( defaultProjection( minLat, minLon, maxLat, maxLon ) );
          end
        else
          begin
            // The projection parameters are set to defaults, but the herds are not projected
            setProjection( defaultProjection( minLat, minLon, maxLat, maxLon ), false );
          end
        ;

        // Count herds
        //------------
        inc( nStepsCompleted );
        if( nil <> @fnPrimaryProgress ) then
          fnPrimaryProgress( trunc( 100 * nStepsCompleted / nSteps ) )
        ;
        if( nil <> @fnProgressMessage ) then
          fnProgressMessage( tr( 'Validating units...' ) )
        ;
        _sim.ptList.recountUnits( self );


        // Initialize output records
        //--------------------------
        inc( nStepsCompleted );
        if( nil <> @fnPrimaryProgress ) then
          fnPrimaryProgress( trunc( 100 * nStepsCompleted / nSteps ) )
        ;
        if( nil <> @fnProgressMessage ) then
          fnProgressMessage( tr( 'Initializing units...' ) )
        ;

        initializeAllOutputRecords();

        // Clean up and go home
        //---------------------
        sdew.Free();
        ptDict.Free();

        _updated := false;

        if( nil <> @fnPrimaryProgress ) then
          fnPrimaryProgress( 100 )
        ;
        if( nil <> @fnProgressMessage ) then
          fnProgressMessage( tr( 'Done!' ) )
        ;

        dbcout( 'Herd list created from database updated: ' + usBoolToText( updated ), DBHERDLIST );
        dbcout( 'Done creating herd list', DBHERDLIST );
      end
    ;
  //---------------------------------------------------------------------------



  //---------------------------------------------------------------------------
  // THerdList: Text export
  //---------------------------------------------------------------------------
    function THerdList.ssXml( const useProjection: boolean ): string;
      var
        h: THerd;
        it: THerdListIterator;
      begin
        result := '<?xml version="1.0" encoding="UTF-16" ?>' + endl;
        result := result + '<herds' + endl;
        result := result + '  xmlns:naadsm="http://www.naadsm.org/schema"' + endl;
        result := result + '  xmlns:xsd="http://www.w3.org/2001/XMLSchema"' + endl;
        result := result + '  xmlns:gml="http://www.opengis.net/gml"' + endl;
        result := result + '  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">' + endl;

        if( useProjection ) then
          begin
            result := result + '  <spatial_reference>' + endl;
            result := result + '    <PROJ4>' + _proj.paramString + '</PROJ4>' + endl;
            result := result + '  </spatial_reference>' + endl;
          end
        ;

        it := THerdListIterator.create( self );

        while( nil <> it.current() ) do
          begin
            h := it.current();

            if( not( h.removeFromDatabase ) ) then
              result := result + h.ssXml( useProjection ) + endl
            ;

            it.incr();
          end
        ;

        result := result +'</herds>' + endl;

        it.Free();
      end
    ;


    {*
    WARNING: this function will attempt to overwrite an existing file without notice.
    FIX ME: no error checking is done.
    }
    function THerdList.writeXMLFile( fileName: string; const useProjection: boolean; errMsg: PString = nil ): boolean;
      var
        xmlFile: TextFile;
        h: THerd;
        it: THerdListIterator;
      begin
        if( useProjection and not( isProjected ) ) then
          raise exception.create( 'Projected coordinates requested when projection did not occur properly in THerdList.writeXMLFile()' )
        ;

        try
          assignUnicode( xmlFile, fileName );
          rewrite( xmlFile );

          writeln( xmlFile, '<?xml version="1.0" encoding="UTF-16" ?>' );
          writeln( xmlFile,'<herds' );
          writeln( xmlFile,'  xmlns:naadsm="http://www.naadsm.org/schema"' );
          writeln( xmlFile,'  xmlns:xsd="http://www.w3.org/2001/XMLSchema"' );
          writeln( xmlFile,'  xmlns:gml="http://www.opengis.net/gml"' );
          writeln( xmlFile,'  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">' + endl );

          if( useProjection ) then
            begin
              writeln( xmlFile, '  <spatial_reference>' );
              writeln( xmlFile, '    <PROJ4>' + _proj.paramString + '</PROJ4>' );
              writeln( xmlFile, '  </spatial_reference>' + endl );
            end
          ;

          it := THerdListIterator.create( self );

          while( nil <> it.current() ) do
            begin
              h := it.current();

              if( not( h.removeFromDatabase ) ) then
                writeln( xmlFile, h.ssXML( useProjection ) );
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
          const writeProdTypeName: boolean;
          const writeInitialStateChar: boolean;
          const useProjection: boolean;
          errMsg: PString = nil
        ): boolean;
      var
        csvFile: TextFile;
        h: THerd;
        header: string;
      begin
        if( useProjection and not( isProjected ) ) then
          raise exception.create( 'Projected coordinates requested when projection did not occur properly in THerdList.writeCSVFile()' )
        ;

        try
          assignFile( csvFile, fileName );
          rewrite( csvFile );

          if( useProjection ) then
            begin
              header := ''
                + 'UnitID' + csvListSep
                + 'ProductionType' + csvListSep
                + 'UnitSize' + csvListSep
                + 'x' + csvListSep
                + 'y' + csvListSep
                + 'Status' + csvListSep
                + 'DaysInState' + csvListSep
                + 'DaysLeftInState'
              ;
            end
          else
            begin
              header := ''
                + 'UnitID' + csvListSep
                + 'ProductionType' + csvListSep
                + 'UnitSize' + csvListSep
                + 'Lat' + csvListSep
                + 'Lon' + csvListSep
                + 'Status' + csvListSep
                + 'DaysInState' + csvListSep
                + 'DaysLeftInState'
              ;
            end
          ;

          writeln( csvFile, header );

          h := self.first();
          while( nil <> h ) do
            begin
              if( not( h.removeFromDatabase ) ) then
                writeln( csvFile, h.csvText( writeProdTypeName, writeInitialStateChar, useProjection ) )
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
    function THerdList.populateDatabase( db: TSMDatabase; fnProgress: TObjFnBool1Int = nil; errMsg: pstring = nil ): boolean;
      var
        h: THerd;
        ptErrors: TCStringList;
        ptIDs: TQStringLongIntMap;
        res: TSqlResult;
        row: TSqlRow;
        db2: TSqlDatabase;
        success, failure: integer;
        ptID: string;
        str: string;
        taskTotal: integer;
        taskCounter: integer;

        vDict: TQStringVariantMap;

        idCounter: integer;

        mergeSuccess: boolean;
      begin
        result := false; // Until shown otherwise.

        vDict := TQStringVariantMap.Create();
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
        taskTotal := self.Count * 4;
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
                    db.quickUpdate( 'initialStateCode', naadsmDiseaseStateCode( h.initialStatus ) );
                    db.quickUpdate( 'daysInInitialState', h.daysInInitialState );
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


        // Iterate over the list to determine the last herd ID number in use
        //------------------------------------------------------------------
        idCounter := db.lastHerdID();
        if( 0 = idCounter ) then
          begin
            h := self.first();
            while( nil <> h ) do
              begin
                Application.ProcessMessages();
                inc( taskCounter );

                if( h.id > idCounter ) then
                  idCounter := h.id
                ;

                h := self.next();

                if( 0 = ( taskCounter mod 25 ) ) then
                  if( nil <> @fnProgress ) then fnProgress( (100*taskCounter) div taskTotal )
                ;
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
                    vdict['initialStateCode'] := naadsmDiseaseStateCode( h.initialStatus );
                    vdict['initialSize'] := h.initialSize;

                    h.setID( db.quickInsert( vDict, 'herdID' ) );
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

        mergeSuccess := db.mergeHerdTables();


        // Alert the user to any failures.
        //--------------------------------
        if( not( mergeSuccess ) ) then
          begin
            // result is false from aboce
            if( nil <> errMsg ) then
              errMsg^ := tr( 'Problems were encountered during import of this herd list.  Please check your file format, and verify that your scenario contains all desired units.' )
            ;
          end
        else if( failure > 0 ) then
          begin
            // result is false from above

            if( nil <> errMsg ) then
              begin
                errMsg^ := tr( 'Problems were encountered during import of this herd list.  Please check your file format, and verify that your scenario contains all desired units.' );
                str := tr( 'xyz unit records of out zyx could not be imported.' );
                str := ansiReplaceStr( str, 'xyz', intToStr( failure ) );
                str := ansiReplaceStr( str, 'zyx', intToStr( failure + success ) );
                errMsg^ := errMsg^ + endl + endl + str;
              end
            ;

            if( ptErrors.Count > 0 ) then
              begin
                if( nil <> errMsg ) then
                  begin
                    errMsg^ := errMsg^ + endl + endl
                    + tr( 'The following production types are not present in the scenario file:' )
                    + endl
                    + ptErrors.Text
                  end
                ;
              end
            ;
          end
        else
          begin
            result := true;
            if( nil <> errMsg ) then
              errMsg^ := ansiReplaceStr( tr( 'xyz unit records were successfully imported.' ), 'xyz', intToStr( success ) )
            ;
          end
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
                if( h.initialStatus in [NAADSMStateLatent, NAADSMStateSubclinical, NAADSMStateClinical] ) then diseaseFound := true;
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

        dbcout( 'isProjected: ' + usBoolToText( isProjected ), true );
        dbcout( 'Northwest LL: (' + usFloatToStr( maxLat ) + ', ' + usFloatToStr( minLon ) + ')', true );
        dbcout( 'Southeast LL: (' + usFloatToStr( minLat ) + ', ' + usFloatToStr( maxLon ) + ')', true );
        dbcout( 'Upper left XY : (' + usFloatToStr( minX ) + ', ' + usFloatToStr( maxY ) + ')', true );
        dbcout( 'Lower right XY: (' + usFloatToStr( maxX ) + ', ' + usFloatToStr( minY ) + ')', true );
        dbcout( endl, true );
        
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


    {*
      Used to double-check that inverse cartographic projection is OK.

      There is nothing in the GUI that uses inverse projection, but there
      are some places in the core library where it is required for output
      processing.

      Right now, this function is just used to generate a list for inspection.
      If necessary, it could be made more robust some day, so that the user
      could ensure that the projection system in use won't cause any problems.
    }
    procedure THerdList.invProjectDebug();
      var
        it: THerdListIterator;
        llr: RLatLon;
        h: THerd;
      begin
        it := THerdListIterator.create( self );

        while( nil <> it.current() ) do
          begin
            h := it.current();

            llr := _proj.pjInv( h.x, h.y );

            dbcout( 'Orig: ' + usFloatToStr( h.lat ) + ', ' + usFloatToStr( h.lon ), true );
            dbcout( 'invP: ' + usFloatToStr( llr.lat ) + ', ' + usFloatToStr( llr.lon ), true );
            dbcout( endl, true );
            
            it.incr();
          end
        ;

        it.Free();
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
                _smdb.quickUpdate( h.id, 'finalStateCode', naadsmDiseaseStateCode( h.diseaseStatus ) );
                _smdb.quickUpdate( 'finalControlStateCode', controlStatusCode( h.controlStatus ) );
                _smdb.quickUpdate( 'finalDetectionStateCode', detectionStatusCode( h.detectionStatus ) );
                
                _smdb.quickUpdate( 'cumulInfected', h.ctrlActivities.cumulInfections );
                _smdb.quickUpdate( 'cumulDetected', h.ctrlActivities.cumulDetections );
                _smdb.quickUpdate( 'cumulDestroyed', h.ctrlActivities.cumulDestructions );
                _smdb.quickUpdate( 'cumulVaccinated', h.ctrlActivities.cumulVaccinations );
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

        setEastAndWest();
        setMinMaxXY();
      end
    ;


    procedure THerdList.setMinMaxLL();
      var
        h: THerd;
        it: THerdListIterator;
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

                _maxX := MinDouble;
                _maxY := MinDouble;

                _minX := MaxDouble;
                _minY := MaxDouble;

                while( nil <> it.current() ) do
                  begin
                    h := it.current();

                    _maxLat := max( _maxLat, h.lat );
                    _minlat := min( _minLat, h.lat );
                    _maxLon := max( _maxLon, h.lon );
                    _minLon := min( _minLon, h.lon );

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

        it.Free();

        setEastAndWest();
        setMinMaxXY();
      end
    ;


    procedure THerdList.setEastAndWest();
      var
        h: THerd;
        it: THerdListIterator;

        adjMinLon, adjMaxLon: double;
        presLonW, presLonE: double;

        testLon, testLat: double;
      begin
        // Try to figure out which lon is "east" and which is "west"
        //-----------------------------------------------------------

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
        it := THerdListIterator.create( self );
        
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



    procedure THerdList.setMinMaxXY();
      var
        xyr: RPoint;

        procedure setMinsAndMaxes();
          begin
            _minX := min( _minX, xyr.x );
            _maxX := max( _maxX, xyr.x );
            _minY := min( _minY, xyr.y );
            _maxY := max( _maxY, xyr.y );
          end
        ;
      begin
        _minX := MaxDouble;
        _minY := MaxDouble;
        _maxX := MinDouble;
        _maxY := MinDouble;

        if( nil <> _proj ) then
          begin
            // Project all four combinations of min and max lat and lon,
            // and set min and max x and y accordingly.
            xyr := _proj.pjFwd( minLon, minLat );
            setMinsAndMaxes();
            xyr := _proj.pjFwd( maxLon, minLat );
            setMinsAndMaxes();
            xyr := _proj.pjFwd( minLon, maxLat );
            setMinsAndMaxes();
            xyr := _proj.pjFwd( maxLon, maxLat );
            setMinsAndMaxes();
          end
        ;
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


    function THerdList.getMinX(): double;
      begin
        if( MaxDouble = _minX ) then setMinMaxXY();
        assert( MaxDouble <> _minX );
        result := _minX;
      end
    ;


    function THerdList.getMaxX(): double;
      begin
        if( MinDouble = _maxX ) then setMinMaxXY();
        assert( MinDouble <> _maxX );
        result := _maxX;
      end
    ;


    function THerdList.getMinY(): double;
      begin
        if( MaxDouble = _minY ) then setMinMaxXY();
        assert( MaxDouble <> _minY );
        result := _minY;
      end
    ;


    function THerdList.getMaxY(): double;
      begin
        if( MinDouble = _maxY ) then setMinMaxXY();
        assert( MinDouble <> _maxY );
        result := _maxY;
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

        if ( count <= 1 ) then //  This is one more than you might assume because the above inherited call causes the count to update...
          begin
            _maxLat := dm.lat;
            _minLat := dm.lat;
            _maxLon := dm.lon;
            _minLon := dm.lon;
          end
        else
          begin
            if( dm.lat > maxLat ) then _maxLat := dm.lat;
            if( dm.lat < minLat ) then _minLat := dm.lat;

            if( dm.lon > maxLon ) then _maxLon := dm.lon;
            if( dm.lon < minLon ) then _minLon := dm.lon;
          end
        ;
        
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
