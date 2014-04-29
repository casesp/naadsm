unit AirborneSpreadParams;

(*
AirborneSpreadParams.pas
------------------------
Begin: 2005/01/21
Last revision: $Date: 2013-06-27 19:11:33 $ $Author: areeves $
Version number: $Revision: 1.5.6.8 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2005 - 2010 Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
    SysUtils,
    Dialogs,

    QLists,

    Sdew,

    SqlClasses,

    ChartFunction,
    ProbDensityFunctions,
    SMDatabase,
    Models,

    FunctionEnums
  ;

  type TAirborneSpreadParams = class( TModelWithFunctions )
    protected
      _xmlModelList: TQStringList;

      _id: integer;

      //-----------------------------------------------------------------------------
      // These may not be needed, now that I have a class for production type pair.
      //-----------------------------------------------------------------------------
      _fromProdType: string;
      _toProdType: string;
      _fromProdTypeID: integer;
      _toProdTypeID: integer;
      //-----------------------------------------------------------------------------

      _useAirborne: boolean;

      _probSpread1km: double;
      _windStart: integer;
      _windEnd: integer;
      _maxSpread: double;

      _pdfDelayName: string;

      procedure initialize();

      // XML import
      function getXmlModelList(): TQStringList;

      //-----------------------------------------------------------------------------
      // These may not be needed, now that I have a class for production type pair.
      //-----------------------------------------------------------------------------
      procedure setFromProdType( val: string );
      procedure setToProdType( val: string );
      procedure setFromProdTypeID( val: integer );
      procedure setToProdTypeID( val: integer );
      function getFromProdType(): string;
      function getToProdType(): string;
      function getFromProdTypeID(): integer;
      function getToProdTypeID(): integer;
      //-----------------------------------------------------------------------------

      procedure setUseAirborne( val: boolean );
      procedure setProbSpread1km( val: double );
      procedure setWindStart( val: integer );
      procedure setWindEnd( val: integer );
      procedure setMaxSpread( val: double );

      function getUseAirborne(): boolean;
      function getProbSpread1km(): double;
      function getWindStart(): integer;
      function getWindEnd(): integer;
      function getMaxSpread(): double;

      procedure setID( val: integer );
      function getID(): integer;

      procedure setPdfDelayName( val: string );
      function getPdfDelayName(): string;

      function getPdfDelay(): TPdf;

      // Overridden from TModelWithFunctions
      //------------------------------------
      function getChartSet(): TChartSet; override;

      // Overridden from TModel
      //-----------------------
      function getUpdated(): boolean; override;

    public
      constructor create(); overload;
      constructor create( sim: TObject; toProdTypeID, fromProdTypeID: integer ); overload;
      constructor create( const src: TAirborneSpreadParams; sim: TObject ); overload;

      constructor create(
        db: TSMDatabase;
        modelID: integer;
        sim: TObject;
        srcID: integer;
        destID: integer;
        populateFromDB: boolean
      ); overload;

      destructor destroy(); override;


      // Overridden from TModel
      //-----------------------
      procedure debug(); override;
      function validate( err: PString = nil ): boolean; override;
      function ssXml(): string; override;
      function populateDatabase( db: TSMDatabase; fromPTP: boolean = false ): integer; reintroduce;

      // Overridden from TModelWithFunctions
      //------------------------------------
      procedure removeChart( const chartName: string ); override;
      function chart( const whichChart: TSMChart; addlInfo: integer = -1 ): TChartFunction; override;
      procedure setChart( const whichChart: TSMChart; fn: TChartFunction; addlInfo: integer = -1 ); override;
      function hasChartName( const chartName: string; const whichChart: TSMChart ): boolean; override;
      function functionsAreValid(): boolean; override;

      // XML import
      //-----------
      class function createXmlModelList(): TQStringList;
      procedure importXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
      property xmlModelList: TQStringList read getXmlModelList;

      property id: integer read getID write setID;

      property fromProdType: string read getFromProdType write setFromProdType;
      property toProdType: string read getToProdType write setToProdType;
      property fromProdTypeID: integer read getFromProdTypeID; // write setFromProdTypeID;
      property toProdTypeID: integer read getToProdTypeID; // write setToProdTypeID;
      property probSpread1km: double read getProbSpread1km write setProbSpread1km;
      property windStart: integer read getWindStart write setWindStart;
      property windEnd: integer read getWindEnd write setWindEnd;
      property maxSpread: double read getMaxSpread write setMaxSpread;
      property useAirborne: boolean read getUseAirborne write setUseAirborne;
      property isValid: boolean read getIsValid;

      property pdfDelayName: string read getPdfDelayName write setPdfDelayName;

      property pdfDelay: TPdf read getPdfDelay;
    end
  ;


implementation

  uses
    Math,
    StrUtils,
    Variants,

    RoundToXReplacement_3c,
    MyStrUtils,
    DebugWindow,
    I88n,
    ARMath,
    
    SMSimulationInput,
    ProductionType,
    FunctionDictionary
  ;
  
  const
    DBSHOWMSG: boolean = false; // set to true to enable debugging messages for this unit.

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  procedure TAirborneSpreadParams.initialize();
    begin
      id := -1;
      _sim := nil;
      useAirborne := false;
      setFromProdTypeID( -1 );
      setToProdTypeID( -1 );
      windStart := 0;
      windEnd := 0;
      maxSpread := 1.0;
      probSpread1km := 0;
      _updated := false;

      _xmlModelList := nil;

      setPdfDelayName( '' );
    end
  ;

  constructor TAirborneSpreadParams.create();
    begin
      inherited create();
      initialize();
    end
  ;


  constructor TAirborneSpreadParams.create( sim: TObject; toProdTypeID, fromProdTypeID: integer );
    begin
      inherited create();
      initialize();

      _sim := sim;
      _fromProdTypeID := fromProdTypeID;
      _toProdTypeID := toProdTypeID;

      _updated := true;
    end
  ;


  constructor TAirborneSpreadParams.create( const src: TAirborneSpreadParams; sim: TObject );
    begin
      inherited create( src );

      _sim := sim;

      _id := src._id;

      _fromProdType := src._fromProdType;
      _toProdType := src._toProdType;
      _fromProdTypeID := src._fromProdTypeID;
      _toProdTypeID := src._toProdTypeID;

      _useAirborne := src._useAirborne;

      _probSpread1km := src._probSpread1km;
      _windStart := src._windStart;
      _windEnd := src._windEnd;
      _maxSpread := src._maxSpread;

      setPdfDelayName( src._pdfDelayName );

      _updated := src._updated;
    end
  ;


  constructor TAirborneSpreadParams.create(
        db: TSMDatabase;
        modelID: integer;
        sim: TObject;
        srcID: integer;
        destID: integer;
        populateFromDB: boolean
      );
    var
      q: string;
      res: TSqlResult;
      row: TSqlRow;
      db2: TSqlDatabase;
    begin
      inherited create();
      initialize();

      _sim := sim;

      setToProdTypeID( destID );
      setFromProdTypeID( srcID );

      if( populateFromDB ) then
        begin
          db2 := db as TSqlDatabase;

          q := 'SELECT '
            + ' inDiseaseSpread.transportDelayPdfID,'
            + ' delayChart.chartName AS delayChartName,'
            + ' inDiseaseSpread.probAirborneSpread1km,' // Used for airborne, not contact
            + ' inDiseaseSpread.maxDistAirborneSpread,' // Used for airborne, not contact
            + ' inDiseaseSpread.windDirectionStart,' // Used for airborne, not contact
            + ' inDiseaseSpread.windDirectionEnd' // Used for airborne, not contact
            + ' FROM inDiseaseSpread '
            + ' LEFT OUTER JOIN inChart delayChart ON delayChart.chartID = inDiseaseSpread.transportDelayPdfID'
            + ' WHERE spreadID = ' + intToStr( modelID )
          ;

          res := TSqlResult.create( q, db2 );

          if( res.numRows = 1 ) then
            begin
              setID( modelID );
              useAirborne := true;

              row := res.fetchArrayFirst();

              if( null <> row.field('maxDistAirborneSpread') ) then setMaxSpread(  row.field( 'maxDistAirborneSpread' ) );
              if( null <> row.field('windDirectionStart') ) then setWindStart( row.field( 'windDirectionStart' ) );
              if( null <> row.field('windDirectionEnd') ) then setWindEnd( row.field( 'windDirectionEnd' ) );
              if( null <> row.field('probAirborneSpread1km') ) then probSpread1km := row.field('probAirborneSpread1km');

              if( null <> row.field('transportDelayPdfID') ) then
                begin
                  setPdfDelayName( row.field( 'delayChartName' ) );
                end
              ;
            end
          else if( res.numRows = 0 ) then
            // do nothing
          else
            raise exception.Create( 'Too many matching rows in TAirborneSpreadParams.create' )
          ;

          res.Free();
        end
      ;

      updated := false;
    end
  ;


  destructor TAirborneSpreadParams.destroy();
    begin
      // The function dictionary is freed elsewhere.
      // PDFs are handled by the function dictionary:
      // don't free them here, but do decrement their counters.
      setPdfDelayName( '' );

      freeAndNil( _xmlModelList );

      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Database functions
//-----------------------------------------------------------------------------
  function TAirborneSpreadParams.populateDatabase( db: TSMDatabase; fromPTP: boolean = false ): integer;
    var
      q: string;
      dict: TQueryDictionary;
      ptpField: string;
      dbAction: TQueryType;
    begin

      if( -1 = id ) then
        dbAction := QInsert
      else
        dbAction := QUpdate
      ;

      // populate the chart and record their IDs, if necessary.
      if( nil <> pdfDelay ) then
        if( -1 = pdfDelay.id ) then pdfDelay.id := pdfDelay.populateDatabase( db )
      ;

      // See if there's a record of the production type pair.
      // If not, create one.
      if( not fromPTP ) then
        db.makeProductionTypePair( self.fromProdTypeID, self.toProdTypeID )
      ;

      // once the chart is in, add the record for the spread model
      dict := TQueryDictionary.create();

      ptpField := 'airborneContactSpreadID';

      dict['spreadMethodCode'] := '"A"';

      dict['windDirectionStart'] := intToStr( windStart );
      dict['windDirectionEnd'] := intToStr( windEnd );
      dict['probAirborneSpread1km'] := usFloatToStr( probSpread1km );
      dict['maxDistAirborneSpread'] := usFloatToStr( maxSpread );

      if( pdfDelay <> nil ) then
        dict['transportDelayPdfID']      := intToStr( pdfDelay.id )
      else
        dict['transportDelayPdfID']      := 'NULL'
      ;

      if( dbAction = QInsert ) then
        begin
          q := writeQuery( 'inDiseaseSpread', QInsert, dict );
          db.execute( q );
          id := db.lastInsertID();

          if( not fromPTP ) then
            begin
              q := 'UPDATE inProductionTypePair '
                + 'SET ' + ptpField + ' = ' + intToStr( id ) + ' '
                + 'WHERE sourceProductionTypeID = ' + intToStr( self.fromProdTypeID ) + ' '
                + 'AND destProductionTypeID = ' + intToStr( self.toProdTypeID )
              ;
              db.execute( q );
            end
          ;
        end
      else // Update the spread model record
        begin
          q := writeQuery( 'inDiseaseSpread', QUpdate, dict, 'WHERE [spreadID] = ' + intToStr( id ) );
          db.execute( q );
        end
      ;

      dict.Free();

      _updated := false;

      result := id;
    end
  ;
//-----------------------------------------------------------------------------



  function TAirborneSpreadParams.ssXml(): string;
    var
      str: string;
      strExp: string;
      useAirborneExponentialDecay: boolean;
    begin
      useAirborneExponentialDecay := ( _sim as TSMSimulationInput).useAirborneExponentialDecay;

      if( useAirborneExponentialDecay ) then
        strExp := 'airborne-spread-exponential-model'
      else
        strExp := 'airborne-spread-model'
      ;

      str := '';

      str := str + '  <' + strExp + ' from-production-type = "' + encodeXml( fromProdType )
        + '" to-production-type = "' + encodeXml( toProdType )
        + '">' + endl
      ;

      str := str + '    <prob-spread-1km>' + usFloatToStr( probSpread1km ) + '</prob-spread-1km>' + endl;

      str := str + '    <wind-direction-start>' + endl
        + '      <value>' + intToStr( windStart ) + '</value>' + endl
        + '      ' + chartUnitTypeAsSSXml( UDegrees ) + endl
        + '    </wind-direction-start>' + endl
      ;

      str := str + '    <wind-direction-end>' + endl
        + '      <value>' + intToStr( windEnd ) + '</value>' + endl
        + '      ' + chartUnitTypeAsSSXml( UDegrees ) + endl
        + '    </wind-direction-end>' + endl
      ;

      if( not( useAirborneExponentialDecay ) ) then
        begin
          str := str + '    <max-spread>' + endl
            + '      <value>' + usFloatToStr( maxSpread ) + '</value>' + endl
            + '      ' + chartUnitTypeAsSSXml( UKilometers ) + endl
            + '    </max-spread>' + endl
          ;
        end
      ;

      str := str + '    <delay>' + endl;
      str := str + pdfDelay.ssXml( 3 );
      str := str + '    </delay>' + endl;

      str := str + '  </' + strExp + '>' + endl;

      result := str;
    end
  ;


  function TAirborneSpreadParams.validate( err: PString = nil ): boolean;
    var
      msg: string;
      submsg: string;
      useExp: boolean;
    begin
      // Contact models must have all of these defined:
      //   pdfDelay
      //   windStart >= 0
      //   windEnd >= 0
      //   maxSpread > 1, unless using exponential decay
      //   1 >= probSpread1km >= 0

      if( nil = _sim ) then
        begin
          raise exception.Create( 'The airborne spread params''s _sim is nil in TAirborneSpreadParams.validate()' );
          useExp := false;
        end
      else
        useExp := (_sim as TSMSimulationInput).useAirborneExponentialDecay
      ;

      result := true;

      if( 0 > windStart ) then
        begin
          if( nil <> err ) then msg := msg + '  ' + tr( 'Wind direction start angle is invalid.'  )+ endl;
          result := false;
        end
      ;

      if( 0 > windEnd ) then
        begin
          if( nil <> err ) then msg := msg + '  ' + tr( 'Wind direction end angle is invalid.' ) + endl;
          result := false;
        end
      ;

      if( not( useExp ) ) then
        begin
          if( 1.0 >= maxSpread ) then
            begin
              if( nil <> err ) then msg := msg + '  ' + tr( 'Maximum distance of spread must be greater than 1 km.' ) + endl;
              result := false;
            end
          ;
        end
      ;

      if( ( 0.0 > probSpread1km ) or ( 1.0 < probSpread1km ) ) then
        begin
          if( nil <> err ) then msg := msg + '  ' + tr( 'Probability of spread at 1 km is invalid.' ) + endl;
          result := false;
        end
      ;

      if( useExp and ( 1.0 = probSpread1km ) ) then
        begin
          if( nil <> err ) then msg := msg + '  ' + tr( 'Probability of spread at 1 km must be less than 1 for exponential decay.' ) + endl;
          result := false;
        end
      ;

      submsg := '';
      if( nil = pdfDelay ) then
        begin
          if( nil <> err ) then msg := msg + '  ' + tr( 'Transport delay PDF is not set.' ) + endl;
          result := false;
        end
      else if( not( pdfDelay.validate( @submsg ) ) ) then
        begin
          if( nil <> err ) then msg := msg + '  ' + tr( 'Transport delay PDF is not valid:' ) + ' ' + submsg + endl;
          result := false;
        end
      ;

      if( ( false = result ) and ( nil <> err ) ) then
        begin
          msg := endl + ansiReplaceStr( ansiReplaceStr( tr( 'Airborne spread from xyz to zyx:' ) , 'xyz', fromProdType ), 'zyx', toProdType ) + endl + msg;
          err^ := err^ + msg;
        end
      ;

    end
  ;



  { debugging }
  procedure TAirborneSpreadParams.debug();
    var
      msg: string;
    begin;
      msg := 'BEGIN TAIRBORNESPREADPARAMS DEBUG' + endl;
      msg := msg + 'FromProdType: ' + fromProdType + ' (type ID ' + intToStr( fromProdTypeID ) + ')' + endl;
      msg := msg + 'ToProdType: ' + toProdType + ' (type ID ' + intToStr( toProdTypeID ) + ')' + endl;
      dbcout( msg + endl, true );


      msg := 'Wind direction start: ' + intToStr( windStart ) + endl;
      msg := msg + 'Wind direction end: ' + intToStr( windEnd ) + endl;

      dbcout( msg, true );
      dbcout( 'Prob. spread 1 km: ' + usFloatToStr( probSpread1km ) + endl, true );
      dbcout( 'Max dist. spread: ' + usFloatToStr( maxSpread ) + endl, true );

      if( nil <> pdfDelay ) then
        begin
          dbcout( 'Delay: ', true );
          pdfDelay.debug();
        end
      else
        dbcout( 'Delay is not set.', true )
      ;

      dbcout( 'END TAIRBORNESPREADPARAMS DEBUG' + endl, true );

    end;


//-----------------------------------------------------------------------------
// Property getter/setter functions
//-----------------------------------------------------------------------------
  procedure TAirborneSpreadParams.setID( val: integer ); begin _id := val; _updated := true; end;
  function TAirborneSpreadParams.getID(): integer; begin result := _id; _updated := true; end;


  procedure TAirborneSpreadParams.setPdfDelayName( val: string );
    begin
      val := trim( val );

      // Decrement the reference counter for the old function...
      decrFnRefCounter( _pdfDelayName );
      // ...and increment the reference counter for the new function.
      incrFnRefCounter( val );

      _pdfDelayName := val;
      _updated := true;
    end
  ;

  function TAirborneSpreadParams.getPdfDelayName(): string; begin result := _pdfDelayName; end;


  function TAirborneSpreadParams.getPdfDelay(): TPdf;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _pdfDelayName ) ) then
            begin
              if( fnDictionary.value( _pdfDelayName ).fn is TPdf ) then
                result := fnDictionary.value( _pdfDelayName ).fn as TPdf
              else
                begin
                  setPdfDelayName( '' );
                  result := nil;
                end
              ;
            end
          else
            result := nil
          ;
        end
      ;
    end
  ;


//------------------------------------------------------------------------------
// Overridden from TModelWithFunctions
//------------------------------------------------------------------------------
  procedure TAirborneSpreadParams.removeChart( const chartName: string );
    begin
      if( chartName = self.pdfDelayName ) then self.pdfDelayName := '';
    end
  ;


  function TAirborneSpreadParams.chart( const whichChart: TSMChart; addlInfo: integer = -1 ): TChartFunction;
    begin
      result := nil;

      if( AIRDelay = whichChart ) then
        begin
          if ( self.fnDictionary.contains( self.pdfDelayName ) ) then
            result := self.fnDictionary.value( self.pdfDelayName ).fn;
        end
      else
        //raise exception.create( 'Unrecognized contact type in TAirborneSpreadParams.chart()' )
      ;
    end
  ;


  procedure TAirborneSpreadParams.setChart( const whichChart: TSMChart; fn: TChartFunction; addlInfo: integer = -1 );
    var
      newName: string;
    begin
      if( nil = fn ) then
        newName := ''
      else
        newName := fn.name
      ;

      if( AIRDelay = whichChart ) then
        self.pdfDelayName := newName
      else
        //raise exception.create( 'Unrecognized contact type in TAirborneSpreadParams.chart()' )
      ;
    end
  ;


  function TAirborneSpreadParams.hasChartName( const chartName: string; const whichChart: TSMChart ): boolean;
    begin
      if( AIRDelay = whichChart ) then
        result := ( self.pdfDelayName = chartName )
      else
        begin
          result := false;
          //raise exception.create( 'Unrecognized contact type in TAirborneSpreadParams.chart()' );
        end
      ;
    end
  ;
  

  function TAirborneSpreadParams.functionsAreValid(): boolean;
    begin
      result := true;

      if( fnDictionary.contains( _pdfDelayName ) ) then
        begin
          if( not( fnDictionary.value( _pdfDelayName ).fn is TPdf ) ) then
            begin
              setPdfDelayName( '' );
              result := false;
            end
          ;
        end
      ;
    end
  ;


  //-----------------------------------------------------------------------------
  // These may not be needed, now that I have a class for production type pair.
  //-----------------------------------------------------------------------------
  procedure TAirborneSpreadParams.setFromProdType( val: string ); begin _fromProdType := val; _updated := true; end;
  procedure TAirborneSpreadParams.setToProdType( val: string ); begin _toProdType := val; _updated := true; end;
  procedure TAirborneSpreadParams.setFromProdTypeID( val: integer ); begin _fromProdTypeID := val; _updated := true; end;
  procedure TAirborneSpreadParams.setToProdTypeID( val: integer ); begin _toProdTypeID := val; _updated := true; end;


  function TAirborneSpreadParams.getFromProdType(): string;
    var
      sim: TSMSimulationInput;
    begin
      if( length( _fromProdType ) = 0 ) then
        begin
          if( nil = _sim ) then
              raise Exception.Create( 'TAirborneSpreadParams simulation is not set' )
          else
            begin
              sim := _sim as TSMSimulationInput;
              _fromProdType := sim.getProdTypeName( getFromProdTypeID() );
            end
          ;
        end
      ;

      result := _fromProdType;
    end
  ;



  function TAirborneSpreadParams.getToProdType(): string;
    var
      sim: TSMSimulationInput;
    begin
      if( length( _toProdType ) = 0 ) then
        begin
          if( _sim = nil ) then
              raise Exception.Create( 'TAirborneSpreadParams scenario is not set' )
          else
            begin
              sim := _sim as TSMSimulationInput;
              _toProdType := sim.getProdTypeName( getToProdTypeID() );
            end
          ;
        end
      ;

      result := _toProdType;
    end
  ;
  //-----------------------------------------------------------------------------

  procedure TAirborneSpreadParams.setUseAirborne( val: boolean ); begin _useAirborne := val; _updated := true; end;
  procedure TAirborneSpreadParams.setProbSpread1km( val: double ); begin _probSpread1km := val; _updated := true; end;
  procedure TAirborneSpreadParams.setWindStart( val: integer ); begin _windStart := val; _updated := true; end;
  procedure TAirborneSpreadParams.setWindEnd( val: integer ); begin _windEnd := val; _updated := true; end;
  procedure TAirborneSpreadParams.setMaxSpread( val: double ); begin _maxSpread := val; _updated := true; end;

  function TAirborneSpreadParams.getUseAirborne(): boolean; begin Result := _useAirborne; end;
  function TAirborneSpreadParams.getProbSpread1km(): double; begin Result := _probSpread1km; end;
  function TAirborneSpreadParams.getWindStart(): integer; begin Result := _windStart; end;
  function TAirborneSpreadParams.getWindEnd(): integer; begin Result := _windEnd; end;
  function TAirborneSpreadParams.getMaxSpread(): double; begin Result := _maxSpread; end;

  function TAirborneSpreadParams.getUpdated(): boolean; begin result := _updated; end;


  function TAirborneSpreadParams.getChartSet(): TChartSet;
    begin
      result := [ AIRDelay ];
    end
  ;


  function TAirborneSpreadParams.getFromProdTypeID(): integer;
    var
      val: integer;
      parentalUnit: TSMSimulationInput;
    begin
      if( PRODTYPEUNASSIGNED <> _fromProdTypeID ) then
        val := _fromProdTypeID
      else
        begin
          if( _sim = nil ) then
            begin
              raise Exception.Create( 'TAirborneSpreadParams parent is not set' );
              val := PRODTYPEUNASSIGNED;
            end
          else
            begin
              parentalUnit := _sim as TSMSimulationInput;
              val := parentalUnit.getProdTypeID( _fromProdType );
              setFromProdTypeID( val );
            end
          ;
        end
      ;

      if( PRODTYPEIDNOTFOUND = val ) then
        raise Exception.Create( 'Cannot find ptID for "' + _fromProdType +  '" in TAirborneSpreadParams' )
      ;

      Result := val;
    end
  ;


  function TAirborneSpreadParams.getToProdTypeID(): integer;
    var
      val: integer;
      parentalUnit: TSMSimulationInput;
    begin
      if( PRODTYPEUNASSIGNED <> _toProdTypeID ) then
        val := _toProdTypeID
      else
        begin
          if( _sim = nil ) then
            begin
              raise Exception.Create( 'TAirborneSpreadParams parent is not set' );
              val := PRODTYPEUNASSIGNED;
            end
          else
            begin
              parentalUnit := _sim as TSMSimulationInput;
              val := parentalUnit.getProdTypeID( _toProdType );
              setToProdTypeID( val );
            end
          ;
        end
      ;

      if( PRODTYPEIDNOTFOUND = val ) then
        raise Exception.Create( 'Cannot find ptID for "' + _toProdType +  '" in TAirborneSpreadParams' )
      ;

      Result := val;
    end
  ;
//-----------------------------------------------------------------------------



//------------------------------------------------------------------------------
// XML import
//------------------------------------------------------------------------------
  class function TAirborneSpreadParams.createXmlModelList(): TQStringList;
    begin
      result := TQStringList.create();
      result.Append( 'airborne-spread-exponential-model' );
      result.Append( 'airborne-spread-model' );
    end
  ;


  function TAirborneSpreadParams.getXmlModelList(): TQStringList;
    begin
      if( nil = _xmlModelList ) then
        _xmlModelList := createXmlModelList()
      ;

      result := _xmlModelList;
    end
  ;


  procedure TAirborneSpreadParams.importXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
    var
      subElement: pointer;
      pdf: TPdf;
      val: double;
      isExponential: boolean;
    begin
      if( 'airborne-spread-model' = sdew.GetElementName( model ) ) then
        isExponential := false
      else // airborne-spread-exponential-model
        isExponential := true
      ;
      (_sim as TSMSimulationInput).useAirborneExponentialDecay := isExponential;

      self.useAirborne := true;

      // Delay function
      //---------------
      subElement := Sdew.GetElementByName( model, 'delay' );
      if( nil = subElement ) then
        appendToPString( errMsg, tr( 'Airborne spread model XML does not contain a valid function for transport delay.' ) )
      else
        begin
          pdf := createPdfFromXml( subElement, Sdew );
          if( nil = pdf ) then
            appendToPString( errMsg, tr( 'Airborne spread model XML does not contain a valid function for transport delay.' ) )
          else
            begin
              if ( strIsEmpty( pdf.name ) ) then
                pdf.name := 'Airborne delay'
              ;

              pdf.dbField := word( AIRDelay );
              self.pdfDelayName := fnDictionary.checkAndInsert( pdf );
            end
          ;
        end
      ;

      // Wind directions
      //----------------
      subElement := Sdew.GetElementByName( model, 'wind-direction-start' );
      if ( nil = subElement ) then
        appendToPString( errMsg, tr( 'Airborne spread model XML does not contain valid values for wind direction.' ) )
      else
        begin
          subElement := Sdew.GetElementByName( subElement, 'value' );
          if ( nil = subElement ) then
            appendToPString( errMsg, tr( 'Airborne spread model XML does not contain valid values for wind direction.' ) )
          else
            begin
              val := usStrToFloat( Sdew.GetElementContents( subElement ), NaN );
              if( isNaN( val ) ) then
                appendToPString( errMsg, tr( 'Airborne spread model XML does not contain valid values for wind direction.' ) )
              else
                self.windStart := RoundDbl( val )
              ;
            end
          ;
        end
      ;

      subElement := Sdew.GetElementByName( model, 'wind-direction-end' );
      if ( nil = subElement ) then
        appendToPString( errMsg, tr( 'Airborne spread model XML does not contain valid values for wind direction.' ) )
      else
        begin
          subElement := Sdew.GetElementByName( subElement, 'value' );
          if ( nil = subElement ) then
            appendToPString( errMsg, tr( 'Airborne spread model XML does not contain valid values for wind direction.' ) )
          else
            begin
              val := usStrToFloat( Sdew.GetElementContents( subElement ), NaN );
              if( isNaN( val ) ) then
                appendToPString( errMsg, tr( 'Airborne spread model XML does not contain valid values for wind direction.' ) )
              else
                self.windEnd := RoundDbl( val )
              ;
            end
          ;
        end
      ;

      // Probability of spread at 1 km
      //------------------------------
      subElement := Sdew.GetElementByName( model, 'prob-spread-1km' );
      if( nil = subElement ) then
          appendToPString( errMsg, tr( 'Airborne spread model XML does not contain a valid value for probability of disease spread at 1 km.' ) )
      else
        begin
          val := usStrToFloat( Sdew.GetElementContents( subElement ), NaN );
          if( not isProbability( val ) ) then
            appendToPString( errMsg, tr( 'Airborne spread model XML does not contain a valid value for probability of disease spread at 1 km.' ) )
          else
            self.probSpread1km := val
          ;
        end
      ;

      // Max distance of spread (for linear decay)
      //------------------------------------------
      subElement := sdew.GetElementByName( model, 'max-spread' );

      // Linear decay must have a <max-spread> tag.
      if( ( nil = subElement ) and ( not isExponential ) ) then
        appendToPString( errMsg, tr( 'Airborne spread model XML does not contain a valid value for maximum distance of disease spread.' ) )

      // Exponential decay cannot have a <max-spread> tag.
      else if( ( nil <> subElement ) and isExponential ) then
        appendToPString( errMsg, tr( 'Airborne spread model XML does not contain a valid value for maximum distance of disease spread.' ) )

      // If we get this far, things seem OK.
      else if( not isExponential ) then
        begin
          subElement := Sdew.GetElementByName( subElement, 'value' );
          if( nil = subElement ) then
            appendToPString( errMsg, tr( 'Airborne spread model XML does not contain a valid value for maximum distance of disease spread.' ) )
          else
            begin
              val := usStrToFloat( Sdew.GetElementContents( subElement ), NaN );
              if( isNaN( val ) ) then
                appendToPString( errMsg, tr( 'Airborne spread model XML does not contain a valid value for maximum distance of disease spread.' ) )
              else
                self.maxSpread := val
              ;
            end
          ;
        end
      ;
    end
  ;
//------------------------------------------------------------------------------

end.
