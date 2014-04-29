unit ContactSpreadParams;

(*
ContactSpreadParams.pas
-----------------------
Begin: 2005/01/06
Last revision: $Date: 2013-06-27 19:11:33 $ $Author: areeves $
Version number: $Revision: 1.4.4.8 $
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

    ProbDensityFunctions,
    RelFunction,
    ChartFunction,
    SMDatabase,
    Models,

    FunctionEnums
  ;

  type TContactType = ( CMUnknown, CMDirect, CMIndirect );

  function contactTypeToString( val: TContactType ): string;
  
  function stringToContactType( str: string ): TContactType;

  type TContactSpreadParams = class( TModelWithFunctions )
    protected
      _xmlModelList: TQStringList;

      _id: integer;

      _contactType: TContactType;

      _fromProdType: string;
      _toProdType: string;
      _fromProdTypeID: integer;
      _toProdTypeID: integer;

      _meanContactRate: double;
      _contactRateUnits: string;
      _useFixedContactRate: boolean;
      _fixedContactRate: double;

      _probInfect: double;

      _latentCanInfect: boolean;
      _subClinicalCanInfect: boolean;

      _pdfDistanceName: string;
      _pdfDelayName: string;
      _relMovementControlName: string;

      function getChartSet(): TChartSet; override;

      procedure setID( val: integer );
      function getID(): integer;

      procedure setContactType( val: TContactType );
      function getContactType(): TContactType;

      procedure setPdfDistanceName( val: string );
      procedure setPdfDelayName( val: string );
      procedure setRelMovementControlName( val: string );

      function getPdfDistanceName(): string;
      function getPdfDelayName(): string;
      function getRelMovementControlName(): string;

      function getPdfDistance(): TPdf;
      function getPdfDelay(): TPdf;
      function getRelMovementControl(): TRelFunction;

      //------------------------------------------------------------------------------
      // These may not be needed, now that I have a class for production type pair.
      //------------------------------------------------------------------------------
      procedure setFromProdType( val: string );
      procedure setToProdType( val: string );
      procedure setFromProdTypeID( val: integer );
      procedure setToProdTypeID( val: integer );
      function getFromProdType(): string;
      function getToProdType(): string;
      function getFromProdTypeID(): integer;
      function getToProdTypeID(): integer;
      //------------------------------------------------------------------------------

      procedure setMeanContactRate( val: double );
      procedure setContactRateUnits( val: string );
      procedure setUseFixedContactRate( val: boolean );
      procedure setFixedContactRate( val: double );
      function getMeanContactRate(): double;
      function getContactRateUnits(): string;
      function getUseFixedContactRate(): boolean;
      function getFixedContactRate(): double;

      procedure setProbInfect( val: double );
      function getProbInfect(): double;

      procedure setLatentCanInfect( val: boolean );
      procedure setSubClinicalCanInfect( val: boolean );
      function getLatentCanInfect(): boolean;
      function getSubClinicalCanInfect(): boolean;

      function getUsePrevalence(): boolean;

      // XML import
      function getXmlModelList(): TQStringList;

      procedure initialize();

      // Overridden from TModel
      //-----------------------
      function getUpdated(): boolean; override;

    public
      constructor create(); overload;
      constructor create( contactType: TContactType; sim: TObject; toProdTypeID, fromProdTypeID: integer ); overload;

      constructor create(
        db: TSMDatabase;
        modelID: integer;
        ct: TContactType;
        sim: TObject;
        srcID: integer;
        destID: integer;
        populateFromDB: boolean
      ); overload;

      constructor create( const src: TContactSpreadParams; sim: TObject ); overload;

      destructor destroy(); override;

      // Overridden from TModel
      //-----------------------
      function validate( err: PString = nil ): boolean; override;
      procedure debug(); override;
      function ssXml(): string; override;
      function populateDatabase( db: TSMDatabase; fromPTP: boolean = false; action: TQueryType = QInsert ): integer; reintroduce;

      // Overridden from TModelWithFunctions
      //------------------------------------
      procedure removeChart( const chartName: string ); override;
      function chart( const whichChart: TSMChart; addlInfo: integer = -1 ): TChartFunction; override;
      procedure setChart( const whichChart: TSMChart; fn: TChartFunction; addlInfo: integer = -1 ); override;
      function hasChartName( const chartName: string; const whichChart: TSMChart ): boolean; override;
      procedure changeChart(
        const whichChart: TSMChart;
        const oldChartName: string;
        newChart: TChartFunction;
        addlInfo: integer = -1
      ); override;

      // XML import
      //-----------
      class function createXmlModelList(): TQStringList;
      procedure importXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
      property xmlModelList: TQStringList read getXmlModelList;

      function functionsAreValid(): boolean; override;

      // Properties
      //-----------
      property id: integer read getID write setID;

      property contactType: TContactType read getContactType write setContactType;

      //------------------------------------------------------------------------------
      // These may not be needed, now that I have a class for production type pair.
      //------------------------------------------------------------------------------
      property fromProdType: string read getFromProdType write setFromProdType;
      property toProdType: string read getToProdType write setToProdType;
      property fromProdTypeID: integer read getFromProdTypeID write setFromProdTypeID;
      property toProdTypeID: integer read getToProdTypeID write setToProdTypeID;
      //------------------------------------------------------------------------------

      property meanContactRate: double read getMeanContactRate write setMeanContactRate;
      property contactRateUnits: string read getContactRateUnits write setContactRateUnits;
      property useFixedContactRate: boolean read getUseFixedContactRate write setUseFixedContactRate;
      property fixedContactRate: double read getFixedContactRate write setFixedContactRate;

      property probInfect: double read getProbInfect write setProbInfect;

      property latentCanInfect: boolean read getLatentCanInfect write setLatentCanInfect;
      property subClinicalCanInfect: boolean read getSubClinicalCanInfect write setSubClinicalCanInfect;
      property usePrevalence: boolean read getUsePrevalence;

      property pdfDistanceName: string read getPdfDistanceName write setPdfDistanceName;
      property pdfDelayName: string read getPdfDelayName write setPdfDelayName;
      property relMovementControlName: string read getRelMovementControlName write setRelMovementControlName;

      property pdfDistance: TPdf read getPdfDistance;
      property pdfDelay: TPdf read getPdfDelay;
      property relMovementControl: TRelFunction read getRelMovementControl;

      property isValid: boolean read getIsValid;

      property updated: boolean read getUpdated;
    end
  ;


implementation

  uses
    StrUtils,
    Variants,
    Math,

    ARMath,
    MyStrUtils,
    DebugWindow,
    I88n,

    SMSimulationInput,
    ProductionType,
    FunctionDictionary
  ;

  const
    DBSHOWMSG: boolean = false; // set to true to enable debugging messages in this unit

//------------------------------------------------------------
// Global helper functions
//------------------------------------------------------------
  function contactTypeToString( val: TContactType ): string;
    begin
      case val of
        CMDirect: result := tr( 'Direct' );
        CMIndirect: result := tr( 'Indirect' );
        else result := tr( 'Unknown' );
      end;
    end
  ;

  
  function stringToContactType( str: string ): TContactType;
    begin
      str := fixup( str );

      if( str = 'indirect' ) then
        result := CMindirect
      else if( str = 'direct' ) then
        result := CMDirect
      else
        result := CMUnknown
      ;

    end
  ;
//------------------------------------------------------------



//------------------------------------------------------------
// Construction/initialization/destruction
//------------------------------------------------------------
  constructor TContactSpreadParams.create();
    begin
      inherited create();
      initialize();
    end
  ;


  constructor TContactSpreadParams.create( contactType: TContactType; sim: TObject; toProdTypeID, fromProdTypeID: integer );
    begin
      inherited create();
      initialize();
      setContactType( contactType );
      _sim := sim;
      _toProdTypeID := toProdTypeID;
      _fromProdTypeID := fromProdTypeID;
    end
  ;



  procedure TContactSpreadParams.initialize();
    begin
      _sim := nil;
      fromProdTypeID := -1;
      toProdTypeID := -1;
      id := -1;

      _xmlModelList := nil;

      setContactType( CMUnknown );
      setMeanContactRate( -1.0 );
      setUseFixedContactRate( false );
      setFixedContactRate( -1 );
      setProbInfect( -1.0 );

      _updated := false;
    end
  ;

  constructor TContactSpreadParams.create(
        db: TSMDatabase;
        modelID: integer;
        ct: TContactType;
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

      initialize();

      fromProdTypeID := srcID;
      toProdTypeID := destID;

      setContactType( ct );
      _sim := sim;

      if( populateFromDB ) then
        begin
          db2 := db as TSqlDatabase;

          q := 'SELECT'
            // + ' inDiseaseSpread.spreadID,' // Don't need this in the query
            // + ' inDiseaseSpread.spreadMethodCode,' // Don't need this in the query
            + ' inDiseaseSpread.latentCanInfect,'
            + ' inDiseaseSpread.subclinicalCanInfect,'
            + ' inDiseaseSpread.meanContactRate,'
            + ' inDiseaseSpread.useFixedContactRate,'
            + ' inDiseaseSpread.fixedContactRate,'
            + ' inDiseaseSpread.infectionProbability,'
            + ' inDiseaseSpread.distancePdfID,'
            + ' distanceChart.chartName AS distanceChartName,'
            + ' inDiseaseSpread.movementControlRelID,'
            + ' movementControlChart.chartName AS movementControlChartName,'
            + ' inDiseaseSpread.transportDelayPdfID,'
            + ' delayChart.chartName AS delayChartName'
            + ' FROM'
            + ' (('
            + ' inDiseaseSpread'
            + ' LEFT OUTER JOIN inChart distanceChart ON distanceChart.chartID = inDiseaseSpread.distancePdfID )'
            + ' LEFT OUTER JOIN inChart movementControlChart ON movementControlChart.chartID = inDiseaseSpread.movementControlRelID )'
            + ' LEFT OUTER JOIN inChart delayChart ON delayChart.chartID = inDiseaseSpread.transportDelayPdfID'
            + ' WHERE spreadID = ' + intToStr( modelID )
          ;

          res := TSqlResult.create( q, db2 );

          if( res.numRows = 1 ) then
            begin
              setID( modelID );

              row := res.fetchArrayFirst();

              if( null <> row.field( 'latentCanInfect' ) ) then setLatentCanInfect( row.field( 'latentCanInfect' ) <> 0  );
              if( null <> row.field( 'subclinicalCanInfect' ) ) then setSubclinicalCanInfect( row.field( 'subclinicalCanInfect' ) <> 0 );

              if( null <> row.field( 'meanContactRate' ) ) then setMeanContactRate( row.field( 'meanContactRate' ) );

              if( null <> row.field( 'useFixedContactRate' ) ) then setUseFixedContactRate( row.field( 'useFixedContactRate' ) );
              if( null <> row.field( 'fixedContactRate' ) ) then setFixedContactRate( row.field( 'fixedContactRate' ) );

              if( null <> row.field( 'infectionProbability' ) ) then setProbInfect( row.field( 'infectionProbability' ) );

              if( null <> row.field( 'distancePdfID' ) ) then
                begin
                  setPdfDistanceName( row.field( 'distanceChartName' ) );
                end
              ;

              if( null <> row.field( 'transportDelayPdfID' ) ) then
                begin
                  setPdfDelayName( row.field( 'delayChartName' ) );
                end
              ;

              if( null <> row.field( 'movementControlRelID' ) ) then
                begin
                  setrelMovementControlName( row.field( 'movementControlChartName' ) );
                end
              ;

            end
          else if( res.numRows = 0 ) then
            // do nothing
          else
            raise exception.Create( 'Too many matching rows in TContactSpreadParams.create' )
          ;

          res.Free();
        end
      ;

      _updated := false;
    end
  ;


  constructor TContactSpreadParams.create( const src: TContactSpreadParams; sim: TObject );
    begin
      inherited create( src );

      _sim := sim;

      _id := src._id;

      _contactType := src._contactType;

      _fromProdType := src._fromProdType;
      _toProdType := src._toProdType;
      _fromProdTypeID := src._fromProdTypeID;
      _toProdTypeID := src._toProdTypeID;

      _meanContactRate := src._meanContactRate;
      _contactRateUnits := src._contactRateUnits;
      _useFixedContactRate := src._useFixedContactRate;
      _fixedContactRate := src._fixedContactRate;

      _probInfect := src._probInfect;

      _latentCanInfect := src._latentCanInfect;
      _subClinicalCanInfect := src._subClinicalCanInfect;

      setPdfDistanceName( src._pdfDistanceName );
      setPdfDelayName( src._pdfDelayName );
      setRelMovementControlName( src._relMovementControlName );

      _updated := src._updated;
    end
  ;


  destructor TContactSpreadParams.destroy();
    begin
      // The function dictionary is freed elsewhere.
      // Disease periods are handled by the function dictionary:
      // don't free them here, but do decrement their counters.
      setPdfDistanceName( '' );
      setPdfDelayName( '' );
      setRelMovementControlName( '' );

      freeAndNil( _xmlModelList );

      inherited destroy();
    end
  ;
//------------------------------------------------------------



//------------------------------------------------------------
// Database functions
//------------------------------------------------------------
  function TContactSpreadParams.populateDatabase( db: TSMDatabase; fromPTP: boolean = false; action: TQueryType = QInsert ): integer;
    var
      q: string;
      dict: TQueryDictionary;
      ptpField: string;
    begin

      // Populate the charts and record their IDs, if necessary.
      //--------------------------------------------------------
      if( nil <> pdfDistance ) then
        if( 0 > pdfDistance.id ) then pdfDistance.id := pdfDistance.populateDatabase( db )
      ;


      if( nil <> pdfDelay ) then
        if( 0 > pdfDelay.id ) then pdfDelay.id := pdfDelay.populateDatabase( db )
      ;


      if( nil <> relMovementControl ) then
        if( 0 > relMovementControl.id ) then relMovementControl.id := relMovementControl.populateDatabase( db )
      ;

      if( not fromPTP ) then
        begin
          // See if there's a record of the production type pair.
          // If not, create one.
          db.makeProductionTypePair( self.fromProdTypeID, self.toProdTypeID );
        end
      ;


      // Once the charts are in, add the record for the contact model.
      //==============================================================

      // Create the query
      //-----------------
      dict := TQueryDictionary.create();

      dict['meanContactRate'] := DATABASE_NULL_VALUE;
      dict['fixedContactRate'] := DATABASE_NULL_VALUE;
      dict['infectionProbability'] := DATABASE_NULL_VALUE;
      
      dict['distancePdfID'] := DATABASE_NULL_VALUE;
      dict['movementControlRelID'] := DATABASE_NULL_VALUE;
      dict['transportDelayPdfID'] := DATABASE_NULL_VALUE;

      if( CMDirect = contactType ) then
        begin
          dict['spreadMethodCode'] := '"D"';
          ptpField := 'directContactSpreadID';
        end
      else if( CMIndirect = contactType ) then
        begin
          dict['spreadMethodCode'] := '"I"';
          ptpField := 'indirectContactSpreadID';
        end
      ;

      dict['latentCanInfect'] := usBoolToText( latentCanInfect );
      dict['subclinicalCanInfect'] := usBoolToText( subClinicalCanInfect );

      dict['useFixedContactRate'] := usBoolToText( useFixedContactRate );
      if( 0.0 <= meanContactRate ) then dict['meanContactRate'] := usFloatToStr( meanContactRate );
      if( 0 <= fixedContactRate ) then dict['fixedContactRate'] := usFloatToStr( fixedContactRate );

      if( 0 <= probInfect ) then dict['infectionProbability'] := usFloatToStr( probInfect );

      if( nil <> pdfDistance ) then
        dict['distancePdfID'] := intToStr( pdfDistance.id )
      ;

      if( nil <> relMovementControl ) then
        dict['movementControlRelID'] := intToStr( relMovementControl.id )
      ;

      if( nil <> pdfDelay ) then
        dict['transportDelayPdfID'] := intToStr( pdfDelay.id )
      ;


      // Execute the query
      //-------------------
      if( QInsert = action ) then
        begin
          q := writeQuery( 'inDiseaseSpread', QInsert, dict );
          db.execute( q );
          id := db.lastInsertID();

          if( not fromPTP ) and (length( ptpField ) >0 )then  //If the contactType has not yet been set, CAN NOT run this update! [Causes an SQL syntax error):10-12/2006, SPC 
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
      else // Update the contact model record
        begin
          if( -1 = id ) then
            begin
              q := writeQuery( 'inDiseaseSpread', QInsert, dict );
              db.execute( q );
              id := db.lastInsertID();
            end
          else
            begin
              q := writeQuery( 'inDiseaseSpread', QUpdate, dict, 'WHERE `spreadID` = ' + intToStr( id ) );
              db.execute( q );
            end
          ;
        end
      ;

      dict.Free();

      _updated := false;

      result := id;
    end
  ;
//------------------------------------------------------------




  function TContactSpreadParams.validate( err: PString = nil ): boolean;
    var
      msg: string;
      submsg: string;
    begin
      // Contact models must have all of these defined:
      //  - relMovementControl
      //  - pdfDelay
      //  - pdfDistance
      //  - probInfect >= 0
      //  - meanContactRate >= 0

      dbcout( '%%%%% Validating contact spread params', DBSHOWMSG );
      dbcout( 'ToProdType: ' + toProdType, DBSHOWMSG );
      dbcout( 'FromProdType: ' + fromProdType, DBSHOWMSG );

      result := true;

      if
        ( 0 > probInfect )
      and
        ( ( CMIndirect = _contactType ) or ( ( CMDirect = _contactType ) and ( not (_sim as TSMSimulationInput).useWithinHerdPrevalence ) ) )
      then
        begin
          if( err <> nil ) then msg := msg + '  ' + tr( 'Probability of infection transfer is not set.' ) + endl;
          result := false;
        end
      ;

      dbcout( 'Trying to check fixed contact rate', DBSHOWMSG );
      dbcout( 'UseFixedContactRate: ' + usBoolToText( useFixedContactRate ), DBSHOWMSG );
      dbcout( 'fixedContactRate: ' + usFloatToStr( fixedContactRate ), DBSHOWMSG );

      if( useFixedContactRate ) then
        begin
          if( 0 > fixedContactRate ) then
            begin
              if( nil <> err ) then msg := msg + '  ' + tr( 'Fixed contact rate is not set.' ) + endl;
              result := false;
            end
          ;
        end
      else if( 0 > meanContactRate ) then
        begin
          if( nil <> err ) then msg := msg + '  ' + tr( 'Mean contact rate is not set.' ) + endl;
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

      submsg := '';
      if( nil = pdfDistance ) then
        begin
          if( nil <> err ) then msg := msg + '  ' + tr( 'Transport distance PDF is not set.' ) + endl;
          result := false;
        end
      else if( not( pdfDistance.validate( @submsg ) ) ) then
        begin
          if( nil <> err ) then msg := msg + '  ' + tr( 'Transport distance PDF is not valid:' ) + ' ' + submsg + endl;
          result := false;
        end
      ;

      submsg := '';
      if( nil = relMovementControl ) then
        begin
          if( nil <> err ) then msg := msg + '  ' + tr( 'Movement control function is not set.' ) + endl;
          result := false;
        end
      else if( not( relMovementControl.validate( @submsg ) ) ) then
        begin
          if( nil <> err ) then msg := msg + '  ' + tr( 'Movement control function is not valid:' ) + ' ' + submsg + endl;
          result := false;
        end
      ;

      if( ( false = result ) and ( nil <> err ) ) then
        begin
          msg :=
            endl
            + ansiReplaceStr( ansiReplaceStr( ansiReplaceStr( tr( 'Direct/indirect contact from xyz to zyx:' ), 'Direct/indirect', contactTypeToString( contactType ) ), 'xyz', fromProdType ), 'zyx', toProdType )
            + endl + msg
          ;
          err^ := err^ + msg;
        end
      ;

      dbcout( 'Done validating', DBSHOWMSG );
    end
  ;



  function TContactSpreadParams.ssXml(): string;
    var
      str: string;
      ct: string;
      usePrevalence: boolean;
    begin
      usePrevalence := (_sim as TSMSimulationInput).useWithinHerdPrevalence;

      if( contactType = CMDirect ) then
        ct := '"direct"'
      else if( contactType = CMIndirect ) then
        ct := '"indirect"'
      ;

      str := '';

      str := '  <contact-spread-model from-production-type = "' + encodeXml( fromProdType )
        + '" to-production-type = "' + encodeXml( toProdType )
        + '" contact-type = ' + ct
        + '>' + endl
      ;


      if( useFixedContactRate ) then
        begin
          str := str + '    <fixed-movement-rate>' + endl
            + '      <value>' + usFloatToStr( fixedContactRate ) + '</value>'
            + chartUnitTypeAsSSXml( UPerDay )
            + '    </fixed-movement-rate>' + endl;
          ;
        end
      else
        begin
          str := str + '    <movement-rate>' + endl
            + '      <value>' + usFloatToStr( meanContactRate ) + '</value>'
            + chartUnitTypeAsSSXml( UPerDay ) + endl
            + '    </movement-rate>' + endl;
          ;
        end
      ;


      str := str + '    <distance>' + endl;
      str := str + pdfDistance.ssXml( 3 );
      str := str + '    </distance>' + endl;

      str := str + '    <delay>' + endl;
      str := str + pdfDelay.ssXml( 3 );
      str := str + '    </delay>' + endl;

      if
        ( not( usePrevalence ) )
      or
        ( not( CMDirect = contactType ) )
      then
        str := str + '    <prob-infect>' + usFloatToStr( probInfect ) + '</prob-infect>' + endl
      else
        str := str + '    <!-- This scenario uses within-herd prevalence instead of probability of infection transfer -->' + endl
      ;

      str := str + '    <movement-control>' + endl;
      str := str +  relMovementControl.ssXml( 3 );
      str := str + '    </movement-control>' + endl;

      str := str + '    <latent-units-can-infect>' + usBoolToText( latentCanInfect ) + '</latent-units-can-infect>' + endl;
      str := str + '    <subclinical-units-can-infect>' + usBoolToText( subclinicalCanInfect ) + '</subclinical-units-can-infect>' + endl;

      str := str + '  </contact-spread-model>' + endl;

      result := str;
    end
  ;


{ debugging }
  procedure TContactSpreadParams.debug();
    var
      msg: string;
    begin;
      msg := 'BEGIN TCONTACTSPREADPARAMS DEBUG' + endl;
      msg := msg + 'ContactType: ' + contactTypeToString( contactType ) + endl;
      msg := msg + 'FromProdType: ' + fromProdType + ' (type ID ' + intToStr( fromProdTypeID ) + ')' + endl;
      msg := msg + 'ToProdType: ' + toProdType + ' (type ID ' + intToStr( toProdTypeID ) + ')' + endl;

      if( useFixedContactRate ) then
        msg := msg + 'fixedContactRate: ' + usFloatToStr( fixedContactRate ) + ' Units: ' + contactRateUnits + endl
      else
        msg := msg + 'meanContactRate: ' + usFloatToStr( meanContactRate ) + ' Units: ' + contactRateUnits + endl
      ;
      dbcout( msg + endl, true );

      if( nil <> pdfDistance ) then
        begin
          dbcout( 'pdfDistance: ' + endl, true );
          pdfDistance.debug();
        end
      else
        dbcout( 'distance is nil!', true )
      ;
      dbcout( endl, true );

      if( nil <> pdfDelay ) then
        begin
          dbcout( 'pdfDelay: ' + endl, true );
          pdfDelay.debug();
        end
      else
        dbcout( 'delay is nil!', true )
      ;
      dbcout( endl, true );

      if( nil <> relMovementControl ) then
        begin
          dbcout( 'relMovementControl: ' + endl, true );
          relMovementControl.debug();
        end
      else
        dbcout( 'movementControl is nil!', true )
      ;
      dbcout( endl, true );

      dbcout( 'probInfect: ' + usFloatToStr( probInfect ) + endl, true );

      msg := 'LatentCanInfect: ';
      if( latentCanInfect ) then msg := msg + 'true'
        else msg := msg + 'false';

      dbcout( msg + endl, true );
      dbcout( 'END TCONTACTSPREADPARAMS DEBUG' + endl, true );
    end
  ;


{ property getter/setter functions }

  procedure TContactSpreadParams.setID( val: integer );
    begin
      _id := val;
      _updated := true;
    end
  ;


  procedure TContactSpreadParams.setContactType( val: TContactType );
    begin
      _contactType := val;
      _updated := true;
    end
  ;


  procedure TContactSpreadParams.setMeanContactRate( val: double );
    begin
      _meanContactRate := val;
      _updated := true;
    end
  ;


  procedure TContactSpreadParams.setContactRateUnits( val: string );
    begin
      _contactRateUnits := val;
      _updated := true;
    end
  ;


  procedure TContactSpreadParams.setUseFixedContactRate( val: boolean );
    begin
      _useFixedContactRate := val;
      _updated := true;
    end
  ;


  procedure TContactSpreadParams.setFixedContactRate( val: double );
    begin
      _fixedContactRate := val;
      _updated := true;
    end
  ;


  procedure TContactSpreadParams.setProbInfect( val: double );
    begin
      _probInfect := val;
      _updated := true;
    end
  ;


  procedure TContactSpreadParams.setLatentCanInfect( val: boolean );
    begin
      _latentCanInfect := val;
      _updated := true;
    end
  ;


  procedure TContactSpreadParams.setSubClinicalCanInfect( val: boolean );
    begin
      _subClinicalCanInfect := val;
      _updated := true;
    end
  ;


  function TContactSpreadParams.getID(): integer; begin result := _id; end;
  function TContactSpreadParams.getContactType(): TContactType; begin Result := _contactType; end;
  function TContactSpreadParams.getMeanContactRate(): double; begin Result := _meanContactRate; end;
  function TContactSpreadParams.getContactRateUnits(): string; begin Result := _contactRateUnits; end;
  function TContactSpreadParams.getUseFixedContactRate(): boolean; begin result := _useFixedContactRate; end;
  function TContactSpreadParams.getFixedContactRate(): double; begin result := _fixedContactRate; end;
  function TContactSpreadParams.getProbInfect(): double; begin Result := _probInfect; end;


  function TContactSpreadParams.getChartSet(): TChartSet;
    begin
      case self.contactType of
        CMDirect: result := [ CMDistanceDirect, CMDelayDirect, CMMovementControlDirect ];
        CMIndirect: result := [ CMDistanceIndirect, CMDelayIndirect, CMMovementControlIndirect ];
        else
          begin
            raise exception.Create( 'Contact type is not set in TContactSpreadParams.getChartSet()' );
            result := [];
          end
        ;
      end;
    end
  ;


  procedure TContactSpreadParams.setPdfDistanceName( val: string );
    begin
      val := trim( val );

      // Decrement the reference counter for the old function...
      decrFnRefCounter( _pdfDistanceName );
      // ...and increment the reference counter for the new function.
      incrFnRefCounter( val );

      _pdfDistanceName := val;
      _updated := true;
    end
  ;

  procedure TContactSpreadParams.setPdfDelayName( val: string );
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


  procedure TContactSpreadParams.setRelMovementControlName( val: string );
    begin
      val := trim( val );

      // Decrement the reference counter for the old function...
      decrFnRefCounter( _relMovementControlName );
      // ...and increment the reference counter for the new function.
      incrFnRefCounter( val );

      _relMovementControlName := val;
      _updated := true;
    end
  ;

  function TContactSpreadParams.getPdfDistanceName(): string; begin result := _pdfDistanceName; end;
  function TContactSpreadParams.getPdfDelayName(): string; begin result := _pdfDelayName; end;
  function TContactSpreadParams.getRelMovementControlName(): string; begin result := _relMovementControlName; end;


  function TContactSpreadParams.getPdfDistance(): TPdf;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _pdfDistanceName ) ) then
            begin
              if( fnDictionary.value( _pdfDistanceName ).fn is TPdf ) then
                result := fnDictionary.value( _pdfDistanceName ).fn as TPdf
              else
                begin
                  setPdfDistanceName( '' );
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


  function TContactSpreadParams.getPdfDelay(): TPdf;
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


  function TContactSpreadParams.getRelMovementControl(): TRelFunction;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _relMovementControlName ) ) then
            begin
              if( fnDictionary.value( _relMovementControlName ).fn is TRelFunction ) then
                result := fnDictionary.value( _relMovementControlName ).fn as TRelFunction
              else
                begin
                  setRelMovementControlName( '' );
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
  function TContactSpreadParams.hasChartName( const chartName: string; const whichChart: TSMChart ): boolean;
    begin
      result := false;

      if( CMDirect = contactType ) then
        begin
          case whichChart of
            CMDistanceDirect: result := ( chartName = self.pdfDistanceName );
            CMMovementControlDirect: result := ( chartName = self.relMovementControlName );
            CMDelayDirect: result := ( chartName = self.pdfDelayName );
          end;
        end
      else if( CMIndirect = contactType ) then
        begin
          case whichChart of
            CMDistanceIndirect: result := ( chartName = self.pdfDistanceName );
            CMMovementControlIndirect: result := ( chartName = self.relMovementControlName );
            CMDelayIndirect: result := ( chartName = self.pdfDelayName );
          end;
        end
      else
        //raise exception.create( 'Unrecognized contact type in TContactSpreadParams.hasChartName()' )
      ;
    end
  ;


  procedure TContactSpreadParams.removeChart( const chartName: string );
    begin
      if( chartName = self.pdfDistanceName ) then self.pdfDistanceName := '';
      if( chartName = self.relMovementControlName ) then self.relMovementControlName := '';
      if( chartName = self.pdfDelayName ) then self.pdfDelayName := '';
    end
  ;


  function TContactSpreadParams.chart( const whichChart: TSMChart; addlInfo: integer = -1 ): TChartFunction;
    begin
      result := nil;

      if( CMDirect = contactType ) then
        begin
          case whichChart of
            CMDistanceDirect:
              begin
                if ( self.fnDictionary.contains( self.pdfDistanceName ) ) then
                  result := self.fnDictionary.value( self.pdfDistanceName ).fn;
              end
            ;
            CMMovementControlDirect:
              begin
                if ( self.fnDictionary.contains( self.relMovementControlName ) ) then
                  result := self.fnDictionary.value( self.relMovementControlName ).fn;
              end
            ;
            CMDelayDirect:
              begin
                if ( self.fnDictionary.contains( self.pdfDelayName ) ) then
                  result := self.fnDictionary.value( self.pdfDelayName ).fn;
              end
            ;
          end;
        end
      else if( CMIndirect = contactType ) then
        begin
          case whichChart of
            CMDistanceIndirect:
              begin
                if ( self.fnDictionary.contains( self.pdfDistanceName ) ) then
                  result := self.fnDictionary.value( self.pdfDistanceName ).fn;
              end
            ;
            CMMovementControlIndirect:
              begin
                if ( self.fnDictionary.contains( self.relMovementControlName ) ) then
                  result := self.fnDictionary.value( self.relMovementControlName ).fn;
              end
            ;
            CMDelayIndirect:
              begin
                if ( self.fnDictionary.contains( self.pdfDelayName ) ) then
                  result := self.fnDictionary.value( self.pdfDelayName ).fn;
              end
            ;
          end;
        end
      else
        //raise exception.create( 'Unrecognized contact type in TContactSpreadParams.chart()' )
      ;
    end
  ;


  procedure TContactSpreadParams.changeChart(
        const whichChart: TSMChart;
        const oldChartName: string;
        newChart: TChartFunction;
        addlInfo: integer = -1
      );
    var
      newName: string;
    begin
      if( nil = newChart ) then
        newName := ''
      else
        newName := newChart.name
      ;

      case whichChart of
        CMDistanceDirect, CMDistanceIndirect:
          begin
            if( oldChartName = self.pdfDistanceName ) then
              self.pdfDistanceName := newName
            ;
          end
        ;
        CMMovementControlDirect, CMMovementControlIndirect:
          begin
            if( oldChartName = self.relMovementControlName ) then
              self.relMovementControlName := newName
            ;
          end
        ;
        CMDelayDirect, CMDelayIndirect:
          begin
            if( oldChartName = self.pdfDelayName ) then
              self.pdfDelayName := newName
            ;
          end
        ;
      end;
    end
  ;


  procedure TContactSpreadParams.setChart( const whichChart: TSMChart; fn: TChartFunction; addlInfo: integer = -1 );
    var
      newName: string;
    begin
      if( nil = fn ) then
        newName := ''
      else
        newName := fn.name
      ;

      if( CMDirect = contactType ) then
        begin
          case whichChart of
            CMDistanceDirect: self.pdfDistanceName := newName;
            CMMovementControlDirect: self.relMovementControlName := newName;
            CMDelayDirect: self.pdfDelayName := newName;
          end;
        end
      else if( CMIndirect = contactType ) then
        begin
          case whichChart of
            CMDistanceIndirect: self.pdfDistanceName := newName;
            CMMovementControlIndirect: self.relMovementControlName := newName;
            CMDelayIndirect: self.pdfDelayName := newName;
          end;
        end
      else
        //raise exception.create( 'Unrecognized contact type in TContactSpreadParams.setChart()' )
      ;
    end
  ;


  function TContactSpreadParams.functionsAreValid(): boolean;
    begin
      result := true;

      if( fnDictionary.contains( _pdfDistanceName ) ) then
        begin
          if( not( fnDictionary.value( _pdfDistanceName ).fn is TPdf ) ) then
            begin
              setPdfDistanceName( '' );
              result := false;
            end
          ;
        end
      ;

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

      if( fnDictionary.contains( _relMovementControlName ) ) then
        begin
          if( not( fnDictionary.value( _relMovementControlName ).fn is TRelFunction ) ) then
            begin
              setRelMovementControlName( '' );
              result := false;
            end
          ;
        end
      ;
    end
  ;
//------------------------------------------------------------------------------



  function TContactSpreadParams.getLatentCanInfect(): boolean;
    begin
      if( CMIndirect = _contactType ) then
          result := false
      else
       result := _latentCanInfect
      ;
    end
  ;


  function TContactSpreadParams.getSubClinicalCanInfect(): boolean; begin Result := _subClinicalCanInfect; end;

  function TContactSpreadParams.getUsePrevalence(): boolean;
    begin
      result := ( ( CMDirect = _contactType ) and ( (_sim as TSMSimulationInput).useWithinHerdPrevalence ) );
    end
  ;

  //------------------------------------------------------------------------------
  // These may not be needed, now that I have a class for production type pair.
  //------------------------------------------------------------------------------
  procedure TContactSpreadParams.setFromProdType( val: string );
    begin
      _fromProdType := val;
      _updated := true;
    end
  ;


  procedure TContactSpreadParams.setToProdType( val: string );
    begin
      _toProdType := val;
      _updated := true;
    end
  ;


  procedure TContactSpreadParams.setFromProdTypeID( val: integer );
    begin
      _fromProdTypeID := val;
      _updated := true;
    end
  ;


  procedure TContactSpreadParams.setToProdTypeID( val: integer );
    begin
      _toProdTypeID := val;
      _updated := true;
    end
  ;


  function TContactSpreadParams.getFromProdType(): string;
    var
      sim: TSMSimulationInput;
    begin
      if( length( _fromProdType ) = 0 ) then
        begin
          if( _sim = nil ) then
              raise Exception.Create( 'TContactSpreadParams sim is not set' )
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



  function TContactSpreadParams.getToProdType(): string;
    var
      sim: TSMSimulationInput;
    begin
      if( length( _toProdType ) = 0 ) then
        begin
          if( _sim = nil ) then
              raise Exception.Create( 'TContactSpreadParams sim is not set' )
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


  
  function TContactSpreadParams.getFromProdTypeID(): integer;
    var
      val: integer;
      sim: TSMSimulationInput;
    begin
      if( PRODTYPEUNASSIGNED <> _fromProdTypeID ) then
        val := _fromProdTypeID
      else
        begin
          if( _sim = nil ) then
            begin
              raise Exception.Create( 'TContactSpreadParams sim is not set' );
              val := PRODTYPEUNASSIGNED;
            end
          else
            begin
              sim := _sim as TSMSimulationInput;
              val := sim.getProdTypeID( _fromProdType );
              fromProdTypeID := val;
            end
          ;
        end
      ;

      if( PRODTYPEIDNOTFOUND = val ) then
        raise Exception.Create( 'Cannot find ptID for "' + _fromProdType +  '" in TContactSpreadParams' )
      ;

      Result := val;
    end
  ;

  

  function TContactSpreadParams.getToProdTypeID(): integer;
    var
      val: integer;
      sim: TSMSimulationInput;
    begin
      if( PRODTYPEUNASSIGNED <> _toProdTypeID ) then
        val := _toProdTypeID
      else
        begin
          if( _sim = nil ) then
            begin
              raise Exception.Create( 'TContactSpreadParams sim is not set' );
              val := PRODTYPEUNASSIGNED;
            end
          else
            begin
              sim := _sim as TSMSimulationInput;
              val := sim.getProdTypeID( _toProdType );
              toProdTypeID := val;
            end
          ;
        end
      ;

      if( PRODTYPEIDNOTFOUND = val ) then
        raise Exception.Create( 'Cannot find ptID for "' + _toProdType +  '" in TContactSpreadParams' )
      ;

      Result := val;
    end
  ;
  //------------------------------------------------------------------------------


  function TContactSpreadParams.getUpdated(): boolean;
    begin
      result := _updated;
    end
  ;
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// XML import
//------------------------------------------------------------------------------
  class function TContactSpreadParams.createXmlModelList(): TQStringList;
    begin
      result := TQStringList.create();
      result.Append( 'contact-spread-model' );
    end
  ;


  function TContactSpreadParams.getXmlModelList(): TQStringList;
    begin
      if( nil = _xmlModelList ) then
        _xmlModelList := createXmlModelList()
      ;

      result := _xmlModelList;
    end
  ;


  procedure TContactSpreadParams.importXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
    var
      subElement: pointer;
      pdf: TPdf;
      rel: TRelFunction;
      val, val2: double;
    begin
      // Get the distance distribution.
      //-------------------------------
      subElement := Sdew.GetElementByName( model, 'distance' );
      if ( nil = subElement ) then
        appendToPString( errMsg, tr( 'Contact spread model XML does not include a valid distance distribution.' ) )
      else
        begin
          pdf := createPdfFromXml( subElement, Sdew );
          if( nil = pdf ) then
            appendToPString( errMsg, tr( 'Contact spread model XML does not include a valid distance distribution.' ) )
          else
            begin
              if ( strIsEmpty( pdf.name ) ) then
                pdf.name := 'Dist ' + contactTypeToString( self.contactType ) + ' movement'
              ;

              if( CMDirect = self.contactType ) then
                pdf.dbField := word( CMDistanceDirect )
              else
                pdf.dbField := word( CMDistanceIndirect )
              ;
              self.pdfDistanceName := fnDictionary.checkAndInsert( pdf );
            end
          ;
        end
      ;

      // Get the delay distribution.
      //----------------------------
      subElement := Sdew.GetElementByName( model, 'delay' );
      if ( nil = subElement ) then
        appendToPString( errMsg, tr( 'Contact spread model XML does not include a valid delay distribution.' ) )
      else
        begin
          pdf := createPdfFromXml( subElement, Sdew );
          if( nil = pdf ) then
            appendToPString( errMsg, tr( 'Contact spread model XML does not include a valid delay distribution.' ) )
          else
            begin
              if ( strIsEmpty( pdf.name ) ) then
                pdf.name := 'Delay ' + contactTypeToString( self.contactType ) + ' shipping';

              if ( CMDirect = self.contactType ) then
                pdf.dbField := word( CMDelayDirect )
              else
                pdf.dbField := word( CMDelayIndirect )
              ;

              self.pdfDelayName := fnDictionary.checkAndInsert( pdf );
            end
          ;
        end
      ;

      // Probability of disease transfer
      //--------------------------------
      subElement := Sdew.GetElementByName( model, 'prob-infect' );
      if ( nil = subElement ) then
        begin
          // This is not necessarily an error: if within-herd prevalence is used,
          // there will be no probability of infection transfer for direct contact.

          if( CMIndirect = self.contactType ) then
            appendToPString( errMsg, tr( 'Contact spread model XML does not include a valid probability of infection transfer.' ) )
          ;
        end
      else
        begin
          val := usStrToFloat( Sdew.GetElementContents( subElement ), NaN );
          if( not isProbability( val ) ) then
            appendToPString( errMsg, tr( 'Contact spread model XML does not include a valid probability of infection transfer.' ) )
          else
            self.probInfect := val
          ;
        end
      ;

      // Contact rate
      //-------------
      val := NaN;
      val2 := NaN;

      subElement := Sdew.GetElementByName( model, 'movement-rate' );
      if ( nil <> subElement ) then
        begin
          subElement := Sdew.GetElementByName( subElement, 'value' );
          if ( nil <> subElement ) then
            begin
              val := usStrToFloat( Sdew.GetElementContents( subElement ), NaN );
              if( not isNaN( val ) ) then
                begin
                  self.meanContactRate := val;
                  self.useFixedContactRate := false;
                end
              ;
            end
          ;
        end
      ;

      subElement := Sdew.GetElementByName( model, 'fixed-movement-rate' );
      if ( nil <> subElement ) then
        begin
          subElement := Sdew.GetElementByName( subElement, 'value' );
          if ( nil <> subElement ) then
            begin
              val2 := usStrToFloat( sdew.getElementContents( subElement ), NaN );
              if( not isNaN( val2 ) ) then
                begin
                  self.fixedContactRate := val2;
                  self.useFixedContactRate := true;
                end
              ;
            end
          ;
        end
      ;

      if( isNaN( val ) and isNaN( val2 ) ) then
        appendToPString( errMsg, tr( 'Contact spread model XML does not include a valid contact rate.' ) )
      ;
      if( ( not isNaN( val ) ) and ( not isNaN( val2 ) ) ) then
        appendToPString( errMsg, tr( 'Contact spread model XML includes ambiguous contact rates.' ) )
      ;


      // Movement control function
      //--------------------------
      subElement := Sdew.GetElementByName( model, 'movement-control' );
      if ( nil = subElement ) then
        appendToPString( errMsg, tr( 'Contact spread model XML does not include a valid movement control function.' ) )
      else
        begin
          rel := createRelFromXml( subElement, sdew );

          if( nil = rel ) then
            appendToPString( errMsg, tr( 'Contact spread model XML does not include a valid movement control function.' ) )
          else
            begin
              if ( strIsEmpty( rel.name ) ) then
                rel.name := 'Movement control effect ' + contactTypeToString( self.contactType );

              if ( CMDirect = self.contactType ) then
                rel.dbField := word( CMMovementControlDirect )
              else
                rel.dbField := word( CMMovementControlIndirect )
              ;
              self.relMovementControlName := fnDictionary.checkAndInsert( rel );
            end
          ;
        end
      ;

      // Miscellaneous parameters
      //-------------------------
      subElement := Sdew.GetElementByName( model, 'latent-units-can-infect' );
      if ( nil <> subElement ) then
        self.latentCanInfect :=  usTextToBool( Sdew.GetElementContents( subElement ) )
      ;

      subElement := Sdew.GetElementByName( model, 'subclinical-units-can-infect' );
      if ( nil <> subElement ) then
        self.subClinicalCanInfect :=  usTextToBool( Sdew.GetElementContents( subElement ) );
      ;
    end
  ;
//------------------------------------------------------------------------------

end.
