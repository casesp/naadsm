unit ContactModel;

(*
ContactModel.pas
-----------------
Begin: 2005/01/06
Last revision: $Date: 2008/11/25 22:00:58 $ $Author: areeves $
Version number: $Revision: 1.49 $
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
    SysUtils,
    Dialogs,
    ProbDensityFunctions,
    RelFunction,
    ChartFunction,
    SMDatabase,
    SqlClasses,
    Models,
    FunctionEnums
  ;

  type TContactType = ( CMUnknown, CMDirect, CMIndirect );

  function contactTypeToString( val: TContactType ): string;
  
  function stringToContactType( str: string ): TContactType;

  type TContactModel = class( TModelWithFunctions )
    protected
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

      _distanceName: string;
      _delayName: string;
      _movementControlName: string;

      procedure setID( val: integer );
      function getID(): integer;

      procedure setContactType( val: TContactType );
      function getContactType(): TContactType;

      procedure setdistanceName( val: string );
      procedure setdelayName( val: string );
      procedure setmovementControlName( val: string );

      function getdistanceName(): string;
      function getdelayName(): string;
      function getmovementControlName(): string;

      function getDistance(): TPdf;
      function getDelay(): TPdf;
      function getMovementControl(): TRelFunction;

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

      constructor create( const src: TContactModel; sim: TObject ); overload;

      destructor destroy(); override;

      // Overridden from TModel
      //-----------------------
      function validate( err: PString = nil ): boolean; override;
      procedure debug(); override;
      function ssXml(): string; override;
      function populateDatabase( db: TSMDatabase; fromPTP: boolean = false; action: TQueryType = QInsert ): integer; reintroduce;

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

      property distanceName: string read getdistanceName write setdistanceName;
      property delayName: string read getdelayName write setdelayName;
      property movementControlName: string read getmovementControlName write setmovementControlName;

      property pdfDistance: TPdf read getDistance;
      property pdfDelay: TPdf read getDelay;
      property relMovementControl: TRelFunction read getMovementControl;

      property isValid: boolean read getIsValid;

      property sim: TObject write setSim;

      property updated: boolean read getUpdated;
    end
  ;

  const
  	DBCONTACTMODEL: boolean = false; // set to true to enable debugging messages in this unit

implementation

	uses
    StrUtils,
  	Variants,

    MyStrUtils,
    USStrUtils,
    DebugWindow,
    I88n,

    SMSimulationInput,
    ProductionType,
    FunctionDictionary
  ;

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
  constructor TContactModel.create();
    begin
      inherited create();
      initialize();
    end
  ;


  constructor TContactModel.create( contactType: TContactType; sim: TObject; toProdTypeID, fromProdTypeID: integer );
    begin
      inherited create();
      initialize();
      setContactType( contactType );
      _sim := sim;
      _toProdTypeID := toProdTypeID;
      _fromProdTypeID := fromProdTypeID;
    end
  ;



  procedure TContactModel.initialize();
  	begin
    	_sim := nil;
    	fromProdTypeID := -1;
      toProdTypeID := -1;
      id := -1;

     	setContactType( CMUnknown );
      setMeanContactRate( -1.0 );
      setUseFixedContactRate( false );
      setFixedContactRate( -1 );
      setProbInfect( -1.0 );

      _updated := false;
    end
  ;

	constructor TContactModel.create(
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
                  setdistanceName( row.field( 'distanceChartName' ) );
                end
              ;

              if( null <> row.field( 'transportDelayPdfID' ) ) then
                begin
                  setdelayName( row.field( 'delayChartName' ) );
                end
              ;

              if( null <> row.field( 'movementControlRelID' ) ) then
                begin
                  setmovementControlName( row.field( 'movementControlChartName' ) );
                end
              ;

            end
          else if( res.numRows = 0 ) then
            // do nothing
          else
            raise exception.Create( 'Too many matching rows in TContactModel.create' )
          ;
        end
      ;

      _updated := false;
    end
  ;


  constructor TContactModel.create( const src: TContactModel; sim: TObject );
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

      _distanceName := src._distanceName;
      _delayName := src._delayName;
      _movementControlName := src._movementControlName;

      _updated := src._updated;
    end
  ;


  destructor TContactModel.destroy();
    begin
      // The function dictionary is freed elsewhere.
      // Disease periods are handled by the function dictionary:
      // don't free them here, but do decrement their counters.
      if( fnDictionary.contains( distanceName ) ) then
        fnDictionary.value( distanceName ).decrRefCounter()
      ;

      if( fnDictionary.contains( delayName ) ) then
        fnDictionary.value( delayName ).decrRefCounter()
      ;

      if( fnDictionary.contains( movementControlName ) ) then
        fnDictionary.value( movementControlName ).decrRefCounter()
      ;
      
      inherited destroy();
    end
  ;
//------------------------------------------------------------



//------------------------------------------------------------
// Database functions
//------------------------------------------------------------
  function TContactModel.populateDatabase( db: TSMDatabase; fromPTP: boolean = false; action: TQueryType = QInsert ): integer;
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

      dict['latentCanInfect'] := boolToStr( latentCanInfect );
      dict['subclinicalCanInfect'] := boolToStr( subClinicalCanInfect );

      dict['useFixedContactRate'] := boolToStr( useFixedContactRate );
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




  function TContactModel.validate( err: PString = nil ): boolean;
  	var
      msg: string;
    	submsg: string;
  	begin
      // Contact models must have all of these defined:
      //	- relMovementControl
      //	- pdfDelay
      // 	- pdfDistance
      // 	- probInfect >= 0
      //	- meanContactRate >= 0

      dbcout( '%%%%% Validating contact model', DBCONTACTMODEL );
      dbcout( 'ToProdType: ' + toProdType, DBCONTACTMODEL );
      dbcout( 'FromProdType: ' + fromProdType, DBCONTACTMODEL );

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

      dbcout( 'Trying to check fixed contact rate', DBCONTACTMODEL );
      dbcout( 'UseFixedContactRate: ' + usBoolToText( useFixedContactRate ), DBCONTACTMODEL );
      dbcout( 'fixedContactRate: ' + usFloatToStr( fixedContactRate ), DBCONTACTMODEL );

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

      dbcout( 'Done validating', DBCONTACTMODEL );
    end
  ;



	function TContactModel.ssXml(): string;
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
            + chartUnitTypeAsSSXml( UnitsPerDay )
            + '    </fixed-movement-rate>' + endl;
          ;
        end
      else
        begin
          str := str + '    <movement-rate>' + endl
            + '      <value>' + usFloatToStr( meanContactRate ) + '</value>'
            + chartUnitTypeAsSSXml( UnitsPerDay ) + endl
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
  procedure TContactModel.debug();
    var
      msg: string;
    begin;
    	msg := 'BEGIN TCONTACTMODEL DEBUG' + endl;
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
      dbcout( 'END TCONTACTMODEL DEBUG' + endl, true );
    end
  ;


{ property getter/setter functions }

	procedure TContactModel.setID( val: integer );
    begin
      _id := val;
      _updated := true;
    end
  ;


  procedure TContactModel.setContactType( val: TContactType );
    begin
      _contactType := val;
      _updated := true;
    end
  ;


  procedure TContactModel.setMeanContactRate( val: double );
    begin
      _meanContactRate := val;
      _updated := true;
    end
  ;


  procedure TContactModel.setContactRateUnits( val: string );
    begin
      _contactRateUnits := val;
      _updated := true;
    end
  ;


  procedure TContactModel.setUseFixedContactRate( val: boolean );
    begin
      _useFixedContactRate := val;
      _updated := true;
    end
  ;


  procedure TContactModel.setFixedContactRate( val: double );
    begin
      _fixedContactRate := val;
      _updated := true;
    end
  ;


  procedure TContactModel.setProbInfect( val: double );
    begin
      _probInfect := val;
      _updated := true;
    end
  ;


  procedure TContactModel.setLatentCanInfect( val: boolean );
    begin
      _latentCanInfect := val;
      _updated := true;
    end
  ;


  procedure TContactModel.setSubClinicalCanInfect( val: boolean );
    begin
      _subClinicalCanInfect := val;
      _updated := true;
    end
  ;


  function TContactModel.getID(): integer; begin result := _id; end;
  function TContactModel.getContactType(): TContactType; begin Result := _contactType; end;
  function TContactModel.getMeanContactRate(): double; begin Result := _meanContactRate; end;
  function TContactModel.getContactRateUnits(): string; begin Result := _contactRateUnits; end;
  function TContactModel.getUseFixedContactRate(): boolean; begin result := _useFixedContactRate; end;
  function TContactModel.getFixedContactRate(): double; begin result := _fixedContactRate; end;
  function TContactModel.getProbInfect(): double; begin Result := _probInfect; end;

  procedure TContactModel.setdistanceName( val: string );
    begin
      val := trim( val );
      _distanceName := val;

      if( '' <> val ) then
        begin
          if( fnDictionary.contains( val ) ) then
            fnDictionary.value( val ).incrRefCounter()
          ;
        end
      ;
      _updated := true;
    end
  ;

  procedure TContactModel.setdelayName( val: string );
    begin
      val := trim( val );
      _delayName := val;

      if( '' <> val ) then
        begin
          if( fnDictionary.contains( val ) ) then
            fnDictionary.value( val ).incrRefCounter()
          ;
        end
      ;
      _updated := true;
    end
  ;


  procedure TContactModel.setmovementControlName( val: string );
    begin
      val := trim( val );
      _movementControlName := val;

      if( '' <> val ) then
        begin
          if( fnDictionary.contains( val ) ) then
            fnDictionary.value( val ).incrRefCounter()
          ;
        end
      ;
      _updated := true;
    end
  ;

  function TContactModel.getdistanceName(): string; begin result := _distanceName; end;
  function TContactModel.getdelayName(): string; begin result := _delayName; end;
  function TContactModel.getmovementControlName(): string; begin result := _movementControlName; end;


  function TContactModel.getDistance(): TPdf;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _distanceName ) ) then
            begin
              if( fnDictionary.value( _distanceName ).fn is TPdf ) then
                result := fnDictionary.value( _distanceName ).fn as TPdf
              else
                begin
                  setDistanceName( '' );
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


  function TContactModel.getDelay(): TPdf;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _delayName ) ) then
            begin
              if( fnDictionary.value( _delayName ).fn is TPdf ) then
                result := fnDictionary.value( _delayName ).fn as TPdf
              else
                begin
                  setDelayName( '' );
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


  function TContactModel.getMovementControl(): TRelFunction;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _movementControlName ) ) then
            begin
              if( fnDictionary.value( _movementControlName ).fn is TRelFunction ) then
                result := fnDictionary.value( _movementControlName ).fn as TRelFunction
              else
                begin
                  setMovementControlName( '' );
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


  function TContactModel.functionsAreValid(): boolean;
    begin
      result := true;

      if( fnDictionary.contains( _distanceName ) ) then
        begin
          if( not( fnDictionary.value( _distanceName ).fn is TPdf ) ) then
            begin
              setDistanceName( '' );
              result := false;
            end
          ;
        end
      ;

      if( fnDictionary.contains( _delayName ) ) then
        begin
          if( not( fnDictionary.value( _delayName ).fn is TPdf ) ) then
            begin
              setDelayName( '' );
              result := false;
            end
          ;
        end
      ;

      if( fnDictionary.contains( _movementControlName ) ) then
        begin
          if( not( fnDictionary.value( _movementControlName ).fn is TRelFunction ) ) then
            begin
              setMovementControlName( '' );
              result := false;
            end
          ;
        end
      ;
    end
  ;


  function TContactModel.getLatentCanInfect(): boolean;
    begin
      if( CMIndirect = _contactType ) then
          result := false
      else
       result := _latentCanInfect
      ;
    end
  ;


  function TContactModel.getSubClinicalCanInfect(): boolean; begin Result := _subClinicalCanInfect; end;

  function TContactModel.getUsePrevalence(): boolean;
    begin
      dbcout2( 'This is me' );
      dbcout2( 'sim is nil' + boolToStr( nil = _sim ) );
      result := ( ( CMDirect = _contactType ) and ( (_sim as TSMSimulationInput).useWithinHerdPrevalence ) );
    end
  ;

  //------------------------------------------------------------------------------
  // These may not be needed, now that I have a class for production type pair.
  //------------------------------------------------------------------------------
  procedure TContactModel.setFromProdType( val: string );
    begin
      _fromProdType := val;
      _updated := true;
    end
  ;


  procedure TContactModel.setToProdType( val: string );
    begin
      _toProdType := val;
      _updated := true;
    end
  ;


  procedure TContactModel.setFromProdTypeID( val: integer );
    begin
      _fromProdTypeID := val;
      _updated := true;
    end
  ;


  procedure TContactModel.setToProdTypeID( val: integer );
    begin
      _toProdTypeID := val;
      _updated := true;
    end
  ;


  function TContactModel.getFromProdType(): string;
  	var
    	sim: TSMSimulationInput;
  	begin
    	if( length( _fromProdType ) = 0 ) then
      	begin
        	if( _sim = nil ) then
            	raise Exception.Create( 'TContactModel sim is not set' )
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



  function TContactModel.getToProdType(): string;
  	var
    	sim: TSMSimulationInput;
  	begin
    	if( length( _toProdType ) = 0 ) then
      	begin
        	if( _sim = nil ) then
            	raise Exception.Create( 'TContactModel sim is not set' )
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


  
  function TContactModel.getFromProdTypeID(): integer;
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
            	raise Exception.Create( 'TContactModel sim is not set' );
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
        raise Exception.Create( 'Cannot find ptID for "' + _fromProdType +  '" in TContactModel' )
      ;

      Result := val;
    end
  ;

  

  function TContactModel.getToProdTypeID(): integer;
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
            	raise Exception.Create( 'TContactModel sim is not set' );
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
        raise Exception.Create( 'Cannot find ptID for "' + _toProdType +  '" in TContactModel' )
      ;

      Result := val;
    end
  ;
  //------------------------------------------------------------------------------


  function TContactModel.getUpdated(): boolean;
    begin
      result := _updated;
    end
  ;

end.
