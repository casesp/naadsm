unit VaccinationParams;

(*
Vaccination.pas
----------------
Begin: 2005/01/06
Last revision: $Date: 2010-06-03 19:12:29 $ $Author: areeves $
Version number: $Revision: 1.53.4.2 $
Project: NAADSM and related applications
Website:
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2005 - 2010 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
    QLists,
    
    Sdew,

    ChartFunction,
    ProbDensityFunctions,
    RelFunction,
    SMDatabase,
    Models,
    FunctionEnums
  ;

  type TVaccinationParams = class( TModelWithFunctions )
    protected
      _xmlModelList: TQStringList;

    	// Properties
      _prodTypeDescr: string;
      _useVaccination: boolean;
      _immunityDelay: integer;

      _pdfVaccImmuneName: string;

      // For internal use
      procedure initialize();

      // XML import
      function getXmlModelList(): TQStringList;

      // Properties
      procedure setProdTypeDescr( val: string );
      procedure setImmunityDelay( val: integer );
      procedure setUseVaccination( val: boolean );

      function getProdTypeDescr(): string;
      function getImmunityDelay(): integer;
      function getUseVaccination(): boolean;

      procedure setPdfVaccImmuneName( val: string );
      function getPdfVaccImmuneName(): string;

      function getPdfVaccImmune(): TPdf;


      // Overridden from TModel
      //-----------------------
      function getUpdated(): boolean; override;

    public
    	// Construction/initialization/destruction
      constructor create( sim: TObject; prodTypeName: string ); overload;
      constructor create( src: TVaccinationParams; sim: TObject ); overload;
      constructor create( db: TSMDatabase; ptID: integer; prodTypeName: string; sim: TObject ); overload;
      destructor destroy(); override;

      // Overridden from TModel
      //-----------------------
      function validate( err: PString = nil ): boolean; override;
      procedure debug(); override;
      function ssXml(  const productionTypeID: integer ): string; reintroduce;
      function populateDatabase( db: TSMDatabase; ptID: integer ): integer; reintroduce;

      // Overridden from TModelWithFunctions
      //------------------------------------
      procedure setChart( const whichChart: TSMChart; fn: TChartFunction; addlInfo: integer = -1 ); override;
      function chart( const whichChart: TSMChart; addlInfo: integer = -1 ): TChartFunction; override;
      procedure removeChart( const chartName: string ); override;
      function hasChartName( const chartName: string; const whichChart: TSMChart ): boolean; override;
      function functionsAreValid(): boolean; override;

      // XML import
      //-----------
      class function createXmlModelList(): TQStringList;
      procedure importXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
      property xmlModelList: TQStringList read getXmlModelList;

      // Properties
      //-----------
      property prodTypeDescr: string read getProdTypeDescr write setProdTypeDescr;
      property useVaccination: boolean read getUseVaccination write setUseVaccination;
      property daysToImmunity: integer read getImmunityDelay write setImmunityDelay;

      property pdfVaccImmuneName: string read getpdfVaccImmuneName write setPdfVaccImmuneName;
      property pdfVaccImmune: TPdf read getPdfVaccImmune;
    end
  ;


implementation

  uses
    SysUtils,
    StrUtils,
    Variants,

    MyStrUtils,
    DebugWindow,
    SqlClasses,
    I88n,

    FunctionDictionary,
    SMSimulationInput
  ;


  const
    DBVACCINATIONPARAMS: boolean = false; // set to true to enable debugging messages for this unit.

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TVaccinationParams.create( sim: TObject; prodTypeName: string );
  	begin
    	inherited create();
      initialize();

      _prodTypeDescr := prodTypeName;
      _sim := sim;
    end
  ;


  constructor TVaccinationParams.create( src: TVaccinationParams; sim: TObject );
    begin
      inherited create( src );

      _sim := sim;

      _prodTypeDescr := src._prodTypeDescr;
      _useVaccination := src._useVaccination;
      _immunityDelay := src._immunityDelay;

      setPdfVaccImmuneName( src._pdfVaccImmuneName );

      _updated := src._updated;
    end
  ;


  constructor TVaccinationParams.create( db: TSMDatabase; ptID: integer; prodTypeName: string; sim: TObject );
    var
      q: string;
      db2: TSqlDatabase;
      res: TSqlResult;
      row: TSqlRow;
    begin
    	inherited create();
      initialize();

      _prodTypeDescr := prodTypeName;
      _sim := sim;

      db2 := db as TSqlDatabase;

      q := 'SELECT'
        + ' inProductionType.useVaccination,'
        + ' inProductionType.vaccDaysToImmunity,'
        + ' inProductionType.vaccImmunePeriodPdfID,'
        + ' vaccImmuneChart.chartName AS vaccImmuneChartName'
        + ' FROM inProductionType'
        + ' LEFT OUTER JOIN inChart vaccImmuneChart'
        + ' ON vaccImmuneChart.chartID = inProductionType.vaccImmunePeriodPdfID'
        + ' WHERE productionTypeID = ' + intToStr( ptID )
      ;

      res := TSqlResult.create( q, db2 );

      row := res.fetchArrayFirst();

      if( null <> row.field('useVaccination') ) then useVaccination := row.field('useVaccination');
      if( null <> row.field('vaccDaysToImmunity') ) then daysToImmunity := row.field('vaccDaysToImmunity');

      if( null <> row.field('vaccImmunePeriodPdfID') ) then
        setPdfVaccImmuneName( row.field( 'vaccImmuneChartName' ) )
      ;

      _updated := false;

      freeAndNil( res );
    end
  ;


  procedure TVaccinationParams.initialize();
  	begin
      _xmlModelList := nil;

      useVaccination := false;
      daysToImmunity := 0;
      prodTypeDescr := '';
      setPdfVaccImmuneName( '' );
    end
  ;

  destructor TVaccinationParams.destroy();
  	begin
      freeAndNil( _xmlModelList );

      // The function dictionary is freed elsewhere.
      // Functions are handled by the function dictionary:
      // don't free them here, but do decrement their counters.
      setPdfVaccImmuneName( '' );

    	inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Database population
//-----------------------------------------------------------------------------
	function TVaccinationParams.populateDatabase( db: TSMDatabase; ptID: integer ): integer;
  	var
    	q: string;
      dict: TQueryDictionary;
  	begin
      dict := TQueryDictionary.create();

      dict['useVaccination'] := usBoolToText( useVaccination );
      dict['vaccDaysToImmunity'] := intToStr( daysToImmunity );
      
      if( nil <> pdfVaccImmune ) then
      	dict['vaccImmunePeriodPdfID'] := intToStr( pdfVaccImmune.id )
      else
     		dict['vaccImmunePeriodPdfID'] := 'NULL'
      ;

      q := writeQuery(
      	'inProductionType',
        QUpdate,
        dict,
        'WHERE `productionTypeID` = ' + intToStr( ptID )
      );

      dbcout( 'Updating vacc params in db:', DBVACCINATIONPARAMS );
      dbcout( q, DBVACCINATIONPARAMS );

      result := integer( db.execute( q ) );

      dict.Clear();
      dict.Free();

      _updated := false;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// XML export
//-----------------------------------------------------------------------------
	function TVaccinationParams.ssXml(  const productionTypeID: integer ): string;
  	begin
      result := '';

      result := result + '  <vaccine-model production-type="' + encodeXml( prodTypeDescr ) + '" production-type-id="' + intToStr( productionTypeID ) + '">' + endl;

      result := result + '    <delay>' + endl;
      result := result + '      <value>' + intToStr( daysToImmunity ) + '</value>' + endl;
      result := result + '      <units><xdf:unit>day</xdf:unit></units>' + endl;
      result := result + '    </delay>' + endl;

      result := result + '    <immunity-period>' + endl;
      result := result + pdfVaccImmune.ssXml( 3 );
      result := result + '    </immunity-period>' + endl;

      result := result + '  </vaccine-model>' + endl;

      result := result + endl;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Data validation
//-----------------------------------------------------------------------------
  function TVaccinationParams.validate( err: PString = nil ): boolean;
  	var
    	msg: string;
    	submsg: string;
  	begin
    	result := true;
			msg := '';

      submsg := '';

      if( useVaccination ) then
      	begin
          if( 0 > daysToImmunity ) then
            begin
              if( nil <> err ) then
              	msg := msg + '  ' + tr( 'Days to immunity must be equal to or greater than 0.' ) + endl
              ;
              result := false;
            end
          ;

          if( nil = pdfVaccImmune ) then
          	begin
            	if( nil <> err ) then msg := msg + '  ' + tr( 'Vaccine immune period is not set.' ) + endl;
              result := false;
            end
          else if( not( pdfVaccImmune.validate( @submsg ) ) ) then
          	begin
            	if( nil <> err ) then msg := msg + '  ' + tr( 'Vaccine immune period is not valid:' ) + submsg + endl;
              result := false;
            end
          ;
        end
      ;

      if( ( result = false ) and ( nil <> err ) ) then
      	begin
					msg := endl + ansiReplaceStr( tr( 'Ring vaccination parameters for xyz:' ), 'xyz', prodTypeDescr ) + endl + msg;
          err^ := err^ + msg;
        end
      ;
		end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Debugging
//-----------------------------------------------------------------------------
	procedure TVaccinationParams.debug();
  	begin
    	dbcout( '-------------BEGIN VACCINATION PARAMS', true );
      dbcout( 'Vaccination: delay=' + intToStr( daysToImmunity ), true );

      if( nil <> pdfVaccImmune ) then
      	begin
          dbcout( 'Vaccine immune period:', true );
          pdfVaccImmune.debug();
        end
      else
      	dbcout( 'VACCINE IMMUNE PERIOD IS UNDEFINED', true )
      ;

      dbcout( '-------------END VACCINATION PARAMS', true );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
  function TVaccinationParams.getUpdated(): boolean;
    begin
      result :=
        _updated
      or
        fnDictionary.functionExistsAndIsUpdated( _pdfVaccImmuneName )
      ;
    end
  ;


  function TVaccinationParams.getProdTypeDescr(): string;
    begin
      if( 0 = length( _prodTypeDescr ) ) then
        raise exception.Create( 'TVaccinationParams._prodTypeDescr is not set' )
      ;
      result := _prodTypeDescr;
    end
  ;

  function TVaccinationParams.getImmunityDelay(): integer; begin Result := _immunityDelay; end;

  function TVaccinationParams.getUseVaccination(): boolean; begin result :=_useVaccination; end;

  procedure TVaccinationParams.setUseVaccination( val: boolean ); begin _useVaccination := val; _updated := true; end;
  procedure TVaccinationParams.setProdTypeDescr( val: string ); begin _prodTypeDescr := val; end;
  procedure TVaccinationParams.setImmunityDelay( val: integer ); begin _immunityDelay := val; end;

  procedure TVaccinationParams.setPdfVaccImmuneName( val: string );
    begin
      val := trim( val );

      // Decrement the reference counter for the old function...
      decrFnRefCounter( _pdfVaccImmuneName );
      // ...and increment the reference counter for the new function.
      incrFnRefCounter( val );

      _pdfVaccImmuneName := val;
      _updated := true;
    end
  ;


  function TVaccinationParams.getPdfVaccImmuneName(): string; begin result := _pdfVaccImmuneName; end;


  function TVaccinationParams.getPdfVaccImmune(): TPdf;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _pdfVaccImmuneName ) ) then
            begin
              if( fnDictionary.value( _pdfVaccImmuneName ).fn is TPdf ) then
                result := fnDictionary.value( _pdfVaccImmuneName ).fn as TPdf
              else
                begin
                  setPdfVaccImmuneName( '' );
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
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Overridden from TModelWithFunctions
//-----------------------------------------------------------------------------
  procedure TVaccinationParams.setChart( const whichChart: TSMChart; fn: TChartFunction; addlInfo: integer = -1 );
    var
      newName: string;
    begin
      if( nil = fn ) then
        newName := ''
      else
        newName := fn.name
      ;

      case whichChart of
        VacImmunePeriod: self.pdfVaccImmuneName := newName;
      end;
    end
  ;


  function TVaccinationParams.chart( const whichChart: TSMChart; addlInfo: integer = -1 ): TChartFunction;
    begin
      result := nil;

      if ( self.fnDictionary <> nil ) then
        begin
          case whichChart of
            VacImmunePeriod:
              if ( self.fnDictionary.contains( self.pdfVaccImmuneName ) ) then
                result := self.fnDictionary.value( self.pdfVaccImmuneName ).fn
              ;
          end;
        end;
    end
  ;


  procedure TVaccinationParams.removeChart( const chartName: string );
    begin
      if( chartName = self.pdfVaccImmuneName ) then self.pdfVaccImmuneName := '';
    end
  ;


  function TVaccinationParams.hasChartName( const chartName: string; const whichChart: TSMChart ): boolean;
    begin
      result := false;
      
      case whichChart of
        VacImmunePeriod: result := ( chartName = self.pdfVaccImmuneName );
      end;
    end
  ;


  function TVaccinationParams.functionsAreValid(): boolean;
    begin
      result := true;

      if( fnDictionary.contains( _pdfVaccImmuneName ) ) then
        begin
          if( not( fnDictionary.value( _pdfVaccImmuneName ).fn is TPdf ) ) then
            begin
              setPdfVaccImmuneName( '' );
              result := false;
            end
          ;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// XML import
//-----------------------------------------------------------------------------
  class function TVaccinationParams.createXmlModelList(): TQStringList;
    begin
      result := TQStringList.create();
      result.Append( 'vaccine-model' );
    end
  ;


  function TVaccinationParams.getXmlModelList(): TQStringList;
    begin
      if( nil = _xmlModelList ) then
        _xmlModelList := createXmlModelList()
      ;

      result := _xmlModelList;
    end
  ;


  procedure TVaccinationParams.importXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
    var
      e: pointer;
      fn: TPdf;
      delay: integer;
    begin
      if( 'vaccine-model' <> sdew.GetElementName( model ) ) then
        raise exception.Create( 'There is a problem in TVaccinationParams.importXml' )
      ;

      // NOTE:
      //   Since the units used in the vaccine-model remain constant, they
      //   are not read in here.  If this changes in the future, remember to
      //   add the code here to read them in.
      
      delay := -1; // This invalid default value may be changed below

      if( nil <> sdew.GetElementByName( model, 'delay') ) then
        begin
          if ( nil <> sdew.GetElementByName( Sdew.GetElementByName( model, 'delay'), 'value' ) ) then
            delay := myStrToInt( Sdew.GetElementContents( Sdew.GetElementByName( Sdew.GetElementByName( model, 'delay'), 'value')), -1 )
          ;
          self.daysToImmunity := delay;
          self.useVaccination := true;
        end
      ;

      e := sdew.GetElementByName( model, 'immunity-period' );
      if( nil <> e ) then
        begin
         fn := createPdfFromXml( e, sdew );

          if( strIsEmpty( fn.name ) ) then
            fn.name := 'Vaccine immune period' + ' - ' + self.prodTypeDescr
          ;
          fn.dbField := word( VacImmunePeriod );
          self.pdfVaccImmuneName := fnDictionary.checkAndInsert( fn );
          self.useVaccination := true;
        end
      else
        appendToPstring( errMsg, tr( 'Vaccine XML does not contain a function for the duration of immunity.' ) )
      ;
    end
  ;
//-----------------------------------------------------------------------------


end.
