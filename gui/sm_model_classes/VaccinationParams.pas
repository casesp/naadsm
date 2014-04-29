unit VaccinationParams;

(*
Vaccination.pas
----------------
Begin: 2005/01/06
Last revision: $Date: 2008/11/25 22:00:59 $ $Author: areeves $
Version number: $Revision: 1.45 $
Project: NAADSM and related applications
Website:
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2005 - 2008 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
    ChartFunction,
    ProbDensityFunctions,
    RelFunction,
    SMDatabase,
    Models,
    FunctionEnums
  ;

  type TVaccinationParams = class( TModelWithFunctions )
    protected
    	// Properties
      _prodTypeDescr: string;
      _useVaccination: boolean;
      _immunityDelay: integer;

      _vaccImmunePdfName: string;

      // For internal use
      procedure initialize();

      // Properties
      procedure setProdTypeDescr( val: string );
      procedure setImmunityDelay( val: integer );
      procedure setUseVaccination( val: boolean );

      function getProdTypeDescr(): string;
      function getImmunityDelay(): integer;
      function getUseVaccination(): boolean;

      procedure setVaccImmunePdfName( val: string );
      function getVaccImmunePdfName(): string;

      function getVaccImmunePdf(): TPdf;


      // Overridden from TModel
      //-----------------------
      function getUpdated(): boolean; override;

    public
    	// Construction/initialization/destruction
      constructor create( sim: TObject; prodTypeName: string ); overload;
      constructor create( src: TVaccinationParams; sim: TObject ); overload;
      destructor destroy(); override;

      procedure initializeFromDB( db: TSMDatabase; ptID: integer; ptDescr: string );

      // Overridden from TModel
      //-----------------------
      function validate( err: PString = nil ): boolean; override;
      procedure debug(); override;
      function ssXml(  const productionTypeID: integer ): string; reintroduce;
      function populateDatabase( db: TSMDatabase; ptID: integer ): integer; reintroduce;

      function functionsAreValid(): boolean; override;

      // Properties
      //-----------
      property prodTypeDescr: string read getProdTypeDescr write setProdTypeDescr;
      property useVaccination: boolean read getUseVaccination write setUseVaccination;
      property daysToImmunity: integer read getImmunityDelay write setImmunityDelay;

      property vaccImmunePdfName: string read getVaccImmunePdfName write setVaccImmunePdfName;
      property pdfVaccImmune: TPdf read getVaccImmunePdf;
    end
  ;


implementation

  uses
    SysUtils,
    StrUtils,
    Variants,

    MyStrUtils,
    USStrUtils,
    DebugWindow,
    SqlClasses,
    I88n,

    FunctionDictionary
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

      _vaccImmunePdfName := src._vaccImmunePdfName;

      _updated := src._updated;
    end
  ;


  procedure TVaccinationParams.initializeFromDB( db: TSMDatabase; ptID: integer; ptDescr: string );
    var
      q: string;
      db2: TSqlDatabase;
      res: TSqlResult;
      row: TSqlRow;
    begin
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
        begin
          setVaccImmunePdfName( row.field( 'vaccImmuneChartName' ) );
        end
      ;

      prodTypeDescr := ptDescr;

      _updated := false;

      freeAndNil( res );
    end
  ;


  procedure TVaccinationParams.initialize();
  	begin
      useVaccination := false;
      daysToImmunity := 0;
      prodTypeDescr := '';
      _vaccImmunePdfName := '';
    end
  ;

  destructor TVaccinationParams.destroy();
  	begin
      // The function dictionary is freed elsewhere.
      // Functions are handled by the function dictionary:
      // don't free them here, but do decrement their counters.
      if( nil <> fnDictionary ) then
        begin
          if( fnDictionary.contains( vaccImmunePdfName ) ) then
            fnDictionary.value( vaccImmunePdfName ).decrRefCounter()
          ;
        end
      ;

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

      dict['useVaccination'] := boolToStr( useVaccination );
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
      result := '' + endl;

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
  // FIX ME!
  function TVaccinationParams.getUpdated(): boolean; begin result := _updated; end;

  function TVaccinationParams.getProdTypeDescr(): string;
    begin
      if( 0 = length( _prodTypeDescr ) ) then
        raise exception.Create( 'TVaccinationParams._prodTypeDescr is not set' )
      ;
      result := _prodTypeDescr;
    end
  ;

  function TVaccinationParams.getImmunityDelay(): integer; begin Result := _immunityDelay; end;
  function TVaccinationParams.getUseVaccination(): boolean; begin result := _useVaccination; end;

  procedure TVaccinationParams.setUseVaccination( val: boolean ); begin _useVaccination := val; _updated := true; end;
  procedure TVaccinationParams.setProdTypeDescr( val: string ); begin _prodTypeDescr := val; end;
  procedure TVaccinationParams.setImmunityDelay( val: integer ); begin _immunityDelay := val; end;

  procedure TVaccinationParams.setVaccImmunePdfName( val: string );
    begin
      val := trim( val );
      _vaccImmunePdfName := val;

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


  function TVaccinationParams.getVaccImmunePdfName(): string; begin result := _vaccImmunePdfName; end;


  function TVaccinationParams.getVaccImmunePdf(): TPdf;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _vaccImmunePdfName ) ) then
            begin
              if( fnDictionary.value( _vaccImmunePdfName ).fn is TPdf ) then
                result := fnDictionary.value( _vaccImmunePdfName ).fn as TPdf
              else
                begin
                  setVaccImmunePdfName( '' );
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


  function TVaccinationParams.functionsAreValid(): boolean;
    begin
      result := true;

      if( fnDictionary.contains( _vaccImmunePdfName ) ) then
        begin
          if( not( fnDictionary.value( _vaccImmunePdfName ).fn is TPdf ) ) then
            begin
              setVaccImmunePdfName( '' );
              result := false;
            end
          ;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------

end.
