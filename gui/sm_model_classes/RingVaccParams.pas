unit RingVaccParams;

(*
RingVaccParams.pas
-------------------
Begin: 2005/06/03
Last revision: $Date: 2011-10-19 01:24:13 $ $Author: areeves $
Version number: $Revision: 1.29.6.6 $
Project: NAADSM and related applications
Website:
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
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
    
    SMDatabase,
    Models
  ;

  type TRingVaccParams = class( TModel )
    protected
      _xmlModelList: TQStringList;

      _prodTypeDescr: string;
      _isRingTrigger: boolean;
      _vacRadius: real;
      _startNumber: integer;
      _minTimeBetweenVacc: integer;
      _vaccinateDetected: boolean;

      _vaccPriority: integer;

      // XML import
      function getXmlModelList(): TQStringList;

      procedure setProdTypeDescr( val: string );
      procedure setIsRingTrigger( val: boolean );
      procedure setVacRadius( val: real );
      procedure setMinTimeBetweenVacc( val: integer );
      procedure setVaccinateDetected( val: boolean );

      procedure setVaccPriority( val: integer );

      function getProdTypeDescr(): string;
      function getIsRingTrigger(): boolean;
      function getVacRadius(): real;
      function getMinTimeBetweenVacc(): integer;
      function getVaccinateDetected(): boolean;

      procedure initialize();

      // Overridden from TModel
      //-----------------------
      function getUpdated(): boolean; override;

    public
    	// Construction/initialization/destruction
      constructor create( sim: TObject; ptDescr: string ); overload;
      //constructor create( db: TSMDatabase; ptID: integer; ptDescr: string ); overload;
      constructor create( const src: TRingVaccParams; sim: TObject ); overload;
      constructor create( db: TSMDatabase; ptID: integer; prodTypeName: string; sim: TObject ); overload;
      destructor destroy(); override;

      // Overridden from TModel
      //-----------------------
      function populateDatabase( db: TSMDatabase; ptID: integer ): integer; reintroduce;
			function validate( err: PString = nil ): boolean; override;
      procedure debug(); override;
      function ssXml(): string; override;

      // XML import
      //-----------
      class function createXmlModelList(): TQStringList;
      procedure importXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
      property xmlModelList: TQStringList read getXmlModelList;

      property prodTypeDescr: string read getProdTypeDescr write setProdTypeDescr;
      property useRing: boolean read getIsRingTrigger write setIsRingTrigger;
      property ringRadius: real read getVacRadius write setVacRadius;
      property minTimeBetweenVacc: integer read getMinTimeBetweenVacc write setMinTimeBetweenVacc;
      property vaccinateDetected: boolean read getVaccinateDetected write setVaccinateDetected;

      property vaccPriority: integer read _vaccPriority write setVaccPriority;
    end
  ;


  const
  	DBRINGVACCPARAMS: boolean = false; // set to true to enable debugging messages for this unit.

implementation

  uses
    StrUtils,
    SysUtils,
    Variants,

    MyStrUtils,
    DebugWindow,
    SqlClasses,
    I88n,

    SMSimulationInput,
    ProductionType
  ;


//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
	constructor TRingVaccParams.create( sim: TObject; ptDescr: string );
  	begin
   		inherited create();
      initialize();

      _sim := sim;
      _prodTypeDescr := ptDescr;
    end
  ;


  constructor TRingVaccParams.create( const src: TRingVaccParams; sim: TObject );
  	begin
   		inherited create( src );
      
      _xmlModelList := nil;

      _sim := sim;

      _prodTypeDescr := src._prodTypeDescr;
      _isRingTrigger := src._isRingTrigger;
      _vacRadius := src._vacRadius;
      _startNumber := src._startNumber;
      _minTimeBetweenVacc := src._minTimeBetweenVacc;
      _vaccinateDetected := src._vaccinateDetected;

      _vaccPriority := src._vaccPriority;

      _updated := src._updated;
    end
  ;


  constructor TRingVaccParams.create( db: TSMDatabase; ptID: integer; prodTypeName: string; sim: TObject );
    var
      q: string;
      db2: TSqlDatabase;
      res: TSqlResult;
      row: TSqlRow;
    begin
      inherited create();
      initialize();

      _sim := sim;
      _prodTypeDescr := prodTypeName;

      db2 := db as TSqlDatabase;

      q := 'SELECT'
      	+ ' `vaccRing`,'
        + ' `vaccRingRadius`,'
        + ' `vaccMinTimeBetweenVaccinations`,'
        + ' `vaccVaccinateDetected`,'
        + ' `vaccPriority`'
        + ' FROM `inProductionType`'
        + ' WHERE `productionTypeID` = ' + intToStr( ptID )
      ;

      res := TSqlResult.create( q, db2 );
      row := res.fetchArrayFirst();

      if( null <> row.field('vaccRing') ) then useRing := row.field('vaccRing');
      if( null <> row.field('vaccRingRadius') ) then ringRadius := row.field('vaccRingRadius');
      if( null <> row.field('vaccMinTimeBetweenVaccinations') ) then minTimeBetweenVacc := row.field('vaccMinTimeBetweenVaccinations');
      if( null <> row.field('vaccVaccinateDetected') ) then vaccinateDetected := row.field('vaccVaccinateDetected' );

      if( null <> row.field('vaccPriority') ) then
        _vaccPriority := integer( row.field('vaccPriority') )
      else
        _vaccPriority := ptID
      ;

      res.Free();

      _updated := false;
    end
  ;


  procedure TRingVaccParams.initialize();
  	begin
      _xmlModelList := nil;

    	prodTypeDescr := '';
      minTimeBetweenVacc := 0;
      ringRadius := 0;
      useRing := false;
      vaccinateDetected := false;

      _vaccPriority := -1;

      _updated := false;
    end
  ;


  destructor TRingVaccParams.destroy();
  	begin
      freeAndNil( _xmlModelList );
    	inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Database population
//-----------------------------------------------------------------------------
	function TRingVaccParams.populateDatabase( db: TSMDatabase; ptID: integer ): integer;
  	var
    	q: string;
      dict: TQueryDictionary;
      key: string;
  	begin
    	if( DBRINGVACCPARAMS) then debug();

      dict := TQueryDictionary.create();

      dict['vaccRing'] := db.sqlBool( useRing );
      dict['vaccMinTImeBetweenVaccinations'] := intToStr( minTimeBetweenVacc );
      dict['vaccRingRadius'] := usFloatToStr( ringRadius );
      dict['vaccVaccinateDetected'] := db.sqlBool( vaccinateDetected );

      key :=  self.prodTypeDescr + '+' + 'ring';
      dbcout( '~~~~~~~~~~Looking for key ' + key, DBRINGVACCPARAMS );
      
      if ( ( self._sim as TSMSimulationInput).controlParams.ssVaccPriorities.HasKey( key  ) ) then
        dict['vaccPriority'] := IntToStr( (self._sim as TSMSimulationInput).controlParams.ssVaccPriorities.Item[ key ] )
      else
        begin
          //dbcout( 'No vacc priority specified for "' + key + '" in TRingVaccParams.populateDatabase', true );
          dict['vaccPriority'] := intToStr( -1 );
        end
      ;

      q := writeQuery(
      	'inProductionType',
        QUpdate,
        dict,
        'WHERE `productionTypeID` = ' + intToStr( ptID )
      );

      dbcout( q, DBRINGVACCPARAMS );

      result := integer( db.execute( q ) );

      dict.Clear();
      dict.Free();

      _updated := false;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Data validation
//-----------------------------------------------------------------------------
  function TRingVaccParams.validate( err: PString = nil ): boolean;
  	var
    	msg: string;
  	begin
    	result := true;
			msg := '';

      if( useRing ) then
      	begin
          if( 0 > minTimeBetweenVacc ) then
            begin
              if( nil <> err ) then msg := msg + '  ' + tr( 'Minimum time between vaccinations must be equal to or greater than 0.' ) + endl;
              result := false;
            end
          ;

          if( 0 > ringRadius ) then
            begin
              if( nil <> err ) then msg := msg + '  ' + tr( 'Ring radius must be greater than or equal to 0.' ) + endl;
              result := false;
            end
          ;
        end
      ;

      if( ( result = false ) and ( nil <> err ) ) then
      	begin
					msg := endl + ansiReplaceStr( tr( 'Ring vaccination parameters for xyz are not valid:' ), 'xyz', prodTypeDescr ) + endl + msg;
          err^ := err^ + msg;
        end
      ;
		end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Debugging
//-----------------------------------------------------------------------------
  procedure TRingVaccParams.debug();
    begin
      dbcout( '-------------BEGIN RING VACCINATION PARAMS', true );
      dbcout( 'Ring vaccination: isTrigger=' + usBoolToText( useRing ) + ', radius=' + usFloatToStr( ringRadius ), true );
      dbcout( 'Vaccination priority: ' + intToStr( vaccPriority ), true );
      dbcout( 'Min time between vaccinations: ' + intToStr( minTimeBetweenVacc ), true );
      dbcout( 'Vaccinate detected units: ' + usBoolToText( vaccinateDetected ), true );
      dbcout( '-------------END RING VACCINATION PARAMS', true );
    end
  ;
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
  function TRingVaccParams.getUpdated(): boolean; begin result := _updated; end;

  procedure TRingVaccParams.setProdTypeDescr( val: string ); begin _prodTypeDescr := val; _updated := true; end;
  procedure TRingVaccParams.setIsRingTrigger( val: boolean ); begin _isRingTrigger := val; _updated := true; end;
  procedure TRingVaccParams.setVacRadius( val: real ); begin _vacRadius := val; _updated := true; end;
  procedure TRingVaccParams.setMinTimeBetweenVacc( val: integer ); begin _minTimeBetweenVacc := val; _updated := true; end;
  procedure TRingVaccParams.setVaccinateDetected( val: boolean ); begin _vaccinateDetected := val; _updated := true; end;

  procedure TRingVaccParams.setVaccPriority( val: integer ); begin _vaccPriority := val; _updated := true; end;

  function TRingVaccParams.getProdTypeDescr(): string;
    begin
      if( 0 = length( _prodTypeDescr ) ) then
        raise exception.Create( 'TRingVaccParams._prodTypeDescr is not set' )
      ;
      result := _prodTypeDescr;
    end
  ;

  function TRingVaccParams.getIsRingTrigger(): boolean; begin Result := _isRingTrigger; end;
  function TRingVaccParams.getVacRadius(): real; begin Result := _vacRadius; end;
  function TRingVaccParams.getMinTimeBetweenVacc(): integer; begin Result := _minTimeBetweenVacc; end;
  function TRingVaccParams.getVaccinateDetected(): boolean; begin result := _vaccinateDetected; end;
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// XML Export
//-----------------------------------------------------------------------------
  function TRingVaccParams.ssXML(): string;
    begin
      raise exception.Create( 'Use TProductionType.ssRingVaccXml() instead of TRingVaccParams.ssXML()' );
      result := '';
    end
  ;
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// XML import
//-----------------------------------------------------------------------------

//note: Vaccine related parameters (vaccine-model) are imported by VaccinationParams

  class function TRingVaccParams.createXmlModelList(): TQStringList;
    begin
      result := TQStringList.create();
      result.Append( 'ring-vaccination-model' );
    end
  ;

  function TRingVaccParams.getXmlModelList(): TQStringList;
    begin
      if( nil = _xmlModelList ) then
        _xmlModelList := createXmlModelList()
      ;

      result := _xmlModelList;
    end
  ;

  procedure TRingVaccParams.importXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
    var
      value: double;
      subElement: pointer;
      priority: integer;
      minTimeBetweenVacc: integer;
      vaccinateDetectedUnits: boolean;
      smSim: TSMSimulationInput;
      ok: boolean;
    begin
      smSim := sim as TSMSimulationInput;
      ok := false;

      // Parse this production type as the "source" type.
      //-------------------------------------------------
      if( self.prodTypeDescr = sdew.GetElementAttribute( model, 'from-production-type' ) ) then
        begin
          ok := true;
          subElement := Sdew.GetElementByName( model, 'radius' );
          if ( nil <> subElement ) then
            begin
              if ( nil <> Sdew.GetElementByName( subElement, 'value' ) ) then
                begin
                  value := usStrToFloat( Sdew.GetElementContents( Sdew.GetElementByName( subElement, 'value' ) ) );
                  //  Units are present in the xml file, but not stored anywhere in the current schema.
                  self.ringRadius := value;
                end
              ;
            end
          ;

          subElement := Sdew.GetElementByName( model, 'min-time-between-vaccinations');
          if ( nil <> subElement ) then
            begin
              minTimeBetweenVacc := StrToInt( Sdew.GetElementContents( Sdew.GetElementByName( subElement, 'value' ) ) );
              //  Units are present in the xml file, but not stored in anywhere in the current schema.
              self.minTimeBetweenVacc := minTimeBetweenVacc;
            end
          ;

          subElement := Sdew.GetElementByName( model, 'vaccinate-detected-units');
          if ( nil <> subElement ) then
            begin
              vaccinateDetectedUnits := StrToBool( Sdew.GetElementContents( subElement ) );
              //  Units are present in the xml file, but not stored in anywhere in the current schema.
              self.vaccinateDetected := vaccinateDetectedUnits;
            end
          ;

          self.useRing := true;

          // Set the vaccination priority for the from-type to bogus value so that the populateDatabase() function won't
          // throw an exception.  The from-type doesn't need a priority here, but the populate function doesn't know that.

          if( not( smSim.controlParams.ssVaccPriorities.contains( self.prodTypeDescr + '+' + 'ring' ) ) ) then
             smSim.controlParams.ssVaccPriorities.Add( self.prodTypeDescr + '+' + 'ring', -1 )
          ;
        end
      ;

      // Parse this production type as the "destination" type.
      //------------------------------------------------------

      if( self.prodTypeDescr = sdew.GetElementAttribute( model, 'to-production-type' ) ) then
        begin
          ok := true;
          //Priorities go with the "to type", i.e. destination.
          if( nil = sdew.GetElementByName( model, 'priority' ) ) then
            begin
              appendToPstring( errMsg, tr( 'Priority is missing from ring vaccination XML.' ) );
              exit;
            end
          else
            begin
              priority := myStrToInt( Sdew.GetElementContents( Sdew.GetElementByName( model, 'priority' ) ), -1 );
              if( -1 = priority ) then
                begin
                  appendToPstring( errMsg, tr( 'Priority is missing from ring vaccination XML.' ) );
                  exit;
                end
              ;

              self.vaccPriority := priority;

              if( smSim.controlParams.ssVaccPriorities.contains( self.prodTypeDescr + '+' + 'ring' ) ) then
                begin
                  if ( smSim.controlParams.ssVaccPriorities.Item[self.prodTypeDescr + '+' + 'ring'] > priority ) then
                    smSim.controlParams.ssVaccPriorities.Item[self.prodTypeDescr + '+' + 'ring'] := priority
                  ;
                end
              else
                smSim.controlParams.ssVaccPriorities.Item[self.prodTypeDescr + '+' + 'ring'] := priority
              ;
            end
          ;
        end
      ;

      if not ok then raise exception.Create( 'Unhandled GetElementAttribute value in TRingVaccParams.importXml()' );
    end
  ;
//-----------------------------------------------------------------------------

end.
