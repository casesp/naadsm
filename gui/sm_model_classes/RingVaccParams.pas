unit RingVaccParams;

(*
RingVaccParams.pas
-------------------
Begin: 2005/06/03
Last revision: $Date: 2008/03/12 22:10:53 $ $Author: areeves $
Version number: $Revision: 1.22 $
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
    SMDatabase,
    Models
  ;

  type TRingVaccParams = class( TModel )
    protected
      _prodTypeDescr: string;
      _isRingTrigger: boolean;
      _vacRadius: real;
      _startNumber: integer;
      _minTimeBetweenVacc: integer;

      _vaccPriority: integer;

      procedure setProdTypeDescr( val: string );
      procedure setIsRingTrigger( val: boolean );
      procedure setVacRadius( val: real );
      procedure setMinTimeBetweenVacc( val: integer );

      procedure setVaccPriority( val: integer );

      function getProdTypeDescr(): string;
      function getIsRingTrigger(): boolean;
      function getVacRadius(): real;
      function getMinTimeBetweenVacc(): integer;

      procedure initialize();

      // Overridden from TModel
      //-----------------------
      function getUpdated(): boolean; override;

    public
    	// Construction/initialization/destruction
      constructor create( sim: TObject; ptDescr: string ); overload;
      constructor create( db: TSMDatabase; ptID: integer; ptDescr: string ); overload;
      constructor create( const src: TRingVaccParams; sim: TObject ); overload;
      destructor destroy(); override;

      // Overridden from TModel
      //-----------------------
      function populateDatabase( db: TSMDatabase; ptID: integer ): integer; reintroduce;
			function validate( err: PString = nil ): boolean; override;
      procedure debug(); override;
      function ssXml(): string; override;

      property prodTypeDescr: string read getProdTypeDescr write setProdTypeDescr;
      property useRing: boolean read getIsRingTrigger write setIsRingTrigger;
      property ringRadius: real read getVacRadius write setVacRadius;
      property minTimeBetweenVacc: integer read getMinTimeBetweenVacc write setMinTimeBetweenVacc;

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
    USStrUtils,
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

      _sim := sim;

      _prodTypeDescr := src._prodTypeDescr;
      _isRingTrigger := src._isRingTrigger;
      _vacRadius := src._vacRadius;
      _startNumber := src._startNumber;
      _minTimeBetweenVacc := src._minTimeBetweenVacc;

      _vaccPriority := src._vaccPriority;

      _updated := src._updated;
    end
  ;


  constructor TRingVaccParams.create( db: TSMDatabase; ptID: integer; ptDescr: string );
    var
      q: string;
      db2: TSqlDatabase;
      res: TSqlResult;
      row: TSqlRow;
    begin
      inherited create();
      initialize();

      db2 := db as TSqlDatabase;

      q := 'SELECT'
      	+ ' `vaccRing`,'
        + ' `vaccRingRadius`,'
        + ' `vaccMinTimeBetweenVaccinations`,'
        + ' `vaccPriority`'
        + ' FROM `inProductionType`'
        + ' WHERE `productionTypeID` = ' + intToStr( ptID )
      ;

      res := TSqlResult.create( q, db2 );
      row := res.fetchArrayFirst();

      if( null <> row.field('vaccRing') ) then useRing := row.field('vaccRing');
      if( null <> row.field('vaccRingRadius') ) then ringRadius := row.field('vaccRingRadius');
      if( null <> row.field('vaccMinTimeBetweenVaccinations') ) then minTimeBetweenVacc := row.field('vaccMinTimeBetweenVaccinations');

      if( null <> row.field('vaccPriority') ) then
        _vaccPriority := integer( row.field('vaccPriority') )
      else
        _vaccPriority := ptID
      ;

      prodTypeDescr := ptDescr;

      res.Free();

      _updated := false;
    end
  ;


  procedure TRingVaccParams.initialize();
  	begin
    	prodTypeDescr := '';
      minTimeBetweenVacc := 0;
      ringRadius := 0;
      useRing := false;

      _vaccPriority := -1;

      _updated := false;
    end
  ;


  destructor TRingVaccParams.destroy();
  	begin
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

      dict['vaccRing'] := boolToStr( useRing );
      dict['vaccMinTImeBetweenVaccinations'] := intToStr( minTimeBetweenVacc );
      dict['vaccRingRadius'] := usFloatToStr( ringRadius );


      key :=  self.prodTypeDescr + '+' + 'ring';
      dbcout( '~~~~~~~~~~Looking for key ' + key, DBRINGVACCPARAMS );
      
      if ( ( self._sim as TSMSimulationInput).controlParams.ssVaccPriorities.HasKey( key  ) ) then
        dict['vaccPriority'] := IntToStr( (self._sim as TSMSimulationInput).controlParams.ssVaccPriorities.Item[ key ] )
      else
        raise exception.Create( 'No vacc priority specified in TRingVaccParams.populateDatabase' )
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
  procedure TRingVaccParams.debug();
    begin
      dbcout( '-------------BEGIN RING VACCINATION PARAMS', true );
      dbcout( 'Ring vaccination: isTrigger=' + boolToStr( useRing ) + ', radius=' + usFloatToStr( ringRadius ), true );
      dbcout( 'Min time between vaccinations: ' + intToStr( minTimeBetweenVacc ), true );
      dbcout( '-------------END RING VACCINATION PARAMS', true );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// XML
//-----------------------------------------------------------------------------
  function TRingVaccParams.ssXML(): string;
    begin
      // I don't think this one works...
      result := '';
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
//-----------------------------------------------------------------------------
end.
