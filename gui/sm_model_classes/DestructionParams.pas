unit DestructionParams;

(*
DestructionParams.pas
----------------------
Begin: 2005/01/06
Last revision: $Date: 2008/10/23 20:24:39 $ $Author: areeves $
Version number: $Revision: 1.36 $
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
    RelFunction,
    SMDatabase,
    Models
  ;

  type TDestructionParams = class( TModel )
    protected
    	// Properties
      //-------------
      _prodTypeDescr: string;

      // Indicates whether detected, infected units will be destroyed (basic destruction)
      _destroyDetectedUnits: boolean;

      // Indicates whether detection will trigger ring destruction
      _isRingTrigger: boolean;
      _ringRadius: double;

      // Indicates whether units of this type are destroyed in ring destruction
      _isRingTarget: boolean;

      // Tracing parameters
      //-------------------
      // Indicates whether tracing will trigger pre-emptive slaughter
      _traceDirectContact: boolean;
      _traceIndirectContact: boolean;
      _directTracePeriod: integer;
      _indirectTracePeriod: integer;
      _indirectTraceSuccess: real;
      _directTraceSuccess: real;

      // Indicates whether detection will trigger pre-emptive slaughter
      _destroyDirectTraces: boolean;
      _destroyIndirectTraces: boolean;

      _destrPriority: integer;

      procedure initialize();


      // Overridden from TModel
      //-----------------------
      function getUpdated(): boolean; override;


      // Properties
      //-------------
      procedure setProdTypeDescr( val: string );
      function getProdTypeDescr(): string;

      procedure setDestrPriority( val: integer );

      // For any reason
      function getDestroyForAnyReason(): boolean;

      // For basic destruction
      procedure setDestroyDetectedUnits( val: boolean );
      function getDestroyDetectedUnits(): boolean;

      // For ring destruction
      procedure setIsRingTrigger( val: boolean );
      function getIsRingTrigger(): boolean;
      procedure setRingRadius( val: double );
      function getRingRadius(): double;
      procedure setIsRingTarget( val: boolean );
      function getIsRingTarget(): boolean;

      // For tracing
      procedure setDirectTracePeriod( val: integer );
      function getDirectTracePeriod(): integer;
      procedure setIndirectTracePeriod( val: integer );
      function getIndirectTracePeriod(): integer;
      procedure setTraceDirectContact( val: boolean );
      function getTraceDirectContact(): boolean;
      procedure setTraceIndirectContact( val: boolean );
      function getTraceIndirectContact(): boolean;
      procedure setIndirectTraceSuccess( val: double );
      procedure setDirectTraceSuccess( val: double );
      function getIndirectTraceSuccess(): double;
      function getDirectTraceSuccess(): double;

      // For traceback destruction
      procedure setDestroyDirectTraces( val: boolean );
      function getDestroyDirectTraces(): boolean;
      procedure setDestroyIndirectTraces( val: boolean );
      function getDestroyIndirectTraces(): boolean;

    public
    	constructor create(sim: TObject; ptDescr: string ); overload;
      constructor create( const src: TDestructionParams; sim: TObject ); overload;
      destructor destroy(); override;

      procedure initializeFromDB( db: TSMDatabase; ptID: integer; prodTypeName: string );

      // Module functions
      //------------------
      function ssBasicDestrModelXml( priority: integer; const productionTypeID: integer ): string;
      function ssTracebackDestrModelXml(
        priority: integer;
        traceTypeDescr: string;
        const includeDestructionGlobal: boolean;
        const productionTypeID: integer
      ): string;


      // Overridden from TModel
      //-----------------------
      function ssXml(): string; override;
      function populateDatabase( db: TSMDatabase; ptID: integer ): integer; reintroduce;
  		function validate( err: PString = nil ): boolean; override;
      procedure debug(); override;


      // Properties
      //------------
			property prodTypeDescr: string read getProdTypeDescr write setProdTypeDescr;

      // For any reason
      property destroyForAnyReason: boolean read getDestroyForAnyReason;

      // For basic destruction
      property destroyDetectedUnits: boolean read getDestroyDetectedUnits write setDestroyDetectedUnits;

      // For ring destruction
      property isRingTarget: boolean read getIsRingTarget write setIsRingTarget;
      property isRingTrigger: boolean read getIsRingTrigger write setIsRingTrigger;
      property ringRadius: double read getRingRadius write setRingRadius;

      // For traceback destruction
      property destroyDirectTraces: boolean read getDestroyDirectTraces write setDestroyDirectTraces;
      property destroyIndirectTraces: boolean read getDestroyIndirectTraces write setDestroyIndirectTraces;

      // For tracing
      property directTracePeriod: integer read getDirectTracePeriod write setDirectTracePeriod;
      property indirectTracePeriod: integer read getIndirectTracePeriod write setIndirectTracePeriod;
      property traceDirectContact: boolean read getTraceDirectContact write setTraceDirectContact;
      property traceIndirectContact: boolean read getTraceIndirectContact write setTraceIndirectContact;
      property indirectTraceSuccess: double read getIndirectTraceSuccess write setIndirectTraceSuccess;
      property directTraceSuccess: double read getDirectTraceSuccess write setDirectTraceSuccess;

      property destrPriority: integer read _destrPriority write setDestrPriority;
    end
  ;

  const DBDESTRUCTIONPARAMS: boolean = false; // Set to true to enable debugging messages for this unit.



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

    SMSimulationInput
  ;

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
	constructor TDestructionParams.create( sim: TObject; ptDescr: string );
  	begin
      inherited create();
      initialize();

      _sim := sim;
      _prodTypeDescr := ptDescr;
    end
  ;


  constructor TDestructionParams.create( const src: TDestructionParams; sim: TObject );
    begin
      inherited create( src );
      _sim := sim;
      
    	_destroyDetectedUnits := src._destroyDetectedUnits;

      _isRingTrigger := src._isRingTrigger;
      _ringRadius := src._ringRadius;
      _isRingTarget := src._isRingTarget;

      _traceDirectContact := src._traceDirectContact;
      _traceIndirectContact := src._traceIndirectContact;

      _directTracePeriod := src._directTracePeriod;
      _directTraceSuccess := src._directTraceSuccess;

      _indirectTracePeriod := src._indirectTracePeriod;
      _indirectTraceSuccess := src._indirectTraceSuccess;

      _destroyDirectTraces := src._destroyDirectTraces;
      _destroyIndirectTraces := src._destroyIndirectTraces;

      prodTypeDescr := src.prodTypeDescr;

      _destrPriority := src._destrPriority;

      _updated := src._updated;
    end
  ;


  procedure TDestructionParams.initializeFromDB( db: TSMDatabase; ptID: integer; prodTypeName: string );
    var
      q: string;
      db2: TSqlDatabase;
      res: TSqlResult;
      row: TSqlRow;
    begin
      db2 := db as TSqlDatabase;

      q := 'SELECT'
      	+ ' `useBasicDestruction`,'

        + ' `destrIsRingTarget`,'
        + ' `destrIsRingTrigger`,'
        + ' `destrRingRadius`,'

        + ' `traceDirect`,'
        + ' `traceDirectSuccess`,'
        + ' `traceDirectTracePeriod`,'

        + ' `traceIndirect`,'
        + ' `traceIndirectSuccess`,'
        + ' `traceIndirectTracePeriod`,'

        + ' `destrDirectTraces`,'
        + ' `destrIndirectTraces`,'

        + ' `destrPriority`'

        + ' FROM `inProductionType`'
        + ' WHERE `productionTypeID` = ' + intToStr( ptID )
      ;

      res := TSqlResult.create( q, db2 );
      row := res.fetchArrayFirst();

      if( null <> row.field('useBasicDestruction') ) then
        destroyDetectedUnits := boolean( row.field('useBasicDestruction') )
      else
        destroyDetectedUnits := false
      ;

      if( null <> row.field('destrIsRingTarget') ) then
        isRingTarget := boolean( row.field('destrIsRingTarget') )
      else
        isRingTarget := false
      ;

      if( null <> row.field('destrIsRingTrigger') ) then
        isRingTrigger := boolean( row.field('destrIsRingTrigger') )
      else
        isRingTrigger := false
      ;
      if( null <> row.field('destrRingRadius') ) then ringRadius := row.field('destrRingRadius');

      if( null <> row.field('traceDirect') ) then
        traceDirectContact := boolean( row.field('traceDirect') )
      else
        traceDirectContact := false
      ;
      if( null <> row.field('traceDirectSuccess') ) then directTraceSuccess := double( row.field('traceDirectSuccess') );
      if( null <> row.field('traceDirectTracePeriod') ) then directTracePeriod := integer( row.field('traceDirectTracePeriod') );

      if( null <> row.field('traceIndirect') ) then
        traceIndirectContact := boolean( row.field('traceIndirect') )
      else
        traceIndirectContact := false
      ;
      if( null <> row.field('traceIndirectSuccess') ) then indirectTraceSuccess := double( row.field('traceIndirectSuccess') );
      if( null <> row.field('traceIndirectTracePeriod') ) then indirectTracePeriod := integer( row.field('traceIndirectTracePeriod') );

      if( null <> row.field('destrDirectTraces') ) then
        destroyDirectTraces := boolean( row.field('destrDirectTraces') )
      else
        destroyDirectTraces := false
      ;

      if( null <> row.field('destrIndirectTraces') ) then
        destroyIndirectTraces := boolean( row.field('destrIndirectTraces') )
      else
        destroyIndirectTraces := false
      ;

      if( null <> row.field('destrPriority') ) then
        _destrPriority := integer( row.field('destrPriority') )
      else
        _destrPriority := ptID
      ;

      prodTypeDescr := prodTypeName;

      freeAndNil( res );

      _updated := false;
    end
  ;


  procedure TDestructionParams.initialize();
  	begin
    	// Yes, I know that it is unnecessary to initialize values in Delphi.
      // I think it should be done anyway.
    	_destroyDetectedUnits := false;

      _isRingTrigger := false;
      _ringRadius := 0.0;

      _isRingTarget := false;

      _directTracePeriod := 0;
      _directTraceSuccess := 0.0;

      _indirectTracePeriod := 0;
      _indirectTraceSuccess := 0.0;

      _traceDirectContact := false;
      _traceIndirectContact := false;

      _destroyDirectTraces := false;
      _destroyIndirectTraces := false;

      _destrPriority := -1;

      _updated := false;

      _prodTypeDescr := '';
    end
  ;


  destructor TDestructionParams.destroy();
    begin
			inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Database population
//-----------------------------------------------------------------------------
  function TDestructionParams.populateDatabase( db: TSMDatabase; ptID: integer ): integer;
  	var
    	q: string;
      dict: TQueryDictionary;
  	begin
      dict := TQueryDictionary.create();

      dict['useBasicDestruction'] := boolToStr( destroyDetectedUnits );

      dict['destrIsRingTarget'] := boolToStr( isRingTarget );
      dict['destrIsRingTrigger'] := boolToStr( isRingTrigger );
      dict['destrRingRadius' ] := usFloatToStr( ringRadius );

      dict['traceDirect'] := boolToStr( traceDirectContact );
      dict['traceDirectSuccess'] := usFloatToStr( directTraceSuccess );
      dict['traceDirectTracePeriod'] := intToStr( directTracePeriod );

      dict['traceIndirect'] := boolToStr( traceIndirectContact );
      dict['traceIndirectSuccess'] := usFloatToStr( indirectTraceSuccess );
      dict['traceIndirectTracePeriod'] := intToStr( indirectTracePeriod );

      dict['destrDirectTraces'] := boolToStr( destroyDirectTraces );
      dict['destrIndirectTraces'] := boolToStr( destroyIndirectTraces );

      if ( ( self._sim as TSMSimulationInput).controlParams.ssDestrPriorities.HasKey( self.prodTypeDescr + '+' + 'basic' ) ) then
        dict['destrPriority'] := IntToStr( (self._sim as TSMSimulationInput).controlParams.ssDestrPriorities.Item[ self.prodTypeDescr + '+' + 'basic' ] )
      else
        raise exception.Create( 'No destr priority specified in TDestructionParams.populateDatabase' )
      ;

     //??? Are there other "priorities" which need to be written here as well???
     
      q := writeQuery(
      	'inProductionType',
        QUpdate,
        dict,
        'WHERE `productionTypeID` = ' + intToStr( ptID )
      );
      
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
  function TDestructionParams.ssXml(): string;
    begin
      // I don't think this one will work...
      result := '';
    end
  ;


	function TDestructionParams.ssBasicDestrModelXml( priority: integer; const productionTypeID: integer ): string;
  	begin
    	result := endl;
      result := result + '  <basic-destruction-model production-type="' + encodeXml( prodTypeDescr ) + '" production-type-id="' + intToStr( productionTypeID ) + '">' + endl;
      result := result + '    <priority>' + intToStr( priority ) + '</priority>  <!-- Based on the production type and destruction reason (basic aka detection) -->' + endl;
      result := result + '  </basic-destruction-model>' + endl;
      result := result + endl;
    end
  ;


	function TDestructionParams.ssTracebackDestrModelXml(
        priority: integer;
        traceTypeDescr: string;
        const includeDestructionGlobal: boolean;
        const productionTypeID: integer
      ): string;
  	var
    	s: string;
      success: real;
      period: integer;
      quarantineOnly: boolean;
  	begin
    	// FIX ME: outputs may (or may not) be needed here


      // FIX ME: this is an ugly hack
      if( 'direct' = traceTypeDescr ) then
      	begin
          dbcout( 'destroyDirectTraces: ' + usBoolToText(destroyDirectTraces), DBDESTRUCTIONPARAMS );
          dbcout( 'includeDestructionGlobal: ' + usBoolToText(includeDestructionGlobal), DBDESTRUCTIONPARAMS );

        	success := directTraceSuccess;
          period := directTracePeriod;
          quarantineOnly := ( not( destroyDirectTraces ) or not( includeDestructionGlobal ) );
        end
      else
      	begin
          dbcout( 'destroyIndirectTraces: ' + usBoolToText(destroyIndirectTraces), DBDESTRUCTIONPARAMS );
          dbcout( 'includeDestructionGlobal: ' + usBoolToText(includeDestructionGlobal), DBDESTRUCTIONPARAMS );

        	success := indirectTraceSuccess;
          period := indirectTracePeriod;
          quarantineOnly := ( not( destroyIndirectTraces ) or not( includeDestructionGlobal ) );
        end
      ;

      s := endl;
      s := s + '  <trace-back-destruction-model production-type="' + encodeXml( prodTypeDescr ) + '" production-type-id="' + intToStr( productionTypeID ) + '" contact-type="' + traceTypeDescr + '">' + endl;
      s := s + '    <priority>' + intToStr( priority ) + '</priority> <!-- Based on production type and destruction reason (trace direct/indirect) -->' + endl;
      s := s + '      <trace-success>' + usFloatToStr( success ) + '</trace-success>' + endl;
      s := s + '      <trace-period>' + endl;
      s := s + '        <value>' + intToStr( period ) + '</value>' + endl;
      s := s + '        <units><xdf:unit>day</xdf:unit></units>' + endl;
      s := s + '      </trace-period>' + endl;
      s := s + '    <quarantine-only>' + usBoolToText( quarantineOnly ) + '</quarantine-only>' + endl;
      s := s + '  </trace-back-destruction-model>' + endl;
      s := s + endl;

      result := s;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Data validation
//-----------------------------------------------------------------------------
  function TDestructionParams.validate( err: PString = nil ): boolean;
  	var
    	msg: string;
    	submsg: string;
  	begin
    	result := true;
			msg := '';

      submsg := '';

    	// There is nothing to validate for basic destruction.

      // For ring destruction:
      // 0 <= ring radius, if is ring trigger

      if( isRingTrigger ) then
        begin
          if( 0.0 > ringRadius ) then
            begin
              if( nil <> err ) then
                msg := msg + '  ' + tr( 'Destruction ring radius must be specified.' ) + endl
              ;

              result := false;
            end
          ;
        end
      ;


      // For traceback destruction:
      // 0 <= probabilities of success <=1

      if( traceDirectContact ) then
      	begin
        	if( ( 0 > directTraceSuccess ) or ( 1 < directTraceSuccess ) ) then
          	begin
              if( nil <> err ) then
              	msg := msg + '  ' + tr( 'Probability of direct trace success must be between 0 and 1.' ) + endl
              ;
              result := false;
            end
          ;
        end
      ;

      if( traceIndirectContact ) then
      	begin
        	if( ( 0 > indirectTraceSuccess ) or ( 1 < indirectTraceSuccess ) ) then
          	begin
              if( nil <> err ) then
              	msg := msg + '  ' + tr( 'Probability of indirect trace success must be between 0 and 1.' ) + endl
              ;
              result := false;
            end
          ;
        end
      ;

      if( ( result = false ) and ( nil <> err ) ) then
      	begin
					msg := endl + ansiReplaceStr( tr( 'Destruction parameters for xyz:' ), 'xyz', prodTypeDescr ) + endl + msg;
          err^ := err^ + msg;
        end
      ;

    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Debugging
//-----------------------------------------------------------------------------
  procedure TDestructionParams.debug();
    begin
      dbcout( '---------DESTRUCTION PARAMETERS' + endl, true );

      if( destroyDetectedUnits ) then
      	dbcout( 'Destroy detected units: TRUE', true )
      else
      	dbcout( 'Destroy detected units: FALSE', true )
      ;

      if( isRingTrigger ) then
      	begin
        	dbcout( 'Ring trigger: TRUE' + endl, true );
        	dbcout( 'Ring destruction: radius ' + usFloatToStr( ringRadius ) + endl, true );
        end
      else
      	dbcout( 'Ring trigger: FALSE' + endl, true )
      ;

      if( traceDirectContact ) then
        dbcout( 'Direct trace destruction: period ' + intToStr( directTracePeriod )
          + ', success rate ' + usFloatToStr( directTraceSuccess )
          + endl, true )
      else
      	dbcout( 'Direct trace destruction: FALSE', true )
      ;

      if( traceIndirectContact ) then
        dbcout( 'Indirect trace destruction: period ' + intToStr( indirectTracePeriod )
          + ', success rate ' + usFloatToStr( indirectTraceSuccess )
          + endl, true )
      else
      	dbcout( 'Indirect trace destruction: FALSE', true )
      ;


      dbcout( '---------END DESTRUCTION PARAMETERS' + endl, true );

    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
  function TDestructionParams.getProdTypeDescr(): string;
    begin
      if( 0 = length( _prodTypeDescr ) ) then
        raise exception.Create( 'TDestructionParams._prodTypeDescr is not set' )
      ;
      result := _prodTypeDescr;
    end
  ;

  procedure TDestructionParams.setProdTypeDescr( val: string ); begin _prodTypeDescr := val; end;

  
  function TDestructionParams.getDestroyForAnyReason(): boolean;
    begin
      result :=
        destroyDetectedUnits
      or
        destroyDirectTraces
      or
        destroyIndirectTraces
      or
        isRingTarget
      ;
    end
  ;

  // Destruction of detected units
  function TDestructionParams.getDestroyDetectedUnits(): boolean; begin Result := _destroyDetectedUnits; end;

  procedure TDestructionParams.setDestroyDetectedUnits( val: boolean );
    begin
      _destroyDetectedUnits := val;
      _updated := true;
    end
  ;

  // Ring destruction
  function TDestructionParams.getIsRingTrigger(): boolean; begin Result := _isRingTrigger; end;

  procedure TDestructionParams.setIsRingTrigger( val: boolean );
    begin
      _isRingTrigger := val;
      _updated := true;
    end
  ;

  function TDestructionParams.getRingRadius(): double; begin Result := _ringRadius; end;

  procedure TDestructionParams.setRingRadius( val: double );
    begin
      _ringRadius := val;
      _updated := true;
    end
  ;

  procedure TDestructionParams.setIsRingTarget( val: boolean );
    begin
      _isRingTarget := val;
      _updated := true;
    end
  ;

  function TDestructionParams.getIsRingTarget(): boolean; begin result := _isRingTarget; end;

  // Traceback destruction
  procedure TDestructionParams.setDestroyDirectTraces( val: boolean );
    begin
      _destroyDirectTraces := val;
      _updated := true;
    end
  ;

  function TDestructionParams.getDestroyDirectTraces(): boolean; begin result := _destroyDirectTraces; end;

  procedure TDestructionParams.setDestroyIndirectTraces( val: boolean );
    begin
      _destroyIndirectTraces := val;
      _updated := true;
    end
  ;

  function TDestructionParams.getDestroyIndirectTraces(): boolean; begin result := _destroyIndirectTraces; end;

  // Tracing
  function TDestructionParams.getDirectTracePeriod(): integer; begin Result := _directTracePeriod; end;
  function TDestructionParams.getIndirectTracePeriod(): integer; begin Result := _indirectTracePeriod; end;
  function TDestructionParams.getTraceDirectContact(): boolean; begin Result := _traceDirectContact; end;
  function TDestructionParams.getTraceIndirectContact(): boolean; begin Result := _traceIndirectContact; end;
  function TDestructionParams.getIndirectTraceSuccess(): double; begin Result := _indirectTraceSuccess; end;
  function TDestructionParams.getDirectTraceSuccess(): double; begin Result := _directTraceSuccess; end;

  procedure TDestructionParams.setDirectTracePeriod( val: integer );
    begin
      _directTracePeriod := val;
      _updated := true;
    end
  ;

  procedure TDestructionParams.setIndirectTracePeriod( val: integer );
    begin
      _indirectTracePeriod := val;
      _updated := true;
    end
  ;

  procedure TDestructionParams.setTraceDirectContact( val: boolean );
    begin
      _traceDirectContact := val;
      _updated := true;
    end
  ;

  procedure TDestructionParams.setTraceIndirectContact( val: boolean );
    begin
      _traceIndirectContact := val;
      _updated := true;
    end
  ;

  procedure TDestructionParams.setIndirectTraceSuccess( val: double );
    begin
      _indirectTraceSuccess := val;
      _updated := true;
    end
  ;

  procedure TDestructionParams.setDirectTraceSuccess( val: double );
    begin
      _directTraceSuccess := val;
      _updated := true;
    end
  ;

  procedure TDestructionParams.setDestrPriority( val: integer );
    begin
      _destrPriority := val;
      _updated := true;
    end
  ;


  function TDestructionParams.getUpdated(): boolean; begin result := _updated; end;
//-----------------------------------------------------------------------------

end.
