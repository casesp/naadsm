unit DestructionParams;

(*
DestructionParams.pas
----------------------
Begin: 2005/01/06
Last revision: $Date: 2010-07-13 19:32:47 $ $Author: rhupalo $
Version number: $Revision: 1.42.4.3 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2005 - 2010 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
    QStringMaps,
    QLists,

    Sdew,

    Models,
    RelFunction,

    SMDatabase
  ;

  type TDestructionParams = class( TModel )
    protected
    	// Properties
      //-------------
      _xmlModelList: TQStringList;

      _prodTypeDescr: string;

      // Indicates whether detected, infected units will be destroyed (basic destruction)
      _destroyDetectedUnits: boolean;

      // Indicates whether detection will trigger ring destruction
      _isRingTrigger: boolean;
      _ringRadius: double;

      // Indicates whether units of this type are destroyed in ring destruction
      _isRingTarget: boolean;

      // Parameters for destruction of traced units
      //-------------------------------------------
      // Indicates whether tracing will trigger pre-emptive slaughter
      _destroyDirectForwardTraces: boolean;
      _destroyIndirectForwardTraces: boolean;
      _destroyDirectBackTraces: boolean;
      _destroyIndirectBackTraces: boolean;

      _destrPriority: integer;

      procedure initialize();

      // Overridden from TModel
      //-----------------------
      function getUpdated(): boolean; override;

      // XML import
      //-----------
      function getXmlModelList(): TQStringList;
      procedure importBasicDestructionModelXml( model: pointer; sdew: TSdew; errMsg: pstring );
      procedure importTraceDestructionXml( model: pointer; sdew: TSdew; errMsg: pstring );
      procedure importRingDestructionXml( model: pointer; sdew: TSdew; errMsg: pstring );

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

      // For destruction of traced units
      procedure setDestroyDirectForwardTraces( val: boolean );
      function getDestroyDirectForwardTraces(): boolean;
      procedure setDestroyIndirectForwardTraces( val: boolean );
      function getDestroyIndirectForwardTraces(): boolean;
      procedure setDestroyDirectBackTraces( val: boolean );
      function getDestroyDirectBackTraces(): boolean;
      procedure setDestroyIndirectBackTraces( val: boolean );
      function getDestroyIndirectBackTraces(): boolean;
      function getUseTraceDestruction(): boolean;

    public
    	constructor create(sim: TObject; ptDescr: string ); overload;
      constructor create( const src: TDestructionParams; sim: TObject ); overload;
      constructor create( db: TSMDatabase; ptID: integer; prodTypeName: string; sim: TObject ); overload;

      destructor destroy(); override;

      // Module functions
      //------------------
      function ssBasicDestrModelXml( const productionTypeID, priority: integer ): string;
      function ssTraceDestrModelXml( const productionTypeID: integer; destrPriorityList: TQStringLongIntMap ): string;

      // Overridden from TModel
      //-----------------------
      function ssXml(): string; override;
      function populateDatabase( db: TSMDatabase; ptID: integer ): integer; reintroduce;
  		function validate( err: PString = nil ): boolean; override;
      procedure debug(); override;

      // XML import
      //-----------
      class function createXmlModelList(): TQStringList;
      procedure importXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
      property xmlModelList: TQStringList read getXmlModelList;

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

      // For destruction of traced units
      property destroyDirectForwardTraces: boolean read getDestroyDirectForwardTraces write setDestroyDirectForwardTraces;
      property destroyIndirectForwardTraces: boolean read getDestroyIndirectForwardTraces write setDestroyIndirectForwardTraces;
      property destroyDirectBackTraces: boolean read getDestroyDirectBackTraces write setDestroyDirectBackTraces;
      property destroyIndirectBackTraces: boolean read getDestroyIndirectBackTraces write setDestroyIndirectBackTraces;
      property useTraceDestruction: boolean read getUseTraceDestruction;

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

      _destroyDirectForwardTraces := src._destroyDirectForwardTraces;
      _destroyIndirectForwardTraces := src._destroyIndirectForwardTraces;
      _destroyDirectBackTraces := src._destroyDirectBackTraces;
      _destroyIndirectBackTraces := src._destroyIndirectBackTraces;

      prodTypeDescr := src.prodTypeDescr;

      _destrPriority := src._destrPriority;

      _updated := src._updated;
    end
  ;


  constructor TDestructionParams.create( db: TSMDatabase; ptID: integer; prodTypeName: string; sim: TObject );
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
      	+ ' `useBasicDestruction`,'

        + ' `destrIsRingTarget`,'
        + ' `destrIsRingTrigger`,'
        + ' `destrRingRadius`,'

        + ' `destrDirectForwardTraces`,'
        + ' `destrIndirectForwardTraces`,'
        + ' `destrDirectBackTraces`,'
        + ' `destrIndirectBackTraces`,'

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

      if( null <> row.field('destrDirectForwardTraces') ) then
        destroyDirectForwardTraces := boolean( row.field('destrDirectForwardTraces') )
      else
        destroyDirectForwardTraces := false
      ;

      if( null <> row.field('destrIndirectForwardTraces') ) then
        destroyIndirectForwardTraces := boolean( row.field('destrIndirectForwardTraces') )
      else
        destroyIndirectForwardTraces := false
      ;

      if( null <> row.field('destrDirectBackTraces') ) then
        destroyDirectBackTraces := boolean( row.field('destrDirectBackTraces') )
      else
        destroyDirectBackTraces := false
      ;

      if( null <> row.field('destrIndirectBackTraces') ) then
        destroyIndirectBackTraces := boolean( row.field('destrIndirectBackTraces') )
      else
        destroyIndirectBackTraces := false
      ;

      if( null <> row.field('destrPriority') ) then
        _destrPriority := integer( row.field('destrPriority') )
      else
        _destrPriority := ptID
      ;

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

      _xmlModelList := nil;

      _destroyDirectForwardTraces := false;
      _destroyIndirectForwardTraces := false;
      _destroyDirectBackTraces := false;
      _destroyIndirectBackTraces := false;

      _destrPriority := -1;

      _updated := false;

      _prodTypeDescr := '';
    end
  ;


  destructor TDestructionParams.destroy();
    begin
      freeAndNil( _xmlModelList );
      
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

      dict['useBasicDestruction'] := usBoolToText( destroyDetectedUnits );

      dict['destrIsRingTarget'] := usBoolToText( isRingTarget );
      dict['destrIsRingTrigger'] := usBoolToText( isRingTrigger );
      dict['destrRingRadius' ] := usFloatToStr( ringRadius );

      dict['destrDirectForwardTraces'] := usBoolToText( destroyDirectForwardTraces );
      dict['destrIndirectForwardTraces'] := usBoolToText( destroyIndirectForwardTraces );
      dict['destrDirectBackTraces'] := usBoolToText( destroyDirectBackTraces );
      dict['destrIndirectBackTraces'] := usBoolToText( destroyIndirectBackTraces );
      
      if ( ( self._sim as TSMSimulationInput).controlParams.ssDestrPriorities.HasKey( self.prodTypeDescr + '+' + 'basic' ) ) then
        dict['destrPriority'] := IntToStr( (self._sim as TSMSimulationInput).controlParams.ssDestrPriorities.Item[ self.prodTypeDescr + '+' + 'basic' ] )
      else
        begin
          //raise exception.Create( 'No destr priority specified in TDestructionParams.populateDatabase' );
          dbcout( 'No destr priority specified in TDestructionParams.populateDatabase', true );
          dict['destrPriority'] := intToStr( -1 );
        end
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
      raise exception.Create( 'Don''t use TDestructionParams.ssXml()!!' );
      result := '';
    end
  ;


	function TDestructionParams.ssBasicDestrModelXml( const productionTypeID, priority: integer ): string;
  	begin
    	result := endl;
      result := result + '  <basic-destruction-model production-type="' + encodeXml( prodTypeDescr ) + '" production-type-id="' + intToStr( productionTypeID ) + '">' + endl;
      result := result + '    <priority>' + intToStr( priority ) + '</priority>  <!-- Based on the production type and destruction reason (basic aka detection) -->' + endl;
      result := result + '  </basic-destruction-model>' + endl;
      result := result + endl;
    end
  ;


  function TDestructionParams.ssTraceDestrModelXml( const productionTypeID: integer; destrPriorityList: TQStringLongIntMap ): string;
    var
      priority: integer;
    begin
      result := '';

      if( destroyDirectForwardTraces ) then
        begin
          priority := destrPriorityList[ prodTypeDescr + '+' + 'direct-forward' ];
          result := result
            + '  <trace-destruction-model production-type="' + encodeXml( prodTypeDescr ) + '" production-type-id="' + intToStr( productionTypeID ) + '"'
            + ' contact-type="direct" direction="out">' + endl
            + '    <priority>' + intToStr( priority ) + '</priority>' + endl
            + '  </trace-destruction-model>' + endl + endl
          ;
        end
      ;

      if( destroyIndirectForwardTraces ) then
        begin
          priority := destrPriorityList[ prodTypeDescr + '+' + 'indirect-forward' ];
          result := result
            + '  <trace-destruction-model production-type="' + encodeXml( prodTypeDescr ) + '" production-type-id="' + intToStr( productionTypeID ) + '"'
            + ' contact-type="indirect" direction="out">' + endl
            + '    <priority>' + intToStr( priority ) + '</priority>' + endl
            + '  </trace-destruction-model>' + endl + endl
          ;
        end
      ;

      if( destroyDirectBackTraces ) then
        begin
          priority := destrPriorityList[ prodTypeDescr + '+' + 'direct-back' ];
          result := result
            + '  <trace-destruction-model production-type="' + encodeXml( prodTypeDescr ) + '" production-type-id="' + intToStr( productionTypeID ) + '"'
            + ' contact-type="direct" direction="in">' + endl
            + '    <priority>' + intToStr( priority ) + '</priority>' + endl
            + '  </trace-destruction-model>' + endl + endl
          ;
        end
      ;

      if( destroyIndirectBackTraces ) then
        begin
          priority := destrPriorityList[ prodTypeDescr + '+' + 'indirect-back' ];
          result := result
            + '  <trace-destruction-model production-type="' + encodeXml( prodTypeDescr ) + '" production-type-id="' + intToStr( productionTypeID ) + '"'
            + ' contact-type="indirect" direction="in">' + endl
            + '    <priority>' + intToStr( priority ) + '</priority>' + endl
            + '  </trace-destruction-model>' + endl + endl
          ;
        end
      ;
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
      dbcout( '---------DESTRUCTION PARAMETERS for ' + _prodTypeDescr + endl, true );

      dbcout( 'Destroy detected units: ' + usBoolToText( destroyDetectedUnits ) , true );
      dbcout( 'Destroy direct fwd traces: ' + usBoolToText( destroyDirectForwardTraces ) , true );
      dbcout( 'Destroy indirect fwd traces: ' + usBoolToText( destroyIndirectForwardTraces ) , true );
      dbcout( 'Destroy direct back traces: ' + usBoolToText( destroyDirectBackTraces ) , true );
      dbcout( 'Destroy indirect back traces: ' + usBoolToText( destroyIndirectBackTraces ) , true );
      dbcout( 'Destruction priority: ' + intToStr( destrPriority ), true );

      if( isRingTrigger ) then
      	begin
        	dbcout( 'Ring trigger: TRUE' + endl, true );
        	dbcout( 'Ring destruction: radius ' + usFloatToStr( ringRadius ) + endl, true );
        end
      else
      	dbcout( 'Ring trigger: FALSE' + endl, true )
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
        useTraceDestruction
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

  // For destruction of traced units
  procedure TDestructionParams.setDestroyDirectForwardTraces( val: boolean );
    begin
      _destroyDirectForwardTraces := val;
      _updated := true;
    end
  ;

  function TDestructionParams.getDestroyDirectForwardTraces(): boolean; begin result := _destroyDirectForwardTraces; end;

  procedure TDestructionParams.setDestroyIndirectForwardTraces( val: boolean );
    begin
      _destroyIndirectForwardTraces := val;
      _updated := true;
    end
  ;

  function TDestructionParams.getDestroyIndirectForwardTraces(): boolean; begin result := _destroyIndirectForwardTraces; end;

  procedure TDestructionParams.setDestroyDirectBackTraces( val: boolean );
    begin
      _destroyDirectBackTraces := val;
      _updated := true;
    end
  ;

  function TDestructionParams.getDestroyDirectBackTraces(): boolean; begin result := _destroyDirectBackTraces; end;

  procedure TDestructionParams.setDestroyIndirectBackTraces( val: boolean );
    begin
      _destroyIndirectBackTraces := val;
      _updated := true;
    end
  ;

  function TDestructionParams.getDestroyIndirectBackTraces(): boolean; begin result := _destroyIndirectBackTraces; end;


  function TDestructionParams.getUseTraceDestruction(): boolean;
    begin
      result := (
          destroyDirectForwardTraces
        or
          destroyIndirectForwardTraces
        or
          destroyDirectBackTraces
        or
          destroyIndirectBackTraces
      );
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



//-----------------------------------------------------------------------------
// XML import
//-----------------------------------------------------------------------------
  class function TDestructionParams.createXmlModelList(): TQStringList;
    begin
      result := TQStringList.create();
      result.Append( 'basic-destruction-model' );
      result.Append( 'trace-destruction-model' );
      result.Append( 'ring-destruction-model' );
    end
  ;


  function TDestructionParams.getXmlModelList(): TQStringList;
    begin
      if( nil = _xmlModelList ) then
        _xmlModelList := createXmlModelList()
      ;

      result := _xmlModelList;
    end
  ;


  procedure TDestructionParams.importXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
    begin
      if( 'basic-destruction-model' = sdew.GetElementName( model ) ) then
        importBasicDestructionModelXml( model, sdew, errMsg )
      ;
      if( 'trace-destruction-model' = sdew.GetElementName( model ) ) then
        importTraceDestructionXml( model, sdew, errMsg )
      ;
      if( 'ring-destruction-model' = sdew.GetElementName( model ) ) then
        importRingDestructionXml( model, sdew, errMsg )
      ;
    end
  ;


  procedure TDestructionParams.importBasicDestructionModelXml( model: pointer; sdew: TSdew; errMsg: pstring );
    var
      priority: integer;
      smSim: TSMSimulationInput;
    begin
      smSim := sim as TSMSimulationInput;
      
      if ( nil = sdew.GetElementByName( model, 'priority' ) ) then
        begin
          appendToPstring( errMsg, tr( 'Priority is missing from basic destruction XML.' ) );
          exit;
        end
      else
        begin
          priority := myStrToInt( Sdew.GetElementContents( Sdew.GetElementByName( model, 'priority' ) ), -1 );
          if( -1 = priority ) then
            begin
              appendToPstring( errMsg, tr( 'Priority is missing from basic destruction XML.' ) );
              exit;
            end
          ;

          self.destrPriority := priority;
          self.destroyDetectedUnits := true;

          if( smSim.controlParams.ssDestrPriorities.contains( self.prodTypeDescr + '+' + 'basic' ) ) then
            begin
              if( priority < smSim.controlParams.ssDestrPriorities.Item[self.prodTypeDescr + '+' + 'basic'] ) then
                smSim.controlParams.ssDestrPriorities.Item[self.prodTypeDescr + '+' + 'basic'] := priority
              ;
            end
          else
            smSim.controlParams.ssDestrPriorities.Item[self.prodTypeDescr + '+' + 'basic'] := priority
          ;
        end
      ;
    end
  ;


  procedure TDestructionParams.importTraceDestructionXml( model: pointer; sdew: TSdew; errMsg: pstring );
    var
      contactType: string;
      contactDirection: string;
      priority: integer;
      smSim: TSMSimulationInput;
      destString: string;
    begin
      smSim := sim as TSMSimulationInput;

      contactType :=  Sdew.GetElementAttribute( model, 'contact-type' );
      contactDirection :=  Sdew.GetElementAttribute( model, 'direction' );

      if( ( 'direct' <> contactType ) and ( 'indirect' <> contactType ) ) then
        begin
          appendToPstring( errMsg, tr( 'Trace destruction XML includes an invalid contact type.' ) );
          exit;
        end
      ;

      if( ( 'in' <> contactDirection )  and ( 'out' <> contactDirection ) ) then
        begin
          appendToPstring( errMsg, tr( 'Trace destruction XML includes an invalid contact direction.' ) );
          exit;
        end
      ;

      if ( nil = sdew.GetElementByName( model, 'priority' ) ) then
        begin
          appendToPstring( errMsg, tr( 'Priority is missing from trace destruction XML.' ) );
          exit;
        end
      else
        begin
          priority := myStrToInt( Sdew.GetElementContents( Sdew.GetElementByName( model, 'priority' ) ), -1 );
          if( -1 = priority ) then
            begin
              appendToPstring( errMsg, tr( 'Priority is missing from trace destruction XML.' ) );
              exit;
            end
          ;
        end
      ;

      if( 'direct' = contactType ) then
        begin
          if( 'in' = contactDirection ) then
            begin
              self.destroyDirectBackTraces := true;
              destString := 'direct-back';
            end
          else if( 'out' = contactDirection ) then
            begin
              self.destroyDirectForwardTraces := true;
              destString := 'direct-forward';
            end
          else
            raise exception.create( 'Someone forgot something in TDestructionParams.importTraceDestructionXml' )
          ;
        end
      else if( 'indirect' = contactType ) then
        begin
          if( contactDirection = 'in' ) then
            begin
              self.destroyIndirectBackTraces := true;
              destString := 'indirect-back';
            end
          else if( contactDirection = 'out' ) then
            begin
              self.destroyIndirectForwardTraces := true;
              destString := 'indirect-forward';
            end
          else
            raise exception.create( 'Someone forgot something in TDestructionParams.importTraceDestructionXml' )
          ;
        end
      ;

      if( smSim.controlParams.ssDestrPriorities.contains( self.prodTypeDescr + '+' + destString ) ) then
        begin
          if( priority < smSim.controlParams.ssDestrPriorities.Item[self.prodTypeDescr + '+' + destString] ) then
            smSim.controlParams.ssDestrPriorities.Item[self.prodTypeDescr + '+' + destString] := priority
          ;
        end
      else
        smSim.controlParams.ssDestrPriorities.Item[self.prodTypeDescr + '+' + destString] := priority
      ;
    end
  ;


  procedure TDestructionParams.importRingDestructionXml( model: pointer; sdew: TSdew; errMsg: pstring );
    var
      value: double;
      subElement: pointer;
      priority: integer;

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

          self.isRingTrigger := true;

          // Set the destruction priority for the from-type to bogus value so that the populateDatabase() function won't
          // throw an exception.  The from-type doesn't need a priority here, but the populate function doesn't know that.

          if( not( smSim.controlParams.ssDestrPriorities.contains( self.prodTypeDescr + '+' + 'ring' ) ) ) then
             smSim.controlParams.ssDestrPriorities.Add( self.prodTypeDescr + '+' + 'ring', -1 )
          ;
        end
      ;

      // Parse this production type as the "destination" type.
      //------------------------------------------------------

      if( self.prodTypeDescr = sdew.GetElementAttribute( model, 'to-production-type' ) ) then
        begin
          ok:= true;
          //Priorities go with the "to type", i.e. destination.
          if( nil = sdew.GetElementByName( model, 'priority' ) ) then
            begin
              appendToPstring( errMsg, tr( 'Priority is missing from ring destruction XML.' ) );
              exit;
            end
          else
            begin
              priority := myStrToInt( Sdew.GetElementContents( Sdew.GetElementByName( model, 'priority' ) ), -1 );
              if( -1 = priority ) then
                begin
                  appendToPstring( errMsg, tr( 'Priority is missing from ring destruction XML.' ) );
                  exit;
                end
              ;

              self.isRingTarget := true;

              if( smSim.controlParams.ssDestrPriorities.contains( self.prodTypeDescr + '+' + 'ring' ) ) then
                begin
                  if ( smSim.controlParams.ssDestrPriorities.Item[self.prodTypeDescr + '+' + 'ring'] > priority ) then
                    smSim.controlParams.ssDestrPriorities.Item[self.prodTypeDescr + '+' + 'ring'] := priority
                  ;
                end
              else
                smSim.controlParams.ssDestrPriorities.Item[self.prodTypeDescr + '+' + 'ring'] := priority
              ;
            end
          ;
        end
      ;

      if not ok then raise exception.Create( 'Unhandled GetElementAttribute value in TDestructionParams.importRingDestructionXml()' );
    end
  ;
//-----------------------------------------------------------------------------

end.
