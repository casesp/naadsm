unit DetectionParams;

(*
DetectionParams.pas
--------------------
Begin: 2005/06/03
Last revision: $Date: 2008/11/25 22:00:58 $ $Author: areeves $
Version number: $Revision: 1.33 $
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
    ChartFunction,
    RelFunction,
    Models,
    SMDatabase,
    FunctionEnums
  ;

  type TDetectionParams = class( TModelWithFunctions )
    protected
    	// Properties
      _prodTypeDescr: string;

      _relObsVsTimeClinicalName: string;
      _relReportVsFirstDetectionName: string;

      // For internal use
      procedure initialize();

      // Properties
      procedure setProdTypeDescr( val: string );
      function getProdTypeDescr(): string;

      procedure setRelObsVsTimeClinicalName( val: string );
      procedure setRelReportVsFirstDetectionName( val: string );

      function getRelObsVsTimeClinicalName(): string;
      function getRelReportVsFirstDetectionName(): string;

      function getRelObsVsTimeClinical(): TRelFunction;
      function getRelReportVsFirstDetection(): TRelFunction;


      // Overridden from TModel
      //-----------------------
      function getUpdated(): boolean; override;

    public
      constructor create( sim: TObject; prodTypeName: string ); overload;
      constructor create( prodTypeName: string ); overload;
      constructor create( const src: TDetectionParams; sim: TObject ); overload;
      destructor destroy(); override;

      procedure initializeFromDB( db: TSMDatabase; ptID: integer; prodTypeName: string );

      // Overridden from TModel
      //-----------------------
      procedure debug(); override;
			function validate( err: PString = nil ): boolean; override;
      function ssXML(  const productionTypeID: integer ): string; reintroduce;
      function populateDatabase( db: TSMDatabase; ptID: integer ): integer; reintroduce;
      function functionsAreValid(): boolean; override;
      
      // Properties
      //-----------
      property prodTypeDescr: string read getProdTypeDescr write setProdTypeDescr;

      property relObsVsTimeClinicalName: string read getRelObsVsTimeClinicalName write setRelObsVsTimeClinicalName;
      property relReportVsFirstDetectionName: string read getRelReportVsFirstDetectionName write setRelReportVsFirstDetectionName;

      property relObsVsTimeClinical: TRelFunction read getRelObsVsTimeClinical;
      property relReportVsFirstDetection: TRelFunction read getRelReportVsFirstDetection;
    end
  ;



implementation

  uses
    SysUtils,
    Variants,

    MyStrUtils,
    USStrUtils,
    DebugWindow,
    SqlClasses,
    I88n,

    FunctionDictionary
  ;

  const DBSHOWMSG = false; // Set to true to enable debugging messages for this unit

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TDetectionParams.create( sim: TObject; prodTypeName: string );
    begin
      inherited create();
      initialize();

      _prodTypeDescr := prodTypeName;
      _sim := sim;
    end
  ;


  constructor TDetectionParams.create( prodTypeName: string );
    begin
    	inherited create();
      initialize();
      prodTypeDescr := prodTypeName;

      _updated := false;
    end
  ;


  procedure TDetectionParams.initializeFromDB( db: TSMDatabase; ptID: integer; prodTypeName: string  );
    var
      q: string;
      db2: TSqlDatabase;
      res: TSqlResult;
      row: TSqlRow;
  	begin
      db2 := db as TSqlDatabase;

      q := 'SELECT'
        + ' inProductionType.detProbObsVsTimeClinicalRelID,'
        + ' probVsTimeClinicalChart.chartName as probVsTimeClinicalName,'
        + ' inProductionType.detProbReportVsFirstDetectionRelID,'
        + ' probVsFirstDetectionChart.chartName as probVsFirstDetectionName'
        + ' FROM'
        + ' ( ('
        + ' inProductionType'
        + ' LEFT OUTER JOIN inChart probVsTimeClinicalChart'
        + ' ON probVsTimeClinicalChart.chartID = inProductionType.detProbObsVsTimeClinicalRelID'
        + ' )'
        + ' LEFT OUTER JOIN inChart probVsFirstDetectionChart'
        + ' ON probVsFirstDetectionChart.chartID = inProductionType.detProbReportVsFirstDetectionRelID'
        + ' ) '
        + ' WHERE inProductionType.productionTypeID = ' + intToStr( ptID )
      ;


      res := TSqlResult.create( q, db2 );
      row := res.fetchArrayFirst();

      if( null <> row.field('detProbObsVsTimeClinicalRelID') ) then
        begin
      	  setrelObsVsTimeClinicalName( row.field( 'probVsTimeClinicalName' ) );
        end
      ;

      if( null <> row.field('detProbReportVsFirstDetectionRelID') ) then
        begin
      	  setrelReportVsFirstDetectionName( row.field( 'probVsFirstDetectionName' ) );
        end
      ;

      prodTypeDescr := prodTypeName;

      _updated := false;

      freeAndNil( res );
    end
  ;


  constructor TDetectionParams.create( const src: TDetectionParams; sim: TObject );
    begin
    	inherited create( src );
      _sim := sim;

      _prodTypeDescr := src._prodTypeDescr;

      _relObsVsTimeClinicalName := src._relObsVsTimeClinicalName;
      _relReportVsFirstDetectionName := src._relReportVsFirstDetectionName;

      _updated := src._updated;
    end
  ;


  procedure TDetectionParams.initialize();
  	begin
      _prodTypeDescr := '';
      _updated := false;

      _relObsVsTimeClinicalName := '';
      _relReportVsFirstDetectionName := '';
    end
  ;


  destructor TDetectionParams.destroy();
    begin
      dbcout( '*** TDetectionParams.destroy() for "' + _prodTypeDescr + '"', DBSHOWMSG );
      // The function dictionary is freed elsewhere.
      // Functions are handled by the function dictionary:
      // don't free them here, but do decrement their counters.
      if( nil <> fnDictionary ) then
        begin
          if( fnDictionary.contains( relObsVsTimeClinicalName ) ) then
            fnDictionary.value( relObsVsTimeClinicalName ).decrRefCounter()
          ;

          if( fnDictionary.contains( relReportVsFirstDetectionName ) ) then
            fnDictionary.value( relReportVsFirstDetectionName ).decrRefCounter()
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
  function TDetectionParams.populateDatabase( db: TSMDatabase; ptID: integer ): integer;
  	var
    	q: string;
      dict: TQueryDictionary;
  	begin
      dict := TQueryDictionary.create();

      if( nil <> relObsVsTimeClinical ) then
      	dict['detProbObsVsTimeClinicalRelID'] := intToStr( relObsVsTimeClinical.id )
      else
      	dict['detProbObsVsTimeClinicalRelID'] := DATABASE_NULL_VALUE
      ;

      if( nil <> relReportVsFirstDetection ) then
      	dict['detProbReportVsFirstDetectionRelID'] := intToStr( relReportVsFirstDetection.id )
      else
     		dict['detProbReportVsFirstDetectionRelID'] := DATABASE_NULL_VALUE
      ;

      //Assuming the presence of this object instance indicates the desire to use detection.....
      dict['useDetection'] := boolToStr( true );

      q := writeQuery(
        'inProductionType',
        QUpdate,
        dict,
        'WHERE [productionTypeID] = ' + intToStr( ptID )
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
  function TDetectionParams.ssXML( const productionTypeID: integer ): string;
  	var
    	str: string;
    begin
      str := str + '  <detection-model production-type="' + encodeXml( prodTypeDescr ) + '" production-type-id="' + intToStr( productionTypeID ) + '">' + endl;

      str := str + '    <prob-report-vs-time-clinical>' + endl;
      str := str + relObsVsTimeClinical.ssXml( 3 );
      str := str + '    </prob-report-vs-time-clinical>' + endl;

      str := str + '    <prob-report-vs-time-since-outbreak>' + endl;
      str := str + relReportVsFirstDetection.ssXml( 3 );
      str := str + '    </prob-report-vs-time-since-outbreak>' + endl;

      str := str + '  </detection-model>' + endl;

      result := str;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Data validation
//-----------------------------------------------------------------------------
  function TDetectionParams.validate( err: PString = nil ): boolean;
  	var
    	msg: string;
    	submsg: string;
  	begin
    	result := true;
			msg := '';

      submsg := '';
      if( nil = relObsVsTimeClinical ) then
      	begin
       		if( nil <> err ) then msg := msg + '  ' + tr( 'Probability of observing clinical signs versus time clinical is not set.' ) + endl;
          result := false;
        end
      else if( not( relObsVsTimeClinical.validate( @submsg) ) ) then
      	begin
					if( nil <> err ) then msg := msg + '  ' + tr( 'Probability of observing clinical signs versus time clinical is not valid:' ) + ' ' + submsg + endl;
          result := false;
        end
      (*
      // I think this one actually can start at 0...
      else if( relObsVsTimeClinical.yStartsAtZero ) then
        begin
					if( nil <> err ) then msg := msg + '  Probability of observing clinical signs versus time clinical is not valid: detection will not occur if the baseline probability is 0.' + endl;
          result := false;
        end
      *)
      ;

      submsg := '';
      if( nil = relReportVsFirstDetection ) then
      	begin
       		if( nil <> err ) then msg := msg + '  ' + tr( 'Probability of reporting versus time since first detection is not set.' ) + endl;
          result := false;
        end
      else if( not( relReportVsFirstDetection.validate( @submsg) ) ) then
      	begin
					if( nil <> err ) then msg := msg + '  ' + tr( 'Probability of reporting versus time since first detection is not valid:' ) + ' ' + submsg + endl;
          result := false;
      	end
      else if( relReportVsFirstDetection.yStartsAtZero ) then
        begin
					if( nil <> err ) then msg := msg + '  ' + tr( 'Probability of reporting versus time since first detection is not valid: detection will not occur if the baseline probability is 0.' ) + endl;
          result := false;
        end
      ;

      if( ( result = false ) and ( nil <> err ) ) then
      	err^ := err^ + msg
      ;

    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Debugging
//-----------------------------------------------------------------------------
  procedure TDetectionParams.debug();
    begin
      dbcout( '##DETECTION PARAMS', true );

      if( nil <> relObsVsTimeClinical ) then
      	begin
          dbcout( 'ProbObsVsTimeClinical:', true );
          relObsVsTimeClinical.debug();
        end
      else
      	dbcout( 'ProbObsVsTimeClinical IS UNDEFINED', true )
      ;

      if( nil <> relReportVsFirstDetection ) then
      	begin
        	dbcout( 'ProbReportVsFirstDetection:', true );
      		relReportVsFirstDetection.debug();
        end
      else
      	dbcout( 'ProbReportVsFirstDetection IS UNDEFINED', true )
      ;
      
      dbcout( '##END DETECTION PARAMS', true );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
  procedure TDetectionParams.setProdTypeDescr( val: string ); begin _prodTypeDescr := val; end;

  function TDetectionParams.getProdTypeDescr(): string;
    begin
      if( 0 = length( _prodTypeDescr ) ) then
        raise exception.Create( 'TDetectionParams._prodTypeDescr is not set' )
      ;
      result := _prodTypeDescr;
    end
  ;

  // FIX ME!
  function TDetectionParams.getUpdated(): boolean; begin result := _updated; end;

  procedure TDetectionParams.setRelObsVsTimeClinicalName( val: string );
    begin
      val := trim( val );
      _relObsVsTimeClinicalName := val;

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


  procedure TDetectionParams.setRelReportVsFirstDetectionName( val: string );
    begin
      val := trim( val );
      _relReportVsFirstDetectionName := val;

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


  function TDetectionParams.getRelObsVsTimeClinicalName(): string; begin result := _relObsVsTimeClinicalName; end;
  function TDetectionParams.getRelReportVsFirstDetectionName(): string; begin result := _relReportVsFirstDetectionName; end;


  function TDetectionParams.getRelObsVsTimeClinical(): TRelFunction;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _relObsVsTimeClinicalName ) ) then
            begin
              if( fnDictionary.value( _relObsVsTimeClinicalName ).fn is TRelFunction ) then
                result := fnDictionary.value( _relObsVsTimeClinicalName ).fn as TRelFunction
              else
                begin
                  setRelObsVsTimeClinicalName( '' );
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


  function TDetectionParams.getRelReportVsFirstDetection(): TRelFunction;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _relReportVsFirstDetectionName ) ) then
            begin
              if( fnDictionary.value( _relReportVsFirstDetectionName ).fn is TRelFunction ) then
                result := fnDictionary.value( _relReportVsFirstDetectionName ).fn as TRelFunction
              else
                begin
                  setRelReportVsFirstDetectionName( '' );
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


  function TDetectionParams.functionsAreValid(): boolean;
    begin
      result := true;

      if( fnDictionary.contains( _relObsVsTimeClinicalName ) ) then
        begin
          if( not( fnDictionary.value( _relObsVsTimeClinicalName ).fn is TRelFunction ) ) then
            begin
              setRelObsVsTimeClinicalName( '' );
              result := false;
            end
          ;
        end
      ;

      if( fnDictionary.contains( _relReportVsFirstDetectionName ) ) then
        begin
          if( not( fnDictionary.value( _relReportVsFirstDetectionName ).fn is TRelFunction ) ) then
            begin
              setRelReportVsFirstDetectionName( '' );
              result := false;
            end
          ;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------

end.
