unit SMZoneStats;

(*
SMZoneStats.pas
----------------
Begin: 2007/04/19
Last revision: $Date: 2008/03/12 22:10:54 $ $Author: areeves $
Version number: $Revision: 1.5 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2007 - 2008 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)


interface

  uses
    SqlClasses,

    QIntegerMaps,
    QOrderedDictionaries,

    SMSimulationStats,
    OutputDescriptions
  ;
  
  
  {* A QMap
    The integer keys are members of TIterationOutputVariable as shown in OutputDescriptions.
    The values are instances of TSimulationResultSet.
  }
  type TVariableOutputSet = class( TQIntegerObjectMap )
    protected
      _startType, _endType: TIterationOutputVariable;

      procedure initialize( const statsType: TStatsType );
      procedure generateCalculatedVariables( const statsType: TStatsType );

    public
      constructor create( const statsType: TStatsType ); overload;
      constructor create( db: TSqlDatabase; const statsType: TStatsType; const prodTypeID: integer; const zoneID: integer = -1 ); overload;
      destructor destroy(); override;
    
      function resultSet( const outputVariable: TIterationOutputVariable ): TSimulationResultSet;

      function valueAsString( arrPos, calcPos: integer ): string;

      procedure debug();
    end
   ;
  
  
  {* Another QMap 
    The integer keys are production type IDs, where 0 indicates all production types.
    The values are instances of TVariableOutputSet.
    
    Epi and cost outputs have this level of organization.
  }
  type TProductionTypeOutputSet = class( TQIntegerObjectMap )
    protected
      _startType, _endType: TIterationOutputVariable;

      procedure initialize( const statsType: TStatsType );

    public
      constructor create( const statsType: TStatsType ); overload;
      constructor create( db: TSqlDatabase; const statsType: TStatsType; const zoneID: integer = -1 ); overload;
      destructor destroy(); override;

      class function costsOutputCount(): integer;
      class function createCostsOutputDictionary(): TQOrderedStringStringDictionary;

      function item( const prodTypeID: integer ): TVariableOutputSet;
      function resultSet( const outputVariable: TIterationOutputVariable; const prodTypeID: integer ): TSimulationResultSet;

      procedure debug();
    end
  ;
  
  
  {* Yet another QMap
    The integer keys are zone IDs, where ID 0 indicates all zones.
    The values are instances of TProductionTypeOutputSet.
    
    Zone outputs have this level of organization.
  }
  type TZoneOutputSet = class( TQIntegerObjectMap )
    protected
      _startType, _endType: TIterationOutputVariable;

      procedure initialize( const statsType: TStatsType );

    public
      constructor create( db: TSqlDatabase; const statsType: TStatsType );
      destructor destroy(); override;

      function item( const zoneID: integer ): TProductionTypeOutputSet;
      function resultSet( const outputVariable: TIterationOutputVariable; const prodTypeID, zoneID: integer ): TSimulationResultSet;

      class function zoneOutputCount(): integer;
      class function createZoneOutputDictionary(): TQOrderedStringStringDictionary;

      procedure debug();
    end
  ;



  type TScenarioOutputSet = class
    protected
      _zoneStats: TZoneOutputSet;
      _costStats: TProductionTypeOutputSet;
      //_epiStats: TProductionTypeOutputSet;
    public
      constructor create( db: TSqlDatabase; const costsUsed, zonesUsed: boolean );
      destructor destroy(); override;

      property zoneStats: TZoneOutputSet read _zoneStats;
      property costStats: TProductionTypeOutputSet read _costStats;
      //property epiStats: TProductionTypeOutputSet read _epiStats;
      
    end
  ;


implementation

  uses
    Math,
    SysUtils,
    Variants,
    StrUtils,
    
    QVectors,

    DebugWindow,
    MyStrUtils,
    GuiStrUtils
  ;

//-----------------------------------------------------------------------------
// TVariableOutputSet
//-----------------------------------------------------------------------------
  constructor TVariableOutputSet.create( const statsType: TStatsType );
    begin
      inherited create();
      initialize( statsType );
    end
  ;

  
  constructor TVariableOutputSet.create( db: TSqlDatabase; const statsType: TStatsType; const prodTypeID: integer; const zoneID: integer = -1 );
    var
      res: TSqlResult;
      row: TSqlRow;
      query: string;

      val: variant;

      i: TIterationOutputVariable;
      //adjustment: integer;
    begin
      inherited create();
      initialize( statsType );

      query := '';
      for i := _startType to _endType do
        begin
          if( not( outputDescriptionList.item(i).isCalculated ) ) then
            query := query + '`' + outputDescriptionList.item( i ).name + '`, '
          ;
        end
      ;
      
      // Strip off the extra ", "
      query := leftStr( query, length( query ) - 2 );

      case statsType of
        StatsZones:
          begin
            query := 'SELECT ' + query + ' FROM `outIterationByZoneAndProductionType`';
            //adjustment := 0;
          end
        ;
        StatsCosts:
          begin
            query := 'SELECT ' + query + ' FROM `outIterationCosts`';
            //adjustment := foo;
          end
        ;
      end;

      query := query + ' WHERE `productionTypeID` = ' + intToStr( prodTypeID );
      if( 0 < zoneID ) then
        query := query + ' AND `zoneID` = ' + intToStr( zoneID )
      ;
      query := query + ' ORDER BY iteration';

      res := TSqlResult.create( query, db );

      row := res.fetchArrayFirst();
      while( nil <> row ) do
        begin
          for i := _startType to _endType do
            begin
              if( not( outputDescriptionList.item(i).isCalculated ) ) then
                begin
                  val := row.field( outputDescriptionList.item(i).name );
                  if( null <> val ) then self.resultSet( i ).values.Append( val );
                end
              ;
            end
          ;
          row := res.fetchArrayNext();
        end
      ;

      res.free();


      generateCalculatedVariables( statsType );
    end
  ;


  procedure TVariableOutputSet.initialize( const statsType: TStatsType );
    var
      results: TSimulationResultSet;
      i: TIterationOutputVariable;
      d: TOutputDescription;
    begin
      case statsType of
        StatsZones:
          begin
            _startType := ZUnitDaysInZone;
            _endType := ZCostSurveillance;
          end
        ;
        StatsCosts:
          begin
            _startType := CDestrAppraisal;
            _endType := CCostsTotal;
          end
        ;
      end;

      for i := _startType to _endType do
        begin
          d := outputDescriptionList.item( i );
          results := TSimulationResultSet.create( d.name, d.descr );
          self.Add( integer( i ), results );
        end
      ;
    end
  ;


  destructor TVariableOutputSet.destroy();
    begin
      self.deleteValues();
      inherited destroy();
    end
  ;


  procedure TVariableOutputSet.generateCalculatedVariables( const statsType: TStatsType );
    var
      destrLen, vaccLen, len: integer;
      i: integer;
    begin
      len := 0;

      case statsType of
        StatsZones: { Do nothing: there are no calculated variables. };
        StatsCosts:
          begin
            vaccLen := self.resultSet( CVaccSetup ).values.count;
            if( 0 < vaccLen ) then
              begin
                len := vaccLen;

                self.resultSet( CVaccSubtotal ).values.resize( vaccLen );
                self.resultSet( CVaccSubtotal ).values.fill( 0.0 );

                for i := 0 to vaccLen - 1 do
                  begin
                    self.resultSet( CVaccSubtotal ).values[i] :=
                      self.resultSet( CVaccSetup ).values[i]
                      + self.resultSet( CVaccVaccination ).values[i]
                    ;
                  end
                ;
              end
            ;

            destrLen := self.resultSet( CDestrAppraisal ).values.count;
            if( 0 < destrLen ) then
              begin
                len := destrLen;

                self.resultSet( CDestrSubtotal ).values.resize( destrLen );
                self.resultSet( CDestrSubtotal ).values.fill( 0.0 );

                for i := 0 to destrLen - 1 do
                  begin
                    self.resultSet( CDestrSubtotal ).values[i] :=
                      self.resultSet( CDestrAppraisal ).values[i]
                      + self.resultSet( CDestrCleaning ).values[i]
                      + self.resultSet( CDestrEuthanasia ).values[i]
                      + self.resultSet( CDestrIndemnification ).values[i]
                      + self.resultSet( CDestrDisposal ).values[i]
                    ;
                  end
                ;
              end
            ;

            if(  0 < len ) then
              begin
                self.resultSet( CCostsTotal ).values.resize( len );
                self.resultSet( CCostsTotal ).values.fill( 0.0 );

                for i := 0 to len - 1 do
                  begin
                    if( 0 < vaccLen ) then
                      self.resultSet( CCostsTotal ).values[i] := self.resultSet( CCostsTotal ).values[i] + self.resultSet( CVaccSubtotal ).values[i]
                    ;
                    if( 0 < destrLen ) then
                      self.resultSet( CCostsTotal ).values[i] := self.resultSet( CCostsTotal ).values[i] + self.resultSet( CDestrSubtotal ).values[i]
                    ;
                  end
                ;
              end
            ;
          end
        ;
      end;
    end
  ;


  function TVariableOutputSet.resultSet( const outputVariable: TIterationOutputVariable ): TSimulationResultSet;
    begin
      result := value( integer( outputVariable ) ) as TSimulationResultSet;
    end
  ;


  function TVariableOutputSet.valueAsString( arrPos, calcPos: integer ): string;
    var
      d: double;
      rs: TSimulationResultSet;
    begin
      rs := resultSet( TIterationOutputVariable( arrPos ) );

      if( nil <> rs ) then
        begin
          d := rs.calculationAtPosition( calcPos );
          if( isNaN( d ) ) then
            result := 'n/a'
          else
            result := uiFloatToStr( d, 2 )
          ;
        end
      else
        result := 'n/a'
      ;
    end
  ;


  procedure TVariableOutputSet.debug();
    var
      it: TQIntegerObjectMapIterator;
      rs: TSimulationResultSet;
    begin
      it := TQIntegerObjectMapIterator.create( self );

      repeat
        if( nil <> it.value() ) then
          begin
            rs := it.value() as TSimulationResultSet;

            dbcout( endl + 'TVariableOutputSet debug: index ' + intToStr( it.key() ), true );
            rs.debug();
            dbcout( 'Done TVariableOutputSet debug' + endl, true );

            it.incr();
          end
        ;
      until ( nil = it.value() );

      it.Free();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TProductionTypeOutputSet
//-----------------------------------------------------------------------------
  constructor TProductionTypeOutputSet.create( const statsType: TStatsType );
    begin
      inherited create();
      initialize( statsType );
    end
  ;


  constructor TProductionTypeOutputSet.create( db: TSqlDatabase; const statsType: TStatsType; const zoneID: integer = -1 );
    var
      olSet: TVariableOutputSet;
      res: TSqlResult;
      row: TSqlRow;
      query: string;

      prodTypeID: integer;

      it: TQIntegerObjectMapIterator;
      ol: TVariableOutputSet;
      j: integer;
      
      k: TIterationOutputVariable;
    begin
      inherited create();
      initialize( statsType );

      query := 'SELECT `productionTypeID` FROM `inProductionType`';

      res := TSqlResult.create( query, db );

      row := res.fetchArrayFirst();
      while( nil <> row ) do
        begin
          // For each production type ID and zone ID (if assigned), create an instance of TVariableOutputSet
          // and add it to this map.
          prodTypeID := row.field('productionTypeID');

          olSet := TVariableOutputSet.create( db, statsType, prodTypeID, zoneID );
          self.Add( prodTypeID, olSet );

          row := res.fetchArrayNext();
        end
      ;
      res.free();

      // Create the special case for production type ID 0
      olSet := TVariableOutputSet.create( statsType );

      it := TQIntegerObjectMapIterator.create( self );

      if( nil <> it.value() ) then
        begin
          repeat
            ol := (it.value() as TVariableOutputSet);

            for k := _startType to _endType do
              begin
                if
                  ( 0 = olSet.resultSet( k ).values.count )
                and
                  ( 0 < ol.resultSet( k ).values.count )
                then
                  begin
                    olSet.resultSet( k ).values.resize( ol.resultSet( k ).values.count );
                    olSet.resultSet( k ).values.Fill( 0.0 );
                  end
                ;

                for j := 0 to ol.resultSet( k ).values.count - 1 do
                  olSet.resultSet( k ).values[j] := olSet.resultSet( k ).values[j] + ol.resultSet( k ).values[j]
                ;
              end
            ;

            it.incr();
          until ( nil = it.value() );

        end
      ;

      it.Free();

      self.Add( 0, olSet );
    end
  ;


  procedure TProductionTypeOutputSet.initialize( const statsType: TStatsType );
    begin
      case  statsType of
        StatsZones:
          begin
            _startType := ZUnitDaysInZone;
            _endType := ZCostSurveillance;
          end
        ;
        StatsCosts:
          begin
            _startType := CDestrAppraisal;
            _endType := CCostsTotal;
          end
        ;
      end;
    end
  ;


  destructor TProductionTypeOutputSet.destroy();
    begin
      self.deleteValues();
      inherited destroy();
    end
  ;


  function TProductionTypeOutputSet.item( const prodTypeID: integer ): TVariableOutputSet;
    begin
      result := value( prodTypeID ) as TVariableOutputSet;
    end
  ;


  function TProductionTypeOutputSet.resultSet( const outputVariable: TIterationOutputVariable; const prodTypeID: integer ): TSimulationResultSet;
    begin
      result := self.item( prodTypeID ).resultSet( outputVariable );
    end
  ;


  class function TProductionTypeOutputSet.costsOutputCount(): integer;
    begin
      result := 10;
    end
  ;
  

  class function TProductionTypeOutputSet.createCostsOutputDictionary(): TQOrderedStringStringDictionary;
    begin
      result := createOutputDictionary( CDestrAppraisal, CCostsTotal );
    end
  ;


  procedure TProductionTypeOutputSet.debug();
    var
      it: TQIntegerObjectMapIterator;
      ol: TVariableOutputSet;
    begin
      it := TQIntegerObjectMapIterator.create( self );

      repeat
        if( nil <> it.value() ) then
          begin
            ol := it.value() as TVariableOutputSet;

            dbcout( endl + 'TProductionTypeOutputSet debug: index ' + intToStr( it.key() ), true );
            ol.debug();
            dbcout( 'Done TProductionTypeOutputSet debug' + endl, true );

            it.incr();
          end
        ;
      until ( nil = it.value() );

      it.Free();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TZoneOutputSet
//-----------------------------------------------------------------------------
  constructor TZoneOutputSet.create( db: TSqlDatabase; const statsType: TStatsType );
    var
      ptlSet: TProductionTypeOutputSet;

      res: TSqlResult;
      row: TSqlRow;
      query: string;

      zoneID: integer;
      zoneIDVector: TQIntegerVector;

      ptIDVector: TQIntegerVector;
      pit: TQIntegerObjectMapIterator;
      pl: TProductionTypeOutputSet;

      olSet: TVariableOutputSet;
      oit: TQIntegerObjectMapIterator;
      ol: TVariableOutputSet;

      i, j, k: integer;

      m: TIterationOutputVariable;
    begin
      inherited create();
      initialize( statsType );

      zoneIDVector := TQIntegerVector.create();

      query := 'SELECT `zoneID` FROM `inZone`';

      res := TSqlResult.create( query, db );

      row := res.fetchArrayFirst();
      while( nil <> row ) do
        begin
          // For each zone ID, create an instance of TProductionTypeOutputSet
          // and add it to this map.
          zoneID := row.field('zoneID');
          zoneIDVector.append( zoneID );

          ptlSet := TProductionTypeOutputSet.create( db, statsType, zoneID );
          self.Add( zoneID, ptlSet );

          row := res.fetchArrayNext();
        end
      ;

      res.Free();

      // Create the special case for zone ID 0
      //--------------------------------------
      // Create a new pt-level output set for zone ID 0
      // Build an integer array that contains all production type keys
      // For each pt key, select the corresponding item in each pt-level output set
      // Sum these up to create a new result set, and add it to the new pt-level output set

      ptlSet := TProductionTypeOutputSet.create( statsType );

      ptIDVector := TQIntegerVector.create();

      pit := TQIntegerObjectMapIterator.create( self );
      if( nil <> pit.value() ) then
        begin
          pl := pit.value() as TProductionTypeOutputSet;

          oit := TQIntegerObjectMapIterator.create( pl );
          if( nil <> oit.value() ) then
            begin
              repeat
                ptIDVector.append( oit.key );
                oit.incr();
              until nil = oit.value();
            end
          ;
          oit.Free();
        end
      ;
      pit.Free();

      if( 0 < ptIDVector.count ) then
        begin
          for i := 0 to ptIDVector.count - 1 do
            begin
              // First, just set the sizes for the double arrays
              ol := self.item( zoneIDVector[0] ).item( ptIDVector[0] );
              olSet := TVariableOutputSet.create( statsType );

              for m := _startType to _endType do
                begin
                  olSet.resultSet( m ).values.resize( ol.resultSet( m ).values.count );
                  olSet.resultSet( m ).values.Fill( 0.0 );
                end
              ;

              // Then sum up the values...
              for j := 0 to zoneIDVector.count - 1 do
                begin
                  ol := self.item( zoneIDVector[j] ).item( ptIDVector[i] );

                  for m := _startType to _endType do
                    begin
                      for k := 0 to ol.resultSet( m ).values.count - 1 do
                        olSet.resultSet( m ).values[k] := olSet.resultSet( m ).values[k] + ol.resultSet( m ).values[k]
                      ;
                    end
                  ;
                end
              ;

              // ... and insert the new output-level result set (with summed values) into the new production type-level result set
              ptlSet.insert( ptIDVector[i], olSet );
            end
          ;
        end
      ;

      ptIDVector.Free();
      zoneIDVector.Free();

      // Finally, insert the new production-type level result set into this list, with the special zone ID of 0
      self.insert( 0, ptlSet );
    end
  ;


  procedure TZoneOutputSet.initialize( const statsType: TStatsType );
    begin
      case  statsType of
        StatsZones:
          begin
            _startType := ZUnitDaysInZone;
            _endType := ZCostSurveillance;
          end
        ;
        StatsCosts:
          begin
            _startType := CDestrAppraisal;
            _endType := CCostsTotal;
          end
        ;
      end;
    end
  ;


  destructor TZoneOutputSet.destroy();
    begin
      self.deleteValues();
      inherited destroy();
    end
  ;


  function TZoneOutputSet.item( const zoneID: integer ): TProductionTypeOutputSet;
    begin
      result := value( zoneID ) as TProductionTypeOutputSet;
    end
  ;

  
  function TZoneOutputSet.resultSet( const outputVariable: TIterationOutputVariable; const prodTypeID, zoneID: integer ): TSimulationResultSet;
    begin
      result := self.item( zoneID ).item( prodTypeID ).resultSet( outputVariable );
    end
  ;


  class function TZoneOutputSet.zoneOutputCount(): integer;
    begin
      result := 3;
    end
  ;


  class function TZoneOutputSet.createZoneOutputDictionary(): TQOrderedStringStringDictionary;
    begin
      result := createOutputDictionary( ZUnitDaysInZone, ZCostSurveillance );
    end
  ;


  procedure TZoneOutputSet.debug();
    var
      it: TQIntegerObjectMapIterator;
      pl: TProductionTypeOutputSet;
    begin
      it := TQIntegerObjectMapIterator.create( self );

      repeat
        if( nil <> it.value() ) then
          begin
            pl := it.value() as TProductionTypeOutputSet;

            dbcout( endl + 'TZoneOutputSet debug: index ' + intToStr( it.key() ), true );
            pl.debug();
            dbcout( 'Done TZoneOutputSet debug' + endl, true );

            it.incr();
          end
        ;
      until ( nil = it.value() );

      it.Free();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TScenarioOutputSet
//-----------------------------------------------------------------------------
  constructor TScenarioOutputSet.create( db: TSqlDatabase; const costsUsed, zonesUsed: boolean );
    begin
      inherited create();

      _zoneStats := nil;
      _costStats := nil;
      //_epiStats := nil;

      if( zonesUsed ) then
        _zoneStats := TZoneOutputSet.create( db, StatsZones )
      ;

      if( costsUsed ) then
        _costStats := TProductionTypeOutputSet.create( db, StatsCosts )
      ;

      //_epiStats := TProductionTypeOutputSet.create( db, StatsEpi );
    end
  ;


  destructor TScenarioOutputSet.destroy();
    begin
      if( nil <> _zoneStats ) then
        _zoneStats.Free()
      ;
      if( nil <> _costStats ) then
        _costStats.Free()
      ;

      //_epiStats.free();

      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------

end.