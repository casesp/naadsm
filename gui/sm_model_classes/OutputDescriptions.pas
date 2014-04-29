unit OutputDescriptions;

(*
OutputDescriptions.pas
----------------------
Begin: 2007/04/20
Last revision: $Date: 2008/10/22 19:27:49 $ $Author: areeves $
Version number: $Revision: 1.7 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2007 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)


interface

  uses
    SysUtils,
    
    QIntegerMaps,
    QOrderedDictionaries
  ;

  type TStatsType = (
    StatsUnspecified,
    StatsEpi,
    StatsCosts,
    StatsZones
  );

  type TIterationOutputVariable = (
    // Zones
    ZUnitDaysInZone,
    ZAnimalDaysInZone,
    ZCostSurveillance,

    // Costs
    CDestrAppraisal,
    CDestrCleaning,
    CDestrEuthanasia,
    CDestrIndemnification,
    CDestrDisposal,
    CDestrSubtotal, // Calculated
    CVaccSetup,
    CVaccVaccination,
    CVaccSubtotal, // Calculated
    CCostsTotal // Calculated

    // FIX ME: add epi outputs

  );


  type TDailyOutputVariable = (
    // Zone outputs
    DEunitDaysInZone,
    DEanimalDaysInZone,
    DEunitsInZone,
    DEanimalsInZone,

    // Epi outputs
    DEtsdUSusc,
    DEtsdASusc,
    DEtsdULat,
    DEtsdALat,
    DEtsdUSubc,
    DEtsdASubc,
    DEtsdUClin,
    DEtsdAClin,
    DEtsdUNImm,
    DEtsdANImm,
    DEtsdUVImm,
    DEtsdAVImm,
    DEtsdUDest,
    DEtsdADest,
    DEtscUSusc,
    DEtscASusc,
    DEtscULat,
    DEtscALat,
    DEtscUSubc,
    DEtscASubc,
    DEtscUClin,
    DEtscAClin,
    DEtscUNImm,
    DEtscANImm,
    DEtscUVImm,
    DEtscAVImm,
    DEtscUDest,
    DEtscADest,
    DEinfnUAir,
    DEinfnAAir,
    DEinfnUDir,
    DEinfnADir,
    DEinfnUInd,
    DEinfnAInd,
    DEinfcUIni,
    DEinfcAIni,
    DEinfcUAir,
    DEinfcAAir,
    DEinfcUDir,
    DEinfcADir,
    DEinfcUInd,
    DEinfcAInd,
    DEexpcUDir,
    DEexpcADir,
    DEexpcUInd,
    DEexpcAInd,
    DEtrcUDir,
    DEtrcADir,
    DEtrcUInd,
    DEtrcAInd,
    DEtrcUDirp,
    DEtrcADirp,
    DEtrcUIndp,
    DEtrcAIndp,
    DEtrnUDir, // New in 3.1.18
    DEtrnADir, // New in 3.1.18
    DEtrnUInd, // New in 3.1.18
    DEtrnAInd, // New in 3.1.18
    DEdetnUClin,
    DEdetnAClin,
    DEdesnUAll,
    DEdesnAAll,
    DEvaccnUAll,
    DEvaccnAAll,
    DEdetcUClin,
    DEdetcAClin,
    DEdescUIni,
    DEdescAIni,
    DEdescUDet,
    DEdescADet,
    DEdescUDir,
    DEdescADir,
    DEdescUInd,
    DEdescAInd,
    DEdescURing,
    DEdescARing,
    DEvaccUIni,
    DEvaccAIni,
    DEvaccURing,
    DEvaccARing,
    DEzonnFoci,
    DEzoncFoci,
    DEappUInfectious
  );


  type TOutputDescription = class
    protected
      _outputVariable: TIterationOutputVariable;
      _name: string;
      _descr: string;
      _isCalculated: boolean;

      constructor create( outputVariable: TIterationOutputVariable; name, descr: string; isCalculated: boolean );
    public
      property outputVariable: TIterationOutputVariable read _outputVariable;
      property name: string read _name;
      property descr: string read _descr;
      property isCalculated: boolean read _isCalculated;
    end
  ;


  type TOutputDescriptionList = class( TQIntegerObjectMap )
    public
      constructor create(); override;
      destructor destroy(); override;
      
      function item( const outputVariable: TIterationOutputVariable ): TOutputDescription;
    end
  ;

  function createOutputDictionary( const startType, endType: TIterationOutputVariable ): TQOrderedStringStringDictionary;

  var
    outputDescriptionList: TOutputDescriptionList;
    
implementation

  uses
    I88n
  ;

  function createOutputDictionary( const startType, endType: TIterationOutputVariable ): TQOrderedStringStringDictionary;
    var
      dict: TQOrderedStringStringDictionary;
      i: TIterationOutputVariable;
      d: TOutputDescription;
    begin
      dict := TQOrderedStringStringDictionary.Create();

      for i := startType to endType do
        begin
          d := outputDescriptionList.item( i );
          dict[ d.name ] := d.descr;
        end
      ;

      result := dict;
    end
  ;


  constructor TOutputDescription.create( outputVariable: TIterationOutputVariable; name, descr: string; isCalculated: boolean );
    begin
      inherited create();
      _outputVariable := outputVariable;
      _name := name;
      _descr := descr;
      _isCalculated := isCalculated;
    end
  ;


  constructor TOutputDescriptionList.create();
    var
      d: TOutputDescription;
    begin
      inherited create();

      // Zone outputs
      //-------------
      d := TOutputDescription.create( ZUnitDaysInZone, 'unitDaysInZone', tr( 'Total number of unit days spent in a zone (1 unit for 1 day = 1 unit day, 1 unit for 2 days = 2 unit days, etc.)' ), false );
      self.Add( integer( ZUnitDaysInZone ), d );

      d := TOutputDescription.create( ZAnimalDaysInZone, 'animalDaysInZone', tr( 'Total number of animal days spent in a zone (1 animal for 1 day = 1 animal day, 1 animal for 2 days = 2 animal days, etc.)' ), false );
      self.Add( integer( ZAnimalDaysInZone ), d );

      d := TOutputDescription.create( ZCostSurveillance, 'costSurveillance', tr( 'Total cost associated with surveillance in a zone over the course of an iteration.' ), false );
      self.Add( integer( ZCostSurveillance ), d );

      // Cost outputs
      //-------------
      d := TOutputDescription.create( CDestrAppraisal, 'destrAppraisal', tr( 'Total cost of appraisal for all units destroyed over the course of an iteration.' ), false );
      self.Add( integer( CDestrAppraisal ), d );

      d := TOutputDescription.create( CDestrCleaning, 'destrCleaning', tr( 'Total cost of cleaning and disinfection for all units destroyed over the course of an iteration.' ), false );
      self.Add( integer( CDestrCleaning ), d );

      d := TOutputDescription.create( CDestrEuthanasia, 'destrEuthanasia', tr( 'Total cost of euthanasia for all animals in units destroyed over the course of an iteration.' ), false );
      self.Add( integer( CDestrEuthanasia ), d );

      d := TOutputDescription.create( CDestrIndemnification, 'destrIndemnification', tr( 'Total cost of indemnification for all animals in units destroyed over the course of an iteration.' ), false );
      self.Add( integer( CDestrIndemnification ), d );
					
      d := TOutputDescription.create( CDestrDisposal, 'destrDisposal', tr( 'Total cost of carcass disposal for all animals in units destroyed over the course of an iteration.' ), false );
      self.Add( integer( CDestrDisposal ), d );					

      d := TOutputDescription.create( CDestrSubtotal, 'destrSubtotal', tr( 'Total cost associated with destruction over the course of an iteration.' ), true );
      self.Add( integer( CDestrSubtotal ), d );					

      d := TOutputDescription.create( CVaccSetup, 'vaccSetup', tr( 'Total cost of vaccination setup for all units vaccinated over the course of an iteration.' ), false );
      self.Add( integer( CVaccSetup ), d );

      d := TOutputDescription.create( CVaccVaccination, 'vaccVaccination', tr( 'Total cost of vaccination for all animals in units vaccinated over the course of an iteration.' ), false );
      self.Add( integer( CVaccVaccination ), d );

      d := TOutputDescription.create( CVaccSubtotal, 'vaccSubtotal', tr( 'Total cost associated with vaccination over the course of an iteration.' ), true );
      self.Add( integer( CVaccSubtotal ), d );

      d := TOutputDescription.create( CCostsTotal, 'costsTotal', tr( 'Total cost of all activities over the course of an iteration.' ), true );
      self.Add( integer( CCostsTotal ), d );

      // Epi outputs
      //------------
      (*
      d := TOutputDescription.create( );
      self.Add( integer(  ), d );
      *)
    end
  ;


  destructor TOutputDescriptionList.destroy();
    begin
      deleteValues();
      inherited destroy();
    end
  ;


  function TOutputDescriptionList.item( const outputVariable: TIterationOutputVariable ): TOutputDescription;
    begin
      result := self.value( integer( outputVariable ) ) as TOutputDescription;
    end
  ;


initialization
  outputDescriptionList := TOutputDescriptionList.create();

finalization
  if( nil <> outputDescriptionList ) then
    freeAndNil( outputDescriptionList )
  ;

end.