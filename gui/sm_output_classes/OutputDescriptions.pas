unit OutputDescriptions;

(*
OutputDescriptions.pas
----------------------
Begin: 2007/04/20
Last revision: $Date: 2013-06-27 19:11:37 $ $Author: areeves $
Version number: $Revision: 1.11.4.14 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <aaron.reeves@naadsm.org>
--------------------------------------------------
Copyright (C) 2007 - 2011 Colorado State University

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
    StaNAADSMStateUnspecified,
    StatsEpi,
    StatsCosts,
    StatsPTZones,
    StatsZones
  );


  {*
    REMEMBER: When adding to this list, update the "first"/"last" and the
    "set type" functions below.

    Outputs can be "complete" or "incomplete": "complete" means that every
    single iteration will have a value for the output of interest.  Examples
    include the total numbers of exposed, infected, and destroyed units.

    "Incomplete" means that it is possible for an iteration not to have an output.
    The output for day of first detection, for example, can be incomplete if detection
    never occurs in some iterations.

    "Calculated" outputs are not stored in the database, but are generated from
    other outputs that are stored.
  }
  type TIterationOutputType = (
    // Zones
    //------
    ZZonesOccurred,
    ZMaxZoneArea,
    ZMaxZoneAreaDay,
    ZFinalZoneArea,
    ZMaxZonePerimeter,
    ZMaxZonePerimeterDay,
    ZFinalZonePerimeter,

    // Zones and production types
    //---------------------------
    ZPTUnitDaysInZone,
    ZPTAnimalDaysInZone,
    ZPTCostSurveillance,

    // Costs
    //------
    CDestrAppraisal,
    CDestrCleaning,
    CDestrEuthanasia,
    CDestrIndemnification,
    CDestrDisposal,
    CDestrSubtotal, // Calculated
    CVaccSetup,
    CVaccVaccination,
    CVaccSubtotal, // Calculated
    CCostsTotal, // Calculated

    // Epi outputs
    //------------
    // Cumulative totals for each disease state
    EpiTscUSusc,
    EpiTscASusc,
    EpiTscULat,
    EpiTscALat,
    EpiTscUSubc,
    EpiTscASubc,
    EpiTscUClin,
    EpiTscAClin,
    EpiTscUNImm,
    EpiTscANImm,
    EpiTscUVImm,
    EpiTscAVImm,
    EpiTscUDest,
    EpiTscADest,

    // Cumulative totals for cause of infection
    EpiInfcUIni,
    EpiInfcAIni,
    EpiInfcUAir,
    EpiInfcAAir,
    EpiInfcUDir,
    EpiInfcADir,
    EpiInfcUInd,
    EpiInfcAInd,
    EpiInfcUAll, // Calculated
    EpiInfcAAll, // Calculated

    // Cumulative totals for exposures
    EpiExpcUDir,
    EpiExpcADir,
    EpiExpcUInd,
    EpiExpcAInd,
    EpiExpcUAll, // Calculated
    EpiExpcAAll, // Calculated

    // Cumulative totals for trace-forwards (renamed in 3.2.0)
    EpiTrcUDirFwd,
    EpiTrcADirFwd,
    EpiTrcUIndFwd,
    EpiTrcAIndFwd,
    EpiTrcUDirpFwd,
    EpiTrcADirpFwd,
    EpiTrcUIndpFwd,
    EpiTrcAIndpFwd,

    // Cumulative totals for trace-backs (new in 3.2.0)
    EpiTrcUDirBack,
    EpiTrcADirBack,
    EpiTrcUIndBack,
    EpiTrcAIndBack,
    EpiTrcUDirpBack,
    EpiTrcADirpBack,
    EpiTrcUIndpBack,
    EpiTrcAIndpBack,

    // Calculated cumulative totals for tracing (new in 3.2.0)
    EpiTrcUDirAll, // Calculated
    EpiTrcADirAll, // Calculated
    EpiTrcUIndAll, // Calculated
    EpiTrcAIndAll, // Calculated
    EpiTrcUAll,    // Calculated
    EpiTrcAAll,   // Calculated

    // Cumulative totals for trace origins (new in 3.2.11 and 4.0.5)
    EpiTocUDirFwd,
    EpiTocUIndFwd,
    EpiTocUDirBack,
    EpiTocUIndBack,

    // Calculated cumulative totals for trace origins (new in 3.2.11 and 4.0.5)
    EpiTocUDirAll, // Calculated
    EpiTocUIndAll, // Calculated
    EpiTocUAll,    // Calculated

    // Cumulative totals for detection by clinical signs
    EpiDetcUClin,
    EpiDetcAClin,

    // Cumulative totals for detection by diagnostic testing (new in 3.2.0)
    EpiDetcUTest,
    EpiDetcATest,

    // Calculated cumulative overall totals for detection (new in 3.2.0)
    EpiDetcUAll,  // Calculated
    EpiDetcAAll,  // Calculated

    // Cumulative totals for destruction
    EpiDescUIni,
    EpiDescAIni,
    EpiDescUDet,
    EpiDescADet,
    EpiDescUDirFwd,
    EpiDescADirFwd,
    EpiDescUIndFwd,
    EpiDescAIndFwd,
    EpiDescUDirBack, // New in 3.2.0
    EpiDescADirBack, // New in 3.2.0
    EpiDescUIndBack, // New in 3.2.0
    EpiDescAIndBack, // New in 3.2.0
    EpiDescURing,
    EpiDescARing,
    EpiDescUAll, // Calculated
    EpiDescAAll, // Calculated

    // Final outputs for destruction queue (introduced in 3.2.0)
    EpiDeswUMax, // Requires special treatment for the case of "All production types"
    EpiDeswAMax, // Requires special treatment for the case of "All production types"
    EpiDeswUMaxDay, // Requires special treatment for the case of "All production types"
    EpiDeswAMaxDay, // Requires special treatment for the case of "All production types"
    EpiDeswUTimeMax, // Requires special treatment for the case of "All production types"
    EpiDeswUTimeAvg, // Requires special treatment for the case of "All production types"
    EpiDeswUDaysInQueue,
    EpiDeswADaysInQueue,

    // Cumulative totals for vaccination
    EpiVaccUIni,
    EpiVaccAIni,
    EpiVaccURing,
    EpiVaccARing,
    EpiVaccUAll, // Calculated, new in 3.2.0
    EpiVaccAAll, // Calculated, new in 3.2.0

    // Final outputs for vaccination queue (introduced in 3.2.0)
    EpiVacwUMax, // Requires special treatment for the case of "All production types"
    EpiVacwAMax, // Requires special treatment for the case of "All production types"
    EpiVacwUMaxDay, // Requires special treatment for the case of "All production types"
    EpiVacwAMaxDay, // Requires special treatment for the case of "All production types"
    EpiVacwUTimeMax, // Requires special treatment for the case of "All production types"
    EpiVacwUTimeAvg, // Requires special treatment for the case of "All production types"
    // For NAADSM 3.x, and for at least the time being, these two outputs are no longer saved.
    //EpiVacwUDaysInQueue,
    //EpiVacwADaysInQueue,

    // Cumulative totals for herd exams (introduced in 3.2.0)
    EpiExmcUDirFwd,
    EpiExmcADirFwd,
    EpiExmcUIndFwd,
    EpiExmcAIndFwd,
    EpiExmcUDirBack,
    EpiExmcADirBack,
    EpiExmcUIndBack,
    EpiExmcAIndBack,
    EpiExmcUDirAll, // Calculated
    EpiExmcADirAll, // Calculated
    EpiExmcUIndAll, // Calculated
    EpiExmcAIndAll, // Calculated
    EpiExmcUAll,    // Calculated
    EpiExmcAAll,    // Calculated

    // Cumulative totals for diagnostic testing (introduced in 3.2.0)
    EpiTstcUDirFwd,
    EpiTstcADirFwd,
    EpiTstcUIndFwd,
    EpiTstcAIndFwd,
    EpiTstcUDirBack,
    EpiTstcADirBack,
    EpiTstcUIndBack,
    EpiTstcAIndBack,
    EpiTstcUDirAll, // Calculated
    EpiTstcADirAll, // Calculated
    EpiTstcUIndAll, // Calculated
    EpiTstcAIndAll, // Calculated
    EpiTstcUAll,    // Calculated
    EpiTstcAAll,    // Calculated
    EpiTstcUTruePos,
    EpiTstcATruePos,
    EpiTstcUTrueNeg,
    EpiTstcATrueNeg,
    EpiTstcUFalsePos,
    EpiTstcAFalsePos,
    EpiTstcUFalseNeg,
    EpiTstcAFalseNeg,

    // Cumulative totals for zone foci
    EpiZoncFoci,

    // First and last detections
    EpiDetOccurred, // Calculated
    EpiFirstDetection, // Incomplete dataset

    // Number infected at time of first detection
    EpiFirstDetUInf, // Incomplete dataset
    EpiFirstDetAInf, // Incomplete dataset

    EpiLastDetection, // Incomplete dataset

    // Other first events
    EpiVaccOccurred, // Calculated
    EpiFirstVaccination, // Incomplete dataset
    EpiDestrOccurred, // Calculated
    EpiFirstDestruction, // Incomplete dataset

    // Outbreak duration
    EpiDiseaseEnded, // Calculated
    EpiDiseaseDuration, // Pseudocalculated, incomplete dataset
    EpiOutbreakEnded, // Calculated
    EpiOutbreakDuration // Pseudocalculated, incomplete dataset
  );

  type TIterationOutputTypeSet = set of TIterationOutputType;

  function firstZoneIterationOutput(): TIterationOutputType;
  function lastZoneIterationOutput(): TIterationOutputType;
  function firstZonePTIterationOutput(): TIterationOutputType;
  function lastZonePTIterationOutput(): TIterationOutputType;
  function firstCostIterationOutput(): TIterationOutputType;
  function lastCostIterationOutput(): TIterationOutputType;
  function firstEpiIterationOutput(): TIterationOutputType;
  function lastEpiIterationOutput(): TIterationOutputType;

  function zoneIterationOutputTypes(): TIterationOutputTypeSet;
  function zonePTIterationOutputTypes(): TIterationOutputTypeSet;
  function costIterationOutputTypes(): TIterationOutputTypeSet;
  function zonePTCostIterationOutputTypes(): TIterationOutputTypeSet;
  function epiIterationOutputTypes(): TIterationOutputTypeSet;

  // TDailyOutputType is used in unit SelectDailyOutputs.
  // All of the following were introduced in version 3.1.17, unless indicated otherwise.
  type TDailyOutputType = (
    // Zone outputs
    //-------------
    DZunitDaysInZone,
    DZanimalDaysInZone,
    DZunitsInZone,
    DZanimalsInZone,

    // Epi outputs
    //------------
    // Daily counts for each disease state
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

    // Running totals for each disease state
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

    // New (incident) counts for cause of infection
    DEinfnUAir,
    DEinfnAAir,
    DEinfnUDir,
    DEinfnADir,
    DEinfnUInd,
    DEinfnAInd,

    // Running totals for cause of infection
    DEinfcUIni,
    DEinfcAIni,
    DEinfcUAir,
    DEinfcAAir,
    DEinfcUDir,
    DEinfcADir,
    DEinfcUInd,
    DEinfcAInd,

    // Running totals for exposure
    DEexpcUDir,
    DEexpcADir,
    DEexpcUInd,
    DEexpcAInd,

    // New (incident) counts for trace-forwards (introduced in 3.1.18, renamed in 3.2.0)
    DEtrnUDirFwd,
    DEtrnADirFwd,
    DEtrnUIndFwd,
    DEtrnAIndFwd,

    // New (incident) counts for trace-backs (introduced in 3.2.0)
    DEtrnUDirBack,
    DEtrnADirBack,
    DEtrnUIndBack,
    DEtrnAIndBack,

    // Running totals for trace-forwards (renamed in 3.2.0)
    DEtrcUDirFwd,
    DEtrcADirFwd,
    DEtrcUIndFwd,
    DEtrcAIndFwd,
    DEtrcUDirpFwd,
    DEtrcADirpFwd,
    DEtrcUIndpFwd,
    DEtrcAIndpFwd,

    // Running totals for trace-backs (introduced in 3.2.0)
    DEtrcUDirBack,
    DEtrcADirBack,
    DEtrcUIndBack,
    DEtrcAIndBack,
    DEtrcUDirpBack,
    DEtrcADirpBack,
    DEtrcUIndpBack,
    DEtrcAIndpBack,

    // New (incident) counts for herd exams (introduced in 3.2.0)
    DEexmnUAll,
    DEexmnAAll,

    // New (incident) counts for trace origins (introduced in 3.2.11 and 4.0.5)
    DEtonUDirFwd,
    DEtonUIndFwd,
    DEtonUDirBack,
    DEtonUIndBack,

    // Running totals for trace origins (introduced in 3.2.11 and 4.0.5)
    DEtocUDirFwd,
    DEtocUIndFwd,
    DEtocUDirBack,
    DEtocUIndBack,

    // New (incident) counts for diagnostic testing (introduced in 3.2.0)
    DEtstnUTruePos,
    DEtstnATruePos,
    DEtstnUTrueNeg,
    DEtstnATrueNeg,
    DEtstnUFalsePos,
    DEtstnAFalsePos,
    DEtstnUFalseNeg,
    DEtstnAFalseNeg,

    // New (incident) counts for detection by clinical signs
    DEdetnUClin,
    DEdetnAClin,

    // New (incident) counts for detection by diagnostic testing (introduced in 3.2.0)
    DEdetnUTest,
    DEdetnATest,

    // Running totals for detection by clinical signs
    DEdetcUClin,
    DEdetcAClin,

    // Running totals for detection by diagnostic testing (introduced in 3.2.0)
    DEdetcUTest,
    DEdetcATest,

    // New (incident) counts for destruction
    DEdesnUAll,
    DEdesnAAll,

    // Running totals for destruction
    DEdescUIni,
    DEdescAIni,
    DEdescUDet,
    DEdescADet,
    DEdescUDirFwd,
    DEdescADirFwd,
    DEdescUIndFwd,
    DEdescAIndFwd,
    DEdescUDirBack, // new in 3.2.0
    DEdescADirBack, // new in 3.2.0
    DEdescUIndBack, // new in 3.2.0
    DEdescAIndBack, // new in 3.2.0
    DEdescURing,
    DEdescARing,

    // Daily counts of units/animals in queue to be destroyed (introduced in 3.2.0)
    DEdeswUAll,
    DEdeswAAll,

    // New (incident) counts for vaccination
    DEvacnUAll,
    DEvacnAAll,

    // Running totals for vaccination
    DEvaccUIni,
    DEvaccAIni,
    DEvaccURing,
    DEvaccARing,

    // Daily counts of units/animals in queue to be vaccinated (introduced in 3.2.0)
    DEvacwUAll,
    DEvacwAAll,

    // Running totals for herd exams (introduced in 3.2.0)
    DEexmcUDirFwd,
    DEexmcADirFwd,
    DEexmcUIndFwd,
    DEexmcAIndFwd,
    DEexmcUDirBack,
    DEexmcADirBack,
    DEexmcUIndBack,
    DEexmcAIndBack,

    // Running totals for diagnostic testing (introduced in 3.2.0)
    DEtstcUDirFwd,
    DEtstcADirFwd,
    DEtstcUIndFwd,
    DEtstcAIndFwd,
    DEtstcUDirBack,
    DEtstcADirBack,
    DEtstcUIndBack,
    DEtstcAIndBack,
    DEtstcUTruePos,
    DEtstcATruePos,
    DEtstcUTrueNeg,
    DEtstcATrueNeg,
    DEtstcUFalsePos,
    DEtstcAFalsePos,
    DEtstcUFalseNeg,
    DEtstcAFalseNeg,

    // New (incident) counts for zone foci
    DEzonnFoci,

    // Running totals for zone foci
    DEzoncFoci,

    // Daily counts of known infectious units
    DEappdUInfectious
  );

  type TOutputDescription = class
    protected
      _outputVariable: TIterationOutputType;
      _name: string;
      _descr: string;
      _isCalculated: boolean;
      _isIncompleteDataSet: boolean;

      constructor create(
        outputVariable: TIterationOutputType;
        name, descr: string;
        isCalculated: boolean;
        isIncompleteDataSet: boolean
      );
    public
      property outputVariable: TIterationOutputType read _outputVariable;
      property name: string read _name;
      property descr: string read _descr;
      property isCalculated: boolean read _isCalculated;
      property isIncompleteDataSet: boolean read _isIncompleteDataSet;
    end
  ;


  // The list contains descriptions of all of the iteration outputs: see TIterationOutputType above.
  type TOutputDescriptionList = class( TQIntegerObjectMap )
    public
      constructor create(); override;
      destructor destroy(); override;
      
      function item( const outputVariable: TIterationOutputType ): TOutputDescription;
    end
  ;

  function createOutputDictionary( const startType, endType: TIterationOutputType ): TQOrderedStringStringDictionary;

  var
    // This global variable is initialized by the main program loop,
    // and is used in several other units.
    outputDescriptionList: TOutputDescriptionList;
    
implementation

  uses
    I88n
  ;

  function firstZoneIterationOutput(): TIterationOutputType; begin result := ZZonesOccurred; end;
  function lastZoneIterationOutput(): TIterationOutputType; begin result := ZFinalZonePerimeter; end;
  function firstZonePTIterationOutput(): TIterationOutputType; begin result := ZPTUnitDaysInZone; end;
  function lastZonePTIterationOutput(): TIterationOutputType; begin result := ZPTCostSurveillance; end;
  function firstCostIterationOutput(): TIterationOutputType; begin result := CDestrAppraisal; end;
  function lastCostIterationOutput(): TIterationOutputType; begin result := CCostsTotal; end;
  function firstEpiIterationOutput(): TIterationOutputType; begin result := EpiTscUSusc; end;
  function lastEpiIterationOutput(): TIterationOutputType; begin result := EpiOutbreakDuration; end;

  function zoneIterationOutputTypes(): TIterationOutputTypeSet;
    begin
      result := [ ZZonesOccurred..ZFinalZonePerimeter ];
    end
  ;


  function zonePTIterationOutputTypes(): TIterationOutputTypeSet;
    begin
      result := [ ZPTUnitDaysInZone..ZPTCostSurveillance ];
    end
  ;


  function costIterationOutputTypes(): TIterationOutputTypeSet;
    begin
      result := [ CDestrAppraisal..CCostsTotal ];
    end
  ;


  function zonePTCostIterationOutputTypes(): TIterationOutputTypeSet;
    begin
      result := [ ZPTCostSurveillance ];
    end
  ;


  function epiIterationOutputTypes(): TIterationOutputTypeSet;
    begin
      result := [ EpiTscUSusc..EpiOutbreakDuration ];
    end
  ;


  function createOutputDictionary( const startType, endType: TIterationOutputType ): TQOrderedStringStringDictionary;
    var
      dict: TQOrderedStringStringDictionary;
      i: TIterationOutputType;
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


  constructor TOutputDescription.create(
        outputVariable: TIterationOutputType;
        name, descr: string;
        isCalculated: boolean;
        isIncompleteDataSet: boolean
      );
    begin
      inherited create();
      _outputVariable := outputVariable;
      _name := name;
      _descr := descr;
      _isCalculated := isCalculated;
      _isIncompleteDataSet := isIncompleteDataSet;
    end
  ;


  constructor TOutputDescriptionList.create();
    var
      d: TOutputDescription;
    begin
      inherited create();

      // Zone outputs
      //-------------
      d := TOutputDescription.create( ZZonesOccurred, 'zoneOccurred', tr( 'Number of iterations in which zone foci were established' ), true, false );
      self.Add( integer( ZZonesOccurred ), d );

      d := TOutputDescription.create( ZMaxZoneArea, 'maxZoneArea', tr( 'Maximum area (in square kilometers) reached for the indicated zone over the course of an iteration' ), false, true );
      self.Add( integer( ZMaxZoneArea ), d );

      d := TOutputDescription.create( ZMaxZoneAreaDay, 'maxZoneAreaDay', tr( 'Day on which maximum area for the indicated zone is reached' ), false, true );
      self.Add( integer( ZMaxZoneAreaDay ), d );

      d := TOutputDescription.create( ZFinalZoneArea, 'finalZoneArea', tr( 'Area (in square kilometers) of the indicated zone at the end of an iteration' ), false, true );
      self.Add( integer( ZFinalZoneArea ), d );

      d := TOutputDescription.create( ZMaxZonePerimeter, 'maxZonePerimeter', tr( 'Maximum perimeter (in kilometers) reached for the indicated zone over the course of an iteration' ), false, true );
      self.Add( integer( ZMaxZonePerimeter ), d );

      d := TOutputDescription.create( ZMaxZonePerimeterDay, 'maxZonePerimeterDay', tr( 'Day on which maximum perimeter for the indicated zone is reached' ), false, true );
      self.Add( integer( ZMaxZonePerimeterDay ), d );

      d := TOutputDescription.create( ZFinalZonePerimeter, 'finalZonePerimeter', tr( 'Perimeter (in kilometers) of the indicated zone at the end of an iteration' ), false, true );
      self.Add( integer( ZFinalZonePerimeter ), d );

      // Zone/production type outputs
      //-----------------------------
      d := TOutputDescription.create( ZPTUnitDaysInZone, 'unitDaysInZone', tr( 'Total number of unit days spent in a zone (1 unit for 1 day = 1 unit day, 1 unit for 2 days = 2 unit days, etc.)' ), false, false );
      self.Add( integer( ZPTUnitDaysInZone ), d );

      d := TOutputDescription.create( ZPTAnimalDaysInZone, 'animalDaysInZone', tr( 'Total number of animal days spent in a zone (1 animal for 1 day = 1 animal day, 1 animal for 2 days = 2 animal days, etc.)' ), false, false );
      self.Add( integer( ZPTAnimalDaysInZone ), d );

      d := TOutputDescription.create( ZPTCostSurveillance, 'costSurveillance', tr( 'Total cost associated with surveillance in a zone over the course of an iteration.' ), false, true );
      self.Add( integer( ZPTCostSurveillance ), d );

      // Cost outputs
      //-------------
      d := TOutputDescription.create( CDestrAppraisal, 'destrAppraisal', tr( 'Total cost of appraisal for all units destroyed over the course of an iteration.' ), false, true );
      self.Add( integer( CDestrAppraisal ), d );

      d := TOutputDescription.create( CDestrCleaning, 'destrCleaning', tr( 'Total cost of cleaning and disinfection for all units destroyed over the course of an iteration.' ), false, true );
      self.Add( integer( CDestrCleaning ), d );

      d := TOutputDescription.create( CDestrEuthanasia, 'destrEuthanasia', tr( 'Total cost of euthanasia for all animals in units destroyed over the course of an iteration.' ), false, true );
      self.Add( integer( CDestrEuthanasia ), d );

      d := TOutputDescription.create( CDestrIndemnification, 'destrIndemnification', tr( 'Total cost of indemnification for all animals in units destroyed over the course of an iteration.' ), false, true );
      self.Add( integer( CDestrIndemnification ), d );
					
      d := TOutputDescription.create( CDestrDisposal, 'destrDisposal', tr( 'Total cost of carcass disposal for all animals in units destroyed over the course of an iteration.' ), false, true );
      self.Add( integer( CDestrDisposal ), d );					

      d := TOutputDescription.create( CDestrSubtotal, 'destrSubtotal', tr( 'Total cost associated with destruction over the course of an iteration.' ), true, true );
      self.Add( integer( CDestrSubtotal ), d );					

      d := TOutputDescription.create( CVaccSetup, 'vaccSetup', tr( 'Total cost of vaccination setup for all units vaccinated over the course of an iteration.' ), false, true );
      self.Add( integer( CVaccSetup ), d );

      d := TOutputDescription.create( CVaccVaccination, 'vaccVaccination', tr( 'Total cost of vaccination for all animals in units vaccinated over the course of an iteration.' ), false, true );
      self.Add( integer( CVaccVaccination ), d );

      d := TOutputDescription.create( CVaccSubtotal, 'vaccSubtotal', tr( 'Total cost associated with vaccination over the course of an iteration.' ), true, true );
      self.Add( integer( CVaccSubtotal ), d );

      d := TOutputDescription.create( CCostsTotal, 'costsTotal', tr( 'Total cost of all activities over the course of an iteration.' ), true, true );
      self.Add( integer( CCostsTotal ), d );

      // Epi outputs
      //------------
      // Cumulative totals for each disease state
      d := TOutputDescription.create( EpiTscUSusc, 'tscUSusc', tr( 'Number of units that are or become susceptible over the course of an iteration' ), false, false );
      self.Add( integer( EpiTscUSusc ), d );

      d := TOutputDescription.create( EpiTscASusc, 'tscASusc', tr( 'Total number of animals in all units that are or become susceptible over the course of an iteration' ), false, false );
      self.Add( integer( EpiTscASusc ), d );

      d := TOutputDescription.create( EpiTscULat, 'tscULat', tr( 'Number of units that are or become latent over the course of an iteration' ), false, false );
      self.Add( integer( EpiTscULat ), d );

      d := TOutputDescription.create( EpiTscALat, 'tscALat', tr( 'Total number of animals in all units that are or become latent over the course of an iteration' ), false, false );
      self.Add( integer( EpiTscALat ), d );

      d := TOutputDescription.create( EpiTscUSubc, 'tscUSubc', tr( 'Number of units that are or become subclinically infectious over the course of an iteration' ), false, false );
      self.Add( integer( EpiTscUSubc ), d );

      d := TOutputDescription.create( EpiTscASubc, 'tscASubc', tr( 'Total number of animals in all units that are or become infectious over the course of an iteration' ), false, false );
      self.Add( integer( EpiTscASubc ), d );

      d := TOutputDescription.create( EpiTscUClin, 'tscUClin', tr( 'Number of units that are or become clinical over the course of an iteration' ), false, false );
      self.Add( integer( EpiTscUClin ), d );

      d := TOutputDescription.create( EpiTscAClin, 'tscAClin', tr( 'Total number of animals in all units that are or become clinical over the course of an iteration' ), false, false );
      self.Add( integer( EpiTscAClin ), d );

      d := TOutputDescription.create( EpiTscUNImm, 'tscUNImm', tr( 'Number of units that are or become naturally immune over the course of an iteration' ), false, false );
      self.Add( integer( EpiTscUNImm ), d );

      d := TOutputDescription.create( EpiTscANImm, 'tscANImm', tr( 'Total number of animals in all units that are or become naturally immune over the course of an iteration' ), false, false );
      self.Add( integer( EpiTscANImm ), d );

      d := TOutputDescription.create( EpiTscUVImm, 'tscUVImm', tr( 'Number of units that are or become vaccine immune over the course of an iteration' ), false, false );
      self.Add( integer( EpiTscUVImm ), d );

      d := TOutputDescription.create( EpiTscAVImm, 'tscAVImm', tr( 'Total number of animals in all units that are or become vaccine immune over the course of an iteration' ), false, false );
      self.Add( integer( EpiTscAVImm ), d );

      d := TOutputDescription.create( EpiTscUDest, 'tscUDest', tr( 'Number of units that are destroyed over the course of an iteration' ), false, false );
      self.Add( integer( EpiTscUDest ), d );

      d := TOutputDescription.create( EpiTscADest, 'tscADest', tr( 'Total number of animals in all units that are destroyed over the course of an iteration' ), false, false );
      self.Add( integer( EpiTscADest ), d );

      // Cumulative totals for cause of infection
      d := TOutputDescription.create( EpiInfcUIni, 'infcUIni', tr( 'Number of units that are initially infected at the beginning of an iteration' ), false, false );
      self.Add( integer( EpiInfcUIni ), d );

      d := TOutputDescription.create( EpiInfcAIni, 'infcAIni', tr( 'Number of animals in units that are initially infected at the beginning of an iteration' ), false, false );
      self.Add( integer( EpiInfcAIni ), d );

      d := TOutputDescription.create( EpiInfcUAir, 'infcUAir', tr( 'Number of units that become infected by airborne spread over the course of an iteration' ), false, false );
      self.Add( integer( EpiInfcUAir ), d );

      d := TOutputDescription.create( EpiInfcAAir, 'infcAAir', tr( 'Number of animals in units that become infected by airborne spread over the course of an iteration' ), false, false );
      self.Add( integer( EpiInfcAAir ), d );

      d := TOutputDescription.create( EpiInfcUDir, 'infcUDir', tr( 'Number of units that become infected by direct contact over the course of an iteration' ), false, false );
      self.Add( integer( EpiInfcUDir ), d );

      d := TOutputDescription.create( EpiInfcADir, 'infcADir', tr( 'Number of animals in units that become infected by direct contact over the course of an iteration' ), false, false );
      self.Add( integer( EpiInfcADir ), d );

      d := TOutputDescription.create( EpiInfcUInd, 'infcUInd', tr( 'Number of units that become infected by indirect contact over the course of an iteration' ), false, false );
      self.Add( integer( EpiInfcUInd ), d );

      d := TOutputDescription.create( EpiInfcAInd, 'infcAInd', tr( 'Number of animals in units that become infected by indirect contact over the course of an iteration' ), false, false );
      self.Add( integer( EpiInfcAInd ), d );

      d := TOutputDescription.create( EpiInfcUAll, 'infcUAll', tr( 'Total number of units that become infected over the course of an iteration' ) + ' ' + tr ( '(not including initially infected units)' ), true, false );
      self.Add( integer( EpiInfcUAll ), d );

      d := TOutputDescription.create( EpiInfcAAll, 'infcAAll', tr( 'Total number of animals in units that become infected over the course of an iteration' ) + ' ' + tr ( '(not including initially infected units)' ), true, false );
      self.Add( integer( EpiInfcAAll ), d );

      // Cumulative totals for exposures
      d := TOutputDescription.create( EpiExpcUDir, 'expcUDir', tr( 'Total number of units directly exposed to any infected unit over the course of an iteration' ), false, false );
      self.Add( integer( EpiExpcUDir ), d );

      d := TOutputDescription.create( EpiExpcADir, 'expcADir', tr( 'Total number of animals in units directly exposed to any infected unit over the course of an iteration' ), false, false );
      self.Add( integer( EpiExpcADir ), d );

      d := TOutputDescription.create( EpiExpcUInd, 'expcUInd', tr( 'Total number of units indirectly exposed to any infected unit over the course of an iteration' ), false, false );
      self.Add( integer( EpiExpcUInd ), d );

      d := TOutputDescription.create( EpiExpcAInd, 'expcAInd', tr( 'Total number of animals in units indirectly exposed to any infected unit over the course of an iteration' ), false, false );
      self.Add( integer( EpiExpcAInd ), d );

      d := TOutputDescription.create( EpiExpcUAll, 'expcUAll', tr( 'Total number units exposed by any contact to any infected unit over the course of an iteration' ), true, false );
      self.Add( integer( EpiExpcUAll ), d );

      d := TOutputDescription.create( EpiExpcAAll, 'expcAAll', tr( 'Total number of animals in units exposed by any contact to any infected unit over the course of an iteration' ), true, false );
      self.Add( integer( EpiExpcAAll ), d );

      // Cumulative totals for trace-forwards
      d := TOutputDescription.create( EpiTrcUDirFwd, 'trcUDirFwd', tr( 'Number of units directly exposed and successfully traced forward over the course of an iteration' ), false, false );
      self.Add( integer( EpiTrcUDirFwd ), d );

      d := TOutputDescription.create( EpiTrcADirFwd, 'trcADirFwd', tr( 'Total number of animals in all units directly exposed and successfully traced forward over the course of an iteration' ), false, false );
      self.Add( integer( EpiTrcADirFwd ), d );

      d := TOutputDescription.create( EpiTrcUIndFwd, 'trcUIndFwd', tr( 'Number of units indirectly exposed and successfully traced forward over the course of an iteration' ), false, false );
      self.Add( integer( EpiTrcUIndFwd ), d );

      d := TOutputDescription.create( EpiTrcAIndFwd, 'trcAIndFwd', tr( 'Total number of animals in all units indirectly exposed and successfully traced forward over the course of an iteration' ), false, false );
      self.Add( integer( EpiTrcAIndFwd ), d );

      d := TOutputDescription.create( EpiTrcUDirpFwd, 'trcUDirpFwd', tr( 'Number of units directly exposed that could possibly have been traced forward over the course of an iteration' ), false, false );
      self.Add( integer( EpiTrcUDirpFwd ), d );

      d := TOutputDescription.create( EpiTrcADirpFwd, 'trcADirpFwd', tr( 'Total number of animals in all units directly exposed that could possibly have been traced forward over the course of an iteration' ), false, false );
      self.Add( integer( EpiTrcADirpFwd ), d );

      d := TOutputDescription.create( EpiTrcUIndpFwd, 'trcUIndpFwd', tr( 'Number of units indirectly exposed that could possibly have been traced forward over the course of an iteration' ), false, false );
      self.Add( integer( EpiTrcUIndpFwd ), d );

      d := TOutputDescription.create( EpiTrcAIndpFwd, 'trcAIndpFwd', tr( 'Total number of animals in units indirectly exposed that could possibly have been traced forward over the course of an iteration' ), false, false );
      self.Add( integer( EpiTrcAIndpFwd ), d );

      // Cumulative totals for trace-backs
      d := TOutputDescription.create( EpiTrcUDirBack, 'trcUDirBack', tr( 'Number of units successfully traced back from a detected unit after direct contact over the course of the iteration' ), false, false );
      self.Add( integer( EpiTrcUDirBack ), d );

      d := TOutputDescription.create( EpiTrcADirBack, 'trcADirBack', tr( 'Total number of animals in units successfully traced back from a detected unit over the course of the iteration' ), false, false );
      self.Add( integer( EpiTrcADirBack ), d );

      d := TOutputDescription.create( EpiTrcUIndBack, 'trcUIndBack', tr( 'Number of units successfully traced back from a detected unit after indirect contact over the course of the iteration' ), false, false );
      self.Add( integer( EpiTrcUIndBack ), d );

      d := TOutputDescription.create( EpiTrcAIndBack, 'trcAIndBack', tr( 'Total number of animals in units successfully traced back from a detected unit after indirect contact over the course of the iteration' ), false, false );
      self.Add( integer( EpiTrcAIndBack ), d );

      d := TOutputDescription.create( EpiTrcUDirpBack, 'trcUDirpBack', tr( 'Number of units that could possibly have been traced back from a detected unit after direct contact over the course of the iteration' ), false, false );
      self.Add( integer( EpiTrcUDirpBack ), d );

      d := TOutputDescription.create( EpiTrcADirpBack, 'trcADirpBack', tr( 'Total number of animals in units that could possibly have been traced back from a detected unit after direct contact over the course of the iteration' ), false, false );
      self.Add( integer( EpiTrcADirpBack ), d );

      d := TOutputDescription.create( EpiTrcUIndpBack, 'trcUIndpBack', tr( 'Number of units that could possibly have been traced back from a detected unit after indirect contact over the course of the iteration' ), false, false );
      self.Add( integer( EpiTrcUIndpBack ), d );

      d := TOutputDescription.create( EpiTrcAIndpBack, 'trcAIndpBack', tr( 'Total number of animals in units that could possibly have been traced back from a detected unit after indirect contact over the course of the iteration' ), false, false );
      self.Add( integer( EpiTrcAIndpBack ), d );

      d := TOutputDescription.create( EpiTrcUDirAll, 'trcUDirAll', tr( 'Total number of units successfully identified by tracing (either forward or back) of direct contact over the course of the iteration' ), true, false );
      self.Add( integer( EpiTrcUDirAll ), d );

      d := TOutputDescription.create( EpiTrcADirAll, 'trcADirAll', tr( 'Total number of animals in units identified by tracing (either forward or back) of direct contact over the course of the iteration' ), true, false );
      self.Add( integer( EpiTrcADirAll ), d );

      d := TOutputDescription.create( EpiTrcUIndAll, 'trcUIndAll', tr( 'Total number of units successfully identified by tracing (either forward or back) of indirect contact over the course of the iteration' ), true, false );
      self.Add( integer( EpiTrcUIndAll ), d );

      d := TOutputDescription.create( EpiTrcAIndAll, 'trcAIndAll', tr( 'Total number of animals in units successfully identified by tracing (either forward or back) of indirect contact over the course of the iteration' ), true, false );
      self.Add( integer( EpiTrcAIndAll ), d );

      d := TOutputDescription.create( EpiTrcUAll, 'trcUAll', tr( 'Total number of units successfully identified by tracing (either forward or back) of contact (either direct or indirect) over the course of the iteration' ), true, false );
      self.Add( integer( EpiTrcUAll ), d );

      d := TOutputDescription.create( EpiTrcAAll, 'trcAAll', tr( 'Total number of animals in units successfully identified by tracing (either forward or back) of contact (either direct or indirect) over the course of the iteration' ), true, false );
      self.Add( integer( EpiTrcAAll ), d );

      // Cumulative totals for trace origins
      d := TOutputDescription.create( EpiTocUDirFwd, 'tocUDirFwd', tr( 'Number of trace-forwards of direct contact that originate at units of the designated type over the course of an iteration' ), false, false );
      self.Add( integer( EpiTocUDirFwd ), d );

      d := TOutputDescription.create( EpiTocUIndFwd, 'tocUIndFwd', tr( 'Number of trace-forwards of indirect contact that originate at units of the designated type over the course of an iteration' ), false, false );
      self.Add( integer( EpiTocUIndFwd ), d );

      d := TOutputDescription.create( EpiTocUDirBack, 'tocUDirBack', tr( 'Number of trace-backs of direct contact that originate at units of the designated type over the course of an iteration' ), false, false );
      self.Add( integer( EpiTocUDirBack ), d );

      d := TOutputDescription.create( EpiTocUIndBack, 'tocUIndBack', tr( 'Number of trace-backs of indirect contact that originate at units of the designated type over the course of an iteration' ), false, false );
      self.Add( integer( EpiTocUIndBack ), d );

      d := TOutputDescription.create( EpiTocUDirAll, 'tocUDirAll', tr( 'Number of traces (forward or back) of direct contact that originate at units of the designated type over the course of an iteration' ), true, false );
      self.Add( integer( EpiTocUDirAll ), d );

      d := TOutputDescription.create( EpiTocUIndAll, 'tocUIndAll', tr( 'Number of traces (forward or back) of direct contact that originate at units of the designated type over the course of an iteration' ), true, false );
      self.Add( integer( EpiTocUIndAll ), d );

      d := TOutputDescription.create( EpiTocUAll, 'tocUAll', tr( 'Number of traces (forward or back) of contact (direct or indirect) that originate at units of the designated type over the course of an iteration' ), true, false );
      self.Add( integer( EpiTocUAll ), d );

      // Cumulative totals for detection
      d := TOutputDescription.create( EpiDetcUClin, 'detcUClin', tr( 'Number of units detected by clinical signs over the course of an iteration' ), false, false );
      self.Add( integer( EpiDetcUClin ), d );

      d := TOutputDescription.create( EpiDetcAClin, 'detcAClin', tr( 'Total number of animals in all units detected by clinical signs over the course of an iteration' ), false, false );
      self.Add( integer( EpiDetcAClin ), d );

      d := TOutputDescription.create( EpiDetcUTest, 'detcUTest', tr( 'Number of units detected by diagnostic testing over the course of the iteration.  This value includes true- as well as false-positive units' ), false, false );
      self.Add( integer( EpiDetcUTest ), d );

      d := TOutputDescription.create( EpiDetcATest, 'detcATest', tr( 'Total number of animals in units detected by diagnostic testing over the course of the iteration' ), false, false );
      self.Add( integer( EpiDetcATest ), d );

      d := TOutputDescription.create( EpiDetcUAll, 'detcUAll', tr( 'Total number of units detected, either by diagnostic testing or by clinical signs, over the course of the iteration' ), true, false );
      self.Add( integer( EpiDetcUAll ), d );

      d := TOutputDescription.create( EpiDetcAAll, 'detcAAll', tr( 'Total number of animals in units detected, either by diagnostic testing or by clinical signs, over the course of the iteration' ), true, false );
      self.Add( integer( EpiDetcAAll ), d );

      // Cumulative totals for destruction
      d := TOutputDescription.create( EpiDescUIni, 'descUIni', tr( 'Number of units destroyed prior to the start of the simulation' ), false, false );
      self.Add( integer( EpiDescUIni ), d );

      d := TOutputDescription.create( EpiDescAIni, 'descAIni', tr( 'Total number of animals in units destroyed prior to the start of the simulation' ), false, false );
      self.Add( integer( EpiDescAIni ), d );

      d := TOutputDescription.create( EpiDescUDet, 'descUDet', tr( 'Number of units destroyed because disease was detected over the course of an iteration' ), false, false );
      self.Add( integer( EpiDescUDet ), d );

      d := TOutputDescription.create( EpiDescADet, 'descADet', tr( 'Total number of animals in units destroyed because disease was detected over the course of an iteration' ), false, false );
      self.Add( integer( EpiDescADet ), d );

      d := TOutputDescription.create( EpiDescUDirFwd, 'descUDirFwd', tr( 'Number of units destroyed due to a successful trace forward of direct contact with an infected unit over the course of the iteration' ), false, false );
      self.Add( integer( EpiDescUDirFwd ), d );

      d := TOutputDescription.create( EpiDescADirFwd, 'descADirFwd', tr( 'Total number of animals in units destroyed due to a successful trace forward of direct contact with an infected unit over the course of the iteration' ), false, false );
      self.Add( integer( EpiDescADirFwd ), d );

      d := TOutputDescription.create( EpiDescUIndFwd, 'descUIndFwd', tr( 'Number of units destroyed due to a successful trace forward of indirect contact with an infected unit over the course of the iteration' ), false, false );
      self.Add( integer( EpiDescUIndFwd ), d );

      d := TOutputDescription.create( EpiDescAIndFwd, 'descAIndFwd', tr( 'Total number of animals in units destroyed due to a successful trace forward of indirect contact with an infected unit over the course of the iteration' ), false, false );
      self.Add( integer( EpiDescAIndFwd ), d );

      d := TOutputDescription.create( EpiDescURing, 'descURing', tr( 'Number of units destroyed because they were in a destruction ring over the course of an iteration' ), false, false );
      self.Add( integer( EpiDescURing ), d );

      d := TOutputDescription.create( EpiDescARing, 'descARing', tr( 'Total number of animals in units destroyed because they were in a destruction ring over the course of an iteration' ), false, false );
      self.Add( integer( EpiDescARing ), d );

      d := TOutputDescription.create( EpiDescUAll, 'descUAll', tr( 'Number of units destroyed for any reason over the course of an iteration' ) + ' ' + tr ( '(not including initially destroyed units)' ), true, false );
      self.Add( integer( EpiDescUAll ), d );

      d := TOutputDescription.create( EpiDescAAll, 'descAAll', tr( 'Total number of animals in units destroyed for any reason over the course of an iteration' ) + ' ' + tr ( '(not including initially destroyed units)' ), true, false );
      self.Add( integer( EpiDescAAll ), d );

      d := TOutputDescription.create( EpiDescUDirBack, 'descUDirBack', tr( 'Number of units destroyed due to a successful trace back of direct contact with an infected unit over the course of the iteration' ), false, false );
      self.Add( integer( EpiDescUDirBack ), d );

      d := TOutputDescription.create( EpiDescADirBack, 'descADirBack', tr( 'Total number of animals in units destroyed due to a successful trace back of direct contact with an infected unit over the course of the iteration' ), false, false );
      self.Add( integer( EpiDescADirBack ), d );

      d := TOutputDescription.create( EpiDescUIndBack, 'descUIndBack', tr( 'Number of units destroyed due to a successful trace back of indirect contact with an infected unit over the course of the iteration' ), false, false );
      self.Add( integer( EpiDescUIndBack ), d );

      d := TOutputDescription.create( EpiDescAIndBack, 'descAIndBack', tr( 'Total number of animals in units destroyed due to a successful trace back of indirect contact with an infected unit over the course of the iteration' ), false, false );
      self.Add( integer( EpiDescAIndBack ), d );

      // Final outputs for destruction queue
      d := TOutputDescription.create( EpiDeswUMax, 'deswUMax', tr( 'Maximum number of units in queue for destruction on any given day over the course of the iteration' ), false, false );
      self.Add( integer( EpiDeswUMax ), d );

      d := TOutputDescription.create( EpiDeswAMax, 'deswAMax', tr( 'Maximum number of animals in queue for destruction on any given day over the course of the iteration' ), false, false );
      self.Add( integer( EpiDeswAMax ), d );

      d := TOutputDescription.create( EpiDeswUMaxDay, 'deswUMaxDay', tr( 'The first simulation day on which the maximum number of units in queue for destruction was reached' ), false, false );
      self.Add( integer( EpiDeswUMaxDay ), d );

      d := TOutputDescription.create( EpiDeswAMaxDay, 'deswAMaxDay', tr( 'The first simulation day on which the maximum number of animals in queue for destruction was reached' ), false, false );
      self.Add( integer( EpiDeswAMaxDay ), d );

      d := TOutputDescription.create( EpiDeswUTimeMax, 'deswUTimeMax', tr( 'Maximum number of days spent in queue for destruction by any single unit over the course of the iteration' ), false, false );
      self.Add( integer( EpiDeswUTimeMax ), d );

      d := TOutputDescription.create( EpiDeswUTimeAvg, 'deswUTimeAvg', tr( 'Average number of days spent by each unit in queue for destruction over the course of the iteration' ), false, false );
      self.Add( integer( EpiDeswUTimeAvg ), d );

      d := TOutputDescription.create( EpiDeswUDaysInQueue, 'deswUDaysInQueue', tr( 'Total number of unit-days spent in queue for destruction' ), false, false );
      self.Add( integer( EpiDeswUDaysInQueue ), d );

      d := TOutputDescription.create( EpiDeswADaysInQueue, 'deswADaysInQueue', tr( 'Total number of animal-days spent in queue for destruction' ), false, false );
      self.Add( integer( EpiDeswADaysInQueue ), d );

      // Cumulative totals for vaccination
      d := TOutputDescription.create( EpiVaccUIni, 'vaccUIni', tr( 'Number of units that were vaccine immune prior to the start of the simulation' ), false, false );
      self.Add( integer( EpiVaccUIni ), d );

      d := TOutputDescription.create( EpiVaccAIni, 'vaccAIni', tr( 'Total number of animals in units that were vaccine immune prior to the start of the simulation' ), false, false );
      self.Add( integer( EpiVaccAIni ), d );

      d := TOutputDescription.create( EpiVaccURing, 'vaccURing', tr( 'Number of units vaccinated in rings around detected-infected units over the course of an iteration' ), false, false );
      self.Add( integer( EpiVaccURing ), d );

      d := TOutputDescription.create( EpiVaccARing, 'vaccARing', tr( 'Total number of animals in all units vaccinated in rings around detected-infected units over the course of an iteration' ), false, false );
      self.Add( integer( EpiVaccARing ), d );

      d := TOutputDescription.create( EpiVaccUAll, 'vaccUAll', tr( 'Number of units vaccinated for any reason over the course of an iteration' ) + ' ' + tr ( '(not including initially vaccinated units)' ), true, false );
      self.Add( integer( EpiVaccUAll ), d );

      d := TOutputDescription.create( EpiVaccAAll, 'vaccAAll', tr( 'Total number of animals in all units vaccinated for any reason over the course of an iteration' ) + ' ' + tr ( '(not including initially vaccinated units)' ), true, false );
      self.Add( integer( EpiVaccAAll ), d );

      // Final outputs for vaccination queue
      d := TOutputDescription.create( EpiVacwUMax, 'vacwUMax', tr( 'Maximum number of units in queue for vaccination on any given day over the course of the iteration' ), false, false );
      self.Add( integer( EpiVacwUMax ), d );

      d := TOutputDescription.create( EpiVacwAMax, 'vacwAMax', tr( 'Maximum number of animals in queue for vaccination on any given day over the course of the iteration' ), false, false );
      self.Add( integer( EpiVacwAMax ), d );

      d := TOutputDescription.create( EpiVacwUMaxDay, 'vacwUMaxDay', tr( 'The first simulation day on which the maximum number of units in queue for vaccination was reached' ), false, false );
      self.Add( integer( EpiVacwUMaxDay ), d );

      d := TOutputDescription.create( EpiVacwAMaxDay, 'vacwAMaxDay', tr( 'The first simulation day on which the maximum number of animals in queue for vaccination was reached' ), false, false );
      self.Add( integer( EpiVacwAMaxDay ), d );

      d := TOutputDescription.create( EpiVacwUTimeMax, 'vacwUTimeMax', tr( 'Maximum number of days spent in queue for vaccination by any single unit over the course of the iteration' ), false, false );
      self.Add( integer( EpiVacwUTimeMax ), d );

      d := TOutputDescription.create( EpiVacwUTimeAvg, 'vacwUTimeAvg', tr( 'Average number of days spent in queue for vaccination by each unit that was vaccinated over the course of the iteration' ), false, false );
      self.Add( integer( EpiVacwUTimeAvg ), d );

      // For NAADSM 3.x, and for at least the time being, these two outputs are no longer saved.
      (*
      d := TOutputDescription.create( EpiVacwUDaysInQueue, 'vacwUDaysInQueue', tr( 'Total number of unit-days spent in queue for vaccination' ), false, false );
      self.Add( integer( EpiVacwUDaysInQueue ), d );

      d := TOutputDescription.create( EpiVacwADaysInQueue, 'vacwADaysInQueue', tr( 'Total number of animal-days spent in queue for vaccination' ), false, false );
      self.Add( integer( EpiVacwADaysInQueue ), d );
      *)

      // Cumulative totals for herd exams
      d := TOutputDescription.create( EpiExmcUDirFwd, 'exmcUDirFwd', tr( 'Number of units subjected to a herd exam after a trace-forward of direct contact over the course of the iteration' ), false, false );
      self.Add( integer( EpiExmcUDirFwd ), d );

      d := TOutputDescription.create( EpiExmcADirFwd, 'exmcADirFwd', tr( 'Total number of animals in units subjected to a herd exam after a trace-forward of direct contact over the course of the iteration' ), false, false );
      self.Add( integer( EpiExmcADirFwd ), d );

      d := TOutputDescription.create( EpiExmcUIndFwd, 'exmcUIndFwd', tr( 'Number of units subjected to a herd exam after a trace-forward of indirect contact over the course of the iteration' ), false, false );
      self.Add( integer( EpiExmcUIndFwd ), d );

      d := TOutputDescription.create( EpiExmcAIndFwd, 'exmcAIndFwd', tr( 'Total number of animals in units subjected to a herd exam after a trace-forward of indirect contact over the course of the iteration' ), false, false );
      self.Add( integer( EpiExmcAIndFwd ), d );

      d := TOutputDescription.create( EpiExmcUDirBack, 'exmcUDirBack', tr( 'Number of units subjected to a herd exam after a trace-back of direct contact over the course of the iteration' ), false, false );
      self.Add( integer( EpiExmcUDirBack ), d );

      d := TOutputDescription.create( EpiExmcADirBack, 'exmcADirBack', tr( 'Total number of animals in units subjected to a herd exam after a trace-back of direct contact over the course of the iteration' ), false, false );
      self.Add( integer( EpiExmcADirBack ), d );

      d := TOutputDescription.create( EpiExmcUIndBack, 'exmcUIndBack', tr( 'Number of units subjected to a herd exam after a trace-back of indirect contact over the course of the iteration' ), false, false );
      self.Add( integer( EpiExmcUIndBack ), d );

      d := TOutputDescription.create( EpiExmcAIndBack, 'exmcAIndBack', tr( 'Total number of animals in units subjected to a herd exam after a trace-back of indirect contact over the course of the iteration' ), false, false );
      self.Add( integer( EpiExmcAIndBack ), d );

      d := TOutputDescription.create( EpiExmcUDirAll, 'exmcUDirAll', tr( 'Total number of units subjected to a herd exam after a trace-forward or trace-back after direct contact over the course of the iteration' ), true, false );
      self.Add( integer( EpiExmcUDirAll ), d );

      d := TOutputDescription.create( EpiExmcADirAll, 'exmcADirAll', tr( 'Total number of animals in units subjected to a herd exam after a trace-forward or trace-back after direct contact over the course of the iteration' ), true, false );
      self.Add( integer( EpiExmcADirAll ), d );

      d := TOutputDescription.create( EpiExmcUIndAll, 'exmcUIndAll', tr( 'Total number of units subjected to a herd exam after a trace-forward or trace-back after indirect contact over the course of the iteration' ), true, false );
      self.Add( integer( EpiExmcUIndAll ), d );

      d := TOutputDescription.create( EpiExmcAIndAll, 'exmcAIndAll', tr( 'Total number of animals in units subjected to a herd exam after a trace-forward or trace-back after indirect contact over the course of the iteration' ), true, false );
      self.Add( integer( EpiExmcAIndAll ), d );

      d := TOutputDescription.create( EpiExmcUAll, 'exmcUAll', tr( 'Total number of units subjected to a herd exam after a trace-forward or trace-back after contact (either direct or indirect) over the course of the iteration' ), true, false );
      self.Add( integer( EpiExmcUAll ), d );

      d := TOutputDescription.create( EpiExmcAAll, 'exmcAAll', tr( 'Total number of animals in units subjected to a herd exam after a trace-forward or trace-back after contact (either direct or indirect) over the course of the iteration' ), true, false );
      self.Add( integer( EpiExmcAAll ), d );

      // Cumulative totals for diagnostic testing
      d := TOutputDescription.create( EpiTstcUDirFwd, 'tstcUDirFwd', tr( 'Number of units subjected to diagnostic testing after a trace-forward of direct contact over the course of the iteration' ), false, false );
      self.Add( integer( EpiTstcUDirFwd ), d );

      d := TOutputDescription.create( EpiTstcADirFwd, 'tstcADirFwd', tr( 'Total number of animals in units subjected to diagnostic testing after a trace-forward of direct contact over the course of the iteration' ), false, false );
      self.Add( integer( EpiTstcADirFwd ), d );

      d := TOutputDescription.create( EpiTstcUIndFwd, 'tstcUIndFwd', tr( 'Number of units subjected to diagnostic testing after a trace-forward of indirect contact over the course of the iteration' ), false, false );
      self.Add( integer( EpiTstcUIndFwd ), d );

      d := TOutputDescription.create( EpiTstcAIndFwd, 'tstcAIndFwd', tr( 'Total number of animals in units subjected to diagnostic testing after a trace-forward of indirect contact over the course of the iteration' ), false, false );
      self.Add( integer( EpiTstcAIndFwd ), d );

      d := TOutputDescription.create( EpiTstcUDirBack, 'tstcUDirBack', tr( 'Number of units subjected to diagnostic testing after a trace-back of direct contact over the course of the iteration' ), false, false );
      self.Add( integer( EpiTstcUDirBack ), d );

      d := TOutputDescription.create( EpiTstcADirBack, 'tstcADirBack', tr( 'Total number of animals in units subjected to diagnostic testing after a trace-back of direct contact over the course of the iteration' ), false, false );
      self.Add( integer( EpiTstcADirBack ), d );

      d := TOutputDescription.create( EpiTstcUIndBack, 'tstcUIndBack', tr( 'Number of units subjected to diagnostic testing after a trace-back of indirect contact over the course of the iteration' ), false, false );
      self.Add( integer( EpiTstcUIndBack ), d );

      d := TOutputDescription.create( EpiTstcAIndBack, 'tstcAIndBack', tr( 'Total number of animals in units subjected to diagnostic testing after a trace-back of indirect contact over the course of the iteration' ), false, false );
      self.Add( integer( EpiTstcAIndBack ), d );

      d := TOutputDescription.create( EpiTstcUDirAll, 'tstcUDirAll', tr( 'Total number of units subjected to diagnostic testing after a trace-forward or trace-back of direct contact over the course of the iteration' ), true, false );
      self.Add( integer( EpiTstcUDirAll ), d );

      d := TOutputDescription.create( EpiTstcADirAll, 'tstcADirAll', tr( 'Total number of animals in units subjected to diagnostic testing after a trace-forward or trace-back of direct contact over the course of the iteration' ), true, false );
      self.Add( integer( EpiTstcADirAll ), d );

      d := TOutputDescription.create( EpiTstcUIndAll, 'tstcUIndAll', tr( 'Total number of units subjected to diagnostic testing after a trace-forward or trace-back of indirect contact over the course of the iteration' ), true, false );
      self.Add( integer( EpiTstcUIndAll ), d );

      d := TOutputDescription.create( EpiTstcAIndAll, 'tstcAIndAll', tr( 'Total number of animals in units subjected to diagnostic testing after a trace-forward or trace-back of indirect contact over the course of the iteration' ), true, false );
      self.Add( integer( EpiTstcAIndAll ), d );

      d := TOutputDescription.create( EpiTstcUAll, 'tstcUAll', tr( 'Total number of units subjected to diagnostic testing after a trace-forward or trace-back of contact (either direct or indirect) over the course of the iteration' ), true, false );
      self.Add( integer( EpiTstcUAll ), d );

      d := TOutputDescription.create( EpiTstcAAll, 'tstcAAll', tr( 'Total number of animals in units ubjected to diagnostic testing after a trace-forward or trace-back of contact (either direct or indirect) over the course of the iteration' ), true, false );
      self.Add( integer( EpiTstcAAll ), d );

      d := TOutputDescription.create( EpiTstcUTruePos, 'tstcUTruePos', tr( 'Number of tested units with a true positive diagnostic test result over the course of the iteration' ), false, false );
      self.Add( integer( EpiTstcUTruePos ), d );

      d := TOutputDescription.create( EpiTstcATruePos, 'tstcATruePos', tr( 'Total number of animals in tested units with a true positive diagnostic test result over the course of the iteration' ), false, false );
      self.Add( integer( EpiTstcATruePos ), d );

      d := TOutputDescription.create( EpiTstcUTrueNeg, 'tstcUTrueNeg', tr( 'Number of tested units with a true negative diagnostic test result over the course of the iteration' ), false, false );
      self.Add( integer( EpiTstcUTrueNeg ), d );

      d := TOutputDescription.create( EpiTstcATrueNeg, 'tstcATrueNeg', tr( 'Total number of animals in tested units with a true negative diagnostic test result over the course of the iteration' ), false, false );
      self.Add( integer( EpiTstcATrueNeg ), d );

      d := TOutputDescription.create( EpiTstcUFalsePos, 'tstcUFalsePos', tr( 'Number of tested units with a false positive diagnostic test result over the course of the iteration' ), false, false );
      self.Add( integer( EpiTstcUFalsePos ), d );

      d := TOutputDescription.create( EpiTstcAFalsePos, 'tstcAFalsePos', tr( 'Total number of animals in tested units with a false positive diagnostic test result over the course of the iteration' ), false, false );
      self.Add( integer( EpiTstcAFalsePos ), d );

      d := TOutputDescription.create( EpiTstcUFalseNeg, 'tstcUFalseNeg', tr( 'Number of tested units with a false negative diagnostic test result over the course of the iteration' ), false, false );
      self.Add( integer( EpiTstcUFalseNeg ), d );
      
      d := TOutputDescription.create( EpiTstcAFalseNeg, 'tstcAFalseNeg', tr( 'Total number of animals in tested units with a false negative diagnostic test result over the course of the iteration' ), false, false );
      self.Add( integer( EpiTstcAFalseNeg ), d );

      // Cumulative totals for zone foci
      d := TOutputDescription.create( EpiZoncFoci, 'zoncFoci', tr( 'Total number of new zone foci created around units of the indicated type over the course of an iteration' ), false, false );
      self.Add( integer( EpiZoncFoci ), d );

      // First events
      d := TOutputDescription.create( EpiDetOccurred, 'detOccurred', tr( 'Number of iterations in which infected units were detected' ), true, false );
      self.Add( integer( EpiDetOccurred ), d );

      d := TOutputDescription.create( EpiFirstDetection, 'firstDetection', tr( 'Day of first detection of an infected unit in the specified iteration' ), false, true );
      self.Add( integer( EpiFirstDetection ), d );

      d := TOutputDescription.create( EpiFirstDetUInf, 'firstDetUInf', tr( 'Number of units already infected at the time of first detection of an infected unit of any production type in the specified iteration' ), false, true );
      self.Add( integer( EpiFirstDetUInf ), d );

      d := TOutputDescription.create( EpiFirstDetAInf, 'firstDetAInf', tr( 'Number of animals in units already infected at the time of first detection of an infected unit of any production type in the specified iteration' ), false, true );
      self.Add( integer( EpiFirstDetAInf ), d );

      d := TOutputDescription.create( EpiLastDetection, 'lastDetection', tr( 'Day of last detection of an infected unit in the specified iteration' ), false, true );
      self.Add( integer( EpiLastDetection ), d );

      d := TOutputDescription.create( EpiVaccOccurred, 'vaccOccurred', tr( 'Number of iterations in which vaccination occurred during the simulated time period' ), true, false );
      self.Add( integer( EpiVaccOccurred ), d );

      d := TOutputDescription.create( EpiFirstVaccination, 'firstVaccination', tr( 'Day of first vaccination of a unit in the specified iteration' ), false, true );
      self.Add( integer( EpiFirstVaccination ), d );

      d := TOutputDescription.create( EpiDestrOccurred, 'destrOccurred', tr( 'Number of iterations in which destruction occurred during the simulated time period' ), true, false );
      self.Add( integer( EpiDestrOccurred ), d );

      d := TOutputDescription.create( EpiFirstDestruction, 'firstDestruction', tr( 'Day of first destruction of a unit in the specified iteration' ), false, true );
      self.Add( integer( EpiFirstDestruction ), d );

      // Outbreak duration
      d := TOutputDescription.create( EpiDiseaseEnded, 'diseaseEnded', tr( 'Number of iterations in which the active disease phase ended' ), true, false );
      self.Add( integer( EpiDiseaseEnded ), d );

      d := TOutputDescription.create( EpiDiseaseDuration, 'diseaseDuration', tr( 'Duration of the active disease phase in the specified iteration' ), true , true );
      self.Add( integer( EpiDiseaseDuration ), d );

      d := TOutputDescription.create( EpiOutbreakEnded, 'outbreakEnded', tr( 'Number of iterations in which the outbreak ended' ), true, false );
      self.Add( integer( EpiOutbreakEnded ), d );

      d := TOutputDescription.create( EpiOutbreakDuration, 'outbreakDuration', tr( 'Duration of the outbreak in the specified iteration' ), true, true );
      self.Add( integer( EpiOutbreakDuration ), d );
    end
  ;


  destructor TOutputDescriptionList.destroy();
    begin
      deleteValues();
      inherited destroy();
    end
  ;


  function TOutputDescriptionList.item( const outputVariable: TIterationOutputType ): TOutputDescription;
    begin
      result := self.value( integer( outputVariable ) ) as TOutputDescription;
    end
  ;


end.