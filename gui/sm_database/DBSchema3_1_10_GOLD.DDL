--    This SQL DDL script was generated by Microsoft Visual Studio (Release Date: LOCAL BUILD).

--    Driver Used : Microsoft Visual Studio - Microsoft Access Driver.
--    Document    : C:\Documents and Settings\apreeves\My Documents\NAADSM\DatabaseSchemas\DBSchema3_1_10_GOLD.vsd.
--    Time Created: April 17, 2007 12:59 PM.
--    Operation   : From Visio Update Database Wizard.
--    Connected data source : naadsm-3_1_x
--    Connected server      : ACCESS
--    Connected database    : C:\DOCUMENTS AND SETTINGS\APREEVES\My Documents\NAADSM\DatabaseSchemas\naadsm3_1_x.mdb



-- Update existing table `outIterationByZoneAndProductionType`.
create table `outIterationByZoneAndProductionType_IM0` ( 
	`iteration` INTEGER,
	`zoneID` INTEGER,
	`productionTypeID` INTEGER,
	`unitDays` LONG,
	`animalDays` LONG); 

insert into `outIterationByZoneAndProductionType_IM0` (`iteration`, `zoneID`, `productionTypeID`, `unitDays`, `animalDays`) 
    select `outIterationByZoneAndProductionType`.`iteration`, 
           `outIterationByZoneAndProductionType`.`zoneID`, 
           `outIterationByZoneAndProductionType`.`productionTypeID`, 
           `outIterationByZoneAndProductionType`.`unitDays`, 
           `outIterationByZoneAndProductionType`.`animalDays`  
    from `outIterationByZoneAndProductionType`;


alter table `outIterationByZoneAndProductionType`
	drop constraint `inZone_outIterationByZoneAndProductionType_FK1`;


alter table `outIterationByZoneAndProductionType`
	drop constraint `inProductionType_outIterationByZoneAndProductionType_FK1`;


drop table `outIterationByZoneAndProductionType`; 


-- `outIterationByZoneAndProductionType` : Table of outIterationByZoneAndProductionType
-- 	`iteration` : iteration identifies outIterationByZoneAndProductionType
-- 	`zoneID` : zoneID partly identifies outIterationByZoneAndProductionType
-- 	`productionTypeID` : productionTypeID partly identifies outIterationByZoneAndProductionType
-- 	`unitDays` : unitDays is of outIterationByZoneAndProductionType
-- 	`animalDays` : animalDays is of outIterationByZoneAndProductionType
-- 	`costSurveillance` : costSurveillance is of outIterationByZoneAndProductionType  
create table `outIterationByZoneAndProductionType` ( 
	`iteration` INTEGER,
	`zoneID` INTEGER,
	`productionTypeID` INTEGER,
	`unitDays` LONG,
	`animalDays` LONG,
	`costSurveillance` CURRENCY); 

-- Note: Access does not support the mandatory (not null) constraint in SQL DDL.
-- The mandatory constraints for table '`outIterationByZoneAndProductionType`' will be created in the database using DAO
-- only if there is a connection to the Access MDB file during DDL generation.
-- Not nullable columns for table 'outIterationByZoneAndProductionType' :
-- 	`iteration`
-- 	`zoneID`
-- 	`productionTypeID`


insert into `outIterationByZoneAndProductionType` (`iteration`, `zoneID`, `productionTypeID`, `unitDays`, `animalDays`) 
    select `outIterationByZoneAndProductionType_IM0`.`iteration`, 
           `outIterationByZoneAndProductionType_IM0`.`zoneID`, 
           `outIterationByZoneAndProductionType_IM0`.`productionTypeID`, 
           `outIterationByZoneAndProductionType_IM0`.`unitDays`, 
           `outIterationByZoneAndProductionType_IM0`.`animalDays`  
    from `outIterationByZoneAndProductionType_IM0`;


drop table `outIterationByZoneAndProductionType_IM0`; 


alter table `outIterationByZoneAndProductionType`
	add constraint `outIterationByZoneAndProductionType_PK` primary key (`iteration`, `zoneID`, `productionTypeID`) ; 


-- Update existing table `inProductionType`.
create table `inProductionType_IM0` ( 
	`productionTypeID` COUNTER,
	`descr` VARCHAR(255),
	`useDiseaseTransition` BIT,
	`disLatentPeriodPdfID` INTEGER,
	`disSubclinicalPeriodPdfID` INTEGER,
	`disClinicalPeriodPdfID` INTEGER,
	`disImmunePeriodPdfID` INTEGER,
	`disPrevalenceRelID` INTEGER,
	`useDetection` BIT,
	`detProbObsVsTimeClinicalRelID` INTEGER,
	`detProbReportVsFirstDetectionRelID` INTEGER,
	`traceDirect` BIT,
	`traceDirectSuccess` DOUBLE,
	`traceDirectTracePeriod` INTEGER,
	`traceIndirect` BIT,
	`traceIndirectSuccess` DOUBLE,
	`traceIndirectTracePeriod` INTEGER,
	`useBasicDestruction` BIT,
	`destrIsRingTrigger` BIT,
	`destrRingRadius` DOUBLE,
	`destrIsRingTarget` BIT,
	`destrDirectTraces` BIT,
	`destrIndirectTraces` BIT,
	`destrPriority` INTEGER,
	`useVaccination` BIT,
	`vaccMinTimeBetweenVaccinations` INTEGER,
	`vaccDaysToImmunity` INTEGER,
	`vaccImmunePeriodPdfID` INTEGER,
	`vaccRing` BIT,
	`vaccRingRadius` DOUBLE,
	`vaccPriority` INTEGER,
	`useCosts` BIT,
	`costDestrAppraisalPerUnit` DOUBLE,
	`costDestrCleaningPerUnit` DOUBLE,
	`costDestrEuthanasiaPerAnimal` DOUBLE,
	`costDestrIndemnificationPerAnimal` DOUBLE,
	`costDestrDisposalPerAnimal` DOUBLE,
	`costVaccSetupPerUnit` DOUBLE,
	`costVaccThreshold` INTEGER,
	`costVaccBaselinePerAnimal` DOUBLE,
	`costVaccAdditionalPerAnimal` DOUBLE,
	`zoneDetectionIsTrigger` BIT,
	`zoneDirectTraceIsTrigger` BIT,
	`zoneIndirectTraceIsTrigger` BIT); 

insert into `inProductionType_IM0` ( `productionTypeID`, `descr`, `useDiseaseTransition`, `disLatentPeriodPdfID`, `disSubclinicalPeriodPdfID`, `disClinicalPeriodPdfID`, `disImmunePeriodPdfID`, `disPrevalenceRelID`, `useDetection`, `detProbObsVsTimeClinicalRelID`, `detProbReportVsFirstDetectionRelID`, `traceDirect`, `traceDirectSuccess`, `traceDirectTracePeriod`, `traceIndirect`, `traceIndirectSuccess`, `traceIndirectTracePeriod`, `useBasicDestruction`, `destrIsRingTrigger`, `destrRingRadius`, `destrIsRingTarget`, `destrDirectTraces`, `destrIndirectTraces`, `destrPriority`, `useVaccination`, `vaccMinTimeBetweenVaccinations`, `vaccDaysToImmunity`, `vaccImmunePeriodPdfID`, `vaccRing`, `vaccRingRadius`, `vaccPriority`, `useCosts`, `costDestrAppraisalPerUnit`, `costDestrCleaningPerUnit`, `costDestrEuthanasiaPerAnimal`, `costDestrIndemnificationPerAnimal`, `costDestrDisposalPerAnimal`, `costVaccSetupPerUnit`, `costVaccThreshold`, `costVaccBaselinePerAnimal`, `costVaccAdditionalPerAnimal`, `zoneDetectionIsTrigger`, `zoneDirectTraceIsTrigger`, `zoneIndirectTraceIsTrigger`) 
    select `inProductionType`.`productionTypeID`, 
           `inProductionType`.`descr`, 
           `inProductionType`.`useDiseaseTransition`, 
           `inProductionType`.`disLatentPeriodPdfID`, 
           `inProductionType`.`disSubclinicalPeriodPdfID`, 
           `inProductionType`.`disClinicalPeriodPdfID`, 
           `inProductionType`.`disImmunePeriodPdfID`, 
           `inProductionType`.`disPrevalenceRelID`, 
           `inProductionType`.`useDetection`, 
           `inProductionType`.`detProbObsVsTimeClinicalRelID`, 
           `inProductionType`.`detProbReportVsFirstDetectionRelID`, 
           `inProductionType`.`traceDirect`, 
           `inProductionType`.`traceDirectSuccess`, 
           `inProductionType`.`traceDirectTracePeriod`, 
           `inProductionType`.`traceIndirect`, 
           `inProductionType`.`traceIndirectSuccess`, 
           `inProductionType`.`traceIndirectTracePeriod`, 
           `inProductionType`.`useBasicDestruction`, 
           `inProductionType`.`destrIsRingTrigger`, 
           `inProductionType`.`destrRingRadius`, 
           `inProductionType`.`destrIsRingTarget`, 
           `inProductionType`.`destrDirectTraces`, 
           `inProductionType`.`destrIndirectTraces`, 
           `inProductionType`.`destrPriority`, 
           `inProductionType`.`useVaccination`, 
           `inProductionType`.`vaccMinTimeBetweenVaccinations`, 
           `inProductionType`.`vaccDaysToImmunity`, 
           `inProductionType`.`vaccImmunePeriodPdfID`, 
           `inProductionType`.`vaccRing`, 
           `inProductionType`.`vaccRingRadius`, 
           `inProductionType`.`vaccPriority`, 
           `inProductionType`.`useCosts`, 
           `inProductionType`.`costDestrAppraisalPerUnit`, 
           `inProductionType`.`costDestrCleaningPerUnit`, 
           `inProductionType`.`costDestrEuthanasiaPerAnimal`, 
           `inProductionType`.`costDestrIndemnificationPerAnimal`, 
           `inProductionType`.`costDestrDisposalPerAnimal`, 
           `inProductionType`.`costVaccSetupPerUnit`, 
           `inProductionType`.`costVaccThreshold`, 
           `inProductionType`.`costVaccBaselinePerAnimal`, 
           `inProductionType`.`costVaccAdditionalPerAnimal`, 
           `inProductionType`.`zoneDetectionIsTrigger`, 
           `inProductionType`.`zoneDirectTraceIsTrigger`, 
           `inProductionType`.`zoneIndirectTraceIsTrigger`  
    from `inProductionType`;


alter table `dynHerd`
	drop constraint `inProductionType_dynHerd_FK1`;


alter table `inProductionTypePair`
	drop constraint `inProductionType_inProductionTypePairs_FK1`;


alter table `inProductionTypePair`
	drop constraint `inProductionType_inProductionTypePairs_FK2`;


alter table `outDailyByProductionType`
	drop constraint `inProductionType_outDailyByProductionType_FK1`;


alter table `outIterationByProductionType`
	drop constraint `inProductionType_outIterationByProductionType_FK1`;


alter table `outEpidemicCurves`
	drop constraint `inProductionType_outEpidemicCurves_FK1`;


alter table `outIterationCosts`
	drop constraint `inProductionType_outIterationCosts_FK1`;


alter table `inZoneProductionTypePair`
	drop constraint `inProductionType_inZoneProductionTypePair_FK1`;


alter table `outDailyByZoneAndProductionType`
	drop constraint `inProductionType_outDailyByZoneAndProductionType_FK1`;


alter table `inProductionType`
	drop constraint `FK1`;


alter table `inProductionType`
	drop constraint `FK2`;


alter table `inProductionType`
	drop constraint `FK4`;


alter table `inProductionType`
	drop constraint `inChart_inProductionType_FK3`;


alter table `inProductionType`
	drop constraint `inChart_inProductionType_FK5`;


alter table `inProductionType`
	drop constraint `inChart_inProductionType_FK6`;


alter table `inProductionType`
	drop constraint `inChart_inProductionType_FK7`;


alter table `inProductionType`
	drop constraint `inChart_inProductionType_FK8`;


drop table `inProductionType`; 


-- `inProductionType` : Table of inProductionType
-- 	`productionTypeID` : A unique ID for each production type present in the scenario.
-- 	`descr` : A unique description of the production type.
-- 	`useDiseaseTransition` : Indicates whether units of this production type will undergo disease transition.
-- 	`disLatentPeriodPdfID` : ID number of the probability density function used to define the latent period for units of this production type.  This field references inChart.chartID.
-- 	`disSubclinicalPeriodPdfID` : ID number of the probability density function used to define the subclinical period for units of this production type.  This field references inChart.chartID.
-- 	`disClinicalPeriodPdfID` : ID number of the probability density function used to define the clinical period for units of this production type.  This field references inChart.chartID.
-- 	`disImmunePeriodPdfID` : ID number of the probability density function used to define the natural immune period for units of this production type.  This field references inChart.chartID.
-- 	`disPrevalenceRelID` : disPrevalenceRelID is of inProductionType
-- 	`useDetection` : Indicates whether disease detection will be modeled for units of this production type.
-- 	`detProbObsVsTimeClinicalRelID` : ID number of the relational function used to define the probability of observing clinical signs in units of this production type.  This field references inChart.chartID.
-- 	`detProbReportVsFirstDetectionRelID` : ID number of the relational function used to define the probabilty of reporting disease in units of this production type.  This field references inChart.chartID.
-- 	`traceDirect` : Indicates whether trace surveillance for direct contacts will be attempted.
-- 	`traceDirectSuccess` : The probability of success of identifying direct contacts by trace surveillance.
-- 	`traceDirectTracePeriod` : The period of time, in days, for which traceback surveillance of direct contacts will be attempted.
-- 	`traceIndirect` : Indicates whether trace surveillance for indirect contacts will be attempted.
-- 	`traceIndirectSuccess` : The probability of success of identifying indirect contacts by trace surveillance.
-- 	`traceIndirectTracePeriod` : The period of time, in days, for which traceback surveillance of indirect contacts will be attempted.
-- 	`useBasicDestruction` : Indicates whether detected clinical units of this production type will be destroyed.
-- 	`destrIsRingTrigger` : Indicates whether detection of a unit of this type will trigger the formation of destruction ring.
-- 	`destrRingRadius` : Radius, in kilometers, of the destruction ring.
-- 	`destrIsRingTarget` : Indicates whether units of this type will be subject to preemptive ring destruction.
-- 	`destrDirectTraces` : Indicates whether units of this type identified by traceback of direct contacts will be subject to preemptive destruction.
-- 	`destrIndirectTraces` : Indicates whether units of this type identified by traceback of indirect contacts will be subject to preemptive destruction.
-- 	`destrPriority` : The destruction priority of this production type, relative to other production types.  A lower number indicates higher priority.
-- 	`useVaccination` : Indicates whether units of this type will be subject to vaccination.
-- 	`vaccMinTimeBetweenVaccinations` : The minimum time, in days, between vaccinations for units of this production type.
-- 	`vaccDaysToImmunity` : The number of days required for the onset of vaccine immunity in a newly vaccinated unit of this type.
-- 	`vaccImmunePeriodPdfID` : ID number of the probability density function used to define the vaccine immune period for units of this production type.  This field references inChart.chartID.
-- 	`vaccRing` : Indicates whether detection of a clinical unit of this type will trigger a vaccination ring. 
-- 	`vaccRingRadius` : Radius, in kilometers, of the vaccination ring.
-- 	`vaccPriority` : The vaccination priority of this production type, relative to other production types. A lower number indicates higher priority.
-- 	`costDestrAppraisalPerUnit` : The cost associated with appraisal for each destroyed unit of this type.
-- 	`costDestrCleaningPerUnit` : The cost of cleaning and disinfection for each destroyed unit of this type.
-- 	`costDestrEuthanasiaPerAnimal` : The cost of euthanizing each destroyed animal of this type.
-- 	`costDestrIndemnificationPerAnimal` : The cost of indemnification for each destroyed animal of this type.
-- 	`costDestrDisposalPerAnimal` : The cost of carcass disposal for each destroyed animal of this type.
-- 	`costVaccSetupPerUnit` : The cost of site setup for each vaccinated unit of this type.
-- 	`costVaccThreshold` : The number of animals of this type that can be vaccinated before the cost of vaccination increases.
-- 	`costVaccBaselinePerAnimal` : The baseline cost of vaccination for each vaccinated animal of this type.  This cost applies to all vaccinations before the threshold set in costVaccThreshold is met.
-- 	`costVaccAdditionalPerAnimal` : The additional cost of vaccination for each vaccinated animal of this type, after the threshold is exceeded.
-- 	`zoneDetectionIsTrigger` : Indicates whether detection of units of this type will trigger the formation of a surveillance zone.
-- 	`zoneDirectTraceIsTrigger` : Indicates whether direct tracing of units of this type will trigger the formation of a surveillance zone.
-- 	`zoneIndirectTraceIsTrigger` : Indicates whether indirect tracing of units of this type will trigger the formation of a surveillance zone.  
create table `inProductionType` ( 
	`productionTypeID` COUNTER,
	`descr` VARCHAR(255),
	`useDiseaseTransition` BIT,
	`disLatentPeriodPdfID` INTEGER,
	`disSubclinicalPeriodPdfID` INTEGER,
	`disClinicalPeriodPdfID` INTEGER,
	`disImmunePeriodPdfID` INTEGER,
	`disPrevalenceRelID` INTEGER,
	`useDetection` BIT,
	`detProbObsVsTimeClinicalRelID` INTEGER,
	`detProbReportVsFirstDetectionRelID` INTEGER,
	`traceDirect` BIT,
	`traceDirectSuccess` DOUBLE,
	`traceDirectTracePeriod` INTEGER,
	`traceIndirect` BIT,
	`traceIndirectSuccess` DOUBLE,
	`traceIndirectTracePeriod` INTEGER,
	`useBasicDestruction` BIT,
	`destrIsRingTrigger` BIT,
	`destrRingRadius` DOUBLE,
	`destrIsRingTarget` BIT,
	`destrDirectTraces` BIT,
	`destrIndirectTraces` BIT,
	`destrPriority` INTEGER,
	`useVaccination` BIT,
	`vaccMinTimeBetweenVaccinations` INTEGER,
	`vaccDaysToImmunity` INTEGER,
	`vaccImmunePeriodPdfID` INTEGER,
	`vaccRing` BIT,
	`vaccRingRadius` DOUBLE,
	`vaccPriority` INTEGER,
	`costDestrAppraisalPerUnit` DOUBLE,
	`costDestrCleaningPerUnit` DOUBLE,
	`costDestrEuthanasiaPerAnimal` DOUBLE,
	`costDestrIndemnificationPerAnimal` DOUBLE,
	`costDestrDisposalPerAnimal` DOUBLE,
	`costVaccSetupPerUnit` DOUBLE,
	`costVaccThreshold` INTEGER,
	`costVaccBaselinePerAnimal` DOUBLE,
	`costVaccAdditionalPerAnimal` DOUBLE,
	`zoneDetectionIsTrigger` BIT,
	`zoneDirectTraceIsTrigger` BIT,
	`zoneIndirectTraceIsTrigger` BIT); 

insert into `inProductionType` (`productionTypeID`, `descr`, `useDiseaseTransition`, `disLatentPeriodPdfID`, `disSubclinicalPeriodPdfID`, `disClinicalPeriodPdfID`, `disImmunePeriodPdfID`, `disPrevalenceRelID`, `useDetection`, `detProbObsVsTimeClinicalRelID`, `detProbReportVsFirstDetectionRelID`, `traceDirect`, `traceDirectSuccess`, `traceDirectTracePeriod`, `traceIndirect`, `traceIndirectSuccess`, `traceIndirectTracePeriod`, `useBasicDestruction`, `destrIsRingTrigger`, `destrRingRadius`, `destrIsRingTarget`, `destrDirectTraces`, `destrIndirectTraces`, `destrPriority`, `useVaccination`, `vaccMinTimeBetweenVaccinations`, `vaccDaysToImmunity`, `vaccImmunePeriodPdfID`, `vaccRing`, `vaccRingRadius`, `vaccPriority`, `costDestrAppraisalPerUnit`, `costDestrCleaningPerUnit`, `costDestrEuthanasiaPerAnimal`, `costDestrIndemnificationPerAnimal`, `costDestrDisposalPerAnimal`, `costVaccSetupPerUnit`, `costVaccThreshold`, `costVaccBaselinePerAnimal`, `costVaccAdditionalPerAnimal`, `zoneDetectionIsTrigger`, `zoneDirectTraceIsTrigger`, `zoneIndirectTraceIsTrigger`) 
    select `inProductionType_IM0`.`productionTypeID`,
           `inProductionType_IM0`.`descr`, 
           `inProductionType_IM0`.`useDiseaseTransition`, 
           `inProductionType_IM0`.`disLatentPeriodPdfID`, 
           `inProductionType_IM0`.`disSubclinicalPeriodPdfID`, 
           `inProductionType_IM0`.`disClinicalPeriodPdfID`, 
           `inProductionType_IM0`.`disImmunePeriodPdfID`, 
           `inProductionType_IM0`.`disPrevalenceRelID`, 
           `inProductionType_IM0`.`useDetection`, 
           `inProductionType_IM0`.`detProbObsVsTimeClinicalRelID`, 
           `inProductionType_IM0`.`detProbReportVsFirstDetectionRelID`, 
           `inProductionType_IM0`.`traceDirect`, 
           `inProductionType_IM0`.`traceDirectSuccess`, 
           `inProductionType_IM0`.`traceDirectTracePeriod`, 
           `inProductionType_IM0`.`traceIndirect`, 
           `inProductionType_IM0`.`traceIndirectSuccess`, 
           `inProductionType_IM0`.`traceIndirectTracePeriod`, 
           `inProductionType_IM0`.`useBasicDestruction`, 
           `inProductionType_IM0`.`destrIsRingTrigger`, 
           `inProductionType_IM0`.`destrRingRadius`, 
           `inProductionType_IM0`.`destrIsRingTarget`, 
           `inProductionType_IM0`.`destrDirectTraces`, 
           `inProductionType_IM0`.`destrIndirectTraces`, 
           `inProductionType_IM0`.`destrPriority`, 
           `inProductionType_IM0`.`useVaccination`, 
           `inProductionType_IM0`.`vaccMinTimeBetweenVaccinations`, 
           `inProductionType_IM0`.`vaccDaysToImmunity`, 
           `inProductionType_IM0`.`vaccImmunePeriodPdfID`, 
           `inProductionType_IM0`.`vaccRing`, 
           `inProductionType_IM0`.`vaccRingRadius`, 
           `inProductionType_IM0`.`vaccPriority`, 
           `inProductionType_IM0`.`costDestrAppraisalPerUnit`, 
           `inProductionType_IM0`.`costDestrCleaningPerUnit`, 
           `inProductionType_IM0`.`costDestrEuthanasiaPerAnimal`, 
           `inProductionType_IM0`.`costDestrIndemnificationPerAnimal`, 
           `inProductionType_IM0`.`costDestrDisposalPerAnimal`, 
           `inProductionType_IM0`.`costVaccSetupPerUnit`, 
           `inProductionType_IM0`.`costVaccThreshold`, 
           `inProductionType_IM0`.`costVaccBaselinePerAnimal`, 
           `inProductionType_IM0`.`costVaccAdditionalPerAnimal`, 
           `inProductionType_IM0`.`zoneDetectionIsTrigger`, 
           `inProductionType_IM0`.`zoneDirectTraceIsTrigger`, 
           `inProductionType_IM0`.`zoneIndirectTraceIsTrigger`  
    from `inProductionType_IM0`;


drop table `inProductionType_IM0`; 


alter table `inProductionType`
	add constraint `inProductionType_PK` primary key (`productionTypeID`) ; 


-- Update existing table `inGeneral`.
create table `inGeneral_IM0` ( 
	`inGeneralID` VARCHAR(10),
	`scenarioDescr` LONGTEXT,
	`iterations` INTEGER,
	`days` INTEGER,
	`simStopReason` VARCHAR(25),
	`includeContactSpread` BIT,
	`includeAirborneSpread` BIT,
	`useAirborneExponentialDecay` BIT,
	`useWithinHerdPrevalence` BIT,
	`includeCosts` BIT,
	`costTrackDestruction` BIT,
	`costTrackVaccination` BIT,
	`costTrackZoneSurveillance` BIT,
	`useFixedRandomSeed` BIT,
	`randomSeed` INTEGER,
	`saveAllDailyOutputs` BIT,
	`saveDailyOutputsForIterations` INTEGER,
	`writeDailyStatesFile` BIT,
	`dailyStatesFileName` VARCHAR(255),
	`saveDailyEvents` BIT,
	`saveDailyExposures` BIT,
	`useCustomOutputs` BIT,
	`writeNAADSMapOutput` BIT,
	`NAADSMapDirectory` VARCHAR(255)); 

insert into `inGeneral_IM0` (`inGeneralID`, `scenarioDescr`, `iterations`, `days`, `simStopReason`, `includeContactSpread`, `includeAirborneSpread`, `useAirborneExponentialDecay`, `useWithinHerdPrevalence`, `includeCosts`, `costTrackDestruction`, `costTrackVaccination`, `costTrackZoneSurveillance`, `useFixedRandomSeed`, `randomSeed`, `saveAllDailyOutputs`, `saveDailyOutputsForIterations`, `writeDailyStatesFile`, `dailyStatesFileName`, `saveDailyEvents`, `saveDailyExposures`, `useCustomOutputs`, `writeNAADSMapOutput`, `NAADSMapDirectory`) 
    select `inGeneral`.`inGeneralID`, 
           `inGeneral`.`scenarioDescr`, 
           `inGeneral`.`iterations`, 
           `inGeneral`.`days`, 
           `inGeneral`.`simStopReason`, 
           `inGeneral`.`includeContactSpread`, 
           `inGeneral`.`includeAirborneSpread`, 
           `inGeneral`.`useAirborneExponentialDecay`, 
           `inGeneral`.`useWithinHerdPrevalence`, 
           `inGeneral`.`includeCosts`, 
           `inGeneral`.`costTrackDestruction`, 
           `inGeneral`.`costTrackVaccination`, 
           `inGeneral`.`costTrackZoneSurveillance`, 
           `inGeneral`.`useFixedRandomSeed`, 
           `inGeneral`.`randomSeed`, 
           `inGeneral`.`saveAllDailyOutputs`, 
           `inGeneral`.`saveDailyOutputsForIterations`, 
           `inGeneral`.`writeDailyStatesFile`, 
           `inGeneral`.`dailyStatesFileName`, 
           `inGeneral`.`saveDailyEvents`, 
           `inGeneral`.`saveDailyExposures`, 
           `inGeneral`.`useCustomOutputs`, 
           `inGeneral`.`writeNAADSMapOutput`, 
           `inGeneral`.`NAADSMapDirectory`  
    from `inGeneral`;


drop table `inGeneral`; 


-- `inGeneral` : Table of inGeneral
-- 	`inGeneralID` : A primary key for the single record stored in this table.  This value is always "NAADSMXXXX".
-- 	`scenarioDescr` : The description of the scenario.
-- 	`iterations` : The number of iterations of this scenario that should be run.
-- 	`days` : The number of days that iterations of this scenario should run, if the command Run->Start and run until specified day... is used.
-- 	`simStopReason` : The criterion used to end each iteration.  This may be that the specified number of days has passed, the first detection has occurred, or the outbreak has ended.
-- 	`includeContactSpread` : Indicates whether disease spread by direct or indirect contact is used in the scenario.
-- 	`includeAirborneSpread` : Indicates whether airborne spread is used in the model.
-- 	`useAirborneExponentialDecay` : Indicates whether exponential decay (TRUE) or linear decay (FALSE) is used with airborne spread.
-- 	`useWithinHerdPrevalence` : useWithinHerdPrevalence is of inGeneral
-- 	`costTrackDestruction` : costTrackDestruction is of inGeneral
-- 	`costTrackVaccination` : costTrackVaccination is of inGeneral
-- 	`costTrackZoneSurveillance` : costTrackZoneSurveillance is of inGeneral
-- 	`useFixedRandomSeed` : Indicates whether a specified seed value for the random number generator should be used.
-- 	`randomSeed` : The specified seed value for the random number generator.
-- 	`saveAllDailyOutputs` : Indicates whether daily outputs should be stored for every iteration.
-- 	`saveDailyOutputsForIterations` : The number of iterations for which daily outputs should be stored.  The minimum value is 3.
-- 	`writeDailyStatesFile` : Indicates whether a plain text file with the state of each unit on each day of each iteration should be written.
-- 	`dailyStatesFileName` : The file name of the plain text file described above.
-- 	`saveDailyEvents` : Indicates whether all events should be recorded in the scenario database.
-- 	`saveDailyExposures` : Indicates whether all exposures should be recorded in the scenario database.
-- 	`useCustomOutputs` : Indicates whether custom outputs should be generated.
-- 	`writeNAADSMapOutput` : writeNAADSMapOutput is of inGeneral
-- 	`NAADSMapDirectory` : NAADSMapDirectory is of inGeneral  
create table `inGeneral` ( 
	`inGeneralID` VARCHAR(10),
	`scenarioDescr` LONGTEXT,
	`iterations` INTEGER,
	`days` INTEGER,
	`simStopReason` VARCHAR(25),
	`includeContactSpread` BIT,
	`includeAirborneSpread` BIT,
	`useAirborneExponentialDecay` BIT,
	`useWithinHerdPrevalence` BIT,
	`costTrackDestruction` BIT,
	`costTrackVaccination` BIT,
	`costTrackZoneSurveillance` BIT,
	`useFixedRandomSeed` BIT,
	`randomSeed` INTEGER,
	`saveAllDailyOutputs` BIT,
	`saveDailyOutputsForIterations` INTEGER,
	`writeDailyStatesFile` BIT,
	`dailyStatesFileName` VARCHAR(255),
	`saveDailyEvents` BIT,
	`saveDailyExposures` BIT,
	`useCustomOutputs` BIT,
	`writeNAADSMapOutput` BIT,
	`NAADSMapDirectory` VARCHAR(255)); 

insert into `inGeneral` (`inGeneralID`, `scenarioDescr`, `iterations`, `days`, `simStopReason`, `includeContactSpread`, `includeAirborneSpread`, `useAirborneExponentialDecay`, `useWithinHerdPrevalence`, `costTrackDestruction`, `costTrackVaccination`, `costTrackZoneSurveillance`, `useFixedRandomSeed`, `randomSeed`, `saveAllDailyOutputs`, `saveDailyOutputsForIterations`, `writeDailyStatesFile`, `dailyStatesFileName`, `saveDailyEvents`, `saveDailyExposures`, `useCustomOutputs`, `writeNAADSMapOutput`, `NAADSMapDirectory`) 
    select `inGeneral_IM0`.`inGeneralID`, 
           `inGeneral_IM0`.`scenarioDescr`, 
           `inGeneral_IM0`.`iterations`, 
           `inGeneral_IM0`.`days`, 
           `inGeneral_IM0`.`simStopReason`, 
           `inGeneral_IM0`.`includeContactSpread`, 
           `inGeneral_IM0`.`includeAirborneSpread`, 
           `inGeneral_IM0`.`useAirborneExponentialDecay`, 
           `inGeneral_IM0`.`useWithinHerdPrevalence`, 
           `inGeneral_IM0`.`costTrackDestruction`, 
           `inGeneral_IM0`.`costTrackVaccination`, 
           `inGeneral_IM0`.`costTrackZoneSurveillance`, 
           `inGeneral_IM0`.`useFixedRandomSeed`, 
           `inGeneral_IM0`.`randomSeed`, 
           `inGeneral_IM0`.`saveAllDailyOutputs`, 
           `inGeneral_IM0`.`saveDailyOutputsForIterations`, 
           `inGeneral_IM0`.`writeDailyStatesFile`, 
           `inGeneral_IM0`.`dailyStatesFileName`, 
           `inGeneral_IM0`.`saveDailyEvents`, 
           `inGeneral_IM0`.`saveDailyExposures`, 
           `inGeneral_IM0`.`useCustomOutputs`, 
           `inGeneral_IM0`.`writeNAADSMapOutput`, 
           `inGeneral_IM0`.`NAADSMapDirectory`  
    from `inGeneral_IM0`;


drop table `inGeneral_IM0`; 


alter table `inGeneral`
	add constraint `inGeneral_PK` primary key (`inGeneralID`) ; 


create index `inGeneralID` on `inGeneral` (
	`inGeneralID` ASC); 


-- Restore foreign key constraints for existing tables
alter table `outIterationByZoneAndProductionType`
	add constraint `inZone_outIterationByZoneAndProductionType_FK1` foreign key (
		`zoneID`)
	 references `inZone` (
		`zoneID`); 

alter table `outIterationByZoneAndProductionType`
	add constraint `inProductionType_outIterationByZoneAndProductionType_FK1` foreign key (
		`productionTypeID`)
	 references `inProductionType` (
		`productionTypeID`); 

alter table `inProductionType`
	add constraint `FK1` foreign key (
		`disLatentPeriodPdfID`)
	 references `inChart` (
		`chartID`); 

alter table `inProductionType`
	add constraint `FK2` foreign key (
		`disSubclinicalPeriodPdfID`)
	 references `inChart` (
		`chartID`); 

alter table `inProductionType`
	add constraint `FK4` foreign key (
		`disImmunePeriodPdfID`)
	 references `inChart` (
		`chartID`); 

alter table `inProductionType`
	add constraint `inChart_inProductionType_FK3` foreign key (
		`disClinicalPeriodPdfID`)
	 references `inChart` (
		`chartID`); 

alter table `inProductionType`
	add constraint `inChart_inProductionType_FK5` foreign key (
		`detProbObsVsTimeClinicalRelID`)
	 references `inChart` (
		`chartID`); 

alter table `inProductionType`
	add constraint `inChart_inProductionType_FK6` foreign key (
		`detProbReportVsFirstDetectionRelID`)
	 references `inChart` (
		`chartID`); 

alter table `inProductionType`
	add constraint `inChart_inProductionType_FK7` foreign key (
		`vaccImmunePeriodPdfID`)
	 references `inChart` (
		`chartID`); 

alter table `inProductionType`
	add constraint `inChart_inProductionType_FK8` foreign key (
		`disPrevalenceRelID`)
	 references `inChart` (
		`chartID`); 

alter table `dynHerd`
	add constraint `inProductionType_dynHerd_FK1` foreign key (
		`productionTypeID`)
	 references `inProductionType` (
		`productionTypeID`); 

alter table `inProductionTypePair`
	add constraint `inProductionType_inProductionTypePairs_FK1` foreign key (
		`sourceProductionTypeID`)
	 references `inProductionType` (
		`productionTypeID`); 

alter table `inProductionTypePair`
	add constraint `inProductionType_inProductionTypePairs_FK2` foreign key (
		`destProductionTypeID`)
	 references `inProductionType` (
		`productionTypeID`); 

alter table `outDailyByProductionType`
	add constraint `inProductionType_outDailyByProductionType_FK1` foreign key (
		`productionTypeID`)
	 references `inProductionType` (
		`productionTypeID`); 

alter table `outIterationByProductionType`
	add constraint `inProductionType_outIterationByProductionType_FK1` foreign key (
		`productionTypeID`)
	 references `inProductionType` (
		`productionTypeID`); 

alter table `outEpidemicCurves`
	add constraint `inProductionType_outEpidemicCurves_FK1` foreign key (
		`productionTypeID`)
	 references `inProductionType` (
		`productionTypeID`); 

alter table `outIterationCosts`
	add constraint `inProductionType_outIterationCosts_FK1` foreign key (
		`productionTypeID`)
	 references `inProductionType` (
		`productionTypeID`); 

alter table `inZoneProductionTypePair`
	add constraint `inProductionType_inZoneProductionTypePair_FK1` foreign key (
		`productionTypeID`)
	 references `inProductionType` (
		`productionTypeID`); 

alter table `outDailyByZoneAndProductionType`
	add constraint `inProductionType_outDailyByZoneAndProductionType_FK1` foreign key (
		`productionTypeID`)
	 references `inProductionType` (
		`productionTypeID`); 


-- This is the end of the Microsoft Visual Studio generated SQL DDL script.
