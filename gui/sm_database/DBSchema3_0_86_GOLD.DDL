create table `inProductionType_IM0` ( 
	`productionTypeID` COUNTER,
	`descr` VARCHAR(255),
	`useDiseaseTransition` BIT,
	`disLatentPeriodPdfID` INTEGER,
	`disSubclinicalPeriodPdfID` INTEGER,
	`disClinicalPeriodPdfID` INTEGER,
	`disImmunePeriodPdfID` INTEGER,
	`useDetection` BIT,
	`detProbObsVsTimeClinicalRelID` INTEGER,
	`detProbReportVsFirstDetectionRelID` INTEGER,
	`survDirect` BIT,
	`survDirectSuccess` DOUBLE,
	`survDirectTracePeriod` INTEGER,
	`survIndirect` BIT,
	`survIndirectSuccess` DOUBLE,
	`survIndirectTracePeriod` INTEGER,
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
	`costVaccAdditionalPerAnimal` DOUBLE); 

insert into `inProductionType_IM0` ( `productionTypeID`, `descr`, `useDiseaseTransition`, `disLatentPeriodPdfID`, `disSubclinicalPeriodPdfID`, `disClinicalPeriodPdfID`, `disImmunePeriodPdfID`, `useDetection`, `detProbObsVsTimeClinicalRelID`, `detProbReportVsFirstDetectionRelID`, `survDirect`, `survDirectSuccess`, `survDirectTracePeriod`, `survIndirect`, `survIndirectSuccess`, `survIndirectTracePeriod`, `useBasicDestruction`, `destrIsRingTrigger`, `destrRingRadius`, `destrIsRingTarget`, `destrDirectTraces`, `destrIndirectTraces`, `destrPriority`, `useVaccination`, `vaccMinTimeBetweenVaccinations`, `vaccDaysToImmunity`, `vaccImmunePeriodPdfID`, `vaccRing`, `vaccRingRadius`, `vaccPriority`, `useCosts`, `costDestrAppraisalPerUnit`, `costDestrCleaningPerUnit`, `costDestrEuthanasiaPerAnimal`, `costDestrIndemnificationPerAnimal`, `costDestrDisposalPerAnimal`, `costVaccSetupPerUnit`, `costVaccThreshold`, `costVaccBaselinePerAnimal`, `costVaccAdditionalPerAnimal`) 
    select `inProductionType`.`productionTypeID`, 
           `inProductionType`.`descr`, 
           `inProductionType`.`useDiseaseTransition`, 
           `inProductionType`.`disLatentPeriodPdfID`, 
           `inProductionType`.`disSubclinicalPeriodPdfID`, 
           `inProductionType`.`disClinicalPeriodPdfID`, 
           `inProductionType`.`disImmunePeriodPdfID`, 
           `inProductionType`.`useDetection`, 
           `inProductionType`.`detProbObsVsTimeClinicalRelID`, 
           `inProductionType`.`detProbReportVsFirstDetectionRelID`, 
           `inProductionType`.`survDirect`, 
           `inProductionType`.`survDirectSuccess`, 
           `inProductionType`.`survDirectTracePeriod`, 
           `inProductionType`.`survIndirect`, 
           `inProductionType`.`survIndirectSuccess`, 
           `inProductionType`.`survIndirectTracePeriod`, 
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
           `inProductionType`.`costVaccAdditionalPerAnimal`  
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

drop table `inProductionType`; 

create table `inProductionType` ( 
	`productionTypeID` COUNTER,
	`descr` VARCHAR(255),
	`useDiseaseTransition` BIT,
	`disLatentPeriodPdfID` INTEGER,
	`disSubclinicalPeriodPdfID` INTEGER,
	`disClinicalPeriodPdfID` INTEGER,
	`disImmunePeriodPdfID` INTEGER,
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
	`costVaccAdditionalPerAnimal` DOUBLE); 

insert into `inProductionType` (`productionTypeID`, `descr`, `useDiseaseTransition`, `disLatentPeriodPdfID`, `disSubclinicalPeriodPdfID`, `disClinicalPeriodPdfID`, `disImmunePeriodPdfID`, `useDetection`, `detProbObsVsTimeClinicalRelID`, `detProbReportVsFirstDetectionRelID`, `traceDirect`, `traceDirectSuccess`, `traceDirectTracePeriod`, `traceIndirect`, `traceIndirectSuccess`, `traceIndirectTracePeriod`, `useBasicDestruction`, `destrIsRingTrigger`, `destrRingRadius`, `destrIsRingTarget`, `destrDirectTraces`, `destrIndirectTraces`, `destrPriority`, `useVaccination`, `vaccMinTimeBetweenVaccinations`, `vaccDaysToImmunity`, `vaccImmunePeriodPdfID`, `vaccRing`, `vaccRingRadius`, `vaccPriority`, `useCosts`, `costDestrAppraisalPerUnit`, `costDestrCleaningPerUnit`, `costDestrEuthanasiaPerAnimal`, `costDestrIndemnificationPerAnimal`, `costDestrDisposalPerAnimal`, `costVaccSetupPerUnit`, `costVaccThreshold`, `costVaccBaselinePerAnimal`, `costVaccAdditionalPerAnimal`) 
    select `inProductionType_IM0`.`productionTypeID`, 
           `inProductionType_IM0`.`descr`, 
           `inProductionType_IM0`.`useDiseaseTransition`, 
           `inProductionType_IM0`.`disLatentPeriodPdfID`, 
           `inProductionType_IM0`.`disSubclinicalPeriodPdfID`, 
           `inProductionType_IM0`.`disClinicalPeriodPdfID`, 
           `inProductionType_IM0`.`disImmunePeriodPdfID`, 
           `inProductionType_IM0`.`useDetection`, 
           `inProductionType_IM0`.`detProbObsVsTimeClinicalRelID`, 
           `inProductionType_IM0`.`detProbReportVsFirstDetectionRelID`, 
           `inProductionType_IM0`.`survDirect`, 
           `inProductionType_IM0`.`survDirectSuccess`, 
           `inProductionType_IM0`.`survDirectTracePeriod`, 
           `inProductionType_IM0`.`survIndirect`, 
           `inProductionType_IM0`.`survIndirectSuccess`, 
           `inProductionType_IM0`.`survIndirectTracePeriod`, 
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
           `inProductionType_IM0`.`useCosts`, 
           `inProductionType_IM0`.`costDestrAppraisalPerUnit`, 
           `inProductionType_IM0`.`costDestrCleaningPerUnit`, 
           `inProductionType_IM0`.`costDestrEuthanasiaPerAnimal`, 
           `inProductionType_IM0`.`costDestrIndemnificationPerAnimal`, 
           `inProductionType_IM0`.`costDestrDisposalPerAnimal`, 
           `inProductionType_IM0`.`costVaccSetupPerUnit`, 
           `inProductionType_IM0`.`costVaccThreshold`, 
           `inProductionType_IM0`.`costVaccBaselinePerAnimal`, 
           `inProductionType_IM0`.`costVaccAdditionalPerAnimal`  
    from `inProductionType_IM0`;

drop table `inProductionType_IM0`; 

alter table `inProductionType`
	add constraint `inProductionType_PK` primary key (`productionTypeID`) ; 

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

create table `inControlsGlobal_IM0` ( 
	`controlsGlobalID` CHAR(10),
	`includeDetection` BIT,
	`includeSurveillance` BIT,
	`includeDestruction` BIT,
	`destrProgramDelay` INTEGER,
	`destrCapacityRelID` INTEGER,
	`destrPriorityOrder` VARCHAR(255),
	`destrReasonOrder` VARCHAR(255),
	`includeVaccination` BIT,
	`vaccDetectedUnitsBeforeStart` INTEGER,
	`vaccCapacityRelID` INTEGER,
	`vaccPriorityOrder` VARCHAR(255)); 

insert into `inControlsGlobal_IM0` (`controlsGlobalID`, `includeDetection`, `includeSurveillance`, `includeDestruction`, `destrProgramDelay`, `destrCapacityRelID`, `destrPriorityOrder`, `destrReasonOrder`, `includeVaccination`, `vaccDetectedUnitsBeforeStart`, `vaccCapacityRelID`, `vaccPriorityOrder`) 
    select `inControlsGlobal`.`controlsGlobalID`, 
           `inControlsGlobal`.`includeDetection`, 
           `inControlsGlobal`.`includeSurveillance`, 
           `inControlsGlobal`.`includeDestruction`, 
           `inControlsGlobal`.`destrProgramDelay`, 
           `inControlsGlobal`.`destrCapacityRelID`, 
           `inControlsGlobal`.`destrPriorityOrder`, 
           `inControlsGlobal`.`destrReasonOrder`, 
           `inControlsGlobal`.`includeVaccination`, 
           `inControlsGlobal`.`vaccDetectedUnitsBeforeStart`, 
           `inControlsGlobal`.`vaccCapacityRelID`, 
           `inControlsGlobal`.`vaccPriorityOrder`  
    from `inControlsGlobal`;

alter table `inControlsGlobal`
	drop constraint `inChart_inControlsGlobal_FK1`;

alter table `inControlsGlobal`
	drop constraint `inChart_inControlsGlobal_FK2`;

drop table `inControlsGlobal`; 

create table `inControlsGlobal` ( 
	`controlsGlobalID` CHAR(10),
	`includeDetection` BIT,
	`includeTracing` BIT,
	`includeDestruction` BIT,
	`destrProgramDelay` INTEGER,
	`destrCapacityRelID` INTEGER,
	`destrPriorityOrder` VARCHAR(255),
	`destrReasonOrder` VARCHAR(255),
	`includeVaccination` BIT,
	`vaccDetectedUnitsBeforeStart` INTEGER,
	`vaccCapacityRelID` INTEGER,
	`vaccPriorityOrder` VARCHAR(255)); 

insert into `inControlsGlobal` (`controlsGlobalID`, `includeDetection`, `includeTracing`, `includeDestruction`, `destrProgramDelay`, `destrCapacityRelID`, `destrPriorityOrder`, `destrReasonOrder`, `includeVaccination`, `vaccDetectedUnitsBeforeStart`, `vaccCapacityRelID`, `vaccPriorityOrder`) 
    select `inControlsGlobal_IM0`.`controlsGlobalID`, 
           `inControlsGlobal_IM0`.`includeDetection`, 
           `inControlsGlobal_IM0`.`includeSurveillance`, 
           `inControlsGlobal_IM0`.`includeDestruction`, 
           `inControlsGlobal_IM0`.`destrProgramDelay`, 
           `inControlsGlobal_IM0`.`destrCapacityRelID`, 
           `inControlsGlobal_IM0`.`destrPriorityOrder`, 
           `inControlsGlobal_IM0`.`destrReasonOrder`, 
           `inControlsGlobal_IM0`.`includeVaccination`, 
           `inControlsGlobal_IM0`.`vaccDetectedUnitsBeforeStart`, 
           `inControlsGlobal_IM0`.`vaccCapacityRelID`, 
           `inControlsGlobal_IM0`.`vaccPriorityOrder`  
    from `inControlsGlobal_IM0`;


drop table `inControlsGlobal_IM0`; 


alter table `inControlsGlobal`
	add constraint `inControlsGlobal_PK` primary key (`controlsGlobalID`) ; 

alter table `inControlsGlobal`
	add constraint `inChart_inControlsGlobal_FK1` foreign key (
		`destrCapacityRelID`)
	 references `inChart` (
		`chartID`); 

alter table `inControlsGlobal`
	add constraint `inChart_inControlsGlobal_FK2` foreign key (
		`vaccCapacityRelID`)
	 references `inChart` (
		`chartID`); 

