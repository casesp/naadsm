---------------------------
-- Go from 3.2.11 to 3.2.13
----------------------------
-- alter table outIterationByProductionType add column firstDetUInf long;
-- alter table outIterationByProductionType add column firstDetAInf long;


---------------------------------------------------------------
-- Modifications to the major output tables for 4.0.0 and 4.0.1
---------------------------------------------------------------
alter table outIterationByProductionType drop column infcUAir;
alter table outIterationByProductionType drop column infcAAir;
alter table outIterationByProductionType drop column infcUDir;
alter table outIterationByProductionType drop column infcADir;
alter table outIterationByProductionType drop column infcUInd;
alter table outIterationByProductionType drop column infcAInd;
alter table outIterationByProductionType add column tscUDead long;
alter table outIterationByProductionType add column tscADead long;
alter table outIterationByProductionType add column infcUAll long;
alter table outIterationByProductionType add column infcAAll long;
alter table outIterationByProductionType add column deadcUIni long;
alter table outIterationByProductionType add column deadcAIni long;
alter table outIterationByProductionType add column deadcU long;
alter table outIterationByProductionType add column deadcA long;
alter table outIterationByProductionType add column detcUDead long;
alter table outIterationByProductionType add column detcADead long;
alter table outIterationByProductionType add column detcUAll long;
alter table outIterationByProductionType add column detcAAll long;
alter table outIterationByProductionType add column deadcUInDestrQueue long;
alter table outIterationByProductionType add column deadcAInDestrQueue long;
alter table outIterationByProductionType add column adqcUAir long;
alter table outIterationByProductionType add column adqcAAir long;
alter table outIterationByProductionType add column adqcUDir long;
alter table outIterationByProductionType add column adqcADir long;
alter table outIterationByProductionType add column adqcUInd long;
alter table outIterationByProductionType add column adqcAInd long;
alter table outIterationByProductionType add column adqcULcl long;
alter table outIterationByProductionType add column adqcALcl long;

    
alter table outDailyByProductionType drop column infnUAir long;
alter table outDailyByProductionType drop column infnAAir long;
alter table outDailyByProductionType drop column infnUDir long;
alter table outDailyByProductionType drop column infnADir long;
alter table outDailyByProductionType drop column infnUInd long;
alter table outDailyByProductionType drop column infnAInd long;   
alter table outDailyByProductionType drop column infcUAir long;
alter table outDailyByProductionType drop column infcAAir long;
alter table outDailyByProductionType drop column infcUDir long;
alter table outDailyByProductionType drop column infcADir long;
alter table outDailyByProductionType drop column infcUInd long;
alter table outDailyByProductionType drop column infcAInd long;
alter table outDailyByProductionType add column tsdUDead long;
alter table outDailyByProductionType add column tsdADead long;
alter table outDailyByProductionType add column tscUDead long;
alter table outDailyByProductionType add column tscADead long;
alter table outDailyByProductionType add column infnUAll long;
alter table outDailyByProductionType add column infnAAll long;
alter table outDailyByProductionType add column infcUAll long;
alter table outDailyByProductionType add column infcAAll long;
alter table outDailyByProductionType add column deadcUIni long;
alter table outDailyByProductionType add column deadcAIni long;
alter table outDailyByProductionType add column deadcU long;
alter table outDailyByProductionType add column deadcA long;
alter table outDailyByProductionType add column deadnU long;
alter table outDailyByProductionType add column deadnA long;
alter table outDailyByProductionType add column detnUDead long;
alter table outDailyByProductionType add column detnADead long;
alter table outDailyByProductionType add column detnUAll long;
alter table outDailyByProductionType add column detnAAll long;
alter table outDailyByProductionType add column detcUDead long;
alter table outDailyByProductionType add column detcADead long;
alter table outDailyByProductionType add column detcUAll long;
alter table outDailyByProductionType add column detcAAll long;
alter table outDailyByProductionType add column deadnUInDestrQueue long;
alter table outDailyByProductionType add column deadnAInDestrQueue long;
alter table outDailyByProductionType add column deadcUInDestrQueue long;
alter table outDailyByProductionType add column deadcAInDestrQueue long;
alter table outDailyByProductionType add column adqnUAir long;
alter table outDailyByProductionType add column adqnAAir long;
alter table outDailyByProductionType add column adqnUDir long;
alter table outDailyByProductionType add column adqnADir long;
alter table outDailyByProductionType add column adqnUInd long;
alter table outDailyByProductionType add column adqnAInd long;    
alter table outDailyByProductionType add column adqcUAir long;
alter table outDailyByProductionType add column adqcAAir long;
alter table outDailyByProductionType add column adqcUDir long;
alter table outDailyByProductionType add column adqcADir long;
alter table outDailyByProductionType add column adqcUInd long;
alter table outDailyByProductionType add column adqcAInd long;
alter table outDailyByProductionType add column adqnULcl long;
alter table outDailyByProductionType add column adqnALcl long;
alter table outDailyByProductionType add column adqcULcl long;
alter table outDailyByProductionType add column adqcALcl long;

alter table inSelectDailyOutputs drop column sinfnUAir;
alter table inSelectDailyOutputs drop column sinfnAAir;
alter table inSelectDailyOutputs drop column sinfnUDir;
alter table inSelectDailyOutputs drop column sinfnADir;
alter table inSelectDailyOutputs drop column sinfnUInd;
alter table inSelectDailyOutputs drop column sinfnAInd;   
alter table inSelectDailyOutputs drop column sinfcUAir;
alter table inSelectDailyOutputs drop column sinfcAAir;
alter table inSelectDailyOutputs drop column sinfcUDir;
alter table inSelectDailyOutputs drop column sinfcADir;
alter table inSelectDailyOutputs drop column sinfcUInd;
alter table inSelectDailyOutputs drop column sinfcAInd;
alter table inSelectDailyOutputs add column stsdUDead bit;
alter table inSelectDailyOutputs add column stsdADead bit;
alter table inSelectDailyOutputs add column stscUDead bit;
alter table inSelectDailyOutputs add column stscADead bit;
alter table inSelectDailyOutputs add column sinfnUAll bit;
alter table inSelectDailyOutputs add column sinfnAAll bit;
alter table inSelectDailyOutputs add column sinfcUAll bit;
alter table inSelectDailyOutputs add column sinfcAAll bit;
alter table inSelectDailyOutputs add column sdeadcUIni bit;
alter table inSelectDailyOutputs add column sdeadcAIni bit;
alter table inSelectDailyOutputs add column sdeadcU bit;
alter table inSelectDailyOutputs add column sdeadcA bit;
alter table inSelectDailyOutputs add column sdeadnU bit;
alter table inSelectDailyOutputs add column sdeadnA bit;
alter table inSelectDailyOutputs add column sdetnUDead bit;
alter table inSelectDailyOutputs add column sdetnADead bit;
alter table inSelectDailyOutputs add column sdetnUAll bit;
alter table inSelectDailyOutputs add column sdetnAAll bit;
alter table inSelectDailyOutputs add column sdetcUDead bit;
alter table inSelectDailyOutputs add column sdetcADead bit;
alter table inSelectDailyOutputs add column sdetcUAll bit;
alter table inSelectDailyOutputs add column sdetcAAll bit;
alter table inSelectDailyOutputs add column sdeadnUInDestrQueue bit;
alter table inSelectDailyOutputs add column sdeadnAInDestrQueue bit;
alter table inSelectDailyOutputs add column sdeadcUInDestrQueue bit;
alter table inSelectDailyOutputs add column sdeadcAInDestrQueue bit;
alter table inSelectDailyOutputs add column sadqnUAir bit;
alter table inSelectDailyOutputs add column sadqnAAir bit;
alter table inSelectDailyOutputs add column sadqnUDir bit;
alter table inSelectDailyOutputs add column sadqnADir bit;
alter table inSelectDailyOutputs add column sadqnUInd bit;
alter table inSelectDailyOutputs add column sadqnAInd bit;    
alter table inSelectDailyOutputs add column sadqcUAir bit;
alter table inSelectDailyOutputs add column sadqcAAir bit;
alter table inSelectDailyOutputs add column sadqcUDir bit;
alter table inSelectDailyOutputs add column sadqcADir bit;
alter table inSelectDailyOutputs add column sadqcUInd bit;
alter table inSelectDailyOutputs add column sadqcAInd bit;
alter table inSelectDailyOutputs add column sadqnULcl bit;
alter table inSelectDailyOutputs add column sadqnALcl bit;
alter table inSelectDailyOutputs add column sadqcULcl bit;
alter table inSelectDailyOutputs add column sadqcALcl bit;


---------------------------------------------------------------------------
-- Deal with all of the input tables that were modified from 3.2.x to 4.0.0
---------------------------------------------------------------------------

alter table `inDiseaseSpread`
	drop constraint `inChart_inDiseaseSpread_FK3`;

create table `readCostApplicationCodes` ( 
	`costApplicationCode` CHAR(1),
	`definition` VARCHAR(255), constraint `readCostApplicationCodes_PK` primary key (`costApplicationCode`) ); 

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
	`traceDirectForward` BIT,
	`traceDirectBack` BIT,
	`traceDirectSuccess` DOUBLE,
	`traceDirectTracePeriod` INTEGER,
	`traceIndirectForward` BIT,
	`traceIndirectBack` BIT,
	`traceIndirectSuccess` DOUBLE,
	`traceIndirectTracePeriod` INTEGER,
	`traceDelayPdfID` INTEGER,
	`useBasicDestruction` BIT,
	`destrIsRingTrigger` BIT,
	`destrRingRadius` DOUBLE,
	`destrIsRingTarget` BIT,
	`destrDirectForwardTraces` BIT,
	`destrIndirectForwardTraces` BIT,
	`destrDirectBackTraces` BIT,
	`destrIndirectBackTraces` BIT,
	`destrPriority` INTEGER,
	`useVaccination` BIT,
	`vaccMinTimeBetweenVaccinations` INTEGER,
	`vaccVaccinateDetected` BIT,
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
	`zoneIndirectTraceIsTrigger` BIT,
	`examDirectForward` BIT,
	`examDirectForwardMultiplier` DOUBLE,
	`examIndirectForward` BIT,
	`examIndirectForwardMultiplier` DOUBLE,
	`examDirectBack` BIT,
	`examDirectBackMultiplier` DOUBLE,
	`examIndirectBack` BIT,
	`examIndirectBackMultiplier` DOUBLE,
	`testDirectForward` BIT,
	`testIndirectForward` BIT,
	`testDirectBack` BIT,
	`testIndirectBack` BIT,
	`testSpecificity` DOUBLE,
	`testSensitivity` DOUBLE,
	`testDelayPdfID` INTEGER); 

insert into `inProductionType_IM0` ( `productionTypeID`, `descr`, `useDiseaseTransition`, `disLatentPeriodPdfID`, `disSubclinicalPeriodPdfID`, `disClinicalPeriodPdfID`, `disImmunePeriodPdfID`, `disPrevalenceRelID`, `useDetection`, `detProbObsVsTimeClinicalRelID`, `detProbReportVsFirstDetectionRelID`, `traceDirectForward`, `traceDirectBack`, `traceDirectSuccess`, `traceDirectTracePeriod`, `traceIndirectForward`, `traceIndirectBack`, `traceIndirectSuccess`, `traceIndirectTracePeriod`, `traceDelayPdfID`, `useBasicDestruction`, `destrIsRingTrigger`, `destrRingRadius`, `destrIsRingTarget`, `destrDirectForwardTraces`, `destrIndirectForwardTraces`, `destrDirectBackTraces`, `destrIndirectBackTraces`, `destrPriority`, `useVaccination`, `vaccMinTimeBetweenVaccinations`, `vaccVaccinateDetected`, `vaccDaysToImmunity`, `vaccImmunePeriodPdfID`, `vaccRing`, `vaccRingRadius`, `vaccPriority`, `costDestrAppraisalPerUnit`, `costDestrCleaningPerUnit`, `costDestrEuthanasiaPerAnimal`, `costDestrIndemnificationPerAnimal`, `costDestrDisposalPerAnimal`, `costVaccSetupPerUnit`, `costVaccThreshold`, `costVaccBaselinePerAnimal`, `costVaccAdditionalPerAnimal`, `zoneDetectionIsTrigger`, `zoneDirectTraceIsTrigger`, `zoneIndirectTraceIsTrigger`, `examDirectForward`, `examDirectForwardMultiplier`, `examIndirectForward`, `examIndirectForwardMultiplier`, `examDirectBack`, `examDirectBackMultiplier`, `examIndirectBack`, `examIndirectBackMultiplier`, `testDirectForward`, `testIndirectForward`, `testDirectBack`, `testIndirectBack`, `testSpecificity`, `testSensitivity`, `testDelayPdfID`) 
    select  `inProductionType`.`productionTypeID`,
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
           `inProductionType`.`traceDirectForward`, 
           `inProductionType`.`traceDirectBack`, 
           `inProductionType`.`traceDirectSuccess`, 
           `inProductionType`.`traceDirectTracePeriod`, 
           `inProductionType`.`traceIndirectForward`, 
           `inProductionType`.`traceIndirectBack`, 
           `inProductionType`.`traceIndirectSuccess`, 
           `inProductionType`.`traceIndirectTracePeriod`, 
           `inProductionType`.`traceDelayPdfID`, 
           `inProductionType`.`useBasicDestruction`, 
           `inProductionType`.`destrIsRingTrigger`, 
           `inProductionType`.`destrRingRadius`, 
           `inProductionType`.`destrIsRingTarget`, 
           `inProductionType`.`destrDirectForwardTraces`, 
           `inProductionType`.`destrIndirectForwardTraces`, 
           `inProductionType`.`destrDirectBackTraces`, 
           `inProductionType`.`destrIndirectBackTraces`, 
           `inProductionType`.`destrPriority`, 
           `inProductionType`.`useVaccination`, 
           `inProductionType`.`vaccMinTimeBetweenVaccinations`, 
           `inProductionType`.`vaccVaccinateDetected`, 
           `inProductionType`.`vaccDaysToImmunity`, 
           `inProductionType`.`vaccImmunePeriodPdfID`, 
           `inProductionType`.`vaccRing`, 
           `inProductionType`.`vaccRingRadius`, 
           `inProductionType`.`vaccPriority`, 
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
           `inProductionType`.`zoneIndirectTraceIsTrigger`, 
           `inProductionType`.`examDirectForward`, 
           `inProductionType`.`examDirectForwardMultiplier`, 
           `inProductionType`.`examIndirectForward`, 
           `inProductionType`.`examIndirectForwardMultiplier`, 
           `inProductionType`.`examDirectBack`, 
           `inProductionType`.`examDirectBackMultiplier`, 
           `inProductionType`.`examIndirectBack`, 
           `inProductionType`.`examIndirectBackMultiplier`, 
           `inProductionType`.`testDirectForward`, 
           `inProductionType`.`testIndirectForward`, 
           `inProductionType`.`testDirectBack`, 
           `inProductionType`.`testIndirectBack`, 
           `inProductionType`.`testSpecificity`, 
           `inProductionType`.`testSensitivity`, 
           `inProductionType`.`testDelayPdfID`  
    from `inProductionType`;

alter table `dynHerd`
	drop constraint `inProductionType_dynHerd_FK1`;

alter table `inProductionTypePair`
	drop constraint `inProductionType_inProductionTypePairs_FK1`;

alter table `inProductionTypePair`
	drop constraint `inProductionType_inProductionTypePairs_FK2`;

alter table `outIterationByProductionType`
	drop constraint `inProductionType_outIterationByProductionType_FK1`;


alter table `outEpidemicCurves`
	drop constraint `inProductionType_outEpidemicCurves_FK1`;

alter table `outIterationCosts`
	drop constraint `inProductionType_outIterationCosts_FK1`;

alter table `inZoneProductionTypePair`
	drop constraint `inProductionType_inZoneProductionTypePair_FK1`;

alter table `outIterationByZoneAndProductionType`
	drop constraint `inProductionType_outIterationByZoneAndProductionType_FK1`;

alter table `outDailyByZoneAndProductionType`
	drop constraint `inProductionType_outDailyByZoneAndProductionType_FK1`;

alter table `outDailyByProductionType`
	drop constraint `inProductionType_outDailyByProductionType_FK1`;

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

alter table `inProductionType`
	drop constraint `inChart_inProductionType_FK9`;

alter table `inProductionType`
	drop constraint `inChart_inProductionType_FK10`;

drop table `inProductionType`; 

create table `inProductionType` ( 
	`productionTypeID` COUNTER,
	`descr` VARCHAR(255),
	`useDiseaseTransition` BIT,
	`disLatentPeriodPdfID` INTEGER,
	`disSubclinicalPeriodPdfID` INTEGER,
	`disClinicalPeriodPdfID` INTEGER,
	`disImmunePeriodPdfID` INTEGER,
	`disPrevalenceInfectedRelID` INTEGER,
	`disPrevalenceSheddingRelID` INTEGER,
	`disProbMortality` DOUBLE,
	`useDetection` BIT,
	`detProbObsVsTimeClinicalRelID` INTEGER,
	`detProbReportClinVsFirstDetectionRelID` INTEGER,
	`detProbObsVsTimeDeadRelID` INTEGER,
  `detProbReportDeadVsFirstDetectionRelID` INTEGER,
	`traceDirectForward` BIT,
	`traceDirectBack` BIT,
	`traceDirectSuccess` DOUBLE,
	`traceDirectTracePeriod` INTEGER,
	`traceIndirectForward` BIT,
	`traceIndirectBack` BIT,
	`traceIndirectSuccess` DOUBLE,
	`traceIndirectTracePeriod` INTEGER,
	`traceDelayPdfID` INTEGER,
	`useBasicDestruction` BIT,
	`destrIsRingTrigger` BIT,
	`destrRingRadius` DOUBLE,
	`destrIsRingTarget` BIT,
	`destrDirectForwardTraces` BIT,
	`destrIndirectForwardTraces` BIT,
	`destrDirectBackTraces` BIT,
	`destrIndirectBackTraces` BIT,
	`destrPriority` INTEGER,
	`useVaccination` BIT,
	`vaccMinTimeBetweenVaccinations` INTEGER,
	`vaccVaccinateDetected` BIT,
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
	`costDestrAppraiseDeadUnitsCode` CHAR(1),
	`costDestrCleanDeadUnitsCode` CHAR(1),
	`costDestrIndemnifyDeadUnitsCode` CHAR(1),
	`costDestrDisposeDeadUnitsCode` CHAR(1),
	`costVaccSetupPerUnit` DOUBLE,
	`costVaccThreshold` INTEGER,
	`costVaccBaselinePerAnimal` DOUBLE,
	`costVaccAdditionalPerAnimal` DOUBLE,
	`zoneDetectionIsTrigger` BIT,
	`zoneDirectTraceIsTrigger` BIT,
	`zoneIndirectTraceIsTrigger` BIT,
	`examDirectForward` BIT,
	`examDirectForwardMultiplier` DOUBLE,
	`examIndirectForward` BIT,
	`examIndirectForwardMultiplier` DOUBLE,
	`examDirectBack` BIT,
	`examDirectBackMultiplier` DOUBLE,
	`examIndirectBack` BIT,
	`examIndirectBackMultiplier` DOUBLE,
	`testDirectForward` BIT,
	`testIndirectForward` BIT,
	`testDirectBack` BIT,
	`testIndirectBack` BIT,
	`testSpecificity` DOUBLE,
	`testSensitivity` DOUBLE,
	`testDelayPdfID` INTEGER); 

insert into `inProductionType` (`productionTypeID`, `descr`, `useDiseaseTransition`, `disLatentPeriodPdfID`, `disSubclinicalPeriodPdfID`, `disClinicalPeriodPdfID`, `disImmunePeriodPdfID`, `disPrevalenceInfectedRelID`, `useDetection`, `detProbObsVsTimeClinicalRelID`, `detProbReportClinVsFirstDetectionRelID`, `traceDirectForward`, `traceDirectBack`, `traceDirectSuccess`, `traceDirectTracePeriod`, `traceIndirectForward`, `traceIndirectBack`, `traceIndirectSuccess`, `traceIndirectTracePeriod`, `traceDelayPdfID`, `useBasicDestruction`, `destrIsRingTrigger`, `destrRingRadius`, `destrIsRingTarget`, `destrDirectForwardTraces`, `destrIndirectForwardTraces`, `destrDirectBackTraces`, `destrIndirectBackTraces`, `destrPriority`, `useVaccination`, `vaccMinTimeBetweenVaccinations`, `vaccVaccinateDetected`, `vaccDaysToImmunity`, `vaccImmunePeriodPdfID`, `vaccRing`, `vaccRingRadius`, `vaccPriority`, `costDestrAppraisalPerUnit`, `costDestrCleaningPerUnit`, `costDestrEuthanasiaPerAnimal`, `costDestrIndemnificationPerAnimal`, `costDestrDisposalPerAnimal`, `costVaccSetupPerUnit`, `costVaccThreshold`, `costVaccBaselinePerAnimal`, `costVaccAdditionalPerAnimal`, `zoneDetectionIsTrigger`, `zoneDirectTraceIsTrigger`, `zoneIndirectTraceIsTrigger`, `examDirectForward`, `examDirectForwardMultiplier`, `examIndirectForward`, `examIndirectForwardMultiplier`, `examDirectBack`, `examDirectBackMultiplier`, `examIndirectBack`, `examIndirectBackMultiplier`, `testDirectForward`, `testIndirectForward`, `testDirectBack`, `testIndirectBack`, `testSpecificity`, `testSensitivity`, `testDelayPdfID`) 
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
           `inProductionType_IM0`.`traceDirectForward`, 
           `inProductionType_IM0`.`traceDirectBack`, 
           `inProductionType_IM0`.`traceDirectSuccess`, 
           `inProductionType_IM0`.`traceDirectTracePeriod`, 
           `inProductionType_IM0`.`traceIndirectForward`, 
           `inProductionType_IM0`.`traceIndirectBack`, 
           `inProductionType_IM0`.`traceIndirectSuccess`, 
           `inProductionType_IM0`.`traceIndirectTracePeriod`, 
           `inProductionType_IM0`.`traceDelayPdfID`, 
           `inProductionType_IM0`.`useBasicDestruction`, 
           `inProductionType_IM0`.`destrIsRingTrigger`, 
           `inProductionType_IM0`.`destrRingRadius`, 
           `inProductionType_IM0`.`destrIsRingTarget`, 
           `inProductionType_IM0`.`destrDirectForwardTraces`, 
           `inProductionType_IM0`.`destrIndirectForwardTraces`, 
           `inProductionType_IM0`.`destrDirectBackTraces`, 
           `inProductionType_IM0`.`destrIndirectBackTraces`, 
           `inProductionType_IM0`.`destrPriority`, 
           `inProductionType_IM0`.`useVaccination`, 
           `inProductionType_IM0`.`vaccMinTimeBetweenVaccinations`, 
           `inProductionType_IM0`.`vaccVaccinateDetected`, 
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
           `inProductionType_IM0`.`zoneIndirectTraceIsTrigger`, 
           `inProductionType_IM0`.`examDirectForward`, 
           `inProductionType_IM0`.`examDirectForwardMultiplier`, 
           `inProductionType_IM0`.`examIndirectForward`, 
           `inProductionType_IM0`.`examIndirectForwardMultiplier`, 
           `inProductionType_IM0`.`examDirectBack`, 
           `inProductionType_IM0`.`examDirectBackMultiplier`, 
           `inProductionType_IM0`.`examIndirectBack`, 
           `inProductionType_IM0`.`examIndirectBackMultiplier`, 
           `inProductionType_IM0`.`testDirectForward`, 
           `inProductionType_IM0`.`testIndirectForward`, 
           `inProductionType_IM0`.`testDirectBack`, 
           `inProductionType_IM0`.`testIndirectBack`, 
           `inProductionType_IM0`.`testSpecificity`, 
           `inProductionType_IM0`.`testSensitivity`, 
           `inProductionType_IM0`.`testDelayPdfID`  
    from `inProductionType_IM0`;

drop table `inProductionType_IM0`; 

alter table `inProductionType`
	add constraint `inProductionType_PK` primary key (`productionTypeID`) ; 

create table `inGeneral_IM0` ( 
	`inGeneralID` VARCHAR(10),
	`language` CHAR(2),
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
	`saveIterationOutputsForHerds` BIT,
	`useCustomOutputs` BIT,
	`writeNAADSMapOutput` BIT,
	`NAADSMapDirectory` VARCHAR(255)); 

insert into `inGeneral_IM0` (`inGeneralID`, `language`, `scenarioDescr`, `iterations`, `days`, `simStopReason`, `includeContactSpread`, `includeAirborneSpread`, `useAirborneExponentialDecay`, `useWithinHerdPrevalence`, `costTrackDestruction`, `costTrackVaccination`, `costTrackZoneSurveillance`, `useFixedRandomSeed`, `randomSeed`, `saveAllDailyOutputs`, `saveDailyOutputsForIterations`, `writeDailyStatesFile`, `dailyStatesFileName`, `saveDailyEvents`, `saveDailyExposures`, `saveIterationOutputsForHerds`, `useCustomOutputs`, `writeNAADSMapOutput`, `NAADSMapDirectory`) 
    select `inGeneral`.`inGeneralID`, 
           `inGeneral`.`language`, 
           `inGeneral`.`scenarioDescr`, 
           `inGeneral`.`iterations`, 
           `inGeneral`.`days`, 
           `inGeneral`.`simStopReason`, 
           `inGeneral`.`includeContactSpread`, 
           `inGeneral`.`includeAirborneSpread`, 
           `inGeneral`.`useAirborneExponentialDecay`, 
           `inGeneral`.`useWithinHerdPrevalence`, 
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
           `inGeneral`.`saveIterationOutputsForHerds`, 
           `inGeneral`.`useCustomOutputs`, 
           `inGeneral`.`writeNAADSMapOutput`, 
           `inGeneral`.`NAADSMapDirectory`  
    from `inGeneral`;

drop table `inGeneral`; 

create table `inGeneral` ( 
	`inGeneralID` VARCHAR(10),
	`language` CHAR(2),
	`scenarioDescr` LONGTEXT,
	`iterations` INTEGER,
	`days` INTEGER,
	`simStopReason` VARCHAR(25),
	`includeContactSpread` BIT,
	`includeAirborneSpread` BIT,
	`windDirectionStart` INTEGER,
	`windDirectionEnd` INTEGER,
	`includeLocalAreaSpread` BIT,
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
	`saveIterationOutputsForHerds` BIT,
	`useCustomOutputs` BIT,
	`writeNAADSMapOutput` BIT,
	`NAADSMapDirectory` VARCHAR(255)); 

insert into `inGeneral` (`inGeneralID`, `language`, `scenarioDescr`, `iterations`, `days`, `simStopReason`, `includeContactSpread`, `includeAirborneSpread`, `useWithinHerdPrevalence`, `costTrackDestruction`, `costTrackVaccination`, `costTrackZoneSurveillance`, `useFixedRandomSeed`, `randomSeed`, `saveAllDailyOutputs`, `saveDailyOutputsForIterations`, `writeDailyStatesFile`, `dailyStatesFileName`, `saveDailyEvents`, `saveDailyExposures`, `saveIterationOutputsForHerds`, `useCustomOutputs`, `writeNAADSMapOutput`, `NAADSMapDirectory`) 
    select `inGeneral_IM0`.`inGeneralID`, 
           `inGeneral_IM0`.`language`, 
           `inGeneral_IM0`.`scenarioDescr`, 
           `inGeneral_IM0`.`iterations`, 
           `inGeneral_IM0`.`days`, 
           `inGeneral_IM0`.`simStopReason`, 
           `inGeneral_IM0`.`includeContactSpread`, 
           `inGeneral_IM0`.`includeAirborneSpread`, 
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
           `inGeneral_IM0`.`saveIterationOutputsForHerds`, 
           `inGeneral_IM0`.`useCustomOutputs`, 
           `inGeneral_IM0`.`writeNAADSMapOutput`, 
           `inGeneral_IM0`.`NAADSMapDirectory`  
    from `inGeneral_IM0`;

drop table `inGeneral_IM0`; 

alter table `inGeneral`
	add constraint `inGeneral_PK` primary key (`inGeneralID`) ; 

create index `inGeneralID` on `inGeneral` (
	`inGeneralID` ASC); 

create table `inDiseaseSpread_IM0` ( 
	`spreadID` COUNTER,
	`spreadMethodCode` VARCHAR(3),
	`latentCanInfect` BIT,
	`subclinicalCanInfect` BIT,
	`meanContactRate` DOUBLE,
	`useFixedContactRate` BIT,
	`fixedContactRate` DOUBLE,
	`infectionProbability` DOUBLE,
	`distancePdfID` INTEGER,
	`movementControlRelID` INTEGER,
	`transportDelayPdfID` INTEGER,
	`probAirborneSpread1km` DOUBLE,
	`maxDistAirborneSpread` DOUBLE,
	`windDirectionStart` INTEGER,
	`windDirectionEnd` INTEGER); 

insert into `inDiseaseSpread_IM0` ( `spreadID`, `spreadMethodCode`, `latentCanInfect`, `subclinicalCanInfect`, `meanContactRate`, `useFixedContactRate`, `fixedContactRate`, `infectionProbability`, `distancePdfID`, `movementControlRelID`, `transportDelayPdfID`, `probAirborneSpread1km`, `maxDistAirborneSpread`, `windDirectionStart`, `windDirectionEnd`) 
    select `inDiseaseSpread`.`spreadID`,
           `inDiseaseSpread`.`spreadMethodCode`, 
           `inDiseaseSpread`.`latentCanInfect`, 
           `inDiseaseSpread`.`subclinicalCanInfect`, 
           `inDiseaseSpread`.`meanContactRate`, 
           `inDiseaseSpread`.`useFixedContactRate`, 
           `inDiseaseSpread`.`fixedContactRate`, 
           `inDiseaseSpread`.`infectionProbability`, 
           `inDiseaseSpread`.`distancePdfID`, 
           `inDiseaseSpread`.`movementControlRelID`, 
           `inDiseaseSpread`.`transportDelayPdfID`, 
           `inDiseaseSpread`.`probAirborneSpread1km`, 
           `inDiseaseSpread`.`maxDistAirborneSpread`, 
           `inDiseaseSpread`.`windDirectionStart`, 
           `inDiseaseSpread`.`windDirectionEnd`  
    from `inDiseaseSpread`;


alter table `inProductionTypePair`
	drop constraint `inDiseaseSpread_inProductionTypePairs_FK1`;


alter table `inProductionTypePair`
	drop constraint `inDiseaseSpread_inProductionTypePairs_FK2`;


alter table `inProductionTypePair`
	drop constraint `inDiseaseSpread_inProductionTypePairs_FK3`;


alter table `inDiseaseSpread`
	drop constraint `inChart_inProductionTypePair_FK1`;


alter table `inDiseaseSpread`
	drop constraint `inChart_inProductionTypePair_FK3`;


alter table `inDiseaseSpread`
	drop constraint `readSpreadMethodCodes_inDiseaseSpread_FK1`;

alter table `outDailyExposures`
	drop constraint `readSpreadMethodCodes_outDailyExposures_FK1`;  

drop table `inDiseaseSpread`; 

create table `inDiseaseSpread` ( 
	`spreadID` COUNTER,
	`spreadMethodCode` VARCHAR(3),
	`meanContactRate` DOUBLE,
	`useFixedContactRate` BIT,
	`fixedContactRate` DOUBLE,
	`infectionProbability` DOUBLE,
	`distancePdfID` INTEGER,
	`movementControlRelID` INTEGER,
	`lclAirDistanceBetweenUnits` DOUBLE,
	`lclAirSusceptibleInRecipientUnit` INTEGER,
	`lclAirInfectiousInSourceUnit` INTEGER,
	`lclAirProbSpread` DOUBLE,
	`dirPropnUnitsInShipmentPdfID` INTEGER); 

insert into `inDiseaseSpread` ( `spreadID`, `spreadMethodCode`, `meanContactRate`, `useFixedContactRate`, `fixedContactRate`, `infectionProbability`, `distancePdfID`, `movementControlRelID`) 
    select `inDiseaseSpread_IM0`.`spreadID`,
           `inDiseaseSpread_IM0`.`spreadMethodCode`, 
           `inDiseaseSpread_IM0`.`meanContactRate`, 
           `inDiseaseSpread_IM0`.`useFixedContactRate`, 
           `inDiseaseSpread_IM0`.`fixedContactRate`, 
           `inDiseaseSpread_IM0`.`infectionProbability`, 
           `inDiseaseSpread_IM0`.`distancePdfID`, 
           `inDiseaseSpread_IM0`.`movementControlRelID`  
    from `inDiseaseSpread_IM0`;

drop table `inDiseaseSpread_IM0`; 


---AR
-- Update existing table `inProductionTypePair`.
create table `inProductionTypePair_IM0` ( 
	`sourceProductionTypeID` INTEGER,
	`destProductionTypeID` INTEGER,
	`useDirectContact` BIT,
	`directContactSpreadID` INTEGER,
	`useIndirectContact` BIT,
	`indirectContactSpreadID` INTEGER,
	`useAirborneSpread` BIT,
	`airborneContactSpreadID` INTEGER); 

insert into `inProductionTypePair_IM0` (`sourceProductionTypeID`, `destProductionTypeID`, `useDirectContact`, `directContactSpreadID`, `useIndirectContact`, `indirectContactSpreadID`, `useAirborneSpread`, `airborneContactSpreadID`) 
    select `inProductionTypePair`.`sourceProductionTypeID`, 
           `inProductionTypePair`.`destProductionTypeID`, 
           `inProductionTypePair`.`useDirectContact`, 
           `inProductionTypePair`.`directContactSpreadID`, 
           `inProductionTypePair`.`useIndirectContact`, 
           `inProductionTypePair`.`indirectContactSpreadID`, 
           `inProductionTypePair`.`useAirborneSpread`, 
           `inProductionTypePair`.`airborneContactSpreadID`  
    from `inProductionTypePair`;

drop table `inProductionTypePair`; 

create table `inProductionTypePair` ( 
	`sourceProductionTypeID` INTEGER,
	`destProductionTypeID` INTEGER,
	`useDirectContact` BIT,
	`directContactSpreadID` INTEGER,
	`useIndirectContact` BIT,
	`indirectContactSpreadID` INTEGER,
	`useAirborneSpread` BIT,
	`airborneContactSpreadID` INTEGER,
	`useLocalAreaSpread` BIT,
	`localAreaSpreadID` INTEGER); 

insert into `inProductionTypePair` (`sourceProductionTypeID`, `destProductionTypeID`, `useDirectContact`, `directContactSpreadID`, `useIndirectContact`, `indirectContactSpreadID`, `useAirborneSpread`, `airborneContactSpreadID`) 
    select `inProductionTypePair_IM0`.`sourceProductionTypeID`, 
           `inProductionTypePair_IM0`.`destProductionTypeID`, 
           `inProductionTypePair_IM0`.`useDirectContact`, 
           `inProductionTypePair_IM0`.`directContactSpreadID`, 
           `inProductionTypePair_IM0`.`useIndirectContact`, 
           `inProductionTypePair_IM0`.`indirectContactSpreadID`, 
           `inProductionTypePair_IM0`.`useAirborneSpread`, 
           `inProductionTypePair_IM0`.`airborneContactSpreadID`  
    from `inProductionTypePair_IM0`;


drop table `inProductionTypePair_IM0`; 
---AR

-- OK to here!

alter table `inDiseaseSpread`
	add constraint `inProductionTypePair_PK` primary key (`spreadID`) ; 


-- OK

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
		`detProbReportClinVsFirstDetectionRelID`)
	 references `inChart` (
		`chartID`); 

alter table `inProductionType`
	add constraint `inChart_inProductionType_FK7` foreign key (
		`vaccImmunePeriodPdfID`)
	 references `inChart` (
		`chartID`); 

alter table `inProductionType`
	add constraint `inChart_inProductionType_FK8` foreign key (
		`disPrevalenceInfectedRelID`)
	 references `inChart` (
		`chartID`); 

alter table `inProductionType`
	add constraint `inChart_inProductionType_FK9` foreign key (
		`traceDelayPdfID`)
	 references `inChart` (
		`chartID`); 

alter table `inProductionType`
	add constraint `inChart_inProductionType_FK10` foreign key (
		`testDelayPdfID`)
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

alter table `outIterationByZoneAndProductionType`
	add constraint `inProductionType_outIterationByZoneAndProductionType_FK1` foreign key (
		`productionTypeID`)
	 references `inProductionType` (
		`productionTypeID`); 

alter table `outDailyByZoneAndProductionType`
	add constraint `inProductionType_outDailyByZoneAndProductionType_FK1` foreign key (
		`productionTypeID`)
	 references `inProductionType` (
		`productionTypeID`); 

alter table `outDailyByProductionType`
	add constraint `inProductionType_outDailyByProductionType_FK1` foreign key (
		`productionTypeID`)
	 references `inProductionType` (
		`productionTypeID`); 

alter table `inDiseaseSpread`
	add constraint `inChart_inProductionTypePair_FK1` foreign key (
		`distancePdfID`)
	 references `inChart` (
		`chartID`); 

alter table `inDiseaseSpread`
	add constraint `inChart_inProductionTypePair_FK3` foreign key (
		`movementControlRelID`)
	 references `inChart` (
		`chartID`); 

alter table `inDiseaseSpread`
	add constraint `readSpreadMethodCodes_inDiseaseSpread_FK1` foreign key (
		`spreadMethodCode`)
	 references `readSpreadMethodCodes` (
		`spreadMethodCode`); 
    
alter table `outDailyExposures`
	add constraint `readSpreadMethodCodes_outDailyExposures_FK1` foreign key (
		`spreadMethodCode`)
	 references `readSpreadMethodCodes` (
		`spreadMethodCode`);    

alter table `inProductionTypePair`
	add constraint `inDiseaseSpread_inProductionTypePairs_FK1` foreign key (
		`directContactSpreadID`)
	 references `inDiseaseSpread` (
		`spreadID`); 

alter table `inProductionTypePair`
	add constraint `inDiseaseSpread_inProductionTypePairs_FK2` foreign key (
		`indirectContactSpreadID`)
	 references `inDiseaseSpread` (
		`spreadID`); 

alter table `inProductionTypePair`
	add constraint `inDiseaseSpread_inProductionTypePairs_FK3` foreign key (
		`airborneContactSpreadID`)
	 references `inDiseaseSpread` (
		`spreadID`);
		
alter table `inProductionTypePair`
	add constraint `inDiseaseSpread_inProductionTypePair_FK4` foreign key (
		`localAreaSpreadID`)
	 references `inDiseaseSpread` (
		`spreadID`); 

alter table `inDiseaseSpread`
	add constraint `inChart_inDiseaseSpread_FK3` foreign key (
		`dirPropnUnitsInShipmentPdfID`)
	 references `inChart` (
		`chartID`); 

alter table `inProductionType`
	add constraint `inChart_inProductionType_FK11` foreign key (
		`detProbObsVsTimeDeadRelID`)
	 references `inChart` (
		`chartID`); 

alter table `inProductionType`
	add constraint `inChart_inProductionType_FK12` foreign key (
		`disPrevalenceSheddingRelID`)
	 references `inChart` (
		`chartID`); 

alter table `inProductionType`
	add constraint `readCostApplicationCodes_inProductionType_FK1` foreign key (
		`costDestrAppraiseDeadUnitsCode`)
	 references `readCostApplicationCodes` (
		`costApplicationCode`); 

alter table `inProductionType`
	add constraint `readCostApplicationCodes_inProductionType_FK2` foreign key (
		`costDestrCleanDeadUnitsCode`)
	 references `readCostApplicationCodes` (
		`costApplicationCode`); 

alter table `inProductionType`
	add constraint `readCostApplicationCodes_inProductionType_FK3` foreign key (
		`costDestrIndemnifyDeadUnitsCode`)
	 references `readCostApplicationCodes` (
		`costApplicationCode`); 

alter table `inProductionType`
	add constraint `readCostApplicationCodes_inProductionType_FK4` foreign key (
		`costDestrDisposeDeadUnitsCode`)
	 references `readCostApplicationCodes` (
		`costApplicationCode`); 
      
alter table `inProductionType`
	add constraint `inChart_inProductionType_FK13` foreign key (
		`detProbReportDeadVsFirstDetectionRelID`)
	 references `inChart` (
		`chartID`); 

update `inChart` set `fieldName` = "DetProbReportClinVsFirstDetection" where `fieldName` = "DetProbReportVsFirstDetection";

insert into `readDetectionStateCodes` ( `detectionStateCode`, `definition` ) values ( "X", "Detected as dead from disease" );
insert into `readStatusCodes` ( `statusCode`, `definition` ) values ( "X", "Dead from disease" );

insert into `readCostApplicationCodes` ( `costApplicationCode`, `definition` ) values ( "A", "Cost always applies" );
insert into `readCostApplicationCodes` ( `costApplicationCode`, `definition` ) values ( "N", "Cost never applies" );
insert into `readCostApplicationCodes` ( `costApplicationCode`, `definition` ) values ( "D", "Cost applies only after a unit has been queued for destruction" );

insert into `readSpreadMethodCodes` ( `spreadMethodCode`, `definition` ) values( "L", "Local area spread" );

update DBSchemaVersion SET versionDate = '2001-03-23 07:26:26', versionID = 1300929986, versionNumber = '4.0.5';












