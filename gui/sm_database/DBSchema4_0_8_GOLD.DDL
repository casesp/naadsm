alter table outDailyByProductionType alter column vacwAAll double;
alter table outDailyByProductionType alter column deswAAll double;

alter table outDailyByProductionType add column desnUDcd long;
alter table outDailyByProductionType add column desnADcd long;
alter table outDailyByProductionType add column desnUDcdInDestrQueue long;
alter table outDailyByProductionType add column desnADcdInDestrQueue long; 

alter table inSelectDailyOutputs add column sdesnUDcd bit;
alter table inSelectDailyOutputs add column sdesnADcd bit;
alter table inSelectDailyOutputs add column sdesnUDcdInDestrQueue bit;
alter table inSelectDailyOutputs add column sdesnADcdInDestrQueue bit; 
