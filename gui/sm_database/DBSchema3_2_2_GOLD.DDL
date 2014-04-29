-- Drop foreign keys that use fields in the lookup tables
alter table `dynHerd`
	drop constraint `readDetectionStateCodes_dynHerd_FK1`;
	
alter table `outDailyEvents`
	drop constraint `readEventCodes_outDailyEvents_FK1`;


-- Update the lookup tables
update `readEventCodes` set `eventCode` = "M" where `eventCode` = "X";
update `readDetectionStateCodes` set `detectionStateCode` = "M" where `detectionStateCode` = "X";


-- Update tables that use the lookup values
update `dynHerd` set `finalDetectionStateCode` = "M" where `finalDetectionStateCode` = "X";
update `outDailyEvents` set `eventCode` = "M" where `eventCode` = "X";


-- Recreate foreign keys
alter table `dynHerd`
	add constraint `readDetectionStateCodes_dynHerd_FK1` foreign key (
		`finalDetectionStateCode`)
	 references `readDetectionStateCodes` (
		`detectionStateCode`); 

alter table `outDailyEvents`
	add constraint `readEventCodes_outDailyEvents_FK1` foreign key (
		`eventCode`)
	 references `readEventCodes` (
		`eventCode`); 