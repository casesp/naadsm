-- This update, in the NAADSM 4 line, accomplishes the same goal that
-- schema update 3.2.5 accomplishes in the NAADSM 3.2 line.

UPDATE `inChart` SET `yAxisUnits` = "Percent" WHERE `fieldName` = "ZONMovementDirect";
UPDATE `inChart` SET `yAxisUnits` = "Percent" WHERE `fieldName` = "ZONMovementIndirect";
UPDATE `inChart` SET `yAxisUnits` = "Percent" WHERE `fieldName` = "DPrevInfected";
UPDATE `inChart` SET `yAxisUnits` = "Percent" WHERE `fieldName` = "DPrevShedding";
UPDATE `inChart` SET `yAxisUnits` = "Percent" WHERE `fieldName` = "CMMovementControlIndirect";
UPDATE `inChart` SET `yAxisUnits` = "Percent" WHERE `fieldName` = "CMMovementControlDirect";
UPDATE `inChart` SET `yAxisUnits` = "Percent probability" WHERE `fieldName` = "DetProbObsVsTimeClinical";
UPDATE `inChart` SET `yAxisUnits` = "Percent probability" WHERE `fieldName` = "DetProbObsVsTimeDead";
UPDATE `inChart` SET `yAxisUnits` = "Percent probability" WHERE `fieldName` = "DetProbReportVsFirstDetection";



