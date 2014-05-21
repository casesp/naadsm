CREATE TABLE "DBSchemaVersion" (
  "VersionNumber" varchar(255),
  "VersionApplication" Char(10),
  "VersionDate" timestamp,
  "VersionInfoURL" varchar(255),
  "VersionID" integer
);

CREATE TABLE "inScenario" (
  "scenarioID" serial,
  "descr" text
);

CREATE TABLE "inProductionType" (
  "productionTypeID" serial,
  "scenarioID" integer,
  "descr" varchar(255)
);

CREATE TABLE "inZone" (
  "zoneID" serial,
  "scenarioID" integer,
  "descr" varchar(255)
);

CREATE TABLE "outDailyByProductionType" (
  "scenarioID" integer,
  "iteration" integer,
  "productionTypeID" integer,
  "day" integer,
  "tsdUSusc" integer,
  "tsdASusc" integer,
  "tsdULat" integer,
  "tsdALat" integer,
  "tsdUSubc" integer,
  "tsdASubc" integer,
  "tsdUClin" integer,
  "tsdAClin" integer,
  "tsdUNImm" integer,
  "tsdANImm" integer,
  "tsdUVImm" integer,
  "tsdAVImm" integer,
  "tsdUDest" integer,
  "tsdADest" integer,
  "tscUSusc" integer,
  "tscASusc" integer,
  "tscULat" integer,
  "tscALat" integer,
  "tscUSubc" integer,
  "tscASubc" integer,
  "tscUClin" integer,
  "tscAClin" integer,
  "tscUNImm" integer,
  "tscANImm" integer,
  "tscUVImm" integer,
  "tscAVImm" integer,
  "tscUDest" integer,
  "tscADest" integer,
  "infnUAir" integer,
  "infnAAir" integer,
  "infnUDir" integer,
  "infnADir" integer,
  "infnUInd" integer,
  "infnAInd" integer,
  "infcUIni" integer,
  "infcAIni" integer,
  "infcUAir" integer,
  "infcAAir" integer,
  "infcUDir" integer,
  "infcADir" integer,
  "infcUInd" integer,
  "infcAInd" integer,
  "expcUDir" integer,
  "expcADir" integer,
  "expcUInd" integer,
  "expcAInd" integer,
  "trcUDirFwd" integer,
  "trcADirFwd" integer,
  "trcUIndFwd" integer,
  "trcAIndFwd" integer,
  "trcUDirpFwd" integer,
  "trcADirpFwd" integer,
  "trcUIndpFwd" integer,
  "trcAIndpFwd" integer,
  "tocUDirFwd" integer,
  "tocUIndFwd" integer,
  "tocUDirBack" integer,
  "tocUIndBack" integer,
  "trnUDirFwd" integer,
  "trnADirFwd" integer,
  "trnUIndFwd" integer,
  "trnAIndFwd" integer,
  "trcUDirBack" integer,
  "trcADirBack" integer,
  "trcUIndBack" integer,
  "trcAIndBack" integer,
  "trcUDirpBack" integer,
  "trcADirpBack" integer,
  "trcUIndpBack" integer,
  "trcAIndpBack" integer,
  "trnUDirBack" integer,
  "trnADirBack" integer,
  "trnUIndBack" integer,
  "trnAIndBack" integer,
  "tonUDirFwd" integer,
  "tonUIndFwd" integer,
  "tonUDirBack" integer,
  "tonUIndBack" integer,
  "exmcUDirFwd" integer,
  "exmcADirFwd" integer,
  "exmcUIndFwd" integer,
  "exmcAIndFwd" integer,
  "exmcUDirBack" integer,
  "exmcADirBack" integer,
  "exmcUIndBack" integer,
  "exmcAIndBack" integer,
  "exmnUAll" integer,
  "exmnAAll" integer,
  "tstcUDirFwd" integer,
  "tstcADirFwd" integer,
  "tstcUIndFwd" integer,
  "tstcAIndFwd" integer,
  "tstcUDirBack" integer,
  "tstcADirBack" integer,
  "tstcUIndBack" integer,
  "tstcAIndBack" integer,
  "tstcUTruePos" integer,
  "tstcATruePos" integer,
  "tstnUTruePos" integer,
  "tstnATruePos" integer,
  "tstcUTrueNeg" integer,
  "tstcATrueNeg" integer,
  "tstnUTrueNeg" integer,
  "tstnATrueNeg" integer,
  "tstcUFalsePos" integer,
  "tstcAFalsePos" integer,
  "tstnUFalsePos" integer,
  "tstnAFalsePos" integer,
  "tstcUFalseNeg" integer,
  "tstcAFalseNeg" integer,
  "tstnUFalseNeg" integer,
  "tstnAFalseNeg" integer,
  "detnUClin" integer,
  "detnAClin" integer,
  "detcUClin" integer,
  "detcAClin" integer,
  "detnUTest" integer,
  "detnATest" integer,
  "detcUTest" integer,
  "detcATest" integer,
  "descUIni" integer,
  "descAIni" integer,
  "descUDet" integer,
  "descADet" integer,
  "descUDirFwd" integer,
  "descADirFwd" integer,
  "descUIndFwd" integer,
  "descAIndFwd" integer,
  "descUDirBack" integer,
  "descADirBack" integer,
  "descUIndBack" integer,
  "descAIndBack" integer,
  "descURing" integer,
  "descARing" integer,
  "desnUAll" integer,
  "desnAAll" integer,
  "deswUAll" integer,
  "deswAAll" double precision,
  "vaccUIni" integer,
  "vaccAIni" integer,
  "vaccURing" integer,
  "vaccARing" integer,
  "vacnUAll" integer,
  "vacnAAll" integer,
  "vacwUAll" integer,
  "vacwAAll" double precision,
  "zonnFoci" integer,
  "zoncFoci" integer,
  "appdUInfectious" integer
);

CREATE TABLE "outDailyByZone" (
  "scenarioID" integer,
  "iteration" integer,
  "day" integer,
  "zoneID" integer,
  "zoneArea" double precision,
  "zonePerimeter" double precision
);

CREATE TABLE "outDailyByZoneAndProductionType" (
  "scenarioID" integer,
  "iteration" integer,
  "day" integer,
  "zoneID" integer,
  "productionTypeID" integer,
  "unitDaysInZone" integer,
  "animalDaysInZone" integer,
  "unitsInZone" integer,
  "animalsInZone" integer
);

CREATE TABLE "outEpidemicCurves" (
  "scenarioID" integer,
  "iteration" integer,
  "day" integer,
  "productionTypeID" integer,
  "infectedUnits" integer,
  "infectedAnimals" integer,
  "detectedUnits" integer,
  "detectedAnimals" integer,
  "infectiousUnits" integer,
  "apparentInfectiousUnits" integer
);

CREATE TABLE "outGeneral" (
  "scenarioID" integer,
  "outGeneralID" Char(10),
  "simulationStartTime" timestamp,
  "simulationEndTime" timestamp,
  "completedIterations" integer,
  "version" varchar(50)
);

CREATE TABLE "outIteration" (
  "scenarioID" integer,
  "iteration" integer,
  "diseaseEnded" boolean,
  "diseaseEndDay" integer,
  "outbreakEnded" boolean,
  "outbreakEndDay" integer,
  "zoneFociCreated" boolean,
  "deswUMax" integer,
  "deswUMaxDay" integer,
  "deswAMax" double precision,
  "deswAMaxDay" integer,
  "deswUTimeMax" integer,
  "deswUTimeAvg" double precision,
  "vacwUMax" integer,
  "vacwUMaxDay" integer,
  "vacwAMax" double precision,
  "vacwAMaxDay" integer,
  "vacwUTimeMax" integer,
  "vacwUTimeAvg" double precision
);

CREATE TABLE "outIterationByProductionType" (
  "scenarioID" integer,
  "iteration" integer,
  "productionTypeID" integer,
  "tscUSusc" integer,
  "tscASusc" integer,
  "tscULat" integer,
  "tscALat" integer,
  "tscUSubc" integer,
  "tscASubc" integer,
  "tscUClin" integer,
  "tscAClin" integer,
  "tscUNImm" integer,
  "tscANImm" integer,
  "tscUVImm" integer,
  "tscAVImm" integer,
  "tscUDest" integer,
  "tscADest" integer,
  "infcUIni" integer,
  "infcAIni" integer,
  "infcUAir" integer,
  "infcAAir" integer,
  "infcUDir" integer,
  "infcADir" integer,
  "infcUInd" integer,
  "infcAInd" integer,
  "expcUDir" integer,
  "expcADir" integer,
  "expcUInd" integer,
  "expcAInd" integer,
  "trcUDirFwd" integer,
  "trcADirFwd" integer,
  "trcUIndFwd" integer,
  "trcAIndFwd" integer,
  "trcUDirpFwd" integer,
  "trcADirpFwd" integer,
  "trcUIndpFwd" integer,
  "trcAIndpFwd" integer,
  "trcUDirBack" integer,
  "trcADirBack" integer,
  "trcUIndBack" integer,
  "trcAIndBack" integer,
  "trcUDirpBack" integer,
  "trcADirpBack" integer,
  "trcUIndpBack" integer,
  "trcAIndpBack" integer,
  "tocUDirFwd" integer,
  "tocUIndFwd" integer,
  "tocUDirBack" integer,
  "tocUIndBack" integer,
  "exmcUDirFwd" integer,
  "exmcADirFwd" integer,
  "exmcUIndFwd" integer,
  "exmcAIndFwd" integer,
  "exmcUDirBack" integer,
  "exmcADirBack" integer,
  "exmcUIndBack" integer,
  "exmcAIndBack" integer,
  "tstcUDirFwd" integer,
  "tstcADirFwd" integer,
  "tstcUIndFwd" integer,
  "tstcAIndFwd" integer,
  "tstcUDirBack" integer,
  "tstcADirBack" integer,
  "tstcUIndBack" integer,
  "tstcAIndBack" integer,
  "tstcUTruePos" integer,
  "tstcATruePos" integer,
  "tstcUTrueNeg" integer,
  "tstcATrueNeg" integer,
  "tstcUFalsePos" integer,
  "tstcAFalsePos" integer,
  "tstcUFalseNeg" integer,
  "tstcAFalseNeg" integer,
  "detcUClin" integer,
  "detcAClin" integer,
  "detcUTest" integer,
  "detcATest" integer,
  "descUIni" integer,
  "descAIni" integer,
  "descUDet" integer,
  "descADet" integer,
  "descUDirFwd" integer,
  "descADirFwd" integer,
  "descUIndFwd" integer,
  "descAIndFwd" integer,
  "descUDirBack" integer,
  "descADirBack" integer,
  "descUIndBack" integer,
  "descAIndBack" integer,
  "descURing" integer,
  "descARing" integer,
  "deswUMax" integer,
  "deswAMax" double precision,
  "deswUMaxDay" integer,
  "deswAMaxDay" integer,
  "deswUTimeMax" integer,
  "deswUTimeAvg" double precision,
  "deswUDaysInQueue" double precision,
  "deswADaysInQueue" double precision,
  "vaccUIni" integer,
  "vaccAIni" integer,
  "vaccURing" integer,
  "vaccARing" integer,
  "vacwUMax" integer,
  "vacwAMax" double precision,
  "vacwUMaxDay" integer,
  "vacwAMaxDay" integer,
  "vacwUTimeMax" integer,
  "vacwUTimeAvg" double precision,
  "zoncFoci" integer,
  "firstDetection" integer,
  "firstDetUInf" integer,
  "firstDetAInf" integer,
  "firstDestruction" integer,
  "firstVaccination" integer,
  "lastDetection" integer
);

CREATE TABLE "outIterationByZone" (
  "scenarioID" integer,
  "iteration" integer,
  "zoneID" integer,
  "maxZoneArea" double precision,
  "maxZoneAreaDay" integer,
  "finalZoneArea" double precision,
  "maxZonePerimeter" double precision,
  "maxZonePerimeterDay" integer,
  "finalZonePerimeter" double precision
);

CREATE TABLE "outIterationByZoneAndProductionType" (
  "scenarioID" integer,
  "iteration" integer,
  "zoneID" integer,
  "productionTypeID" integer,
  "unitDaysInZone" integer,
  "animalDaysInZone" integer,
  "costSurveillance" money
);

-- Table keys and constraints
-- Primary key for table "DBSchemaVersion":
CREATE UNIQUE INDEX "DBSchemaVersion_PK" ON "DBSchemaVersion" ( "VersionApplication", "VersionNumber" );
ALTER TABLE "DBSchemaVersion" ADD PRIMARY KEY USING INDEX "DBSchemaVersion_PK";


-- Primary key for table "inScenario":
CREATE UNIQUE INDEX "inScenario_PK" ON "inScenario" ( "scenarioID" );
ALTER TABLE "inScenario" ADD PRIMARY KEY USING INDEX "inScenario_PK";


-- Primary key for table "inProductionType":
CREATE UNIQUE INDEX "inProductionType_PK" ON "inProductionType" ( "productionTypeID", "scenarioID" );
ALTER TABLE "inProductionType" ADD PRIMARY KEY USING INDEX "inProductionType_PK";

-- Other indices for table "inProductionType":
CREATE INDEX "inScenario_inProductionType_FK1" ON "inProductionType" ( "scenarioID" );


-- Primary key for table "inZone":
CREATE UNIQUE INDEX "inZone_PK" ON "inZone" ( "zoneID", "scenarioID" );
ALTER TABLE "inZone" ADD PRIMARY KEY USING INDEX "inZone_PK";

-- Other indices for table "inProductionType":
CREATE INDEX "inScenario_inZone_FK1" ON "inZone" ( "scenarioID" );


-- Primary key for table "outDailyByProductionType":
CREATE UNIQUE INDEX "outDailyByProductionType_PK" ON "outDailyByProductionType" ( "productionTypeID", "day", "iteration", "scenarioID" );
ALTER TABLE "outDailyByProductionType" ADD PRIMARY KEY USING INDEX "outDailyByProductionType_PK";

-- Other indices for table "outDailyByProductionType"
CREATE INDEX "inProductionType_outDailyByProductionType_FK1" ON "outDailyByProductionType" ( "productionTypeID" );
CREATE INDEX "inScenario_outDailyByProductionType_FK1" ON "outDailyByProductionType" ( "scenarioID" );
CREATE INDEX "outDailyByProductionType_I1" ON "outDailyByProductionType" ( "iteration" );
CREATE INDEX "outDailyByProductionType_I2" ON "outDailyByProductionType" ( "day" );


-- Primary key for table "outDailyByZone":
CREATE UNIQUE INDEX "outDailyByZone_PK" ON "outDailyByZone" ( "iteration", "day", "zoneID", "scenarioID" );
ALTER TABLE "outDailyByZone" ADD PRIMARY KEY USING INDEX "outDailyByZone_PK";

-- Other indices for table "outDailyByZone"
CREATE INDEX "inZone_outDailyByZone_FK1" ON "outDailyByZone" ( "zoneID" );
CREATE INDEX "inScenario_outDailyByZone_FK1" ON "outDailyByZone" ( "scenarioID" );
CREATE INDEX "outDailyByZone_I1" ON "outDailyByZone" ( "iteration" );
CREATE INDEX "outDailyByZone_I2" ON "outDailyByZone" ( "day" );


-- Primary key for table "outDailyByZoneAndProductionType":
CREATE UNIQUE INDEX "outDailyByZoneAndProductionType_PK" ON "outDailyByZoneAndProductionType" ( "day", "iteration", "zoneID", "productionTypeID", "scenarioID" );
ALTER TABLE "outDailyByZoneAndProductionType" ADD PRIMARY KEY USING INDEX "outDailyByZoneAndProductionType_PK";

-- Other indices for table "outDailyByZoneAndProductionType"
CREATE INDEX "inProductionType_outDailyByZoneAndProductionType_FK1" ON "outDailyByZoneAndProductionType" ( "productionTypeID" );
CREATE INDEX "inZone_outDailyByZoneAndProductionType_FK1" ON "outDailyByZoneAndProductionType" ( "zoneID" );
CREATE INDEX "inScenario_outDailyByZoneAndProductionType_FK1" ON "outDailyByZoneAndProductionType" ( "scenarioID" );
CREATE INDEX "outDailyByZoneAndProductionType_I1" ON "outDailyByZoneAndProductionType" ( "iteration" );
CREATE INDEX "outDailyByZoneAndProductionType_I2" ON "outDailyByZoneAndProductionType" ( "day" );


-- Primary key for table "outEpidemicCurves":
CREATE UNIQUE INDEX "outEpidemicCurves_PK" ON "outEpidemicCurves" ( "iteration", "day", "productionTypeID", "scenarioID" );
ALTER TABLE "outEpidemicCurves" ADD PRIMARY KEY USING INDEX "outEpidemicCurves_PK";

-- Other indices for table "outEpidemicCurves"
CREATE INDEX "inProductionType_outEpidemicCurves_FK1" ON "outEpidemicCurves" ( "productionTypeID" );
CREATE INDEX "inScenario_outEpidemicCurves_FK1" ON "outEpidemicCurves" ( "scenarioID" );
CREATE INDEX "outEpidemicCurves_I1" ON "outEpidemicCurves" ( "iteration" );
CREATE INDEX "outEpidemicCurves_I2" ON "outEpidemicCurves" ( "day" );


-- Primary key for table "outGeneral":
CREATE UNIQUE INDEX "outGeneral_PK" ON "outGeneral" ( "outGeneralID", "scenarioID" );
ALTER TABLE "outGeneral" ADD PRIMARY KEY USING INDEX "outGeneral_PK";

-- Other indices for table "outGeneral":
CREATE INDEX "inScenario_outGeneral_FK1" ON "outGeneral" ( "scenarioID" );


-- Primary key for table "outIteration":
CREATE UNIQUE INDEX "outIteration_PK" ON "outIteration" ( "iteration", "scenarioID" );
ALTER TABLE "outIteration" ADD PRIMARY KEY USING INDEX "outIteration_PK";

-- Other indices for table "outIteration":
CREATE INDEX "inScenario_outIteration_FK1" ON "outIteration" ( "scenarioID" );
CREATE INDEX "outIteration_I1" ON "outIteration" ( "iteration" );


-- Primary key for table "outIterationByProductionType":
CREATE UNIQUE INDEX "outDailyEpiByProductionType_PK" ON "outIterationByProductionType" ( "iteration", "productionTypeID", "scenarioID" );
ALTER TABLE "outIterationByProductionType" ADD PRIMARY KEY USING INDEX "outDailyEpiByProductionType_PK";

-- Other indices for table "outIterationByProductionType"
CREATE INDEX "inProductionType_outIterationByProductionType_FK1" ON "outIterationByProductionType" ( "productionTypeID" );
CREATE INDEX "inScenario_outIterationByProductionType_FK1" ON "outIterationByProductionType" ( "scenarioID" );
CREATE INDEX "outIterationByProductionType_I1" ON "outIterationByProductionType" ( "iteration" );


-- Primary key for table "outIterationByZone":
CREATE UNIQUE INDEX "outIterationByZone_PK" ON "outIterationByZone" ( "iteration", "zoneID", "scenarioID" );
ALTER TABLE "outIterationByZone" ADD PRIMARY KEY USING INDEX "outIterationByZone_PK";

-- Other indices for table "outIterationByZone"
CREATE INDEX "inZone_outIterationByZone_FK1" ON "outIterationByZone" ( "zoneID" );
CREATE INDEX "inScenario_outIterationByZone_FK1" ON "outIterationByZone" ( "scenarioID" );
CREATE INDEX "outIterationByZone_I1" ON "outIterationByZone" ( "iteration" );


-- Primary key for table "outIterationByZoneAndProductionType":
CREATE UNIQUE INDEX "outIterationByZoneAndProductionType_PK" ON "outIterationByZoneAndProductionType" ( "iteration", "zoneID", "productionTypeID", "scenarioID" );
ALTER TABLE "outIterationByZoneAndProductionType" ADD PRIMARY KEY USING INDEX "outIterationByZoneAndProductionType_PK";

-- Other indices for table "outIterationByZoneAndProductionType"
CREATE INDEX "inProductionType_outIterationByZoneAndProductionType_FK1" ON "outIterationByZoneAndProductionType" ( "productionTypeID" );
CREATE INDEX "inZone_outIterationByZoneAndProductionType_FK1" ON "outIterationByZoneAndProductionType" ( "zoneID" );
CREATE INDEX "inScenario_outIterationByZoneAndProductionType_FK1" ON "outIterationByZoneAndProductionType" ( "scenarioID" );
CREATE INDEX "outIterationByZoneAndProductionType_I1" ON "outIterationByZoneAndProductionType" ( "iteration" );

-- Foreign keys
ALTER TABLE "outDailyByProductionType"
  ADD CONSTRAINT "inProductionType_outDailyByProductionType_FK" FOREIGN KEY ( "productionTypeID", "scenarioID" )
  REFERENCES "inProductionType" ( "productionTypeID", "scenarioID" );
  
  
ALTER TABLE "outDailyByZoneAndProductionType"
  ADD CONSTRAINT "inProductionType_outDailyByZoneAndProductionType_FK" FOREIGN KEY ( "productionTypeID", "scenarioID" )
  REFERENCES "inProductionType" ( "productionTypeID", "scenarioID" );
  
ALTER TABLE "outDailyByZoneAndProductionType"
  ADD CONSTRAINT "inZone_outDailyByZoneAndProductionType_FK" FOREIGN KEY ( "zoneID", "scenarioID" )
  REFERENCES "inZone" ( "zoneID", "scenarioID" ); 

ALTER TABLE "outDailyByZoneAndProductionType"
  ADD CONSTRAINT "inScenario_outDailyByZoneAndProductionType_FK" FOREIGN KEY ( "scenarioID" )
  REFERENCES "inScenario" ( "scenarioID" );   
  

ALTER TABLE "outEpidemicCurves"
  ADD CONSTRAINT "inProductionType_outEpidemicCurves_FK" FOREIGN KEY ( "productionTypeID", "scenarioID" )
  REFERENCES "inProductionType" ( "productionTypeID", "scenarioID" );

ALTER TABLE "outEpidemicCurves"
  ADD CONSTRAINT "inScenario_outEpidemicCurves_FK" FOREIGN KEY ( "scenarioID" )
  REFERENCES "inScenario" ( "scenarioID" );    

  
ALTER TABLE "outIterationByProductionType"
  ADD CONSTRAINT "inProductionType_outIterationByProductionType_FK" FOREIGN KEY ( "productionTypeID", "scenarioID" )
  REFERENCES "inProductionType" ( "productionTypeID", "scenarioID" );

ALTER TABLE "outIterationByProductionType"
  ADD CONSTRAINT "inScenario_outIterationByProductionType_FK" FOREIGN KEY ( "scenarioID" )
  REFERENCES "inScenario" ( "scenarioID" );   

  
ALTER TABLE "outIterationByZoneAndProductionType"
  ADD CONSTRAINT "inProductionType_outIterationByZoneAndProductionType_FK" FOREIGN KEY ( "productionTypeID", "scenarioID" )
  REFERENCES "inProductionType" ( "productionTypeID", "scenarioID" );

ALTER TABLE "outIterationByZoneAndProductionType"
  ADD CONSTRAINT "inZone_outIterationByZoneAndProductionType_FK" FOREIGN KEY ( "zoneID", "scenarioID" )
  REFERENCES "inZone" ( "zoneID", "scenarioID" );
  
ALTER TABLE "outIterationByZoneAndProductionType"
  ADD CONSTRAINT "inScenario_outIterationByZoneAndProductionType_FK" FOREIGN KEY ( "scenarioID" )
  REFERENCES "inScenario" ( "scenarioID" );    
 
 
ALTER TABLE "outDailyByZone"
  ADD CONSTRAINT "inSurveillanceZone_outDailyByZone_FK" FOREIGN KEY ( "zoneID", "scenarioID" )
  REFERENCES "inZone" ( "zoneID", "scenarioID" );

ALTER TABLE "outDailyByZone"
  ADD CONSTRAINT "inScenario_outDailyByZone_FK" FOREIGN KEY ( "scenarioID" )
  REFERENCES "inScenario" ( "scenarioID" );   


ALTER TABLE "outIterationByZone"
  ADD CONSTRAINT "inZone_outIterationByZone_FK" FOREIGN KEY ( "zoneID", "scenarioID" )
  REFERENCES "inZone" ( "zoneID", "scenarioID" );

ALTER TABLE "outIterationByZone"
  ADD CONSTRAINT "inScenario_outIterationByZone_FK" FOREIGN KEY ( "scenarioID" )
  REFERENCES "inScenario" ( "scenarioID" );    

  
ALTER TABLE "outIteration"
  ADD CONSTRAINT "inScenario_outIteration_FK" FOREIGN KEY ( "scenarioID" )
  REFERENCES "inScenario" ( "scenarioID" );      

  
ALTER TABLE "outGeneral"
  ADD CONSTRAINT "inScenario_outGeneral_FK" FOREIGN KEY ( "scenarioID" )
  REFERENCES "inScenario" ( "scenarioID" );    

  
ALTER TABLE "inProductionType"
  ADD CONSTRAINT "inScenario_inProductionType_FK" FOREIGN KEY ( "scenarioID" )
  REFERENCES "inScenario" ( "scenarioID" );   


ALTER TABLE "inZone"
  ADD CONSTRAINT "inScenario_inZone_FK" FOREIGN KEY ( "scenarioID" )
  REFERENCES "inScenario" ( "scenarioID" );  

  