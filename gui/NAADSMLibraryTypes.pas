unit NAADSMLibraryTypes;

(*
NAADSMLibraryTypes.pas
----------------------
Begin: 2009/07/17
Last revision: $Date: 2013-04-02 19:47:37 $ $Author: areeves $
Version: $Revision: 1.14.2.15 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2009 - 2013 Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
    Graphics
  ;

  // Force enums to be the size of integers for compatibility with C.
  // See http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/devcommon/compdirsminimumenumerationsize_xml.html
  // or Delphi help for more details.
  {$MINENUMSIZE 4}

  // Used to indicate success of failure of exposures and traces
  type TNAADSMSuccess = (
    NAADSMSuccessUnspecified,
    NAADSMSuccessTrue,
    NAADSMSuccessFalse
  );

  // Used to indicate trace direction
  type TNAADSMTraceDirection = (
    NAADSMTraceNeither,
    NAADSMTraceForwardOrOut,
    NAADSMTraceBackOrIn
  );

  // Used to indicate type of exposure, contact, or infection
  type TNAADSMContactType = (
    NAADSMUnspecifiedInfectionType,
    NAADSMDirectContact,
    NAADSMIndirectContact,
    NAADSMAirborneSpread,
    NAADSMInitiallyInfected,
    NAADSMLocalAreaSpread
  );

  // Used to indicate diagnostic test results
  type TNAADSMTestResult = (
    NAADSMTestUnspecified,
    NAADSMTestTruePositive,
    NAADSMTestTrueNegative,
    NAADSMTestFalsePositive,
    NAADSMTestFalseNegative
  );

  // Used to indicate reasons for detection
  type TNAADSMDetectionReason = (
    NAADSMDetectionReasonUnspecified,
    NAADSMDetectionClinicalSigns,
    NAADSMDetectionDiagnosticTest,
    NAADSMDetectionDeadFromDisease,
    NAADSMDetectionDeadFromDiseaseByVaccinationTeam,
    NAADSMDetectionDeadFromDiseaseByDestructionTeam
  );

  // Used to indicate reasons for control activities
  type TNAADSMControlReason = (
    NAADSMControlReasonUnspecified,
    NAADSMControlRing,
    NAADSMControlTraceForwardDirect,
    NAADSMControlTraceForwardIndirect,
    NAADSMControlTraceBackDirect,
    NAADSMControlTraceBackIndirect,
    NAADSMControlDetection,
    NAADSMControlDetectionDeadFromDisease,
    NAADSMControlInitialState,
    NAADSMControlRoutine // Currently unused in NAADSM 4, but included here for possible porting of routine/prophylactic vaccination back to version 4
  );

  // Used when a herd's actual disease state changes
  type TNAADSMDiseaseState = (
  	NAADSMStateSusceptible,
    NAADSMStateLatent,
    NAADSMStateSubclinical,
    NAADSMStateClinical,
    NAADSMStateNaturallyImmune,
    NAADSMStateVaccineImmune,
    NAADSMStateDestroyed,
    NAADSMStateDeadFromDisease,
    NAADSMStateUnspecified
  );

  {* Record type used in combination with the DLL when a herd's actual disease state changes. }
  type THRDUpdate = record
      herdIndex: integer;
      status: TNAADSMDiseaseState;
    end
  ;

  {* Record type used in combination with the DLL when a herd is infected. }
  type THRDInfect = record
      herdIndex: integer;
      day: integer;
    end
  ;

  {* Record type used in combination with the DLL when a detection occurs. }
  type THRDDetect = record
      herdIndex: integer;
      reason: TNAADSMDetectionReason;
      testResult: TNAADSMTestResult;
    end
  ;

  {* Record type used in combination with the DLL when a herd is destroyed or vaccinated. }
  type THRDControl = record
      herdIndex: integer;
      reason: TNAADSMControlReason;
      dayCommitmentMade: integer;
    end
  ;

  {* Record type used in combination with the DLL when a herd is exposed to infection. }
  type THRDExpose = record
      srcIndex: integer;
      srcStatus: TNAADSMDiseaseState;
      destIndex: integer;
      destStatus: TNAADSMDiseaseState;
      day: integer;
      isAdequate: TNAADSMSuccess;
      exposureMethod: TNAADSMContactType;
    end
  ;

  {* Record type used in combination with the DLL when a herd is traced. }
  type THRDTrace = record
      identifiedIndex: integer;
      identifiedStatus: TNAADSMDiseaseState;
      originIndex: integer;
      originStatus: TNAADSMDiseaseState;
      day: integer;
      initiatedDay: integer;
      success: TNAADSMSuccess;
      traceType: TNAADSMTraceDirection;
      contactType: TNAADSMContactType;
    end
  ;

  {* Record type used in combination with the DLL when a herd is examined after tracing. }
  type THRDExam = record
      herdIndex: integer;
      traceType: TNAADSMTraceDirection;
      contactType: TNAADSMContactType;
      diseaseDetected: TNAADSMSuccess;
    end
  ;

  {* Record type used in combination with the DLL when a herd is diagnostically tested after tracing. }
  type THRDTest = record
      herdIndex: integer;
      testResult: TNAADSMTestResult;
      traceType: TNAADSMTraceDirection;
      contactType: TNAADSMContactType;
    end
  ;

  {* Record type used in combination with the DLL when a herd's zone assignment changes. }
  type THRDZone = record
      herdIndex: integer;
      zoneLevel: integer;
    end
  ;

  // Simple conversion functions for NAADSM types
  //---------------------------------------------
  function naadsmSuccessStr( const val: TNAADSMSuccess ): string;
  function naadsmSuccessIsTrue( const val: TNAADSMSuccess ): boolean;
  function naadsmTraceDirectionStr( const val: TNAADSMTraceDirection ): string;
  function naadsmContactCode( const val: TNAADSMContactType ): char;
  function naadsmContactStr( const val: TNAADSMContactType ): string;
  function naadsmDetectionReasonStr( const val: TNAADSMDetectionReason ): string;
  function naadsmControlReasonStr( const val: TNAADSMControlReason ): string;

  // Disease state conversion and related functions
  //-----------------------------------------------
  function naadsmIsInfectedState( status: TNAADSMDiseaseState ): boolean;
  function naadsmDiseaseStateFromCode( statusCode: char ): TNAADSMDiseaseState;
  function naadsmDiseaseStateFromString( statusStr: string ): TNAADSMDiseaseState;
  function naadsmDiseaseStateCode( status: TNAADSMDiseaseState ): char;
  function naadsmDiseaseStateXml( status: TNAADSMDiseaseState ): string;
  function naadsmDiseaseStateStr( const val: TNAADSMDiseaseState ): string;
  function naadsmDiseaseStateColor( s: TNAADSMDiseaseState ): TColor;
  function naadsmFirstDiseaseState(): TNAADSMDiseaseState;
  function naadsmLastDiseaseState(): TNAADSMDiseaseState;

  // Test result conversion functions
  //---------------------------------
  function naadsmTestResultStr( const val: TNAADSMTestResult ): string;
  function naadsmTestResultFromCode( const val: char ): TNAADSMTestResult;
  function naadsmTestResultCode( const val: TNAADSMTestResult ): char;

  // Debugging functions
  //--------------------
  procedure debugHRDExpose( const e: THRDExpose );
  procedure debugHRDControl( const c: THRDControl );

implementation

  uses
    SysUtils,
    StrUtils,

    MyStrUtils,
    DebugWindow,
    I88n,

    StatusEnums
  ;

//-----------------------------------------------------------------------------
// Simple conversion functions for NAADSM types
//-----------------------------------------------------------------------------
  function naadsmSuccessStr( const val: TNAADSMSuccess ): string;
    begin
      case val of
        NAADSMSuccessTrue: result := tr( 'true' );
        NAADSMSuccessFalse: result := tr( 'false' );
        else
          begin
            raise exception.create( 'Unrecoginized value (' + intToStr( cardinal( val ) ) + ' in naadsmSuccessStr()' );
            result := '';
          end
        ;
      end;
    end
  ;


  function naadsmSuccessIsTrue( const val: TNAADSMSuccess ): boolean;
    begin
      if( NAADSMSuccessUnspecified = val ) then
        raise exception.create( 'NAADSMSuccessUnspecified encountered in naadsmSuccessIsTrue()' )
      ;
      result := ( NAADSMSuccessTrue = val );
    end
  ;


  function naadsmTraceDirectionStr( const val: TNAADSMTraceDirection ): string;
    begin
      case val of
        NAADSMTraceForwardOrOut: result := 'TRACE FORWARD';
        NAADSMTraceBackOrIn: result := 'TRACE BACK';
        else
          begin
            raise exception.create( 'Unrecoginized value (' + intToStr( cardinal( val ) ) + ' in naadsmTraceDirectionStr()' );
            result := '';
          end
        ;
      end;
    end
  ;


  function naadsmContactCode( const val: TNAADSMContactType ): char;
    begin
      case val of
        NAADSMDirectContact: result := 'D';
        NAADSMIndirectContact: result := 'I';
        NAADSMAirborneSpread: result := 'A';
        NAADSMInitiallyInfected: result := 'N';
        NAADSMLocalAreaSpread: result := 'L';
        else
          begin
            raise exception.Create( 'Unrecognized exposure method in TSMExposure.create()' );
            result := 'U';
          end
        ;
      end;
    end
  ;


  function naadsmContactStr( const val: TNAADSMContactType ): string;
    begin
      case val of
        NAADSMDirectContact: result := 'DIRECT CONTACT';
        NAADSMIndirectContact: result := 'INDIRECT CONTACT';
        NAADSMAirborneSpread: result := 'AIRBORNE';
        NAADSMInitiallyInfected: result := 'INITIALLY INFECTED';
        NAADSMLocalAreaSpread: result := 'LOCAL AREA SPREAD';
        else
          begin
            raise exception.create( 'Unrecoginized value (' + intToStr( cardinal( val ) ) + ' in naadsmExposureMethodStr()' );
            result := '';
          end
        ;
      end;
    end
  ;


  function naadsmDetectionReasonStr( const val: TNAADSMDetectionReason ): string;
    begin
      case val of
        NAADSMDetectionClinicalSigns: result := 'DETECTION BY CLINICAL SIGNS';
        NAADSMDetectionDiagnosticTest: result := 'DETECTION BY DIAGNOSTIC TEST';
        NAADSMDetectionDeadFromDisease: result := 'DETECTION BY DEAD FROM DISEASE';
        NAADSMDetectionDeadFromDiseaseByVaccinationTeam: result := 'DETECTED DEAD FROM VACC TEAM';
        NAADSMDetectionDeadFromDiseaseByDestructionTeam: result := 'DETECTED DEAD BY DESTR TEAM';
        else
          begin
            raise exception.create( 'Unrecoginized value (' + intToStr( cardinal( val ) ) + ') in naadsmDetectionReasonStr()' );
            result := '';
          end
        ;
      end;
    end
  ;


  function naadsmControlReasonStr( const val: TNAADSMControlReason ): string;
    begin
      case val of
        NAADSMControlRing: result := 'CONTROL RING';
        NAADSMControlTraceForwardDirect: result := 'CONTROL TRACE FORWARD DIRECT';
        NAADSMControlTraceForwardIndirect: result := 'CONTROL TRACE FORWARD INDIRECT';
        NAADSMControlTraceBackDirect: result := 'CONTROL TRACE BACK DIRECT';
        NAADSMControlTraceBackIndirect: result := 'CONTROL TRACE BACK INDIRECT';
        NAADSMControlDetection: result := 'CONTROL DETECTION';
        NAADSMControlDetectionDeadFromDisease: result := 'CONTROL DETECTION DEAD FROM DISEASE';
        NAADSMControlInitialState: result := 'CONTROL INITIAL STATE';
        else
          begin
            raise exception.create( 'Unrecoginized value (' + intToStr( cardinal( val ) ) + ' in naadsmSuccessStr()' );
            result := '';
          end
        ;
      end;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Disease state conversion and related functions
//-----------------------------------------------------------------------------
  function naadsmFirstDiseaseState(): TNAADSMDiseaseState;
    begin
      result := NAADSMStateSusceptible;
    end
  ;


  function naadsmLastDiseaseState(): TNAADSMDiseaseState;
    begin
      result := NAADSMStateDeadFromDisease;
    end
  ;


  function naadsmIsInfectedState( status: TNAADSMDiseaseState ): boolean;
    begin
      result := ( status in [ NAADSMStateLatent .. NAADSMStateClinical ] );
    end
  ;


  function naadsmDiseaseStateStr( const val: TNAADSMDiseaseState ): string;
    begin
    	case val of
        NAADSMStateSusceptible: result := tr( 'Susceptible' );
        NAADSMStateLatent: result := tr( 'Latent' );
        NAADSMStateSubclinical: result := tr( 'Subclinical' );
        NAADSMStateClinical: result := tr( 'Clinical' );
        NAADSMStateNaturallyImmune: result := tr( 'Naturally immune' );
        NAADSMStateVaccineImmune: result := tr( 'Vaccine immune' );
        NAADSMStateDestroyed: result := tr( 'Destroyed' );
        NAADSMStateDeadFromDisease: result := tr( 'Dead from disease' );
        NAADSMStateUnspecified:
          begin
            raise exception.create( 'Unspecified transition state code in naadsmDiseaseStateStr()' );
            result := tr( 'Unspecified' );
          end
        ;
        else
          begin
            raise exception.create( 'Unrecognized transition state code (' + intToStr( ord(val) ) + ') in naadsmDiseaseStateStr()' );
            result := tr( 'Unspecified' );
          end
        ;
      end;
    end
  ;


  function naadsmDiseaseStateXml( status: TNAADSMDiseaseState ): string;
  	begin
    	case status of
        NAADSMStateSusceptible: result := 'Susceptible';
        NAADSMStateLatent: result := 'Latent';
        NAADSMStateSubclinical: result := 'Infectious subclinical';
        NAADSMStateClinical: result := 'Infectious clinical';
        NAADSMStateNaturallyImmune: result := 'Naturally immune';
        NAADSMStateVaccineImmune: result := 'Vaccine immune';
        NAADSMStateDestroyed: result := 'Destroyed';
        NAADSMStateDeadFromDisease: result := 'Dead from disease';
        else
          begin
            raise exception.create( 'Unrecognized transition state code (' + intToStr( ord(status) ) + ') in naadsmDiseaseStateXml' );
            result := 'Unspecified';
          end
        ;
      end;
    end
  ;


  function naadsmDiseaseStateCode( status: TNAADSMDiseaseState ): char;
  	begin
    	case status of
        NAADSMStateSusceptible: result := 'S';
        NAADSMStateLatent: result := 'L';
        NAADSMStateSubclinical: result := 'B';
        NAADSMStateClinical: result := 'C';
        NAADSMStateNaturallyImmune: result := 'N';
        NAADSMStateVaccineImmune: result := 'V';
        NAADSMStateDestroyed: result := 'D';
        NAADSMStateDeadFromDisease: result := 'X';
        else
          begin
            raise exception.create( 'Unrecognized transition state code (' + intToStr( ord(status) ) + ') in naadsmDiseaseStateCode' );
            result := 'U';
          end
        ;
      end;
    end
  ;


  function naadsmDiseaseStateColor( s: TNAADSMDiseaseState ): TColor;
    begin
      case s of
        NAADSMStateSusceptible: 		result := clBlack;
        NAADSMStateLatent: 				result := clYellow;
        NAADSMStateSubclinical:	  result := clFuchsia;
        NAADSMStateClinical: 			result := clRed;
        NAADSMStateNaturallyImmune:  result := clLime;
        NAADSMStateVaccineImmune:  result := $00FF0000; // CUSTOM COLOR: Bright blue
        NAADSMStateDestroyed: 			result := clWhite;
        NAADSMStateDeadFromDisease: result := clGreen;
        else
        	raise exception.Create( 'Unrecognized transition state (' + intToStr(ord(s)) + ') in naadsmDiseaseStateColor()' )
        ;
      end;
    end
  ;


  // WARNING: statusCode must already be trimmed and upper case.
  // (This will save multiple thousands of function calls.)
  function naadsmDiseaseStateFromCode( statusCode: char ): TNAADSMDiseaseState;
  	begin
    	if( 'S' = statusCode ) then result := NAADSMStateSusceptible
      else if( 'L' = statusCode ) then result := NAADSMStateLatent
      else if( 'B' = statusCode ) then result := NAADSMStateSubclinical
      else if( 'C' = statusCode ) then result := NAADSMStateClinical
      else if( 'N' = statusCode ) then result := NAADSMStateNaturallyImmune
      else if( 'V' = statusCode ) then result := NAADSMStateVaccineImmune
      else if( 'D' = statusCode ) then result := NAADSMStateDestroyed
      else if( 'X' = statusCode ) then result := NAADSMStateDeadFromDisease
      else
        begin
          raise exception.Create( 'Unrecognized statusCode (' + statusCode + ') in naadsmDiseaseStateFromCode' );
      	  result := NAADSMStateUnspecified;
        end
      ;
    end
  ;


  function naadsmDiseaseStateFromString( statusStr: string ): TNAADSMDiseaseState;
  	begin
      statusStr := fixup( statusStr );

      // These are the "preferred" strings, because they're guaranteed to be translated:
    	if( ansiLowerCase( tr( 'Susceptible' ) ) = statusStr ) then result := NAADSMStateSusceptible
      else if( ansiLowerCase( tr( 'Latent' )  ) = statusStr ) then result := NAADSMStateLatent
      else if( ansiLowerCase( tr( 'Subclinical' ) ) = statusStr ) then result := NAADSMStateSubclinical
      else if( ansiLowerCase( tr( 'Clinical' ) ) = statusStr ) then result := NAADSMStateClinical
      else if( ansiLowerCase( tr( 'Natural immune' ) ) = statusStr ) then result := NAADSMStateNaturallyImmune
      else if( ansiLowerCase( tr( 'Vaccine immune' ) ) = statusStr ) then result := NAADSMStateVaccineImmune
      else if( ansiLowerCase( tr( 'Destroyed' ) ) = statusStr ) then result := NAADSMStateDestroyed
      else if( ansiLowerCase( tr( 'Dead from disease' ) ) = statusStr ) then result := NAADSMStateDeadFromDisease

      // These strings are occasionally used in the interface:
      else if( ansiLowerCase( tr( 'Naturally immune' ) ) = statusStr ) then result := NAADSMStateNaturallyImmune
      else if( ansiLowerCase( tr( 'Nat Immune' ) ) = statusStr ) then result := NAADSMStateNaturallyImmune
      else if( ansiLowerCase( tr( 'Vac Immune' ) ) = statusStr ) then result := NAADSMStateVaccineImmune

      // These strings are "preferred" for NAADSM XML, and will always be in English:
    	else if( 'susceptible' = statusStr ) then result := NAADSMStateSusceptible
      else if( 'latent' = statusStr ) then result := NAADSMStateLatent
      else if( 'subclinical' = statusStr ) then result := NAADSMStateSubclinical
      else if( 'clinical' = statusStr ) then result := NAADSMStateClinical
      else if( 'natural immune' = statusStr ) then result := NAADSMStateNaturallyImmune
      else if( 'vaccine immune' = statusStr ) then result := NAADSMStateVaccineImmune
      else if( 'destroyed' = statusStr ) then result := NAADSMStateDestroyed
      else if( 'dead from disease' = statusStr ) then result := NAADSMStateDeadFromDisease

      // These strings may also appear in NAADSM XML, and will always be in English:
      else if( 'incubating' = statusStr ) then result := NAADSMStateLatent
      else if( 'infectious subclinical' = statusStr ) then result := NAADSMStateSubclinical
      else if( 'infectioussubclinical' = statusStr ) then result := NAADSMStateSubclinical
      else if( 'infectious clinical' = statusStr ) then result := NAADSMStateClinical
      else if( 'infectiousclinical' = statusStr ) then result := NAADSMStateClinical
      else if( 'naturally immune' = statusStr ) then result := NAADSMStateNaturallyImmune
      else if( 'naturally immune' = statusStr ) then result := NAADSMStateNaturallyImmune
      else if( 'vaccineimmune' = statusStr ) then result := NAADSMStateVaccineImmune
      else
        begin
          raise exception.Create( 'Unrecognized statusStr (' + statusStr + ') in naadsmDiseaseStateFromString' );
      	  result := NAADSMStateUnspecified;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Test result conversion functions
//-----------------------------------------------------------------------------
  function naadsmTestResultStr( const val: TNAADSMTestResult ): string;
    begin
      case val of
        NAADSMTestTruePositive: result := tr( 'True positive' );
        NAADSMTestTrueNegative: result := tr( 'True negative' );
        NAADSMTestFalsePositive: result := tr( 'False positive' );
        NAADSMTestFalseNegative: result := tr( 'False negative' );
        else
          begin
            raise exception.create( 'Unrecoginized value (' + intToStr( cardinal( val ) ) + ' in naadsmSuccessStr()' );
            result := '';
          end
        ;
      end;
    end
  ;


  function naadsmTestResultFromCode( const val: char ): TNAADSMTestResult;
    begin
      if( 'P' = val ) then
        result := NAADSMTestTruePositive
      else if( 'Q' = val ) then
        result := NAADSMTestFalsePositive
      else if( 'N' = val ) then
        result := NAADSMTestTrueNegative
      else if( 'O' = val ) then
        result := NAADSMTestFalseNegative
      else
        begin
          raise exception.Create( 'Unrecognized code (' + val + ') in naadsmTestResultFromCode()' );
          result := NAADSMTestUnspecified;
        end
      ;
    end
  ;


  function naadsmTestResultCode( const val: TNAADSMTestResult ): char;
    begin
      case val of
        NAADSMTestTruePositive: result := 'P';
        NAADSMTestTrueNegative: result := 'N';
        NAADSMTestFalsePositive: result := 'Q';
        NAADSMTestFalseNegative: result := 'O';
      else
        begin
          raise exception.create( 'Unrecognized test result (' + intToStr( ord( val ) ) + ') in naadsmTestResultCode()' );
          result := 'U';
        end
      ;
      end;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Debugging functions
//-----------------------------------------------------------------------------
  procedure debugHRDExpose( const e: THRDExpose );
    begin
      dbcout( endl, true );
      dbcout( '--HRDExpose:', true );
      dbcout( 'srcIndex: ' + intToStr( e.srcIndex ), true );
      dbcout( 'srcStatus: ' + naadsmDiseaseStateStr( e.srcStatus ), true );
      dbcout( 'destIndex: ' + intToStr( e.destIndex ), true );
      dbcout( 'destStatus: ' + naadsmDiseaseStateStr( e.destStatus ), true );
      dbcout( 'day: ' + intToStr( e.day ), true );
      dbcout( 'isAdequate: ' + naadsmSuccessStr( e.isAdequate ), true );
      dbcout( 'exposureMethod: ' + naadsmContactStr( e.exposureMethod ), true );
      dbcout( '--Done HRDExpose', true );
      dbcout( endl, true );
    end
  ;

  procedure debugHRDControl( const c: THRDControl );
    begin
      dbcout( endl, true );
      dbcout( '--THRDControl:', true );
      dbcout( 'herdIndex: ' + intToStr( c.herdIndex ), true );
      dbcout( 'reason: ' + naadsmControlReasonStr( c.reason ), true );
      dbcout( 'day commitment made: ' + intToStr( c.dayCommitmentMade ), true );
      dbcout( '--Done THRDControl', true );
      dbcout( endl, true );
    end
  ;

//-----------------------------------------------------------------------------
end.
