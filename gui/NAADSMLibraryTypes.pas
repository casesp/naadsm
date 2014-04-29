unit NAADSMLibraryTypes;

(*
NAADSMLibraryTypes.pas
----------------------
Begin: 2009/07/17
Last revision: $Date: 2011-05-17 22:27:45 $ $Author: areeves $
Version: $Revision: 1.9.4.5 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2009 - 2011 Animal Population Health Institute, Colorado State University

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
    NAADSMInitiallyInfected
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
    NAADSMDetectionDiagnosticTest
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
    NAADSMControlInitialState
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
      infectionSourceType: TNAADSMContactType;
    end
  ;

  {* Record type used in combination with the DLL when a detection occurs. }
  type THRDDetect = record
      herdIndex: integer;
      reason: TNAADSMDetectionReason;
      testResult: TNAADSMTestResult;
    end
  ;

  {* A class needed for detection events in NAADSM 3.  See comments associated with NAADSMLibrary.naadsm_detect_herd.
     This class will not exist in NAADSM 4! }
  type TNAADSM3Detect = class
    public
      herdIndex: integer;
      reason: TNAADSMDetectionReason;
      testResult: TNAADSMTestResult;

      constructor create( d: THRDDetect );
      destructor destroy(); override;
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
      initiatedDay: integer;
      finalizedDay: integer;
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
  function naadsmContactStr( const val: TNAADSMContactType ): string;
  function naadsmDetectionReasonStr( const val: TNAADSMDetectionReason ): string;
  function naadsmControlActivityStr( const val: TNAADSMControlReason ): string;

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
  procedure debugHRDTrace( const t: THRDTrace );

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


  function naadsmContactStr( const val: TNAADSMContactType ): string;
    begin
      case val of
        NAADSMDirectContact: result := 'DIRECT CONTACT';
        NAADSMIndirectContact: result := 'INDIRECT CONTACT';
        NAADSMAirborneSpread: result := 'AIRBORNE';
        NAADSMInitiallyInfected: result := 'INITIALLY INFECTED';
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
        else
          begin
            raise exception.create( 'Unrecoginized value (' + intToStr( cardinal( val ) ) + ' in naadsmSuccessStr()' );
            result := '';
          end
        ;
      end;
    end
  ;


  function naadsmControlActivityStr( const val: TNAADSMControlReason ): string;
    begin
      case val of
        NAADSMControlRing: result := 'CONTROL RING';
        NAADSMControlTraceForwardDirect: result := 'CONTROL TRACE FORWARD DIRECT';
        NAADSMControlTraceForwardIndirect: result := 'CONTROL TRACE FORWARD INDIRECT';
        NAADSMControlTraceBackDirect: result := 'CONTROL TRACE BACK DIRECT';
        NAADSMControlTraceBackIndirect: result := 'CONTROL TRACE BACK INDIRECT';
        NAADSMControlDetection: result := 'CONTROL DETECTION';
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
      result := NAADSMStateDestroyed;
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
      dbcout( 'initiatedDay: ' + intToStr( e.initiatedDay ), true );
      dbcout( 'finalizedDay: ' + intToStr( e.finalizedDay ), true );
      dbcout( 'isAdequate: ' + naadsmSuccessStr( e.isAdequate ), true );
      dbcout( 'exposureMethod: ' + naadsmContactStr( e.exposureMethod ), true );
      dbcout( '--Done HRDExpose', true );
      dbcout( endl, true );
    end
  ;


  procedure debugHRDTrace( const t: THRDTrace );
    begin
      dbcout( endl, true );
      dbcout( '--HRDTrace:', true );
      dbcout( 'day: ' + intToStr( t.day ), true );
      dbcout( 'initiatedDay: ' + intToStr( t.initiatedDay ), true );
      dbcout( 'identifiedIndex: ' + intToStr( t.identifiedIndex ), true );
      dbcout( 'identifiedStatus: ' + naadsmDiseaseStateStr( t.identifiedStatus ), true );
      dbcout( 'originIndex: ' + intToStr( t.originIndex ), true );
      dbcout( 'originStatus: ' + naadsmDiseaseStateStr( t.originStatus ), true );
      dbcout( 'success: ' + naadsmSuccessStr( t.success ), true );
      dbcout( 'traceType: ' + naadsmTraceDirectionStr( t.traceType ), true );
      dbcout( 'contactType: ' + naadsmContactStr( t.contactType ), true );
      dbcout( '--Done HRDTrace', true );
      dbcout( endl, true );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// NAADSM 3 detection
//-----------------------------------------------------------------------------
  {* A class needed for detection events in NAADSM 3.  See comments associated with NAADSMLibrary.naadsm_detect_herd.
     This class will not exist in NAADSM 4! }
  constructor TNAADSM3Detect.create( d: THRDDetect );
    begin
      herdIndex := d.herdIndex;
      reason := d.reason;
      testResult := d.testResult;
    end
  ;

  
  destructor TNAADSM3Detect.destroy();
    begin
      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------

end.
