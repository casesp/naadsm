unit StatusEnums;

(*
StatusEnums.pas
----------------
Begin: 2005/06/20
Last revision: $Date: 2008/04/18 20:35:19 $ $Author: areeves $
Version number: $Revision: 1.24 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2005 - 2008 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

{$INCLUDE Defs.inc}

interface

  const
    EVT_TRANSITION_STATE_CHANGE = 'R';
    EVT_TRACED_DIRECT = 'T';
    EVT_TRACED_INDIRECT = 'I';
    EVT_DESTROYED = 'D';
    EVT_VACCINATED = 'V';
    EVT_INFECTED = 'F';
    EVT_DETECTED = 'E';
    EVT_ZONE_FOCUS = 'Z';
    EVT_ZONE_CHANGED = 'C';

	{*
  	Mostly compatible with SHARCSpread HRD_status_t
  }
  type TTransitionState = (
  	tsSusceptible = 0,
    tsLatent = 1,
    tsSubClinical = 2,
    tsClinical = 3,
    tsNaturalImmune = 4,
    tsVaccineImmune = 5,
    tsDestroyed = 6,
    tsDummy = 254,
    tsUnspecified = 255  // Not in HRD_status_t
  );


  type TApparentStatus = (
  	asUnspecified = 0,
    asUnknown = ord('U'), // Note that 'unknown' has a different meaning than 'unspecified'
    asDetected = ord('E'),
    asTracedDirect = ord('T'),
    asTracedIndirect = ord('I'),
    asVaccinated = ord('V'),
    asDestroyed = ord('D')
  );


  function apparentStatusFromCode( statusCode: char ): TApparentStatus;
  function transitionStateFromCode( statusCode: string ): TTransitionState;
  function transitionStateFromString( statusStr: string ): TTransitionState;

  function transitionStateCode( status: TTransitionState ): string;
  function apparentStatusCode( status: TApparentStatus ): char;
  function xmlTransitionStateString( status: TTransitionState ): string;
  function transitionStateString( status: TTransitionState ): string;
  function apparentStatusString( status: TApparentStatus ): string;

implementation

	uses
  	SysUtils,
    MyStrUtils,
    USStrUtils,
    I88n
  ;

  // WARNING: statusCode must already be trimmed and upper case.
  // (This will save multiple thousands of function calls.)
  function transitionStateFromCode( statusCode: string ): TTransitionState;
  	begin
    	if( 'S' = statusCode ) then result := tsSusceptible
      else if( 'L' = statusCode ) then result := tsLatent
      else if( 'B' = statusCode ) then result := tsSubclinical
      else if( 'C' = statusCode ) then result := tsClinical
      else if( 'N' = statusCode ) then result := tsNaturalImmune
      else if( 'V' = statusCode ) then result := tsVaccineImmune
      else if( 'D' = statusCode ) then result := tsDestroyed
      else
        begin
          raise exception.Create( 'Unrecognized statusCode (' + statusCode + ') in transitionStateFromCode' );
      	  result := tsUnspecified;
        end
      ;
    end
  ;


  function transitionStateFromString( statusStr: string ): TTransitionState;
  	begin
      statusStr := fixup( statusStr );

      // These are the "preferred" strings, because they're guaranteed to be translated:
    	if( ansiLowerCase( tr( 'Susceptible' ) ) = statusStr ) then result := tsSusceptible
      else if( ansiLowerCase( tr( 'Latent' )  ) = statusStr ) then result := tsLatent
      else if( ansiLowerCase( tr( 'Subclinical' ) ) = statusStr ) then result := tsSubclinical
      else if( ansiLowerCase( tr( 'Clinical' ) ) = statusStr ) then result := tsClinical
      else if( ansiLowerCase( tr( 'Natural immune' ) ) = statusStr ) then result := tsNaturalImmune
      else if( ansiLowerCase( tr( 'Vaccine immune' ) ) = statusStr ) then result := tsVaccineImmune
      else if( ansiLowerCase( tr( 'Destroyed' ) ) = statusStr ) then result := tsDestroyed

      // These strings are occasionally used in the interface:
      else if( ansiLowerCase( tr( 'Naturally immune' ) ) = statusStr ) then result := tsNaturalImmune
      else if( ansiLowerCase( tr( 'Nat Immune' ) ) = statusStr ) then result := tsNaturalImmune
      else if( ansiLowerCase( tr( 'Vac Immune' ) ) = statusStr ) then result := tsVaccineImmune

      // These strings are "preferred" for NAADSM XML, and will always be in English:
    	else if( 'susceptible' = statusStr ) then result := tsSusceptible
      else if( 'latent' = statusStr ) then result := tsLatent
      else if( 'subclinical' = statusStr ) then result := tsSubclinical
      else if( 'clinical' = statusStr ) then result := tsClinical
      else if( 'natural immune' = statusStr ) then result := tsNaturalImmune
      else if( 'vaccine immune' = statusStr ) then result := tsVaccineImmune
      else if( 'destroyed' = statusStr ) then result := tsDestroyed

      // These strings may also appear in NAADSM XML:
      else if( 'incubating' = statusStr ) then result := tsLatent
      else if( 'infectious subclinical' = statusStr ) then result := tsSubClinical
      else if( 'infectioussubclinical' = statusStr ) then result := tsSubClinical
      else if( 'naturally immune' = statusStr ) then result := tsNaturalImmune
      else if( 'naturally immune' = statusStr ) then result := tsNaturalImmune
      else if( 'vaccineimmune' = statusStr ) then result := tsVaccineImmune
      else
        begin
          raise exception.Create( 'Unrecognized statusStr (' + statusStr + ') in transitionStateFromString' );
      	  result := tsUnspecified;
        end
      ;
    end
  ;

  
  // WARNING: statusCode must already be trimmed.
  // This will save multiple thousands of function calls.
  function apparentStatusFromCode( statusCode: char ): TApparentStatus;
  	begin
      case statusCode of
        'U', 'E', 'T', 'I', 'V', 'D': result := TApparentStatus( ord( statusCode ) );
        else
          begin
            raise exception.create( 'Unrecognized apparent status code (' + statusCode + ') in apparentStatusFromCode' );
            result := asUnspecified;
          end
        ;
      end;
    end
  ;


  function transitionStateCode( status: TTransitionState ): string;
  	begin
    	case status of
        tsSusceptible: result := 'S';
        tsLatent: result := 'L';
        tsSubClinical: result := 'B';
        tsClinical: result := 'C';
        tsNaturalImmune: result := 'N';
        tsVaccineImmune: result := 'V';
        tsDestroyed: result := 'D';
        else
          begin
            raise exception.create( 'Unrecognized transition state code (' + intToStr( ord(status) ) + ') in transitionStateCode' );
            result := 'U';
          end
        ;
      end;
    end
  ;


  function xmlTransitionStateString( status: TTransitionState ): string;
  	begin
    	case status of
        tsSusceptible: result := 'Susceptible';
        tsLatent: result := 'Latent';
        tsSubClinical: result := 'Infectious subclinical';
        tsClinical: result := 'Infectious clinical';
        tsNaturalImmune: result := 'Naturally immune';
        tsVaccineImmune: result := 'Vaccine immune';
        tsDestroyed: result := 'Destroyed';
        else
          begin
            raise exception.create( 'Unrecognized transition state code (' + intToStr( ord(status) ) + ') in xmlTransitionStateString' );
            result := 'Unspecified';
          end
        ;
      end;
    end
  ;



  function transitionStateString( status: TTransitionState ): string;
  	begin
    	case status of
        tsSusceptible: result := tr( 'Susceptible' );
        tsLatent: result := tr( 'Latent' );
        tsSubClinical: result := tr( 'Subclinical' );
        tsClinical: result := tr( 'Clinical' );
        tsNaturalImmune: result := tr( 'Naturally immune' );
        tsVaccineImmune: result := tr( 'Vaccine immune' );
        tsDestroyed: result := tr( 'Destroyed' );
        else
          begin
            raise exception.create( 'Unrecognized transition state code (' + intToStr( ord(status) ) + ') in transitionStateCode' );
            result := tr( 'Unspecified' );
          end
        ;
      end;
    end
  ;


  function apparentStatusCode( status: TApparentStatus ): char;
  	begin
      case status of
        asUnknown, asDetected, asTracedDirect, asTracedIndirect, asVaccinated, asDestroyed:
          result := chr( ord( status ) );
        else
          begin
            raise exception.create( 'Unrecognized apparent status code (' + intToStr( ord(status) ) + ') in apparentStatusCode' );
            result := char(0);
          end
        ;
      end;
    end
  ;


  function apparentStatusString( status: TApparentStatus ): string;
    begin
      case status of
        asUnknown: result := tr( 'Unknown' );
        asDetected: result := tr( 'Detected' );
        asTracedDirect: result := tr( 'Traced, direct' );
        asTracedIndirect: result := tr( 'Traced, indirect' );
        asVaccinated: result := tr( 'Vaccinated' );
        asDestroyed: result := tr( 'Destroyed' );
        else result := 'UNSPECIFIED: THIS IS A PROBLEM.';
      end;
    end
  ;

end.
