unit StatusEnums;

(*
StatusEnums.pas
----------------
Begin: 2005/06/20
Last revision: $Date: 2013-06-27 19:11:36 $ $Author: areeves $
Version number: $Revision: 1.32.4.6 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <aaron.reeves@naadsm.org>
--------------------------------------------------
Copyright (C) 2005 - 2009 Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

{$INCLUDE Defs.inc}

interface

  uses
    Graphics,

    NAADSMLibraryTypes
  ;

  type TColorArray = array of TColor;

  type TControlStatus = (
  	asUnspecified,
    asNoControl,          // 'U'
    asDetected,           // 'E'
    asTracedDirectFwd,    // 'T'
    asTracedIndirectFwd,  // 'I'
    asTracedDirectBack,   // 'K'
    asTracedIndirectBack, // 'J'
    asVaccinated,         // 'V'
    asDestroyed,          // 'D'
    asInDestructionQueue  // 'Q'
  );


  // Order matters for this type!
  type TDetectionStatus = (
    dsUnspecified,
    dsNoStatus,           // 'U': not infected, with no detection activity
    dsInfectedUndetected, // 'I': Undetected latent, subclinical, or clinical units
    dsExamined,           // 'M': Units (possibly but not necessarily infected) subjected to a herd exam, but not (yet) detected.
    dsTestTrueNeg,        // 'N'
    dsTestFalseNeg,       // 'O'
    dsTestFalsePos,       // 'Q'
    dsTestTruePos,        // 'P':
    dsDetectedClinical,   // 'E': Units detected on the basis of clinical signs (including by herd exam).  In NAADSM 3.x, these are always true positives.
    dsDestroyed           // 'D'
  );


  type TEventStatus = (
    esUnspecified,
    esFirstDetection,     // 'E'
    esFirstDestruction,   // 'D'
    esFirstVaccination,   // 'V'
    esOutbreakOver        // 'O'
  );

  
  function controlStatusFromCode( statusCode: char ): TControlStatus;
  function controlStatusCode( status: TControlStatus ): char;
  function controlStatusString( status: TControlStatus ): string;
  function controlStatusColor( s: TControlStatus ): TColor;

  function detectionStatusFromCode( code: char ): TDetectionStatus;
  function detectionStatusCode( status: TDetectionStatus ): char;
  function detectionStatusString( status: TDetectionStatus ): string;
  function detectionStatusColor( s: TDetectionStatus ): TColor;

  function eventStatusFromCode( eventCode: char ): TEventStatus;
  function eventStatusCode( status: TEventStatus ): char;
  function eventStatusString( status: TEventStatus ): string;
  function eventStatusColor( s: TEventStatus ): TColor;

  function zoneColor( const zoneLevel: integer ): TColor;

implementation

	uses
  	SysUtils,
    MyStrUtils,
    I88n
  ;

  var
    _perimeterColors: TColorArray;

//-----------------------------------------------------------------------------
// Control status helper functions
//-----------------------------------------------------------------------------
  function controlStatusFromCode( statusCode: char ): TControlStatus;
  	begin
      case statusCode of
        'U': result := asNoControl;
        'E': result := asDetected;
        'T': result := asTracedDirectFwd;
        'I': result := asTracedIndirectFwd;
        'K': result := asTracedDirectBack;
        'J': result := asTracedIndirectBack;
        'V': result := asVaccinated;
        'D': result := asDestroyed;
        'Q': result := asInDestructionQueue;
        else
          begin
            raise exception.create( 'Unrecognized control status code (' + statusCode + ') in controlStatusFromCode()' );
            result := asUnspecified;
          end
        ;
      end;
    end
  ;


  function controlStatusCode( status: TControlStatus ): char;
  	begin
      case status of
        asNoControl: result := 'U';
        asDetected: result := 'E';
        asTracedDirectFwd: result := 'T';
        asTracedIndirectFwd: result := 'I';
        asTracedDirectBack: result := 'K';
        asTracedIndirectBack: result := 'J';
        asVaccinated: result := 'V';
        asDestroyed: result := 'D';
        asInDestructionQueue: result := 'Q';
        else
          begin
            raise exception.create( 'Unrecognized control status code (' + intToStr( ord(status) ) + ') in controlStatusCode()' );
            result := char(0);
          end
        ;
      end;
    end
  ;


  function controlStatusString( status: TControlStatus ): string;
    begin
      case status of
        asNoControl: result := tr( 'No control' );
        asDetected: result := tr( 'Detected' );
        asTracedDirectFwd: result := tr( 'Trace forward of direct contact' );
        asTracedIndirectFwd: result := tr( 'Trace forward of indirect contact' );
        asTracedDirectBack: result := tr( 'Trace back of direct contact' );
        asTracedIndirectBack: result := tr( 'Trace back of indirect contact' );
        asVaccinated: result := tr( 'Vaccinated' );
        asDestroyed: result := tr( 'Destroyed' );
        asInDestructionQueue: result := tr( 'In destruction queue' );
        else
          begin
            raise exception.create( 'Unrecognized control status code (' + intToStr( ord(status) ) + ') in controlStatusString()' );
            result := 'Unspecified';
          end
        ;
      end;
    end
  ;


  function controlStatusColor( s: TControlStatus ): TColor;
  	begin
   		case s of
        asNoControl: result := clBlack;
        asDetected: result := clGreen;
        asTracedDirectFwd: result := clNavy;
        asTracedDirectBack: result := clSkyBlue;
        asTracedIndirectFwd: result := clPurple;
        asTracedIndirectBack: result := $00C864C8; // CUSTOM COLOR: Light purple
        asInDestructionQueue: result := $00A6FFFF; // CUSTOM COLOR: Light yellow
        asVaccinated: result := $00FF0000; // CUSTOM COLOR: Bright blue
        asDestroyed: result := clWhite;
        else
        	raise exception.Create( 'Unrecognized control status (' + intToStr(ord(s)) + ') in controlStatusColor()' )
        ;
      end;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Detection status helper functions
//-----------------------------------------------------------------------------
  function detectionStatusFromCode( code: char ): TDetectionStatus;
    begin
      case code of
        'U': result := dsNoStatus;
        'I': result := dsInfectedUndetected;
        'E': result := dsDetectedClinical;
        'M': result := dsExamined;
        'P': result := dsTestTruePos;
        'Q': result := dsTestFalsePos;
        'N': result := dsTestTrueNeg;
        'O': result := dsTestFalseNeg;
        'D': result := dsDestroyed;
        else
          begin
            raise exception.create( 'Unrecognized code (' + code + ') in detectionStatusFromCode()' );
            result := dsUnspecified;
          end
        ;
      end;
    end
  ;


  function detectionStatusCode( status: TDetectionStatus ): char;
    begin
      case status of
        dsNoStatus: result := 'U';
        dsInfectedUndetected: result := 'I';
        dsDetectedClinical: result := 'E';
        dsExamined: result := 'M';
        dsTestTruePos: result := 'P';
        dsTestFalsePos: result := 'Q';
        dsTestTrueNeg: result := 'N';
        dsTestFalseNeg: result := 'O';
        dsDestroyed: result := 'D';
        else
          begin
            raise exception.create( 'Unrecognized value (' + intToStr( ord(status) ) + ') in detectionStatusCode()' );
            result := char(0);
          end
        ;
      end;
    end
  ;


  function detectionStatusString( status: TDetectionStatus ): string;
    begin
      case status of
        dsNoStatus: result := tr( 'Uninfected, no disease control activity' );
        dsInfectedUndetected: result := tr( 'Infected but undetected' );
        dsDetectedClinical: result := tr( 'Detected by clinical signs' );
        dsExamined: result := tr( 'Examined' );
        dsTestTruePos: result := tr( 'Test true positive' );
        dsTestFalsePos: result := tr( 'Test false positive' );
        dsTestTrueNeg: result := tr( 'Test true negative' );
        dsTestFalseNeg: result := tr( 'Test false negative' );
        dsDestroyed: result := tr( 'Destroyed' );
        else
          begin
            raise exception.create( 'Unrecognized value (' + intToStr( ord(status) ) + ') in detectionStatusString()' );
            result := 'Unspecified';
          end
        ;
      end;
    end
  ;


  function detectionStatusColor( s: TDetectionStatus ): TColor;
    begin
      case s of
        dsNoStatus: result := clBlack;
        dsInfectedUndetected: result := clRed;
        dsExamined: result :=  $00CC49A2; // CUSTOM COLOR: Light purple
        dsTestTrueNeg: result := clSkyBlue;
        dsTestFalseNeg: result := $00FF64FF; // CUSTOM COLOR: Light fuchsia
        dsTestFalsePos: result := clYellow;
        dsTestTruePos: result := $0000A8FF; // CUSTOM COLOR: Orange
        dsDetectedClinical: result := clGreen;
        dsDestroyed: result := clWhite;
        else
          begin
            raise exception.create( 'Unrecognized value (' + intToStr( ord(s) ) + ') in detectionStatusColor()' );
            result := clLime;
          end
        ;
      end;
    end
  ;
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// Event status helper functions
//-----------------------------------------------------------------------------

  function eventStatusFromCode( eventCode: char ): TEventStatus;
    begin
      case eventCode of
        'E': result := esFirstDetection;
        'D': result := esFirstDestruction;
        'V': result := esFirstVaccination;
        'O': result := esOutbreakOver;
        else
          begin
            raise exception.create( 'Unrecognized code (' + eventCode + ') in eventStatusFromCode()' );
            result := esUnspecified;
          end;
        ;
      end;
    end
  ;

  function eventStatusCode( status: TEventStatus ): char;
    begin
      case status of
        esFirstDetection: result := 'E';
        esFirstDestruction: result := 'D';
        esFirstVaccination: result := 'V';
        esOutbreakOver: result := 'O';
        else
          begin
            raise exception.create( 'Unrecognized value (' + intToStr( ord(status) ) + ') in eventStatusCode()' );
            result := char(0);
          end
        ;
      end;
    end
  ;

  function eventStatusString( status: TEventStatus ): string;
    begin
      case status of
        esFirstDetection: result := tr( 'First Detection' );
        esFirstDestruction: result := tr( 'First Destruction' );
        esFirstVaccination: result := tr( 'First Vaccination' );
        esOutbreakOver: result := tr( 'Outbreak Over' );
        else
          begin
            raise exception.create( 'Unrecognized value (' + intToStr( ord(status) ) + ') in eventStatusString()' );
            result := 'Unspecified';
          end
        ;
      end;
    end
  ;

  function eventStatusColor( s: TEventStatus ): TColor;
    begin
      case s of
        esFirstDetection: result :=    controlStatusColor(asDetected);
        esFirstDestruction: result :=  controlStatusColor(asDestroyed);
        esFirstVaccination: result :=  controlStatusColor(asVaccinated);
        esOutbreakOver: result :=      clRed;
        else
          begin
            raise exception.create( 'Unrecognized value (' + intToStr( ord(s) ) + ') in eventStatusColor()' );
            result := clLime;
          end
        ;
      end;
    end
  ;

//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Zone colors
//-----------------------------------------------------------------------------
  function zoneColor( const zoneLevel: integer ): TColor;
    begin
      result := _perimeterColors[ ( zoneLevel - 1 ) mod length( _perimeterColors ) ]
    end
  ;
//-----------------------------------------------------------------------------


initialization
  setLength( _perimeterColors, 4 );
  _perimeterColors[0] := clMaroon;
  _perimeterColors[1] := clTeal;
  _perimeterColors[2] := clNavy;
  _perimeterColors[3] := clOlive;


finalization
  setLength( _perimeterColors, 0 );

end.
