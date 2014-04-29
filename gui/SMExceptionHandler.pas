unit SMExceptionHandler;

(*
SMExceptionHandler.pas
----------------------
Begin: 2009/05/22
Last revision: $Date: 2011-07-07 16:37:55 $ $Author: areeves $
Version: $Revision: 1.5.10.1 $
Project: APHI General Purpose Delphi Library
Website: http://www.naadsm.org/opensource/delphi
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2009 - 2011 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

{$INCLUDE Defs.inc}

interface

  // NOTE: EurekaLog is NOT open-source software.
  // The distribution of a version of NAADSM that has been compiled
  // with EurekaLog would violate the terms of the GPL.  Code is provided
  // for users who wish to build the NAADSM user interface with EurekaLog for
  // their own use, but not for distribution.
  //
  // The use of the function buildErrorZip does not require EurekaLog, and is
  // completely compatible with the GPL.

	function buildErrorZip( scenarioFileName, herdFileName, logFileName: string ): string;

  var
    eurekaLogPath: string;

implementation

  uses
    Classes,
    SysUtils,
    StrUtils,

    {$IFDEF EUREKALOG}
      ExceptionLog,
      ETypes,
      ECore,
    {$ENDIF}

    ZipMstr,
    
    DebugWindow,
    ZipFunctions,
    MyStrUtils,
    WindowsUtils,

    NAADSMLibrary
  ;


  // These two types are used to rename files contained in zip archives once created.
  // The interface for the zip library is way ugly, but what can you do?
  type ZipRenameRec = record
      Source:	String;	// The Source filename/path as present in the ziparchive.
      Dest:	String;	// The Destination filename/path, this will be the new name.
      DateTime:	Integer;	// A new Date/Time for this specific destination or 0.
    end
  ;

  type pZipRenameRec = ^ZipRenameRec;


  function buildErrorZip( scenarioFileName, herdFileName, logFileName: string ): string;
    var
      zipFileName: string;
      i: integer;
      zipper: TZipMaster;
      zipRenameList:	TList;
      renRec, renRec2, renRec3:	pZipRenameRec;
    begin
      // Come up with a unique file name
      //---------------------------------
      i := 1;
      repeat
        zipFileName := 'NAADSM_Errors' + intToStr( i ) + '.log';
        inc( i );
      until not( fileExists( zipFileName ) );

      // Create the zip archive
      //-----------------------
      zipper := TZipMaster.Create( nil );

      zipper.ZipFileName := zipFileName;
      zipper.FSpecArgs.Add( scenarioFileName );
      zipper.FSpecArgs.Add( herdFileName );
      if( 0 < length( logFileName ) ) then
      	zipper.FSpecArgs.Add( logFileName )
      ;

      zipper.Add();

      // Rename the contents of the zip file to something meaningful
      //------------------------------------------------------------
      zipRenameList	:= TList.Create();

      New( renRec );
      renRec^.Source	:= shortFileName( scenarioFileName );
      renRec^.Dest	:= 'scenario.xml';
      renRec^.DateTime := 0;
      ZipRenameList.Add( renRec );

      New( renRec2 );
      renRec2^.Source	:= shortFileName( herdFileName );
      renRec2^.Dest	:= 'herds.xml';
      renRec2^.DateTime := 0;
      ZipRenameList.Add( renRec2 );

      New( renRec3 );
      if( 0 < length( logFileName ) ) then
      	begin
          renRec3^.Source	:= shortFileName( logFileName );
          renRec3^.Dest	:= 'exception.txt';
          renRec3^.DateTime := 0;
          ZipRenameList.Add( renRec3 );
        end
      ;

      zipper.Rename( zipRenameList, 0 );

      // Clean up
      //---------
      freeAndNil( zipper );
      dispose( renRec );
      dispose( renRec2 );
      dispose( renRec3 );

      zipRenameList.Free();

      result := zipFileName;
    end
  ;


  {$IFDEF EUREKALOG}
    procedure MyNotify( ExcRecord: TEurekaExceptionRecord; var Handled: Boolean );
      begin
        {$IFDEF DEBUG}
          ExcRecord.CurrentModuleOptions.EMailSendMode := esmNoSend; 
        {$ENDIF}

        {$IFDEF CONSOLEAPP}
          ExcRecord.CurrentModuleOptions.ExceptionDialogType := edtNone;
        {$ENDIF}
      end
    ;


    procedure MyAttachedFiles( EurekaExceptionRecord: TEurekaExceptionRecord; AttachedFiles: TStrings );
      begin
        // Clear all EurekaLog attached files (as screenshot, last HTML page,...)
        //AttachedFiles.Clear();

        if
          ( 0 < length( simFileName ) )
        and
          ( fileExists( simFileName ) )
        then
          AttachedFiles.Add( simFileName )
        ;

        if
          ( 0 < length( herdFileName ) )
        and
          ( fileExists( herdFileName ) )
        then
          AttachedFiles.Add( herdFileName )
        ;

        if
          ( 0 < length( logFileName ) )
        and
          ( fileExists( logFileName ) )
        then
          AttachedFiles.Add( logFileName )
        ;

      end
    ;
  {$ENDIF}


//-------------------------------------------
// Currently unused: keep for code reference
//-------------------------------------------
  (*
  function dumpHerdStates( var hsFile: TextFile; var DsU: File ): string;
    const
      MAXLENGTH = 43; // length( '<td width="50%">InfectiousSubclinical</td> ' )
    var
      recBuf : array[0..1999] of DataSetRecType;
      nr: integer; // Number of records read from file
      u: integer; // loop counter
      str: string;
      i: integer;
      state: string;
      padding, pc: integer;
      pct: string;
      herdCounter: integer;
      daysLeft: string;
    begin
      Reset(DsU, Sizeof(DataSetRecType));

      padding := 0;
      str := '';
      herdCounter := 0;

      if( clReadableDailyStates ) then str := '<tr>';

      repeat // a block of records at a time until the end of the file
          BlockRead(DsU, recBuf, 2000, nr);

          for u := 0 to nr - 1 do // for each unit...
            begin

              herdCounter := herdCounter + 1;

              case recBuf[u].status of
                tsSusceptible: begin i := 0; state := 'Susceptible'; end;
                NAADSMStateLatent: begin i := 1; state := 'Latent'; end;
                NAADSMStateSubclinical: begin i := 2; state := 'Subclinical'; end;
                NAADSMStateClinical: begin i := 3; state := 'Clinical'; end;
                NAADSMStateNaturallyImmune: begin i := 4; state := 'NaturallyImmune'; end;
                NAADSMStateVaccineImmune: begin i := 5; state := 'VaccineImmune'; end;
                NAADSMStateDestroyed: begin i := 6; state := 'Destroyed'; end;
              else
                begin i := -1; state := 'UNDEFINED'; end;
              end;

              if( clReadableDailyStates ) then
                begin
                  daysLeft := ' (' + intToStr( recBuf[u].DaysInState ) + ', ' + intToStr( recBuf[u].DaysLeftInState ) + ')';
                  str := str + '<td width="xx%">' + state + daysLeft + '</td>';
                  //str := str + '<td width="xx%">' + state + '</td>';
                  padding := MAXLENGTH - length( str );
                  for pc := 0 to padding do str := str + ' ';
                end
              else
                str := str + intToStr( i ) + ' '
              ;
            
            end
          ;

      until nr = 0;		

      if( clReadableDailyStates ) then
        begin
          pct := intToStr( round( 100 / herdCounter ) ) + '%';
          str := ansiReplaceStr( str, 'xx%', pct );
          str := left( str, length( str ) - padding ) + '</tr>';
        end
      ;

      result := str;

    end
  ;
  *)
//-----------------------------------------


initialization
  {$IFDEF EUREKALOG}
    {$IFDEF DEBUG}
      //CurrentEurekaLogOptions.OutputPath := tempFileName( currentDir() );
      //CurrentEurekaLogOptions.OutputPath := ansiReplaceStr( CurrentEurekaLogOptions.OutputPath, '.tmp', '.elf' );
      CurrentEurekaLogOptions.OutputPath := currentDir() + 'log.elf';
    {$ELSE}
      CurrentEurekaLogOptions.OutputPath := currentDir() + 'naadsmErrorLog.elf';
    {$ENDIF}
    eurekaLogPath := CurrentEurekaLogOptions.OutputPath;

    AttachedFilesRequest := MyAttachedFiles;
    ExceptionNotify := MyNotify;
  {$ELSE}
    eurekaLogPath := '';
  {$ENDIF}

end.