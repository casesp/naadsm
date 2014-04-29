unit StringConsts;

(*
StringConsts.pas
----------------
Begin: 2005/06/08
Last revision: $Date: 2008/11/25 22:03:19 $ $Author: areeves $
Version: $Revision: 1.104 $
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

  uses
    SMDatabase
  ;


	// IMPORTANT!!
	// DON'T FORGET: when updating this unit, also update Application.Title in SpreadModel.dpr.
  // For reasons that I don't understand, Application.Title cannot be set
  // from an existing constant.
  //
  // ALSO DON'T FORGET: update project options before release
  // (Project -> Options... -> Version Info)
  //
  // ALSO DON'T FORGET: assign an update reason below.
  //
  // FINALLY DON'T FORGET: check the database schema information in SMDatabase.pas.
  // It may or may not need to be altered.
	const
    MAJORVERSIONNUMBER = '3.1';
    MINORVERSIONNUMBER = '19';

    {$IF Defined( CHEYENNE ) }
      MIN_COMPATIBLE_DLL_VERSION = '3.1.18-CHEYENNE'; // Used to determine if the right version of the core model is present
      BRANCHNAME = 'Cheyenne';
      BRANCHNAMEPARENS = ' (Cheyenne) ';
    {$ELSEIF Defined( LARAMIE ) }
      MIN_COMPATIBLE_DLL_VERSION = '3.1.18-LARAMIE'; // Used to determine if the right version of the core model is present
      BRANCHNAME = 'Laramie';
      BRANCHNAMEPARENS = ' (Laramie) ';
    {$ELSE}
      MIN_COMPATIBLE_DLL_VERSION = '3.1.18'; // Used to determine if the right version of the core model is present
      BRANCHNAME = '';
      BRANCHNAMEPARENS = ' ';
    {$IFEND}

    BUILDNUMBER = '081116';

    APPNAME = 'NAADSM';
    VERSIONNUMBER = MAJORVERSIONNUMBER + '.' + MINORVERSIONNUMBER + ' Build ' + BUILDNUMBER;

  	// Used as the caption for the main form.
    {$IFDEF DEBUG}
  	  MASTERCAPTION = APPNAME + BRANCHNAMEPARENS + MAJORVERSIONNUMBER + '.' + MINORVERSIONNUMBER + '-' + BUILDNUMBER;
    {$ELSE}
      MASTERCAPTION = APPNAME + BRANCHNAMEPARENS + MAJORVERSIONNUMBER;
    {$ENDIF} // DEBUG

    // Unused at the moment, but potentially useful
    {$IFDEF DEBUG}
      SHORTMASTERCAPTION = APPNAME + BRANCHNAMEPARENS + MAJORVERSIONNUMBER + '.' + MINORVERSIONNUMBER + '-' + BUILDNUMBER;
    {$ELSE}
      SHORTMASTERCAPTION = APPNAME + BRANCHNAMEPARENS + MAJORVERSIONNUMBER;
    {$ENDIF}

    // Used as a caption for some dialog boxes
    {$IFDEF DEBUG}
      SHORTERMASTERCAPTION = APPNAME + BRANCHNAMEPARENS + MAJORVERSIONNUMBER + '.' + MINORVERSIONNUMBER;
    {$ELSE}
      SHORTERMASTERCAPTION = SHORTMASTERCAPTION;
    {$ENDIF}

    // The application title that will appear on the app's taskbar button.
    APPTITLE = SHORTERMASTERCAPTION;

    WEBSITE = 'http://www.naadsm.org';

  type TVersionUpdateReason = (
    VERSOK, // Changes do not affect existing inputs or outputs (the database updater handles minor updates)
    VERSBug, // A bug was discovered in the previous version: outputs may be invalid
    VERSModelSpecChange, // The model specification has changed: inputs and/or outputs are invalid
      // (This shouldn't happen without at least a change in minor app version and input translation)
    VERSUnrecognized // Some other program mucked around with things.
  );


  function versionUpdateReason( db: TSMDatabase; versionID: pstring = nil ): TVersionUpdateReason;

implementation

  uses
    SqlClasses,
    MyStrUtils,
    USStrUtils,
    DebugWindow,

    SysUtils,
    Variants
  ;


  function versionUpdateReason( db: TSMDatabase; versionID: pstring = nil ): TVersionUpdateReason;
    var
      db2: TSqlDatabase;
      res: TSqlResult;
      row: TSqlRow;
      oldVersion: string;
    begin
      db2 := db as TSqlDatabase;
      res := TSqlResult.create( 'SELECT `version` FROM `outGeneral`', db2 );

      result := VERSUnrecognized;

      // Versions through 3.0.58 are automatically updated.
      // (outGeneral.version wasn't introduced until 3.0.59)

      if( not res.success ) then
        begin
          if( nil <> versionID ) then versionID^ := '';
          // Do nothing else
        end
      else
        begin
          row := res.fetchArrayFirst();

          if( nil = row ) then
            begin
              if( nil <> versionID ) then versionID^ := '';
              result := VERSOK;
              // Do nothing else
            end
          else if( null = row.field('version') ) then
            begin
              if( nil <> versionID ) then versionID^ := '';
              result := VERSOK;
              // Do nothing else
            end
          else
            begin
              oldVersion := trim( row.field('version') );

              if( nil <> versionID ) then versionID^ := oldVersion;

              // As new versions are released into the wild, add items here to describe the reasons for the updates.
              if
                ( '3.0.59' = oldVersion )
              or
                ( '3.0.60' = oldVersion )
              or
                ( '3.0.61' = oldVersion )
              or
                ( '3.0.62' = oldVersion )
              or
                ( '3.0.63' = oldVersion )
              or
                ( '3.0.64' = oldVersion )
              or
                ( '3.0.65' = oldVersion )
              or
                ( '3.0.66' = oldVersion )
              or
                ( '3.0.67' = oldVersion )
              or
                ( '3.0.68' = oldVersion )
              or
                ( '3.0.69' = oldVersion )
              or
                ( '3.0.70' = oldVersion )
              or
                ( '3.0.71' = oldVersion )
              or
                ( '3.0.72' = oldVersion )
              or
                ( '3.0.73' = oldVersion )
              or
                ( '3.0.74' = oldVersion )
              or
                ( '3.0.75' = oldVersion )
              or
                ( '3.0.76' = oldVersion )
              or
                ( '3.0.77' = oldVersion )
              then
                begin
                  // Anything prior to the first public release (3.0.78) should be
                  // considered "buggy" for one reason or another: see _ReleaseNotes.txt.
                  // (Since these versions were never used for analytical purposes, it's not
                  // a problem to treat them as invalid.)
                  result := VERSBug;
                end
              else if
                ( '3.0.78' = oldVersion )
              or
                ( '3.0.79' = oldVersion )
              or
                ( '3.0.80' = oldVersion )
              or
                ( '3.0.81' = oldVersion )
              or
                ( '3.0.82' = oldVersion )
              or
                ( '3.0.83' = oldVersion )
              or
                ( '3.0.84' = oldVersion )
              or
                ( '3.0.85' = oldVersion )
              then
                begin
                  // Released versions 3.0.78 and 3.0.85 introduce some new features,
                  // but don't invalidate anything done in a previous "good" release.
                  // Version 3.1.15 fixes a problem with RTree searches in the core model.
                  // The problem existed but was not detected in versions 3.0.78 through 3.0.84.
                  // Becasuse results from version 3.1.15 may vary from older versions,
                  // version 3.1.15 should be considered a specification change.
                  result := VERSModelSpecChange;
                end
              else if
                ( '3.1.0' = oldVersion )
              or
                ( '3.1.1' = oldVersion )
              or
                ( '3.1.2' = oldVersion )
              or
                ( '3.1.3' = oldVersion )
              or
                ( '3.1.4' = oldVersion )
              or
                ( '3.1.5' = oldVersion )
              or
                ( '3.1.6' = oldVersion )
              or
                ( '3.1.7' = oldVersion )
              or
                ( '3.1.8' = oldVersion )
              or
                ( '3.1.9' = oldVersion )
              or
                ( '3.1.10' = oldVersion )
              or
                ( '3.1.11' = oldVersion )
              or
                ( '3.1.12' = oldVersion )
              or
                ( '3.1.13' = oldVersion )
              or
                ( '3.1.14' = oldVersion )
              or
                ( '3.1.15' = oldVersion ) // Version 3.1.15 fixes bugs related to RTree searches.
              or
                ( '3.1.16' = oldVersion ) // Version 3.1.16 fixes more bugs related to RTree searches.
              then
                begin
                  result := VERSBUG;
                end
              else if
                ( '3.1.17' = oldVersion )
              or
                ( '3.1.18' = oldVersion )
              or
                ( '3.1.19' = oldVersion )
              {$IF Defined( CHEYENNE ) }
                then
                  result := VERSModelSpecChange
                else if
                  ( '3.0.80-Cheyenne' = oldVersion )
                or
                  // There was no 3.0.81-Cheyenne
              	  ( '3.0.82-Cheyenne' = oldVersion )
              	or
              	  // There was no 3.0.83-Cheyenne
              	  ( '3.0.84-Cheyenne' = oldVersion )
                then
                  begin
                    // See comment above RE specification change.
                    result := VERSModelSpecChange;
                  end
                else if
                  // There was no 3.0.85-Cheyenne
                  // There were no versions of Cheyenne that correspond to 3.1.0 through 3.1.12
                  ( '3.1.13-Cheyenne' = oldVersion )
                or
                  ( '3.1.14-Cheyenne' = oldVersion )
                or
                  ( '3.1.15-Cheyenne' = oldVersion )
                or
                  ( '3.1.16-Cheyenne' = oldVersion )
                then
                  begin
                    // See comments above RE bugs in versions 3.1.0 - 3.1.16.
                    result := VERSBUG;
                  end
                else if
                  ( '3.1.17-Cheyenne' = oldVersion )
                or
                  ( '3.1.18-Cheyenne' = oldVersion )
                or
                  ( '3.1.18-Cheyenne' = oldVersion )
              {$ELSEIF Defined( LARAMIE ) }
                then
                  result := VERSModelSpecChange
                else if
                  ( '3.1.18-Laramie' = oldVersion )
                or
                  ( '3.1.19-Laramie' = oldVersion )
              {$IFEND}
              then
                result := VERSOK
              ;
            end
          ;
        end
      ;

      res.free();
    end
  ;

end.
