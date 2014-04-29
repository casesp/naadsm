unit StringConsts;

(*
StringConsts.pas
----------------
Begin: 2005/06/08
Last revision: $Date: 2012-10-01 19:59:42 $ $Author: areeves $
Version: $Revision: 1.114.4.37 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2005 - 2012 Colorado State University

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
	// DON'T FORGET: when updating this unit, also update project options before release
  // (Project -> Options... -> Version Info)
  //
  // ALSO DON'T FORGET: assign an update reason in TSMDatabase.versionUpdateReason().
  //
  // FINALLY DON'T FORGET: check the database schema information in TSMDatabase.setUpdateReason().
  // It may or may not need to be altered.
	const
    MAJORVERSION = '3';
    MINORVERSION = '2';
    RELEASENUMBER = '18';
    MAJORVERSIONNUMBER = MAJORVERSION + '.' + MINORVERSION;
    MINORVERSIONNUMBER = RELEASENUMBER;

    // Add new major version numbers here as they are released.
    // Any change in the XML schema warrants a change in the minor version number (i.e., 3.2 to 3.3).
    const ALL_MAJOR_VERSIONS: array[ 0..0 ] of string = (
      '3.2'
    );

    {$IF Defined( CHEYENNE ) }
      MIN_COMPATIBLE_DLL_VERSION = '3.2.18-CHEYENNE'; // Used to determine if the right version of the core model is present
      BRANCHNAME = 'Cheyenne';
      BRANCHNAMEPARENS = ' (Cheyenne) ';
      SIM_DLL_NAME = 'cheyenne.dll';
      IS_EXPERIMENTAL = true;
      VERSIONFOROUTPUT = MAJORVERSIONNUMBER + '.' + MINORVERSIONNUMBER + '-Cheyenne';
    {$ELSEIF Defined( LARAMIE ) }
      MIN_COMPATIBLE_DLL_VERSION = '3.2.18-LARAMIE'; // Used to determine if the right version of the core model is present
      BRANCHNAME = 'Laramie';
      BRANCHNAMEPARENS = ' (Laramie) ';
      SIM_DLL_NAME = 'laramie.dll';
      IS_EXPERIMENTAL = true;
      VERSIONFOROUTPUT = MAJORVERSIONNUMBER + '.' + MINORVERSIONNUMBER + '-Laramie';
    {$ELSEIF Defined( RIVERTON ) }
      MIN_COMPATIBLE_DLL_VERSION = '3.2.18-RIVERTON'; // Used to determine if the right version of the core model is present
      BRANCHNAME = 'Riverton';
      BRANCHNAMEPARENS = ' (Riverton) ';
      SIM_DLL_NAME = 'riverton.dll';
      IS_EXPERIMENTAL = true;
      VERSIONFOROUTPUT = MAJORVERSIONNUMBER + '.' + MINORVERSIONNUMBER + '-Riverton';
    {$ELSEIF Defined( TORRINGTON ) }
      MIN_COMPATIBLE_DLL_VERSION = '3.2.18-TORRINGTON'; // Used to determine if the right version of the core model is present
      BRANCHNAME = 'Torrington';
      BRANCHNAMEPARENS = ' (Torrington) ';
      SIM_DLL_NAME = 'torrington.dll';
      IS_EXPERIMENTAL = true;
      VERSIONFOROUTPUT = MAJORVERSIONNUMBER + '.' + MINORVERSIONNUMBER + '-Torrington';
    {$ELSEIF Defined( WHEATLAND ) }
      MIN_COMPATIBLE_DLL_VERSION = '3.2.18-WHEATLAND'; // Used to determine if the right version of the core model is present
      BRANCHNAME = 'Wheatland';
      BRANCHNAMEPARENS = ' (Wheatland) ';
      SIM_DLL_NAME = 'wheatland.dll';
      IS_EXPERIMENTAL = true;
      VERSIONFOROUTPUT = MAJORVERSIONNUMBER + '.' + MINORVERSIONNUMBER + '-Wheatland';
    {$ELSE}
      MIN_COMPATIBLE_DLL_VERSION = '3.2.18'; // Used to determine if the right version of the core model is present
      BRANCHNAME = '';
      BRANCHNAMEPARENS = ' ';
      SIM_DLL_NAME = 'naadsm.dll';
      IS_EXPERIMENTAL = false;
      VERSIONFOROUTPUT = MAJORVERSIONNUMBER + '.' + MINORVERSIONNUMBER;
    {$IFEND}

    BUILDNUMBER = '20120829';

    APPNAME = 'NAADSM';
    VERSIONNUMBER = MAJORVERSIONNUMBER + '.' + MINORVERSIONNUMBER + ' Build ' + BUILDNUMBER;

  	// Used as the caption for the main form.
    {$IF Defined( DEBUG ) }
  	  MASTERCAPTION = APPNAME + BRANCHNAMEPARENS + MAJORVERSIONNUMBER + '.' + MINORVERSIONNUMBER + '-' + BUILDNUMBER;
    {$ELSEIF Defined( NOSHOWVERSION ) }  // for making documentation screen captures when we want to hide the major version number
       MASTERCAPTION = APPNAME;
    {$ELSE}
      MASTERCAPTION = APPNAME + BRANCHNAMEPARENS + MAJORVERSIONNUMBER;
    {$IFEND} // main form caption options

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
    WEBSITE_SUPPORT_FORUMS = 'http://www.naadsm.org/forum';
    
    COPYRIGHTDATES = '2003 - 2012';

  // Returns false if multiple experimental symbols are defined
  function experimentalVersionDefinitionsOK( var msg: string ): boolean;
    
implementation

  uses
    MyStrUtils,
    I88n
  ;

  function experimentalVersionDefinitionsOK( var msg: string ): boolean;
    var
      nDefs: integer;
    begin
      // Experimental versions are currently mutually exclusive.
      // Only one symbol should be defined at a time.  This function will
      // return false if multiple symbols are defined.

      // I would like to have a compile-time method for catching multiple
      // definitions, but so far, I haven't come up with a practical way to do it.

      nDefs := 0;

      {$IFDEF CHEYENNE}
        inc( nDefs );
      {$ENDIF}

      {$IFDEF LARAMIE}
        inc( nDefs );
      {$ENDIF}

      {$IFDEF RIVERTON}
        inc( nDefs );
      {$ENDIF}

      {$IFDEF TORRINGTON}
        inc( nDefs );
      {$ENDIF}

      {$IFDEF WHEATLAND}
        inc( nDefs );
      {$ENDIF}

      result := ( nDefs <= 1 );

      if( not result ) then
        begin
          msg := msg + endl + tr( 'Several experimental versions are simultaneously defined in this application.  This is an error caused by the application developers.' );
          msg := msg + '  ' + tr( 'Please check with the NAADSM Development Team to solve this problem.' );
        end
      ;
    end
  ;

end.
