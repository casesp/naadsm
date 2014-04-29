(*
Defs.inc
--------
Begin: 2006/03/01
Last revision: $Date: 2013-05-14 17:01:47 $ $Author: areeves $
Version: $Revision: 1.93.6.23 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2006 - 2013 NAADSM Development Team

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

// Define the following symbol to display and use the debugging window.
//{$DEFINE DEBUG}

// The following symbol must be defined for the PDF classes to have access to an SQL database.
// (For NAADSM, it should ALWAYS be defined.)
{$DEFINE DATABASE_ENABLED}

// The following symbol must be defined for the PDF classes to generate calculations, instead
// of simply storing parameters.
// (For NAADSM, it should ALWAYS be defined.)
{$DEFINE USE_GSL}

// The following symbol must be defined for Delphi units to use data structures in QClasses.
// (For NAADSM, it should ALWAYS be defined.)
{$DEFINE USE_QCLASSES}

// The following symbol must be defined for Delphi units to use SDEW for XML import.
// (For NAADSM, it should ALWAYS be defined.)
{$DEFINE SDEW_ENABLED}

// Uncomment these two lines to compile NAADSM as a console application.
//{$APPTYPE CONSOLE}
//{$DEFINE CONSOLEAPP}

// Define the following symbol to compile NAADSM as the experimental 'Cheyenne' version.
// NOTE: A symbol for only one experimental version should be defined at a time.
//{$DEFINE CHEYENNE}

// Define the following symbol to compile NAADSM as the experimental 'Laramie' version.
// NOTE: A symbol for only one experimental version should be defined at a time.
//{$DEFINE LARAMIE}

// If DEBUG is defined, this symbol can be used to disable network connections in NAADSM.
// If DEBUG is not defined, then this symbol has no effect.
{$DEFINE DISABLEWEBCHECK}

// Define the following symbol to use the application log.
//{$DEFINE APPLOG}

// Define the following symbol to enable unit self-testing upon initialization.
// (Only a few units currently have self-testing capability.)
//{$DEFINE TESTING}

// Define the following symbol to enable making screen shots of the main form w/o showing a version number.
//{$DEFINE NOSHOWVERSION}



