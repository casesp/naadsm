unit ReadXMLInput;
(*
ReadXMLInput.pas
-----------------
Begin: 2006/09/28
Last revision: $Date: 2008/11/25 22:02:35 $ $Author: areeves $
Version number: $Revision: 1.25 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Shaun Case <Shaun.Case@colostate.edu>
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2006 - 2008 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

(*
Revision History:
-----------------
$Log: ReadXMLInput.pas,v $
Revision 1.25  2008/11/25 22:02:35  areeves
Moved TProductionTypeList to its own unit; removed unnecessary type casting.

Revision 1.24  2008/10/23 20:25:38  areeves
Production type and zone IDs are now preserved in XML.

Revision 1.23  2008/10/17 20:07:02  areeves
A bunch of little bug fixes...

Revision 1.22  2008/10/17 19:20:47  areeves
A bunch of little bug fixes...

Revision 1.21  2008/10/16 20:37:44  shaun
--  Added line to set the internal destruction priority variable used in the PoductionType object during validation.  This value appears to mimic the "basic" destruction priority value.  (There are several destruction priorities associated with one production type, so this is kinda ambiguous).

Revision 1.20  2008/10/16 18:28:34  shaun
--  Fixed XML imports for Destruction priorities, reasons.  Vaccination methods were okay; the problems for vaccination errors  lay in other code files.

Revision 1.19  2008/10/15 16:23:09  areeves
Manual merge with Gilpin.

Revision 1.18  2008/09/18 22:50:47  areeves
Substantial revisions to model and chart inheritance schemes.

Revision 1.17  2008/04/18 20:35:19  areeves
Bug and translation fixes for version 3.1.17.

Revision 1.16  2008/03/12 22:10:55  areeves
Development branch "Fremont" merged into main trunk.

Revision 1.15.2.2  2008/02/28 16:45:12  areeves
Replaced float-to-string conversion functions for NAADSM I88n.

Revision 1.15.2.1  2008/02/27 00:41:28  areeves
Translation work is continuing.

Revision 1.15  2007/06/25 17:35:01  areeves
All XML import functions are now capable of creating missing production types, if necessary.

Revision 1.14  2007/06/25 16:50:33  areeves
- Import of functions with same purpose no longer results in naming scheme clashes.
- Import of spread models and herds with unrecognized production types now works properly.

Revision 1.13  2007/05/30 17:08:15  areeves
Merge from branch 'ElPaso' is complete.

Revision 1.12.4.1  2007/05/30 16:49:07  areeves
-- Delphi Fundamentals completely eliminated.
-- Lots of compiler warnings fixed.
-- QClasses-related memory leaks fixed.

Revision 1.12  2007/04/25 17:56:55  areeves
Post-'Douglas' merge.

Revision 1.11.4.2  2007/04/24 21:12:30  shaun
--  Fixed bugs:  Missing day on last Zone displayed in Zone status form, Window cascade/arrange/close  issues, Zone import issues in XML.
--  Added a dynamic Zone legend to the frmMap form.

Revision 1.11.4.1  2007/04/20 05:22:56  areeves
Bug related to zone radius import from XML fixed

Revision 1.11  2007/02/08 23:58:45  areeves
Minor bug fixes and new debugging capabilities.

Revision 1.10  2007/01/30 04:13:51  areeves
Swapped HTML encoding for XML encoding (fewer special characters are handled by "official" XML)

Revision 1.9  2007/01/30 04:00:45  areeves
Special characters are in the scenario description are now HTML encoded.

Revision 1.8  2007/01/20 00:43:57  areeves
Merged all changes from branch "Custer".

Revision 1.4.2.5.2.9  2007/01/19 19:08:46  areeves
- Added prevalence chart to XML importer.
- Cleaned up XML import process and displayed messages.

Revision 1.4.2.5.2.8  2007/01/16 21:23:45  areeves
Bug fixes and new features for version 3.1.1: see programming log.

Revision 1.4.2.5.2.7  2007/01/12 20:25:39  shaun
--  Fixed bug in XML import for multiple zones, which caused an invalid INSERT statement and error.

Revision 1.4.2.5.2.6  2007/01/11 17:15:39  shaun
--  Fixes for detectionParams and zoneParams to set the production type description, on import from XML.

Revision 1.4.2.5.2.5  2007/01/11 15:06:38  shaun
--  Added "trace-back-zone-focus-model", "basic-zone-focus-model", and "detection-model" with "zone" to the XML import.

Revision 1.4.2.5.2.4  2007/01/10 22:36:38  areeves
Renamed Scew and Delphi_Scew.dll to Sdew and sdew.dll, respectively.

Revision 1.4.2.5.2.3  2007/01/09 22:04:00  shaun
--  xml Zone import modifications.

Revision 1.4.2.5.2.2  2007/01/09 18:01:36  areeves
Basic zone functionality is in place, but not necessarily working correctly.

Revision 1.4.2.5.2.1  2007/01/04 18:54:11  areeves
- Zones partly implemented.
- All bug fixes from 'Costilla' and HEAD implemented.
- All new features from 'Costilla' implemented.

Revision 1.5  2007/01/04 18:16:22  areeves
Features from 'Costilla' merged into HEAD.

Revision 1.4.2.10  2007/01/04 17:40:37  shaun
--  Got an extra "end" statement...took it out.

Revision 1.4.2.9  2007/01/04 17:27:57  shaun
--  During an import for a herd XML file the productionTypeList.recountUnits() function needs to be called after the whole herdList has been built.  This sets the initial values of _unitCount for each productionType to the animal counts.  Without this call a call to the menu item "Daily unit status for 1 iteration" will fail with an exception when iterating through the productionTypeList, which checks the _unitCount of each productionType and raises an exception if any of them have not yet been set.  Normally, this IS set during a database read, but was not done for an XML import.  This is now fixed.

Revision 1.4.2.8  2007/01/04 17:13:08  areeves
Bug fixes made in Custer merged in to Costilla.

Revision 1.4.2.7  2006/12/19 22:49:06  shaun
--  Added ring-destruction priorities although they are not currently used during XML import....

Revision 1.4.2.6  2006/12/19 22:13:26  shaun
--  Fixed XML import issues with regard to priorities for vaccinations and destruction, (ring).
--  Added function ReadHerdXML, which simply adds herds from a herd XML file to a herdList passed to it.  This is used by the herd.pas file for adding herds to existing herd populations via an XML file in the setup section of the program.

Revision 1.4.2.5  2006/12/19 17:02:19  areeves
Multiple fixes to  bugs identified during GUI testing.

Revision 1.4.2.4  2006/12/15 15:10:22  areeves
Bug fixes, as listed in APHI programming log (pp. 3.52 - 3.56)

Revision 1.4.2.3  2006/12/12 20:57:07  areeves
Changes for QClasses merged into 'Costilla'

Revision 1.4.2.2  2006/11/10 23:33:37  areeves
Merged changes from ClearCreek via HEAD.

Revision 1.4  2006/11/10 06:03:50  areeves
Removed 'ClearCreek' designation from variable and function names.

Revision 1.3  2006/11/10 05:13:06  areeves
Trying to resolve CVS server problems.

Revision 1.1.4.5  2006/10/24 18:40:31  shaun
--  Fixed missing functionDictionary inserts for the global parameters for destruction capacity and vaccination capacity.
--  Removed some trailing spaces from ChartFunction names before inserting to functionDictionary.

Revision 1.1.4.4  2006/10/24 05:02:23  areeves
Removed interface-based PDF/REL function handling.

Revision 1.1.4.3  2006/10/20 20:21:52  areeves
- Removed obsolete XML parsing code.
- Removed obsolete and unnecessary files.

Revision 1.1.4.2  2006/10/20 18:48:42  areeves
XML parsing from 'Conejos' merged into 'ClearCreek'.

Revision 1.1.2.15  2006/10/19 16:49:25  shaun
--  Added a debugging flag to use with the dbcout functions....

Revision 1.1.2.14  2006/10/19 15:59:28  shaun
--  Modified the progress bar messages.
--  Added code to insure that function pointers are set in objects so that a simulation can be run straight from the import action....otherwise must close and reopen the database file.

Revision 1.1.2.13  2006/10/19 15:26:28  shaun
--  Added progress meter to operation...

Revision 1.1.2.12  2006/10/17 21:00:58  areeves
BetaPERT and Loglogistic PDFs are now imported from XML correctly.

Revision 1.1.2.11  2006/10/17 19:52:07  shaun
--  More mods for missing global boolean settings

Revision 1.1.2.10  2006/10/17 19:21:27  shaun
--  Fixed QA issues.  (quarantine-only, airborne-exponential, global params:  includeAirborne, usesAirborneExponentialDecay, and includeContactSpread)

Revision 1.1.2.9  2006/10/17 18:37:40  areeves
Fixed bug in triangular PDF parser.

Revision 1.1.2.8  2006/10/13 21:30:21  shaun
--  All fields are now in the database.....
--  Needs QA and testing.  General table has two quirks, which need to be set, but can not be using the existing code.  Mutator functions need to be added.

Revision 1.1.2.7  2006/10/12 17:29:22  shaun
--  Contact Models are complete.
--  Airborne spread models are complete.
--  Needs ring models done next....

Revision 1.1.2.6  2006/10/10 21:51:49  shaun
Adding spread models now....

Revision 1.1.2.5  2006/10/06 22:04:00  shaun
--  Mods for Detection, and Destruction

Revision 1.1.2.4  2006/10/05 18:07:32  shaun
Checkin for Thursday.

Detection models are done...now working on destruction models..

Revision 1.1.2.3  2006/10/03 20:20:13  shaun
Vaccine models are working.
Code is being added for the dection models now.

Revision 1.1.2.2  2006/09/29 21:08:53  shaun
Check for the weekend...still working on next phases, Vaccine Models...

Revision 1.1.2.1  2006/09/29 18:43:25  shaun
Added to the project to read scenario and herd xml files in order to place their data into the database.
*)

interface

  uses
    Windows,
    Messages,
    SysUtils,
    Variants,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    StdCtrls,
    Math,

    MyDialogs,
    QStringMaps,

    Sdew,

    FormProgress,

    XMLReader,
    Loc,
    xmlHerd,
    DiseaseModelStatMethods,
    TypInfo,
    ChartFunction,
    ProbDensityFunctions,
    Points,
    SMScenario,
    FunctionEnums,
    ProductionType,
    ProductionTypeList,
    FunctionDictionary,
    Herd,
    StatusEnums,
    VaccinationParams,
    RelFunction,
    DetectionParams,
    DestructionParams,
    GlobalControlParams,
    ProductionTypePair,
    ProductionTypePairList,
    ContactModel,
    AirborneSpreadModel,
    RingVaccParams,
    Zone,
    ZoneParams
  ;


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


type Loc = record
  latitude: double;
  longitude: double;
end;

type TUnitsPtr = ^TUnits;
type statMethodFunc = function( Element:Pointer; Sdew:TSdew; Units:TUnitsPtr ):TChartFunction;
type xmlProcessFunc = function( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;

type statEquates = record
  name:String;
  sFunc: statMethodFunc;
end;

type xmlProcessEquates = record
  name:String;
  xFunc: xmlProcessFunc;
end;

////////////////////////////////////////////////////////////////////////////////
//  Forward declarations of functions needed by the XML parsing routines.
//  These are callback functions, which are called by the XMLReader object as it
//  parses an XML file.
//////////////////////////////////////////////////////////////////////
function ProcessUniformStatMethod     ( Element:Pointer; Sdew:TSdew; Units:TUnitsPtr ):TChartFunction;
function ProcessGammaStatMethod       ( Element:Pointer; Sdew:TSdew; Units:TUnitsPtr ):TChartFunction;
function ProcessBetaPertStatMethod    ( Element:Pointer; Sdew:TSdew; Units:TUnitsPtr ):TChartFunction;
function ProcessPiecewiseStatMethod   ( Element:Pointer; Sdew:TSdew; Units:TUnitsPtr ):TChartFunction;
function ProcessTriangularStatMethod  ( Element:Pointer; Sdew:TSdew; Units:TUnitsPtr ):TChartFunction;
function ProcessLogisticsStatMethod   ( Element:Pointer; Sdew:TSdew; Units:TUnitsPtr ):TChartFunction;
function ProcessWeibullStatMethod     ( Element:Pointer; Sdew:TSdew; Units:TUnitsPtr ):TChartFunction;
function ProcessPointStatMethod       ( Element:Pointer; Sdew:TSdew; Units:TUnitsPtr ):TChartFunction;
function ProcessGaussianStatMethod    ( Element:Pointer; Sdew:TSdew; Units:TUnitsPtr ):TChartFunction;
function ProcessLoglogisticsStatMethod( Element:Pointer; Sdew:TSdew; Units:TUnitsPtr ):TChartFunction;
function ProcessPearson5StatMethod    ( Element:Pointer; Sdew:TSdew; Units:TUnitsPtr ):TChartFunction;
function ProcessLognormalStatMethod   ( Element:Pointer; Sdew:TSdew; Units:TUnitsPtr ):TChartFunction;
function ProcessBetaStatMethod        ( Element:Pointer; Sdew:TSdew; Units:TUnitsPtr ):TChartFunction;
function ProcessExponentialStatMethod ( Element:Pointer; Sdew:TSdew; Units:TUnitsPtr ):TChartFunction;

function ProcessZoneModels              ( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
function ProcessDiseaseModels           ( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
function ProcessVaccineModels           ( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
function ProcessDetectionModels         ( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
function ProcessRingDestructionModel    ( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
function ProcessBasicDestruction        ( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
function ProcessTracebackDestruction    ( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
function ProcessGlobalControlParams     ( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
function ProcessContactSpreadModel      ( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
function ProcessAirborneModel           ( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
function ProcessRingVaccineModel        ( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
function ProcessBasicZoneFocusModel     ( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
function ProcessTraceBackZoneFocusModel ( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;


type TSMScenarioPtr = ^TSMScenario;

type TXMLConvert = class(TObject)
  protected
    _smScenario:TSMScenarioPtr;
    _sfilename:String;
    _hfilename:String;

  public
    constructor create( HerdsFilename:String; ScenarioFilename:String; smScenario:TSMScenarioPtr );overload;
    procedure ConvertXMLToDatabase( err: pstring = nil );
    function ReadHerdXML( _hList:THerdList ):bool;
end;

type TVaccineDelay = class( TObject )
  protected
    _delay:String;
    _units:String;
  public
    constructor create( Delay:String; Units:String ); overload;
    function getDelay():String;
    function getUnits():String;
end;

type TVaccineStatMethod = class( TObject )
  protected
    _statMethod:TChartFunction;
  public
    constructor create( statMethod:TChartFunction );
    function getStatMethod():TChartFunction ;
end;



implementation
  uses
    StrUtils,
    
    MyStrUtils,
    USStrUtils,
    DebugWindow,
    I88n
  ;

var errorMessage: string;
var _internalDestructionPriorityList: TQStringLongIntMap;

const UseDebug: Boolean = false; // Set to true to enable debugging for this unit.

constructor TVaccineStatMethod.create( statMethod:TChartFunction );
  begin
    _statMethod := statMethod;
  end;

function TVaccineStatMethod.getStatMethod():TChartFunction ;
  begin
    result := _statMethod;
  end;

/////////////////////////////////////////
// Array of stat method names
const constStatMethods: array[0..13] of String =
(
  'uniform'    ,
  'gamma'      ,
  'beta-pert'  ,
  'piecewise'  ,
  'triangular' ,
  'logistic'   ,
  'weibull'    ,
  'point'      ,
  'gaussian'   ,
  'loglogistic',
  'pearson5'   ,
  'lognormal'  ,
  'beta'       ,
  'exponential'
);

///////////////////////////////////////////////////////////////////////////////
//  Array of stat method names and their associated callback functions.
//  This is used by the XMLReader object as it parses the XML file.  When
//  it encounters one of these stat methods, by name in an element, it calls
//  the associated callback function.
///////////////////////
const constNumStatMethods = 13;
const constStatMethodTypes:  array[0..constNumStatMethods] of statEquates =
(
  ( name: 'uniform';     sFunc: ProcessUniformStatMethod      ),
  ( name: 'gamma';       sFunc: ProcessGammaStatMethod        ),
  ( name: 'beta-pert';   sFunc: ProcessBetaPertStatMethod     ),
  ( name: 'piecewise';   sFunc: ProcessPiecewiseStatMethod    ),
  ( name: 'triangular';  sFunc: ProcessTriangularStatMethod   ),
  ( name: 'logistic';    sFunc: ProcessLogisticsStatMethod    ),
  ( name: 'weibull';     sFunc: ProcessWeibullStatMethod      ),
  ( name: 'point';       sFunc: ProcessPointStatMethod        ),
  ( name: 'gaussian';    sFunc: ProcessGaussianStatMethod     ),
  ( name: 'loglogistic'; sFunc: ProcessLoglogisticsStatMethod ),
  ( name: 'pearson5';    sFunc: ProcessPearson5StatMethod     ),
  ( name: 'lognormal';   sFunc: ProcessLognormalStatMethod    ),
  ( name: 'beta';        sFunc: ProcessBetaStatMethod         ),
  ( name: 'exponential'; sFunc: ProcessExponentialStatMethod  )
);

const constNumXMLProcs = 13;
const constXMLProcs:  array[0..constNumXMLProcs] of xmlProcessEquates =
(
  ( name: 'zone-model';                   xFunc: ProcessZoneModels           ),
  ( name: 'disease-model';                xFunc: ProcessDiseaseModels        ),
  ( name: 'vaccine-model';                xFunc: ProcessVaccineModels        ),
  ( name: 'detection-model';              xFunc: ProcessDetectionModels      ),
  ( name: 'basic-destruction-model';      xFunc: ProcessBasicDestruction     ),
  ( name: 'trace-back-destruction-model'; xFunc: ProcessTracebackDestruction ),
  ( name: 'ring-destruction-model';       xFunc: ProcessRingDestructionModel ),
  ( name: 'resources-and-implementation-of-controls-model'; xFunc: ProcessGlobalControlParams ),
  ( name: 'contact-spread-model';         xFunc: ProcessContactSpreadModel   ),
  ( name: 'airborne-spread-model';        xFunc: ProcessAirborneModel ),
  ( name: 'airborne-spread-exponential-model';        xFunc: ProcessAirborneModel ),
  ( name: 'ring-vaccination-model';       xFunc: ProcessRingVaccineModel ),
  ( name: 'basic-zone-focus-model';       xFunc: ProcessBasicZoneFocusModel ),
  ( name: 'trace-back-zone-focus-model';  xFunc: ProcessTraceBackZoneFocusModel )
);

constructor TVaccineDelay.create( Delay:String; Units:String );
  begin
    _delay := Delay;
    _units := Units;
  end;

function TVaccineDelay.getDelay():String;
  begin
    result := _delay;
  end;

function TVaccineDelay.getUnits():String;
  begin
    result := _units;
  end;


///////////////////////////////////////////////////////////////////////////////
//  These two functions are the callback functions used when loading a herd
//  XML file.  This process is started by clicking button 1. (See below)
//////////////////////////////////////////////////////////////////////////
   function ProcessHerdLocation( Element: Pointer; Sdew: TSdew; extData:Pointer): TObject;
     var
       latitude: double;
       longitude: double;
       location: TLoc;
     begin
       latitude := usStrToFloat(Sdew.GetElementContents(Sdew.GetElementByName(Element, PChar('latitude'))));
       longitude := usStrToFloat(Sdew.GetElementContents(Sdew.GetElementByName(Element, PChar('longitude'))));
       location := TLoc.create( latitude, longitude );
       result := location;
     end;

   function ProcessHerd( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
      var
         id: String;
         productionType: String;
         location: TLoc;
         size: Integer;
         status: String;
         tempXMLReader: TXMLReader;
         locationCallbacks : CallbackArray;
         Herd: TxmlHerd;

      begin
         SetLength(locationCallbacks, 1);
         locationCallbacks[0].Tag := 'location';
         locationCallbacks[0].Callback := ProcessHerdLocation;
         SetLength(locationCallbacks[0].ObjectList,0);

         tempXMLReader := TXMLReader.create( @locationCallbacks, Sdew, Element, @location );
         tempXMLReader.Run();
         tempXMLReader.destroy();

         id := Sdew.GetElementContents(Sdew.GetElementByName(Element, PChar('id')));
         productionType := Sdew.GetElementContents(Sdew.GetElementByName(Element, PChar('production-type')));
         size := StrToInt(Sdew.GetElementContents(Sdew.GetElementByName(Element, PChar('size'))));
         status := Sdew.GetElementContents(Sdew.GetElementByName(Element, PChar('status')));

         {*** Instantiate a TxmlHerd object here and load the above values to save to the database ***}
         if ( length(locationCallbacks[0].ObjectList) > 0 ) then
           Herd := TxmlHerd.create(id, productionType, size, TLoc(locationCallbacks[0].ObjectList[0]), status )
         else
           begin
             Herd := nil;
             Application.MessageBox('This herd file is not valid.  A herd is missing a location.', 'Error');
           end;

         result := Herd;

      end;


////////////////////////////////////////////////////////////////////////////////
//  These are the implementations of all of the callback functions seen in the
//  const declaration above of "constStatMethodTypes"
//
//  NOTE:  Anything returned by these callback fuctions is placed, by
//         the XMLReader object, into the array of records passed
//         to it prior to calling this callback.. As a result, the
//         XMLReader returns these updated arrays back to the calling
//         code for it's use.  Notice, in some of these functions, which
//         are nested, how they check this information and add it to their
//         own, creating objects to return.  See the XMLCallbacks record
//         definition in XMLReader for more information about the
//         structure of this data.
///////////////////////////////////////////////////////////////////////
function ProcessUniformStatMethod( Element:Pointer; Sdew:TSdew; Units:TUnitsPtr ):TChartFunction;
  var
    statMethod: TChartFunction;
    a:String;
    b:String;
  begin
    a := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'a' ));
    b := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'b' ));
    statMethod := TChartFunction( TPdfUniform.create( usStrToFloat(a), usStrToFloat(b), chartUnitTypeFromDatabase(Units^.GetUnits())  ) );

    result := statMethod;
  end;

function ProcessGammaStatMethod( Element:Pointer; Sdew:TSdew; Units:TUnitsPtr ):TChartFunction;
  var
    statMethod: TChartFunction;
    alpha:String;
    beta:String;
  begin
    alpha := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'alpha' ));
    beta := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'beta' ));
    statMethod := TChartFunction( TPdfGamma.create( usStrToFloat(alpha), usStrToFloat(beta), chartUnitTypeFromDatabase(Units^.GetUnits()) ) );

    result := statMethod;
  end;

function ProcessBetaPertStatMethod( Element:Pointer; Sdew:TSdew; Units:TUnitsPtr ):TChartFunction;
  var
    statMethod: TChartFunction;
    min:String;
    max:String;
    mode:String;
  begin
    min := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'min' ));
    max := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'max' ));
    mode := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'mode' ));
    statMethod := TChartFunction( TPdfBetaPert.create( usStrToFloat(min), usStrToFloat(mode), usStrToFloat(max), chartUnitTypeFromDatabase(Units^.GetUnits())) );

    result := statMethod;
  end;

function ProcessPiecewiseStatMethod( Element:Pointer; Sdew:TSdew; Units:TUnitsPtr ):TChartFunction;
  var
    statMethod: TChartFunction;
    Pieces:RPointArray;
    I:Integer;
    Count:Integer;
    value:String;
    P:String;
    Index:Integer;
    hadError:bool;
    pairs:Integer;
  begin
    statMethod := nil;

    Count := Sdew.GetElementCount( Element);
    if (( Count mod 2 ) = 0 ) then
      begin
        Index := 0;
        pairs := Count div 2;
        SetLength( Pieces, pairs );
        hadError := false;
        For I := 1 to pairs do
          begin
            if ( String(Sdew.GetElementNameByIndex( Element, Index ) ) = 'value' ) then
              begin
                value := Sdew.GetElementContents(Sdew.GetElementByIndex( Element, Index ));
                if ( String( Sdew.GetElementNameByIndex( Element, Index + 1) ) = 'p' ) then
                  begin
                    P := Sdew.GetElementContents(Sdew.GetElementByIndex( Element, Index + 1 ));
                    Index := Index + 2;
                    Pieces[I - 1].x := usStrToFloat(value);
                    Pieces[I - 1].y := usStrToFloat(P);
                  end
                else
                  begin
                    hadError := true;
                    Application.MessageBox('The piecewise statistics method in this file has invalid parameters','ERROR');
                  end;
              end
            else
              begin
                hadError := true;
                Application.MessageBox('The piecewise statistics method in this file has invalid parameters','ERROR');
              end;
          end;

          if ( not hadError ) then
            begin
              statMethod := TChartFunction( TPdfPiecewise.create( RPointArray(Pieces), chartUnitTypeFromDatabase(Units^.GetUnits())) );
            end;
      end
    else
      begin
        Application.MessageBox('The piecewise statistics method in this file has an invalid number of parameters','ERROR');
      end;

    result := statMethod;
  end;

function ProcessTriangularStatMethod( Element:Pointer; Sdew:TSdew; Units:TUnitsPtr ):TChartFunction;
  var
    statMethod: TChartFunction;
    a:String;
    b:String;
    c:String;
  begin
    a := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'a' ));
    b := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'b' ));
    c := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'c' ));
    statMethod := TChartFunction( TPdfTriangular.create( usStrToFloat(a), usStrToFloat(c), usStrToFloat(b), chartUnitTypeFromDatabase(Units^.GetUnits()) ) );

    result := statMethod;
  end;

function ProcessLogisticsStatMethod( Element:Pointer; Sdew:TSdew; Units:TUnitsPtr ):TChartFunction;
  var
    statMethod: TChartFunction;
    location:String;
    scale:String;
  begin
    location := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'location' ));
    scale := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'scale' ));
    statMethod := TChartFunction( TPdfLogistic.create( usStrToFloat(location), usStrToFloat(scale), chartUnitTypeFromDatabase(Units^.GetUnits())) );

    result := statMethod;
  end;

function ProcessWeibullStatMethod( Element:Pointer; Sdew:TSdew; Units:TUnitsPtr ):TChartFunction;
  var
    statMethod: TChartFunction;
    alpha:String;
    beta:String;
  begin
    alpha := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'alpha' ));
    beta := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'beta' ));
    statMethod := TChartFunction( TPdfWeibull.create( usStrToFloat(alpha), usStrToFloat(beta), chartUnitTypeFromDatabase(Units^.GetUnits())) );

    result := statMethod;
  end;

function ProcessPointStatMethod( Element:Pointer; Sdew:TSdew; Units:TUnitsPtr ):TChartFunction;
  var
    statMethod: TChartFunction;
    _point:String;
  begin
    _point := Sdew.GetElementContents( Element );
    statMethod := TChartFunction( TPdfPoint.create( usStrToFloat(_point), chartUnitTypeFromDatabase(Units^.GetUnits())) );

    result := statMethod;
  end;

function ProcessGaussianStatMethod( Element:Pointer; Sdew:TSdew; Units:TUnitsPtr ):TChartFunction;
  var
    statMethod: TChartFunction;
    mean:String;
    stddev:String;
  begin
    mean := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'mean' ));
    stddev := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'stddev' ));
    statMethod := TChartFunction( TPdfGaussian.create( usStrToFloat(mean), usStrToFloat(stddev), chartUnitTypeFromDatabase(Units^.GetUnits())) );

    result := statMethod;
  end;

function ProcessLoglogisticsStatMethod( Element:Pointer; Sdew:TSdew; Units:TUnitsPtr ):TChartFunction;
  var
    statMethod: TChartFunction;
    location:String;
    scale:String;
    shape:String;
  begin
    location := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'location' ));
    scale := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'scale' ));
    shape := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'shape' ));
    statMethod := TChartFunction( TPdfLoglogistic.create( usStrToFloat(location), usStrToFloat(scale), usStrToFloat(shape), chartUnitTypeFromDatabase(Units^.GetUnits())) );

    result := statMethod;
  end;

function ProcessPearson5StatMethod( Element:Pointer; Sdew:TSdew; Units:TUnitsPtr ):TChartFunction;
  var
    statMethod: TChartFunction;
    alpha:String;
    beta:String;
  begin
    alpha := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'alpha' ));
    beta := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'beta' ));
    statMethod := TChartFunction( TPdfPearson5.create( usStrToFloat(alpha), usStrToFloat(beta), chartUnitTypeFromDatabase(Units^.GetUnits())) );

    result := statMethod;
  end;

function ProcessLognormalStatMethod( Element:Pointer; Sdew:TSdew; Units:TUnitsPtr ):TChartFunction;
  var
    statMethod: TChartFunction;
    zeta:String;
    sigma:String;
  begin
    zeta := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'zeta' ));
    sigma := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'sigma' ));
    statMethod := TChartFunction( TPdfLognormal.create( TPdfLognormal.calculateMean( usStrToFloat(zeta), usStrToFloat(sigma)),TPdfLognormal.calculateStddev( usStrToFloat(zeta), usStrToFloat(sigma)) , chartUnitTypeFromDatabase(Units^.GetUnits())) );

    result := statMethod;
  end;

function ProcessBetaStatMethod( Element:Pointer; Sdew:TSdew; Units:TUnitsPtr ):TChartFunction;
  var
    statMethod: TChartFunction;
    alpha:String;
    beta:String;
    location:String;
    scale:String;
  begin
    alpha := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'alpha' ));
    beta := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'beta' ));
    location := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'location' ));
    scale := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'scale' ));
    statMethod := TChartFunction( TPdfBeta.create( usStrToFloat(alpha), usStrToFloat(beta), usStrToFloat(location), usStrToFloat(scale), chartUnitTypeFromDatabase(Units^.GetUnits())) );

    result := statMethod;
  end;

function ProcessExponentialStatMethod( Element:Pointer; Sdew:TSdew; Units:TUnitsPtr ):TChartFunction;
  var
    statMethod: TChartFunction;
    mean:String;
  begin
    mean := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'mean' ));
    statMethod := TChartFunction( TPdfExponential.create( usStrToFloat(mean), chartUnitTypeFromDatabase(Units^.GetUnits()) ) );

    result := statMethod;
  end;

function ProcessStatMethods( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    statMethod: TChartFunction;
    I:Integer;
    name:String;
    sFunc:statMethodFunc;
  begin
    statMethod := nil;
    sFunc := nil;
    name := Sdew.GetElementName(Element);

    for I := 0 to 13 do
      begin
        if ( constStatMethodTypes[i].name = name ) then
          begin
            sFunc := constStatMethodTypes[i].sFunc;
            break;
          end;
      end;

    if ( Assigned( sFunc ) ) then
      statMethod := sFunc( Element, Sdew, TUnitsPtr(extData) );

    result := statMethod;
  end;

function ProcessPeriodModels( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    statMethodCallbacks : CallbackArray;
    tempXMLReader: TXMLReader;
    I:Integer;
    periodName: String;
    periodObject: TDiseasePeriod;
    units:  TUnits;
    statMethod:TChartFunction;
  begin
    periodObject := nil;
    SetLength( statMethodCallbacks, 14 );
    for I := 0 to 13 do
      begin
        statMethodCallbacks[I].Tag := constStatMethods[I];
        statMethodCallbacks[I].Callback := ProcessStatMethods;
        SetLength(statMethodCallbacks[I].ObjectList,0);
      end;

    periodName := Sdew.GetElementName(Element);
    if ( Sdew.GetElementByName( Sdew.GetElementByName(Element, 'units'), 'xdf:unit') <> nil ) then
      units := TUnits.create((Sdew.GetElementContents( Sdew.GetElementByName( Sdew.GetElementByName(Element, 'units'), 'xdf:unit') )))
    else
      if ( Sdew.GetElementByName( Sdew.GetElementByName(Element, 'units'), 'xdf:unitless') <> nil ) then
        units := TUnits.create('unitless')
      else
        units := nil;

    tempXMLReader := TXMLReader.create( @statMethodCallbacks, Sdew, Element, Pointer(@units) );
    tempXMLReader.Run();
    tempXMLReader.destroy();

    for I := 0 to 13 do
      begin
        if (length(statMethodCallbacks[I].ObjectList) > 0 ) then
          begin
            statMethod := TChartFunction(statMethodCallbacks[I].ObjectList[0]);
            if ( Assigned( statMethod ) ) then
              statMethod.name := periodName;
              periodObject := TDiseasePeriod.create( statMethod, DiseaseModelStatMethods.NONE, units );
            break;
          end;
      end;
    result := periodObject;

  end;

function ProcessVaccineDelay( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    delay:String;
    units:String;
    VDelay:TVaccineDelay;
  begin
    delay := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'value' ));
    units := Sdew.GetElementContents( Sdew.GetElementByName( Sdew.GetElementByName(Element, 'units'), 'xdf:unit') );

    VDelay := TVaccineDelay.create( delay, units );
    result := VDelay;
  end;

function ProcessVaccineImmunityPeriod( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    statMethodCallbacks : CallbackArray;
    tempXMLReader: TXMLReader;
    I:Integer;
    periodName: String;
    units:  TUnits;
    statMethod:TChartFunction;
    ret_val:TVaccineStatMethod;
  begin
  //This routine will need to call the TChartFunction methods used for other sections of the XML file....
    ret_val := nil;

    SetLength( statMethodCallbacks, 14 );
    for I := 0 to 13 do
      begin
        statMethodCallbacks[I].Tag := constStatMethods[I];
        statMethodCallbacks[I].Callback := ProcessStatMethods;
        SetLength(statMethodCallbacks[I].ObjectList,0);
      end;
    periodName := Sdew.GetElementName(Element);

    if ( Sdew.GetElementByName( Sdew.GetElementByName(Element, 'units'), 'xdf:unit') <> nil ) then
      units := TUnits.create((Sdew.GetElementContents( Sdew.GetElementByName( Sdew.GetElementByName(Element, 'units'), 'xdf:unit') )))
    else
      if ( Sdew.GetElementByName( Sdew.GetElementByName(Element, 'units'), 'xdf:unitless') <> nil ) then
        units := TUnits.create('unitless')
      else
        units := nil;

    //units := TUnits.create((Sdew.GetElementContents( Sdew.GetElementByName( Sdew.GetElementByName(Element, 'units'), 'xdf:unit') )));

    tempXMLReader := TXMLReader.create( @statMethodCallbacks, Sdew, Element, Pointer(@units) );
    tempXMLReader.Run();
    tempXMLReader.destroy();

    for I := 0 to 13 do
      begin
        if (length(statMethodCallbacks[I].ObjectList) > 0 ) then
          begin
            statMethod := TChartFunction(statMethodCallbacks[I].ObjectList[0]);
            if ( Assigned( statMethod ) ) then
              begin
                ret_val := TVaccineStatMethod.create( statMethod );
              end;
            break;
          end;
      end;

    result := ret_val;
  end;


function ProcessBasicDestruction( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    _smScenarioPtr:TSMScenarioPtr;
    _prodTypeList:TProductionTypeList;
    _prodType:TProductionType;
    prodType:String;
    prodTypeID: integer;
    Priority:String;
    DParms:TDestructionParams;
  begin
    result := nil;

    _smScenarioPtr := TSMScenarioPtr(extData);
    _prodTypeList := _smScenarioPtr^.simInput.ptList;

    prodType := Sdew.GetElementAttribute( Element, 'production-type' );
    prodTypeID := myStrToInt( Sdew.GetElementAttribute( Element, 'production-type-id' ), -1 );

    _prodType := _prodTypeList.findProdType( prodType );

    if( nil = _prodType ) then
      begin
        _prodType := TProductionType.create( prodTypeID, prodType, false, _smScenarioPtr^.simInput );
        _prodType.populateDatabase( _smScenarioPtr^.simInput.database, true );
        _prodTypeList.append( _prodType );
      end
    ;

    if ( Assigned(_prodType.destructionParams) ) then
      DParms := _prodType.destructionParams
    else
      begin
        DParms := TDestructionParams.create();
        DParms.sim := _smScenarioPtr.simInput;
        DParms.prodTypeDescr := prodType;
        _prodType.destructionParams := DParms;
      end
    ;

    _smScenarioPtr^.simInput.controlParams.useDestructionGlobal := true;
    DParms.destroyDetectedUnits := true;
    Priority := Sdew.GetElementContents( Sdew.GetElementByName(Element, 'priority') );

    if ( _internalDestructionPriorityList.contains( 'basic' ) ) then
      begin
        if ( _internalDestructionPriorityList.Item['basic'] > StrToInt( Priority ) ) then
           _internalDestructionPriorityList.Item['basic'] := StrToInt( Priority );
       end
     else
       begin
           _internalDestructionPriorityList.Item['basic'] := StrToInt( Priority );
       end;

    DParms.destrPriority := StrToInt( Priority );   
    _smScenarioPtr.simInput.controlParams.ssDestrPriorities.Add( prodType + '+' + 'basic', StrToInt( Priority ));
    _smScenarioPtr^.simInput.controlParams.useDestructionGlobal := true;
    //    _smScenarioPtr^.simInput.updateDatabase();
    _prodType.populateDatabase( _smScenarioPtr.simInput.database );
  end
;


function ProcessTracebackDestruction( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    _smScenarioPtr:TSMScenarioPtr;
    _prodTypeList:TProductionTypeList;
    _prodType:TProductionType;
    prodType:String;
    prodTypeID: integer;
    Priority:Integer;
    DParms:TDestructionParams;
    contactType:String;
    Period:Integer;
    Success:Double;
    QuarantineOnly:Boolean;
  begin
    result := nil; // until established otherwise

    _smScenarioPtr := TSMScenarioPtr(extData);
    _prodTypeList := _smScenarioPtr^.simInput.ptList;

    prodType := Sdew.GetElementAttribute( Element, 'production-type' );
    prodTypeID := myStrToInt( Sdew.GetElementAttribute( Element, 'production-type-id' ), -1 );
    contactType := Sdew.GetElementAttribute( Element, 'contact-type' );

    _prodType := _prodTypeList.findProdType( prodType );

    if( nil = _prodType ) then
      begin
        _prodType := TProductionType.create( prodTypeID, prodType, false, _smScenarioPtr^.simInput );
        _prodType.populateDatabase( _smScenarioPtr^.simInput.database, true );
        _prodTypeList.append( _prodType );
      end
    ;

    if ( Assigned(_prodType.destructionParams) ) then
      DParms := _prodType.destructionParams
    else
      begin
        DParms := TDestructionParams.create();
        DParms.sim := _smScenarioPtr.simInput;
        DParms.prodTypeDescr := prodType;
        _prodType.destructionParams := DParms;
      end
    ;

    //??? Is this correct to set this for other types of destruction other than basic????
    _smScenarioPtr^.simInput.controlParams.useDestructionGlobal := true;

    Success := usStrToFloat( Sdew.GetElementContents(Sdew.GetElementByName( Element, 'trace-success' )) );
    //NOTE:  Ignoring the units in the period element because they are always set to "day" by the xml output routines for DestructionParams.
    Period := StrToInt( Sdew.GetElementContents( Sdew.GetElementByName( Sdew.GetElementByName( Element, 'trace-period'), 'value')));
    Priority := StrToInt( Sdew.GetElementContents(Sdew.GetElementByName(Element, 'priority')));
    QuarantineOnly := StrToBool( Sdew.GetElementContents( Sdew.GetElementByName( Element, 'quarantine-only') ) );


    //NOTE:  This is a placeholder in case the traceback element is found before the basic one, or if there
    //       is no basic one.....Notice that we will not set the "includeDestructionGlobal" until/unless we
    //       find an actual basic destruction element.
    _smScenarioPtr.simInput.controlParams.ssDestrPriorities.Add( prodType + '+' + 'basic', 0 );

    if ( contactType = 'direct' ) then
      begin
        DParms.traceDirectContact := true;
        DParms.directTracePeriod := Period;
        DParms.directTraceSuccess := Success;
        if ( not QuarantineOnly ) then
          DParms.destroyDirectTraces := true
        ;

        if ( _internalDestructionPriorityList.contains( 'direct' ) ) then
          begin
            if ( _internalDestructionPriorityList.Item['direct'] > Priority ) then
               _internalDestructionPriorityList.Item['direct'] := Priority;
           end
         else
           begin
               _internalDestructionPriorityList.Item['direct'] := Priority;
           end;

        _smScenarioPtr.simInput.controlParams.ssDestrPriorities.Add( prodType + '+' + 'direct', Priority );
      end
    else
      begin
        DParms.traceIndirectContact := true;
        DParms.indirectTracePeriod := Period;
        DParms.indirectTraceSuccess := Success;
        if ( not QuarantineOnly ) then
          DParms.destroyIndirectTraces := true
        ;
        if ( _internalDestructionPriorityList.contains( 'indirect' ) ) then
          begin
            if ( _internalDestructionPriorityList.Item['indirect'] > Priority ) then
               _internalDestructionPriorityList.Item['indirect'] := Priority;
           end
         else
           begin
               _internalDestructionPriorityList.Item['indirect'] := Priority;
           end;
        _smScenarioPtr.simInput.controlParams.ssDestrPriorities.Add( prodType + '+' + 'indirect', Priority );
      end
    ;
    _smScenarioPtr^.simInput.controlParams.useDestructionGlobal := true;
    //         _smScenarioPtr^.simInput.updateDatabase();
    _prodType.populateDatabase( _smScenarioPtr.simInput.database );
  end
;


function ProcessRingDestructionModel( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    _smScenarioPtr:TSMScenarioPtr;
    _prodTypeList:TProductionTypeList;
    prodTypeSrc, prodTypeDest:String;
    _prodTypeSrc, _prodTypeDest:TProductionType;
    value:Double;
    DParms:TDestructionParams;
    ret_val:TObject;
    subElement:Pointer;
    priority: Integer;
    destrPriorityList: TQStringLongIntMap;
  begin
    ret_val := nil;

    _smScenarioPtr := TSMScenarioPtr(extData);
    _prodTypeList := _smScenarioPtr^.simInput.ptList;
     destrPriorityList := _smScenarioPtr^.simInput.controlParams.ssDestrPriorities;

    prodTypeSrc :=  Sdew.GetElementAttribute( Element, 'from-production-type' );
    prodTypeDest := Sdew.GetElementAttribute( Element, 'to-production-type');

    //Get or create the production-type-pair here....
    _prodTypeSrc := _prodTypeList.findProdType( prodTypeSrc );
    _prodTypeDest := _prodTypeList.findProdType( prodTypeDest );

    if( nil = _prodTypeSrc ) then // The production type should be created
      begin
        _prodTypeSrc := TProductionType.create( -1, prodTypeSrc, false, _smScenarioPtr^.simInput );
        _prodTypeSrc.populateDatabase( _smScenarioPtr^.simInput.database );
        _prodTypeList.append( _prodTypeSrc );
      end
    ;

    if ( nil = _prodTypeDest ) then // The production type should be created
      begin
        _prodTypeDest := TProductionType.create( -1, prodTypeDest, false, _smScenarioPtr^.simInput );
        _prodTypeDest.populateDatabase( _smScenarioPtr^.simInput.database );
        _prodTypeList.append( _prodTypeDest );
      end
    ;

    subElement := Sdew.GetElementByName( Element, 'radius' );
    if ( subElement <> nil ) then
      begin
        if ( Sdew.GetElementByName( subElement, 'value' ) <> nil ) then
          begin
            value := usStrToFloat( Sdew.GetElementContents(Sdew.GetElementByName( subElement, 'value' )) );
            DParms := _prodTypeSrc.destructionParams;

            if ( DParms = nil ) then
              begin
                DParms := TDestructionParams.create();
                _prodTypeSrc.destructionParams := DParms;
                DParms.sim := _smScenarioPtr.simInput;
                DParms.prodTypeDescr := prodTypeSrc;
                _prodTypeSrc.destructionParams := DParms;
              end
            ;

            DParms.isRingTrigger := true;
            DParms.ringRadius := value; //Note:  Units are not used, tho they are in the xml file...assume Km.

            DParms := _prodTypeDest.destructionParams;
            if ( DParms = nil ) then
              begin
                DParms := TDestructionParams.create();
                _prodTypeDest.destructionParams := DParms;
                DParms.sim := _smScenarioPtr.simInput;
                DParms.prodTypeDescr := prodTypeDest;
                _prodTypeDest.destructionParams := DParms;
              end
            ;
            DParms.isRingTarget := true;

            //Priorities go with the "to type", i.e. destination....
            //NOTE:  These destruction ring priorities are NOT currently used
            //       in the system, but may be in the future.
            if ( Sdew.GetElementByName( Element, 'priority') <> nil ) then
              begin
                priority := StrToInt( Sdew.GetElementContents( Sdew.GetElementByName( Element, 'priority')) );

                if ( _internalDestructionPriorityList.contains( 'ring' ) ) then
                  begin
                    if ( _internalDestructionPriorityList.Item['ring'] > priority ) then
                      _internalDestructionPriorityList.Item['ring'] := priority;
                  end
                else
                  begin
                      _internalDestructionPriorityList.Item['ring'] := priority;
                  end;

                destrPriorityList.Add( prodTypeDest + '+' + 'ring', priority );
                //Set to bogus value so that the populateDatabase() function won't
                // throw an exception...because from-type doesn't need a priority here...but the populate function doesn't know that....
                destrPriorityList.Add( prodTypeSrc + '+' + 'ring', -1 );
              end
            ;


            _smScenarioPtr^.simInput.controlParams.useDestructionGlobal := true;
            _prodTypeSrc.populateDatabase( _smScenarioPtr^.simInput.database );
            _prodTypeDest.populateDatabase( _smScenarioPtr^.simInput.database );
          end
        ;
      end
    ;

    result := ret_val;
  end
;


function ProcessRingVaccineModel( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    _smScenarioPtr:TSMScenarioPtr;
    _prodTypeList:TProductionTypeList;
    prodTypeSrc, prodTypeDest:String;
    _prodTypeSrc, _prodTypeDest:TProductionType;
    value:Double;
    ret_val:TObject;
    subElement:Pointer;
    priority:Integer;
    minTimeBetweenVacc:Integer;
    ringVaccSrc, ringVaccDest:TRingVaccParams;
  begin
    ret_val := nil;

    _smScenarioPtr := TSMScenarioPtr(extData);
    _prodTypeList := _smScenarioPtr^.simInput.ptList;

    prodTypeSrc :=  Sdew.GetElementAttribute( Element, 'from-production-type' );
    prodTypeDest := Sdew.GetElementAttribute( Element, 'to-production-type');

    //Get or create the production-type-pair here....
    _prodTypeSrc := _prodTypeList.findProdType( prodTypeSrc );
    _prodTypeDest := _prodTypeList.findProdType( prodTypeDest );

    if( nil = _prodTypeSrc ) then // The production type should be created
      begin
        _prodTypeSrc := TProductionType.create( -1, prodTypeSrc, false, _smScenarioPtr^.simInput );
        _prodTypeSrc.populateDatabase( _smScenarioPtr^.simInput.database );
        _prodTypeList.append( _prodTypeSrc );
      end
    ;

    if( nil = _prodTypeDest ) then // The production type should be created
      begin
        _prodTypeDest := TProductionType.create( -1, prodTypeDest, false, _smScenarioPtr^.simInput );
        _prodTypeDest.populateDatabase( _smScenarioPtr^.simInput.database );
        _prodTypeList.append( _prodTypeDest );
      end
    ;


    ringVaccSrc := _prodTypeSrc.ringVaccParams;
    if ( ringVaccSrc = nil ) then
      begin
        ringVaccSrc := TRingVaccParams.create();
        ringVaccSrc.sim := _smScenarioPtr^.simInput;
        ringVaccSrc.prodTypeDescr := prodTypeSrc;
        _prodTypeSrc.ringVaccParams := ringVaccSrc;
      end
    ;

    ringVaccDest := _prodTypeDest.ringVaccParams;
    if ( ringVaccDest = nil ) then
      begin
        ringVaccDest := TRingVaccParams.create();
        ringVaccDest.sim := _smScenarioPtr^.simInput;
        ringVaccDest.prodTypeDescr := prodTypeDest;
        _prodTypeDest.ringVaccParams := ringVaccDest;
      end
    ;

    ringVaccSrc.useRing := true;

    //Priorities go with the "to type", i.e. destination.
    if ( Sdew.GetElementByName( Element, 'priority') <> nil ) then
      begin
        priority := StrToInt( Sdew.GetElementContents( Sdew.GetElementByName( Element, 'priority')) );
        _smScenarioPtr^.simInput.controlParams.ssVaccPriorities.Add( prodTypeDest + '+' + 'ring', priority );
        //Set to bogus value so that the populateDatabase() function won't
        // throw an exception...because from-type doesn't need a priority here...but the populate function doesn't know that....
        _smScenarioPtr^.simInput.controlParams.ssVaccPriorities.Add( prodTypeSrc + '+' + 'ring', -1 );
      end
    ;

    subElement := Sdew.GetElementByName( Element, 'radius' );
    if ( subElement <> nil ) then
      begin
        if ( Sdew.GetElementByName( subElement, 'value' ) <> nil ) then
          begin
            value := usStrToFloat( Sdew.GetElementContents( Sdew.GetElementByName( subElement, 'value' ) ) );
            //  Units are present in the xml file, but not stored anywhere in the current schema.
            ringVaccSrc.ringRadius := value;

            subElement := Sdew.GetElementByName( Element, 'min-time-between-vaccinations');
            if ( subElement <> nil ) then
              begin
                minTimeBetweenVacc := StrToInt( Sdew.GetElementContents( Sdew.GetElementByName( subElement, 'value' ) ) );
                //  Units are present in the xml file, but not stored in anywhere in the current schema.
                ringVaccSrc.minTimeBetweenVacc := minTimeBetweenVacc;

                _smScenarioPtr^.simInput.controlParams.useVaccGlobal := true;
                ringVaccSrc.populateDatabase( _smScenarioPtr^.simInput.database, _prodTypeSrc.productionTypeID );
                ringVaccDest.populateDatabase( _smScenarioPtr^.simInput.database, _prodTypeDest.productionTypeID );
                _prodTypeSrc.populateDatabase( _smScenarioPtr^.simInput.database );
                _prodTypeDest.populateDatabase( _smScenarioPtr^.simInput.database );
              end
            ;
          end
        ;
      end
    ;

    result := ret_val;
  end
;


procedure ProcessPercentValues( Element: Pointer; Sdew: TSdew; statMethod:TRelFunction );
  var
    x,y,temp:Double;
    i:Integer;
    Count:Integer;
    gotX:Boolean;
  begin
    Count := Sdew.GetElementCount( Element );
    gotX := false;
    x := 0.0;

    for i := 0 to (Count-1) do
      begin
        if ( Sdew.GetElementName(Sdew.GetElementByIndex( Element, i)) = 'value' ) then
          begin
            temp := usStrToFloat( Sdew.GetElementContents(Sdew.GetElementByIndex( Element, i)) );
            if ( gotX ) then
              begin
                y := temp * 100.0;
                gotX := false;
                statMethod.addPoint( x, y );
              end
            else
              begin
                x := temp;
                gotX := true;
              end
            ;
          end
        ;
      end
    ;
  end
;


procedure ProcessValues( Element: Pointer; Sdew: TSdew; statMethod:TRelFunction );
  var
    x,y,temp:Double;
    i:Integer;
    Count:Integer;
    gotX:Boolean;
  begin
    Count := Sdew.GetElementCount( Element );
    gotX := false;
    x := 0.0;
    
    for i := 0 to (Count-1) do
      begin
        if ( Sdew.GetElementName(Sdew.GetElementByIndex( Element, i)) = 'value' ) then
          begin
            temp := usStrToFloat( Sdew.GetElementContents(Sdew.GetElementByIndex( Element, i)) );
            if ( gotX ) then
              begin
                y := temp;
                gotX := false;
                statMethod.addPoint( x, y );
              end
            else
              begin
                x := temp;
                gotX := true;
              end
            ;
          end
        ;
      end
    ;
  end
;


function GetStatModel( Element:Pointer; Sdew:TSdew ):TObject;
  var
    units:TUnits;
    statMethodCallbacks:CallbackArray;
    tempXMLReader:TXMLReader;
    ret_val:TObject;
    I:Integer;
  begin
    ret_val := nil;
    SetLength( statMethodCallbacks, 14 );
    for I := 0 to 13 do
      begin
        statMethodCallbacks[I].Tag := constStatMethods[I];
        statMethodCallbacks[I].Callback := ProcessStatMethods;
        SetLength(statMethodCallbacks[I].ObjectList,0);
      end;

    if ( Sdew.GetElementByName( Sdew.GetElementByName(Element, 'units'), 'xdf:unit') <> nil ) then
      units := TUnits.create((Sdew.GetElementContents( Sdew.GetElementByName( Sdew.GetElementByName(Element, 'units'), 'xdf:unit') )))
    else
      begin
        if ( Sdew.GetElementByName( Sdew.GetElementByName(Element, 'units'), 'xdf:unitless') <> nil ) then
          units := TUnits.create('unitless')
        else
          units := nil
        ;
      end
    ;

    tempXMLReader := TXMLReader.create( @statMethodCallbacks, Sdew, Element, Pointer(@units) );
    tempXMLReader.Run();
    tempXMLReader.destroy();

    for I := 0 to 13 do
      begin
        if ( length( statMethodCallbacks[I].ObjectList ) > 0 ) then
          begin
            if ( Assigned( statMethodCallbacks[I].ObjectList[0] ) ) then
              ret_val := statMethodCallbacks[I].ObjectList[0]
            ;
          end
        ;
      end
    ;
    result := ret_val;
  end
;

function ProcessAirborneModel( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    ret_val:TObject;
    _smScenarioPtr:TSMScenarioPtr;
    _prodTypeList:TProductionTypeList;
    _ptpList:TProductionTypePairList;
    _ptp:TProductionTypePair;
    _prodTypeSrc, _prodTypeDest:TProductionType;
    prodTypeSrc, prodTypeDest:String;
    _airModel:TAirborneSpreadModel;
    subElement:Pointer;
    ssubElement:Pointer;
    unitElement:Pointer;
    statMethod:TChartFunction;
    probSpread:Double;
    Index:Integer;
    ChartID:Integer;
    windDirStart, windDirEnd:Integer;
    maxSpread:Double;
    exponential:Boolean;
    nFunctionsWithSameName: integer;
  begin
    nFunctionsWithSameName := 0;
    ret_val := nil;
    exponential := false;
    _smScenarioPtr := TSMScenarioPtr(extData);
    _prodTypeList := _smScenarioPtr^.simInput.ptList;
    _ptpList := _smScenarioPtr^.simInput.ptpList;

    if ( Sdew.GetElementName( Element ) = 'airborne-spread-exponential-model' ) then
      exponential := true;
      
    prodTypeSrc :=  Sdew.GetElementAttribute( Element, 'from-production-type' );
    prodTypeDest := Sdew.GetElementAttribute( Element, 'to-production-type');

    //Get or create the production-type-pair here....
    _prodTypeSrc := _prodTypeList.findProdType( prodTypeSrc );
    _prodTypeDest := _prodTypeList.findProdType( prodTypeDest );

    if( nil = _prodTypeSrc ) then // The production type should be created
      begin
        _prodTypeSrc := TProductionType.create( -1, prodTypeSrc, false, _smScenarioPtr^.simInput );
        _prodTypeSrc.populateDatabase( _smScenarioPtr^.simInput.database );
        _prodTypeList.append( _prodTypeSrc );
      end
    ;

    if( nil = _prodTypeDest ) then // The production type should be created
      begin
        _prodTypeDest := TProductionType.create( -1, prodTypeDest, false, _smScenarioPtr^.simInput );
        _prodTypeDest.populateDatabase( _smScenarioPtr^.simInput.database );
        _prodTypeList.append( _prodTypeDest );
      end
    ;

    Index := _ptpList.pairPosition( _prodTypeSrc, _prodTypeDest );
    if ( Index <> -1 ) then
      begin
        _ptp := _ptpList.objects[Index] as TProductionTypePair;
      end
    else
      begin
        _ptp := TProductionTypePair.create( _prodTypeSrc, _prodTypeDest, _smScenarioPtr^.simInput );
        _smScenarioPtr^.simInput.database.makeProductionTypePair( _prodTypeSrc.productionTypeID, _prodTypeDest.productionTypeID );
        _ptp.populateDatabase( _smScenarioPtr^.simInput.database );
        _ptpList.Add( _ptp );
      end;
    _airModel := TAirborneSpreadModel.create(  _smScenarioPtr^.simInput.database,
                    -1, _smScenarioPtr^.simInput, _prodTypeSrc.productionTypeID,
                     _prodTypeDest.productionTypeID, false );

    subElement := Sdew.GetElementByName( Element, 'prob-spread-1km' );
    if ( subElement <> nil ) then
      begin
        probSpread := usStrToFloat( Sdew.GetElementContents( subElement ) );
        _airModel.probSpread1km := probSpread;
      end;

    subElement := Sdew.GetElementByName( Element, 'delay' );
    if ( subElement <> nil ) then
      begin
        statMethod := TChartFunction( GetStatModel( subElement, Sdew ) );
        statMethod.name := 'Day airborne delay';

        statMethod.dbField := word( AIRDelay );

        ChartID := _smScenarioPtr^.simInput.database.compareFunctions( smChartStr(TSMChart(statMethod.dbField)), statMethod, nFunctionsWithSameName );

        if ( ChartID = -1 ) then
          begin
            if( 0 < nFunctionsWithSameName ) then
              statMethod.name := statMethod.name + ' (' + intToStr( nFunctionsWithSameName + 1 ) + ')'
            ;
            _smScenarioPtr^.simInput.functionDictionary.insert( statMethod.name, TFunctionDictionaryItem.create( statMethod ) );
            statMethod.populateDatabase( _smScenarioPtr^.simInput.database );
          end
        else
          begin
            statMethod.destroy();
            statMethod := functionFromDB( _smScenarioPtr^.simInput.database, ChartId );
          end;

        _airModel.delayName := statMethod.name;
        //_airModel.pdfDelay := statMethod;
      end;

    subElement := Sdew.GetElementByName( Element, 'wind-direction-start' );
    if ( subElement <> nil ) then
      begin
        ssubElement := Sdew.GetElementByName( subElement, 'value' );
        if ( ssubElement <> nil ) then
          begin
            windDirStart := StrToInt( Sdew.GetElementContents( ssubElement ) );
            _airModel.windStart := windDirStart;
          end;
      end;

    subElement := Sdew.GetElementByName( Element, 'wind-direction-end' );
    if ( subElement <> nil ) then
      begin
        ssubElement := Sdew.GetElementByName( subElement, 'value' );
        if ( ssubElement <> nil ) then
          begin
            windDirEnd := StrToInt( Sdew.GetElementContents( ssubElement ) );
            _airModel.windEnd := windDirEnd;
          end;
      end;

    if ( not exponential ) then
      begin
        subElement := Sdew.GetElementByName( Element, 'max-spread' );
        if ( subElement <> nil ) then
          begin
            ssubElement := Sdew.GetElementByName( subElement, 'value' );
            if ( ssubElement <> nil ) then
              begin
                maxSpread := usStrToFloat( Sdew.GetElementContents( ssubElement ) );
                _airModel.maxSpread := maxSpread;

                unitElement := Sdew.GetElementByName( subElement, 'units' );
                if ( unitElement <> nil ) then
                  begin
                    if ( Sdew.GetElementByName( unitElement, 'xdf:unit') <> nil ) then
                      _airModel.maxSpreadUnits := Sdew.GetElementContents( Sdew.GetElementByName( unitElement, 'xdf:unit'))
                    else
                      if (Sdew.GetElementByName( unitElement, 'xdf:unitless') <> nil ) then
                        _airModel.maxSpreadUnits := 'unitless';
                  end;
              end;
          end;
      end;

    _ptp.airborne := _airModel;
    _airModel.useAirborne := true;

    _smScenarioPtr^.simInput.includeAirborneSpreadGlobal := true;

    if ( exponential ) then
      _smScenarioPtr^.simInput.useAirborneExponentialDecay := true;

    _smScenarioPtr^.simInput.populateDatabase();
    _ptp.populateDatabase( _smScenarioPtr^.simInput.database, true );
    _airModel.populateDatabase( _smScenarioPtr^.simInput.database );

    result := ret_val;
  end
;


function ProcessContactSpreadZoneModel( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    _smScenarioPtr:TSMScenarioPtr;
    _prodTypeList:TProductionTypeList;
    _prodTypeSrc:TProductionType;
    prodTypeSrc:String;
    contactType:String;
    ZoneName:String;
    subElement:Pointer;
    nextUnitElement, unitElement:Pointer;
    Chart:TRelFunction;
    ChartID:Integer;
    _zoneList:TZoneList;
    _zoneParams: TZoneParams;
    _zoneId:Integer;
    nFunctionsWithSameName: integer;
  begin
    nFunctionsWithSameName := 0;
    result := nil;

    _smScenarioPtr := TSMScenarioPtr(extData);
    _prodTypeList := _smScenarioPtr^.simInput.ptList;
    _zoneList := _smScenarioPtr^.simInput.zoneList;

    prodTypeSrc :=  Sdew.GetElementAttribute( Element, 'from-production-type' );
    contactType :=  Sdew.GetElementAttribute( Element, 'contact-type' );
    ZoneName := Sdew.GetElementAttribute( Element, 'zone' );

    if ( _zoneList.find( ZoneName ) = nil ) then
      begin
        // We have a problem here...this should already exist.
      end
    ;

    _prodTypeSrc := _prodTypeList.findProdType( prodTypeSrc );

    if ( _prodTypeSrc = nil ) then // Create a production type.
      begin
        _prodTypeSrc := TProductionType.create( -1, prodTypeSrc, false, _smScenarioPtr^.simInput );
        _prodTypeSrc.populateDatabase( _smScenarioPtr^.simInput.database );
        _prodTypeList.append( _prodTypeSrc );
      end
    ;

    if ( _prodTypeSrc.zoneParams = nil ) then
      begin
        //We have a problem here...this should not happen...
      end
    ;

    //Create the movement control chart here
    subElement := Sdew.GetElementByName( Element, 'movement-control' );
    if ( subElement <> nil ) then
      begin
        Chart := TRelFunction.create();

        unitElement := Sdew.GetElementByName( subElement, 'units' );
        if ( unitElement <> nil ) then
          begin
            if ( Sdew.GetElementByName( unitElement, 'xdf:unit') <> nil ) then
              Chart.xUnits := chartUnitTypeFromDatabase( Sdew.GetElementContents( Sdew.GetElementByName( unitElement, 'xdf:unit')) )
            else if (Sdew.GetElementByName( unitElement, 'xdf:unitless') <> nil ) then
                Chart.xUnits := chartUnitTypeFromDatabase( 'unitless' )
              ;

            nextUnitElement := Sdew.GetNextElement(Element, unitElement);
            if ( nextUnitElement <> nil ) then
              begin
                if ( Sdew.GetElementByName( nextUnitElement, 'xdf:unit') <> nil ) then
                  Chart.yUnits := chartUnitTypeFromDatabase( Sdew.GetElementContents( Sdew.GetElementByName( nextUnitElement, 'xdf:unit')) )
                else if ( Sdew.GetElementByName( nextUnitElement, 'xdf:unitless') <> nil ) then
                  Chart.yUnits := chartUnitTypeFromDatabase( 'percent' )
                ;
              end
            ;
          end
        ;

        Chart.name := contactType  + ' Zone Movement control effect - ' + _prodTypeSrc.productionTypeDescr + ' - ' + ZoneName;
        if ( contactType = 'direct' ) then
          Chart.dbField := word( ZONMovementDirect )
        else
          Chart.dbField := word( ZONMovementIndirect )
        ;

        Chart.chartType := CTRel;

        ProcessPercentValues( subElement, Sdew, Chart );

        ChartID := _smScenarioPtr^.simInput.database.compareFunctions( smChartStr(TSMChart(Chart.dbField)), Chart, nFunctionsWithSameName );

        if ( ChartID = -1 ) then
          begin
            if( 0 < nFunctionsWithSameName ) then
              Chart.name := Chart.name + ' (' + intToStr( nFunctionsWithSameName + 1 ) + ')'
            ;
            _smScenarioPtr^.simInput.functionDictionary.insert( Chart.name, TFunctionDictionaryItem.create( Chart ) );
            Chart.populateDatabase( _smScenarioPtr^.simInput.database );
          end
        else
          begin
            Chart.destroy();
            Chart := TRelFunction (functionFromDB( _smScenarioPtr^.simInput.database, ChartId ) );
          end
        ;

        // Got a chart, now set the name in the zoneParams...
        _zoneParams := _prodTypeSrc.zoneParams;

        if ( _zoneParams.zonePtParamsList = nil ) then
          begin
            _zoneParams := TZoneParams.create( _smScenarioPtr^.simInput.database, _prodTypeSrc.productionTypeID, _prodTypeSrc.productionTypeDescr, _zoneList );
            _zoneParams.sim := _prodTypeSrc.sim;
            _prodTypeSrc.zoneParams :=  _zoneParams;
          end
        ;

         _zoneId := _zoneList.find( ZoneName ).id;
         _zoneParams.setChart( TSMChart(Chart.dbField), Chart, _zoneId );
         _zoneParams.prodTypeDescr := prodTypeSrc;

         if ( Chart.dbField = word( ZONMovementDirect ) ) then
           _zoneParams.zonePtParamsList.paramsForZone(_zoneId).useDirectMovementControl := true
         else if ( Chart.dbField = word( ZONMovementIndirect ) ) then
           _zoneParams.zonePtParamsList.paramsForZone(_zoneId).useIndirectMovementControl := true
          ;

         _zoneParams.populateDatabase( _smScenarioPtr^.simInput.database, _prodTypeSrc.productionTypeID  );
      end
    ;
  end
;


function ProcessContactSpreadModel( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    _smScenarioPtr:TSMScenarioPtr;
    _prodTypeList:TProductionTypeList;
    _ptpList:TProductionTypePairList;
    _ptp:TProductionTypePair;
    _prodTypeSrc, _prodTypeDest:TProductionType;
    prodTypeSrc, prodTypeDest:String;
    contactType:String;
    _contactType:TContactModel;
    subElement:Pointer;
    ssubElement:Pointer;
    nextUnitElement, unitElement:Pointer;
    statMethod:TChartFunction;
    probInfect:Double;
    Chart:TRelFunction;
    Index:Integer;
    contactRate:Double;
    ChartID:Integer;
    nFunctionsWithSameName: integer;
  begin
    result := nil;
    nFunctionsWithSameName := 0;

    _smScenarioPtr := TSMScenarioPtr(extData);
    _prodTypeList := _smScenarioPtr^.simInput.ptList;
    _ptpList := _smScenarioPtr^.simInput.ptpList;

    // Test to see if this is actually a zone contact model....
    if ( length( Sdew.GetElementAttribute( Element, 'zone')) > 0 ) then
      result := ProcessContactSpreadZoneModel( Element, Sdew, extData )
    else
      begin
        prodTypeSrc :=  Sdew.GetElementAttribute( Element, 'from-production-type' );
        prodTypeDest := Sdew.GetElementAttribute( Element, 'to-production-type');
        contactType :=  Sdew.GetElementAttribute( Element, 'contact-type' );

        //Get or create the production-type-pair here....
        _prodTypeSrc := _prodTypeList.findProdType( prodTypeSrc );
        _prodTypeDest := _prodTypeList.findProdType( prodTypeDest );

        if ( _prodTypeSrc = nil ) then // The production type should be created
          begin
            _prodTypeSrc := TProductionType.create( -1, prodTypeSrc, false, _smScenarioPtr^.simInput );
            _prodTypeSrc.populateDatabase( _smScenarioPtr^.simInput.database );
            _prodTypeList.append( _prodTypeSrc );
          end
        ;

        if ( _prodTypeDest = nil ) then // The production type should be created
          begin
            _prodTypeDest := TProductionType.create( -1, prodTypeDest, false, _smScenarioPtr^.simInput );
            _prodTypeDest.populateDatabase( _smScenarioPtr^.simInput.database );
            _prodTypeList.append( _prodTypeDest );
          end
        ;

        Index := _ptpList.pairPosition( _prodTypeSrc, _prodTypeDest );
        if ( Index <> -1 ) then
          _ptp := _ptpList.objects[Index] as TProductionTypePair
        else
          begin
            _ptp := TProductionTypePair.create( _prodTypeSrc, _prodTypeDest, _smScenarioPtr^.simInput );
            _smScenarioPtr^.simInput.database.makeProductionTypePair( _prodTypeSrc.productionTypeID, _prodTypeDest.productionTypeID );
            _ptp.populateDatabase( _smScenarioPtr^.simInput.database );
            _ptpList.Add( _ptp );
          end
        ;

         _contactType := TContactModel.create();
         _contactType.sim := _smScenarioPtr^.simInput;
         _contactType.fromProdType := prodTypeSrc;
         _contactType.fromProdTypeID := _prodTypeSrc.productionTypeID;
         _contactType.toProdType := prodTypeDest;
         _contactType.toProdTypeID := _prodTypeDest.productionTypeID;

        if ( contactType = 'direct' ) then
          begin
            _ptp.includeDirect := true;
            _contactType.contactType := TContactType( CMDirect );
            _ptp.direct := _contactType;
          end
        else
          begin
            _ptp.includeIndirect := true;
            _contactType.contactType := TContactType( CMIndirect );
            _ptp.indirect := _contactType;
          end
        ;

        subElement := Sdew.GetElementByName( Element, 'distance' );
        if ( subElement <> nil ) then
          begin
            statMethod := TChartFunction( GetStatModel( subElement, Sdew ) );
            statMethod.name := 'Dist ' + contactType + ' movement';

            if _contactType.contactType = CMDirect then
              statMethod.dbField := word( CMDistanceDirect )
            else
              statMethod.dbField := word( CMDistanceIndirect );

            ChartID := _smScenarioPtr^.simInput.database.compareFunctions( smChartStr(TSMChart(statMethod.dbField)), statMethod, nFunctionsWithSameName );

            if ( ChartID = -1 ) then
              begin
                if( 0 < nFunctionsWithSameName ) then
                  statMethod.name := statMethod.name + ' (' + intToStr( nFunctionsWithSameName + 1 ) + ')'
                ;
                _smScenarioPtr^.simInput.functionDictionary.insert( statMethod.name, TFunctionDictionaryItem.create( statMethod ) );
                statMethod.populateDatabase( _smScenarioPtr^.simInput.database );
              end
            else
              begin
                statMethod.destroy();
                statMethod := functionFromDB( _smScenarioPtr^.simInput.database, ChartId );
              end
            ;

            _contactType.distanceName := statMethod.name;
            //_contactType.pdfDistance := statMethod;
          end
        ;

        subElement := Sdew.GetElementByName( Element, 'delay' );
        if ( subElement <> nil ) then
          begin
            statMethod := TChartFunction( GetStatModel( subElement, Sdew ) );
            statMethod.name := 'Delay ' + contactType + ' shipping';

            if ( _contactType.contactType = CMDirect ) then
              statMethod.dbField := word( CMDelayDirect )
            else
              statMethod.dbField := word( CMDelayIndirect )
            ;

            ChartID := _smScenarioPtr^.simInput.database.compareFunctions( smChartStr(TSMChart(statMethod.dbField)), statMethod, nFunctionsWithSameName );

            if ( ChartID = -1 ) then
              begin
                if( 0 < nFunctionsWithSameName ) then
                  statMethod.name := statMethod.name + ' (' + intToStr( nFunctionsWithSameName + 1 ) + ')'
                ;
                _smScenarioPtr^.simInput.functionDictionary.insert( statMethod.name, TFunctionDictionaryItem.create( statMethod ) );
                statMethod.populateDatabase( _smScenarioPtr^.simInput.database );
              end
            else
              begin
                statMethod.destroy();
                statMethod := functionFromDB( _smScenarioPtr^.simInput.database, ChartId );
              end
            ;

            _contactType.delayName := statMethod.name;
          end
        ;

        subElement := Sdew.GetElementByName( Element, 'prob-infect' );
        if ( subElement <> nil ) then
          begin
            probInfect := usStrToFloat( Sdew.GetElementContents( subElement ) );
            _contactType.probInfect := probInfect;
          end
        ;

        subElement := Sdew.GetElementByName( Element, 'movement-control' );
        if ( subElement <> nil ) then
          begin
            Chart := TRelFunction.create();

            unitElement := Sdew.GetElementByName( subElement, 'units' );
            if ( unitElement <> nil ) then
              begin
                if ( Sdew.GetElementByName( unitElement, 'xdf:unit') <> nil ) then
                  Chart.xUnits := chartUnitTypeFromDatabase( Sdew.GetElementContents( Sdew.GetElementByName( unitElement, 'xdf:unit')) )
                else if (Sdew.GetElementByName( unitElement, 'xdf:unitless') <> nil ) then
                  Chart.xUnits := chartUnitTypeFromDatabase( 'unitless' )
                ;

                nextUnitElement := Sdew.GetNextElement(Element, unitElement);
                if ( nextUnitElement <> nil ) then
                  if ( Sdew.GetElementByName( nextUnitElement, 'xdf:unit') <> nil ) then
                    Chart.yUnits := chartUnitTypeFromDatabase( Sdew.GetElementContents( Sdew.GetElementByName( nextUnitElement, 'xdf:unit')) )
                  else if ( Sdew.GetElementByName( nextUnitElement, 'xdf:unitless') <> nil ) then
                    Chart.yUnits := chartUnitTypeFromDatabase( 'percent' )
                  ;
              end
            ;

            Chart.name := 'Movement control effect ' + contactType;
            if ( _contactType.contactType = CMDirect ) then
              Chart.dbField := word( CMMovementControlDirect )
            else
              Chart.dbField := word( CMMovementControlIndirect )
            ;

            Chart.chartType := CTRel;

            ProcessPercentValues( subElement, Sdew, Chart );


            ChartID := _smScenarioPtr^.simInput.database.compareFunctions( smChartStr(TSMChart(Chart.dbField)), Chart, nFunctionsWithSameName );

            if ( ChartID = -1 ) then
              begin
                if( 0 < nFunctionsWithSameName ) then
                  Chart.name := Chart.name + ' (' + intToStr( nFunctionsWithSameName + 1 ) + ')'
                ;
                _smScenarioPtr^.simInput.functionDictionary.insert( Chart.name, TFunctionDictionaryItem.create( Chart ) );
                Chart.populateDatabase( _smScenarioPtr^.simInput.database );
              end
            else
              begin
                Chart.destroy();
                Chart := TRelFunction ( functionFromDB( _smScenarioPtr^.simInput.database, ChartId ) );
              end
            ;

            _contactType.movementControlName := Chart.name;
          end
        ;

        subElement := Sdew.GetElementByName( Element, 'latent-units-can-infect' );
        if ( subElement <> nil ) then
          _contactType.latentCanInfect :=  StrToBool( Sdew.GetElementContents( subElement ) )
        ;

        subElement := Sdew.GetElementByName( Element, 'subclinical-units-can-infect' );
        if ( subElement <> nil ) then
          _contactType.subClinicalCanInfect :=  StrToBool( Sdew.GetElementContents( subElement ) );
        ;

        subElement := Sdew.GetElementByName( Element, 'movement-rate' );
        if ( subElement <> nil ) then
          begin
            ssubElement := Sdew.GetElementByName( subElement, 'value' );
            if ( ssubElement <> nil ) then
              begin
                contactRate := usStrToFloat( Sdew.GetElementContents( ssubElement ) );
                _contactType.meanContactRate := contactRate;
                _contactType.useFixedContactRate := false;
              end
            ;
          end
        ;

        subElement := Sdew.GetElementByName( Element, 'fixed-movement-rate' );
        if ( subElement <> nil ) then
          begin
            ssubElement := Sdew.GetElementByName( subElement, 'value' );
            if ( ssubElement <> nil ) then
              begin
                contactRate := usStrToFloat( Sdew.GetElementContents( ssubElement ) );
                _contactType.fixedContactRate := contactRate;
                _contactType.useFixedContactRate := true;
              end
            ;
          end
        ;

        _smScenarioPtr^.simInput.includeContactSpreadGlobal := true;
//        _smScenarioPtr^.simInput.updateDatabase();
        _contactType.populateDatabase( _smScenarioPtr^.simInput.database, false );
        _ptp.populateDatabase( _smScenarioPtr^.simInput.database, true  );
      end
    ;
  end
;


function ProcessGlobalControlParams ( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    _smScenarioPtr:TSMScenarioPtr;
    gcParms:TGlobalControlParams;
    Delay:Integer;
    PriorityOrder:String;
    Chart:TRelFunction;
    subElement:Pointer;
    ssubElement:Pointer;
    ChartID:Integer;
    nFunctionsWithSameName: integer;
  begin
    result := nil;
    nFunctionsWithSameName := 0;

    _smScenarioPtr := TSMScenarioPtr(extData);
    gcParms := _smScenarioPtr^.simInput.controlParams;

    //  Get Destruction global settings
    subElement := Sdew.GetElementByName( Element, 'destruction-program-delay' );
    if ( subElement <> nil ) then
      begin
        // Ignoring Units here....always days.
        ssubElement := Sdew.GetElementByName( SubElement, 'value' );
        if ( ssubElement <> nil ) then
          begin
            Delay := StrToInt( Sdew.GetElementContents( ssubElement ) );
            gcParms.destrProgramDelay := Delay;
          end
      else
        errorMessage := errorMessage + 'Warning: No destruction-program-delay value found in this xml file' + endl
      ;
      end
    else
      errorMessage := errorMessage + 'Warning: No destruction-program-delay element found in this xml file' + endl
    ;

    subElement := Sdew.GetElementByName( Element, 'destruction-priority-order');
    if ( subElement <> nil ) then
      begin
        PriorityOrder := Sdew.GetElementContents( subElement );
        gcParms.destrPriorityOrder := PriorityOrder;
      end
    else
      errorMessage := errorMessage + 'Warning: No destruction-priority-order element found in this xml file' + endl
    ;


    subElement := Sdew.GetElementByName( Element, 'destruction-capacity');
    if ( subElement <> nil ) then
      begin
        Chart := TRelFunction.create();

        Chart.xUnits :=  chartUnitTypeFromDatabase( 'days' );
        Chart.yUnits := chartUnitTypeFromDatabase( 'units per day' );
        Chart.name := 'Destruction cap';
        Chart.dbField := word( DestrCapacityGlobal );
        Chart.chartType := CTRel;

        ProcessValues( subElement, Sdew, Chart );

        ChartID := _smScenarioPtr^.simInput.database.compareFunctions( smChartStr(TSMChart(Chart.dbField)), Chart, nFunctionsWithSameName );

        if ( ChartID = -1 ) then
          begin
            if( 0 < nFunctionsWithSameName ) then
              Chart.name := Chart.name + ' (' + intToStr( nFunctionsWithSameName + 1 ) + ')'
            ;
            _smScenarioPtr^.simInput.functionDictionary.insert( Chart.name, TFunctionDictionaryItem.create( Chart ) );
            Chart.populateDatabase( _smScenarioPtr^.simInput.database );
          end
        else
          begin
            Chart.destroy();
            Chart := TRelFunction( functionFromDB( _smScenarioPtr^.simInput.database, ChartId ) );
          end
        ;

        gcParms.destrCapacityName := Chart.name;
      end
    else
      errorMessage := errorMessage + 'Warning: No destruction-capacity element found in this xml file' + endl
    ;

    // Get Vaccination global settings...
    subElement := Sdew.GetElementByName( Element, 'vaccination-program-delay' );
    if ( subElement <> nil ) then
      begin
        Delay := StrToInt( Sdew.GetElementContents( subElement ) );
        gcParms.vaccDetectedUnitsBeforeStart := Delay;
      end
    else
      errorMessage := errorMessage + 'Warning: No vaccination-program-delay element found in this xml file' + endl
    ;

    subElement := Sdew.GetElementByName( Element, 'vaccination-priority-order');
    if ( subElement <> nil ) then
      begin
        PriorityOrder := Sdew.GetElementContents( subElement );
        gcParms.vaccPriorityOrder := PriorityOrder;
      end
    else
      errorMessage := errorMessage + 'Warning: No vaccination-priority-order element found in this xml file' + endl
    ;

    subElement := Sdew.GetElementByName( Element, 'vaccination-capacity');
    if ( subElement <> nil ) then
      begin
        Chart := TRelFunction.create();

        Chart.xUnits :=  chartUnitTypeFromDatabase( 'days' );
        Chart.yUnits := chartUnitTypeFromDatabase( 'units per day' );
        Chart.name := 'Vaccine cap';
        Chart.dbField := word( VaccCapacityGlobal );
        Chart.chartType := CTRel;

        ProcessValues( subElement, Sdew, Chart );

        ChartID := _smScenarioPtr^.simInput.database.compareFunctions( smChartStr(TSMChart(Chart.dbField)), Chart, nFunctionsWithSameName );

        if ( ChartID = -1 ) then
          begin
            if( 0 < nFunctionsWithSameName ) then
              Chart.name := Chart.name + ' (' + intToStr( nFunctionsWithSameName + 1 ) + ')'
            ;
            _smScenarioPtr^.simInput.functionDictionary.insert( Chart.name, TFunctionDictionaryItem.create( Chart ) );
            Chart.populateDatabase( _smScenarioPtr^.simInput.database );
          end
        else
          begin
            Chart.destroy();
            Chart := TRelFunction( functionFromDB( _smScenarioPtr^.simInput.database, ChartId ) );
          end;

        gcParms.vaccCapacityName := Chart.name;
      end
    else
      errorMessage := errorMessage + 'Warning: No vaccination-capacity element found in this xml file' + endl
  ;

    gcParms.populateDatabase( _smScenarioPtr^.simInput.database, true );
  end
;


function ProcessDetectionFunctions( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    statMethod:TRelFunction;
    methodName: String;
    xUnits: String;
    yUnits: String;
    unitElement:Pointer;
    nextUnitElement:Pointer;
  begin
    statMethod := nil;
    xUnits := 'unknown';
    yUnits := 'unknown';

    methodName := Sdew.GetElementName( Element );

    unitElement := Sdew.GetElementByName( Element, 'units');
    if ( unitElement <> nil ) then
      begin
        if ( Sdew.GetElementByName( unitElement, 'xdf:unit') <> nil ) then
          xUnits := Sdew.GetElementContents( Sdew.GetElementByName( unitElement, 'xdf:unit'))
        else if (Sdew.GetElementByName( unitElement, 'xdf:unitless') <> nil ) then
          xUnits := 'unitless'
        ;

        nextUnitElement := Sdew.GetNextElement(Element, unitElement);
        if ( nextUnitElement <> nil ) then
          if ( Sdew.GetElementByName( nextUnitElement, 'xdf:unit') <> nil ) then
            yUnits := Sdew.GetElementContents( Sdew.GetElementByName( nextUnitElement, 'xdf:unit'))
          else if ( Sdew.GetElementByName( nextUnitElement, 'xdf:unitless') <> nil ) then
            yUnits := 'percent'
          ;

        statMethod := TRelFunction.create();
        statMethod.xUnits :=  chartUnitTypeFromDatabase( xUnits );
        statMethod.yUnits := chartUnitTypeFromDatabase( yUnits );
        statMethod.chartType := CTRel;

        if ( methodName = 'prob-report-vs-time-clinical' ) then
          begin
            statMethod.dbField := word( DetProbObsVsTimeClinical );
            statMethod.name := 'Prob observing clinical';
          end
        else if ( methodName = 'prob-report-vs-time-since-outbreak' ) then
            begin
              statMethod.dbField := word( DetProbReportVsFirstDetection );
              statMethod.name := 'Prob reporting';
            end
        else
          begin
            statMethod.dbField := word( XNoChart );
            statMethod.name := 'Unknown';
          end
        ;

        ProcessPercentValues( Element, Sdew, statMethod );

      end
    ;
    result := statMethod;
  end
;


function ProcessDetectionZoneModels( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    _smScenarioPtr: TSMScenarioPtr;
    _prodTypeList: TProductionTypeList;
    _prodType: TProductionType;
    prodType: String;
    prodTypeID: integer;
    zoneName: String;
    ret_val: TObject;
    _zoneList: TZoneList;
    _zoneParams: TZoneParams;
    _zoneId: Integer;
    detectionMultiplier: double;
  begin
    ret_val := nil;
    _smScenarioPtr := TSMScenarioPtr(extData);
    _zoneList := _smScenarioPtr^.simInput.zoneList;
    _prodTypeList := _smScenarioPtr^.simInput.ptList;

    detectionMultiplier := 1.0; // This default value may be changed below.

    prodType := Sdew.GetElementAttribute( Element, 'production-type' );
    prodTypeID := myStrToInt( Sdew.GetElementAttribute( Element, 'production-type-id' ), -1 );
    zoneName := Sdew.GetElementAttribute( Element, 'zone' );

    if ( Sdew.GetElementByName( Element, 'zone-prob-multiplier') <> nil ) then
      detectionMultiplier := usStrToFloat( Sdew.GetElementContents( Sdew.GetElementByName( Element, 'zone-prob-multiplier') ) )
    ;

    _prodType := _prodTypeList.findProdType( prodType );

    if( nil = _prodType ) then
      begin
        _prodType := TProductionType.create( prodTypeID, prodType, false, _smScenarioPtr^.simInput );
        _prodType.populateDatabase( _smScenarioPtr^.simInput.database, true );
        _prodTypeList.append( _prodType );
      end
    ;

    if( ( length(zoneName) > 0 ) AND (_zoneList <> nil ) )  then
      begin
        _zoneParams := _prodType.zoneParams;

        if ( _zoneParams <> nil ) then
          begin
            _zoneId := _zoneList.find( ZoneName ).id;

            //This function also sets the useDetectionMultiplier boolean in the TZoneProdTypeParams object...so no need to set it also, here.
            (_zoneParams.zonePtParamsList.value( _zoneId ) as TZoneProdTypeParams).detectionMultiplier := detectionMultiplier;

            _zoneParams.prodTypeDescr := prodType;

            _zoneParams.populateDatabase( _smScenarioPtr^.simInput.database, _prodType.productionTypeID  );
          end
        ;
      end
    ;

    result := ret_val;
  end
;


function ProcessDetectionModels( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    DetectionSections:CallbackArray;
    tempXMLReader: TXMLReader;
    DParms:TDetectionParams;
    _smScenarioPtr:TSMScenarioPtr;
    _prodTypeList:TProductionTypeList;
    _prodType:TProductionType;
    _fnDictionary:TFunctionDictionary;
    statMethod:TRelFunction;
    prodType:String;
    prodTypeID: integer;
    ret_val:TObject;
  begin
    ret_val := nil;
    _smScenarioPtr := TSMScenarioPtr(extData);
    _prodTypeList := _smScenarioPtr^.simInput.ptList;
    _fnDictionary := _smScenarioPtr^.simInput.functionDictionary;

    SetLength(DetectionSections, 2);
    DetectionSections[0].Tag := 'prob-report-vs-time-clinical';
    DetectionSections[0].Callback := ProcessDetectionFunctions;
    SetLength(DetectionSections[0].ObjectList,0);

    DetectionSections[1].Tag := 'prob-report-vs-time-since-outbreak';
    DetectionSections[1].Callback := ProcessDetectionFunctions;
    SetLength(DetectionSections[1].ObjectList,0);

    prodType := Sdew.GetElementAttribute( Element, 'production-type' );
    prodTypeID := myStrToInt( Sdew.GetElementAttribute( Element, 'production-type-id' ), -1 );

    if ( length(Sdew.GetElementAttribute( Element, 'zone' )) > 0 ) then
      ret_val := ProcessDetectionZoneModels( Element, Sdew, extData )
    else
      begin
        _prodType := _prodTypeList.findProdType( prodType );

        if( nil = _prodType ) then // The production type should be created
          begin
            _prodType := TProductionType.create( prodTypeID, prodType, false, _smScenarioPtr^.simInput );
            _prodType.populateDatabase( _smScenarioPtr^.simInput.database, true );
            _prodTypeList.append( _prodType );
          end
        ;

        tempXMLReader := TXMLReader.create( @DetectionSections, Sdew, Element, extData );
        tempXMLReader.Run();
        tempXMLReader.destroy();

        if ( (length( DetectionSections[0].ObjectList ) > 0 ) and (length( DetectionSections[1].ObjectList ) > 0 ) ) then
          begin
            statMethod := TRelFunction( DetectionSections[0].ObjectList[0] );
            statMethod.name := statMethod.name + ' ' + LowerCase( prodType );
            _fnDictionary.insert( statMethod.name, TFunctionDictionaryItem.create( statMethod ) );
            statMethod.populateDatabase( _smScenarioPtr^.simInput.database );

            statMethod := TRelFunction( DetectionSections[1].ObjectList[0] );
            statMethod.name := statMethod.name + ' ' + LowerCase( prodType );
            _fnDictionary.insert( statMethod.name, TFunctionDictionaryItem.create( statMethod ) );
            statMethod.populateDatabase( _smScenarioPtr^.simInput.database );

            if ( not Assigned( _prodType.detectionParams ) ) then
              begin
                DParms := TDetectionParams.create( prodType );
                DParms.sim := _smScenarioPtr^.simInput;
                _prodType.detectionParams := DParms;
              end
            else
              begin
                DParms := _prodType.detectionParams;
                DParms.prodTypeDescr := prodType;
              end
            ;

            DParms.relObsVsTimeClinicalName := TRelFunction(DetectionSections[0].ObjectList[0]).name;
            //DParms.relObsVsTimeClinical := TRelFunction(DetectionSections[0].ObjectList[0]);
            DParms.relReportVsFirstDetectionName := TRelFunction(DetectionSections[1].ObjectList[0]).name;
            //DParms.relReportVsFirstDetection := TRelFunction(DetectionSections[1].ObjectList[0]);
            DParms.populateDatabase( _smScenarioPtr^.simInput.database, _prodTypeList.findProdTypeID( prodType ) );

            _prodType.useDetection := true;
            _smScenarioPtr^.simInput.controlParams.useTracingGlobal := true;
            _smScenarioPtr^.simInput.controlParams.useDetectionGlobal := true;
//            _smScenarioPtr^.simInput.updateDatabase();
            _prodType.populateDatabase( _smScenarioPtr^.simInput.database );
            ret_val := DParms;
          end
        ;
      end
    ;

    result := ret_val;
  end
;


function ProcessVaccineModels( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    VaccineSections:CallbackArray;
    tempXMLReader: TXMLReader;
    _smScenarioPtr:TSMScenarioPtr;
    _prodTypeList:TProductionTypeList;
    _prodType:TProductionType;
    _fnDictionary:TFunctionDictionary;
    statMethod:TVaccineStatMethod;
    prodType:String;
    prodTypeID: integer;
    vaccParams:TVaccinationParams;
    vacDelay:TVaccineDelay;
    ret_val:TObject;

  begin
    ret_val := nil;
    _smScenarioPtr := TSMScenarioPtr(extData);
    _prodTypeList := _smScenarioPtr^.simInput.ptList;
    _fnDictionary := _smScenarioPtr^.simInput.functionDictionary;

    SetLength(VaccineSections, 2);
    VaccineSections[0].Tag := 'delay';
    VaccineSections[0].Callback := ProcessVaccineDelay;
    SetLength(VaccineSections[0].ObjectList,0);

    VaccineSections[1].Tag := 'immunity-period';
    VaccineSections[1].Callback := ProcessVaccineImmunityPeriod;
    SetLength(VaccineSections[1].ObjectList,0);

    prodType := Sdew.GetElementAttribute( Element, 'production-type' );
    prodTypeID := myStrToInt( Sdew.GetElementAttribute( Element, 'production-type-id' ), -1 );

    _prodType := _prodTypeList.findProdType( prodType );

    if( nil = _prodType ) then // The production type should be created
      begin
        _prodType := TProductionType.create( prodTypeID, prodType, false, _smScenarioPtr^.simInput );
        _prodType.populateDatabase( _smScenarioPtr^.simInput.database, true );
        _prodTypeList.append( _prodType );
      end
    ;

    tempXMLReader := TXMLReader.create( @VaccineSections, Sdew, Element, extData );
    tempXMLReader.Run();
    tempXMLReader.destroy();

    if ( ( length(VaccineSections[0].ObjectList) > 0) and  (length( VaccineSections[1].ObjectList) > 0 ) ) then
      begin
        if ( ( Assigned( VaccineSections[0].ObjectList[0] ) ) and ( Assigned( VaccineSections[1].ObjectList[0] ) ) ) then
          begin
            vacDelay := TVaccineDelay(VaccineSections[0].ObjectList[0]);
            statMethod := TVaccineStatMethod( VaccineSections[1].ObjectList[0] );
            statMethod.getStatMethod().name := prodType + ' Immune period vaccination';
            statMethod.getStatMethod().dbField := word( VacImmunePeriod );
            statMethod.getStatMethod().populateDatabase(_smScenarioPtr^.simInput.database );
            _fnDictionary.insert( statMethod.getStatMethod().name, TFunctionDictionaryItem.create( statMethod.getStatMethod() ) );
            vaccParams := TVaccinationParams.create();
            vaccParams.sim := _smScenarioPtr^.simInput;
            vaccParams.vaccImmunePdfName := statMethod.getStatMethod().name;
            vaccParams.prodTypeDescr := prodType;
            vaccParams.useVaccination := true;
            //vaccParams.pdfVaccImmunePeriod := statMethod.getStatMethod();
            _smScenarioPtr^.simInput.controlParams.useVaccGlobal := true;
            _smScenarioPtr^.simInput.controlParams.populateDatabase( _smScenarioPtr^.simInput.database, true );
            vaccParams.daysToImmunity := StrToInt(vacDelay.getDelay());
            vaccParams.populateDatabase(_smScenarioPtr^.simInput.database, _prodType.productionTypeID );
            ret_val := vaccParams;
            _prodType.vaccinationParams := vaccParams;
            _smScenarioPtr^.simInput.controlParams.useVaccGlobal := true;
//                _smScenarioPtr^.simInput.updateDatabase();
            _prodType.populateDatabase( _smScenarioPtr^.simInput.database );
          end
        ;
      end
    ;

    result := ret_val;
  end
;


function ProcessBasicZoneFocusModel ( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    _prodTypeList: TProductionTypeList;
    prodType: String;
    prodTypeID: integer;
    _prodType: TProductionType;
    _smScenarioPtr:TSMScenarioPtr;
  begin
    result := nil;

    _smScenarioPtr := TSMScenarioPtr(extData);
    _prodTypeList := _smScenarioPtr^.simInput.ptList;

    prodType :=  Sdew.GetElementAttribute( Element, 'production-type' );
    prodTypeID := myStrToInt( Sdew.GetElementAttribute( Element, 'production-type-id' ), -1 );

    _prodType := _prodTypeList.findProdType( prodType );

    if( nil = _prodType ) then // The production type should be created
      begin
        _prodType := TProductionType.create( prodTypeID, prodType, false, _smScenarioPtr^.simInput );
        _prodType.populateDatabase( _smScenarioPtr^.simInput.database, true );
        _prodTypeList.append( _prodType );
      end
    ;

    if ( _prodType.zoneParams <> nil ) then
      begin
        _prodType.zoneParams.detectionIsZoneTrigger := true;
        _prodType.zoneParams.prodTypeDescr := prodType;
        _prodType.zoneParams.populateDatabase( _smScenarioPtr^.simInput.database, _prodType.productionTypeID );
      end
    ;
  end
;


function ProcessTraceBackZoneFocusModel ( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    _prodTypeList: TProductionTypeList;
    prodType: String;
    prodTypeID: integer;
    contactType: String;
    _prodType: TProductionType;
    _smScenarioPtr:TSMScenarioPtr;
  begin
    result := nil;

    _smScenarioPtr := TSMScenarioPtr(extData);
    _prodTypeList := _smScenarioPtr^.simInput.ptList;

    prodType :=  Sdew.GetElementAttribute( Element, 'production-type' );
    prodTypeID := myStrToInt( Sdew.GetElementAttribute( Element, 'production-type-id' ), -1 );

    contactType :=  Sdew.GetElementAttribute( Element, 'contact-type' );

    _prodType := _prodTypeList.findProdType( prodType );

    if( nil = _prodType ) then // The production type should be created
      begin
        _prodType := TProductionType.create( prodTypeID, prodType, false, _smScenarioPtr^.simInput );
        _prodType.populateDatabase( _smScenarioPtr^.simInput.database, true );
        _prodTypeList.append( _prodType );
      end
    ;

    if ( _prodType.zoneParams <> nil ) then
      begin
        if ( contactType = 'direct' ) then
          _prodType.zoneParams.directTraceIsZoneTrigger := true
        else if ( contactType = 'indirect' ) then
            _prodType.zoneParams.indirectTraceIsZoneTrigger := true
        ;

        _prodType.zoneParams.prodTypeDescr := prodType;
        _prodType.zoneParams.populateDatabase( _smScenarioPtr^.simInput.database, _prodType.productionTypeID );
      end
    ;
  end
;



function ProcessZoneModels( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    ZoneName: String;
//    Level: Integer;
    Radius: String;
    _smScenarioPtr:TSMScenarioPtr;
    _zoneList: TZoneList;
    _newZone: TZone;
    zoneID: integer;
  begin
    result := nil;
    
    _smScenarioPtr := TSMScenarioPtr(extData);
    _zoneList := _smScenarioPtr^.simInput.zoneList;

    zoneID := myStrToInt( Sdew.GetElementAttribute( Element, 'zone-id' ), -1 );

    Radius := '-1.0';
    ZoneName := '';

    //NOTE:  Since the units used in the zone-model remain constant, they
    //       are not read in here.  If this changes in the future, remember to
    //       add the code here to read them in.
    //       Additionally:
    //         The Level stored in the XML is not stored in the current database
    //       schema.  Consequently, it, also, is not read in here.  If this changes
    //       then be sure to remember to add the code to read the level here.
    if ( Sdew.GetElementByName( Element, 'name') <> nil ) then
      begin
        ZoneName := Sdew.GetElementContents( Sdew.GetElementByName( Element, 'name'));

        if (  Sdew.GetElementByName( Element, 'radius') <> nil ) then
          if ( Sdew.GetElementByName( Sdew.GetElementByName( Element, 'radius'), 'value') <> nil ) then
            begin
              Radius := Sdew.GetElementContents( Sdew.GetElementByName( Sdew.GetElementByName( Element, 'radius'), 'value'));

              if ( myStrToFloat( Radius ) > 0.0 ) then
                begin
                  if ( _zoneList.find( ZoneName ) = nil ) then
                    begin
                      _newZone := TZone.create( zoneID, ZoneName, myStrToFloat(Radius), _smScenarioPtr^.simInput );
                      _newZone.populateDatabase( _smScenarioPtr^.simInput.database, true );
                      _zoneList.append( _newZone );
                      _smScenarioPtr^.simInput.controlParams.useZonesGlobal := true;
                      _smScenarioPtr^.simInput.populateDatabase();
                    end
                  ;
                end
              ;
            end
          ;
      end
    ;
  end
;


function ProcessDiseaseModels( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    periodCallbacks: CallbackArray;
    tempXMLReader: TXMLReader;
    diseaseModel: TDiseaseModel;

    immunityPeriod: TImmunityPeriod;
    latentPeriod: TLatentPeriod;
    infectiousSubClinicalPeriod: TInfectiousSubClinicalPeriod;
    infectiousClinicalPeriod: TInfectiousClinicalPeriod;

    subElement:Pointer;
    nextUnitElement, unitElement: Pointer;
    chartID: integer;
    prevalenceChart: TRelFunction;

    prodTypeDescr: String;
    prodTypeID: integer;
    _smScenarioPtr: TSMScenarioPtr;
    _prodTypeList: TProductionTypeList;
    _prodType: TProductionType;
    _fnDictionary: TFunctionDictionary;
    nFunctionsWithSameName: integer;
  begin
    diseaseModel := nil;
    immunityPeriod := nil;
    latentPeriod := nil;
    infectiousSubClinicalPeriod := nil;
    infectiousClinicalPeriod := nil;

    nFunctionsWithSameName := 0;

    _smScenarioPtr := TSMScenarioPtr(extData);
    _prodTypeList := _smScenarioPtr^.simInput.ptList;
    _fnDictionary := _smScenarioPtr^.simInput.functionDictionary;

    prodTypeDescr := Sdew.GetElementAttribute( Element, 'production-type' );
    prodTypeID := myStrToInt( Sdew.GetElementAttribute( Element, 'production-type-id' ), -1 );

    _prodType := _prodTypeList.findProdType( prodTypeDescr );

    if( nil = _prodType ) then
      begin
        _prodType := TProductionType.create( prodTypeID, prodTypeDescr, true, _smScenarioPtr^.simInput );
        _prodType.populateDatabase( _smScenarioPtr^.simInput.database, true );
        _prodTypeList.append( _prodType );
      end
    ;

    // Read the prevalence chart, if present
    //--------------------------------------
    subElement := Sdew.GetElementByName( Element, 'prevalence' );
    if ( subElement <> nil ) then
      begin
        prevalenceChart := TRelFunction.create();

        unitElement := Sdew.GetElementByName( subElement, 'units' );
        if ( unitElement <> nil ) then
          begin
            if ( Sdew.GetElementByName( unitElement, 'xdf:unit') <> nil ) then
              prevalenceChart.xUnits := chartUnitTypeFromDatabase( Sdew.GetElementContents( Sdew.GetElementByName( unitElement, 'xdf:unit')) )
            else if (Sdew.GetElementByName( unitElement, 'xdf:unitless') <> nil ) then
              prevalenceChart.xUnits := chartUnitTypeFromDatabase( 'unitless' )
            ;

            nextUnitElement := Sdew.GetNextElement(Element, unitElement);

            if ( nextUnitElement <> nil ) then
              begin
                if ( Sdew.GetElementByName( nextUnitElement, 'xdf:unit') <> nil ) then
                  prevalenceChart.yUnits := chartUnitTypeFromDatabase( Sdew.GetElementContents( Sdew.GetElementByName( nextUnitElement, 'xdf:unit')) )
                else if ( Sdew.GetElementByName( nextUnitElement, 'xdf:unitless') <> nil ) then
                  prevalenceChart.yUnits := chartUnitTypeFromDatabase( 'percent' )
                ;
              end
            ;
          end
        ;

        prevalenceChart.name := prodTypeDescr + ' prevalence';
        prevalenceChart.dbField := word( DPrevalence );
        prevalenceChart.chartType := CTRel;

        ProcessPercentValues( subElement, Sdew, prevalenceChart );

        ChartID := _smScenarioPtr^.simInput.database.compareFunctions( smChartStr(TSMChart(prevalenceChart.dbField)), prevalenceChart, nFunctionsWithSameName );

        if ( ChartID = -1 ) then
          begin
            if( 0 < nFunctionsWithSameName ) then
              prevalenceChart.name := prevalenceChart.name + ' (' + intToStr( nFunctionsWithSameName + 1 ) + ')'
            ;
            _smScenarioPtr^.simInput.functionDictionary.insert( prevalenceChart.name, TFunctionDictionaryItem.create( prevalenceChart ) );
            prevalenceChart.populateDatabase( _smScenarioPtr^.simInput.database );
          end
        else
          begin
            prevalenceChart.destroy();
            prevalenceChart := TRelFunction( functionFromDB( _smScenarioPtr^.simInput.database, ChartId ) );
          end
         ;

        _prodType.prevalenceName := prevalenceChart.name;
        _smScenarioPtr^.simInput.useWithinHerdPrevalence := true;
      end
    else
      _prodType.prevalenceName := ''
    ;


    // Read the disease states
    //------------------------
    SetLength(periodCallbacks, 4);

    periodCallbacks[0].Tag := 'latent-period';
    periodCallbacks[0].Callback := ProcessPeriodModels;
    SetLength(periodCallbacks[0].ObjectList,0);

    periodCallbacks[1].Tag := 'infectious-subclinical-period';
    periodCallbacks[1].Callback := ProcessPeriodModels;
    SetLength(periodCallbacks[1].ObjectList,0);

    periodCallbacks[2].Tag := 'infectious-clinical-period';
    periodCallbacks[2].Callback := ProcessPeriodModels;
    SetLength(periodCallbacks[2].ObjectList,0);

    periodCallbacks[3].Tag := 'immunity-period';
    periodCallbacks[3].Callback := ProcessPeriodModels;
    SetLength(periodCallbacks[3].ObjectList,0);

    tempXMLReader := TXMLReader.create( @periodCallbacks, Sdew, Element, nil );
    tempXMLReader.Run();
    tempXMLReader.destroy();

    if ( length(periodCallbacks[0].ObjectList) > 0 ) then
      latentPeriod := TLatentPeriod(periodCallbacks[0].ObjectList[0])
    ;
    if ( length(periodCallbacks[1].ObjectList) > 0 ) then
      infectiousSubClinicalPeriod := TInfectiousSubClinicalPeriod(periodCallbacks[1].ObjectList[0])
    ;
    if ( length(periodCallbacks[2].ObjectList) > 0 ) then
      infectiousClinicalPeriod := TInfectiousClinicalPeriod(periodCallbacks[2].ObjectList[0])
    ;
    if ( length(periodCallbacks[3].ObjectList) > 0 ) then
      immunityPeriod := TImmunityPeriod(periodCallbacks[3].ObjectList[0])
    ;

    if ( Assigned(latentPeriod) AND Assigned(infectiousSubClinicalPeriod) AND Assigned(infectiousClinicalPeriod) AND Assigned(immunityPeriod) ) then
      begin
        immunityPeriod.GetStatMethod().name := prodTypeDescr + ' immunity period';
        dbcout2( immunityPeriod.GetStatMethod().name, UseDebug );
        immunityPeriod.GetStatMethod().dbField := word( DImmune );
        immunityPeriod.GetStatMethod().populateDatabase(_smScenarioPtr^.simInput.database );
        _fnDictionary.insert( immunityPeriod.GetStatMethod().name, TFunctionDictionaryItem.create( immunityPeriod.GetStatMethod()) );

        latentPeriod.GetStatMethod().name := prodTypeDescr + ' latent period';
        dbcout2( latentPeriod.GetStatMethod().name, UseDebug );
        latentPeriod.GetStatMethod().dbField := word( DLatent );
        latentPeriod.GetStatMethod().populateDatabase(_smScenarioPtr^.simInput.database );
        _fnDictionary.insert( latentPeriod.GetStatMethod().name, TFunctionDictionaryItem.create( latentPeriod.GetStatMethod()) );

        infectiousSubClinicalPeriod.GetStatMethod().name := prodTypeDescr + ' subclinical period';
        dbcout2( infectiousSubClinicalPeriod.GetStatMethod().name, UseDebug );
        infectiousSubClinicalPeriod.GetStatMethod().dbField := word( DSubclinical );
        infectiousSubClinicalPeriod.GetStatMethod().populateDatabase(_smScenarioPtr^.simInput.database );
        _fnDictionary.insert( infectiousSubClinicalPeriod.GetStatMethod().name, TFunctionDictionaryItem.create( infectiousSubClinicalPeriod.GetStatMethod()) );

        infectiousClinicalPeriod.GetStatMethod().name := prodTypeDescr + ' clinical period';
        dbcout2( infectiousClinicalPeriod.GetStatMethod().name, UseDebug );
        infectiousClinicalPeriod.GetStatMethod().dbField := word( DClinical );
        infectiousClinicalPeriod.GetStatMethod().populateDatabase(_smScenarioPtr^.simInput.database );
        _fnDictionary.insert( infectiousClinicalPeriod.GetStatMethod().name, TFunctionDictionaryItem.create( infectiousClinicalPeriod.GetStatMethod()) );

        //diseaseModel := TDiseaseModel.create( immunityPeriod, latentPeriod, infectiousSubClinicalPeriod, infectiousClinicalPeriod, prodTypeDescr );

        _prodType.latentName := latentPeriod.GetStatMethod().name;
        _prodType.subclinicalName := infectiousSubClinicalPeriod.GetStatMethod().name;
        _prodType.clinicalName := infectiousClinicalPeriod.GetStatMethod().name;
        _prodType.immuneName := immunityPeriod.GetStatMethod().name;

        _prodType.populateDatabase( _smScenarioPtr^.simInput.database );
      end
    else
      Application.MessageBox(PChar('This disease model, ' + prodTypeDescr = ', does not contain all the necessary period elements'), 'ERROR')
    ;

    // Is diseaseModel nil?  Why is it being returned?
    result := diseaseModel;
  end
;


function ProcessModels( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    ModelCallbacks : CallbackArray;
    tempXMLReader: TXMLReader;
    I:Integer;
  begin
    SetLength( ModelCallbacks, constNumXMLProcs + 1 );
    for I := 0 to constNumXMLProcs do
      begin
        ModelCallbacks[I].Tag := constXMLProcs[I].name;
        ModelCallbacks[I].Callback := constXMLProcs[I].xFunc;
        SetLength(ModelCallbacks[I].ObjectList,0);
      end;

    tempXMLReader := TXMLReader.create( @ModelCallbacks, Sdew, Element, extData );
    tempXMLReader.Run();
    tempXMLReader.destroy();

    result := nil;
  end
;


function ProcessParamsFile( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    elementName:String;
    _smScenarioPtr:TSMScenarioPtr;
  begin
    _smScenarioPtr := TSMScenarioPtr(extData);

    elementName := Sdew.GetElementName( Element );

    if ( elementName = 'description' ) then
      begin
        _smScenarioPtr.simInput.scenarioDescr := decodeXml( Sdew.GetElementContents( Element ) );
        _smScenarioPtr.simInput.populateDatabase();
      end
    else
      if ( elementName = 'num-days' ) then
        begin
          _smScenarioPtr.simInput.simDays := StrToInt( Sdew.GetElementContents( Element ) );
          _smScenarioPtr.simInput.populateDatabase();
        end
      else
        if ( elementName = 'num-runs' ) then
          begin
            _smScenarioPtr.simInput.simIterations := StrToInt( Sdew.GetElementContents( Element ) );
            _smScenarioPtr.simInput.populateDatabase();
          end;

    result := nil;
  end
;


constructor TXMLConvert.create( HerdsFilename:String; ScenarioFilename:String; smScenario:TSMScenarioPtr );
  begin
    inherited create();
    errorMessage := '';
    
    _smScenario := smScenario;
    _sfilename := ScenarioFilename;
    _hfilename := HerdsFilename;
  end
;






  ///////////////////////////////////////////////////////////////////////////////
 //  This function reads a Herd XML file and returns a hlist for use elsewhere
/////////////////////////////////////////////////////////////////////////
function TXMLConvert.ReadHerdXML( _hList: THerdList ):bool;
  var
       XMLReader: TXMLReader;
       MyCallbacks: CallbackArray;
       I: Integer;
       J: Integer;
       _h: THerd;
       _xmlHerd: TxmlHerd;
       frmProgress:TFormProgress;
       ret_val: boolean;
  begin
       ret_val := false;

       if ( Assigned ( _hList ) ) then
         begin
           ret_val := true;

       SetLength( MyCallbacks, 1);
       MyCallbacks[0].Tag := 'herd';
       MyCallbacks[0].Callback := ProcessHerd;
       SetLength(MyCallbacks[0].ObjectList, 0);

       frmProgress := TFormProgress.create( application.MainForm, PRSingleBar , false );
       frmProgress.show();
       frmProgress.setMessage( tr( 'Reading units...' ) );

       XMLReader := TXMLReader.create(@MyCallbacks, _hfilename, nil, frmProgress );

       XMLReader.Run();
       XMLReader.destroy();
       frmProgress.setPrimary( 0 );
       frmProgress.setMessage( tr( 'Building units...' ) );

       if ( Assigned( _hList ) ) then
         begin
          for I := 0 to length( MyCallbacks ) - 1 do
            begin
               frmProgress.setPrimary( 0 );
               for J := 0 to length( MyCallbacks[I].ObjectList ) - 1 do
                 begin
                   frmProgress.setPrimary( (((J+1) * 100) div length( MyCallbacks[I].ObjectList )) );
                   _xmlHerd := TxmlHerd(MyCallbacks[I].ObjectList[J]);
                   frmProgress.setMessage( ansiReplaceStr( tr( 'Building unit with ID xyz' ), 'xyz', _xmlHerd.GetId() ) );
                   _h := THerd.create( _hList );
                   //_h.setProdType( _smScenario^.simInput.findProdType( _xmlHerd.GetProdType() ));
                   _h.prodTypeName := _xmlHerd.GetProdType();
                   _h.lat := _xmlHerd.GeTLocation().GetLatitude();
                   _h.lon := _xmlHerd.GeTLocation().GetLongitude();
                   _h.initialSize := _xmlHerd.GetSize();
                   _h.initialStatus := transitionStateFromString( _xmlHerd.GetStatus() );
                   _h.daysLeftInInitialState := -1;
                   _hList.append( _h );
                   //_h.simParams := _smScenario.simInput;

                   dbcout( TxmlHerd(MyCallbacks[I].ObjectList[J]).WriteContents(), true );
                 end;
               //_hList.populateDatabase( _smScenario^.simInput.database );
             end;
         end;
       frmProgress.destroy();

       end;

       result := ret_val;
  end
;


  ///////////////////////////////////////////////////////////////////////////////
 //  This function begins the processing of the disease-model XML file
/////////////////////////////////////////////////////////////////////////
procedure TXMLConvert.ConvertXMLToDatabase( err: pstring = nil );
  var
       XMLReader: TXMLReader;
       MyCallbacks: CallbackArray;
       I: Integer;
       J: Integer;
       _h: THerd;
       _hList: THerdList;
       _xmlHerd: TxmlHerd;
       frmProgress:TFormProgress;
       pt: TProductionType;
       destrReasonStr: String;
       index:Integer;
       listlength: Integer;
       tempStr:String;
  begin
       if ( length(_sfilename ) > 0 ) then
         begin
           _internalDestructionPriorityList := TQStringLongIntMap.create();
//'basic,direct,indirect,ring'  As a default order...
           _internalDestructionPriorityList.insert('basic', 1000);
           _internalDestructionPriorityList.insert('direct', 1001);
           _internalDestructionPriorityList.insert('indirect', 1002);
           _internalDestructionPriorityList.insert('ring', 1003);

           SetLength( MyCallbacks, 2);
           MyCallbacks[0].Tag := '*';
           MyCallbacks[0].Callback := ProcessParamsFile;
           SetLength(MyCallbacks[0].ObjectList, 0);

           MyCallbacks[1].Tag := 'models';
           MyCallbacks[1].Callback := ProcessModels;
           SetLength(MyCallbacks[1].ObjectList, 0);

           frmProgress := TFormProgress.create( application.MainForm, PRSingleBar , false );
           frmProgress.setMessage( tr( 'Reading scenario parameters...' ) );
           frmProgress.show();

           XMLReader := TXMLReader.create(@MyCallbacks, _sfilename, _smScenario, frmProgress );
           XMLReader.Run();

           XMLReader.destroy();
           frmProgress.Destroy();

           listlength := 4;

           while( listlength > 0 ) do
             begin
               tempStr := '';
               for index := 0 to listlength - 1 do
                 begin
                  if ( index = 0 ) then
                    tempStr := _internalDestructionPriorityList.keyAtIndex( index )
                  else
                    begin
                      if ( _internalDestructionPriorityList.itemAtIndex( index ) < _internalDestructionPriorityList.value( tempStr ) ) then
                        tempStr := _internalDestructionPriorityList.keyAtIndex( index );
                    end;
                 end;

               destrReasonStr := destrReasonStr + tempStr;
               if ( listlength > 1) then
                 destrReasonStr := destrReasonStr + ',';

               _internalDestructionPriorityList.remove( tempStr );
               listlength := listlength - 1;
             end;

           _smScenario^.simInput.controlParams.destrReasonOrder := destrReasonStr;
           _smScenario^.simInput.populateDatabase();
         end;

       SetLength( MyCallbacks, 1);
       MyCallbacks[0].Tag := 'herd';
       MyCallbacks[0].Callback := ProcessHerd;
       SetLength(MyCallbacks[0].ObjectList, 0);

       frmProgress := TFormProgress.create( application.MainForm, PRSingleBar , false );
       frmProgress.show();
       frmProgress.setMessage( tr( 'Reading units...' ) );

       XMLReader := TXMLReader.create(@MyCallbacks, _hfilename, _smScenario, frmProgress );

       XMLReader.Run();
       XMLReader.destroy();
       frmProgress.setPrimary( 0 );
       frmProgress.setMessage( tr( 'Building units...' ) );
       _hList := _smScenario^.herdList;

       if ( _hList <> nil ) then
         begin
          for I := 0 to length( MyCallbacks ) - 1 do
            begin
               frmProgress.setPrimary( 0 );
               for J := 0 to length( MyCallbacks[I].ObjectList ) - 1 do
                 begin
                   frmProgress.setPrimary( (((J+1) * 100) div length( MyCallbacks[I].ObjectList )) );
                   _xmlHerd := TxmlHerd(MyCallbacks[I].ObjectList[J]);
                   frmProgress.setMessage( ansiReplaceStr( tr( 'Building unit with ID xyz' ), 'xyz', _xmlHerd.GetId() ) );
                   _h := THerd.create( _hList );

                   pt := _smScenario^.simInput.findProdType( _xmlHerd.GetProdType() );

                   if( nil = pt ) then
                     begin
                       pt := TProductionType.create( -1, _xmlHerd.GetProdType(), false, _smScenario^.simInput );
                       pt.populateDatabase( _smScenario^.simInput.database );
                       _smScenario^.simInput.ptList.Append( pt );
                     end
                   ;

                   _h.setProdType( pt );
                   _h.lat := _xmlHerd.GeTLocation().GetLatitude();
                   _h.lon := _xmlHerd.GeTLocation().GetLongitude();
                   _h.initialSize := _xmlHerd.GetSize();
                   _h.initialStatus := transitionStateFromString( _xmlHerd.GetStatus() );
                   _h.daysLeftInInitialState := -1;
                   _hList.append( _h );
                   _h.simParams := _smScenario.simInput;

                   dbcout( TxmlHerd(MyCallbacks[I].ObjectList[J]).WriteContents(), true );
                 end;
               dbcout( 'Populating database with herd list', true );
               _hList.populateDatabase( _smScenario^.simInput.database );
               dbcout( 'Recounting Units', true );
               _smScenario^.simInput.ptList.recountUnits( _hList );
             end;
         end;
       frmProgress.destroy();

       if( nil <> err ) then
         err^ := err^ + errorMessage
       ;
  end
;

end.
