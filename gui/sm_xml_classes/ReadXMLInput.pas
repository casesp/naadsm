unit ReadXMLInput;
(*
ReadXMLInput.pas
-----------------
Begin: 2006/09/28
Last revision: $Date: 2010-02-12 18:37:08 $ $Author: areeves $
Version number: $Revision: 1.45.6.1 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Shaun Case <Shaun.Case@colostate.edu>
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2006 - 2009 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

{* TODO 1 -cXML Input -oscase: Add more robust error checking with exceptions thrown when XML is invalid
*}

{$INCLUDE Defs.inc}

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
    FunctionPointers,
    StringConsts,

    Sdew,

    XMLReader,
    Loc,
    xmlHerd,
    TypInfo,
    ChartFunction,
    ProbDensityFunctions,
    Points,
    SMScenario,
    SMSimulationInput,    
    SMDatabase,
    FunctionEnums,
    ProductionType,
    ProductionTypeList,
    FunctionDictionary,
    Herd,
    StatusEnums,
    NAADSMLibraryTypes,
    VaccinationParams,
    RelFunction,
    DetectionParams,
    DestructionParams,
    TracingParams,
    GlobalControlParams,
    ProductionTypePair,
    ProductionTypePairList,
    ContactSpreadParams,
    AirborneSpreadParams,
    LocalAreaSpreadParams,
    RingVaccParams,
    Zone,
    ProdTypeZoneParams
  ;

  type Loc = record
    latitude: double;
    longitude: double;
  end;


  ////////////////////////////////////////////////////////////////////////////////
  //  Forward declarations of functions needed by the XML parsing routines.
  //  These are callback functions, which are called by the XMLReader object as it
  //  parses an XML file.
  //////////////////////////////////////////////////////////////////////

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
  function ProcessLocalAreaModel          ( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  function ProcessRingVaccineModel        ( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  function ProcessBasicZoneFocusModel     ( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  function ProcessTraceBackZoneFocusModel ( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  function ProcessContactRecorderModel    ( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  function ProcessTraceModel              ( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  function ProcessTraceExamModel          ( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  function ProcessTestModel               ( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  function ProcessTraceDestructionModel   ( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;

// FIX ME: This document block originally went with one of the PDF importers.
// Figure out where it should go now.
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





  type TSMScenarioPtr = ^TSMScenario;
  type TSMInputPtr = TSMSimulationInputPtr;


  type TXmlConvert = class( TObject )
    protected
      _smScenario:TSMScenarioPtr;
      _smInput: TSMInputPtr;
      _sfilename:String;
      _hfilename:String;

      _populateScenario: boolean;

      {* Function pointer to TFormProgress.setMessage() }
      _fnProgressMessage: TObjFnVoid1String;

      {* Function pointer to TFormProgress.setPrimary() }
      _fnProgressSet: TObjFnBool1Int;

      procedure convertScenario( err: pstring );
      procedure convertHerdList(  err: pstring );

    public
      constructor create( HerdsFilename: String; ScenarioFilename: String; smScenario:TSMScenarioPtr ); overload;
      constructor create( HerdsFilename: String; ScenarioFilename: String; smInput:TSMInputPtr ); overload;
      destructor destroy(); override;
      
      procedure ConvertXmlToDatabase( err: pstring = nil );
      function ReadHerdXml( _hList:THerdList = nil; err: pstring = nil ): boolean;

      property progressMessageFunction: TObjFnVoid1String write _fnProgressMessage;
      property progressValueFunction: TObjFnBool1Int write _fnProgressSet;
    end
  ;


  type TValueUnitPair = class( TObject )
    protected
      _value: string;
      _units: string;
    public
      constructor create( value: string; units: string );
      destructor destroy(); override;

      function getValue():String;
      function getUnits():String;
    end
  ;

  type TParamsTag = class( TObject )
    protected
      _name: string;
      _element: Pointer;
    public
      constructor create( name:string; element: Pointer );
      destructor destroy(); override;

      function getName() :string;
      function getElement() :Pointer;
    end
  ;

implementation
  uses
    StrUtils,

    QLists,

    MyStrUtils,
    DebugWindow,
    I88n,
    
    ModelDatabase

    {$IFNDEF CONSOLEAPP}
    ,
    FormProgress,
    FormMain
    {$ENDIF}
  ;

  const DBSHOWMSG: Boolean = false; // Set to true to enable debugging for this unit.

  var
    errorMessage: string;
    _internalDestructionPriorityList: TQStringLongIntMap;
    _destructionPriorityList: TQStringList;
    _internalVaccinationPriorityList: TQStringLongIntMap;

  type xmlProcessFunc = function( Element: Pointer; Sdew: TSdew; extData:Pointer ): TObject;

  type xmlProcessEquates = record
    name:String;
    xFunc: xmlProcessFunc;
  end;

  // Names in this array must exactly match those specified in the XML schema.
  const constNumXMLProcs = 19;
  const constXMLProcs:  array[ 0..constNumXMLProcs - 1 ] of xmlProcessEquates =
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
    ( name: 'local-area-spread-model';      xFunc: ProcessLocalAreaModel ),
    ( name: 'ring-vaccination-model';       xFunc: ProcessRingVaccineModel ),
    ( name: 'basic-zone-focus-model';       xFunc: ProcessBasicZoneFocusModel ),
    ( name: 'trace-back-zone-focus-model';  xFunc: ProcessTraceBackZoneFocusModel ),
    ( name: 'contact-recorder-model';       xFunc: ProcessContactRecorderModel ),
    ( name: 'trace-model';                  xFunc: ProcessTraceModel ),
    ( name: 'trace-exam-model';             xFunc: ProcessTraceExamModel ),
    ( name: 'test-model';                   xFunc: ProcessTestModel ),
    ( name: 'trace-destruction-model';      xFunc: ProcessTraceDestructionModel )
  );


constructor TValueUnitPair.create( value: string; units: string );
  begin
    inherited create();
    
    _value := value;
    _units := units;
  end
;

destructor TValueUnitPair.destroy();
  begin
    inherited destroy();
  end
;


function TValueUnitPair.getValue():String;
  begin
    result := _value;
  end
;


function TValueUnitPair.getUnits():String;
  begin
    result := _units;
  end
;

constructor TParamsTag.create( name:string; element: Pointer );
  begin
    _name := name;
    _element := element;
  end
;

destructor TParamsTag.destroy();
  begin
    inherited destroy();
  end
;

function TParamsTag.getName() :string;
  begin
    result := _name
  end
;

function TParamsTag.getElement() :Pointer;
  begin
    result := _element;
  end
;

///////////////////////////////////////////////////////////////////////////////
//  These two functions are the callback functions used when loading a herd
//  XML file. 
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
    end
  ;


  function ProcessHerd( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
    var
      id: String;
      productionType: String;
      location: TLoc;
      size: Integer;
      status: String;
      daysInInitialState, daysLeftInInitialState: integer;

      tempXMLReader: TXMLReader;
      locationCallbacks : CallbackArray;
      Herd: TxmlHerd;
      e: pointer;
    begin
      SetLength(locationCallbacks, 1);
      locationCallbacks[0].Tag := 'location';
      locationCallbacks[0].Callback := ProcessHerdLocation;
      SetLength(locationCallbacks[0].ObjectList,0);

      tempXMLReader := TXMLReader.create( @locationCallbacks, Sdew, Element, @location );
      tempXMLReader.Run();
      tempXMLReader.free();

      id := Sdew.GetElementContents(Sdew.GetElementByName(Element, PChar('id')));
      productionType := Sdew.GetElementContents(Sdew.GetElementByName(Element, PChar('production-type')));
      size := StrToInt(Sdew.GetElementContents(Sdew.GetElementByName(Element, PChar('size'))));
      status := Sdew.GetElementContents(Sdew.GetElementByName(Element, PChar('status')));

      e := Sdew.GetElementByName( Element, PChar( 'days-in-status' ) );
      if( nil <> e ) then
        daysInInitialState := strToInt( Sdew.GetElementContents( e ) )
      else
        daysInInitialState := -1
      ;

      e := Sdew.GetElementByName( Element, PChar( 'days-left-in-status' ) );
      if( nil <> e ) then
        daysLeftInInitialState := strToInt( Sdew.GetElementContents( e ) )
      else
        daysLeftInInitialState := -1
      ;

      {*** Instantiate a TxmlHerd object here and load the above values to save to the database ***}
      if ( length(locationCallbacks[0].ObjectList) > 0 ) then
        begin
          Herd := TxmlHerd.create(
            id,
            productionType,
            size,
            TLoc(locationCallbacks[0].ObjectList[0]),
            status,
            daysInInitialState,
            daysLeftInInitialState
          );
        end
      else
        begin
          Herd := nil;
          {$IFNDEF CONSOLEAPP}
            msgOK(
              tr( 'This herd file is not valid.  A herd is missing a location.' ),
              tr( 'An error occurred' ),
              IMGCritical,
              frmMain
            );
          {$ENDIF}
        end
      ;

      result := Herd;
    end
  ;



function ProcessVaccineDelay( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    delay:String;
    units:String;
    VDelay: TValueUnitPair;
  begin
    delay := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'value' ));
    units := Sdew.GetElementContents( Sdew.GetElementByName( Sdew.GetElementByName(Element, 'units'), 'xdf:unit') );

    VDelay := TValueUnitPair.create( delay, units );
    result := VDelay;
  end
;




function ProcessBasicDestruction( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    _smInput:TSMInputPtr;
    _prodTypeList:TProductionTypeList;
    _prodType:TProductionType;
    prodType:String;
    prodTypeID: integer;
    Priority:String;
    DParms:TDestructionParams;
  begin
    result := nil;

    _smInput := TSMInputPtr( extData );
    _prodTypeList := _smInput^.ptList;

    prodType := Sdew.GetElementAttribute( Element, 'production-type' );
    prodTypeID := myStrToInt( Sdew.GetElementAttribute( Element, 'production-type-id' ), -1 );

    _prodType := _prodTypeList.findProdType( prodType );

    if( nil = _prodType ) then
      begin
        _prodType := TProductionType.create( prodTypeID, prodType, _smInput^ );
        _prodTypeList.append( _prodType );
      end
    ;

    if ( Assigned(_prodType.destructionParams) ) then
      DParms := _prodType.destructionParams
    else
      begin
        DParms := TDestructionParams.create( _smInput^, prodType );
        _prodType.destructionParams := DParms;
      end
    ;

    _smInput^.controlParams.useDestructionGlobal := true;
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
    _smInput^.controlParams.ssDestrPriorities.Add( prodType + '+' + 'basic', StrToInt( Priority ));
    _destructionPriorityList.insert( StrToInt (Priority), prodType + '+' + 'basic' );
    _smInput^.controlParams.useDestructionGlobal := true;
  end
;


function ProcessTracebackDestruction( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    _smInput:TSMInputPtr;
    _prodTypeList:TProductionTypeList;
    _prodType:TProductionType;
    prodType:String;
    prodTypeID: integer;
    Priority:Integer;
    DParms:TDestructionParams;
    TrParms: TTracingParams;
    contactType:String;
    Period:Integer;
    Success:Double;
    QuarantineOnly:Boolean;
  begin
    result := nil; // until established otherwise

    _smInput := TSMInputPtr( extData );
    _prodTypeList := _smInput^.ptList;

    prodType := Sdew.GetElementAttribute( Element, 'production-type' );
    prodTypeID := myStrToInt( Sdew.GetElementAttribute( Element, 'production-type-id' ), -1 );
    contactType := Sdew.GetElementAttribute( Element, 'contact-type' );

    _prodType := _prodTypeList.findProdType( prodType );

    if( nil = _prodType ) then
      begin
        _prodType := TProductionType.create( prodTypeID, prodType, _smInput^ );
        _prodTypeList.append( _prodType );
      end
    ;

    if ( Assigned(_prodType.destructionParams) ) then
      DParms := _prodType.destructionParams
    else
      begin
        DParms := TDestructionParams.create( _smInput^, prodType );
        _prodType.destructionParams := DParms;
      end
    ;

    if ( Assigned(_prodType.tracingParams) ) then
      TrParms := _prodType.tracingParams
    else
      begin
        TrParms := TTracingParams.create( _smInput^, prodType );
        _prodType.tracingParams := TrParms;
      end
    ;

    //??? Is this correct to set this for other types of destruction other than basic????
    _smInput^.controlParams.useDestructionGlobal := true;

    Success := usStrToFloat( Sdew.GetElementContents(Sdew.GetElementByName( Element, 'trace-success' )) );
    //NOTE:  Ignoring the units in the period element because they are always set to "day" by the xml output routines for DestructionParams.
    Period := StrToInt( Sdew.GetElementContents( Sdew.GetElementByName( Sdew.GetElementByName( Element, 'trace-period'), 'value')));
    Priority := StrToInt( Sdew.GetElementContents(Sdew.GetElementByName(Element, 'priority')));
    QuarantineOnly := StrToBool( Sdew.GetElementContents( Sdew.GetElementByName( Element, 'quarantine-only') ) );


    //NOTE:  This is a placeholder in case the tracing element is found before the basic one, or if there
    //       is no basic one.....Notice that we will not set the "includeDestructionGlobal" until/unless we
    //       find an actual basic destruction element.
    _smInput^.controlParams.ssDestrPriorities.Add( prodType + '+' + 'basic', 0 );

    if ( contactType = 'direct' ) then
      begin
        TrParms.traceDirectForward := true;
        TrParms.directTracePeriod := Period;
        TrParms.directTraceSuccess := Success;
        if ( not QuarantineOnly ) then
          DParms.destroyDirectForwardTraces := true
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

        _smInput^.controlParams.ssDestrPriorities.Add( prodType + '+' + 'direct', Priority );
        _destructionPriorityList.insert( Priority, prodType + '+' + 'direct' );
      end
    else
      begin
        TrParms.traceIndirectForward := true;
        TrParms.indirectTracePeriod := Period;
        TrParms.indirectTraceSuccess := Success;
        if ( not QuarantineOnly ) then
          DParms.destroyIndirectForwardTraces := true
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
        _smInput^.controlParams.ssDestrPriorities.Add( prodType + '+' + 'indirect', Priority );
        _destructionPriorityList.insert( Priority, prodType + '+' + 'indirect' );
      end
    ;
    _smInput^.controlParams.useDestructionGlobal := true;
  end
;


function ProcessRingDestructionModel( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    _smInput:TSMInputPtr;
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

    _smInput := TSMInputPtr( extData );
    _prodTypeList := _smInput^.ptList;
     destrPriorityList := _smInput^.controlParams.ssDestrPriorities;

    prodTypeSrc :=  Sdew.GetElementAttribute( Element, 'from-production-type' );
    prodTypeDest := Sdew.GetElementAttribute( Element, 'to-production-type');

    //Get or create the production-type-pair here....
    _prodTypeSrc := _prodTypeList.findProdType( prodTypeSrc );
    _prodTypeDest := _prodTypeList.findProdType( prodTypeDest );

    if( nil = _prodTypeSrc ) then // The production type should be created
      begin
        _prodTypeSrc := TProductionType.create( -1, prodTypeSrc, _smInput^ );
        _prodTypeList.append( _prodTypeSrc );
      end
    ;

    if ( nil = _prodTypeDest ) then // The production type should be created
      begin
        _prodTypeDest := TProductionType.create( -1, prodTypeDest, _smInput^ );
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
                DParms := TDestructionParams.create( _smInput^, prodTypeSrc );
                _prodTypeSrc.destructionParams := DParms;
              end
            ;

            DParms.isRingTrigger := true;
            DParms.ringRadius := value; //Note:  Units are not used, tho they are in the xml file...assume Km.

            DParms := _prodTypeDest.destructionParams;
            if ( DParms = nil ) then
              begin
                DParms := TDestructionParams.create( _smInput^, prodTypeDest );
                _prodTypeDest.destructionParams := DParms;
              end
            ;
            DParms.isRingTarget := true;

            //Priorities go with the "to type", i.e. destination....
            //NOTE:  These destruction ring priorities are NOT currently used
            //       in the system, but may be in the future.  Currently,
            //       The priorities are calculated based on other destruction
            //       priorities, since destruction priorities, overall, are actually
            //       a two-dimensional mapping of priorities.  Consequently, these
            //       values, as imported from XML, per se, are not directly used.             
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

                _smInput^.controlParams.ssDestrPriorities.Add( prodTypeDest + '+' + 'ring', Priority );

                destrPriorityList.Add( prodTypeDest + '+' + 'ring', priority );
                _destructionPriorityList.insert( priority, prodTypeDest + '+' + 'ring' );
                //Set to bogus value so that the populateDatabase() function won't
                // throw an exception...because from-type doesn't need a priority here...but the populate function doesn't know that....
                destrPriorityList.Add( prodTypeSrc + '+' + 'ring', -1 );
              end
            ;

            _smInput^.controlParams.useDestructionGlobal := true;
          end
        ;
      end
    ;

    result := ret_val;
  end
;


function ProcessRingVaccineModel( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    _smInput:TSMInputPtr;
    _prodTypeList:TProductionTypeList;
    prodTypeSrc, prodTypeDest:String;
    _prodTypeSrc, _prodTypeDest:TProductionType;
    value:Double;
    ret_val:TObject;
    subElement:Pointer;
    priority:Integer;
    minTimeBetweenVacc:Integer;
    ringVaccSrc, ringVaccDest:TRingVaccParams;
    vaccinateDetectedUnits: boolean;
  begin
    ret_val := nil;

    _smInput := TSMInputPtr( extData );
    _prodTypeList := _smInput^.ptList;

    prodTypeSrc :=  Sdew.GetElementAttribute( Element, 'from-production-type' );
    prodTypeDest := Sdew.GetElementAttribute( Element, 'to-production-type');

    //Get or create the production-type-pair here....
    _prodTypeSrc := _prodTypeList.findProdType( prodTypeSrc );
    _prodTypeDest := _prodTypeList.findProdType( prodTypeDest );

    if( nil = _prodTypeSrc ) then // The production type should be created
      begin
        _prodTypeSrc := TProductionType.create( -1, prodTypeSrc, _smInput^ );
        _prodTypeList.append( _prodTypeSrc );
      end
    ;

    if( nil = _prodTypeDest ) then // The production type should be created
      begin
        _prodTypeDest := TProductionType.create( -1, prodTypeDest, _smInput^ );
        _prodTypeList.append( _prodTypeDest );
      end
    ;


    ringVaccSrc := _prodTypeSrc.ringVaccParams;
    if ( ringVaccSrc = nil ) then
      begin
        ringVaccSrc := TRingVaccParams.create( _smInput^, prodTypeSrc );
        _prodTypeSrc.ringVaccParams := ringVaccSrc;
      end
    ;

    ringVaccDest := _prodTypeDest.ringVaccParams;
    if ( ringVaccDest = nil ) then
      begin
        ringVaccDest := TRingVaccParams.create( _smInput^, prodTypeDest );
        _prodTypeDest.ringVaccParams := ringVaccDest;
      end
    ;

    ringVaccSrc.useRing := true;

    //Priorities go with the "to type", i.e. destination.
    if ( Sdew.GetElementByName( Element, 'priority') <> nil ) then
      begin
        priority := StrToInt( Sdew.GetElementContents( Sdew.GetElementByName( Element, 'priority')) );
        _prodTypeDest.ringVaccParams.vaccPriority := priority;

        if ( _smInput^.controlParams.ssVaccPriorities.contains( prodTypeDest + '+' + 'ring' ) ) then
          begin
            if ( _smInput^.controlParams.ssVaccPriorities.Item[prodTypeDest + '+' + 'ring'] > priority ) then
              _smInput^.controlParams.ssVaccPriorities.Item[prodTypeDest + '+' + 'ring'] := priority;
          end
        else
          begin
              _smInput^.controlParams.ssVaccPriorities.Item[prodTypeDest + '+' + 'ring'] := priority;
          end
        ;

//        _smInput^.controlParams.ssVaccPriorities.Add( prodTypeDest + '+' + 'ring', priority );
        //Set to bogus value so that the populateDatabase() function won't
        // throw an exception...because from-type doesn't need a priority here...but the populate function doesn't know that....

        if  not ( _smInput^.controlParams.ssVaccPriorities.contains( prodTypeSrc + '+' + 'ring' ) ) then
           _smInput^.controlParams.ssVaccPriorities.Add( prodTypeSrc + '+' + 'ring', -1 )
        ;
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
          end
        ;
      end
    ;

   subElement := Sdew.GetElementByName( Element, 'min-time-between-vaccinations');
    if ( subElement <> nil ) then
      begin
        minTimeBetweenVacc := StrToInt( Sdew.GetElementContents( Sdew.GetElementByName( subElement, 'value' ) ) );
        //  Units are present in the xml file, but not stored in anywhere in the current schema.
        ringVaccSrc.minTimeBetweenVacc := minTimeBetweenVacc;

        //  SC: Why did I embedded this here?  Does it rely on the existence of this tag?
        _smInput^.controlParams.useVaccGlobal := true;
      end
    ;

   subElement := Sdew.GetElementByName( Element, 'vaccinate-detected-units');
    if ( subElement <> nil ) then
      begin
        vaccinateDetectedUnits := StrToBool( Sdew.GetElementContents( subElement ) );
        //  Units are present in the xml file, but not stored in anywhere in the current schema.
        ringVaccSrc.vaccinateDetected := vaccinateDetectedUnits;
      end
    ;

    result := ret_val;
  end
;


function ProcessLocalAreaModel( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  begin
    // FIX ME: This function needs to be written!
    result := nil;
  end
;


// FIX ME: This function needed to be revised!
function ProcessAirborneModel( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
(*
  var
    ret_val:TObject;
    _smInput:TSMInputPtr;
    _prodTypeList:TProductionTypeList;
    _ptpList:TProductionTypePairList;
    _ptp:TProductionTypePair;
    _prodTypeSrc, _prodTypeDest:TProductionType;
    prodTypeSrc, prodTypeDest:String;
    _airModel:TAirborneSpreadParams;
    subElement:Pointer;
    ssubElement:Pointer;
    unitElement:Pointer;
    statMethod:TChartFunction;
    probSpread:Double;
    Index:Integer;
    windDirStart, windDirEnd:Integer;
    maxSpread:Double;
    exponential:Boolean;
*)
  begin
(*
    ret_val := nil;
    exponential := false;
    _smInput := TSMInputPtr( extData );
    _prodTypeList := _smInput^.ptList;
    _ptpList := _smInput^.ptpList;

    if ( Sdew.GetElementName( Element ) = 'airborne-spread-exponential-model' ) then
      exponential := true;
      
    prodTypeSrc :=  Sdew.GetElementAttribute( Element, 'from-production-type' );
    prodTypeDest := Sdew.GetElementAttribute( Element, 'to-production-type');

    //Get or create the production-type-pair here....
    _prodTypeSrc := _prodTypeList.findProdType( prodTypeSrc );
    _prodTypeDest := _prodTypeList.findProdType( prodTypeDest );

    if( nil = _prodTypeSrc ) then // The production type should be created
      begin
        _prodTypeSrc := TProductionType.create( -1, prodTypeSrc, false, _smInput^ );
        _prodTypeList.append( _prodTypeSrc );
      end
    ;

    if( nil = _prodTypeDest ) then // The production type should be created
      begin
        _prodTypeDest := TProductionType.create( -1, prodTypeDest, false, _smInput^ );
        _prodTypeList.append( _prodTypeDest );
      end
    ;

    Index := _ptpList.pairPosition( _prodTypeSrc, _prodTypeDest );
    if ( Index <> -1 ) then
      begin
        _ptp := _ptpList.at( Index );
      end
    else
      begin
        _ptp := TProductionTypePair.create( _prodTypeSrc, _prodTypeDest, _smInput^ );
        _smInput^.database.makeProductionTypePair( _prodTypeSrc.productionTypeID, _prodTypeDest.productionTypeID );
        _ptpList.Add( _ptp );
      end;
    _airModel := TAirborneSpreadParams.create(  _smInput^.database,
                    -1, _smInput^, _prodTypeSrc.productionTypeID,
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
        statMethod := TChartFunction( createPdfFromXml( subElement, Sdew, nil ) );
        if ( statMethod.name = '' ) then
          statMethod.name := 'Day airborne delay';
          
        statMethod.dbField := word( AIRDelay );
        _smInput^.functionDictionary.checkAndInsert( statMethod );
        _airModel.pdfDelayName := statMethod.name;
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

    _smInput^.includeAirborneSpreadGlobal := true;

    if ( exponential ) then
//      _smInput^.useAirborneExponentialDecay := true
    ;

    result := ret_val;
*)
    result := nil;
  end
;


function ProcessContactSpreadZoneModel( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    _smInput:TSMInputPtr;
    _prodTypeList:TProductionTypeList;
    _prodTypeSrc:TProductionType;
    prodTypeSrc:String;
    contactType:String;
    ZoneName:String;
    subElement:Pointer;
    Chart:TRelFunction;
    _zoneList:TZoneList;
    _zoneParams: TProdTypeZoneParams;
    _zoneId:Integer;
  begin
    result := nil;

    _smInput := TSMInputPtr( extData );
    _prodTypeList := _smInput^.ptList;
    _zoneList := _smInput^.zoneList;

    prodTypeSrc :=  Sdew.GetElementAttribute( Element, 'from-production-type' );
    contactType :=  Sdew.GetElementAttribute( Element, 'contact-type' );
    ZoneName := Sdew.GetElementAttribute( Element, 'zone' );

    if ( nil = _zoneList.find( ZoneName ) ) then
      raise exception.Create( 'Zone does not exist in ProcessContactSpreadZoneModel()' )
    ;

    _prodTypeSrc := _prodTypeList.findProdType( prodTypeSrc );

    if ( _prodTypeSrc = nil ) then // Create a production type.
      begin
        _prodTypeSrc := TProductionType.create( -1, prodTypeSrc, _smInput^ );
        _prodTypeList.append( _prodTypeSrc );
      end
    ;

    if ( nil = _prodTypeSrc.zoneParams ) then
      raise exception.Create( 'prodTypeSrc.zoneParams is nil in ProcessContactSpreadZoneModel()' )
    ;

    //Create the movement control chart here
    subElement := Sdew.GetElementByName( Element, 'movement-control' );
    if ( subElement <> nil ) then
      begin
        Chart := TRelFunction( createRelFromXml( subElement, sdew, nil ) );
        Chart.convertToPercentages();

        if ( Chart.name = '' ) then
          Chart.name := contactType  + ' Zone Movement control effect - ' + _prodTypeSrc.productionTypeDescr + ' - ' + ZoneName;

        if ( contactType = 'direct' ) then
          Chart.dbField := word( ZONMovementDirect )
        else
          Chart.dbField := word( ZONMovementIndirect )
        ;
        _smInput^.functionDictionary.checkAndInsert( chart );

        // Got a chart, now set the name in the zoneParams...
        _zoneParams := _prodTypeSrc.zoneParams;

        if ( _zoneParams.zonePtParamsList = nil ) then
          begin
            _zoneParams := TProdTypeZoneParams.create( _smInput^.database, _prodTypeSrc.productionTypeID, _prodTypeSrc.productionTypeDescr, _zoneList );
            _zoneParams.sim := _prodTypeSrc.sim;
            _prodTypeSrc.zoneParams := _zoneParams;
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
      end
    ;
  end
;


function ProcessContactSpreadModel( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    _smInput:TSMInputPtr;
    _prodTypeList:TProductionTypeList;
    _ptpList:TProductionTypePairList;
    _ptp:TProductionTypePair;
    _prodTypeSrc, _prodTypeDest:TProductionType;
    prodTypeSrc, prodTypeDest:String;
    contactType:String;
    _contactType:TContactSpreadParams;
    subElement:Pointer;
    ssubElement:Pointer;
    statMethod:TChartFunction;
    probInfect:Double;
    Chart:TRelFunction;
    Index:Integer;
    contactRate:Double;
  begin
    result := nil;

    _smInput := TSMInputPtr( extData );
    _prodTypeList := _smInput^.ptList;
    _ptpList := _smInput^.ptpList;

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
            _prodTypeSrc := TProductionType.create( -1, prodTypeSrc, _smInput^ );
            _prodTypeList.append( _prodTypeSrc );
          end
        ;

        if ( _prodTypeDest = nil ) then // The production type should be created
          begin
            _prodTypeList.append( _prodTypeDest );
          end
        ;

        Index := _ptpList.pairPosition( _prodTypeSrc, _prodTypeDest );
        if ( Index <> -1 ) then
          _ptp := _ptpList.at( Index )
        else
          begin
            _ptp := TProductionTypePair.create( _prodTypeSrc, _prodTypeDest, _smInput^ );
            _ptpList.Add( _ptp );
          end
        ;

         _contactType := TContactSpreadParams.create();
         _contactType.sim := _smInput^;
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
            statMethod := TChartFunction( createPdfFromXml( subElement, Sdew, nil ) );
            if ( statMethod.name = '' ) then
              statMethod.name := 'Dist ' + contactType + ' movement';

            if _contactType.contactType = CMDirect then
              statMethod.dbField := word( CMDistanceDirect )
            else
              statMethod.dbField := word( CMDistanceIndirect )
            ;
            _smInput^.functionDictionary.checkAndInsert( statMethod );
            _contactType.pdfDistanceName := statMethod.name;
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
            Chart := TRelFunction( createRelFromXml( subElement, sdew, nil ) );
            chart.convertToPercentages();

            if ( Chart.name = '' ) then
              Chart.name := 'Movement control effect ' + contactType;

            if ( _contactType.contactType = CMDirect ) then
              Chart.dbField := word( CMMovementControlDirect )
            else
              Chart.dbField := word( CMMovementControlIndirect )
            ;
            _smInput^.functionDictionary.checkAndInsert( Chart );
            _contactType.relMovementControlName := Chart.name;
          end
        ;

        // These elements still exist in XML for version 4, but they are no longer user-definable options.
        // They are preserved for the sake of the test suite, which makes pretty heavy use of them.
        (*
        subElement := Sdew.GetElementByName( Element, 'latent-units-can-infect' );
        if ( subElement <> nil ) then
          _contactType.latentCanInfect :=  StrToBool( Sdew.GetElementContents( subElement ) )
        ;

        subElement := Sdew.GetElementByName( Element, 'subclinical-units-can-infect' );
        if ( subElement <> nil ) then
          _contactType.subClinicalCanInfect :=  StrToBool( Sdew.GetElementContents( subElement ) );
        ;
        *)

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

        _smInput^.includeContactSpreadGlobal := true;
      end
    ;
  end
;


function ProcessGlobalControlParams ( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    _smInput:TSMInputPtr;
    gcParms:TGlobalControlParams;
    Delay:Integer;
    PriorityOrder:String;
    Chart:TRelFunction;
    subElement:Pointer;
    ssubElement:Pointer;
  begin
    result := nil;

    _smInput := TSMInputPtr( extData );
    gcParms := _smInput^.controlParams;

    //  Get Destruction global settings
    subElement := Sdew.GetElementByName( Element, 'destruction-program-delay' );
    if ( nil <> subElement ) then
      begin
        // Ignoring Units here....always days.
        ssubElement := Sdew.GetElementByName( SubElement, 'value' );
        if ( nil <> ssubElement ) then
          begin
            Delay := StrToInt( Sdew.GetElementContents( ssubElement ) );
            gcParms.destrProgramDelay := Delay;
          end
      else
        errorMessage := errorMessage + tr('Warning: No destruction-program-delay value found in this xml file') + endl
      ;
      end
    else
      errorMessage := errorMessage + tr('Warning: No destruction-program-delay element found in this xml file') + endl
    ;

    subElement := Sdew.GetElementByName( Element, 'destruction-priority-order');
    if ( nil <> subElement ) then
      begin
        PriorityOrder := Sdew.GetElementContents( subElement );
        gcParms.destrPriorityOrder := PriorityOrder;
      end
    else
      errorMessage := errorMessage + tr('Warning: No destruction-priority-order element found in this xml file') + endl
    ;


    subElement := Sdew.GetElementByName( Element, 'destruction-capacity');
    if ( nil <> subElement ) then
      begin
        Chart := TRelFunction( createRelFromXml( subElement, sdew, nil ) );
        
        if ( Chart.name = '' ) then
          Chart.name := 'Destruction capacity';

        Chart.xUnits :=  chartUnitTypeFromXml( 'days' );
        Chart.yUnits := chartUnitTypeFromXml( 'units per day' );
        Chart.dbField := word( DestrCapacityGlobal );
        _smInput^.functionDictionary.checkAndInsert( Chart );
        gcParms.relDestrCapacityName := Chart.name;
      end
    else
      errorMessage := errorMessage + tr('Warning: No destruction-capacity element found in this xml file') + endl
    ;

    // Get Vaccination global settings...
    subElement := Sdew.GetElementByName( Element, 'vaccination-program-delay' );
    if ( subElement <> nil ) then
      begin
        Delay := StrToInt( Sdew.GetElementContents( subElement ) );
        gcParms.vaccDetectedUnitsBeforeStart := Delay;
      end
    else
      errorMessage := errorMessage + tr('Warning: No vaccination-program-delay element found in this xml file') + endl
    ;

    subElement := Sdew.GetElementByName( Element, 'vaccination-priority-order');
    if ( subElement <> nil ) then
      begin
        PriorityOrder := Sdew.GetElementContents( subElement );
        gcParms.vaccPriorityOrder := PriorityOrder;
      end
    else
      errorMessage := errorMessage + tr('Warning: No vaccination-priority-order element found in this xml file') + endl
    ;

    subElement := Sdew.GetElementByName( Element, 'vaccination-capacity');
    if ( subElement <> nil ) then
      begin
        Chart := TRelFunction( createRelFromXml( subElement, sdew, nil ) );
        
        if ( Chart.name = '' ) then
          chart.name := 'Vaccination capacity';
        Chart.xUnits :=  chartUnitTypeFromXml( 'days' );
        Chart.yUnits := chartUnitTypeFromXml( 'units per day' );
        Chart.dbField := word( VaccCapacityGlobal );
        _smInput^.functionDictionary.checkAndInsert( Chart );
        gcParms.relVaccCapacityName := Chart.name;
      end
    else
      errorMessage := errorMessage + tr('Warning: No vaccination-capacity element found in this xml file') + endl
    ;
  end
;


function ProcessDetectionZoneModels( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    _smInput: TSMInputPtr;
    _prodTypeList: TProductionTypeList;
    _prodType: TProductionType;
    prodType: String;
    prodTypeID: integer;
    zoneName: String;
    ret_val: TObject;
    _zoneList: TZoneList;
    _zoneParams: TProdTypeZoneParams;
    _zoneId: Integer;
    detectionMultiplier: double;
  begin
    ret_val := nil;
    _smInput := TSMInputPtr( extData );
    _zoneList := _smInput^.zoneList;
    _prodTypeList := _smInput^.ptList;

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
        _prodType := TProductionType.create( prodTypeID, prodType, _smInput^ );
        _prodTypeList.append( _prodType );
      end
    ;

    if( ( length(zoneName) > 0 ) AND (_zoneList <> nil ) )  then
      begin
        _zoneParams := _prodType.zoneParams;

        if ( _zoneParams <> nil ) then
          begin
            _zoneId := _zoneList.find( ZoneName ).id;

            //  This function also sets the useDetectionMultiplier boolean in the TZoneProdTypeComboParams object...so no need to set it also, here.
            _zoneParams.zonePtParamsList.value( _zoneId ).detectionMultiplier := detectionMultiplier;

            _zoneParams.prodTypeDescr := prodType;
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
    _smInput:TSMInputPtr;
    _prodTypeList:TProductionTypeList;
    _prodType:TProductionType;
    _fnDictionary:TFunctionDictionary;
    statMethod:TRelFunction;
    prodType:String;
    prodTypeID: integer;
    ret_val:TObject;
  begin
    ret_val := nil;
    _smInput := TSMInputPtr( extData );
    _prodTypeList := _smInput^.ptList;
    _fnDictionary := _smInput^.functionDictionary;

    SetLength(DetectionSections, 2);
    DetectionSections[0].Tag := 'prob-report-vs-time-clinical';
    DetectionSections[0].Callback := createRelFromXml;
    SetLength(DetectionSections[0].ObjectList,0);

    DetectionSections[1].Tag := 'prob-report-vs-time-since-outbreak';
    DetectionSections[1].Callback := createRelFromXml;
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
            _prodType := TProductionType.create( prodTypeID, prodType, _smInput^ );
            _prodTypeList.append( _prodType );
          end
        ;

        tempXMLReader := TXMLReader.create( @DetectionSections, Sdew, Element, extData );
        tempXMLReader.Run();
        tempXMLReader.free();

        if ( (length( DetectionSections[0].ObjectList ) > 0 ) and (length( DetectionSections[1].ObjectList ) > 0 ) ) then
          begin
            statMethod := TRelFunction( DetectionSections[0].ObjectList[0] );
            statMethod.convertToPercentages();
            statMethod.dbField := word( DetProbObsVsTimeClinical );
            if ( statMethod.name = '' ) then
              statMethod.name := 'Probability of observing clinical signs versus days clinical' + ' ' + ansiLowerCase( prodType );
            _fnDictionary.checkAndInsert( statMethod );

            statMethod := TRelFunction( DetectionSections[1].ObjectList[0] );
            statMethod.convertToPercentages();
            statMethod.dbField := word( DetProbReportVsFirstDetection );
            if ( statMethod.name = '' ) then
              statMethod.name := 'Probability of reporting versus days of outbreak' + ' ' + ansiLowerCase( prodType );
            _fnDictionary.checkAndInsert( statMethod );

            if ( not Assigned( _prodType.detectionParams ) ) then
              begin
                DParms := TDetectionParams.create( _smInput^, prodType );
                _prodType.detectionParams := DParms;
              end
            else
              begin
                DParms := _prodType.detectionParams;
                DParms.prodTypeDescr := prodType;
              end
            ;

            DParms.relObsVsTimeClinicalName := TRelFunction(DetectionSections[0].ObjectList[0]).name;
            DParms.relReportVsFirstDetectionName := TRelFunction(DetectionSections[1].ObjectList[0]).name;

            _prodType.detectionParams.useDetection := true;
            _smInput^.controlParams.useTracingGlobal := true;
            _smInput^.controlParams.useDetectionGlobal := true;
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
    _smInput:TSMInputPtr;
    _prodTypeList:TProductionTypeList;
    _prodType:TProductionType;
    _fnDictionary:TFunctionDictionary;
    statMethod: TPdf;
    prodType:String;
    prodTypeID: integer;
    vaccParams:TVaccinationParams;
    vacDelay: TValueUnitPair;
    ret_val:TObject;

  begin
    ret_val := nil;
    vacDelay := nil;

    _smInput := TSMInputPtr( extData );
    _prodTypeList := _smInput^.ptList;
    _fnDictionary := _smInput^.functionDictionary;

    SetLength(VaccineSections, 2);
    VaccineSections[0].Tag := 'delay';
    VaccineSections[0].Callback := ProcessVaccineDelay;
    SetLength(VaccineSections[0].ObjectList,0);

    VaccineSections[1].Tag := 'immunity-period';
    VaccineSections[1].Callback := createPdfFromXml;
    SetLength(VaccineSections[1].ObjectList,0);

    prodType := Sdew.GetElementAttribute( Element, 'production-type' );
    prodTypeID := myStrToInt( Sdew.GetElementAttribute( Element, 'production-type-id' ), -1 );

    _prodType := _prodTypeList.findProdType( prodType );

    if( nil = _prodType ) then // The production type should be created
      begin
        _prodType := TProductionType.create( prodTypeID, prodType, _smInput^ );
        _prodTypeList.append( _prodType );
      end
    ;

    tempXMLReader := TXMLReader.create( @VaccineSections, Sdew, Element, extData );
    tempXMLReader.Run();
    tempXMLReader.free();

    if ( ( length(VaccineSections[0].ObjectList) > 0) and  (length( VaccineSections[1].ObjectList) > 0 ) ) then
      begin
        if ( ( Assigned( VaccineSections[0].ObjectList[0] ) ) and ( Assigned( VaccineSections[1].ObjectList[0] ) ) ) then
          begin
            vacDelay := TValueUnitPair( VaccineSections[0].ObjectList[0] );
            statMethod := TPdf( VaccineSections[1].ObjectList[0] );
            if ( statMethod.name = '' ) then
              statMethod.name := 'Vaccine immune period' + ' - ' + prodType;
            statMethod.dbField := word( VacImmunePeriod );
            _fnDictionary.checkAndInsert( statMethod );
            vaccParams := TVaccinationParams.create( _smInput^, prodType );
            vaccParams.pdfVaccImmuneName := statMethod.name;
            vaccParams.useVaccination := true;
            _smInput^.controlParams.useVaccGlobal := true;
            vaccParams.daysToImmunity := StrToInt(vacDelay.getValue());
            ret_val := vaccParams;
            _prodType.vaccinationParams := vaccParams;
            _smInput^.controlParams.useVaccGlobal := true;
          end
        ;
      end
    ;

    if( nil <> vacDelay ) then
      vacDelay.Free()
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
    _smInput:TSMInputPtr;
  begin
    result := nil;

    _smInput := TSMInputPtr( extData );
    _prodTypeList := _smInput^.ptList;

    prodType :=  Sdew.GetElementAttribute( Element, 'production-type' );
    prodTypeID := myStrToInt( Sdew.GetElementAttribute( Element, 'production-type-id' ), -1 );

    _prodType := _prodTypeList.findProdType( prodType );

    if( nil = _prodType ) then // The production type should be created
      begin
        _prodType := TProductionType.create( prodTypeID, prodType, _smInput^ );
        _prodTypeList.append( _prodType );
      end
    ;

    if ( _prodType.zoneParams <> nil ) then
      begin
        _prodType.zoneParams.detectionIsZoneTrigger := true;
        _prodType.zoneParams.prodTypeDescr := prodType;
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
    _smInput:TSMInputPtr;
  begin
    result := nil;

    _smInput := TSMInputPtr( extData );
    _prodTypeList := _smInput^.ptList;

    prodType :=  Sdew.GetElementAttribute( Element, 'production-type' );
    prodTypeID := myStrToInt( Sdew.GetElementAttribute( Element, 'production-type-id' ), -1 );

    contactType :=  Sdew.GetElementAttribute( Element, 'contact-type' );

    _prodType := _prodTypeList.findProdType( prodType );

    if( nil = _prodType ) then // The production type should be created
      begin
        _prodType := TProductionType.create( prodTypeID, prodType, _smInput^ );
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
      end
    ;
  end
;

function ProcessTraceModel ( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    _smInput:TSMInputPtr;
    _prodTypeList: TProductionTypeList;
    prodType: String;
    prodTypeID: integer;
    contactType: String;
    contactDirection: String;
    _prodType: TProductionType;
    _tracePeriod: Integer;
    subElement: Pointer;
    _error: bool;
  begin
    _error := false;
    _smInput := TSMInputPtr( extData );
    _prodTypeList := _smInput^.ptList;

    prodType :=  Sdew.GetElementAttribute( Element, 'production-type' );
    prodTypeID := myStrToInt( Sdew.GetElementAttribute( Element, 'production-type-id' ), -1 );
    contactType :=  Sdew.GetElementAttribute( Element, 'contact-type' );
    contactDirection :=  Sdew.GetElementAttribute( Element, 'direction' );

    if ( contactType <> 'direct' )  and ( contactType <> 'indirect') then
      begin
        raise exception.Create( 'Invalid contact-type from imported XML used in ProcessTraceModel()' );
        _error := true;
      end
    ;

    if ( contactDirection <> 'in' )  and ( contactDirection <> 'out') then
      begin
        raise exception.Create( 'Invalid contact-direction from imported XML used in ProcessTraceModel()' );
        _error := true;
      end
    ;

    if ( not _error ) then
      begin
        _prodType := _prodTypeList.findProdType( prodType );

        if( nil = _prodType ) then
          begin
            _prodType := TProductionType.create( prodTypeID, prodType, _smInput^ );
            _prodTypeList.append( _prodType );
          end
        ;

        if ( not assigned( _prodType ) ) then
          begin
            raise exception.Create( 'Could not find or create the productionType needed in ProcessTraceModel()' );
            _error := true;
          end
        ;

        subElement := Sdew.GetElementByName( Element, 'trace-period' );
        if ( subElement <> nil ) and not _error then
          subElement := Sdew.GetElementByName( subElement, 'value' )
        else
          begin
            raise exception.Create( 'Can not find <trace-period> in ProcessTraceModel()' );
            _error := true;
          end
        ;

        if ( ( subElement <> nil ) and ( not _error) ) then
          _tracePeriod := StrToInt( Sdew.GetElementContents( subElement ) )
        else
          begin
            raise exception.Create( 'Can not find a trace-period <value> in ProcessTraceModel()' )
          end
        ;

        if ( not _error ) then
          begin
            if ( contactType = 'direct' ) then
              begin
                _prodType.tracingParams.directTracePeriod := _tracePeriod;
                if ( contactDirection = 'in' ) then
                  begin
                    _prodType.tracingParams.traceDirectBack := true;
                  end
                else
                  if ( contactDirection = 'out') then
                    begin
                      _prodType.tracingParams.traceDirectForward := true
                    end
                  else
                    begin   //  This is checked above, but just in case that code changes, leave the else here...
                      raise exception.Create( 'Invalid contact-direction from imported XML used in ProcessTraceModel()' );
                    end
                  ;
              end
            else
              if ( contactType = 'indirect') then
                begin
                  _prodType.tracingParams.indirectTracePeriod := _tracePeriod;
                if ( contactDirection = 'in' ) then
                  begin
                    _prodType.tracingParams.traceIndirectBack := true;
                  end
                else
                  if ( contactDirection = 'out') then
                    begin
                      _prodType.tracingParams.traceIndirectForward := true
                    end
                  else
                    begin   //  This is checked above, but just in case that code changes, leave the else here...
                      raise exception.Create( 'Invalid contact-direction from imported XML used in ProcessTraceModel()' );
                    end
                  ;
                end
              else
                begin   //  This is checked above, but just in case that code changes, leave the else here...
                  raise exception.Create( 'Invalid contact-type from imported XML used in ProcessTraceModel()' );
                end
              ;
          end
        ;
      end
    ;
    result := nil;
  end
;


function ProcessTraceExamModel ( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    _smInput:TSMInputPtr;
    _prodTypeList: TProductionTypeList;
    prodType: String;
    prodTypeID: integer;
    contactType: String;
    contactDirection: String;
    _prodType: TProductionType;
    subElement: Pointer;
    _error: bool;
    detectionMultiplier: double;
    testIfNoSigns: bool;
  begin
    _error := false;
    _smInput := TSMInputPtr( extData );
    _prodTypeList := _smInput^.ptList;

    prodType :=  Sdew.GetElementAttribute( Element, 'production-type' );
    prodTypeID := myStrToInt( Sdew.GetElementAttribute( Element, 'production-type-id' ), -1 );
    contactType :=  Sdew.GetElementAttribute( Element, 'contact-type' );
    contactDirection :=  Sdew.GetElementAttribute( Element, 'direction' );

    if ( contactType <> 'direct' )  and ( contactType <> 'indirect') then
      begin
        raise exception.Create( 'Invalid contact-type from imported XML used in ProcessTraceExamModel()' );
        _error := true;
      end
    ;

    if ( contactDirection <> 'in' )  and ( contactDirection <> 'out') then
      begin
        raise exception.Create( 'Invalid contact-direction from imported XML used in ProcessTraceExamModel()' );
        _error := true;
      end
    ;

    if ( not _error ) then
      begin
        _prodType := _prodTypeList.findProdType( prodType );

        if( nil = _prodType ) then
          begin
            _prodType := TProductionType.create( prodTypeID, prodType, _smInput^ );
            _prodTypeList.append( _prodType );
          end
        ;

        if ( not assigned( _prodType ) ) then
          begin
            raise exception.Create( 'Could not find or create the productionType needed in ProcessTraceExamModel()' );
            _error := true;
          end
        ;

        subElement := Sdew.GetElementByName( Element, 'detection-multiplier' );
        if ( subElement <> nil ) and not _error then
          detectionMultiplier := usStrToFloat( Sdew.GetElementContents( subElement ) )
        else
          begin
            raise exception.Create( 'Can not find <detection-multiplier> in ProcessTraceExamModel()' );
            _error := true;
          end
        ;

        //TrueBoolStrs := ; ??  //  Does this need to be set for StrToBool to work properly....

        subElement := Sdew.GetElementByName( Element, 'test-if-no-signs' );
        if ( subElement <> nil ) and not _error then
          testIfNoSigns := StrToBool( Sdew.GetElementContents( subElement ) )
        else
          begin
            raise exception.Create( 'Can not find <test-if-no-signs> in ProcessTraceExamModel()' );
            _error := true;
          end
        ;
        
        if ( not _error ) then
          begin
            if ( contactType = 'direct' ) then
              begin
                if ( contactDirection = 'in' ) then    // in is Back, out is Forward
                  begin
                    _prodType.tracingParams.examDirectBackMultiplier := detectionMultiplier;
                    _prodType.testingParams.testDirectBack := testIfNoSigns;
                  end
                else
                  if ( contactDirection = 'out') then
                    begin
                      _prodType.tracingParams.examDirectForwardMultiplier := detectionMultiplier;
                      _prodType.testingParams.testDirectForward := testIfNoSigns;
                    end
                  else
                    begin   //  This is checked above, but just in case that code changes, leave the else here...
                      raise exception.Create( 'Invalid contact-direction from imported XML used in ProcessTraceExamModel()' );
                    end
                  ;
              end
            else
              if ( contactType = 'indirect') then
                begin
                  if ( contactDirection = 'in' ) then
                    begin
                      _prodType.tracingParams.examIndirectBackMultiplier := detectionMultiplier;
                      _prodType.testingParams.testIndirectBack := testIfNoSigns;
                    end
                  else
                    if ( contactDirection = 'out') then
                      begin
                        _prodType.tracingParams.examIndirectForwardMultiplier := detectionMultiplier;
                        _prodType.testingParams.testIndirectForward := testIfNoSigns;
                      end
                    else
                      begin   //  This is checked above, but just in case that code changes, leave the else here...
                        raise exception.Create( 'Invalid contact-direction from imported XML used in ProcessTraceExamModel()' );
                      end
                    ;
                end
              else
                begin   //  This is checked above, but just in case that code changes, leave the else here...
                  raise exception.Create( 'Invalid contact-type from imported XML used in ProcessTraceExamModel()' );
                end
              ;
          end
        ;
      end
    ;
    result := nil;
  end
;

function ProcessTestModel ( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    _smInput:TSMInputPtr;
    _prodTypeList: TProductionTypeList;
    prodType: String;
    prodTypeID: integer;
    _prodType: TProductionType;
    subElement: Pointer;
    _error: bool;
    statMethod: TChartFunction;
  begin
    _error := false;
    _smInput := TSMInputPtr( extData );
    _prodTypeList := _smInput^.ptList;

    prodType :=  Sdew.GetElementAttribute( Element, 'production-type' );
    prodTypeID := myStrToInt( Sdew.GetElementAttribute( Element, 'production-type-id' ), -1 );

    if ( not _error ) then
      begin
        _prodType := _prodTypeList.findProdType( prodType );

        if( nil = _prodType ) then
          begin
            _prodType := TProductionType.create( prodTypeID, prodType, _smInput^ );
            _prodTypeList.append( _prodType );
          end
        ;

        if ( not assigned( _prodType ) ) then
          begin
            raise exception.Create( 'Could not find or create the productionType needed in ProcessTestModel()' );
            _error := true;
          end
        ;

        subElement := Sdew.GetElementByName( Element, 'delay' );
        if ( subElement <> nil ) and not _error then
          begin
            statMethod := TChartFunction( createPdfFromXml( subElement, Sdew, nil ) );
            statMethod.dbField := word( TeDelay );
            _smInput^.functionDictionary.checkAndInsert( statMethod );
            _prodType.testingParams.pdfTestDelayName := statMethod.name;
          end
        else
          begin
            raise exception.Create( 'Could not find <delay> PDF function needed in ProcessTestModel()' );
          end
        ;
      end
    ;
    
    result := nil;
  end
;

function ProcessTraceDestructionModel ( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    _smInput:TSMInputPtr;
    _prodTypeList: TProductionTypeList;
    prodType: String;
    prodTypeID: integer;
    contactType: String;
    contactDirection: String;
    _prodType: TProductionType;
    _error: bool;
    DParms: TDestructionParams;
    Priority: Integer;
    destString: String;
    _direction: String;
  begin
    _error := false;
    _smInput := TSMInputPtr( extData );
    _prodTypeList := _smInput^.ptList;

    prodType :=  Sdew.GetElementAttribute( Element, 'production-type' );
    prodTypeID := myStrToInt( Sdew.GetElementAttribute( Element, 'production-type-id' ), -1 );
    contactType :=  Sdew.GetElementAttribute( Element, 'contact-type' );
    contactDirection :=  Sdew.GetElementAttribute( Element, 'direction' );

    if ( contactType <> 'direct' )  and ( contactType <> 'indirect') then
      begin
        raise exception.Create( 'Invalid contact-type from imported XML used in ProcessTraceDestructionModel()' );
        _error := true;
      end
    ;

    if ( contactDirection <> 'in' )  and ( contactDirection <> 'out') then
      begin
        raise exception.Create( 'Invalid contact-direction from imported XML used in ProcessTraceDestructionModel()' );
        _error := true;
      end
    ;

    if ( not _error ) then
      begin
        _prodType := _prodTypeList.findProdType( prodType );

        if( nil = _prodType ) then
          begin
            _prodType := TProductionType.create( prodTypeID, prodType, _smInput^ );
            _prodTypeList.append( _prodType );
          end
        ;

        if ( not assigned( _prodType ) ) then
          begin
            raise exception.Create( 'Could not find or create the productionType needed in ProcessTraceDestructionModel()' );
            _error := true;
          end
        else
          begin
            if ( not Assigned(_prodType.destructionParams) ) then
              begin
                DParms := TDestructionParams.create( _smInput^, prodType );
                _prodType.destructionParams := DParms;
              end
            ;

            if ( contactDirection = 'in' ) then
              begin
                if ( contactType = 'direct' ) then
                  begin
                    _prodType.destructionParams.destroyDirectBackTraces := true;
                  end
                else
                  if ( contacttype = 'indirect' ) then
                    begin
                      _prodType.destructionParams.destroyIndirectBackTraces := true;
                    end
                  else
                    begin
                      raise exception.Create( 'Invalid contact-type in  ProcessTraceDestructionModel()' );
                      _error := true;
                    end
                  ;
              end
            else
              if ( contactDirection = 'out' ) then
                begin
                  if ( contactType = 'direct' ) then
                    begin
                      _prodType.destructionParams.destroyDirectForwardTraces := true;
                    end
                  else
                    if ( contacttype = 'indirect' ) then
                      begin
                        _prodType.destructionParams.destroyIndirectForwardTraces := true;
                      end
                    else
                      begin
                        raise exception.Create( 'Invalid contact-type in  ProcessTraceDestructionModel()' );
                        _error := true;
                      end
                    ;
                end
              else
                begin
                  raise exception.Create( 'Invalid contact-direction in  ProcessTraceDestructionModel()' );
                  _error := true;
                end
              ;
          end
        ;
      end
    ;


    if ( not _error ) then
      begin
        Priority := StrToInt( Sdew.GetElementContents( Sdew.GetElementByName(Element, 'priority') ) );

        if ( contactDirection = 'in' ) then
          _direction := 'back'
        else
          _direction := 'forward';
      
        destString := contactType + '-' + _direction;

        if ( _internalDestructionPriorityList.contains( destString ) ) then
          begin
            if ( _internalDestructionPriorityList.Item[ destString ] > Priority ) then
               _internalDestructionPriorityList.Item[ destString ] := Priority;
           end
         else
           begin
               _internalDestructionPriorityList.Item[ destString ] := Priority;
           end;

        _smInput^.controlParams.ssDestrPriorities.Add( prodType + '+' + destString, Priority );
        _destructionPriorityList.insert( Priority, prodType + '+' + destString );
     end
    ;

    result:= nil;
  end
;

function ProcessContactRecorderModel ( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    _smInput:TSMInputPtr;
    _prodTypeList: TProductionTypeList;
    prodType: String;
    prodTypeID: integer;
    contactType: String;
    _prodType: TProductionType;
    _tracePeriod: Integer;
    _traceSuccess: Double;
    subElement: Pointer;
    _error: bool;
    statMethod: TChartFunction;
  begin
    _error := false;
    _smInput := TSMInputPtr( extData );
    _prodTypeList := _smInput^.ptList;

    prodType :=  Sdew.GetElementAttribute( Element, 'production-type' );
    prodTypeID := myStrToInt( Sdew.GetElementAttribute( Element, 'production-type-id' ), -1 );
    contactType :=  Sdew.GetElementAttribute( Element, 'contact-type' );

    if ( contactType <> 'direct' )  and ( contactType <> 'indirect') then
      begin
        raise exception.Create( 'Invalid contact-type from imported XML used in ProcessContactRecorderModel()' );
        _error := true;
      end
    ;

    if ( not _error ) then
      begin
        _prodType := _prodTypeList.findProdType( prodType );

        if( nil = _prodType ) then
          begin
            _prodType := TProductionType.create( prodTypeID, prodType, _smInput^ );
            _prodTypeList.append( _prodType );
          end
        ;

        subElement := Sdew.GetElementByName( Element, 'trace-period' );
        if ( subElement <> nil ) and not _error then
          subElement := Sdew.GetElementByName( subElement, 'value' )
        else
          begin
            raise exception.Create( 'Can not find <trace-period> in ProcessContactRecorderModel()' );
            _error := true;
          end
        ;

        if ( ( subElement <> nil ) and ( not _error) ) then
          _tracePeriod := StrToInt( Sdew.GetElementContents( subElement ) )
        else
          begin
            raise exception.Create( 'Can not find a trace-period <value> in ProcessContactRecorderModel()' )
          end
        ;

        subElement := Sdew.GetElementByName( Element, 'trace-success' );
        if ( subElement <> nil ) and not _error then
          begin
            _traceSuccess := usStrToFloat( Sdew.GetElementContents( subElement ), -1.0, true );
            if ( _traceSuccess < 0.0 ) OR ( _traceSuccess > 1.0 ) then
              begin
                raise exception.Create( '<trace-success> contained an invalid probability value in ProcessContactRecorderModel()' );
                _error := true;
              end
            ;
          end
        else
          begin
            raise exception.Create( 'Can not find <trace-success> in ProcessContactRecorderModel()' );
            _error := true;
          end
        ;

        //  Create this PDF if it doesn't already exist...
        //    trace-delay   pdf function.

        subElement := Sdew.GetElementByName( Element, 'trace-delay' );
        if ( subElement <> nil ) then
          begin
            statMethod := TChartFunction( createPdfFromXml( subElement, Sdew, nil ) );
            statMethod.dbField := word( TrDelay );
            _smInput^.functionDictionary.checkAndInsert( statMethod );
          end
        else
          begin
            raise exception.Create( 'Can not find <trace-delay> in ProcessContactRecorderModel()' );
            _error := true;
          end
        ;

        if ( not _error ) then
          begin
            if ( contactType = 'direct' ) then
              begin
                _prodType.tracingParams.directTracePeriod := _tracePeriod;
                _prodType.tracingParams.directTraceSuccess := _traceSuccess;
              end
            else
              if ( contactType = 'indirect') then
                begin
                  _prodType.tracingParams.indirectTracePeriod := _tracePeriod;
                  _prodType.tracingParams.indirectTraceSuccess := _traceSuccess;
                end
              else
                begin   //  This is checked above, but just in case that code changes, leave the else here...
                  raise exception.Create( 'Invalid contact-type from imported XML used in ProcessContactRecorderModel()' );
                  _error := true;
                end
              ;

            if ( not _error ) then
              begin
                _prodType.tracingParams.pdfTraceDelayName := statMethod.name;
              end
            ;
          end
        ;
      end  //  End no error, i.e. if contact-type is okay...
    ;

    result := nil;
  end
;


function ProcessZoneModels( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    ZoneName: String;
//    Level: Integer;
    Radius: String;
    _smInput:TSMInputPtr;
    _zoneList: TZoneList;
    _newZone: TZone;
    zoneID: integer;
  begin
    result := nil;
    
    _smInput := TSMInputPtr( extData );
    _zoneList := _smInput^.zoneList;

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

              if ( usStrToFloat( Radius ) > 0.0 ) then
                begin
                  if ( _zoneList.find( ZoneName ) = nil ) then
                    begin
                      _newZone := TZone.create( zoneID, ZoneName, usStrToFloat(Radius), _smInput^ );
                      _zoneList.append( _newZone );
                      _smInput^.controlParams.useZonesGlobal := true;
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
    diseaseCallbacks: CallbackArray;
    tempXMLReader: TXMLReader;

    latentPeriod: TPdf;
    infectiousSubClinicalPeriod: TPdf;
    infectiousClinicalPeriod: TPdf;
    immunityPeriod: TPdf;
    prevInfectedChart: TRelFunction;
    prevSheddingChart: TRelFunction;

    prodTypeDescr: String;
    prodTypeID: integer;
    _smInput: TSMInputPtr;
    _prodTypeList: TProductionTypeList;
    _prodType: TProductionType;
    _fnDictionary: TFunctionDictionary;
  begin
    latentPeriod := nil;
    infectiousSubClinicalPeriod := nil;
    infectiousClinicalPeriod := nil;
    immunityPeriod := nil;
    prevInfectedChart := nil;
    prevSheddingChart := nil;

    _smInput := TSMInputPtr( extData );
    _prodTypeList := _smInput^.ptList;
    _fnDictionary := _smInput^.functionDictionary;

    prodTypeDescr := Sdew.GetElementAttribute( Element, 'production-type' );
    prodTypeID := myStrToInt( Sdew.GetElementAttribute( Element, 'production-type-id' ), -1 );

    _prodType := _prodTypeList.findProdType( prodTypeDescr );

    if( nil = _prodType ) then
      begin
        _prodType := TProductionType.create( prodTypeID, prodTypeDescr, _smInput^ );
        _prodTypeList.append( _prodType );
      end
    ;

    // Read the disease states
    //------------------------
    SetLength( diseaseCallbacks, 5 );

    diseaseCallbacks[0].Tag := 'latent-period';
    diseaseCallbacks[0].Callback := createPdfFromXml;
    SetLength( diseaseCallbacks[0].ObjectList, 0 );

    diseaseCallbacks[1].Tag := 'infectious-subclinical-period';
    diseaseCallbacks[1].Callback := createPdfFromXml;
    SetLength( diseaseCallbacks[1].ObjectList, 0 );

    diseaseCallbacks[2].Tag := 'infectious-clinical-period';
    diseaseCallbacks[2].Callback := createPdfFromXml;
    SetLength( diseaseCallbacks[2].ObjectList, 0 );

    diseaseCallbacks[3].Tag := 'immunity-period';
    diseaseCallbacks[3].Callback := createPdfFromXml;
    SetLength( diseaseCallbacks[3].ObjectList, 0 );

    diseaseCallbacks[4].Tag := 'prevalence';
    diseaseCallbacks[4].Callback := createRelFromXml;
    SetLength( diseaseCallbacks[4].ObjectList, 0 );

    diseaseCallbacks[5].Tag := 'prevalence-infectious';
    diseaseCallbacks[5].Callback := createRelFromXml;
    SetLength( diseaseCallbacks[5].ObjectList, 0 );

    tempXMLReader := TXMLReader.create( @diseaseCallbacks, Sdew, Element, nil );
    tempXMLReader.Run();
    tempXMLReader.free();

    if( 0 < length( diseaseCallbacks[0].ObjectList ) ) then
      latentPeriod := TPdf( diseaseCallbacks[0].ObjectList[0] )
    ;
    if( 0 < length( diseaseCallbacks[1].ObjectList ) ) then
      infectiousSubClinicalPeriod := TPdf( diseaseCallbacks[1].ObjectList[0] )
    ;
    if( 0 < length( diseaseCallbacks[2].ObjectList ) ) then
      infectiousClinicalPeriod := TPdf( diseaseCallbacks[2].ObjectList[0] )
    ;
    if( 0 < length( diseaseCallbacks[3].ObjectList ) ) then
      immunityPeriod := TPdf( diseaseCallbacks[3].ObjectList[0] )
    ;
    if( 0 < length( diseaseCallbacks[4].ObjectList ) ) then
      prevInfectedChart := TRelFunction( diseaseCallbacks[4].ObjectList[0] )
    ;
    if( 0 < length( diseaseCallbacks[5].ObjectList ) ) then
      prevSheddingChart := TRelFunction( diseaseCallbacks[5].ObjectList[0] )
    ;

    if( nil <> latentPeriod ) then
      begin
        if ( latentPeriod.name = '' ) then
          latentPeriod.name := prodTypeDescr + ' latent period'
        ;
        _fnDictionary.checkAndInsert( latentPeriod );
        _prodType.diseaseParams.pdfLatentName := latentPeriod.name;
        dbcout( latentPeriod.name, DBSHOWMSG );
        latentPeriod.dbField := word( DLatent );
      end
    ;

    if( nil <> infectiousSubClinicalPeriod ) then
      begin
        if ( infectiousSubClinicalPeriod.name = '' ) then
          infectiousSubClinicalPeriod.name := prodTypeDescr + ' subclinical period'
        ;
        _fnDictionary.checkAndInsert( infectiousSubClinicalPeriod );
        _prodType.diseaseParams.pdfSubclinicalName := infectiousSubClinicalPeriod.name;
        dbcout( infectiousSubClinicalPeriod.name, DBSHOWMSG );
        infectiousSubClinicalPeriod.dbField := word( DSubclinical );
      end
    ;

    if( nil <> infectiousClinicalPeriod ) then
      begin
        if ( infectiousClinicalPeriod.name = '' ) then
          infectiousClinicalPeriod.name := prodTypeDescr + ' clinical period'
        ;
        _fnDictionary.checkAndInsert( infectiousClinicalPeriod );
        _prodType.diseaseParams.pdfClinicalName := infectiousClinicalPeriod.name;
        dbcout( infectiousClinicalPeriod.name, DBSHOWMSG );
        infectiousClinicalPeriod.dbField := word( DClinical );
      end
    ;

    if( nil <> immunityPeriod ) then
      begin
        if ( immunityPeriod.name = '' ) then
          immunityPeriod.name := prodTypeDescr + ' immune period'
        ;
        _fnDictionary.checkAndInsert( immunityPeriod );
        _prodType.diseaseParams.pdfImmuneName := immunityPeriod.name;
        dbcout( immunityPeriod.name, DBSHOWMSG );
        immunityPeriod.dbField := word( DImmune );
      end
    ;

    if( nil <> prevInfectedChart ) then
      begin
        if ( prevInfectedChart.name = '' ) then
          prevInfectedChart.name := prodTypeDescr + ' prevalence'
        ;
        _fnDictionary.checkAndInsert( prevInfectedChart );
        _prodType.diseaseParams.relPrevInfectedName := prevInfectedChart.name;
        dbcout( prevInfectedChart.name, DBSHOWMSG );
        prevInfectedChart.dbField := word( DPrevInfected );
        prevInfectedChart.convertToPercentages();

        _smInput^.useWithinHerdPrevalence := true;
      end
    ;

    if( nil <> prevSheddingChart ) then
      begin
        if ( prevSheddingChart.name = '' ) then
          prevSheddingChart.name := prodTypeDescr + ' prevalence shedding'
        ;
        _fnDictionary.checkAndInsert( prevSheddingChart );
        _prodType.diseaseParams.relPrevSheddingName := prevSheddingChart.name;
        dbcout( prevSheddingChart.name, DBSHOWMSG );
        prevSheddingChart.dbField := word( DPrevShedding );
        prevSheddingChart.convertToPercentages();

        _smInput^.useWithinHerdPrevalence := true;
      end
    ;

    result := nil; // This seems silly, but the function has to return something.
  end
;


function ProcessModels( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    ModelCallbacks : CallbackArray;
    tempXMLReader: TXMLReader;
    I:Integer;
  begin
    SetLength( ModelCallbacks, constNumXMLProcs );

    for i := 0 to constNumXMLProcs - 1 do
      begin
        ModelCallbacks[I].Tag := constXMLProcs[I].name;
        ModelCallbacks[I].Callback := constXMLProcs[I].xFunc;
        SetLength(ModelCallbacks[I].ObjectList,0);
      end
    ;

    tempXMLReader := TXMLReader.create( @ModelCallbacks, Sdew, Element, extData );
    tempXMLReader.Run();
    tempXMLReader.free();

    result := nil;
  end
;


function ProcessParamsFile( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    elementName:String;
    _smInput:TSMInputPtr;
    i, elementCount:integer;
    ret_val :TParamsTag;
  begin
    ret_val := nil;
    _smInput := TSMInputPtr( extData );

    elementName := Sdew.GetElementName( Element );

    if ( elementName = 'description' ) then
      _smInput^.scenarioDescr := decodeXml( Sdew.GetElementContents( Element ) )
    else if ( elementName = 'num-days' ) then
      begin      //  This extra testing here is because this code allows the
                 //  elements of this section of the file to appear in any order...
        _smInput^.simDays := StrToInt( Sdew.GetElementContents( Element ) );
        if ( ( 32767 = _smInput^.simDays ) AND ( _smInput^.simStopReason <> ssStopAtFirstDetection ) AND ( _smInput^.simStopReason <> ssStopAtDiseaseEnd ) ) then
          _smInput^.simStopReason := ssStopAtEndOfOutBreak
        else if ( 32767 <> _smInput^.simDays ) then
          _smInput^.simStopReason := ssStopAtSpecificDay;
      end
    else if ( elementName = 'num-runs' ) then
      _smInput^.simIterations := StrToInt( Sdew.GetElementContents( Element ) )
    else if ( elementName = 'exit-condition' ) then
      begin
        elementCount := Sdew.GetElementCount( Element );
        //  We will give precedence to these conditions, regardless of what the
        //  stop condition may have been set to previously.  This is because the
        //  num-days element is not a part of the exit-condition tag structure,
        //  and could be used to set the stop condition, eroneously, prior to this
        //  code being run...
        for i := 0 to  elementCount - 1 do
          begin
            if ( 'first-detection' = Sdew.GetElementNameByIndex( Element, i ) ) then
              _smInput^.simStopReason := ssStopAtFirstDetection
            else if ( 'disease-end' = Sdew.GetElementNameByIndex( Element, i ) ) then
              _smInput^.simStopReason := ssStopAtDiseaseEnd
          end
        ;
      end
    else
      begin
        ret_val := TParamsTag.create( elementName, Element );
      end
    ;

    result := ret_val;
  end
;


constructor TXMLConvert.create( HerdsFilename: String; ScenarioFilename: String; smScenario:TSMScenarioPtr );
  begin
    inherited create();
    errorMessage := '';

    _populateScenario := false;
    _smScenario := smScenario;
    _smInput := _smScenario^.simInputPtr;
    _sfilename := ScenarioFilename;
    _hfilename := HerdsFilename;
  end
;

constructor TXMLConvert.create( HerdsFilename:String; ScenarioFilename:String; smInput:TSMInputPtr );
  begin
    inherited create();
    errorMessage := '';

    _populateScenario := false;
    _smScenario := nil;
    _smInput := smInput;
    _sfilename := ScenarioFilename;
    _hfilename := HerdsFilename;
  end
;

destructor TXMLConvert.destroy();
  begin
    inherited destroy();
  end
;





  ///////////////////////////////////////////////////////////////////////////////
 //  This function reads a Herd XML file and fills an hlist for use elsewhere
/////////////////////////////////////////////////////////////////////////
function TXMLConvert.ReadHerdXml( _hList:THerdList = nil; err: pstring = nil ): boolean;
  var
    XMLReader: TXMLReader;
    MyCallbacks: CallbackArray;
    I: Integer;
    J: Integer;
    _h: THerd;
    _xmlHerd: TxmlHerd;
    pt: TProductionType;
    ret_val: boolean;
  begin
    ret_val := true;

    if (  nil = _smInput ) then
      begin
        errorMessage := errorMessage + tr( ansiReplaceStr( 'Cannot proceed with herd XML import.  An error (type xyz) has occurred.  Please check with the developers.', 'xyz', '1' ) );
        ret_val := false;
      end
    else
      if ( not assigned( _smInput^ ) ) then
      begin
        errorMessage := errorMessage + tr( ansiReplaceStr( 'Cannot proceed with herd XML import.  An error (type xyz) has occurred.  Please check with the developers.', 'xyz', '2' ) );
        ret_val := false;
      end
    ;

    if ( ret_val ) then
      begin
        SetLength( MyCallbacks, 1);
        MyCallbacks[0].Tag := 'herd';
        MyCallbacks[0].Callback := ProcessHerd;
        SetLength(MyCallbacks[0].ObjectList, 0);

        if( nil <> @_fnProgressMessage ) then
          _fnProgressMessage( tr( 'Reading units...' ) )
        ;
        if( nil <> @_fnProgressSet ) then
          _fnProgressSet( 0 )
        ;

        XMLReader := TXMLReader.create(@MyCallbacks, _hfilename, _smInput, nil (*frmProgress*) );
        XMLReader.Run();
        XMLReader.free();

        if( nil <> @_fnProgressSet ) then
          _fnProgressSet( 0 )
        ;
        if( nil <> @_fnProgressMessage ) then
          _fnProgressMessage( tr( 'Building units...' ) )
        ;

        if ( _hList = nil ) then
          begin
             ret_val := false;
             // Invalid herd list pointer passed to ReadHerdXML, can not continue
             errorMessage := errorMessage + tr( ansiReplaceStr( 'Cannot proceed with herd XML import.  An error (type xyz) has occurred.  Please check with the developers.', 'xyz', '3' ) );
          end
        ;


        if ( nil <> _hList ) then
          begin
            for I := 0 to length( MyCallbacks ) - 1 do
              begin
                if( nil <> @_fnProgressSet ) then
                  _fnProgressSet( 0 )
                ;

                for J := 0 to length( MyCallbacks[I].ObjectList ) - 1 do
                  begin
                    if( nil <> @_fnProgressSet ) then
                      _fnProgressSet( (((J+1) * 100) div length( MyCallbacks[I].ObjectList )) )
                    ;

                    _xmlHerd := TxmlHerd(MyCallbacks[I].ObjectList[J]);

                    if( nil <> @_fnProgressMessage ) then
                      _fnProgressMessage( ansiReplaceStr( tr( 'Building unit with ID xyz...' ), 'xyz', _xmlHerd.id ) )
                    ;

                    _h := THerd.create( _hList );

                    if( _populateScenario ) then
                      begin
                        pt := _smInput^.findProdType( _xmlHerd.prodType );

                        if( nil = pt ) then
                          begin
                            pt := TProductionType.create( -1, _xmlHerd.prodType, _smInput^ );
                            pt.populateDatabase( _smInput^.database, MDBAAuto );
                            _smInput^.ptList.Append( pt );
                          end
                        ;

                        _h.setProdType( pt );
                      end
                    else
                      _h.prodTypeName := _xmlHerd.prodType
                    ;

                    _h.setLatLon( _xmlHerd.location.GetLatitude(), _xmlHerd.location.GetLongitude() );
                    _h.initialSize := _xmlHerd.size;
                    _h.initialStatus := naadsmDiseaseStateFromString( _xmlHerd.status );
                    _h.daysInInitialState := _xmlHerd.daysInInitialState;
                    _h.daysLeftInInitialState := _xmlHerd.daysLeftInInitialState;
                    _h.simParams := _smInput^;
                                
                    _hList.append( _h );

                    if( _populateScenario ) then
                      _h.simParams := _smInput^
                    ;
                  end
                ;
                _hlist.initializeAllOutputRecords();  
                if( _populateScenario ) then
                  begin
                    dbcout( 'Populating database with herd list', true );
                    _hList.populateDatabase( _smInput^.database );
                    dbcout( 'Recounting Units', true );
                    _smInput^.ptList.recountUnits( _hList );
                  end
                ;
              end
            ;
          end
        ;
      end
    ;
    if ( length( errorMessage ) > 0 ) then
      begin
        ret_val := false;
        if( nil <> err ) then
          err^ := err^ + errorMessage
        ;
      end
    ;
      
    result := ret_val;
  end
;


procedure TXmlConvert.convertScenario( err: pstring );
  var
    XMLReader: TXMLReader;
    MyCallbacks: CallbackArray;
    J,z: Integer;
    pt: TProductionType;
    destrReasonStr: String;
    index:Integer;
    listlength: Integer;
    tempStr:String;
    maxPriority: Int64;
    prodTypeDest:TProductionType;
    prodTypeList:TProductionTypeList;
    priorityList:TQStringList;
    reasons:Array of String;
    destPriorities: Array of Array of Integer;
    totalProgressSteps, currentProgressStep: integer;
    statMethod:TChartFunction;
    haveVersion, isPriorVersion: boolean;
    {$IFNDEF CONSOLEAPP}
    msg: string;
    {$ENDIF}
  begin
    totalProgressSteps := 8;
    currentProgressStep := 0;

    if( nil <> @_fnProgressMessage ) then
      _fnProgressMessage( tr( 'Reading scenario parameters...' ) )
    ;
    if( nil <> @_fnProgressSet ) then
      begin
        _fnProgressSet( trunc( currentProgressStep / totalProgressSteps )  );
        inc( currentProgressStep );
      end
    ;

    _internalDestructionPriorityList := TQStringLongIntMap.create();
    _destructionPriorityList := TQStringList.create();
    _internalVaccinationPriorityList := TQStringLongIntMap.create();

    //'basic,direct-forward,indirect-forward,ring,direct-back,indirect-back'  As a default order...
    _internalDestructionPriorityList.insert('basic', 1000);
    _internalDestructionPriorityList.insert('direct-forward', 1001);
    _internalDestructionPriorityList.insert('indirect-forward', 1002);
    _internalDestructionPriorityList.insert('ringdirect-back', 1003);
    _internalDestructionPriorityList.insert('direct-back', 1004);
    _internalDestructionPriorityList.insert('indirect-back', 1005);

    _internalVaccinationPriorityList.insert('ring', 1003);

    SetLength( MyCallbacks, 2);
    MyCallbacks[0].Tag := '*';
    MyCallbacks[0].Callback := ProcessParamsFile;
    SetLength(MyCallbacks[0].ObjectList, 0);

    MyCallbacks[1].Tag := 'models';
    MyCallbacks[1].Callback := ProcessModels;
    SetLength(MyCallbacks[1].ObjectList, 0);

    //Output section??  Do we want to import this also?

    

    XMLReader := TXMLReader.create( @MyCallbacks, _sfilename, _smInput, nil (*frmProgress*) );
    XMLReader.Run();
    XMLReader.free();

    if( nil <> @_fnProgressMessage ) then
      _fnProgressMessage( tr( 'Processing scenario parameters...' ) )
    ;

    haveVersion := false; //  Use for XML files without version tags
    isPriorVersion := false;  //  Use for XML files with version tags
    
    for Index := 0 to length( MyCallbacks ) -1 do
      begin
        for z := 0 to length( MyCallbacks[Index].ObjectList ) - 1 do
          begin
            if ( 'naadsm-version' = (MyCallbacks[Index].ObjectList[z] as TParamsTag).getName() ) then
              begin
{*  TODO 1 -cXML Input -oscase:  It would be best to also check the version numbers, if found, for the same reasons, i.e. prior versions may have
           less parameters or different understandings of parameters....
           
                if (
    MAJORVERSION = '3';
    MINORVERSION = '2';
    RELEASENUMBER = '1';

    <major-version>3</major-version>
    <minor-version>2</minor-version>
    <release>0</release>
*}
                haveVersion := true;
              end
            ;

            MyCallbacks[Index].ObjectList[z].Free();
          end
        ;
      end
    ;
    
    // Handle ring-vaccine priority list here...check list for -1 priorities and convert them
    // to end of list....  (check list for highest priority value, increment this value for each -1 priority
    // and assign it to that prodtype in the list, replacing its -1 value...)
    //-------------------------------------------------------------------------------------------------------
    if( nil <> @_fnProgressSet ) then
      begin
        _fnProgressSet( trunc( currentProgressStep / totalProgressSteps )  );
        inc( currentProgressStep );
      end
    ;

    priorityList := TQStringList.create();
    prodTypeList := _smInput^.ptList;

    for index := 0 to prodTypeList.Count - 1 do
      begin
        pt := prodTypeList.at( index );
        if ( 0 < pt.ringVaccParams.vaccPriority )  then
          priorityList.insert( pt.ringVaccParams.vaccPriority, pt.productionTypeDescr )
        ;
      end
    ;

    maxPriority := 0;

    for index:= 0 to priorityList.count - 1 do
      begin
        tempStr := priorityList.at( index );

        if ( length( tempStr ) > 0 ) then
          begin
            prodTypeDest := prodTypeList.findProdType( tempStr );
            if ( assigned( prodTypeDest ) ) then
              begin
                inc( maxPriority );
                prodTypeDest.ringVaccParams.vaccPriority := maxPriority;
                prodTypeDest.vaccinationParams.useVaccination := true;
                prodTypeDest.ringVaccParams.updated := true;
                _smInput^.controlParams.ssVaccPriorities.item[ tempStr + '+' + 'ring' ] := maxPriority;
              end
            ;
          end
        ;
      end
    ;

    for index := 0 to prodTypeList.Count - 1 do
      begin
        if ( prodTypeList.at( index ).ringVaccParams.vaccPriority <=0 ) then
          begin
            // inc( maxPriority );
            prodTypeDest := prodTypeList.at( index );
            prodTypeDest.ringVaccParams.vaccPriority := -1; //maxPriority;
            prodTypeDest.vaccinationParams.useVaccination := false;
            prodTypeDest.ringVaccParams.updated := true;
            _smInput^.controlParams.ssVaccPriorities.item[ prodTypeDest.productionTypeDescr + '+' + 'ring' ] := -1;
          end
        ;
      end
    ;

    //  Rebuild the Reason's list for destruction in order.
    //-----------------------------------------------------
    if( nil <> @_fnProgressSet ) then
      begin
        _fnProgressSet( trunc( currentProgressStep / totalProgressSteps )  );
        inc( currentProgressStep );
      end
    ;

    listlength := 6;
    setLength( reasons, 6 );

    while( 0 < listlength ) do
      begin
        tempStr := '';
        for index := 0 to listlength - 1 do
          begin
            if ( index = 0 ) then
              tempStr := _internalDestructionPriorityList.keyAtIndex( index )
            else
              begin
                if ( _internalDestructionPriorityList.itemAtIndex( index ) < _internalDestructionPriorityList.value( tempStr ) ) then
                  tempStr := _internalDestructionPriorityList.keyAtIndex( index )
                ;
              end
            ;
          end
        ;

        reasons[ 6 - listLength ] := tempStr;
        destrReasonStr := destrReasonStr + tempStr;

        if ( listlength > 1) then
          destrReasonStr := destrReasonStr + ','
        ;

        _internalDestructionPriorityList.remove( tempStr );
        listlength := listlength - 1;
      end
    ;

    _smInput^.controlParams.destrReasonOrder := destrReasonStr;


    // Put the reasons in this array in order and fill in missing dictionary values.
    //------------------------------------------------------------------------------
    if( nil <> @_fnProgressSet ) then
      begin
        _fnProgressSet( trunc( currentProgressStep / totalProgressSteps )  );
        inc( currentProgressStep );
      end
    ;

    setLength( destPriorities, 6, prodTypeList.Count );
    listLength := 6;


    // Verify destruction entries in dictionary for all production types and for all reasons.
    //---------------------------------------------------------------------------------------
    if( nil <> @_fnProgressSet ) then
      begin
        _fnProgressSet( trunc( currentProgressStep / totalProgressSteps )  );
        inc( currentProgressStep );
      end
    ;

    for index := 0 to prodTypeList.Count - 1 do
      begin
        for j := 0 to  listLength - 1 do
          begin
            if( not _smInput^.controlParams.ssDestrPriorities.contains( prodTypeList.at( index ).productionTypeDescr + '+' + reasons[j]  )  ) then
              begin
                _smInput^.controlParams.ssDestrPriorities.Add( prodTypeList.at( index ).productionTypeDescr + '+' + reasons[j], -1 );
                destPriorities[j, index] := -1;
              end
            else
              begin
                destPriorities[j, index] := _smInput^.controlParams.ssDestrPriorities.Item[ prodTypeList.at( index ).productionTypeDescr + '+' + reasons[j]];
              end
            ;
          end
        ;
      end
    ;

    //  destPriorities matrix is probably not needed, use this code to re-align priority numbers.
    //  The above matrix code is not saved in CVS anywhere, so leave it here for reference.  If removing
    //  it, make sure to leave the portion of it that sets missing entriy priorities to (-1).
{*
    listLength := 0;
    for index := 0 to _destructionPriorityList.count - 1 do
      begin
        inc( listLength );
        _smInput^.controlParams.ssDestrPriorities.Add( _destructionPriorityList.at( index ), listLength );
      end
    ;
*}
    //  Turns out the destPriorities matrix IS needed....use the code below
    //  from now on to be sure to re-align the priorities.
    //----------------------------------------------------
    if( nil <> @_fnProgressSet ) then
      begin
        _fnProgressSet( trunc( currentProgressStep / totalProgressSteps )  );
        inc( currentProgressStep );
      end
    ;

    for index := 0 to prodTypeList.Count - 1 do
      begin
        for j := 0 to  listLength - 1 do
          _smInput^.controlParams.ssDestrPriorities.Add( prodTypeList.at( index ).productionTypeDescr + '+' + reasons[j], destPriorities[j, index] )
        ;
      end
    ;

    _internalDestructionPriorityList.Free();
    _destructionPriorityList.Free();
    _internalVaccinationPriorityList.Free();



    //  NAADSM 3.2.0 addition:
    //  Check production types for missing tracing parameters in case this was
    //  an XML file from a previous version.

    //  This section checks for missing Tracing_Delay_PDFs, and assigns the
    //  default of no-delay.
    for index := 0 to prodTypeList.Count - 1 do
      begin
        prodTypeDest := prodTypeList.at( index );
        if ( ( prodTypeDest.tracingParams.pdfTraceDelayName = '' ) or ( prodTypeDest.tracingParams.pdfTraceDelay = nil ) ) then
          begin
            if ( not _smInput^.functionDictionary.contains( 'No tracing delay [NAADSM update default]' ) ) then
              begin
                statMethod := TChartFunction( TPDFPoint.create( 0 ) );
                statMethod.name := 'No tracing delay [NAADSM update default]';
                statMethod.dbField := word( TrDelay );
                _smInput^.functionDictionary.checkAndInsert( statMethod );
               end
            ;
            prodTypeDest.tracingParams.pdfTraceDelayName := 'No tracing delay [NAADSM update default]'
          end
        ;
      end
    ;
    //  End 3.2.0 additions.
    

    // This should now be the only time it's necessary to populate the database during an XML import!
    //-----------------------------------------------------------------------------------------------
    if( nil <> @_fnProgressMessage ) then
      _fnProgressMessage( tr( 'Populating database...' ) )
    ;
    if( nil <> @_fnProgressSet ) then
      _fnProgressSet( trunc( currentProgressStep / totalProgressSteps )  )
    ;
    
    _smInput^.populateDatabase( MDBAForceInsert ); // this will force inserts rather than updates

    if( nil <> @_fnProgressSet ) then
      _fnProgressSet( 100  )
    ;

    {$IFNDEF CONSOLEAPP}
      if ( ( not haveVersion ) or isPriorVersion )then
        begin
          msg :=
            tr( 'This XML file was generated with an older version of NAADSM.' )
            + endl + endl
            + tr( 'It is important that you check your double-check your scenario parameters, as new parameters have been added.' )
          ;
          msgOK(
            msg,
            SHORTMASTERCAPTION,
            IMGInformation,
            frmMain
          );
        end
      ;
    {$ENDIF}
        
  end
;



procedure TXMLConvert.convertHerdList( err: pstring );
  begin
    _populateScenario := true;
    readHerdXml( _smScenario^.herdList, err );

//    if ( not _smScenario^.herdList.isProjected ) then
      _smScenario^.herdList.setProjection( _smScenario^.herdList.defaultProjection( _smScenario^.herdList.minLat, _smScenario^.herdList.minLon, _smScenario^.herdList.maxLat, _smScenario^.herdList.maxLon ) );
    _populateScenario := false;
  end
;


  ///////////////////////////////////////////////////////////////////////////////
 //  This function begins the processing of the disease-model XML file
/////////////////////////////////////////////////////////////////////////
procedure TXMLConvert.ConvertXMLToDatabase( err: pstring = nil );
  {$IFNDEF CONSOLEAPP}
  var
    frmProgress:TFormProgress;
    {$ENDIF}
  begin
    if( ( 0 = length( _sfilename ) ) and ( 0 = length( _hfilename ) ) ) then
      exit
    ;

    {$IFNDEF CONSOLEAPP}
      frmProgress := TFormProgress.create( application.MainForm, PRSingleBar, false );
      _fnProgressMessage := frmProgress.setMessage;
      _fnProgressSet := frmProgress.setPrimary;
      frmProgress.show();
    {$ELSE}
      _fnProgressMessage := nil;
      _fnProgressSet := nil;
    {$ENDIF}

    try
      if ( 0 < length( _sfilename ) ) then
        convertScenario( err )
      ;
    
      if( 0 < length( _hfilename ) ) then
        begin
          convertHerdList( err );
        end
      ;
    finally
      {$IFNDEF CONSOLEAPP}
        frmProgress.Release();
      {$ENDIF}
    end;
  end
;

end.
