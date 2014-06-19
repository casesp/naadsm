unit FormDestrGlobal;

(*
FormDestrGlobal.pas/dfm
-----------------------
Begin: 2005/06/10
Last revision: $Date: 2013-06-27 19:11:25 $ $Author: areeves $
Version: $Revision: 1.29.4.2 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <aaron.reeves@naadsm.org>
--------------------------------------------------
Copyright (C) 2005 - 2009 Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
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
    Buttons,
    Menus,
    ActnPopupCtrl,

    FormSMWizardBase,
    ExtCtrls,
    FrameDestrGlobal,
    GlobalControlParams,
    FunctionDictionary,
    GlobalControlParamsList,
    SMSimulationInput
  ;

  type TFormDestrGlobal = class( TFormSMWizardBase )
      pnlCaption: TPanel;
    	fraParams: TFrameDestrGlobal;

  	protected
    	_ctrlParams: TGlobalControlParams;
      _ctrlList: TGlobalControlParamsList;

      _relList: TFunctionDictionary;

      procedure translateUI();

      procedure giveListsToEditors();
      procedure updateDisplay();

      procedure initializeFromSim(); override;
      function dataIsValid(): boolean; override;
      function getDataUpdated(): boolean; override;

      // property getters/setters
      procedure setCtrlParams( val: TGlobalControlParams );
      function getCtrlParams(): TGlobalControlParams;

    public
      constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

      procedure updateMasterDisplay(); override;
      function showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer; override;

			property ctrlParams: TGlobalControlParams read getCtrlParams write setCtrlParams;
    end
  ;


  const
  	DBFORMDESTRGLOBAL: boolean = false; // set to true to enable debugging messages for this unit.

implementation

{$R *.dfm}

	uses
  	ControlUtils,
    MyStrUtils,
    DebugWindow,
    QStringMaps,
    I88n,
    
    ChartFunction,

    FunctionEnums
  ;

//-----------------------------------------------------------------------------
// construction/initialization/destruction
//-----------------------------------------------------------------------------
	constructor TFormDestrGlobal.create( AOwner: TComponent );
  	begin
      inherited create( AOwner );
      translateUI();
      
    	//centerInside( fraParams, self ); // FIX ME: this function sucks.  Figure it out!

      fraParams.smrDestrCapacity.xUnits := UDays;
      fraParams.smrDestrCapacity.yUnits := UHerdsPerDay;
    end
  ;


  procedure TFormDestrGlobal.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:55 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormDestrGlobal.dfm
      // File date: Mon Mar 19 11:05:31 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Scenario parameters: Global destruction options' );
          pnlCaption.Caption := tr( 'Global destruction options' );
        end
      ;

    end
  ;


  destructor TFormDestrGlobal.destroy();
    begin
      // DON'T do this here: since _ctrlParams is a list element, it will be destroyed below.
    	//_ctrlParams.Free();

      // These two private members are nil when the user navigates through the
      // input parameter form wizard and global detection is turned off (the else block of showModal function)
      if assigned( _ctrlParams ) then _ctrlList.Extract( _ctrlParams );
      if assigned(_ctrlList) then freeAndNil( _ctrlList );

    	inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------




  procedure TFormDestrGlobal.initializeFromSim();
    var
      sim: TSMSimulationInput;
    begin
      sim := _smScenarioCopy.simInput;

      _ctrlParams := sim.controlParams;
      fraParams.ctrlParams := _ctrlParams;

      _relList := sim.functionDictionary;
      _ctrlList := TGlobalControlParamsList.create();
      _ctrlList.append( _ctrlParams );
      fraParams.entityList := _ctrlList;
            
      updateMasterDisplay();
    end;

//-----------------------------------------------------------------------------
// reimplemented data handling functions
//-----------------------------------------------------------------------------
{  procedure TFormDestrGlobal.initializeFromDatabase();
  	begin
    	_ctrlParams := TGlobalControlParams.create( _smdb, _smScenario.simInput );
      fraParams.ctrlParams := _ctrlParams;

      _relList := TFunctionDictionary.Create( _smdb, _smScenario.simInput );
      _ctrlList := TGlobalControlParamsList.create();
      _ctrlList.append( _ctrlParams );

      updateMasterDisplay();
    end
  ;


  procedure TFormDestrGlobal.updateDatabase();
  	var
  		i: integer;
  	begin
    	// Deal with new or changed charts first...
      //-----------------------------------------
    	for i := 0 to _relList.Count-1 do
      	begin
        	if( _relList.GetItemByIndex(i).new ) then
          	begin
           		//dbcout( 'NEW CHART:', DBFORMDESTRGLOBAL );
              _relList.GetItemByIndex(i).fn.id := _relList.GetItemByIndex(i).fn.populateDatabase( _smdb );
              //if( DBFORMDESTRGLOBAL ) then _relList.GetItemByIndex(i).fn.debug();
            end
          ;
          if( _relList.GetItemByIndex(i).modified ) then
          	begin
           		//dbcout( 'MODIFIED CHART:', DBFORMDESTRGLOBAL );
              _relList.GetItemByIndex(i).fn.populateDatabase( _smdb, true );
              //if( DBFORMDESTRGLOBAL ) then _relList.GetItemByIndex(i).fn.debug();
            end
          ;

        end
      ;

      // Then update the model entities...
      //-----------------------------------
			if( _ctrlParams.updated ) then _ctrlParams.populateDatabase( _smdb, true );


      // Finally, remove any  deleted charts from the database.
      // (Attempts to delete charts without first updating
      // entities will cause referential integrity errors.)
      //--------------------------------------------------------
    	for i := 0 to _relList.Count - 1 do
      	begin
          if( _relList.GetItemByIndex(i).removed ) then
          	begin
           		dbcout( 'REMOVED CHART: '+ intToStr( _relList.GetItemByIndex(i).fn.id ), DBFORMDESTRGLOBAL );
              _smdb.removeChartFunction( _relList.GetItemByIndex(i).fn.id );
            end
          ;
        end
      ;
    end
  ;
 }

  function TFormDestrGlobal.dataIsValid(): boolean;
  	begin
    	result := true;
    end
  ;


  function TFormDestrGlobal.getDataUpdated(): boolean;
    begin
      result := false;

      if( _ctrlParams.updated ) then
        begin
          result := true;
          exit;
        end
      ;
{
    	for i := 0 to _relList.Count - 1 do
      	begin
          if
            ( _relList  .GetItemByIndex(i).new )
          or
            ( _relList.GetItemByIndex(i).removed )
          then
            begin
              result := true;
              break;
            end
          ;
        end
      ;
}
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// display
//-----------------------------------------------------------------------------
	procedure TFormDestrGlobal.updateDisplay();
  	begin
   		fraParams.updateDisplay();
    end
  ;


	procedure TFormDestrGlobal.updateMasterDisplay();
  	var
      it: TFunctionDictionaryIterator;
  	begin
    	fraParams.smrDestrCapacity.cboChartList.clear();

      it := TFunctionDictionaryIterator.create( _relList );

      repeat
        if( nil <> it.value() ) then
          begin
            if( not it.value().removed ) then
              begin
                if( integer( DestrCapacityGlobal ) = it.value().fn.dbField ) then
                  fraParams.smrDestrCapacity.appendFunction( it.value().fn )
                ;
              end
            ;
          end
        ;

        it.incr();
      until ( nil = it.value() );

      it.Free();

      giveListsToEditors();

      updateDisplay();
    end
  ;


  procedure TFormDestrGlobal.giveListsToEditors();
  	begin
    	fraParams.smrDestrCapacity.setFunctionDict( _relList );
    end
  ;


  function TFormDestrGlobal.showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer;
  	begin
      // need detection of disease in order to conduct destruction campaign
    	if( _smScenarioCopy.simInput.includeDetectionGlobal ) then
    		result := inherited showModal( nextFormToShow, formDisplayed, currentFormIndex )
      else
      	begin
          formDisplayed := false;
      		nextForm := nextFormToShow;
          result := 0;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// properties
//-----------------------------------------------------------------------------
  procedure TFormDestrGlobal.setCtrlParams( val: TGlobalControlParams ); begin _ctrlParams := val; end;
  function TFormDestrGlobal.getCtrlParams(): TGlobalControlParams; begin Result := _ctrlParams; end;
//-----------------------------------------------------------------------------



initialization
	RegisterClass( TFormDestrGlobal );


end.
