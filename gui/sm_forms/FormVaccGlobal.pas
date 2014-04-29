unit FormVaccGlobal;

(*
FormVaccGlobal.pas/dfm
-----------------------
Begin: 2005/06/10
Last revision: $Date: 2008/11/25 22:00:30 $ $Author: areeves $
Version: $Revision: 1.27 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2005 - 2008 Animal Population Health Institute, Colorado State University

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
    ExtCtrls,
    Menus,
    ActnPopupCtrl,

    FormSMWizardBase,
    FrameVaccGlobal,
    GlobalControlParams,
    FunctionDictionary,
    GlobalControlParamsList
  ;


  type TFormVaccGlobal = class( TFormSMWizardBase )
    pnlCaption: TPanel;
    pnlBody: TPanel;
    fraParams: TFrameVaccGlobal;

  	protected
    	_ctrlParams: TGlobalControlParams;
      _ctrlList: TGlobalControlParamsList;

      _relList: TFunctionDictionary;

      procedure translateUI();

      procedure giveListsToEditors();
      procedure updateDisplay();

//      procedure initializeFromDatabase(); override;
//      procedure updateDatabase(); override;
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

			property ctrlParams: TGlobalControlParams read getCtrlParams write setCtrlParams;
    end
  ;

  const
  	DBFORMVACCGLOBAL: boolean = false; // set to true to enable debugging messages for this unit.

implementation

{$R *.dfm}

	uses
  	ControlUtils,
    MyStrUtils,
    GuiStrUtils,
    DebugWindow,
    I88n,

    ChartFunction,
    QStringMaps,
    SMSimulationInput,
    
    FunctionEnums
  ;

//-----------------------------------------------------------------------------
// construction/initialization/destruction
//-----------------------------------------------------------------------------
	constructor TFormVaccGlobal.create( AOwner: TComponent );
  	begin
      inherited create( AOwner );
      translateUI();

      fraParams.smrVaccCapacity.xUnits := UnitsDays;
      fraParams.smrVaccCapacity.yUnits := UnitsHerdsPerDay;
    end
  ;


  procedure TFormVaccGlobal.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:54 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormVaccGlobal.dfm
      // File date: Mon Mar 19 11:05:33 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Scenario parameters: Global vaccination options' );
          pnlCaption.Caption := tr( 'Global vaccination options' );
        end
      ;

    end
  ;


  destructor TFormVaccGlobal.destroy();
    begin
      _ctrlList.Extract( _ctrlParams );
      freeAndNil( _ctrlList );

    	inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



  procedure TFormVaccGlobal.initializeFromSim();
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

	// copied verbatim from FormDestrGlobal
  {*
  procedure TFormVaccGlobal.initializeFromDatabase();
  	begin
    	_ctrlParams := TGlobalControlParams.create( _smdb, _smScenario.simInput );
      fraParams.ctrlParams := _ctrlParams;

      _relList := TFunctionDictionary.Create( _smdb, _smScenario.simInput );
      _ctrlList := TGlobalControlParamsList.create();
      _ctrlList.append( _ctrlParams );

      updateMasterDisplay();
    end
  ;


  // copied verbatim from FormDestrGlobal
  procedure TFormVaccGlobal.updateDatabase();
  	var
  		i: integer;
  	begin
    	// Deal with new or changed charts first...
      //-----------------------------------------
    	for i := 0 to _relList.Count-1 do
      	begin
        	if( _relList.GetItemByIndex(i).new ) then
          	begin
           		//dbcout( 'NEW CHART:', DBFORMVACCGLOBAL );
              _relList.GetItemByIndex(i).fn.id := _relList.GetItemByIndex(i).fn.populateDatabase( _smdb );
              //if( DBFORMVACCGLOBAL ) then _relList.GetItemByIndex(i).fn.debug();
            end
          ;
          if( _relList.GetItemByIndex(i).modified ) then
          	begin
           		//dbcout( 'MODIFIED CHART:', DBFORMVACCGLOBAL );
              _relList.GetItemByIndex(i).fn.populateDatabase( _smdb, true );
              //if( DBFORMVACCGLOBAL ) then _relList.GetItemByIndex(i).fn.debug();
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
    	for i := 0 to _relList.Count-1 do
      	begin
          if( _relList.GetItemByIndex(i).removed ) then
          	begin
           		dbcout( 'REMOVED CHART: ' + intToStr( _relList.GetItemByIndex(i).fn.id ), DBFORMVACCGLOBAL );
              _smdb.removeChartFunction( _relList.GetItemByIndex(i).fn.id );
            end
          ;
        end
      ;
    end
  ;
*}

  // copied verbatim from FormDestrGlobal
  function TFormVaccGlobal.dataIsValid(): boolean;
  	begin
    	result := true;
    end
  ;


  function TFormVaccGlobal.getDataUpdated(): boolean;
    begin
      result := false;

      if( _ctrlParams.updated ) then
        begin
          result := true;
          exit;
        end
      ;
      {*
    	for i := 0 to _relList.Count - 1 do
      	begin
          if
            ( _relList.GetItemByIndex(i).new )
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
      *}
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// display
//-----------------------------------------------------------------------------
	procedure TFormVaccGlobal.updateDisplay();
  	begin
   		fraParams.updateDisplay();
    end
  ;


	procedure TFormVaccGlobal.updateMasterDisplay();
  	var
      it: TFunctionDictionaryIterator;
  	begin
    	fraParams.smrVaccCapacity.cboChartList.clear();

      it := TFunctionDictionaryIterator.create( _relList );

      repeat
        if( nil <> it.value() ) then
          begin
            if ( not it.value().removed ) then
              begin
                if ( it.value().fn.dbField = word(TSMChart(VaccCapacityGlobal)) ) then
                  begin
                    fraParams.smrVaccCapacity.appendFunction( it.value().fn );
                    it.value().RefCounter:= 0;
                  end;
              end;
          end
        ;

        it.incr();
      until ( nil = it.value() );

      it.Free();

     if( _relList.contains( ctrlParams.vaccCapacityName ) ) then
      	_relList.value( ctrlParams.vaccCapacityName ).incrRefCounter()
     ;

      giveListsToEditors();

      updateDisplay();
    end
  ;


  procedure TFormVaccGlobal.giveListsToEditors();
  	begin
    	fraParams.smrVaccCapacity.setFunctionDict( _relList );
      fraParams.smrVaccCapacity.setModelList( _ctrlList );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// properties
//-----------------------------------------------------------------------------
  procedure TFormVaccGlobal.setCtrlParams( val: TGlobalControlParams ); begin _ctrlParams := val; end;
  function TFormVaccGlobal.getCtrlParams(): TGlobalControlParams; begin Result := _ctrlParams; end;
//-----------------------------------------------------------------------------



initialization
	RegisterClass( TFormVaccGlobal );



end.
