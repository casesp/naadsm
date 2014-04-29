unit FormAirborneSpread;

(*
FormAirborneSpread.pas/dfm
--------------------------
Begin: 2005/05/03
Last revision: $Date: 2008/11/25 22:00:29 $ $Author: areeves $
Version: $Revision: 1.26 $
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
    Controls,
    StdCtrls,
    ExtCtrls,
    Classes,
    Forms,
    Buttons,

    SMDatabase,
    SMScenario,
    ProductionTypePair,
    
    FormSMWizardBase,
    FormProdTypePairBase,
    FrameAirborneSpread, Menus, ActnPopupCtrl
  ;

  type TFormAirborneSpread = class( TFormProdTypePairBase )
    fraParams: TFrameAirborneSpread;

    protected
      procedure translateUI();
    
      // This function needs to be reimplemented in every derived class.
      procedure updateDisplay(); override;

      // These functions need to be reimplemented for any PT pair parameter that involves
      // a PDF or relational function with a function editor.
  		procedure prepFunctionDicts(); override;
      procedure giveListsToEditors(); override;

      function dataIsValid(): boolean; override;

      procedure copyParameters( const src: TProductionTypePair; dest: TProductionTypePair ); override;

    public
			constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

      procedure setParams( smScenario: TSMScenario ); override;

			function showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer; override;
    end
  ;


implementation

{$R *.dfm}

	uses
    SysUtils,

    DebugWindow,
    MyStrUtils,
    GuiStrUtils,
    I88n,
    QStringMaps,
    
    AirborneSpreadModel,
    FunctionDictionary,
    ChartFunction,
    FunctionEnums
  ;

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
	constructor TFormAirborneSpread.create( AOwner: TComponent );
  	begin
      inherited create( AOwner );
      translateUI();
      
    	_fnList := nil;
    end
  ;


  procedure TFormAirborneSpread.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:28:38 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormAirborneSpread.dfm
      // File date: Fri Jan 19 17:43:53 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Scenario parameters: Airborne spread' );
          pnlCaption.Caption := tr( 'Airborne spread' );
        end
      ;

    end
  ;


	destructor TFormAirborneSpread.destroy();
  	begin
      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Function-handling functions reimplemented from TFormProdTypePairBase
//-----------------------------------------------------------------------------
  procedure TFormAirborneSpread.prepFunctionDicts();
  	var
    	i: integer;
      ptp: TProductionTypePair;
      it: TFunctionDictionaryIterator;
  	begin
      fraParams.smcTransportDelay.cboChartList.clear();

      it := TFunctionDictionaryIterator.create( _fnList );

      repeat
        if( nil <> it.value() ) then
          begin
            if ( not it.value().removed ) then
              begin
                if ( it.value().fn.dbField = word(TSMChart( AIRDelay ) ) ) then
                  begin
                    if ( nil <> it.value().fn ) then
                      begin
                        fraParams.smcTransportDelay.appendFunction( it.value().fn );
                        it.value().RefCounter := 0;
                      end;
                  end;
              end;
          end
        ;

        it.incr();
      until ( nil = it.value() );

      it.Free();

      for i := 0 to _ptpList.Count-1 do
      	begin
        	ptp := _ptpList.at(i);

          if( nil <> ptp.airborne ) then
          	if( nil <> ptp.airborne.pdfDelay ) then _fnList.value( ptp.airborne.delayName ).incrRefCounter()
          ;
        end
      ;
    end
  ;


  procedure TFormAirborneSpread.giveListsToEditors();
  	begin
    	fraParams.smcTransportDelay.setFunctionDict( _fnList );
      fraParams.smcTransportDelay.setModelList( _ptpList );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Parameter handling functions reimplemented from TFormProdTypePairBase
//-----------------------------------------------------------------------------
  procedure TFormAirborneSpread.setParams( smScenario: TSMScenario );
    begin
      inherited setParams( smScenario );

      fraParams.useAirborneExponentialDecay := _smScenarioCopy.simInput.useAirborneExponentialDecay;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Display functions reimplemented from TFormProdTypePairBase
//-----------------------------------------------------------------------------
	function TFormAirborneSpread.showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer;
  	begin
    	if( _smScenarioCopy.simInput.includeAirborneSpreadGlobal ) then
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


	procedure TFormAirborneSpread.updateDisplay();
  	begin
    	if( nil <> _selectedPTP ) then
      	begin
          if( nil = _selectedPTP.airborne ) then
            _selectedPTP.airborne := TAirborneSpreadModel.create( _selectedPTP.sim, _selectedPTP.dest.productionTypeID, _selectedPTP.source.productionTypeID )
          ;

        	fraParams.Visible := true;
          fraParams.smcTransportDelay.showChart( _selectedPTP, _selectedPTP.airborne.pdfDelay, AIRDelay );

          fraParams.airborneSpreadModel := _selectedPTP.airborne;
    			// Setting the property above will automatically update the parameter display
        end
      else
      	begin
      		fraParams.Visible := false;
          fraParams.airborneSpreadModel := nil;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Data validation
//-----------------------------------------------------------------------------
	function TFormAirborneSpread.dataIsValid(): boolean;
  	begin
    	result := fraParams.isValid();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Copying parameters
//-----------------------------------------------------------------------------
  procedure TFormAirborneSpread.copyParameters( const src: TProductionTypePair; dest: TProductionTypePair );
    begin
      dest.airborne.useAirborne := src.airborne.useAirborne;

      dest.airborne.maxSpread := src.airborne.maxSpread;
      dest.airborne.probSpread1km := src.airborne.probSpread1km;

      dest.airborne.windStart := src.airborne.windStart;
      dest.airborne.windEnd := src.airborne.windEnd;

      dest.setChart( AIRDelay, src.chart( AIRDelay ) );

      dest.updated := true;
    end
  ;
//-----------------------------------------------------------------------------


initialization
	registerClass( TFormAirborneSpread );

end.

