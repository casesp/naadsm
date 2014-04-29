unit FormDetection;

(*
FormDetection.pas/dfm
---------------------
Begin: 2005/06/08
Last revision: $Date: 2009-07-12 23:48:57 $ $Author: areeves $
Version: $Revision: 1.34 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2005 - 2009 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
    // Standard controls
    Forms,
    StdCtrls,
    Controls,
    Classes,
    ExtCtrls,
    Buttons,
    Menus,
    ActnPopupCtrl,

    // Widgets
    FormProdTypeBase,
    FrameDetection,
    FormSMWizardBase,

    // Data structures
    ProductionType,
    FunctionDictionary
  ;

  type TFormDetection = class( TFormProdTypeBase )
      fraParams: TFrameDetection;

    	procedure cbxDetectClick(Sender: TObject);

    protected
      _ignoreClick: boolean;
      
      procedure translateUI();
      
  		procedure updateDisplay(); override;

  		procedure giveListsToEditors(); override;
      procedure prepFunctionDicts(); override;

      function dataIsValid(): boolean; override;

      procedure copyParameters( const src: TProductionType; dest: TProductionType ); override;

    public
    	constructor create( Aowner: TComponent ); override;
      destructor destroy(); override;

			function showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer; override;

    end
  ;


  const
  	DBFORMDETECTION: boolean = false; // set to true to enable debugging for this unit.

implementation

{$R *.dfm}

  uses
    SysUtils,
    FormMain,
    MyStrUtils,
    ChartFunction,
    ControlUtils,
    FunctionEnums,
    QStringMaps,
    I88n
  ;

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TFormDetection.create( Aowner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();
      
      fraParams.cbxDetect.visible := false;
      fraParams.pnlDetectionParams.Visible := false;
      lblProdType.Caption := '';

      _ignoreClick := false;
    end
  ;


  procedure TFormDetection.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:55 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormDetection.dfm
      // File date: Fri May 11 10:34:49 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Scenario parameters: Detection' );
          pnlCaption.Caption := tr( 'Detection' );
        end
      ;

    end
  ;


  destructor TFormDetection.destroy();
  	begin
   		inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Display functions
//-----------------------------------------------------------------------------
	function TFormDetection.showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer;
  	begin
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

  
  procedure TFormDetection.updateDisplay();
  	begin
    	if( nil <> _selectedPT ) then
      	begin
          lblProdType.Caption := ( _selectedPT.productionTypeDescr );
          fraParams.pnlDetectionParams.Visible := _selectedPT.useDetection;
          fraParams.cbxDetect.visible := true;

          // Setting the checkbox "checked" property triggers the checkbox "clicked" event, which is silly...
          _ignoreClick := true;
          fraParams.cbxDetect.Checked := _selectedPT.useDetection;
          _ignoreClick := false;

          if( _selectedPT.useDetection ) then
            begin
            	fraParams.smrProbReportVsDaysInfectious.showChart(
              	_selectedPT,
                _selectedPT.detectionParams.relObsVsTimeClinical,
                DetProbObsVsTimeClinical
              );

              fraParams.smrProbVsFirstDetection.showChart(
              	_selectedPT,
                _selectedPT.detectionParams.relReportVsFirstDetection,
                DetProbReportVsFirstDetection
              );
            end
          ;
        end
      else
        begin
          fraParams.pnlDetectionParams.visible := false;
      		fraParams.cbxDetect.visible := false;
          lblProdType.Caption := '';
      	end
      ;
    end
  ;


  procedure TFormDetection.prepFunctionDicts();
  	var
      it: TFunctionDictionaryIterator;
  	begin
			fraParams.smrProbReportVsDaysInfectious.ClearList();
      fraParams.smrProbVsFirstDetection.ClearList();

      it := TFunctionDictionaryIterator.create( _fnDict );

      repeat
        if( nil <> it.value() ) then
          begin
            if ( not it.value().removed ) then
              begin
                case( it.value().fn.dbField ) of
                  integer( DetProbReportVsFirstDetection ): fraParams.smrProbVsFirstDetection.appendFunction( it.value().fn );
                  integer( DetProbObsVsTimeClinical ): fraParams.smrProbReportVsDaysInfectious.appendFunction( it.value().fn );
                end;
              end
            ;
          end
        ;

        it.incr();
      until ( nil = it.value() );

      it.Free();
    end
  ;

  
  procedure TFormDetection.giveListsToEditors();
  	begin
   		with fraParams do
      	begin
        	smrProbReportVsDaysInfectious.setFunctionDict( _fnDict );
          smrProbVsFirstDetection.setFunctionDict( _fnDict );

          smrProbReportVsDaysInfectious.setModelList( _ptList );
          smrProbVsFirstDetection.setModelList( _ptList );
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI event handling functions
//-----------------------------------------------------------------------------
  procedure TFormDetection.cbxDetectClick(Sender: TObject);
    begin
     	if( not( _ignoreClick ) ) then
        _selectedPT.detectionParams.useDetection := fraParams.cbxDetect.Checked
      ;
     	updateDisplay();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Data and database functions
//-----------------------------------------------------------------------------
  function TFormDetection.dataIsValid(): boolean;
		begin
   		result := true;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Copying parameters
//-----------------------------------------------------------------------------
  procedure TFormDetection.copyParameters( const src: TProductionType; dest: TProductionType );
    begin
      dest.detectionParams.useDetection := src.detectionParams.useDetection;

      dest.setChart( DetProbObsVsTimeClinical, src.chart( DetProbObsVsTimeClinical ) );
      dest.setChart( DetProbReportVsFirstDetection, src.chart( DetProbReportVsFirstDetection ) );

      dest.updated := true;
    end
  ;
//-----------------------------------------------------------------------------


initialization
	RegisterClass( TFormDetection );


end.
