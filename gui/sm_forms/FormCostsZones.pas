unit FormCostsZones;

(*
FormCostsZones.pas/dfm
----------------------
Begin: 2007/04/18
Last revision: $Date: 2013-06-27 19:11:25 $ $Author: areeves $
Version number: $Revision: 1.7.4.2 $
Project: (various)
Website: http://www.naadsm.org
Author: Aaron Reeves <aaron.reeves@naadsm.org>
--------------------------------------------------
Copyright (C) 2007 - 2008 Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
    // Standard includes
    Windows,
    Messages,
    SysUtils,
    Variants,
    Classes,
    Graphics,
    Controls,
    Forms,
    StdCtrls,
    Buttons,
    ExtCtrls,
    Dialogs,
    Menus,
    ActnPopupCtrl,

    Zone,
    ProductionType,

    // Widgets
    FrameCostsZones,
    FormProdTypeBase,
    FormSMWizardBase
  ;


  type TFormCostsZones = class( TFormProdTypeBase )
      fraParams: TFrameCostsZones;

    protected
      _frameSetupComplete: boolean;

      procedure translateUI();

      function getZoneList(): TZoneList;

      procedure updateDisplay(); override;

      function dataIsValid(): boolean; override;

      procedure giveListsToEditors(); override;
      procedure prepFunctionDicts(); override;

      procedure copyParameters( const src: TProductionType; dest: TProductionType ); override;
      
    public
      constructor create( Aowner: TComponent ); override;
      destructor destroy(); override;

      function showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer; override;

      property zoneList: TZoneList read getZoneList;
    end
  ;


implementation

{$R *.dfm}

  uses
    QStringMaps,
    
    FunctionEnums,
    FunctionDictionary,
    ProdTypeZoneParams,
    I88n,
    
    FrameZoneProdTypeParams
  ;

  const
    DBFORMCOSTSZONES: boolean = false; // set to true to enable debugging messages for this unit.
    

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TFormCostsZones.create( Aowner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();
      
      fraParams.Visible := false;
      lblProdType.Caption := '';

      _frameSetupComplete := false;
    end
  ;


  procedure TFormCostsZones.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:56 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormCostsZones.dfm
      // File date: Wed Apr 25 11:56:53 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Scenario parameters: Costs of zone surveillance' );
          pnlCaption.Caption := tr( 'Costs of zone surveillance' );
        end
      ;

    end
  ;


  destructor TFormCostsZones.destroy();
    begin
      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Display functions
//-----------------------------------------------------------------------------
  function TFormCostsZones.showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer;
    begin
      // need detection of disease in order to trigger Surveillance and incurr cost
      if ( _smScenarioCopy.simInput.includeDetectionGlobal and _smScenarioCopy.simInput.includeZonesGlobal and _smScenarioCopy.simInput.costTrackZoneSurveillance ) then
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


  procedure TFormCostsZones.updateDisplay();
    begin
      if( nil <> _selectedPT ) then
        begin
          lblProdType.Caption := _selectedPT.productionTypeDescr;
          fraParams.Visible := true;
          fraParams.prodType := _selectedPT;
        end
      else
        begin
          fraParams.visible := false;
          lblProdType.Caption := '';
        end
      ;
    end
  ;


  procedure TFormCostsZones.prepFunctionDicts();
    begin
      // There are no function lists to prepare, but this is
      // still a convenient place to set up the parameter frame.
      if( not( _frameSetupComplete ) ) then
        begin
          fraParams.initializeFrameDisplay();
          _frameSetupComplete := true;
        end
      ;
    end
  ;

  
  procedure TFormCostsZones.giveListsToEditors();
    begin
      // There are no function editors on this form, so do nothing
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Data and database functions
//-----------------------------------------------------------------------------
  function TFormCostsZones.dataIsValid(): boolean;
    begin
      result := fraParams.isValid();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
  function TFormCostsZones.getZoneList(): TZoneList;
    begin
      if( nil = self._smScenarioCopy ) then
        result := nil
      else
        result := self._smScenarioCopy.simInput.zoneList
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Copying parameters
//-----------------------------------------------------------------------------
  procedure TFormCostsZones.copyParameters( const src: TProductionType; dest: TProductionType );
    var
      it: TZPTListIterator;
      zptDest, zptSrc: TZoneProdTypeComboParams;
    begin
      it := TZPTListIterator.create( src.zoneParams.zonePtParamsList );

      while( nil <> it.value() ) do
        begin
          zptSrc := it.value();
          zptDest := dest.zoneParams.zonePtParamsList[ it.key()];

          zptDest.costSurvPerAnimalDay := zptSrc.costSurvPerAnimalDay;

          it.incr();
        end
      ;

      it.free();

      dest.updated := true;
    end
  ;
//-----------------------------------------------------------------------------


initialization

  RegisterClass( TFormCostsZones );

end.
