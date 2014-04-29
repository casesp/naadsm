unit FormLocalAreaSpread;

(*
FormLocalAreaSpread.pas/dfm
---------------------------
Begin: 2009/11/08
Last revision: $Date: 2009-11-09 00:46:53 $ $Author: areeves $
Version: $Revision: 1.1 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2005 - 2009 Animal Population Health Institute, Colorado State University

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
    Menus,
    ActnPopupCtrl,

    SMDatabase,
    SMScenario,
    ProductionTypePair,
    
    FormSMWizardBase,
    FormProdTypePairBase,
    FrameAirborneOrLocalAreaSpread
  ;

  type TFormLocalAreaSpread = class( TFormProdTypePairBase )
    fraParams: TFrameAirborneOrLocalAreaSpread;

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

      function showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer; override;
    end
  ;


implementation

{$R *.dfm}

  uses
    SysUtils,

    DebugWindow,
    MyStrUtils,
    I88n,
    QStringMaps,
    
    LocalAreaSpreadParams
  ;

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TFormLocalAreaSpread.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();
      fraParams.setCaptions( MTLocalArea );

      _fnList := nil;
    end
  ;


  procedure TFormLocalAreaSpread.translateUI();
    begin
      dbcout( 'Write TFormLocalAreaSpread.translateUI()', true );
      with self do
        begin
          Caption := tr( 'Scenario parameters: Local area spread' );
          pnlCaption.Caption := tr( 'Local area spread' );
        end
      ;
    end
  ;


  destructor TFormLocalAreaSpread.destroy();
    begin
      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Function-handling functions reimplemented from TFormProdTypePairBase
//-----------------------------------------------------------------------------
  procedure TFormLocalAreaSpread.prepFunctionDicts();
    begin
      // Do nothing
    end
  ;


  procedure TFormLocalAreaSpread.giveListsToEditors();
    begin
      // Do nothing
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Display functions reimplemented from TFormProdTypePairBase
//-----------------------------------------------------------------------------
  function TFormLocalAreaSpread.showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer;
    begin
      if( _smScenarioCopy.simInput.includeLocalAreaSpreadGlobal ) then
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


  procedure TFormLocalAreaSpread.updateDisplay();
    begin
      if( nil <> _selectedPTP ) then
        begin
          if( nil = _selectedPTP.localArea ) then
            _selectedPTP.localArea := TLocalAreaSpreadParams.create( _selectedPTP.sim, _selectedPTP.dest.productionTypeID, _selectedPTP.source.productionTypeID )
          ;

          fraParams.Visible := true;
          fraParams.spreadParams := _selectedPTP.localArea;
          // Setting the property above will automatically update the parameter display
        end
      else
        begin
          fraParams.Visible := false;
          fraParams.spreadParams := nil;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Data validation
//-----------------------------------------------------------------------------
  function TFormLocalAreaSpread.dataIsValid(): boolean;
    begin
      result := fraParams.isValid();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Copying parameters
//-----------------------------------------------------------------------------
  procedure TFormLocalAreaSpread.copyParameters( const src: TProductionTypePair; dest: TProductionTypePair );
    begin
      dest.localArea.useLocalArea := src.localArea.useLocalArea;

      dest.localArea.distBetwUnits := src.localArea.distBetwUnits;
      dest.localArea.probSpread := src.localArea.probSpread;

      dest.localArea.nInfectiousInSource := src.localArea.nInfectiousInSource;
      dest.localArea.nSusceptibleInReceipient := src.localArea.nSusceptibleInReceipient;

      dest.updated := true;
    end
  ;
//-----------------------------------------------------------------------------


initialization
  registerClass( TFormLocalAreaSpread );

end.

