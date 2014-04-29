unit FormSpreadOptions;

(*
FormSpreadOptions.pas/dfm
--------------------------
Begin: 2005/05/03
Last revision: $Date: 2012-10-23 22:29:20 $ $Author: areeves $
Version: $Revision: 1.24.6.2 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2005 - 2012 Colorado State University

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
  ExtCtrls,
  Buttons,
  REEdit,
  Menus,
  ActnPopupCtrl,

  FormSMWizardBase,
  FrameWindDirection
;

  type TFormSpreadOptions = class( TFormSMWizardBase )
      pnlCaption: TPanel;
      pnlBody: TPanel;
      grpSpreadOptions: TGroupBox;
      cbxContact: TCheckBox;
      cbxAirborne: TCheckBox;
      cbxLocalArea: TCheckBox;
      gbxWindParams: TGroupBox;
      fraWindDirection: TFrameWindDirection;

      procedure cbxClick(Sender: TObject);
      procedure processWindDirText( sender: TObject );

    protected
      _dataChanged: boolean;
      _loading: boolean;

      procedure translateUI();

      procedure initializeFromSim(); override;
      function dataIsValid(): boolean; override;
      function getDataUpdated(): boolean; override;

    public
      constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;
    end
  ;


implementation

{$R *.dfm}

	uses
  	ControlUtils,
    SqlClasses,
    RegExpDefs,
    MyDialogs,
    MyStrUtils,
    I88n,
    DebugWindow
  ;

	constructor TFormSpreadOptions.create( AOwner: TComponent );
  	begin
      inherited create( AOwner );
      translateUI();

      _dataChanged := false;

      horizCenter( grpSpreadOptions, pnlBody );
      horizCenter( gbxWindParams, pnlBody );
    end
  ;


  procedure TFormSpreadOptions.translateUI();
    begin
      dbcout( 'Rewrite TFormSpreadOptions.translateUI()', true );
    end
  ;


  destructor TFormSpreadOptions.destroy();
  	begin
    	// Nothing else do to in here yet.
    	inherited destroy();
    end
  ;


  procedure TFormSpreadOptions.initializeFromSim();
    begin
      _loading := true;

      cbxContact.Checked := _smScenarioCopy.simInput.includeContactSpreadGlobal;
      cbxAirborne.Checked := _smScenarioCopy.simInput.includeAirborneSpreadGlobal;
      cbxLocalArea.Checked := _smScenarioCopy.simInput.includeLocalAreaSpreadGlobal;

      gbxWindParams.Visible := _smScenarioCopy.simInput.includeAirborneSpreadGlobal;
      
      fraWindDirection.setAngles(
        _smScenarioCopy.simInput.windDirectionStart,
        _smScenarioCopy.simInput.windDirectionEnd
      );
      
      hideStar();

      _dataChanged := false;
      _loading := false;
    end
  ;


  procedure TFormSpreadOptions.processWindDirText( sender: TObject );
    begin
      fraWindDirection.processText( sender );

      _smScenarioCopy.simInput.windDirectionStart := fraWindDirection.startAngle;
      _smScenarioCopy.simInput.windDirectionEnd := fraWindDirection.endAngle;
      _smScenarioCopy.simInput.updated := true;
     end
  ;


  function TFormSpreadOptions.getDataUpdated(): boolean;
    begin
      result := _dataChanged;
    end
  ;


  function TFormSpreadOptions.dataIsValid(): boolean;
  	begin
      result := true;
    end
  ;


  procedure TFormSpreadOptions.cbxClick(Sender: TObject);
    begin
      inherited;

      if( not( _loading ) ) then
        begin
          gbxWindParams.Visible := cbxAirborne.Checked;

          _smScenarioCopy.simInput.includeContactSpreadGlobal := cbxContact.Checked;
          _smScenarioCopy.simInput.includeAirborneSpreadGlobal := cbxAirborne.Checked;
          _smScenarioCopy.simInput.includeLocalAreaSpreadGlobal := cbxLocalArea.Checked;

          if( cbxAirborne.Checked ) then
            begin
              _smScenarioCopy.simInput.windDirectionStart := fraWindDirection.startAngle;
              _smScenarioCopy.simInput.windDirectionEnd := fraWindDirection.endAngle;
            end
          ;

          _smScenarioCopy.simInput.updated := true;
          
          _dataChanged := true;
          showStar();
        end
      ;
    end
  ;


initialization
  RegisterClass( TFormSpreadOptions );

end.
