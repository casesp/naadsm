unit FormSpreadOptions;

(*
FormSpreadOptions.pas/dfm
--------------------------
Begin: 2005/05/03
Last revision: $Date: 2008/03/12 22:10:48 $ $Author: areeves $
Version: $Revision: 1.21 $
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
  ExtCtrls,
  Buttons,
  REEdit,
  
  FormSMWizardBase, Menus, ActnPopupCtrl
;

  type TFormSpreadOptions = class( TFormSMWizardBase )
    pnlCaption: TPanel;
    pnlBody: TPanel;
    grpSpreadOptions: TGroupBox;
    rdoAirborne: TRadioButton;
    rdoContact: TRadioButton;
    rdoBoth: TRadioButton;
    rdoNoSpread: TRadioButton;
    gbxAirborneDecay: TGroupBox;
    rdoAirborneLinear: TRadioButton;
    rdoAirborneExponential: TRadioButton;

    procedure rdoClick(Sender: TObject);

    protected
      _dataChanged: boolean;
      _loading: boolean;

      procedure translateUI();

//      procedure initializeFromDatabase(); override;
//      procedure updateDatabase(); override;
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
    GuiStrUtils,
    I88n
  ;

	constructor TFormSpreadOptions.create( AOwner: TComponent );
  	begin
      inherited create( AOwner );
      translateUI();

      _dataChanged := false;

      horizCenter( grpSpreadOptions, pnlBody );
      horizCenter( gbxAirborneDecay, pnlBody );
    end
  ;


  procedure TFormSpreadOptions.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:54 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormSpreadOptions.dfm
      // File date: Mon Mar 19 11:05:32 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Scenario parameters: Spread options' );
          pnlCaption.Caption := tr( 'Spread options' );
          grpSpreadOptions.Caption := tr( 'What type of SPREAD would you like to model during simulation runs? ' );
          rdoAirborne.Caption := tr( 'Airborne' );
          rdoContact.Caption := tr( 'Contact' );
          rdoBoth.Caption := tr( 'Both airborne and contact' );
          rdoNoSpread.Caption := tr( 'No spread' );
          gbxAirborneDecay.Caption := tr( 'For airborne spread:' );
          rdoAirborneLinear.Caption := tr( 'Rate of disease transfer declines linearly from source' );
          rdoAirborneExponential.Caption := tr( 'Rate of disease transfer declines exponentially from source' );
        end
      ;

    end
  ;


  destructor TFormSpreadOptions.destroy();
  	begin
    	// Nothing else do to in here yet.
    	inherited destroy();
    end
  ;
  
 
  procedure TFormSpreadOptions.initializeFromSim();
    var
      includeContactSpread, includeAirborneSpread: boolean;
      useAirborneExponentialDecay: boolean;
    begin
      _loading := true;

      includeContactSpread := _smScenarioCopy.simInput.includeContactSpreadGlobal;
      includeAirborneSpread := _smScenarioCopy.simInput.includeAirborneSpreadGlobal;
      useAirborneExponentialDecay := _smScenarioCopy.simInput.useAirborneExponentialDecay;

      rdoAirborneExponential.Checked := useAirborneExponentialDecay;
      rdoAirborneLinear.Checked := not useAirborneExponentialDecay;

      if( includeAirborneSpread and includeContactSpread ) then
        begin
      	  rdoBoth.checked := true;
          gbxAirborneDecay.Visible := true;
        end
      else if( includeAirborneSpread ) then
        begin
      	  rdoAirborne.checked := true;
          gbxAirborneDecay.Visible := true;
        end
      else if( includeContactSpread ) then
        begin
          gbxAirborneDecay.Visible := false;
      	  rdoContact.checked := true;
        end
      else if( not( includeAirborneSpread ) and not( includeContactSpread ) ) then
        begin
          gbxAirborneDecay.Visible := false;
      	  rdoNoSpread.checked := true;
        end
      ;

      hideStar();

      _dataChanged := false;
      _loading := false;
    end;

    
  (*
  procedure TFormSpreadOptions.initializeFromDatabase();
  	var
    	q: string;
      db2: TSqlDatabase;
      res: TSqlResult;
      row: TSqlRow;
      includeContactSpread, includeAirborneSpread: boolean;
      useAirborneExponentialDecay: boolean;
  	begin
      _loading := true;

      db2 := _smdb as TSqlDatabase;

    	q := 'SELECT includeContactSpread, includeAirborneSpread, useAirborneExponentialDecay FROM inGeneral';

      res := TSqlResult.create( q, db2 );

      row := res.fetchArrayFirst();

      includeContactSpread := row.field( 'includeContactSpread' );
      includeAirborneSpread := row.field( 'includeAirborneSpread' );
      useAirborneExponentialDecay := row.field( 'useAirborneExponentialDecay' );

      if( useAirborneExponentialDecay ) then
        rdoAirborneExponential.Checked := true
      else
        rdoAirborneLinear.Checked := true
      ;

      if( includeAirborneSpread and includeContactSpread ) then
        begin
      	  rdoBoth.checked := true;
          gbxAirborneDecay.Visible := true;
        end
      else if( includeAirborneSpread ) then
        begin
      	  rdoAirborne.checked := true;
          gbxAirborneDecay.Visible := true;
        end
      else if( includeContactSpread ) then
        begin
          gbxAirborneDecay.Visible := false;
      	  rdoContact.checked := true;
        end
      else if( not( includeAirborneSpread ) and not( includeContactSpread ) ) then
        begin
          gbxAirborneDecay.Visible := false;
      	  rdoNoSpread.checked := true;
        end
      ;

      hideStar();

      _dataChanged := false;
      _loading := false;

      res.free();
    end
  ;


  procedure TFormSpreadOptions.updateDatabase();
  	var
    	q: string;
  	begin

      if( _dataChanged ) then
        begin
          q := 'UPDATE inGeneral SET'
            + ' includeContactSpread = false'
            + ', includeAirborneSpread = false'
            + ', useAirborneExponentialDecay = ' + boolToText( rdoAirborneExponential.Checked )
          ;
          _smdb.execute( q );

          if( rdoAirborne.Checked or rdoBoth.Checked ) then
            begin
              q := 'UPDATE inGeneral SET includeAirborneSpread = true';
              _smdb.execute( q );
            end
          ;

          if( rdoContact.Checked or rdoBoth.Checked ) then
            begin
              q := 'UPDATE inGeneral SET includeContactSpread = true';
              _smdb.execute( q );
            end
          ;
        end
      ;
    end
  ;
*)

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


  procedure TFormSpreadOptions.rdoClick(Sender: TObject);
    begin
      inherited;

      if( not( _loading ) ) then
        begin
          gbxAirborneDecay.Visible := ( rdoAirborne.Checked or rdoBoth.Checked );

          if ( rdoBoth.Checked ) then
            begin
              _smScenarioCopy.simInput.includeContactSpreadGlobal := true;
              _smScenarioCopy.simInput.includeAirborneSpreadGlobal := true;
            end
          else
            begin
              _smScenarioCopy.simInput.includeContactSpreadGlobal := rdoContact.Checked;
              _smScenarioCopy.simInput.includeAirborneSpreadGlobal := rdoAirborne.Checked;
            end;

          _smScenarioCopy.simInput.useAirborneExponentialDecay := rdoAirborneExponential.Checked;

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
