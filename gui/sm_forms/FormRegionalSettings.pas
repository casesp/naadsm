unit FormRegionalSettings;

(*
FormRegionalSettings.pas/dfm
----------------------------
Begin: 2009/4/14
Last revision: $Date: 2013-06-27 19:11:28 $ $Author: areeves $
Version number: $Revision: 1.2.8.1 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <aaron.reeves@naadsm.org>
--------------------------------------------------
Copyright (C) 2009 Colorado State University

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
    ComCtrls,

    PBPageControl,

    SMI88nSettings
  ;

  type TFormRegionalSettings = class( TForm )
      pnlOK: TPanel;
      btnOK: TButton;
      btnCancel: TButton;
      pgcRegionalSettings: TPBPageControl;
      tabCsvOptions: TTabSheet;
      rdoUseSystem: TRadioButton;
      rdoUseCustom: TRadioButton;
      lblDecimalSymbolLabel: TLabel;
      lblListSeparatorLabel: TLabel;
      lblDecimalSymbol: TLabel;
      lblListSeparator: TLabel;
      leDecimalSymbol: TEdit;
      leListSeparator: TEdit;

      procedure rdoClick( sender: TObject );
      procedure btnClick( Sender: TObject );

    protected
      _i88nSettings: TSMI88nSettings;

      procedure translateUI();
      procedure translateUIManual();
      function dataIsValid(): boolean;

    public
      constructor create( AOwner: TComponent; settings: TSMI88nSettings ); reintroduce;

    end
  ;


implementation

{$R *.dfm}

  uses
    DebugWindow,
    MyStrUtils,
    WindowsUtils,
    ControlUtils,
    MyDialogs,
    I88n
  ;





//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TFormRegionalSettings.create( AOwner: TComponent; settings: TSMI88nSettings );
    begin
      inherited create( AOwner );
      translateUI();

      _i88nSettings := settings;

      if( nil = AOwner ) then
        position := poScreenCenter
      else
        position := poOwnerFormCenter
      ;

      leDecimalSymbol.Left := lblDecimalSymbol.Left;
      leListSeparator.Left := lblListSeparator.Left;

      leDecimalSymbol.Text := _i88nSettings.decimalSymbol;
      leListSeparator.Text := _i88nSettings.listSeparator;

      lblDecimalSymbol.Caption := SysUtils.decimalSeparator;
      lblListSeparator.Caption := SysUtils.listSeparator;

      if( _i88nSettings.useCustomListSeparator ) then
        rdoUseCustom.Checked := true
      else
        rdoUseSystem.Checked := true
      ;
    end
  ;


  procedure TFormRegionalSettings.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.3.
      // Generation date: Mon Jun 1 21:36:59 2009
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Gunnison/sm_forms/FormRegionalSettings.dfm
      // File date: Tue Apr 14 11:58:56 2009

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'NAADSM regional settings' );
          btnOK.Caption := tr( '&OK' );
          btnCancel.Caption := tr( '&Cancel' );
          tabCsvOptions.Caption := tr( 'CSV import/export' );
          lblDecimalSymbolLabel.Caption := tr( 'Decimal symbol:' );
          lblListSeparatorLabel.Caption := tr( 'List separator:' );
          lblDecimalSymbol.Caption := tr( 'lblDecimalSymbol' );
          lblListSeparator.Caption := tr( 'Label3' );
          rdoUseSystem.Caption := tr( 'Use system characters for CSV import/export' );
          rdoUseCustom.Caption := tr( 'Use specified characters for CSV import/export in NAADSM' );
        end
      ;

      // If any phrases are found that could not be automatically extracted by
      // Caption Collector, modify the following function to take care of them.
      translateUIManual();
    end
  ;


  procedure TFormRegionalSettings.translateUIManual();
    begin
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Data validation
//-----------------------------------------------------------------------------
  function TFormRegionalSettings.dataIsValid(): boolean;
    var
      msg: string;
    begin
      result := true; // until proven otherwise.

      if( rdoUseSystem.Checked ) then
        begin
          // Make sure that the SYSTEM characters are different
          if( SysUtils.ListSeparator = SysUtils.DecimalSeparator ) then
            begin
              msg := tr( 'The decimal symbol is the same character as the list separator.' )
                + endl + endl
                + tr( 'You can change these system settings using the Windows Control Panel (see the Control Panel option labeled "Regional and Language Options").' )
                + '  '
                + tr( 'Alternatively, you can specify characters to use within NAADSM.' )
              ;
              msgOK( msg, tr( 'Characters cannot be the same' ), IMGWarning, self );
              result := false;
            end
          ;
        end
      else
        begin
          // Did the user specify both characters?
          if( ( 0 = length( leDecimalSymbol.Text ) ) or ( 0 = length( leListSeparator.text ) ) ) then
            begin
              msg := tr( 'Please specify both characters.' );
              msgOK( msg, tr( 'Characters must be specified' ), IMGWarning, self );
              result := false;
            end

          // Are they different?
          else if( leDecimalSymbol.Text = leListSeparator.text ) then
            begin
              msg := tr( 'Please specify different characters.' );
              msgOK( msg, tr( 'Characters cannot be the same' ), IMGWarning, self );
              result := false;
            end
          ;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI event handlers
//-----------------------------------------------------------------------------
  procedure TFormRegionalSettings.rdoClick( sender: TObject );
    begin
      if( rdoUseCustom.checked ) then
        begin
          leDecimalSymbol.Visible := true;
          leListSeparator.Visible := true;

          lblDecimalSymbol.Visible := false;
          lblListSeparator.Visible := false;
        end
      else
        begin
          leDecimalSymbol.Visible := false;
          leListSeparator.Visible := false;

          lblDecimalSymbol.Visible := true;
          lblListSeparator.Visible := true;
        end
      ;
    end
  ;


  procedure TFormRegionalSettings.btnClick(Sender: TObject);
    begin
      if( btnCancel = sender ) then
        close()
      else if( dataIsValid() ) then
        begin
          if( rdoUseCustom.checked ) then
            begin
              _i88nSettings.useCustomListSeparator := true;
              _i88nSettings.useCustomDecimalSymbol := true;

              _i88nSettings.listSeparator := leListSeparator.text[1];
              _i88nSettings.decimalSymbol := leDecimalSymbol.text[1];
            end
          else
            begin
              _i88nSettings.useCustomListSeparator := false;
              _i88nSettings.useCustomDecimalSymbol := false;
            end
          ;

          _i88nSettings.updateRegistry();

          close();
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------


end.
