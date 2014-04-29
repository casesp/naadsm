unit FormLanguageSettings;

(*
FormLanguageSettings.pas/dfm
----------------------------
Begin: 2007/03/08
Last revision: $Date: 2013-06-27 19:11:26 $ $Author: areeves $
Version number: $Revision: 1.2.16.1 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2008 Colorado State University

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

    I88n
  ;

  type TFormLanguageSettings = class( TForm )
      pnlLanguageRdo: TPanel;
      rdoUseSelected: TRadioButton;
      rdoUseSystemDefault: TRadioButton;
      pnlLanguageSelect: TPanel;
      lblLanguageSelect: TLabel;
      cboLanguage: TComboBox;
      pnlLanguageCbx: TPanel;
      cbxUseSelected: TCheckBox;
      pnlOK: TPanel;
      btnOK: TButton;
      btnCancel: TButton;
      rdoAlwaysPrompt: TRadioButton;

      procedure btnClick(Sender: TObject);
      procedure rdoClick(Sender: TObject);

    protected
      _showDialog: boolean;
      _language: TI88nLanguageCode;

      procedure translateUI();

    public
      constructor create(
        AOwner: TComponent;
        const showRadioButtons: boolean;
        const showDialog: boolean
      ); reintroduce;

      class function usePreferredLanguage(): boolean;
      class function useSystemLanguage(): boolean;
      class function preferredLanguage(): TI88nLanguageCode;

      property language: TI88nLanguageCode read _language;
    end
  ;


implementation

{$R *.dfm}

  uses
    Registry,

    DebugWindow,
    WindowsUtils,
    ControlUtils,
    MyDialogs
  ;

//-----------------------------------------------------------------------------
// Public class functions
//-----------------------------------------------------------------------------
  class function TFormLanguageSettings.usePreferredLanguage(): boolean;
    var
      regValue: integer;
    begin
      try
        regValue := getRegistryData( HKEY_CURRENT_USER, 'Software\NAADSM','UsePreferredLanguage' );
      except
        regValue := 0; // Default to false
      end;

      result := ( 0 <> regValue );
    end
  ;


  class function TFormLanguageSettings.useSystemLanguage(): boolean;
    var
      regValue: integer;
    begin
      try
        regValue := getRegistryData( HKEY_CURRENT_USER, 'Software\NAADSM','UseSystemLanguage' );
      except
        regValue := 1; // Default to true
      end;

      result := ( 0 <> regValue );
    end
  ;


  class function TFormLanguageSettings.preferredLanguage(): TI88nLanguageCode;
    var
      regValue: string;
    begin
      try
        regValue := getRegistryData( HKEY_CURRENT_USER, 'Software\NAADSM','PreferredLanguage' );
      except
        regValue := 'LANG_UNSPECIFIED';
      end;

      if( regValue = 'LANG_ENGLISH' ) then
        result := I88nEnglish
      else if( regValue = 'LANG_SPANISH' ) then
        result := I88nSpanish
      else
        result := I88nUnknown
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TFormLanguageSettings.create(
        AOwner: TComponent;
        const showRadioButtons: boolean;
        const showDialog: boolean
      );
    begin
      inherited create( AOwner );
      translateUI();

      if( nil = AOwner ) then
        position := poScreenCenter
      else
        position := poOwnerFormCenter
      ;

      _showDialog := showDialog;

      pnlLanguageSelect.BevelOuter := bvNone;
      pnlLanguageRdo.BevelOuter := bvNone;
      pnlLanguageCbx.BevelOuter := bvNone;
      pnlOK.BevelOuter := bvNone;

      _language := I88nEnglish;

      if( showRadioButtons ) then
        begin
          pnlLanguageCbx.Visible := false;
          self.Height := self.Height - pnlLanguageCbx.Height;
        end
      else
        begin
          pnlLanguageRdo.Visible := false;
          self.Height := self.Height - pnlLanguageRdo.Height;
          btnCancel.Visible := false;
          horizCenterInside( btnOK, pnlOK );
        end
      ;

      if( usePreferredLanguage() ) then
        begin
          if( showRadioButtons ) then
            rdoUseSelected.Checked := true
          else
            cbxUseSelected.Checked := true
          ;
          _language := preferredLanguage();
        end
      ;

      if( languageSupported( systemLanguage() ) ) then
        begin
          if( useSystemLanguage() ) then
            begin
              cboLanguage.Enabled := false;
              rdoUseSystemDefault.Checked := true;
              _language := systemLanguage();
            end
          ;
        end
      else
        rdoUseSystemDefault.Enabled := false
      ;

      case _language of
        I88nEnglish: cboLanguage.ItemIndex := 0;
        I88nSpanish: cboLanguage.ItemIndex := 1;
        else cboLanguage.ItemIndex := 0;
      end;
    end
  ;


  procedure TFormLanguageSettings.translateUI();
    begin
      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'NAADSM Language settings' );
          rdoUseSelected.Caption := tr( 'Always use selected language' );
          rdoUseSystemDefault.Caption := tr( 'Always use this computer''s default language' );
          rdoAlwaysPrompt.Caption := tr( 'Always prompt user for a language selection' );
          lblLanguageSelect.Caption := tr( 'Please select a language:' );
          cboLanguage.Text := tr( 'English' );
          cbxUseSelected.Caption := tr( 'Always use selected language' );
          btnOK.Caption := tr( '&OK' );
          btnCancel.Caption := tr( '&Cancel' );
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI event handlers
//-----------------------------------------------------------------------------
  procedure TFormLanguageSettings.rdoClick(Sender: TObject);
    begin
      if( rdoUseSystemDefault.Checked and languageSupported( systemLanguage() ) ) then
        begin
          _language := systemLanguage();
          case _language of
            I88nEnglish: cboLanguage.ItemIndex := 0;
            I88nSpanish: cboLanguage.ItemIndex := 1;
            else cboLanguage.ItemIndex := 0;
          end;
          cboLanguage.Enabled := false;
        end
      else
        cboLanguage.Enabled := true
      ;
    end
  ;


  procedure TFormLanguageSettings.btnClick(Sender: TObject);
    var
      lang: string;
    begin
      if( btnOK = sender ) then
        begin
          if( 1 = cboLanguage.ItemIndex ) then // Spanish is selected
            begin
              lang := 'LANG_SPANISH';
              _language := I88nSpanish;
            end
          else // English is selected (also the default option)
            begin
              lang := 'LANG_ENGLISH';
              _language := I88nEnglish;
            end
          ;
          setRegistryData( HKEY_CURRENT_USER, 'Software\NAADSM','PreferredLanguage', rdString, lang );

          setRegistryData( HKEY_CURRENT_USER, 'Software\NAADSM','UsePreferredLanguage', rdInteger, 0 );
          setRegistryData( HKEY_CURRENT_USER, 'Software\NAADSM','UseSystemLanguage', rdInteger, 0 );

          if( cbxUseSelected.Checked or rdoUseSelected.Checked ) then
            setRegistryData( HKEY_CURRENT_USER, 'Software\NAADSM','UsePreferredLanguage', rdInteger, 1 )
          else if( rdoUseSystemDefault.Checked ) then
            setRegistryData( HKEY_CURRENT_USER, 'Software\NAADSM','UseSystemLanguage', rdInteger, 1 )
          ;

          if( _showDialog ) then
            msgOK(
              tr( 'New language settings will take effect the next time NAADSM is started.' ),
              tr( 'Language settings' ),
              IMGInformation,
              self
            )
          ;
        end
      ;

      close();
    end
  ;
//-----------------------------------------------------------------------------


end.
