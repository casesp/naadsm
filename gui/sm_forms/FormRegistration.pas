unit FormRegistration;

(*
FormRegistration.pas/dfm
------------------------
Begin: 2006/06/05
Last revision: $Date: 2013-06-27 19:11:28 $ $Author: areeves $
Version number: $Revision: 1.1.4.1 $
Project: APHI General Purpose Delphi Libary
Website: http://www.naadsm.org/opensource/delphi/
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2006 - 2008 Colorado State University

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
    ExtCtrls,
    StdCtrls
	;


	type TFormRegistration = class( TForm )
			pnlButtons: TPanel;
			btnOK: TButton;
			pnlMain: TPanel;
			pnlHeader: TPanel;
			lblRegister: TLabel;
			imgIcon: TImage;
			pnlBody: TPanel;
			rdgRegister: TRadioGroup;

			procedure FormCreate(Sender: TObject);
			procedure btnOKClick(Sender: TObject);
    
    protected
      procedure translateUI();
      
    public
      constructor create( Aowner: TComponent; formToHide: TForm = nil ); reintroduce;

    end
  ;


implementation

{$R *.dfm}
  uses
    ShellAPI,
    Registry,

    WindowsUtils,
    ControlUtils,
    ImageResources,
    I88n,

    StringConsts
  ;

  constructor TFormRegistration.create( Aowner: TComponent; formToHide: TForm = nil );
    begin
      inherited create( AOwner );
      translateUI();
      
      // Deal with form scaling
      //-----------------------
      Assert(not Scaled, 'You should set Scaled property of Form to False!');
      // Set this value to the PPI AT WHICH THE FORM WAS ORIGINALLY DESIGNED!!
      self.PixelsPerInch := 96;
      // FormCreate() will handle the rest.

      if( nil <> formtoHide ) then formToHide.Hide();
      centerScreen( self );
      horizCenterInside( rdgRegister, pnlBody );
      imgIcon.Picture.Bitmap := bitmapStyle( BITInformation );

      self.ShowModal();
    end
  ;


  procedure TFormRegistration.translateUI();
    var
      longString: string;
    begin
      longString :=
        'It appears that you may not have not registered your copy of NAADSM. '
        + ' Registration is a good way to stay in touch with the NAADSM Development Team. '
        + ' The process is simple, and registered users will be updated periodically regarding'
        + ' new releases and features.'
      ;

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Please register NAADSM' );
          btnOK.Caption := tr( '&OK' );
          lblRegister.Caption := tr( longString );
          rdgRegister.Caption := tr( 'Please select a registration option:' );
        end
      ;

      // Set TStrings properties
      with self do
        begin
          rdgRegister.Items[0] := tr( 'Go to NAADSM website to register now' );
          rdgRegister.Items[1] := tr( 'Remind me again later' );
          rdgRegister.Items[2] := tr( 'I have already registered' );
          rdgRegister.Items[3] := tr( 'Skip registration' );
        end
      ;

    end
  ;


	procedure TFormRegistration.FormCreate(Sender: TObject);
		begin
      if Screen.PixelsPerInch <> 96 then
        ScaleBy( Screen.PixelsPerInch, 96 )
      ;
		end
	;


  procedure TFormRegistration.btnOKClick(Sender: TObject);
    begin
      case rdgRegister.ItemIndex of
        0:
          begin
            setRegistryData( HKEY_CURRENT_USER, 'Software\NAADSM','UserIsRegistered', rdInteger, 1 );
            ShellExecute(
              Application.Handle,
              PChar( 'open' ),
              PChar( WEBSITE + '/register.php' ),
              PChar( 0 ),
              nil,
              SW_NORMAL
            );
          end
        ;
        1: setRegistryData( HKEY_CURRENT_USER, 'Software\NAADSM','UserIsRegistered', rdInteger, 0 );
        2: setRegistryData( HKEY_CURRENT_USER, 'Software\NAADSM','UserIsRegistered', rdInteger, 1 );
        3: setRegistryData( HKEY_CURRENT_USER, 'Software\NAADSM','UserIsRegistered', rdInteger, 2 );
      end;

      self.Close();
    end
  ;

end.
