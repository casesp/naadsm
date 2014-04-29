unit FormAbout;

(*
FormAbout.pas/dfm
-----------------
Begin: 2006/04/03
Last revision: $Date: 2013-06-27 19:11:24 $ $Author: areeves $
Version: $Revision: 1.20.4.5 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2006 - 2013 NAADSM Development Team

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
    ExtCtrls
  ;

  type TFormAbout = class( TForm )
  		pnlWholeForm: TPanel;
      pnlBody: TPanel;

      pnlAppTitleContainer: TPanel;
      pnlAppTitle: TPanel;
      lblVersion: TLabel;
      imgAppTitle: TImage;

      pnlLicense: TPanel;
      lblLicenseBlurb: TLabel;

      pnlButtons: TPanel;
      btnOK: TButton;
      btnLicense: TButton;
    	lblCopyright: TLabel;
    	lblWebsite: TLabel;
      lblSupport: TLabel;

      procedure btnOKClick(Sender: TObject);
      procedure btnLicenseClick(Sender: TObject);

    	procedure lblWebsiteClick(Sender: TObject);

      procedure FormCreate(Sender: TObject);
      procedure FormClose(Sender: TObject; var Action: TCloseAction);

    protected
      procedure translateUI();
      procedure translateUIManual();

    public
      constructor create( AOwner: TComponent ); override;
    end
  ;


implementation

{$R *.dfm}

  uses
    StrUtils,
    ShellAPI,

    DialogLongMessage,
    ControlUtils,
    I88n,

    StringConsts
  ;

  constructor TFormAbout.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();

      pnlWholeForm.bevelWidth := 1;
      pnlWholeForm.BevelOuter := bvNone;
      pnlWholeForm.BevelInner := bvNone;

      centerChildren( pnlAppTitle );
    end
  ;


  procedure TFormAbout.translateUI();
    var
      longString: string;
    begin
      longString :=
        'This program is free software; you can redistribute it and/or modify'
        + ' it under the terms of the GNU General Public License as published by the'
        + ' Free Software Foundation; either version 2 of the License, or (at your option)'
        + ' any later version.  For complete license details, please see below.'
      ;

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'About NAADSM' );
          btnOK.Caption := tr( '&OK' );
          btnLicense.Caption := tr( '&License...' );
          lblVersion.Caption := ansiReplaceStr( ansiReplaceStr( tr( 'Version xyz Build abc' ), 'xyz', MAJORVERSIONNUMBER + '.' + MINORVERSIONNUMBER ), 'abc', BUILDNUMBER);
          lblWebsite.Caption := tr( 'lblWebsite' );
          lblLicenseBlurb.Caption := tr( longString );
          lblCopyright.Caption := tr( 'Copyright � 2003 - 2013 NAADSM Development Team' );
          lblSupport.Caption := tr( 'Please see the NAADSM website for contact and support information:' );
        end
      ;

      // If any phrases are found that could not be automatically extracted by
      // Caption Collector, modify the following function to take care of them.
      translateUIManual();
    end
  ;


  procedure TFormAbout.translateUIManual();
    begin
      lblCopyright.Caption := tr( 'Copyright �' ) + ' ' + COPYRIGHTDATES + ' ' + tr( 'NAADSM Development Team' );
      lblVersion.Caption := tr( 'Version' ) + ' ' + MAJORVERSIONNUMBER + '.' + MINORVERSIONNUMBER + ' ' + tr( 'Build' ) + ' ' + BUILDNUMBER + ' ' + BRANCHNAME;
      lblWebsite.Caption := WEBSITE;
    end
  ;


  procedure TFormAbout.FormCreate(Sender: TObject);
    begin
      if Screen.PixelsPerInch <> 96 then
        begin
          ScaleBy( Screen.PixelsPerInch, 96 );
        end
      ;
    end
  ;


  procedure TFormAbout.lblWebsiteClick(Sender: TObject);
    begin
      ShellExecute(
        Application.Handle,
        PChar( 'open' ),
        PChar( WEBSITE ),
        PChar( 0 ),
        nil,
        SW_NORMAL
      );
    end
  ;


  procedure TFormAbout.btnOKClick(Sender: TObject);
    begin
      self.Close();
    end
  ;


  procedure TFormAbout.FormClose(Sender: TObject; var Action: TCloseAction);
    begin
      action := caHide;
    end
  ;


  procedure TFormAbout.btnLicenseClick(Sender: TObject);
    var
      frm: TDialogLongMessage;
    begin
      frm := TDialogLongMessage.create( self, tr( 'GNU General Public License' ) );
      frm.mmoLongMessage.Font.Name := 'Courier New';
      frm.setMessage( i88nLicense() );

      self.Hide();

      frm.ShowModal();
      frm.Release();

      self.Show();
    end
  ;





end.
