unit FormAboutCheyenne;

(*
FormAboutCheyenne.pas/dfm
-------------------------
Begin: 2006/04/03
Last revision: $Date: 2008/10/21 23:34:43 $ $Author: areeves $
Version: $Revision: 1.9 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2006 - 2008 Animal Population Health Institute, Colorado State University

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

  type TFormAboutCheyenne = class( TForm )
  		pnlWholeForm: TPanel;
      pnlBody: TPanel;

      pnlAppTitleContainer: TPanel;
      pnlAppTitle: TPanel;
      lblVersion: TLabel;

      pnlLicense: TPanel;
      lblLicenseBlurb: TLabel;

      pnlButtons: TPanel;
      btnOK: TButton;
      btnLicense: TButton;
			lblCopyright: TLabel;
			lblWebsite: TLabel;
			lblAppTitle: TLabel;
			lblExplanation: TLabel;

      procedure mailtoClick( sender: TObject );
      procedure btnOKClick(Sender: TObject);
      procedure btnLicenseClick(Sender: TObject);

      procedure FormClose(Sender: TObject; var Action: TCloseAction);
    	procedure lblWebsiteClick(Sender: TObject);

    protected
      procedure translateUI();

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

  constructor TFormAboutCheyenne.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();
      
      lblVersion.Caption := tr( 'Version' ) + ' ' + MAJORVERSIONNUMBER + '.' + MINORVERSIONNUMBER + ' ' + tr( 'Build' ) + ' ' + BUILDNUMBER + ' ' + BRANCHNAME;
      lblWebsite.Caption := WEBSITE;

      pnlWholeForm.bevelWidth := 1;
      pnlWholeForm.BevelOuter := bvNone;
      pnlWholeForm.BevelInner := bvNone;
    end
  ;


  procedure TFormAboutCheyenne.translateUI();
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
          Caption := tr( 'About NAADSM-Cheyenne' );
          btnOK.Caption := tr( '&OK' );
          btnLicense.Caption := tr( '&License...' );
          lblExplanation.Caption := tr( 'This is an experimental version of the NAADSM.  This version deviates from the published conceptual specification in ways that may significantly affect the simulation outcome. ' );
          lblVersion.Caption := ansiReplaceStr( ansiReplaceStr( tr( 'Version xyz Build abc' ), 'xyz', MAJORVERSIONNUMBER + '.' + MINORVERSIONNUMBER ), 'abc', BUILDNUMBER);
          lblWebsite.Caption := tr( 'lblWebsite' );
          lblAppTitle.Caption := tr( 'NAADSM-Cheyenne' );
          lblLicenseBlurb.Caption := tr( longString );
          lblCopyright.Caption := tr( 'Copyright © 2003 - 2008 Animal Population Health Institute  at Colorado State University and University of Guelph' );
        end
      ;

    end
  ;


  procedure TFormAboutCheyenne.mailtoClick(Sender: TObject);
    begin
      if( Sender is TLabel ) then
        ShellExecute(
          Application.Handle,
          PChar( 'open' ),
          PChar( 'mailto: ' + (Sender as TLabel).Hint ),
          PChar( 0 ),
          nil,
          SW_NORMAL
        )
      ;
    end
  ;


  procedure TFormAboutCheyenne.lblWebsiteClick(Sender: TObject);
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


  procedure TFormAboutCheyenne.btnOKClick(Sender: TObject);
    begin
      self.Close();
    end
  ;


  procedure TFormAboutCheyenne.FormClose(Sender: TObject; var Action: TCloseAction);
    begin
      action := caHide;
    end
  ;


  procedure TFormAboutCheyenne.btnLicenseClick(Sender: TObject);
    var
      frm: TDialogLongMessage;
    begin
      frm := TDialogLongMessage.create( self, tr( 'GNU General Public License' ) );
      frm.mmoLongMessage.Font.Name := 'Courier New';
      frm.setMessage( i88nLicense() );

      self.Hide();

      frm.ShowModal();
      frm.Free();

      self.Show();
    end
  ;



end.
