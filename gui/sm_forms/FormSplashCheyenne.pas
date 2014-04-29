unit FormSplashCheyenne;

(*
FormSplashCheyenne.pas/dfm
--------------------------
Begin: 2006/04/03
Last revision: $Date: 2008/10/21 23:34:43 $ $Author: areeves $
Version: $Revision: 1.11 $
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

  type TFormSplashCheyenne = class( TForm )
  		pnlWholeForm: TPanel;
      pnlAppTitle: TPanel;
      lblVersion: TLabel;
      Timer1: TTimer;
      lblAdditionalInfo: TLabel;
			lblAppTitle: TLabel;
			lblExplanation: TLabel;

			procedure FormCreate( Sender: TObject );
      procedure FormClose(Sender: TObject; var Action: TCloseAction); virtual;
      procedure timerTimeout(Sender: TObject); virtual;

    protected
      procedure translateUI();

    public
      constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;
      
    end
  ;

  var
    frmSplash: TFormSplashCheyenne;


implementation

{$R *.dfm}

  uses
    ShellAPI,

    Resources,
    DialogLongMessage,
    ControlUtils,
    MyStrUtils,
    GuiStrUtils,
    DebugWindow,
    I88n,

    StringConsts
  ;

  constructor TFormSplashCheyenne.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();
      
      self.Hide();
      lblVersion.Caption := tr( 'Version' ) + ' ' + MAJORVERSIONNUMBER + '.' + MINORVERSIONNUMBER + ' ' + tr( 'Build' ) + ' ' + BUILDNUMBER + ' ' + BRANCHNAME;

      borderStyle := bsNone;

      pnlWholeForm.bevelWidth := 2;
      pnlWholeForm.BevelOuter := bvRaised;
      pnlWholeForm.BevelInner := bvLowered;

      centerChildren( pnlWholeForm );
      self.Show();
    end
  ;

  procedure TFormSplashCheyenne.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:54 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormSplashCheyenne.dfm
      // File date: Thu Oct 12 14:33:58 2006

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'NAADSM splash screen' );
          lblVersion.Caption := tr( 'Version 3.0.78 Build 060403' );
          lblAdditionalInfo.Caption := tr( 'Please see ''About NAADSM'' for contact and support information.' );
          lblAppTitle.Caption := tr( 'NAADSM-Cheyenne' );
          lblExplanation.Caption := tr( 'This is an experimental version of the NAADSM.  This version deviates from the published conceptual specification in ways that may significantly affect the simulation outcome. ' );
        end
      ;

    end
  ;


 	procedure TFormSplashCheyenne.FormCreate(Sender: TObject);
 		begin
       if Screen.PixelsPerInch <> 96 then
         ScaleBy( Screen.PixelsPerInch, 96 )
       ;
 		end
	;


  destructor TFormSplashCheyenne.destroy();
    begin
      frmSplash := nil;
      inherited destroy();
    end
  ;

  procedure TFormSplashCheyenne.FormClose(Sender: TObject; var Action: TCloseAction);
    begin
      Action := caFree;
    end
  ;


  procedure TFormSplashCheyenne.timerTimeout(Sender: TObject);
    begin
      close();
    end
  ;


end.
