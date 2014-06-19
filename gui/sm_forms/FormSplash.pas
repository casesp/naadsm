unit FormSplash;

(*
FormSplash.pas/dfm
------------------
Begin: 2006/04/03
Last revision: $Date: 2013-06-27 19:11:28 $ $Author: areeves $
Version: $Revision: 1.19.6.5 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <aaron.reeves@naadsm.org>
--------------------------------------------------
Copyright (C) 2006 - 2012 Colorado State University

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

  type TFormSplash = class( TForm )
      Timer1: TTimer;
      pnlWholeForm: TPanel;
      lblVersion: TLabel;
      imgAppTitle: TImage;
      lblAdditionalInfo: TLabel;
      lblCopyright: TLabel;
      procedure FormClose(Sender: TObject; var Action: TCloseAction); virtual;
      procedure timerTimeout(Sender: TObject); virtual;

    protected
      procedure translateUI();
      procedure translateUIManual();

    public
      constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;
      
    end
  ;

  var
    frmSplash: TFormSplash;


implementation

{$R *.dfm}

  uses
    ShellAPI,

    Resources,
    DialogLongMessage,
    ControlUtils,
    MyStrUtils,
    DebugWindow,
    I88n,

    StringConsts
  ;

  const
    DBSHOWMSG: boolean = false; // Set to true to enable debugging messages for this unit


  constructor TFormSplash.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();

      self.Hide();

      borderStyle := bsNone;

      pnlWholeForm.bevelWidth := 2;
      pnlWholeForm.BevelOuter := bvRaised;
      pnlWholeForm.BevelInner := bvLowered;

      if Screen.PixelsPerInch <> 96 then
        ScaleBy( Screen.PixelsPerInch, 96 )
      ;

      centerChildren( pnlWholeForm );

      self.Show();
    end
  ;


  procedure TFormSplash.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:54 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormSplash.dfm
      // File date: Fri Jan 19 17:43:54 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'NAADSM splash screen' );
          lblVersion.Caption := tr( 'Version' ) + ' ' + MAJORVERSIONNUMBER + '.' + MINORVERSIONNUMBER + ' ' + tr( 'Build' ) + ' ' + BUILDNUMBER;
          lblAdditionalInfo.Caption := tr( 'Please see ''About NAADSM'' for contact and support information.' );
          lblCopyright.Caption := tr( 'Copyright © 2003 - 2012 Colorado State University and University of Guelph' );
        end
      ;

      // If any phrases are found that could not be automatically extracted by
      // Caption Collector, modify the following function to take care of them.
      translateUIManual();
    end
  ;


  procedure TFormSplash.translateUIManual();
    begin
      lblCopyright.Caption := tr( 'Copyright ©' ) + ' ' + COPYRIGHTDATES + ' ' + tr( 'Colorado State University and University of Guelph' );
      lblVersion.Caption := tr( 'Version' ) + ' ' + MAJORVERSIONNUMBER + '.' + MINORVERSIONNUMBER + ' ' + tr( 'Build' ) + ' ' + BUILDNUMBER + ' ' + BRANCHNAME;
    end
  ;

  
 	destructor TFormSplash.destroy();
    begin
      dbcout( 'Splash screen will be destroyed.', DBSHOWMSG );
      frmSplash := nil;
      inherited destroy();
    end
  ;

  procedure TFormSplash.FormClose(Sender: TObject; var Action: TCloseAction);
    begin
      Action := caFree;
    end
  ;


  procedure TFormSplash.timerTimeout(Sender: TObject);
    begin
      close();
    end
  ;


end.
