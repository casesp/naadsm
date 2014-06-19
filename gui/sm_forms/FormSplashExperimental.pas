unit FormSplashExperimental;

(*
FormSplashExperimental.pas/dfm
------------------------------
Begin: 2006/04/03
Last revision: $Date: 2013-06-27 19:11:28 $ $Author: areeves $
Version: $Revision: 1.2.8.3 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <aaron.reeves@naadsm.org>
--------------------------------------------------
Copyright (C) 2006 - 2010 Colorado State University

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

  type TFormSplashExperimental = class( TForm )
  		pnlWholeForm: TPanel;
      pnlAppTitle: TPanel;
      Timer1: TTimer;
    pnlLeftSpacer: TPanel;
    pnlTopSpacer: TPanel;
    pnlBody: TPanel;
    lblAppTitle: TLabel;
    lblVersion: TLabel;
    lblCopyright: TLabel;
    lblExplanation: TLabel;
    lblAdditionalInfo: TLabel;
    pnlSpacer1: TPanel;
    pnlSpacer2: TPanel;
    pnlSpacer3: TPanel;

			procedure FormCreate( Sender: TObject );
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
    frmSplashExperimental: TFormSplashExperimental;


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

  constructor TFormSplashExperimental.create( AOwner: TComponent );
      procedure setPanelProperties( ctrl: TWinControl );
        var
    	    i: integer;
          wc: TPanel;
        begin
          for i := 0 to ctrl.controlCount - 1 do
            begin
              if( ctrl.Controls[i] is TPanel ) then
                begin
                  wc := ctrl.Controls[i] as TPanel;
                  wc.BevelOuter := bvNone;
                  wc.Color := clWhite;
                  setPanelProperties( wc );
                end
              ;
            end
          ;
        end
      ;
    begin
      inherited create( AOwner );
      translateUI();

      setPanelProperties( self );
      
      self.Hide();

      borderStyle := bsNone;

      pnlWholeForm.bevelWidth := 2;
      pnlWholeForm.BevelOuter := bvRaised;
      pnlWholeForm.BevelInner := bvLowered;

      centerChildren( pnlWholeForm );
      self.Show();
    end
  ;

  procedure TFormSplashExperimental.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:54 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormSplashExperimental.dfm
      // File date: Thu Oct 12 14:33:58 2006

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'NAADSM splash screen' );
          lblVersion.Caption := tr( 'Version 3.0.78 Build 060403' );
          lblAdditionalInfo.Caption := tr( 'Please see ''About NAADSM'' for contact and support information.' );
          lblAppTitle.Caption := tr( 'NAADSM-Experimental' );
          lblExplanation.Caption := tr( 'This is an experimental version of NAADSM.  This version deviates from the published conceptual specification in ways that may significantly affect the simulation outcome.' );
        end
      ;

      // If any phrases are found that could not be automatically extracted by
      // Caption Collector, modify the following function to take care of them.
      translateUIManual();
    end
  ;


  procedure TFormSplashExperimental.translateUIManual();
    begin
      lblAppTitle.Caption := 'NAADSM-' + BRANCHNAME;
      lblCopyright.Caption := tr( 'Copyright ©' ) + ' ' + COPYRIGHTDATES + ' ' + tr( 'Animal Population Health Institute at Colorado State University and University of Guelph' );
      lblVersion.Caption := tr( 'Version' ) + ' ' + MAJORVERSIONNUMBER + '.' + MINORVERSIONNUMBER + ' ' + tr( 'Build' ) + ' ' + BUILDNUMBER + ' ' + BRANCHNAME;
    end
  ;


  procedure TFormSplashExperimental.FormCreate(Sender: TObject);
 		begin
       if Screen.PixelsPerInch <> 96 then
         ScaleBy( Screen.PixelsPerInch, 96 )
       ;
 		end
	;


  destructor TFormSplashExperimental.destroy();
    begin
      frmSplashExperimental := nil;
      inherited destroy();
    end
  ;

  procedure TFormSplashExperimental.FormClose(Sender: TObject; var Action: TCloseAction);
    begin
      Action := caFree;
    end
  ;


  procedure TFormSplashExperimental.timerTimeout(Sender: TObject);
    begin
      close();
    end
  ;


end.
