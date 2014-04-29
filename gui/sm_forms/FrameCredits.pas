unit FrameCredits;

(*
FrameCredits.pas/dfm
--------------------
Begin: 2006/10/11
Last revision: $Date: 2011-02-01 18:33:30 $ $Author: rhupalo $
Version: $Revision: 1.9.4.2 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2006 - 2010 Animal Population Health Institute, Colorado State University

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

  type TFrameCredits = class(TFrame)
      Panel2: TPanel;
      Panel3: TPanel;
      Panel4: TPanel;
      Panel8: TPanel;
      Panel18: TPanel;
      Panel22: TPanel;
      lblDube: TLabel;
      lblDubeAffiliation: TLabel;
      Label36: TLabel;
      lblHarvey: TLabel;
      Label9: TLabel;
      Label10: TLabel;
      Panel5: TPanel;
      Panel6: TPanel;
      Label2: TLabel;
      Label3: TLabel;
      Panel7: TPanel;
      Label4: TLabel;
      Label6: TLabel;
      lblReeves: TLabel;
      Label5: TLabel;
      Panel9: TPanel;
      Panel10: TPanel;
      Label1: TLabel;
      Label7: TLabel;
      Label8: TLabel;
      Panel11: TPanel;
      Label11: TLabel;
      Label12: TLabel;
      Label13: TLabel;
      Panel12: TPanel;
      Panel14: TPanel;
      Panel19: TPanel;
      Panel20: TPanel;
      Panel21: TPanel;
      Panel23: TPanel;
      Panel24: TPanel;
      Panel25: TPanel;
      Panel29: TPanel;
      Panel30: TPanel;
      Label46: TLabel;
      Label47: TLabel;
      Label48: TLabel;
      Panel31: TPanel;
      Panel32: TPanel;
      Label49: TLabel;
      Label50: TLabel;
      Label51: TLabel;
      Panel33: TPanel;
      Label52: TLabel;
      Label53: TLabel;
      Label54: TLabel;
      Panel34: TPanel;
      Panel35: TPanel;
      lblSchoenbaum: TLabel;
      Label55: TLabel;
      Panel36: TPanel;
      lblZagmutt: TLabel;
      Label56: TLabel;
      Label57: TLabel;
      Label58: TLabel;
      Panel1: TPanel;
    Panel37: TPanel;
    Panel13: TPanel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Panel16: TPanel;
    Panel17: TPanel;
    Label22: TLabel;
    Label20: TLabel;
    Panel15: TPanel;
    Panel38: TPanel;
    Panel39: TPanel;
    Label37: TLabel;
    Label38: TLabel;
    Label59: TLabel;
    Label60: TLabel;
    Label61: TLabel;
    Label62: TLabel;
    Label17: TLabel;
    Label19: TLabel;
    Label18: TLabel;
    Label28: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label29: TLabel;
    Label31: TLabel;
    Label30: TLabel;
    Label33: TLabel;
    Label25: TLabel;
    Label32: TLabel;
    Label34: TLabel;
    Label39: TLabel;
    Label35: TLabel;
    Label42: TLabel;
    Label40: TLabel;
    Label41: TLabel;

    protected
      procedure translateUI();

    public
      constructor create( AOwner: TComponent ); override;
    end
  ;

implementation

{$R *.dfm}

  uses
    ShellAPI,

    I88n,
    ControlUtils,

    StringConsts
  ;

  constructor TFrameCredits.create( AOwner: TComponent );
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
    end
  ;


  procedure TFrameCredits.translateUI();
    begin
      Panel1.Caption := tr( 'The NAADSM Development Team' );
    end
  ;

end.
