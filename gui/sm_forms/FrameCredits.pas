unit FrameCredits;

(*
FrameCredits.pas/dfm
--------------------
Begin: 2006/10/11
Last revision: $Date: 2008/04/21 19:26:19 $ $Author: areeves $
Version: $Revision: 1.7 $
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

  type TFrameCredits = class(TFrame)
      Panel1: TPanel;
      Panel2: TPanel;
      Panel3: TPanel;
      lblContactUS: TLabel;
      lblAffiliationCorso: TLabel;
      lblAddress1Corso: TLabel;
      lblAddress2Corso: TLabel;
    lblEmailFordeFolle: TLabel;
      Panel4: TPanel;
      lblContactCAN: TLabel;
      lblDube: TLabel;
      lblDubeAffiliation: TLabel;
      lblAddress1Dube: TLabel;
      lblAddress2Dube: TLabel;
      lblEmailDube: TLabel;
      Panel5: TPanel;
      Panel6: TPanel;
      lblSupportPC: TLabel;
      lblReeves: TLabel;
      lblEmailReeves: TLabel;
      Panel7: TPanel;
      lblSupportParallel: TLabel;
      lblHarvey: TLabel;
      lblEmailHarvey: TLabel;
      Panel9: TPanel;
      Panel10: TPanel;
      Panel11: TPanel;
      lblHill: TLabel;
      Panel12: TPanel;
      Panel13: TPanel;
      lblSeitzinger: TLabel;
      Panel14: TPanel;
      Panel15: TPanel;
      Panel16: TPanel;
      lblMcNab: TLabel;
      Panel17: TPanel;
      lblSalman: TLabel;
      lblStacey: TLabel;
      Label1: TLabel;
      Label4: TLabel;
      Label5: TLabel;
      Label7: TLabel;
      Label8: TLabel;
      Label9: TLabel;
      Label10: TLabel;
      Label2: TLabel;
      Label11: TLabel;
      Label3: TLabel;
      Label12: TLabel;
      Label13: TLabel;
      Label14: TLabel;
      Label15: TLabel;
      Panel8: TPanel;
      Panel23: TPanel;
      Panel24: TPanel;
      Label18: TLabel;
      Label19: TLabel;
      Label23: TLabel;
      Panel25: TPanel;
      Label20: TLabel;
      Label21: TLabel;
      Label22: TLabel;
      Panel18: TPanel;
      Panel19: TPanel;
      Panel20: TPanel;
      lblSchoenbaum: TLabel;
      Label6: TLabel;
      Panel21: TPanel;
      lblZagmutt: TLabel;
      Label16: TLabel;
      Label17: TLabel;
    Panel22: TPanel;
    Panel26: TPanel;
    Panel27: TPanel;
    Label24: TLabel;
    Panel28: TPanel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label25: TLabel;
    Label29: TLabel;
    lblCorso: TLabel;
    lblFordeFolle: TLabel;

      procedure mailtoClick(Sender: TObject);
      
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
    I88n
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


  procedure TFrameCredits.mailtoClick(Sender: TObject);
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


  procedure TFrameCredits.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:53 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FrameCredits.dfm
      // File date: Mon Oct 30 19:41:36 2006

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Panel1.Caption := tr( 'NAADSM model development team:' );
          lblContactUS.Caption := tr( 'Primary contact (US):' );
          lblCorso.Caption := tr( 'Barbara A. Corso' );
          lblAffiliationCorso.Caption := tr( 'USDA-APHIS-VS-CEAH' );
          lblAddress1Corso.Caption := tr( '2150 Centre Ave., Bldg. B, Mail Stop 2W4' );
          lblAddress2Corso.Caption := tr( 'Fort Collins, CO 80526-8117' );
          lblEmailFordeFolle.Caption := tr( 'Kim.N.Forde-Folle@aphis.usda.gov' );
          lblEmailFordeFolle.Hint := tr( 'Kim.N.Forde-Folle@aphis.usda.gov' );
          lblContactCAN.Caption := tr( 'Primary contact (Canada):' );
          lblDube.Caption := tr( 'Caroline Dubé' );
          lblDubeAffiliation.Caption := tr( 'CFIA-Animal Health and Production Division' );
          lblAddress1Dube.Caption := tr( '59 Camelot' );
          lblAddress2Dube.Caption := tr( 'Ottawa, ON K1A 0Y9' );
          lblEmailDube.Caption := tr( 'dubecm@inspection.gc.ca' );
          lblEmailDube.Hint := tr( 'dubecm@inspection.gc.ca' );
          lblSupportPC.Caption := tr( 'Technical support (Windows/PC):' );
          lblReeves.Caption := tr( 'Aaron Reeves' );
          lblEmailReeves.Caption := tr( 'Aaron.Reeves@colostate.edu' );
          lblEmailReeves.Hint := tr( 'Aaron.Reeves@colostate.edu' );
          Label7.Caption := tr( 'Animal Population Health Institute' );
          Label8.Caption := tr( 'Colorado State University' );
          lblSupportParallel.Caption := tr( 'Technical support (Parallel processing):' );
          lblHarvey.Caption := tr( 'Neil Harvey' );
          lblEmailHarvey.Caption := tr( 'nharvey@uoguelph.ca' );
          lblEmailHarvey.Hint := tr( 'nharvey@uoguelph.ca' );
          Label9.Caption := tr( 'Department of Computing && Information Science' );
          Label10.Caption := tr( 'University of Guelph' );
          Label1.Caption := tr( 'USDA-APHIS-VS-CEAH' );
          lblFordeFolle.Caption := tr( 'Kim N. Forde-Folle' );
          lblHill.Caption := tr( 'Ashley E. Hill' );
          Label2.Caption := tr( 'Animal Population Health Institute' );
          Label11.Caption := tr( 'Colorado State University' );
          lblSeitzinger.Caption := tr( 'Ann H. Seitzinger' );
          Label4.Caption := tr( 'USDA-APHIS-VS-CEAH-NCAHS-NAHMS' );
          lblStacey.Caption := tr( 'Deborah A. Stacey' );
          Label5.Caption := tr( 'University of Guelph' );
          Label15.Caption := tr( 'Department of Computing && Information Science' );
          lblMcNab.Caption := tr( 'W. Bruce McNab' );
          Label13.Caption := tr( 'Ministry of Agriculture Food and Rural Affairs' );
          Label14.Caption := tr( 'Province of Ontario' );
          lblSalman.Caption := tr( 'M.D. Salman' );
          Label3.Caption := tr( 'Colorado State University' );
          Label12.Caption := tr( 'Animal Population Health Institute' );
          Panel8.Caption := tr( 'Additional programming:' );
          Label18.Caption := tr( 'Shaun P. Case' );
          Label19.Caption := tr( 'Department of Computer Science' );
          Label23.Caption := tr( 'Colorado State University' );
          Label20.Caption := tr( 'Snehal Shetye' );
          Label21.Caption := tr( 'Department of Mechanical Engineering' );
          Label22.Caption := tr( 'Colorado State University' );
          Panel22.Caption := tr( 'Former development team members:' );
          lblSchoenbaum.Caption := tr( 'Mark A. Schoenbaum' );
          Label6.Caption := tr( 'USDA-APHIS-VS-WRO' );
          lblZagmutt.Caption := tr( 'Francisco Zagmutt-Vergara' );
          Label16.Caption := tr( 'Animal Population Health Institute' );
          Label17.Caption := tr( 'Colorado State University' );
          Panel18.Caption := tr( 'Spanish translation:' );
        end
      ;

    end
  ;


end.
