unit FrameCredits;

(*
FrameCredits.pas/dfm
--------------------
Begin: 2006/10/11
Last revision: $Date: 2012-10-23 22:29:20 $ $Author: areeves $
Version: $Revision: 1.9.10.3 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
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
    ExtCtrls,

    QLists
  ;

  type TFrameCredits = class(TFrame)
    pnlCurrentMembers: TPanel;
      Panel1: TPanel;
    Panel22: TPanel;
    pnlFormerMembers: TPanel;

    protected
      procedure translateUI();
      procedure addPanel( name, dept, inst: string; pnlPair: TPanel; al: TAlign );
      procedure populateCreditsPanel( pnlCreds: TPanel; list: TQObjectList );

    public
      constructor create( AOwner: TComponent ); override;
    end
  ;

  type TPerson = class
    protected
      _name: string;
      _dept: string;
      _inst: string;

    public
      constructor create( name, dept, inst: string );
      destructor destroy(); override;
    end
  ;

implementation

{$R *.dfm}

  uses
    ShellAPI,
    Math,
    
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
    var
      list, list2: TQObjectList;
    begin
      inherited create( AOwner );
      translateUI();

      list := TQObjectList.create();

      list.append( TPerson.create( 'Neil Harvey', 'Department of Computing and Information Science', 'University of Guelph' ) );
      list.append( TPerson.create( 'Aaron Reeves', 'Department of Clinical Sciences', 'Colorado State University' ) );
      list.append( TPerson.create( 'Caroline Dubé', 'Animal Health and Production Division', 'Canadian Food Inspection Agency' ) );
      list.append( TPerson.create( 'W. Bruce McNab', 'Ministry of Agriculture, Food, and Rural Affairs', 'Province of Ontario' ) );
      list.append( TPerson.create( 'Kelly A. Patyk', 'APHIS-VS-CEAH', 'United States Department of Agriculture' ) );
      list.append( TPerson.create( 'Ann H. Seitzinger', 'APHIS-VS-CEAH', 'United States Department of Agriculture' ) );

      list2 := TQObjectList.create();

      list2.append( TPerson.create( 'Celia Antognoli', 'Animal Population Health Institute', 'Colorado State University' ) );
      list2.append( TPerson.create( 'Shaun Case', 'Animal Population Health Institute', 'Colorado State University' ) );
      list2.append( TPerson.create( 'Barbara Corso', 'APHIS-VS-CEAH', 'United States Department of Agriculture' ) );
      list2.append( TPerson.create( 'Conrad Estrada', 'APHIS-VS-NCAHEM', 'United States Department of Agriculture' ) );
      list2.append( TPerson.create( 'Kim N. Forde-Folle', 'APHIS-VS-NCAHEM', 'United States Department of Agriculture' ) );
      list2.append( TPerson.create( 'Ashley E. Hill', 'Animal Population Health Institute', 'Colorado State University' ) );
      list2.append( TPerson.create( 'Ric Hupalo', 'Animal Population Health Institute', 'Colorado State University' ) );
      list2.append( TPerson.create( 'Emery Leger', 'Canadian Food Inspection Agency', '' ) );
      list2.append( TPerson.create( 'Dustin Pendell', 'Dept. of Agricultural and Resource Economics', 'Colorado State University' ) );
      list2.append( TPerson.create( 'Noa Roman-Muniz', 'Animal Population Health Institute', 'Colorado State University' ) );
      list2.append( TPerson.create( 'Mo D. Salman', 'Animal Population Health Institute', 'Colorado State University' ) );
      list2.append( TPerson.create( 'Javier Sanchez', 'Animal Health Risk Assessment Unit', 'Canadian Food Inspection Agency' ) );
      list2.append( TPerson.create( 'Mark A. Schoenbaum', 'APHIS-VS-WRO', 'United States Department of Agriculture' ) );
      list2.append( TPerson.create( 'Anthony "Drew" Schwickerath', 'Animal Population Health Institute', 'Colorado State University' ) );
      list2.append( TPerson.create( 'Snehal Shetye', 'Department of Mechanical Engineering', 'Colorado State University' ) );
      list2.append( TPerson.create( 'Francisco Zagmutt-Vergara', 'Animal Population Health Institute', 'Colorado State University' ) );

      populateCreditsPanel( pnlCurrentMembers, list );
      populateCreditsPanel( pnlFormerMembers, list2 );

      setPanelProperties( self );

      list.freeAllValues();
      list.free();

      list2.freeAllValues();
      list2.free();
    end
  ;


  procedure TFrameCredits.translateUI();
    begin
      Panel1.Caption := tr( 'The NAADSM Development Team' );
    end
  ;


  procedure TFrameCredits.addPanel( name, dept, inst: string; pnlPair: TPanel; al: TAlign );
    var
      pnl: TPanel;
      lblName, lblDept, lblInst: TLabel;
    begin
      pnl := TPanel.create( self );
      pnl.Height := 52;
      pnl.Width := 250;
      pnl.Parent := pnlPair;
      pnl.Align := al;
      pnl.Visible := True;

      lblName := TLabel.Create( self );
      lblName.Parent := pnl;
      lblName.Left := 8;
      lblName.Top := 3;
      lblName.Align := alNone;
      lblName.Caption := name;
      lblName.Visible := True;

      lblDept := TLabel.Create( self );
      lblDept.Parent := pnl;
      lblDept.Left := 8;
      lblDept.Top := 18;
      lblDept.Align := alNone;
      lblDept.Caption := dept;
      lblDept.Visible := True;

      lblInst := TLabel.Create( self );
      lblInst.Parent := pnl;
      lblInst.Left := 8;
      lblInst.Top := 33;
      lblInst.Align := alNone;
      lblInst.Caption := inst;
      lblInst.Visible := True;
    end
  ;


  constructor TPerson.create( name, dept, inst: string );
    begin
      inherited create();
      _name := name;
      _dept := dept;
      _inst := inst;
    end
  ;


  destructor TPerson.destroy();
    begin
      inherited destroy();
    end
  ;


  procedure TFrameCredits.populateCreditsPanel( pnlCreds: TPanel; list: TQObjectList );
    var
      pnlPair: TPanel;
      nPairs: integer;

      i, j: integer;
      person: TPerson;
    begin
      nPairs := ceil( list.count / 2 );

      pnlCreds.Height := nPairs * 52;

      j := 0;

      for i := 0 to nPairs - 1 do
        begin
          pnlPair := TPanel.Create( self );
          pnlPair.Parent := pnlCreds;
          pnlPair.Height := 52;
          pnlPair.Align := alTop;
          pnlPair.Caption := 'pnlPair' + intToStr( i );
          pnlPair.Color := clRed;
          pnlPair.Visible := True;

          person := list.at( j ) as TPerson;
          addPanel( person._name, person._dept, person._inst, pnlPair, alLeft );

          inc( j );
          if( j < list.count ) then
            begin
              person := list.at( j ) as TPerson;
              addPanel( person._name, person._dept, person._inst, pnlPair, alRight );
              inc( j );
            end
          ;
        end
      ;
    end
  ;


end.
