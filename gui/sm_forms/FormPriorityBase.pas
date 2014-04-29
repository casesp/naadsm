unit FormPriorityBase;

(*
FormPriorityBase.pas/dfm
------------------------
Begin: 2005/06/08
Last revision: $Date: 2008/11/25 22:00:30 $ $Author: areeves $
Version: $Revision: 1.20 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2005 - 2008 Animal Population Health Institute, Colorado State University

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
    StdCtrls,
    Buttons,
    Menus,
    ActnPopupCtrl,

    FormSMWizardBase,
    ProductionType,
    ProductionTypeList
  ;

  type TFormPriorityBase = class( TFormSMWizardBase )
      lblInstructions: TLabel;
      mmoExamples: TMemo;
      lblExamples: TLabel;
      pnlExamplesBottom: TPanel;
      pnlInstructions: TPanel;
      pnlCaption: TPanel;
      pnlExamplesTop: TPanel;
      lblSpacerLeftTop: TLabel;
      lblSpacerLeftBottom: TLabel;
      lblSpacerRight: TLabel;
      pnlBody: TPanel;
      lblPrimary: TLabel;
      lbxPrimary: TListBox;
      lblSecondary: TLabel;
      lbxProdType: TListBox;
      lbxReason: TListBox;
      lblDaysHolding: TLabel;
      pnlSpacer: TPanel;

      procedure lbxPrimaryDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
      procedure lbxPrimaryDragDrop(Sender, Source: TObject; X, Y: Integer);
      procedure lbxPrimaryEndDrag(Sender, Target: TObject; X, Y: Integer);
      procedure lbxPrimaryClick(Sender: TObject);
      procedure lbxPrimaryMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    protected
    	// Mark's variables, used internally.  Not sure what they're for.
      // But we still need them...
      //---------------------------------------------------------------
    	datahasChanged: boolean;
      LastDraw: shortint;

    	_ptList: TProductionTypeList;

      // These three variables need to be reset in derived classes!
      //-----------------------------------------------------------
      reasonForActivity: string;
      //sortOrder: TProdTypeOrder; // apparently unused?
      //prodTypePriorityColumn: string; // apparently unused?

      procedure translateUI();

      // Key functions for data handling
      //---------------------------------
      procedure initializeFromSim(); override;
      procedure fillReasons(); virtual; abstract;
      procedure fillProdTypes(); virtual; abstract;

      procedure updateReasons(); virtual; abstract;
      function dataIsValid(): boolean; override;
      function getDataUpdated(): boolean; override;

      // Functions of Mark's for UI purposes
      // I don't really know or care what they do.
      //-------------------------------------------
      procedure changeListOrder(
        Button : TMouseButton;
        y : integer;
        lb : TListBox;
        var DataHasChanged : boolean
      );

  		procedure ClearMarkerInListBox(LB : TListBox; var LastDraw : shortint);

     	// Display functions
      //-------------------
      procedure updateDisplay();

    public
    	constructor create( AOwner: TComponent ); override;
    	destructor destroy(); override;
    end
  ;
  

implementation

{$R *.dfm}

	uses
    StrUtils,

    SqlClasses,
    CStringList,
    MyStrUtils,
    GuiStrUtils,
    DebugWindow,
    I88n
  ;


  constructor TFormPriorityBase.create( AOwner: TComponent );
  	begin
      inherited create( AOwner );
      translateUI();

      reasonForActivity := tr( 'Reason for xyz' );
      //sortOrder := OrderUnspecified;
      //prodTypePriorityColumn := tr( 'Unknown' );

   		lbxProdType.Clear();
      lbxReason.Clear();
      lbxPrimary.Clear();
    end
  ;


  procedure TFormPriorityBase.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:54 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormPriorityBase.dfm
      // File date: Wed Oct 25 14:50:17 2006

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Scenario parameters' );
          pnlCaption.Caption := tr( 'CHANGE THIS IN DERIVED CLASSES' );
          lblInstructions.Caption := tr( 'Prioritize by dragging items up or down in both primary or secondary lists.  Drag higher priority items up and lower priority items down.  Or right-click the item in the white list box and change the priority number.' );
          lblExamples.Caption := tr( 'Priority examples for selections above' );
          lblPrimary.Caption := tr( 'Primary' );
          lblSecondary.Caption := tr( 'Secondary' );
          lblDaysHolding.Caption := tr( 'The more days holding, the higher the priority' );
        end
      ;

      // Set TStrings properties
      with self do
        begin
          lbxPrimary.Items[0] := tr( 'Reason for destruction' );
          lbxPrimary.Items[1] := tr( 'Production type' );
          lbxPrimary.Items[2] := tr( 'Days holding' );

          lbxProdType.Items[0] := tr( 'Pigs' );
          lbxProdType.Items[1] := tr( 'Cattle' );
          lbxProdType.Items[2] := tr( 'Dairy' );

          lbxReason.Items[0] := tr( 'Detected' );
          lbxReason.Items[1] := tr( 'Direct contact' );
          lbxReason.Items[2] := tr( 'Indirect contact' );
          lbxReason.Items[3] := tr( 'Ring' );
        end
      ;

    end
  ;


  destructor TFormPriorityBase.destroy();
  	begin
      inherited destroy();
    end
  ;


  procedure TFormPriorityBase.initializeFromSim();
    begin
    	fillProdTypes();
      fillReasons();
      lbxPrimary.ItemIndex := 0;
      lbxPrimaryClick( lbxPrimary );

      updateDisplay();
    end
  ;


  function TFormPriorityBase.getDataUpdated(): boolean;
    begin
      result := dataHasChanged;
    end
  ;


  function TFormPriorityBase.dataIsValid(): boolean;
  	begin
    	result := true;
    end
  ;


	procedure TFormPriorityBase.updateDisplay();
    var
      s, s1, s2, s3 : string;
      l1,l2,l3 : TStrings;
      L : TStringList;
      i1, i2, i3, cnt: word;
      i: integer;
      Line : string;
    begin
      mmoExamples.Lines.Clear();
      L1 := lbxPrimary.Items;
      L2 := lbxPrimary.Items;
      L3 := lbxPrimary.Items;
      s := reasonForActivity;

      i := lbxPrimary.Items.IndexOf(s);

      case i of
        0: begin L1 := lbxReason.Items; s1 := s; end;
        1: begin L2 := lbxReason.Items; s2 := s; end;
        2: begin L3 := lbxReason.Items; s3 := s; end;
        else raise exception.Create( 'TFormPriorityBase: Unrecognized activity for priorities: ''' + s + '''.' );
      end;

      s := tr( 'Production type' );
      i := lbxPrimary.Items.IndexOf(s);

      case i of
        0: begin L1 := lbxProdType.Items; s1 := s; end;
        1: begin L2 := lbxProdType.Items; s2 := s; end;
        2: begin L3 := lbxProdType.Items; s3 := s; end;
        else raise exception.Create( 'TFormPriorityBase: ''Production type'' priority not found.' );
      end;

      s := tr( 'Days holding' );
      i := lbxPrimary.Items.IndexOf(s);

      if( ( i < 0 ) or ( i > 2 ) ) then
        raise exception.Create( 'TFormPriorityBase: ''Days holding'' priority not found.' );
      ;

      L := TStringList.Create();

      try
        L.Append('2...');
        L.Append('1');

        case i of
          0 : begin L1 := L; s1 := s; end;
          1 : begin L2 := L; s2 := s; end;
          else
            begin
              L3 := L;
              s3 := s;
            end
          ;
        end;

        i1 := 0;
        i2 := 0;
        i3 := 0;
        cnt := 0;

        while( i1 < L1.Count ) do
          begin

            Line := s1
              + '='
              + L1[i1]
              + ', '
              + s2
              + '='
              + L2[i2]
              + ', '
              + s3
              + '='
              + L3[i3]
            ;

            Inc( Cnt );
            Line := IntToStr( cnt ) + '. ' + Line;
            mmoExamples.Lines.Append( Line );

            if i3 < L3.Count - 1 then
              Inc(i3)
            else
              begin
                i3 := 0;
                if i2 < L2.Count - 1 then
                  Inc(i2)
                else
                  begin
                    i2 := 0;
                    if i1 < L1.Count then
                      Inc(i1)
                    else
                      i1 := 0
                    ;
                  end
                ;
              end
            ;
        	end
        ;

      finally
        L.Free;
        mmoExamples.SelStart := 0;
        mmoExamples.SelLength := 0;
      end;

    end
  ;


  procedure TFormPriorityBase.ClearMarkerInListBox(LB : TListBox; var LastDraw : shortint);
    var
      w, ih : integer;
    begin
      if LastDraw <> -1 then
        begin
          w := LB.Width;
          ih := LB.ItemHeight;
          LB.Canvas.Pen.Color := clWhite;
          LB.Canvas.Pen.Style := psDot;
          LB.Canvas.MoveTo(5, LastDraw*ih);
          LB.Canvas.LineTo(w, LastDraw*ih);
          LastDraw := -1;
        end
      ;
    end
  ;


  procedure TFormPriorityBase.lbxPrimaryDragOver(
        Sender, Source: TObject;
        X, Y: Integer;
        State: TDragState;
        var Accept: Boolean
      );
  	var
      st, ds, ih : integer;
      w : integer;
    begin
      if (Source is TListBox) and (Sender is TListBox) then
        begin
          ClearMarkerInListBox(Sender as TListBox,LastDraw);

          st := (Source as TListBox).ItemIndex;
          ih := (Source as TListBox).ItemHeight;
          w := (Sender as TListBox).Width;
          ds := Y div ih;

          if ds > (Source as TListBox).Items.Count then
            ds := (Source as TListBox).Items.Count
          ;

          if (ds <> st) then
            begin
              (Sender as TListBox).Canvas.Pen.Color := clBlack;
              (Sender as TListBox).Canvas.Pen.Style := psDot;
              (Sender as TListBox).Canvas.MoveTo(5, ds*ih);
              (Sender as TListBox).Canvas.LineTo(w, ds*ih);
              LastDraw := ds;
            end
          ;
        end
      ;
    end
  ;


  procedure TFormPriorityBase.lbxPrimaryDragDrop(
        Sender, Source: TObject;
        X, Y: Integer
      );
    var
      ih : integer;
      st, ds : integer;
      s : string;
    begin
      if (Source is TListBox) and (Sender is TListBox) then
        begin
          // erase the line
          ClearMarkerInListBox(Source as TListBox,LastDraw);
          ih := (Source as TListBox).ItemHeight;
          st := (Source as TListBox).ItemIndex;
          ds := Y div ih;

          if ds > (Source as TListBox).Items.Count - 1 then
            ds := (Source as TListBox).Items.Count
          ;

          if ds > st then
            begin
              s := (Sender as TListBox).Items[st];
              (Sender as TListBox).Items.Delete(st);
              (Sender as TListBox).Items.Insert(ds-1,s);
              (Sender as TListBox).ItemIndex := ds-1;
            end
          else
            begin
              s := (Sender as TListBox).Items[st];
              (Sender as TListBox).Items.Delete(st);
              (Sender as TListBox).Items.Insert(ds,s);
              (Sender as TListBox).ItemIndex := ds;
            end
          ;

          DataHasChanged := True;
          updateDisplay();
        end
      ;
    end
  ;


  procedure TFormPriorityBase.lbxPrimaryEndDrag(
        Sender, Target: TObject;
        X, Y: Integer
      );
    begin
      ClearMarkerInListBox(lbxPrimary,LastDraw);
    end
  ;


  procedure TFormPriorityBase.lbxPrimaryClick(Sender: TObject);
    var
      tp, lft: integer;
      cp : string;
    begin
      tp := lbxProdType.Top;
      lft := lbxProdType.Left;

      With (Sender as TListBox) do
      if ItemIndex = -1 then
        cp := ''
      else
        cp := Items[ItemIndex]
      ;

      if( cp = reasonForActivity ) then // Note that reasonForActivity does not need to be translated here.
        begin
          lblDaysHolding.Hide();
          lbxProdType.Hide();
          lbxReason.Top := tp;
          lbxReason.Left := lft;
          lbxReason.Show();
        end
      else
      if cp = tr( 'Production type' ) then
        begin
          lblDaysHolding.Hide();
          lbxReason.Hide();
          lbxProdType.Top := tp;
          lbxProdType.Left := lft;
          lbxProdType.Show();
        end
      else
      if cp = tr( 'Days holding' ) then
        begin
          lbxReason.Hide();
          lbxProdType.Hide();
          lblDaysHolding.Top := tp + lblDaysHolding.Height;
          lblDaysHolding.Left := lft;
          lblDaysHolding.Show();
        end
      else
        begin
          lblDaysHolding.Hide();
          lbxProdType.Hide();
          lbxReason.Hide();
        end
      ;
    end
  ;


  procedure TFormPriorityBase.changeListOrder(
        Button : TMouseButton;
        y : integer;
        lb : TListBox;
        var DataHasChanged : boolean
      );
    var
      ih, p, np, xe : integer;
      ns : string;
      Entry : string;
    begin
      if Button = mbRight then
        begin
          ih := lb.ItemHeight;
          p := y div ih;
          if p < (lb.Items.Count) then
            begin
              lb.ItemIndex := p;

              ns := Inputbox(
              	tr( 'Priority order' ),
                tr( 'Enter a new order number to change priority' ),
                IntToStr(p+1)
              );

              Val(ns, np, xe);

              if (xe = 0) and (ns <> IntToStr(p+1)) then
                begin
                  Entry := lb.Items[p];
                  lb.Items.Delete(p);

                  if np > lb.Items.Count then
                    lb.Items.Append(Entry)
                  else
                    lb.Items.Insert(np-1, Entry)
                  ;

                  lb.ItemIndex := lb.Items.IndexOf(Entry);
                  DataHasChanged := True;
                end
              ;
            end
          ;
        end
      ;
    end
	;


  procedure TFormPriorityBase.lbxPrimaryMouseDown(
        Sender: TObject;
        Button: TMouseButton;
        Shift: TShiftState;
        X, Y: Integer
      );
    begin
      changeListOrder( Button, y, (sender as TListBox), DataHasChanged );
      if (Sender as TListBox).Name = 'lbxPrimary' then lbxPrimaryClick(Sender);
      if DataHasChanged then updateDisplay();
    end
  ;

end.
