unit FrameSingleCostCurve;

(*
FrameSingleCostCurve.pas/dfm
----------------------------
Begin: 2006/01/19
Last revision: $Date: 2009-06-05 19:52:36 $ $Author: areeves $
Version: $Revision: 1.9 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2006 - 2009 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
    // Standard Delphi units
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
    TeeProcs,
    TeEngine,
    Chart,
    ExtCtrls,
    Series,

    // General purpose units
    QVectors,

    // Application-specific widgets
    FrameChartBase
  ;


  type TFrameSingleCostCurve = class( TFrameChartBase )
      pnlCumulCosts: TPanel;
      cbxCumulCosts: TCheckBox;

      pnlCostCategories: TPanel;

      lblDestrCosts: TLabel;

      cbxAppraisal: TCheckBox;
      lineAppraisal: TPanel;

      cbxCAndD: TCheckBox;
      lineCAndD: TPanel;

      cbxEuthanasia: TCheckBox;
      lineEuthanasia: TPanel;

      cbxIndemnification: TCheckBox;
      lineIndemnification: TPanel;

      cbxDisposal: TCheckBox;
      lineDisposal: TPanel;

      cbxDestrSubtotal: TCheckBox;
      lineDestrSubtotal: TPanel;

      lblVaccCosts: TLabel;

      cbxVaccSetup: TCheckBox;
      lineVaccSetup: TPanel;

      cbxVacc: TCheckBox;
      lineVacc: TPanel;


      cbxVaccSubtotal: TCheckBox;
      lineVaccSubtotal: TPanel;

      cbxTotal: TCheckBox;
      lineTotal: TPanel;

      pnlChart: TPanel;
      chtCosts: TChart;
      cbx3D: TCheckBox;
      serTotal: TLineSeries;
      serDestrSubtotal: TLineSeries;
      serVaccSubtotal: TLineSeries;
      serAppraisal: TLineSeries;
      serCAndD: TLineSeries;
      serEuthanasia: TLineSeries;
      serIndemnification: TLineSeries;
      serDisposal: TLineSeries;
      serVaccSetup: TLineSeries;
      serVacc: TLineSeries;
    lblInvisible: TLabel;
    Label1: TLabel;

      procedure cbx3DClick(Sender: TObject);
      procedure cbxCumulCostsClick(Sender: TObject);
      procedure cbxCategoryClicked( Sender: TObject );

    protected
      _arrTotal: TQDoubleVector;
      _arrDestrSubtotal: TQDoubleVector;
      _arrAppraisal: TQDoubleVector;
      _arrCAndD: TQDoubleVector;
      _arrEuthanasia: TQDoubleVector;
      _arrIndemnification: TQDoubleVector;
      _arrDisposal: TQDoubleVector;
      _arrVaccSetup: TQDoubleVector;
      _arrVacc: TQDoubleVector;
      _arrVaccSubtotal: TQDoubleVector;

      _arrTotalCumul: TQDoubleVector;
      _arrDestrSubtotalCumul: TQDoubleVector;
      _arrAppraisalCumul: TQDoubleVector;
      _arrCAndDCumul: TQDoubleVector;
      _arrEuthanasiaCumul: TQDoubleVector;
      _arrIndemnificationCumul: TQDoubleVector;
      _arrDisposalCumul: TQDoubleVector;
      _arrVaccSetupCumul: TQDoubleVector;
      _arrVaccCumul: TQDoubleVector;
      _arrVaccSubtotalCumul: TQDoubleVector;

      _chartFixNeeded: boolean;

      procedure translateUI();
      procedure translateUIManual();

      procedure setupCheckBoxes();

      procedure redrawSeries( series: TLineSeries; arr: TQDoubleVector; arrCumul: TQDoubleVector );

      procedure fixTChartBug();
      function minYEqualsMaxY(): boolean;
      function minXEqualsMaxX(): boolean;
      procedure setVertAxisMinMax( min, max: double );
      procedure setVertAxisAutomatic();
      procedure setHorizAxisMinMax( min, max: double );
      procedure setHorizAxisAutomatic();

    public
      constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

      procedure clearAllSeries();

      procedure setArrays(
        arrTotal: TQDoubleVector;

        arrDestrSubtotal: TQDoubleVector;
        arrAppraisal: TQDoubleVector;
        arrCAndD: TQDoubleVector;
        arrEuthanasia: TQDoubleVector;
        arrIndemnification: TQDoubleVector;
        arrDisposal: TQDoubleVector;
        arrVaccSetup: TQDoubleVector;
        arrVacc: TQDoubleVector;
        arrVaccSubtotal: TQDoubleVector;

        arrTotalCumul: TQDoubleVector;
        arrDestrSubtotalCumul: TQDoubleVector;
        arrAppraisalCumul: TQDoubleVector;
        arrCAndDCumul: TQDoubleVector;
        arrEuthanasiaCumul: TQDoubleVector;
        arrIndemnificationCumul: TQDoubleVector;
        arrDisposalCumul: TQDoubleVector;
        arrVaccSetupCumul: TQDoubleVector;
        arrVaccCumul: TQDoubleVector;
        arrVaccSubtotalCumul: TQDoubleVector
      );

      procedure redrawAllSeries();

      procedure updateForDay( const day: integer );
    end
  ;

  const
    DBFRAMESINGLECOSTCURVE: boolean = false; // Set to true to enable debugging messages for this unit


implementation

{$R *.dfm}


  uses
    // General purpose units
    MyStrUtils,
    DebugWindow,
    I88n
  ;

//-----------------------------------------------------------------------------
// Construction/destruction
//-----------------------------------------------------------------------------
  constructor TFrameSingleCostCurve.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();

      setupCheckBoxes();

      _chartFixNeeded := true;
    end
  ;


  procedure TFrameSingleCostCurve.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.2.
      // Generation date: Thu Feb 28 20:52:37 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FrameSingleCostCurve.dfm
      // File date: Thu Feb 28 09:42:46 2008

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          cbxCumulCosts.Caption := tr( 'Show cumulative costs' );
          cbx3D.Caption := tr( '3-D View' );

          lblDestrCosts.Caption := tr( 'Itemized destruction costs' );
          lblVaccCosts.Caption := tr( 'Itemized vaccination costs' );

          cbxAppraisal.Caption := tr( 'Appraisal' );
          cbxCAndD.Caption := tr( 'Cleaning and disinfection' );
          cbxEuthanasia.Caption := tr( 'Euthanasia' );
          cbxIndemnification.Caption := tr( 'Indemnification' );
          cbxDisposal.Caption := tr( 'Disposal' );
          cbxDestrSubtotal.Caption := tr( 'Destruction subtotal' );
          cbxVaccSetup.Caption := tr( 'Site setup' );
          cbxVacc.Caption := tr( 'Vaccination' );
          cbxVaccSubtotal.Caption := tr( 'Vaccination subtotal' );
          cbxTotal.Caption := tr( 'Total costs' );
        end
      ;

      // Set TChart properties
      with self do
        begin
          chtCosts.BottomAxis.Title.Caption := tr( 'Day of outbreak' );
          chtCosts.LeftAxis.Title.Caption := tr( 'Cost ($)' );
          chtCosts.Title.Text.Strings[0] := tr( 'Direct costs of outbreak by day' );
        end
      ;

      // If any phrases are found that could not be automatically extracted by
      // Caption Collector, modify the following function to take care of them.
      translateUIManual();
    end
  ;


  procedure TFrameSingleCostCurve.translateUIManual();
    begin
    end
  ;


  procedure TFrameSingleCostCurve.setupCheckBoxes();
    var
      startLeft, newLeft: integer;
      textWidth: integer;
      canvas: TCanvas;
      lineOffset, space: integer;
    begin
      canvas := lblInvisible.Canvas;

      startLeft := 16;
      lineOffset := 16;
      space := 40;

      // Top row: Destruction
      //---------------------
      newLeft := startLeft;
      textWidth := canvas.TextWidth( tr( 'Appraisal' ) );
      cbxAppraisal.Left := newLeft;
      lineAppraisal.Left := newLeft + lineOffset;
      lineAppraisal.Width := textWidth + 6;

      newLeft := newLeft + textWidth + space;
      textWidth := canvas.TextWidth( tr( 'Cleaning and disinfection' ) );
      cbxCAndD.Left := newLeft;
      lineCAndD.Left := newLeft + lineOffset;
      lineCAndD.Width := textWidth + 6;

      newLeft := newLeft + textWidth + space;
      textWidth := canvas.TextWidth( tr( 'Euthanasia' ) );
      cbxEuthanasia.Left := newLeft;
      lineEuthanasia.Left := newLeft + lineOffset;
      lineEuthanasia.Width := textWidth + 6;

      newLeft := newLeft + textWidth + space;
      textWidth := canvas.TextWidth( tr( 'Indemnification' ) );
      cbxIndemnification.Left := newLeft;
      lineIndemnification.Left := newLeft + lineOffset;
      lineIndemnification.Width := textWidth + 6;

      newLeft := newLeft + textWidth + space;
      textWidth := canvas.TextWidth( tr( 'Disposal' ) );
      cbxDisposal.Left := newLeft;
      lineDisposal.Left := newLeft + lineOffset;
      lineDisposal.Width := textWidth + 6;

      // Second row: Vaccination
      //------------------------
      newLeft := startLeft;
      textWidth := canvas.TextWidth( tr( 'Site setup' ) );
      cbxVaccSetup.Left := newLeft;
      lineVaccSetup.Left := newLeft + lineOffset;
      lineVaccSetup.Width := textWidth + 6;

      newLeft := newLeft + textWidth + space;
      textWidth := canvas.TextWidth( tr( 'Vaccination' ) );
      cbxVacc.Left := newLeft;
      lineVacc.Left := newLeft + lineOffset;
      lineVacc.Width := textWidth + 6;

      // Third row: Totals
      //------------------
      newLeft := startLeft;
      textWidth := canvas.TextWidth( tr( 'Destruction subtotal' ) );
      cbxDestrSubtotal.Left := newLeft;
      lineDestrSubtotal.Left := newLeft + lineOffset;
      lineDestrSubtotal.Width := textWidth + 6;

      newLeft := newLeft + textWidth + space;
      textWidth := canvas.TextWidth( tr( 'Vaccination subtotal' ) );
      cbxVaccSubtotal.Left := newLeft;
      lineVaccSubtotal.Left := newLeft + lineOffset;
      lineVaccSubtotal.Width := textWidth + 6;

      newLeft := newLeft + textWidth + space;
      textWidth := canvas.TextWidth( tr( 'Total costs' ) );
      cbxTotal.Left := newLeft;
      lineTotal.Left := newLeft + lineOffset;
      lineTotal.Width := textWidth + 6;
    end
  ;


  procedure TFrameSingleCostCurve.setArrays(
        arrTotal: TQDoubleVector;

        arrDestrSubtotal: TQDoubleVector;
        arrAppraisal: TQDoubleVector;
        arrCAndD: TQDoubleVector;
        arrEuthanasia: TQDoubleVector;
        arrIndemnification: TQDoubleVector;
        arrDisposal: TQDoubleVector;
        arrVaccSetup: TQDoubleVector;
        arrVacc: TQDoubleVector;
        arrVaccSubtotal: TQDoubleVector;

        arrTotalCumul: TQDoubleVector;
        arrDestrSubtotalCumul: TQDoubleVector;
        arrAppraisalCumul: TQDoubleVector;
        arrCAndDCumul: TQDoubleVector;
        arrEuthanasiaCumul: TQDoubleVector;
        arrIndemnificationCumul: TQDoubleVector;
        arrDisposalCumul: TQDoubleVector;
        arrVaccSetupCumul: TQDoubleVector;
        arrVaccCumul: TQDoubleVector;
        arrVaccSubtotalCumul: TQDoubleVector
      );
    begin
      dbcout( 'TFrameSingleCostCurve.setArrays...', DBFRAMESINGLECOSTCURVE );

      _arrTotal := arrTotal;

      _arrDestrSubtotal := arrDestrSubtotal;
      _arrAppraisal := arrAppraisal;
      _arrCAndD := arrCAndD;
      _arrEuthanasia := arrEuthanasia;
      _arrIndemnification := arrIndemnification;
      _arrDisposal := arrDisposal;
      _arrVaccSetup := arrVaccSetup;
      _arrVacc := arrVacc;
      _arrVaccSubtotal := arrVaccSubtotal;

      _arrTotalCumul := arrTotalCumul;
      _arrDestrSubtotalCumul := arrDestrSubtotalCumul;
      _arrAppraisalCumul := arrAppraisalCumul;
      _arrCAndDCumul := arrCAndDCumul;
      _arrEuthanasiaCumul := arrEuthanasiaCumul;
      _arrIndemnificationCumul := arrIndemnificationCumul;
      _arrDisposalCumul := arrDisposalCumul;
      _arrVaccSetupCumul := arrVaccSetupCumul;
      _arrVaccCumul := arrVaccCumul;
      _arrVaccSubtotalCumul := arrVaccSubtotalCumul;

      dbcout( 'Done TFrameSingleCostCurve.setArrays', DBFRAMESINGLECOSTCURVE );
    end
  ;


  destructor TFrameSingleCostCurve.destroy();
    begin
      // Don't destroy arrays: they're just references.
      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Chart drawing functions
//-----------------------------------------------------------------------------
  procedure TFrameSingleCostCurve.clearAllSeries();
    begin
      dbcout( 'TFrameSingleCostCurve.clearAllSeries...', DBFRAMESINGLECOSTCURVE );

      serTotal.Clear();

      serDestrSubtotal.Clear();
      serAppraisal.Clear();
      serCAndD.Clear();
      serEuthanasia.Clear();
      serIndemnification.Clear();
      serDisposal.Clear();

      serVaccSubtotal.Clear();
      serVaccSetup.Clear();
      serVacc.Clear();

      _chartFixNeeded := true;

      dbcout( 'Done TFrameSingleCostCurve.clearAllSeries', DBFRAMESINGLECOSTCURVE );
    end
  ;


  procedure TFrameSingleCostCurve.redrawAllSeries();
    begin
      dbcout( 'TFrameSingleCostCurve.redrawAllSeries...', DBFRAMESINGLECOSTCURVE );
      clearAllSeries();

      if( cbxTotal.Checked ) then redrawSeries( serTotal, _arrTotal, _arrTotalCumul );

      if( cbxDestrSubtotal.Checked ) then redrawSeries( serDestrSubtotal, _arrDestrSubtotal, _arrDestrSubtotalCumul );

      if( cbxAppraisal.Checked ) then redrawSeries( serAppraisal, _arrAppraisal, _arrAppraisalCumul );
      if( cbxCAndD.Checked ) then redrawSeries( serCAndD, _arrCAndD, _arrCAndDCumul );
      if( cbxEuthanasia.Checked ) then redrawSeries( serEuthanasia, _arrEuthanasia, _arrEuthanasiaCumul );
      if( cbxIndemnification.Checked ) then redrawSeries( serIndemnification, _arrIndemnification, _arrIndemnificationCumul );
      if( cbxDisposal.Checked ) then redrawSeries( serDisposal, _arrDisposal, _arrDisposalCumul );

      if( cbxVaccSetup.Checked ) then redrawSeries( serVaccSetup, _arrVaccSetup, _arrVaccSetupCumul );
      if( cbxVacc.Checked ) then redrawSeries( serVacc, _arrVacc, _arrVaccCumul );


      if( cbxVaccSubtotal.Checked ) then redrawSeries( serVaccSubtotal, _arrVaccSubtotal, _arrVaccSubtotalCumul );

      dbcout( 'Done TFrameSingleCostCurve.redrawAllSeries', DBFRAMESINGLECOSTCURVE );
    end
  ;


  procedure TFrameSingleCostCurve.redrawSeries( series: TLineSeries; arr: TQDoubleVector; arrCumul: TQDoubleVector );
    var
      i: integer;
      useCumul: boolean;
    begin
      dbcout( 'TFrameSingleCostCurve.redrawSeries "' + series.Name + '"... ', DBFRAMESINGLECOSTCURVE );

      series.Clear();

      useCumul := cbxCumulCosts.Checked;

      for i := 0 to arr.Count - 1 do
        begin
          if( useCumul ) then
            series.AddXY( i + 1, arrCumul[i] )
          else
            series.AddXY( i + 1, arr[i] )
          ;
        end
      ;

      fixTChartBug();

      dbcout( 'Done TFrameSingleCostCurve.redrawSeries', DBFRAMESINGLECOSTCURVE );
    end
  ;


  procedure TFrameSingleCostCurve.updateForDay( const day: integer );
    begin
      if( cbxCumulCosts.Checked ) then
        begin
          if( cbxTotal.Checked ) then serTotal.AddXY( day, _arrTotalCumul[day - 1] );
          if( cbxDestrSubtotal.Checked ) then serDestrSubtotal.AddXY( day, _arrDestrSubtotalCumul[day - 1] );
          if( cbxAppraisal.Checked ) then serAppraisal.AddXY( day, _arrAppraisalCumul[day - 1] );
          if( cbxCAndD.Checked ) then serCAndD.AddXY( day, _arrCAndDCumul[day - 1] );
          if( cbxEuthanasia.Checked ) then serEuthanasia.AddXY( day, _arrEuthanasiaCumul[day - 1] );
          if( cbxIndemnification.Checked ) then serIndemnification.AddXY( day, _arrIndemnificationCumul[day - 1] );
          if( cbxDisposal.Checked ) then serDisposal.AddXY( day, _arrDisposalCumul[day - 1] );
          if( cbxVaccSetup.Checked ) then serVaccSetup.AddXY( day, _arrVaccSetupCumul[day - 1] );
          if( cbxVacc.Checked ) then serVacc.AddXY( day, _arrVaccCumul[day - 1] );
          if( cbxVaccSubtotal.Checked ) then serVaccSubtotal.AddXY( day, _arrVaccSubtotalCumul[day - 1] );
        end
      else
        begin
          if( cbxTotal.Checked ) then serTotal.AddXY( day, _arrTotal[day - 1] );
          if( cbxDestrSubtotal.Checked ) then serDestrSubtotal.AddXY( day, _arrDestrSubtotal[day - 1] );
          if( cbxAppraisal.Checked ) then serAppraisal.AddXY( day, _arrAppraisal[day - 1] );
          if( cbxCAndD.Checked ) then serCAndD.AddXY( day, _arrCAndD[day - 1] );
          if( cbxEuthanasia.Checked ) then serEuthanasia.AddXY( day, _arrEuthanasia[day - 1] );
          if( cbxIndemnification.Checked ) then serIndemnification.AddXY( day, _arrIndemnification[day - 1] );
          if( cbxDisposal.Checked ) then serDisposal.AddXY( day, _arrDisposal[day - 1] );
          if( cbxVaccSetup.Checked ) then serVaccSetup.AddXY( day, _arrVaccSetup[day - 1] );
          if( cbxVacc.Checked ) then serVacc.AddXY( day, _arrVacc[day - 1] );
          if( cbxVaccSubtotal.Checked ) then serVaccSubtotal.AddXY( day, _arrVaccSubtotal[day - 1] );
        end
      ;

      fixTChartBug();
    end
  ;


  procedure TFrameSingleCostCurve.fixTChartBug();
    var
      yNeededFix, xNeededFix: boolean;
      visSeries: TLineSeries;
    begin
      // if all series have the same min and max Y then
      // determine what the proper vert axis should be, and apply it to all series.
      // Otherwise, use automatic vert axis for all series.

      // Do the same for X values and horiz axis.

      // Points are added repeatedly to each series in this unit.
      // Once we get to the point where neither axis needed to have the fix applied when a new point
      // was added, it is no longer necessary to continue to carry out this check.
      // Variable _chartFixNeeded is used to indicate whether or not it is OK to stop checking.

      if( _chartFixNeeded ) then
        begin

          (*
          visSeries := getVisibleSeries();

          if( nil = visSeries ) then
            exit
          ;
          *)

          visSeries := serTotal;

          if( minYEqualsMaxY() ) then
            begin
              setVertAxisMinMax( visSeries.YValues.MinValue - 1.0, visSeries.YValues.MinValue + 1.0 );
              yNeededFix := true;
            end
          else
            begin
              setVertAxisAutomatic();
              yNeededFix := false;
            end
          ;

          if( minXEqualsMaxX() ) then
            begin
              setHorizAxisMinMax( visSeries.XValues.MinValue - 1.0, visSeries.XValues.MinValue + 1.0 );
              xNeededFix := true;
            end
          else
            begin
              setHorizAxisAutomatic();
              xNeededFix := false;
            end
          ;

          _chartFixNeeded := ( yNeededFix or xNeededFix );
        end
      ;
    end
  ;


  function TFrameSingleCostCurve.minYEqualsMaxY(): boolean;
    var
      val: double;
    begin
      result := false;

      if( serTotal.YValues.MaxValue <> serTotal.YValues.MinValue ) then
        exit
      else
        val := serTotal.YValues.MaxValue
      ;

      if( not( ( serDestrSubtotal.YValues.MaxValue = val ) and ( serDestrSubtotal.YValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( serAppraisal.YValues.MaxValue = val ) and ( serAppraisal.YValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( serCAndD.YValues.MaxValue = val ) and ( serCAndD.YValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( serEuthanasia.YValues.MaxValue = val ) and ( serEuthanasia.YValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( serIndemnification.YValues.MaxValue = val ) and ( serIndemnification.YValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( serDisposal.YValues.MaxValue = val ) and ( serDisposal.YValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( serVaccSetup.YValues.MaxValue = val ) and ( serVaccSetup.YValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( serVacc.YValues.MaxValue = val ) and ( serVacc.YValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( serVaccSubtotal.YValues.MaxValue = val ) and ( serVaccSubtotal.YValues.MaxValue = val ) ) ) then
        exit
      else
        result := true
      ;
    end
  ;


  function TFrameSingleCostCurve.minXEqualsMaxX(): boolean;
    var
      val: double;
    begin
      result := false;

      if( serTotal.XValues.MaxValue <> serTotal.XValues.MinValue ) then
        exit
      else
        val := serTotal.XValues.MaxValue
      ;

      if( not( ( serDestrSubtotal.XValues.MaxValue = val ) and ( serDestrSubtotal.XValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( serAppraisal.XValues.MaxValue = val ) and ( serAppraisal.XValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( serCAndD.XValues.MaxValue = val ) and ( serCAndD.XValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( serEuthanasia.XValues.MaxValue = val ) and ( serEuthanasia.XValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( serIndemnification.XValues.MaxValue = val ) and ( serIndemnification.XValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( serDisposal.XValues.MaxValue = val ) and ( serDisposal.XValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( serVaccSetup.XValues.MaxValue = val ) and ( serVaccSetup.XValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( serVacc.XValues.MaxValue = val ) and ( serVacc.XValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( serVaccSubtotal.XValues.MaxValue = val ) and ( serVaccSubtotal.XValues.MaxValue = val ) ) ) then
        exit
      else
        result := true
      ;
    end
  ;


  procedure TFrameSingleCostCurve.setVertAxisMinMax( min, max: double );
    begin
      serTotal.GetVertAxis.SetMinMax( min, max );

      serDestrSubtotal.GetVertAxis.SetMinMax( min, max );
      serAppraisal.GetVertAxis.SetMinMax( min, max );
      serCAndD.GetVertAxis.SetMinMax( min, max );
      serEuthanasia.GetVertAxis.SetMinMax( min, max );
      serIndemnification.GetVertAxis.SetMinMax( min, max );
      serDisposal.GetVertAxis.SetMinMax( min, max );

      serVaccSetup.GetVertAxis.SetMinMax( min, max );
      serVacc.GetVertAxis.SetMinMax( min, max );
      serVaccSubtotal.GetVertAxis.SetMinMax( min, max );
    end
  ;


  procedure TFrameSingleCostCurve.setVertAxisAutomatic();
    begin
      serTotal.GetVertAxis.Automatic := true;

      serDestrSubtotal.GetVertAxis.Automatic := true;
      serAppraisal.GetVertAxis.Automatic := true;
      serCAndD.GetVertAxis.Automatic := true;
      serEuthanasia.GetVertAxis.Automatic := true;
      serIndemnification.GetVertAxis.Automatic := true;
      serDisposal.GetVertAxis.Automatic := true;

      serVaccSetup.GetVertAxis.Automatic := true;
      serVacc.GetVertAxis.Automatic := true;
      serVaccSubtotal.GetVertAxis.Automatic := true;
    end
  ;

  procedure TFrameSingleCostCurve.setHorizAxisMinMax( min, max: double );
    begin
      serTotal.GetHorizAxis.SetMinMax( min, max );

      serDestrSubtotal.GetHorizAxis.SetMinMax( min, max );
      serAppraisal.GetHorizAxis.SetMinMax( min, max );
      serCAndD.GetHorizAxis.SetMinMax( min, max );
      serEuthanasia.GetHorizAxis.SetMinMax( min, max );
      serIndemnification.GetHorizAxis.SetMinMax( min, max );
      serDisposal.GetHorizAxis.SetMinMax( min, max );

      serVaccSetup.GetHorizAxis.SetMinMax( min, max );
      serVacc.GetHorizAxis.SetMinMax( min, max );
      serVaccSubtotal.GetHorizAxis.SetMinMax( min, max );
    end
  ;


  procedure TFrameSingleCostCurve.setHorizAxisAutomatic();
    begin
      serTotal.GetHorizAxis.Automatic := true;

      serDestrSubtotal.GetHorizAxis.Automatic := true;
      serAppraisal.GetHorizAxis.Automatic := true;
      serCAndD.GetHorizAxis.Automatic := true;
      serEuthanasia.GetHorizAxis.Automatic := true;
      serIndemnification.GetHorizAxis.Automatic := true;
      serDisposal.GetHorizAxis.Automatic := true;

      serVaccSetup.GetHorizAxis.Automatic := true;
      serVacc.GetHorizAxis.Automatic := true;
      serVaccSubtotal.GetHorizAxis.Automatic := true;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI event handlers
//-----------------------------------------------------------------------------
  procedure TFrameSingleCostCurve.cbx3DClick( Sender: TObject );
    begin
      chtCosts.View3D := not chtCosts.View3D;
    end
  ;


  procedure TFrameSingleCostCurve.cbxCumulCostsClick(Sender: TObject);
    begin
      redrawAllSeries();
    end
  ;


  procedure TFrameSingleCostCurve.cbxCategoryClicked( Sender: TObject );
    begin
      if( cbxTotal = sender ) then
        begin
          if( cbxTotal.Checked ) then
            redrawSeries( serTotal, _arrTotal, _arrTotalCumul )
          else
            serTotal.Clear()
          ;
        end
      else if( cbxDestrSubtotal = sender ) then
        begin
          if( cbxDestrSubtotal.Checked ) then
            redrawSeries( serDestrSubtotal, _arrDestrSubtotal, _arrDestrSubtotalCumul )
          else
            serDestrSubtotal.Clear()
          ;
        end
      else if( cbxAppraisal = sender ) then
        begin
          if( cbxAppraisal.Checked ) then
            redrawSeries( serAppraisal, _arrAppraisal, _arrAppraisalCumul )
          else
            serAppraisal.Clear()
          ;
        end
      else if( cbxCAndD = sender ) then
        begin
          if( cbxCAndD.Checked ) then
            redrawSeries( serCAndD, _arrCAndD, _arrCAndDCumul )
          else
            serCAndD.Clear()
          ;
        end
      else if( cbxEuthanasia = sender ) then
        begin
          if( cbxEuthanasia.Checked ) then
            redrawSeries( serEuthanasia, _arrEuthanasia, _arrEuthanasiaCumul )
          else
            serEuthanasia.Clear()
          ;
        end
      else if( cbxIndemnification = sender ) then
        begin
          if( cbxIndemnification.Checked ) then
            redrawSeries( serIndemnification, _arrIndemnification, _arrIndemnificationCumul )
          else
            serIndemnification.Clear()
          ;
        end
      else if( cbxDisposal = sender ) then
        begin
          if( cbxDisposal.Checked ) then
            redrawSeries( serDisposal, _arrDisposal, _arrDisposalCumul )
          else
            serDisposal.Clear()
          ;
        end
      else if( cbxVaccSetup = sender ) then
        begin
          if( cbxVaccSetup.Checked ) then
            redrawSeries( serVaccSetup, _arrVaccSetup, _arrVaccSetupCumul )
          else
            serVaccSetup.Clear()
          ;
        end
      else if( cbxVacc = sender ) then
        begin
          if( cbxVacc.Checked ) then
            redrawSeries( serVacc, _arrVacc, _arrVaccCumul )
          else
            serVacc.Clear()
          ;
        end
      else if( cbxVaccSubtotal = sender ) then
        begin
          if( cbxVaccSubtotal.Checked ) then
            redrawSeries( serVaccSubtotal, _arrVaccSubtotal, _arrVaccSubtotalCumul )
          else
            serVaccSubtotal.Clear()
          ;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------




end.
