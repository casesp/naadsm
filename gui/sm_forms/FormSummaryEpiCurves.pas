unit FormSummaryEpiCurves;

(*
FormSummaryEpiCurves.pas/dfm
-----------------------------
Begin: 2005/10/14
Last revision: $Date: 2009-09-19 18:41:23 $ $Author: areeves $
Version: $Revision: 1.20 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2005 - 2009 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

{$include ../Defs.inc}

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
    Buttons,
    ComCtrls,
    Spin,
    ExtCtrls,
    ActnList,
    XPStyleActnCtrls,
    ActnMan,
    ImgList,
    ToolWin,
    ActnCtrls,
    ActnMenus,

    // General purpose units
    DebugWindow,
    
    // Application-specific data structures
    SMEpiCurves,
    SMSimulationInput,
    SMDatabase,
    ProductionType,
    Herd,

    // Application-specific widgets
		FormSMOutputBase, // the base class!
    FrameSummaryEpiCurves,
    FrameStringGridBase,
    FrameSummaryEpiCurveTable,
    FrameChartBase
  ;

  {*
    This form generates and displays "summary epidemic curves" based on the results of
    multiple iterations.

    Extreme caution should be used in interpreting these curves: I don't think that they have any
    inherent meaning, and may be very highly misleading.
  }
  type TFormSummaryEpiCurves = class( TFormSMOutputBase )
      pnlGlobalOptions: TPanel;
      pnlUnitChoices: TPanel;
      rdoHerds: TRadioButton;
      rdoAnimals: TRadioButton;
      pnlCurveChoices: TPanel;
      rdoActual: TRadioButton;
      rdoApparent: TRadioButton;
      lblDaysPerInterval: TLabel;
      speDaysPerInterval: TSpinEdit;
      pgcMain: TPageControl;
      tbsSummaryEpiCurveGraph: TTabSheet;
      tbsSummaryEpiCurveTable: TTabSheet;

      fraSummaryEpiCurveTable: TFrameSummaryEpiCurveTable;
      fraSummaryEpiCurves: TFrameSummaryEpiCurves;
      spacerPanel: TPanel;

      procedure updateCurves( Sender: TObject );
      procedure speDaysPerIntervalChange(Sender: TObject);
      procedure tbsSummaryEpiCurveTableResize(Sender: TObject);

    protected
      _epiCurves: TSMSummaryEpiCurves;
      _ScrollBarVisibleCheck: boolean;
      
      procedure translateUI();
      
      { Used to create a list of all charts on the form that might be saved/copied/printed }
      procedure fillChartDict(); override;

      { Used to create a list of all grids containing text that might be saved/copied/printed }
      procedure fillStringGridDict(); override;
      
      function textHeader(): string; override;

      { Updates the contents of the form when the selected production type changes }
      procedure productionTypeChanged(); override;

      { Updates the contents of the form when the loaded database/simulation changes }
      procedure simChanged(); override;

		public
			constructor create( AOwner: TComponent; sim: TSMSimulationInput; db: TSMDatabase  ); reintroduce;
      destructor destroy(); override;

    end
  ;


	var
		frmEpiCurve: TFormSummaryEpiCurves;


implementation

{$R *.dfm}

  uses
    // Standard Delphi units
    Clipbrd,
    StrUtils,

    // General purpose units
    MyStrUtils,
    MyDialogs,
    MyGraphicsUtils,
    I88n,

    // Application-specific units
    StringConsts,
    FormMain
  ;


//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TFormSummaryEpiCurves.create( AOwner: TComponent; sim: TSMSimulationInput; db: TSMDatabase  );
    begin
      inherited create( AOwner );
      translateUI();

      lblIteration.Visible := false;
      cboIteration.Visible := false;
      
      _ScrollBarVisibleCheck := false;
      
      _smSim := sim;
      _smdb := db;

      // Populate the dropdown list
      setupProdTypeComboBox();

      // Create the summary epi curves object
      _epiCurves := TSMSummaryEpiCurves.create( _smdb );

      // Set the max number of intervals to a reasonable value.
      if( speDaysPerInterval.Value > _epiCurves.curveForProductionType( tr( 'All production types' ) ).daysInCurve div 2 + 1 ) then
        speDaysPerInterval.Value := _epiCurves.curveForProductionType( tr( 'All production types' ) ).daysInCurve div 2 + 1
      ;

      speDaysPerInterval.MaxValue := _epiCurves.curveForProductionType( tr( 'All production types' ) ).daysInCurve div 2 + 1;

      // Show the curves!
      updateCurves( nil );
      tbsSummaryEpiCurveTableResize(nil);

      pgcMain.ActivePageIndex := 0;
    end
  ;


  procedure TFormSummaryEpiCurves.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:54 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormSummaryEpiCurves.dfm
      // File date: Wed May 2 09:53:36 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Summary epidemic curves' );
          lblDaysPerInterval.Caption := tr( 'Simulation days per interval:' );
          rdoHerds.Caption := capitalize( tr( 'units' ) );
          rdoAnimals.Caption := tr( 'Animals' );
          rdoActual.Caption := tr( 'Actual epidemic curve' );
          rdoApparent.Caption := tr( 'Apparent epidemic curve' );
          tbsSummaryEpiCurveGraph.Caption := tr( 'Graphical view' );
          tbsSummaryEpiCurveTable.Caption := tr( 'Tabular view' );
        end
      ;

    end
  ;


  destructor TFormSummaryEpiCurves.destroy();
    begin
      _epiCurves.Free();

      inherited destroy();
    end
  ;


  procedure TFormSummaryEpiCurves.fillChartDict();
    begin
      _chartDict['Epidemic curve'] := fraSummaryEpiCurves;
    end
  ;


	procedure TFormSummaryEpiCurves.fillStringGridDict();
		begin
			_stringGridDict['Summary epidemic curves'] := fraSummaryEpiCurveTable; 
		end
	;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Form updating functions
//-----------------------------------------------------------------------------
  procedure TFormSummaryEpiCurves.productionTypeChanged();
    begin
      try
        screen.Cursor := crHourGlass;
        setControlsEnabled( false );
        updateCurves( nil );
      finally
        setControlsEnabled( true );
        screen.Cursor := crDefault;
      end;
    end
  ;


	procedure TFormSummaryEpiCurves.simChanged();
  	begin
      freeAndNil( _epiCurves );
      _epiCurves := TSMSummaryEpiCurves.create( _smdb );

      updateCurves( nil );
    end
  ;


  procedure TFormSummaryEpiCurves.updateCurves( Sender: TObject );
    begin
      // Display the summary epi curves in graphical format
      fraSummaryEpiCurves.showCurves(
        _epiCurves,
        _selectedPT,
        rdoActual.Checked,
        rdoHerds.Checked,
        speDaysPerInterval.Value
      );

      // Display the summary epi curves in tabular format
      fraSummaryEpiCurveTable.showCurves(
        _epiCurves,
        _selectedPT,
        rdoActual.Checked,
        rdoHerds.Checked,
        speDaysPerInterval.Value
      );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Copying/saving/printing text
//-----------------------------------------------------------------------------
  function TFormSummaryEpiCurves.textHeader(): string;
    var
      ptName: string;
    begin
      if( nil = _selectedPT ) then
        ptName := tr( 'All production types' )
      else
        ptName := _selectedPT.productionTypeDescr
      ;

      result := '## ' + tr( 'NAADSM summary epidemic curves' ) + endl
        + '## ' + tr( 'Application version:' ) + ' ' + MAJORVERSIONNUMBER + '.' + MINORVERSIONNUMBER + '-' + BUILDNUMBER + endl
        + '## ' + tr( 'Date:' ) + ' ' + dateTimeToStr( now() ) + endl
        + '## ' + tr( 'Scenario file:' ) + ' ' + _smdb.permanentDBFileName + endl
        + '## ' + tr( 'Production type:' ) + ' ' + ptName + endl
        + '## ' + ansiReplaceStr( tr( 'Summary of xyz iterations' ), 'xyz', intToStr( _smdb.completedIterations ) ) + endl
        + endl
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI event handling
//-----------------------------------------------------------------------------
  procedure TFormSummaryEpiCurves.speDaysPerIntervalChange(Sender: TObject);
    begin
      try
        updateCurves( sender );
      except
        // Do nothing.
        // (This situation arises when the text box of the spin control is empty.)
      end;
    end
  ;
//-----------------------------------------------------------------------------


  procedure TFormSummaryEpiCurves.tbsSummaryEpiCurveTableResize(Sender: TObject);
    var
      factor: integer;
      visible: boolean;
    begin
      inherited;
      if(_ScrollBarVisibleCheck = false) then
        begin
          visible := false;
          _ScrollBarVisibleCheck := true;
        end
      else
        begin
          if (GetWindowlong(self.fraSummaryEpiCurveTable.stgGrid.Handle, GWL_STYLE) and WS_HSCROLL) <> 0 then
            visible := true
          else
            visible := false
          ;
        end
      ;
      factor := (self.tbsSummaryEpiCurveTable.ClientHeight) div
        (self.fraSummaryEpiCurveTable.stgGrid.DefaultRowHeight +
        self.fraSummaryEpiCurveTable.stgGrid.GridLineWidth );

      if(visible) then
      begin
         self.fraSummaryEpiCurveTable.Height := factor * (self.fraSummaryEpiCurveTable.stgGrid.DefaultRowHeight +
            self.fraSummaryEpiCurveTable.stgGrid.GridLineWidth ) ;
      end
      else
        begin
          self.fraSummaryEpiCurveTable.Height := factor * (self.fraSummaryEpiCurveTable.stgGrid.DefaultRowHeight +
            self.fraSummaryEpiCurveTable.stgGrid.GridLineWidth ) + 4;
        end
      ;
      self.fraSummaryEpiCurveTable.Align := alTop;
      spacerPanel.Align := alClient;
      self.fraSummaryEpiCurveTable.stgGrid.Align := alClient;
    end
  ;

end.
