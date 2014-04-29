unit FormIterationSummary;

(*
FormIterationSummary.pas/dfm
----------------------------
Begin: 2005/07/06
Last revision: $Date: 2008/10/21 18:51:49 $ $Author: areeves $
Version: $Revision: 1.28 $
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
		ComCtrls,
		ExtCtrls, 
		Grids,
		StdCtrls,
		Buttons,
		ClipBrd,
    ToolWin,
    ActnMan,
    ActnCtrls,
    ActnMenus,
		TeeProcs,
		TeEngine,
		Chart,
		Series,

    // Custom standard widgets
		PBPageControl,

    // General purpose units
    SqlClasses,

    // Application-specific data structures
    SMDatabase,
    SMSimulationInput,
    Herd,
    SMSimOutByProdType,
    ProductionType,

    // Application-specific widgets
    FormSMOutputBase, // the base class!
    FrameChartBase,
    FrameSingleEpiCurve,
    FrameEpiIterationSummary, 
    FrameCostIterationSummary
	;


  {*
    This form displays summary information for a single iteration.
  }
	type TFormIterationSummary = class( TFormSMOutputBase )
      pnlCaption: TPanel;

      pgcOutputs: TPBPageControl;
      tabEpiOutputs: TTabSheet;
      sbxEpiOutputs: TScrollBox;
      fraEpiIterationSummary: TFrameEpiIterationSummary;

      tabCostOutputs: TTabSheet;
      fraCostIterationSummary: TFrameCostIterationSummary;
      lblIteration: TLabel;
      cboIteration: TComboBox;
      ScrollBox1: TScrollBox;

      procedure fraEpiIterationSummarycbxSurvClick(Sender: TObject);
      procedure fraEpiIterationSummarycbxDestrClick(Sender: TObject);
      procedure fraEpiIterationSummarycbxVacClick(Sender: TObject);
      procedure fraEpiIterationSummarycbxInfClick(Sender: TObject);
      procedure fraEpiIterationSummarycbxApparentClick(Sender: TObject);
      procedure fraEpiIterationSummarycbxInapparentClick(Sender: TObject);

      procedure fraCostIterationSummarycbxShowTableClick(Sender: TObject);
      procedure fraCostIterationSummarycbxShowChartClick(Sender: TObject);

      procedure pgcOutputsChange(Sender: TObject);

      procedure FormResize(Sender: TObject);
      procedure cboIterationChange(Sender: TObject);

      //procedure FormCanResize( Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);

		protected
      _resizingForm: boolean;

      procedure translateUI();
      procedure translateUIManual();

      { Used to create a list of all charts on the form that might be saved/copied/printed }
      procedure fillChartDict(); override;

      { Used to create a list of all grids containing text that might be saved/copied/printed }
      procedure fillStringGridDict(); override;

      { Updates the contents of the form when the selected production type changes }
      procedure productionTypeChanged(); override;

      { Updates the contents of the form when the loaded database/simulation changes }
      procedure simChanged(); override;

      { Used to enable/disable controls on the form when needed }
      procedure setControlsEnabled( val: boolean ); override;

      { Writes the header that precedes any text exported from the grid(s) }
      function textHeader(): string; override;

      { Writes the footer that precedes any text exported from the grid(s) }
      function textFooter(): string; override;

      procedure initialize();

      procedure setCaption();
		public
      constructor create( AOwner: TComponent; sim: TSMSimulationInput; db: TSMDatabase ); reintroduce;
      destructor destroy(); override;

      procedure reset();

      { Handles chart updates while a simulation is in progress }
      procedure updateForDay( day: integer );
      procedure updateSimComplete();
      procedure resetIteration( iteration: Integer );
		end
	;


  const
    DBFORMITERATIONSUMMARY: boolean = false; // Set to true to enable debugging messages for this form

	var
		frmIterationSummary: TFormIterationSummary;

    
implementation

	{$R *.dfm}

	uses
    // General purpose units
    MyDialogs,
    MyStrUtils,
    GuiStrUtils,
    DebugWindow,
    ControlUtils,
    I88n,

    // Application-specific units
    StringConsts,
    FormMain
	;


//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
	constructor TFormIterationSummary.create( AOwner: TComponent; sim: TSMSimulationInput; db: TSMDatabase );
    var
      frm: TForm;
		begin
      inherited create( AOwner );
      translateUI();

      // Initialize pointers
      //--------------------
      _smSim := sim;
      _smdb := db;
      _selectedPT := nil;

      lblIteration.Left := cboProdTypes.Left + cboProdTypes.Width + 7;
      cboIteration.Left := lblIteration.Left + 56;
      cboIteration.Enabled := false;

      // Size the form
      //--------------
      _resizingForm := false;

      if( AOwner is TForm ) then
        begin
          frm := AOwner as TForm;
          self.Height := round( frm.ClientHeight * 0.75 );
        end
      else
        begin
          self.Height := 650;
        end
      ;

      // Show the data and the active tab
      //----------------------------------
      setupComboBox();

      // FIX ME: check for completed output before displaying anything.
      // (FormMain should also do a check.  This is just a backup.)
      pgcOutputs.ActivePage := tabEpiOutputs;

      initialize();

      if ( frmMain.simIsRunning ) then
        begin
          cboIteration.Enabled := false;
          cboIteration.Clear();
        end
      ;

      setCaption();

      self.Resize();
      fraEpiIterationSummary.Visible := true;
		end
	;


  procedure TFormIterationSummary.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.2.
      // Generation date: Wed Mar 12 16:14:33 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-HEAD/sm_forms/FormIterationSummary.dfm
      // File date: Wed Mar 12 15:34:32 2008

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Summary of 1 iteration' );
          lblIteration.Caption := tr( 'Iteration:' );
          tabEpiOutputs.Caption := tr( 'Epidemiology' );
          fraEpiIterationSummary.cbxSurv.Caption := tr( 'Detection' );
          tabCostOutputs.Caption := tr( 'Cost accounting' );
          fraCostIterationSummary.tabGraph.Caption := tr( 'Graphical view' );
          pnlCaption.Caption := tr( 'Iteration status: completed/aborted/running' );
        end
      ;

      // If any phrases are found that could not be automatically extracted by
      // Caption Collector, modify the following function to take care of them.
      translateUIManual();
    end
  ;


  procedure TFormIterationSummary.translateUIManual();
    begin
      tabEpiOutputs.Caption := '  ' + tr( 'Epidemiology' ) + '  ';
      tabCostOutputs.Caption := '  ' + tr( 'Cost accounting' ) + '  ';
    end
  ;


  procedure TFormIterationSummary.initialize();
    var
      db2: TSqlDatabase;
      res: TSqlResult;
      row: TSqlRow;
      q: string;

      displayedIteration: integer;
    begin
      db2 := _smdb as TSqlDatabase;

      // Fill Iteration ComboBox
      //----------------------------
      q := 'SELECT distinct(iteration) from outDailyByProductionType order by iteration desc';
      res := TSqlResult.create( q, db2 );
      row := res.fetchArrayFirst();

      cboIteration.Items.Clear();

      if ( row <> nil ) then
        begin
          cboIteration.Enabled := true;
          while ( row <> nil ) do
            begin
              cboIteration.Items.Add(row.field('iteration'));
              row := res.fetchArrayNext();
            end
          ;

          cboIteration.ItemIndex := 0;

          displayedIteration := frmMain.displayedIteration;

          if( -1 <> displayedIteration ) then
            begin
              if( 0 <= cboIteration.Items.IndexOf( IntToStr( displayedIteration ) ) ) then
                cboIteration.ItemIndex := cboIteration.Items.IndexOf( IntToStr( displayedIteration ) )
              ;
            end
          ;

          fraEpiIterationSummary.resetSim( _smdb, _smSim, _selectedPT, StrToInt(cboIteration.Items[cboIteration.ItemIndex]) );
          fraCostIterationSummary.resetSim( _smdb, _smSim, _selectedPT, StrToInt(cboIteration.Items[cboIteration.ItemIndex]) );
        end
      else
        begin
          cboIteration.Enabled := false;

          fraEpiIterationSummary.resetSim( _smdb, _smSim, _selectedPT );
          fraCostIterationSummary.resetSim( _smdb, _smSim, _selectedPT );
        end
      ;

      if( not( _smSim.includeCostsGlobal ) ) then
        begin
          pgcOutputs.ActivePage := tabEpiOutputs;
          tabCostOutputs.Enabled := false;
        end
      else
        begin
          dbcout( 'Costs should be enabled', DBFORMITERATIONSUMMARY );
          tabCostOutputs.Enabled := true;
        end
      ;

      pgcOutputs.forceRepaint();
      pgcOutputsChange( nil );

      setCaption();
    end
  ;


	destructor TFormIterationSummary.destroy();
		begin
      inherited destroy();
		end
	;


  procedure TFormIterationSummary.fillStringGridDict();
    begin
      _stringGridDict.Clear();

      if( tabEpiOutputs = pgcOutputs.ActivePage ) then
        begin
          _stringGridDict['Detection'] := fraEpiIterationSummary.fraSgSurv;
          _stringGridDict['Destruction'] := fraEpiIterationSummary.fraSgDestr;
          _stringGridDict['Vaccination'] := fraEpiIterationSummary.fraSgVac;
          _stringGridDict['Infection'] := fraEpiIterationSummary.fraSgInf;
          _showGridName := true;
        end
      else if( tabCostOutputs = pgcOutputs.ActivePage ) then
        begin
          _stringGridDict['Daily costs'] := fraCostIterationSummary.fraTable.fraGrid;
          _showGridName := false;
        end
      ;
    end
  ;


  procedure TFormIterationSummary.fillChartDict();
    begin
      _chartDict.Clear();

      if( tabEpiOutputs = pgcOutputs.ActivePage ) then
        begin
      _chartDict[fraEpiIterationSummary.fraApparent.Name] := fraEpiIterationSummary.fraApparent;
      _chartDict[fraEpiIterationSummary.fraInapparent.Name] := fraEpiIterationSummary.fraInapparent;
        end
      else if( tabCostOutputs = pgcOutputs.ActivePage ) then
        _chartDict['CostChart'] := fraCostIterationSummary.fraChart
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Form updating functions
//-----------------------------------------------------------------------------
  procedure TFormIterationSummary.updateSimComplete();
    begin
      self.initialize();
      self.resetIteration( StrToInt( cboIteration.Items[0] ) );
      frmMain.displayedIteration := StrToInt( cboIteration.Items[0] );
    end
  ;


  {*
    This function handles form updates while a simulation is in progress.
    When a simulation is not running (or when the selected production type is
    changed in midstream), setUpFromDatabase() is used.
  }
  procedure TFormIterationSummary.updateForDay( day: integer );
    begin
      fraEpiIterationSummary.updateForDay( day );
      if( true = _smSim.includeCostsGlobal ) then fraCostiterationSummary.updateForDay( day );
    end
  ;


  procedure TFormIterationSummary.productionTypeChanged();
    var
      activePage: integer;
    begin
      dbcout( '--- Production type changed in TFormIterationSummary!', DBFORMITERATIONSUMMARY );

      try
        screen.Cursor := crHourGlass;
        setControlsEnabled( false );

        activePage := pgcOutputs.ActivePageIndex;

        fraEpiIterationSummary.setProdType( _selectedPT );

        if( _smSim.includeCostsGlobal ) then
          begin
            fraCostIterationSummary.setProdType( _selectedPT );
            pgcOutputs.ActivePageIndex := activePage;
          end
        else
          pgcOutputs.ActivePage := tabEpiOutputs
        ;

      finally
        setControlsEnabled( true );
        screen.Cursor := crDefault;
      end;

      dbcout( 'Done with production type change.', DBFORMITERATIONSUMMARY );
    end
  ;


	procedure TFormIterationSummary.simChanged();
  	begin
      initialize();
    end
  ;


  procedure TFormIterationSummary.reset();
    begin
      fraEpiIterationSummary.reset();
      fraCostIterationSummary.reset();
    end
  ;


  procedure TFormIterationSummary.resetIteration( iteration: Integer );
    var
      currentTab:Integer;
    begin
      currentTab := pgcOutputs.ActivePageIndex;

      if ( cboIteration.Items.IndexOf( IntToStr( iteration ) ) >= 0 ) then
        begin
          pgcOutputs.Visible := false;
          cboIteration.ItemIndex := cboIteration.Items.IndexOf( IntToStr( iteration ) );

          fraEpiIterationSummary.resetSim( _smdb, _smSim, _selectedPT, iteration );
          fraCostIterationSummary.resetSim( _smdb, _smSim, _selectedPT, iteration );

          if( not( _smSim.includeCostsGlobal ) ) then
            begin
              pgcOutputs.ActivePage := tabEpiOutputs;
              tabCostOutputs.Enabled := false;
            end
          else
            begin
              dbcout( 'Costs should be enabled', DBFORMITERATIONSUMMARY );
              tabCostOutputs.Enabled := true;
            end
          ;

          pgcOutputs.forceRepaint();
          pgcOutputsChange( nil );
          pgcOutputs.ActivePageIndex := currentTab;
          pgcOutputs.Visible := true;

          if( iteration > _smdb.completedIterations ) then
            pnlCaption.Caption := tr( 'Iteration status: aborted' )
          else
            pnlCaption.Caption := tr( 'Iteration status: complete' )
          ;
        end
      ;
    end
  ;


  procedure TFormIterationSummary.cboIterationChange(Sender: TObject);
    begin
      inherited;

      if ( assigned( frmMain ) ) then
        frmMain.displayedIteration := StrToInt( cboIteration.Items[cboIteration.ItemIndex] )
      else
        begin
          fraEpiIterationSummary.resetSim( _smdb, _smSim, _selectedPT, StrToInt(cboIteration.Items[cboIteration.ItemIndex]) );
          fraCostIterationSummary.resetSim( _smdb, _smSim, _selectedPT, StrToInt(cboIteration.Items[cboIteration.ItemIndex]) );

          if( not( _smSim.includeCostsGlobal ) ) then
            begin
              pgcOutputs.ActivePage := tabEpiOutputs;
              tabCostOutputs.Enabled := false;
            end
          else
            begin
              dbcout( 'Costs should be enabled', DBFORMITERATIONSUMMARY );
              tabCostOutputs.Enabled := true;
            end
          ;
                    
          pgcOutputs.forceRepaint();

          pgcOutputsChange( nil );
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Text writing
//-----------------------------------------------------------------------------
  function TFormIterationSummary.textHeader(): string;
    var
      ptName: string;
    begin
      if( nil = _selectedPT ) then
        ptName := tr( 'All production types' )
      else
        ptName := _selectedPT.productionTypeDescr
      ;

      if( tabEpiOutputs = pgcOutputs.ActivePage ) then
        result := '## ' + tr( 'NAADSM single iteration summary (epidemiology)' ) + endl
      else if( tabCostOutputs = pgcOutputs.ActivePage ) then
        result := '## ' + tr( 'NAADSM single iteration summary (direct costs)' ) + endl
      else
        result := '## ' + tr( 'NAADSM single iteration summary' ) + endl
      ;

      result := result
        + '## ' + tr( 'Application version:' ) + ' ' + MAJORVERSIONNUMBER + '.' + MINORVERSIONNUMBER + '-' + BUILDNUMBER + endl
        + '## ' + tr( 'Date:' ) + ' ' + dateTimeToStr( now() ) + endl
        + '## ' + tr( 'Scenario file:' ) + ' ' + _smdb.permanentDBFileName + endl
        + '## ' + tr( 'Production type:' ) + ' ' + ptName + endl
      ;
    end
  ;


  function TFormIterationSummary.textFooter(): string;
    begin
      if( tabEpiOutputs = pgcOutputs.ActivePage ) then
        result := endl + '* In the course of a simulation run these activities may occur more than once on a single unit.'
      else
        result := ''
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI-handling member functions
//-----------------------------------------------------------------------------
  procedure TFormIterationSummary.FormResize(Sender: TObject);
    begin
      fraEpiIterationSummary.resizeContents();
    end
  ;


  procedure TFormIterationSummary.fraEpiIterationSummarycbxSurvClick( Sender: TObject );
    begin
      inherited;
      fraEpiIterationSummary.cbxSurvClick(Sender);
      setDataControlsEnabled( not( fraEpiIterationSummary.allTextBoxesUnchecked() ) );
    end
  ;


  procedure TFormIterationSummary.fraEpiIterationSummarycbxDestrClick( Sender: TObject );
    begin
      inherited;
      fraEpiIterationSummary.cbxDestrClick(Sender);
      setDataControlsEnabled( not( fraEpiIterationSummary.allTextBoxesUnchecked() ) );
    end
  ;


  procedure TFormIterationSummary.fraEpiIterationSummarycbxVacClick( Sender: TObject );
    begin
      inherited;
      fraEpiIterationSummary.cbxVacClick(Sender);
      setDataControlsEnabled( not( fraEpiIterationSummary.allTextBoxesUnchecked() ) );
    end
  ;


  procedure TFormIterationSummary.fraEpiIterationSummarycbxInfClick( Sender: TObject );
    begin
      inherited;
      fraEpiIterationSummary.cbxInfClick(Sender);
      setDataControlsEnabled( not( fraEpiIterationSummary.allTextBoxesUnchecked() ) );
    end
  ;


  procedure TFormIterationSummary.fraEpiIterationSummarycbxApparentClick( Sender: TObject );
    begin
      inherited;
      fraEpiIterationSummary.cbxApparentClick(Sender);
      setChartControlsEnabled( not( fraEpiIterationSummary.allChartBoxesUnchecked() ) );
    end
  ;


  procedure TFormIterationSummary.fraEpiIterationSummarycbxInapparentClick( Sender: TObject);
    begin
      inherited;
      fraEpiIterationSummary.cbxInapparentClick(Sender);
      setChartControlsEnabled( not( fraEpiIterationSummary.allChartBoxesUnchecked() ) );
    end
  ;


  procedure TFormIterationSummary.fraCostIterationSummarycbxShowTableClick( Sender: TObject );
    begin
      inherited;
      fraCostIterationSummary.cbxShowTableClick(Sender);
      setControlsEnabled( true );
    end
  ;


  procedure TFormIterationSummary.fraCostIterationSummarycbxShowChartClick( Sender: TObject );
    begin
      inherited;
      fraCostIterationSummary.cbxShowChartClick(Sender);
      setControlsEnabled( true );
    end
  ;


  procedure TFormIterationSummary.setControlsEnabled( val: boolean );
    begin
      cboProdTypes.Enabled := val;

      if( false = val ) then
        begin
          setChildrenEnabled( pnlButtons, val, false );
          mainMenuBar.Enabled := val;
        end
      else
        begin
          if( tabEpiOutputs = pgcOutputs.ActivePage ) then
            begin
              setDataControlsEnabled( not( fraEpiIterationSummary.allTextBoxesUnchecked ) );
              setChartControlsEnabled( not( fraEpiIterationSummary.allChartBoxesUnchecked ) );
            end
          else
            begin
              setDataControlsEnabled( fraCostIterationSummary.tableIsVisible );
              setChartControlsEnabled( fraCostIterationSummary.chartIsVisible );
            end
          ;

          mainMenuBar.Enabled := val;
        end
      ;
    end
  ;


  procedure TFormIterationSummary.pgcOutputsChange( Sender: TObject );
    begin
      inherited;

      dbcout( 'tabEpi client height: ' + intToStr( tabEpiOutputs.ClientHeight ), DBFORMITERATIONSUMMARY );
      dbcout( 'tabCosts client height: ' + intToStr( tabCostOutputs.ClientHeight ), DBFORMITERATIONSUMMARY );

      fillStringGridDict();
      fillChartDict();

      setControlsEnabled( true );
    end
  ;
//-----------------------------------------------------------------------------


  procedure TFormIterationSummary.setCaption();
    begin
      if( frmMain.simIsRunning ) then
        pnlCaption.Caption := tr( 'Iteration status: running' )
      else
        begin
          if( _smdb.containsIncompleteIterations() ) then
            pnlCaption.Caption := tr( 'Iteration status: aborted' )
          else
            pnlCaption.Caption := tr( 'Iteration status: complete' )
          ;
        end
      ;
    end
  ;

end.
