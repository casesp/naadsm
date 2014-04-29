unit FormDailyStatusByProdType;

(*
FormDailyStatusByProdType.pas/dfm
---------------------------------
Begin: 2005/06/28
Last revision: $Date: 2008/11/25 22:00:29 $ $Author: areeves $
Version: $Revision: 1.34 $
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
  	// Standard Delphi units and controls
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
    Buttons,
    ClipBrd,
    ToolWin,
    ActnMan,
    ActnCtrls,
    ActnMenus,

    // Units used for graphs
    TeeProcs,
    TeEngine,
    Chart,
    Series,

    // Application-specific data structures
    SMDatabase,
    ProductionType,
    ProductionTypeList,
    SMSimulationInput,
    SMSimOutByProdType,
    Herd,

    // Application-specific GUI classes
    FormSMOutputBase, // the base class!
    FrameChartBase,
    FrameDailyStatusByProdType
  ;


  {*
    This form displays a graph showing the number of herds in each disease state
    on each day of a single iteration.
  }
  type  TFormDailyStatusByProdType = class( TFormSMOutputBase )
      pnlCaption: TPanel;
      fraBody: TFrameDailyStatusByProdType;
      cboIteration: TComboBox;
      lblIteration: TLabel;
      pnlCheckBoxes: TPanel;
      cbxSusceptible: TCheckBox;
      lineSusceptible: TPanel;
      cbxNatImmune: TCheckBox;
      cbxLatent: TCheckBox;
      cbxVacImmune: TCheckBox;
      cbxSubClinical: TCheckBox;
      cbxDestroyed: TCheckBox;
      cbxClinical: TCheckBox;
      cbxMilestones: TCheckBox;
      lineNatImmune: TPanel;
      lineLatent: TPanel;
      lineVacImmune: TPanel;
      lineSubclinical: TPanel;
      lineDestroyed: TPanel;
      lineClinical: TPanel;
      cbxDetected: TCheckBox;
      lineDetected: TPanel;
      cbxTraceDir: TCheckBox;
      lineTraceDir: TPanel;
      cbxTraceInd: TCheckBox;
      lineTraceInd: TPanel;
      cbxVaccinated: TCheckBox;
      lineVaccinated: TPanel;

      { Toggles chart display between 2D and 3D }
      procedure cbxThreeDClick(Sender: TObject);

      procedure cboIterationChange(Sender: TObject);

      procedure cbxMilestonesClick(Sender: TObject);
      procedure cbxSusceptibleClick(Sender: TObject);
      procedure cbxNatImmuneClick(Sender: TObject);
      procedure cbxLatentClick(Sender: TObject);
      procedure cbxVacImmuneClick(Sender: TObject);
      procedure cbxSubClinicalClick(Sender: TObject);
      procedure cbxDestroyedClick(Sender: TObject);
      procedure cbxClinicalClick(Sender: TObject);
      procedure cbxDetectedClick(Sender: TObject);
      procedure cbxTraceDirClick(Sender: TObject);
      procedure cbxTraceIndClick(Sender: TObject);
      procedure cbxVaccinatedClick(Sender: TObject);

    protected
      _data: TSMSimOutByProdType; // The information to display

      _itDetOccurred: integer; // Day when first detection occurred
      _itDestrOccurred: integer; // Day when first destruction occurred
      _itVaccOccurred: integer; // Day when first vaccination occurred
      _outbreakEnd: integer; // Day when the outbreak ended

      _chartFixNeeded: boolean; // Used with the TChart bug work around.  See chart drawing functions.

      _currentItr: Integer;

      _nextEventY: double;

      procedure translateUI();
      procedure translateUIManual();

      { Used to create a list of all charts on the form that might be saved/copied/printed }
      procedure fillChartDict(); override;

      { Updates the contents of the form when the selected production type changes }
      procedure productionTypeChanged(); override;

      { Updates the contents of the form when the loaded database/simulation changes }
      procedure simChanged(); override;

      { Handles form updates from the database when a simulation is not running or when _selectedPT is changed }
      procedure setupFromDatabase( Itr: Integer );

      { Chart drawing functions }
      procedure drawAllSeries( day: integer );
      procedure fixTChartBug();
      function minYEqualsMaxY(): boolean;
      function minXEqualsMaxX(): boolean;
      procedure setVertAxisMinMax( min, max: double );
      procedure setVertAxisAutomatic();
      procedure setHorizAxisMinMax( min, max: double );
      procedure setHorizAxisAutomatic();
      function eventY(): double;

      { Calculates the total number of units in all disease states }
      function totalUnits(): longint;

      { Calculates the total number of units infected by any mechanism }
  		function totalInfected(): longint;

      procedure updateChartFromDatabase();

      procedure cbxChanged(Sender: TObject);    

      procedure setCaption();

    public
    	constructor create( AOwner: TComponent; sim: TSMSimulationInput; db: TSMDatabase ); reintroduce;
      destructor destroy(); override;

      procedure reset();

      { Handles chart updates while a simulation is in progress }
      procedure updateForDay( day: integer );
      procedure updateSimComplete();

      procedure firstDetection( const day: integer; const ptID: integer );
      procedure firstDestruction( const day: integer; const ptID: integer );
      procedure firstVaccination( const day: integer; const ptID: integer );
      procedure outbreakEnd( day: integer );
      procedure resetIteration( const iteration: Integer );
    end
  ;


  var
		frmDailyStatusByProdType: TFormDailyStatusByProdType;


  const
  	DBFORMDAILYSTATUSBYPRODTYPE: boolean = false; // set to true to enable debugging messages for this unit.

    
implementation

	{$R *.dfm}

  uses
    // Built-in Delphi units
    Math,

    // General-purpose units
    MyStrUtils,
    GuiStrUtils,
    DebugWindow,
    MyDialogs,
    SqlClasses,
    I88n,

    // application-specific classes
    FormMain
  ;

  (*
  const
    OUTBREAKEND_ROW: integer = 0;
    DET_ROW: integer = 2;
    DESTR_ROW: integer = 4;
    VACC_ROW: integer = 6;
  *)

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
	constructor TFormDailyStatusByProdType.create( AOwner: TComponent; sim: TSMSimulationInput; db: TSMDatabase );
  	begin
      inherited create( AOwner );
      translateUI();

      pnlCaption.Caption := '';

      setDataControlsEnabled( false );

      // Initialize pointers
      //--------------------
      _smSim := sim;
      _smdb := db;
      _data := TSMSimOutByProdType.create();
      _selectedPT := nil;

      _currentItr := frmMain.displayedIteration;

      _nextEventY := 0.0;

      // Size the form
      //--------------
      ClientWidth := 556 + lblIteration.Width + cboIteration.Width;
      cboIteration.Items.Clear();
      cboIteration.enabled := false;
      fraBody.chtOutputs.View3D := False;

      _chartFixNeeded := true;

      // Show the data
      //--------------
      cbxSusceptible.Checked := true;
      cbxLatent.Checked := true;
      cbxSubClinical.Checked := true;
      cbxClinical.Checked := true;
      cbxNatImmune.Checked := true;
      cbxVacImmune.Checked := true;
      cbxDestroyed.Checked := true;

      cbxDetected.Checked := false;
      cbxTraceDir.Checked := false;
      cbxTraceInd.Checked := false;
      cbxVaccinated.Checked := false;

      cbxMilestones.Checked := true;

      setupComboBox();
      setUpFromDatabase( -1 );
      setCaption();

      if ( frmMain.simIsRunning ) then
        begin
          cboIteration.enabled := false;
          cboIteration.clear();
        end;
    end
  ;


  procedure TFormDailyStatusByProdType.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.2.
      // Generation date: Thu Feb 28 17:04:49 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormDailyStatusByProdType.dfm
      // File date: Tue Feb 26 22:50:22 2008

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Daily unit status for 1 iteration' );
          pnlCaption.Caption := tr( 'Iteration status: completed/aborted/running' );
          lblIteration.Caption := tr( 'Iteration:' );
          cbxSusceptible.Caption := tr( 'Susceptible' );
          cbxNatImmune.Caption := tr( 'Nat Immune' );
          cbxLatent.Caption := tr( 'Latent' );
          cbxVacImmune.Caption := tr( 'Vac Immune' );
          cbxSubClinical.Caption := tr( 'Subclinical' );
          cbxDestroyed.Caption := tr( 'Destroyed' );
          cbxClinical.Caption := tr( 'Clinical' );

          cbxDetected.Caption := tr( 'Detected' );
          cbxTraceDir.Caption := tr( 'Traced - Direct' );
          cbxTraceDir.Caption := tr( 'Traced - Indirect' );
          cbxVaccinated.Caption := tr( 'Vaccinated' );
          
          cbxMilestones.Caption := capitalize( tr( 'events' ) );
        end
      ;

      // Set TChart properties
      with self do
        begin
        end
      ;

      // If any phrases are found that could not be automatically extracted by
      // Caption Collector, modify the following function to take care of them.
      // Otherwise, this function will be empty:
      translateUIManual();
    end
  ;


  procedure TFormDailyStatusByProdType.translateUIManual();
    begin
    end
  ;


	destructor TFormDailyStatusByProdType.destroy();
  	begin
      _data.Free();
    	inherited destroy();
    end
  ;


  procedure TFormDailyStatusByProdType.fillChartDict();
    begin
      _chartDict[fraBody.Name] := fraBody;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Major chart functions
//-----------------------------------------------------------------------------
  procedure TFormDailyStatusByProdType.productionTypeChanged();
    begin
      setUpFromDatabase( _currentItr );
    end
  ;

  
	procedure TFormDailyStatusByProdType.simChanged();
  	begin
      setUpFromDatabase( _currentItr );

      if ( frmMain.simIsRunning ) then
        begin
          cboIteration.enabled := false;
          cboIteration.clear();
        end
      ;

      setCaption();
    end
  ;


  procedure TFormDailyStatusByProdType.reset();
    begin
      fraBody.SeriesSusc.Clear();
      fraBody.SeriesLatent.Clear;
      fraBody.SeriesSubClinical.Clear();
      fraBody.SeriesClinical.Clear();
      fraBody.SeriesNatImmune.Clear();
      fraBody.SeriesVacImmune.Clear();
      fraBody.SeriesDestroyed.Clear();

      fraBody.SeriesDetected.Clear();
      fraBody.SeriesTraceDir.Clear();
      fraBody.SeriesTraceInd.Clear();
      fraBody.SeriesVaccinated.Clear();

      fraBody.SeriesPDetection.Clear();
      fraBody.SeriesPDestroy.Clear();
      fraBody.SeriesPVaccination.Clear();      
      fraBody.SeriesPOver.Clear();

      _itDetOccurred := -1;
      _itDestrOccurred := -1;
      _itVaccOccurred := -1;
      _outbreakEnd := -1;
      _nextEventY := 0.0;

      if( assigned( _smSim ) ) then
        fraBody.chtOutputs.LeftAxis.Title.Caption := tr( 'Proportion of' ) +  ' ' + intToStr( totalUnits() ) + ' ' + tr( 'units X 100%' )
      else
      	fraBody.chtOutputs.LeftAxis.Title.Caption := tr( '(Simulation is not loaded)' )
      ;
    end
  ;


  {*
    This function handles chart updates from the database: either when
    a simulation is not running, or when the selected production type is
    changed in midstream.  Function updateForDay() is used for dynamic
    updates.
  }
  procedure TFormDailyStatusByProdType.setupFromDatabase( Itr: Integer );
    var
      db2: TSqlDatabase;
      res: TSqlResult;
      row: TSqlRow;
      q: string;

      displayedIteration: integer;
    begin
      try
        screen.Cursor := crHourGlass;
        setChartControlsEnabled( false );

        reset();

        dbcout( 'Starting update chart from database', DBFORMDAILYSTATUSBYPRODTYPE );
        db2 := _smdb as TSqlDatabase;

        // Fill Iteration ComboBox
        //----------------------------
        cboIteration.Items.Clear();

        q := 'SELECT distinct(iteration) from outDailyByProductionType order by iteration desc';
        res := TSqlResult.create( q, db2 );

        if ( Itr = -1 ) then
          begin

            row := res.fetchArrayFirst();

            cboIteration.Items.Clear();

            if ( row <> nil ) then
              cboIteration.Enabled := true
            ;

            while ( row <> nil ) do
              begin
                 cboIteration.Items.Add(row.field('iteration'));
                 row := res.fetchArrayNext();
              end;

            cboIteration.ItemIndex := 0;

            if ( _currentItr = -1 ) then
              begin
                if ( cboIteration.Items.Count > 0 ) then
                  begin
                    if ( cboIteration.ItemIndex = -1 ) then
                      begin
                        displayedIteration := -1;
                        _currentItr := -1;
                      end
                    else
                      begin
                        displayedIteration := myStrToInt( cboIteration.Items[cboIteration.ItemIndex], -1 );
                        _currentItr := displayedIteration;
                      end
                    ;
                  end
                else
                  displayedIteration := -1;
              end
            else
              begin
                if ( cboIteration.Items.IndexOf( IntToStr( _currentItr ) ) >= 0 ) then
                  begin
                    cboIteration.ItemIndex := cboIteration.Items.IndexOf( IntToStr( _currentItr ) );
                    displayedIteration := _currentItr;
                  end
                else
                  begin
                    displayedIteration := myStrToInt( cboIteration.Items[cboIteration.ItemIndex], -1 );
                    _currentItr := displayedIteration;
                  end
                ;
              end
            ;
          end
        else
          begin
            displayedIteration := Itr;
            _currentItr := displayedIteration;
          end
        ;

        if ( -1 <> displayedIteration ) then
          updateChartFromDatabase()
        ;

        // clean up
        //----------
        dbcout( 'Done drawing from database', DBFORMDAILYSTATUSBYPRODTYPE );
        res.Free();

      finally
        setChartControlsEnabled( true );
        screen.Cursor := crDefault;
      end;
    end
  ;


  procedure TFormDailyStatusByProdType.updateSimComplete();
    begin
      self.setupFromDatabase( -1 );
      self.resetIteration( StrToInt( cboIteration.Items[0] ) );
      frmMain.displayedIteration := StrToInt( cboIteration.Items[0] );
    end
  ;


  {*
    This function handles chart updates while a simulation is in progress.
    When a simulation is not running (or when the selected production type is
    changed in midstream), setUpFromDatabase() is used.
  }
	procedure TFormDailyStatusByProdType.updateForDay( day: integer );
  	var
    	ptit: TProductionTypeListIterator;
      selectedPTName: string;
    begin
      cboIteration.Enabled := false;
      
    	dbcout( endl + 'Updating chart for day' + endl, DBFORMDAILYSTATUSBYPRODTYPE );

      _data.clear();

      ptit := TProductionTypeListIterator.create( _smSim.ptList );

      if( nil <> _selectedPT ) then
      	selectedPTName := _selectedPT.productionTypeDescr
      else
      	selectedPTName := ''
      ;

      while( nil <> ptit.current() ) do
      	begin
        	if
          	( nil = _selectedPT ) // All production types should be included
          or
         		( ptit.current().productionTypeDescr = selectedPTName ) // This production type is selected
					then
            _data.addDailyRecordsFrom( ptit.current().currentOutputs )
          ;

        	ptit.incr();
        end
      ;
      ptit.Free();

      drawAllSeries( day );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Calculations for chart values
//-----------------------------------------------------------------------------
  function TFormDailyStatusByProdType.totalUnits(): longint;
  	var
    	ptit: TProductionTypeListIterator;
      selectedPTName: string;
  	begin
      ptit := TProductionTypeListIterator.create( _smSim.ptList );

      if( nil <> _selectedPT ) then
      	selectedPTName := _selectedPT.productionTypeDescr
      else
      	selectedPTName := ''
      ;

      result := 0;

      while( nil <> ptit.current() ) do
      	begin
        	if
          	( nil = _selectedPT ) // All production types should be included
          or
         		( ptit.current().productionTypeDescr = selectedPTName ) // This production type is selected
					then
            result := result + ptit.current().unitCount
          ;
          
        	ptit.incr();
        end
      ;
     	ptit.Free();
    end
  ;


  // FIX ME: This function is unused???  What is it for?
  function TFormDailyStatusByProdType.totalInfected(): longint;
  	var
    	ptit: TProductionTypeListIterator;
      selectedPTName: string;
  	begin
     	result := 0;
      ptit := TProductionTypeListIterator.create( _smSim.ptList );

      if( nil <> _selectedPT ) then
      	selectedPTName := _selectedPT.productionTypeDescr
      else
      	selectedPTName := ''
      ;

      while( nil <> ptit.current() ) do
      	begin
        	if
          	( nil = _selectedPT ) // All production types should be included
          or
         		( ptit.current().productionTypeDescr = selectedPTName ) // This production type is selected
					then
          	begin
              result := result
                + ptit.current().currentOutputs.tsdULat
                + ptit.current().currentOutputs.tsdUSubc
                + ptit.current().currentOutputs.tsdUClin
              ;
            end
          ;

        	ptit.incr();
        end
      ;
     	ptit.Free();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Chart drawing functions
//-----------------------------------------------------------------------------
  procedure TFormDailyStatusByProdType.drawAllSeries( day: integer );
  	var
    	multiplier: double;
      total: longint;
  	begin
    	total := totalUnits();

      if( 0 = total ) then
      	begin
       		dbcout( endl + 'drawAllSeries is exiting early.' + endl, DBFORMDAILYSTATUSBYPRODTYPE );
      		exit;
        end
      ;

    	multiplier := 100 / total;

      dbcout( 'Day ' + inttoStr( day )
          + ', # susceptible '
          + intToStr( _data.tsdUSusc )
          + ', total ' + inttoStr( total ),
        DBFORMDAILYSTATUSBYPRODTYPE
      );

      //Graphs...
      fraBody.SeriesSusc.Active := cbxSusceptible.Checked;
      fraBody.SeriesLatent.Active := cbxLatent.Checked;
      fraBody.SeriesSubClinical.Active := cbxSubClinical.Checked;
      fraBody.SeriesClinical.Active := cbxClinical.Checked;
      fraBody.SeriesNatImmune.Active := cbxNatImmune.Checked;
      fraBody.SeriesVacImmune.Active := cbxVacImmune.Checked;
      fraBody.SeriesDestroyed.Active := cbxDestroyed.Checked;

      fraBody.SeriesDetected.Active := cbxDetected.Checked;
      fraBody.SeriesTraceDir.Active := cbxTraceDir.Checked;
      fraBody.SeriesTraceInd.Active := cbxTraceInd.Checked;
      fraBody.SeriesVaccinated.Active := cbxVaccinated.Checked;

      //Milestone markers...
      fraBody.SeriesPDetection.Active := cbxMilestones.Checked;
      fraBody.SeriesPDestroy.Active := cbxMilestones.Checked;
      fraBody.SeriesPVaccination.Active := cbxMilestones.Checked;
      fraBody.SeriesPOver.Active := cbxMilestones.Checked;

      fraBody.SeriesSusc.AddXY(          day, _data.tsdUSusc * multiplier );
      fraBody.SeriesLatent.AddXY(        day, _data.tsdULat  * multiplier );
      fraBody.SeriesSubClinical.AddXY(   day, _data.tsdUSubc * multiplier );
      fraBody.SeriesClinical.AddXY(      day, _data.tsdUClin * multiplier );
      fraBody.SeriesNatImmune.AddXY(     day, _data.tsdUNImm * multiplier );
      fraBody.SeriesVacImmune.AddXY(     day, _data.tsdUVImm * multiplier );
      fraBody.SeriesDestroyed.AddXY(     day, _data.tsdUDest * multiplier );

      fraBody.SeriesDetected.AddXY(      day, _data.detnUClin * multiplier );
      fraBody.SeriesTraceDir.AddXY(      day, _data.trnUDir   * multiplier );
      fraBody.SeriesTraceInd.AddXY(      day, _data.trnUInd   * multiplier );
      fraBody.SeriesVaccinated.AddXY(    day, _data.vaccnUAll * multiplier );

      fixTChartBug();
    end
  ;


  procedure TFormDailyStatusByProdType.firstDetection( const day: integer; const ptID: integer );
  	begin
      dbcout2( 'Drawing first detection...' );

      // If all production types are shown, another pt may have been detected earlier.
      // If so, don't show another dot.
      if( -1 <> _itDetOccurred ) then
        exit
      ;

      if( ( -1 <> ptID ) and ( nil <> _selectedPT )  and ( -1 < day ) ) then
        begin
          if( ptID = _selectedPT.productionTypeID ) then
            begin
              fraBody.seriesPDetection.addXY( day, eventY() );
              dbcout( 'First detection on day ' + intToStr( day ), DBFORMDAILYSTATUSBYPRODTYPE );
            end
          ;
        end
      else if( -1 < day ) then
        begin
          fraBody.SeriesPDetection.AddXY( day, eventY() );
          _itDetOccurred := day;
          dbcout( 'First detection on day ' + intToStr( day ), DBFORMDAILYSTATUSBYPRODTYPE );
        end
      ;

      fixTChartBug();
    end
  ;


  procedure TFormDailyStatusByProdType.firstDestruction( const day: integer; const ptID: integer );
  	begin
      dbcout2( 'Drawing first destruction...' );

      // If all production types are shown, another pt may have been destroyed earlier.
      // If so, don't show another dot.
      if( -1 <> _itDestrOccurred ) then
        exit
      ;

      if( ( -1 <> ptID ) and ( nil <> _selectedPT ) and ( -1 < day ) ) then
        begin
          if( ptID = _selectedPT.productionTypeID ) then fraBody.seriesPDestroy.AddXY( day, eventY() );
        end
      else if( -1 < day ) then
        begin
          fraBody.SeriesPDestroy.addXY( day, eventY() );
          _itDestrOccurred := day;
        end
      ;

      fixTChartBug();
    end
  ;


  procedure TFormDailyStatusByProdType.firstVaccination( const day: integer; const ptID: integer );
  	begin
      dbcout2( 'Drawing first vaccination...' );

      // If all production types are shown, another pt may have been vaccinated earlier.
      // If so, don't show another dot.
      if( -1 <> _itVaccOccurred ) then
        exit
      ;

      if( ( -1 <> ptID ) and ( nil <> _selectedPT ) and ( -1 < day ) ) then
        begin
          if( ptID = _selectedPT.productionTypeID ) then fraBody.seriesPVaccination.AddXY( day, eventY() );
        end
      else if( -1 < day ) then
        begin
          fraBody.SeriesPVaccination.AddXY( day, eventY() );
          _itVaccOccurred := day;
        end
      ;

      fixTChartBug();
    end
  ;


  procedure TFormDailyStatusByProdType.outbreakEnd( day: integer );
  	begin
    	fraBody.SeriesPOver.AddXY( day, eventY() );
      _outbreakEnd := day;

      fixTChartBug();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI events and housekeeping
//-----------------------------------------------------------------------------
  procedure TFormDailyStatusByProdType.cbxThreeDClick( Sender: TObject );
    begin
      fraBody.chtOutputs.View3D := not fraBody.chtOutputs.View3D;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Functions for dealing with TChart
//-----------------------------------------------------------------------------
  procedure TFormDailyStatusByProdType.fixTChartBug();
    var
      yNeededFix, xNeededFix: boolean;
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
          if( minYEqualsMaxY() ) then
            begin
              setVertAxisMinMax( fraBody.seriesSusc.YValues.MinValue - 1.0, fraBody.seriesSusc.YValues.MinValue + 1.0 );
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
              setHorizAxisMinMax( fraBody.seriesSusc.XValues.MinValue - 1.0, fraBody.seriesSusc.XValues.MinValue + 1.0 );
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


  function TFormDailyStatusByProdType.minYEqualsMaxY(): boolean;
    var
      val: double;
    begin
      result := false;

      if( fraBody.SeriesSusc.YValues.MaxValue <> fraBody.SeriesSusc.YValues.MinValue ) then
        exit
      else
        val := fraBody.SeriesSusc.YValues.MaxValue
      ;

      if( not( ( fraBody.SeriesLatent.YValues.MaxValue = val ) and ( fraBody.SeriesLatent.YValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( fraBody.SeriesSubClinical.YValues.MaxValue = val ) and ( fraBody.SeriesSubClinical.YValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( fraBody.SeriesClinical.YValues.MaxValue = val ) and ( fraBody.SeriesClinical.YValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( fraBody.SeriesNatImmune.YValues.MaxValue = val ) and ( fraBody.SeriesNatImmune.YValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( fraBody.SeriesVacImmune.YValues.MaxValue = val ) and ( fraBody.SeriesVacImmune.YValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( fraBody.SeriesDestroyed.YValues.MaxValue = val ) and ( fraBody.SeriesDestroyed.YValues.MaxValue = val ) ) ) then
        exit

      else if( not( ( fraBody.SeriesDetected.YValues.MaxValue = val ) and ( fraBody.SeriesDetected.YValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( fraBody.SeriesTraceDir.YValues.MaxValue = val ) and ( fraBody.SeriesTraceDir.YValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( fraBody.SeriesTraceInd.YValues.MaxValue = val ) and ( fraBody.SeriesTraceInd.YValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( fraBody.SeriesVaccinated.YValues.MaxValue = val ) and ( fraBody.SeriesVaccinated.YValues.MaxValue = val ) ) ) then
        exit
        
      else if( not( ( fraBody.SeriesPDetection.YValues.MaxValue = val ) and ( fraBody.SeriesPDetection.YValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( fraBody.SeriesPDestroy.YValues.MaxValue = val ) and ( fraBody.SeriesPDestroy.YValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( fraBody.SeriesPVaccination.YValues.MaxValue = val ) and ( fraBody.SeriesPVaccination.YValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( fraBody.SeriesPOver.YValues.MaxValue = val ) and ( fraBody.SeriesPOver.YValues.MaxValue = val ) ) ) then
        exit
      else
        result := true
      ;

    end
  ;


  function TFormDailyStatusByProdType.minXEqualsMaxX(): boolean;
    var
      val: double;
    begin
      result := false;

      if( fraBody.SeriesSusc.XValues.MaxValue <> fraBody.SeriesSusc.XValues.MinValue ) then
        exit
      else
        val := fraBody.SeriesSusc.XValues.MaxValue
      ;

      if( not( ( fraBody.SeriesLatent.XValues.MaxValue = val ) and ( fraBody.SeriesLatent.XValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( fraBody.SeriesSubClinical.XValues.MaxValue = val ) and ( fraBody.SeriesSubClinical.XValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( fraBody.SeriesClinical.XValues.MaxValue = val ) and ( fraBody.SeriesClinical.XValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( fraBody.SeriesNatImmune.XValues.MaxValue = val ) and ( fraBody.SeriesNatImmune.XValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( fraBody.SeriesVacImmune.XValues.MaxValue = val ) and ( fraBody.SeriesVacImmune.XValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( fraBody.SeriesDestroyed.XValues.MaxValue = val ) and ( fraBody.SeriesDestroyed.XValues.MaxValue = val ) ) ) then
        exit

      else if( not( ( fraBody.SeriesDetected.XValues.MaxValue = val ) and ( fraBody.SeriesDetected.XValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( fraBody.SeriesTraceDir.XValues.MaxValue = val ) and ( fraBody.SeriesTraceDir.XValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( fraBody.SeriesTraceInd.XValues.MaxValue = val ) and ( fraBody.SeriesTraceInd.XValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( fraBody.SeriesVaccinated.XValues.MaxValue = val ) and ( fraBody.SeriesVaccinated.XValues.MaxValue = val ) ) ) then
        exit

      else if( not( ( fraBody.SeriesPDetection.XValues.MaxValue = val ) and ( fraBody.SeriesPDetection.XValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( fraBody.SeriesPDestroy.XValues.MaxValue = val ) and ( fraBody.SeriesPDestroy.XValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( fraBody.SeriesPVaccination.XValues.MaxValue = val ) and ( fraBody.SeriesPVaccination.XValues.MaxValue = val ) ) ) then
        exit
      else if( not( ( fraBody.SeriesPOver.XValues.MaxValue = val ) and ( fraBody.SeriesPOver.XValues.MaxValue = val ) ) ) then
        exit
      else
        result := true
      ;
    end
  ;


  procedure TFormDailyStatusByProdType.setVertAxisMinMax( min, max: double );
    begin
      fraBody.SeriesSusc.GetVertAxis.SetMinMax( min, max );
      fraBody.SeriesLatent.GetVertAxis.SetMinMax( min, max );
      fraBody.SeriesSubClinical.GetVertAxis.SetMinMax( min, max );
      fraBody.SeriesClinical.GetVertAxis.SetMinMax( min, max );
      fraBody.SeriesNatImmune.GetVertAxis.SetMinMax( min, max );
      fraBody.SeriesVacImmune.GetVertAxis.SetMinMax( min, max );
      fraBody.SeriesDestroyed.GetVertAxis.SetMinMax( min, max );

      fraBody.SeriesDetected.GetVertAxis.SetMinMax( min, max );
      fraBody.SeriesTraceDir.GetVertAxis.SetMinMax( min, max );
      fraBody.SeriesTraceInd.GetVertAxis.SetMinMax( min, max );
      fraBody.SeriesVaccinated.GetVertAxis.SetMinMax( min, max );

      fraBody.SeriesPDetection.GetVertAxis.SetMinMax( min, max );
      fraBody.SeriesPDestroy.GetVertAxis.SetMinMax( min, max );
      fraBody.SeriesPVaccination.GetVertAxis.SetMinMax( min, max );
      fraBody.SeriesPOver.GetVertAxis.SetMinMax( min, max );
    end
  ;


  procedure TFormDailyStatusByProdType.setVertAxisAutomatic();
    begin
      fraBody.SeriesSusc.GetVertAxis.Automatic := true;
      fraBody.SeriesLatent.GetVertAxis.Automatic := true;
      fraBody.SeriesSubClinical.GetVertAxis.Automatic := true;
      fraBody.SeriesClinical.GetVertAxis.Automatic := true;
      fraBody.SeriesNatImmune.GetVertAxis.Automatic := true;
      fraBody.SeriesVacImmune.GetVertAxis.Automatic := true;
      fraBody.SeriesDestroyed.GetVertAxis.Automatic := true;

      fraBody.SeriesDetected.GetVertAxis.Automatic := true;
      fraBody.SeriesTraceDir.GetVertAxis.Automatic := true;
      fraBody.SeriesTraceInd.GetVertAxis.Automatic := true;
      fraBody.SeriesVaccinated.GetVertAxis.Automatic := true;

      fraBody.SeriesPDetection.GetVertAxis.Automatic := true;
      fraBody.SeriesPDestroy.GetVertAxis.Automatic := true;
      fraBody.SeriesPVaccination.GetVertAxis.Automatic := true;
      fraBody.SeriesPOver.GetVertAxis.Automatic := true;
    end
  ;

  procedure TFormDailyStatusByProdType.setHorizAxisMinMax( min, max: double );
    begin
      fraBody.SeriesSusc.GetHorizAxis.SetMinMax( min, max );
      fraBody.SeriesLatent.GetHorizAxis.SetMinMax( min, max );
      fraBody.SeriesSubClinical.GetHorizAxis.SetMinMax( min, max );
      fraBody.SeriesClinical.GetHorizAxis.SetMinMax( min, max );
      fraBody.SeriesNatImmune.GetHorizAxis.SetMinMax( min, max );
      fraBody.SeriesVacImmune.GetHorizAxis.SetMinMax( min, max );
      fraBody.SeriesDestroyed.GetHorizAxis.SetMinMax( min, max );

      fraBody.SeriesDetected.GetHorizAxis.SetMinMax( min, max );
      fraBody.SeriesTraceDir.GetHorizAxis.SetMinMax( min, max );
      fraBody.SeriesTraceInd.GetHorizAxis.SetMinMax( min, max );
      fraBody.SeriesVaccinated.GetHorizAxis.SetMinMax( min, max );

      fraBody.SeriesPDetection.GetHorizAxis.SetMinMax( min, max );
      fraBody.SeriesPDestroy.GetHorizAxis.SetMinMax( min, max );
      fraBody.SeriesPVaccination.GetHorizAxis.SetMinMax( min, max );
      fraBody.SeriesPOver.GetHorizAxis.SetMinMax( min, max );
    end
  ;


  procedure TFormDailyStatusByProdType.setHorizAxisAutomatic();
    begin
      fraBody.SeriesSusc.GetHorizAxis.Automatic := true;
      fraBody.SeriesLatent.GetHorizAxis.Automatic := true;
      fraBody.SeriesSubClinical.GetHorizAxis.Automatic := true;
      fraBody.SeriesClinical.GetHorizAxis.Automatic := true;
      fraBody.SeriesNatImmune.GetHorizAxis.Automatic := true;
      fraBody.SeriesVacImmune.GetHorizAxis.Automatic := true;
      fraBody.SeriesDestroyed.GetHorizAxis.Automatic := true;

      fraBody.SeriesDetected.GetHorizAxis.Automatic := true;
      fraBody.SeriesTraceDir.GetHorizAxis.Automatic := true;
      fraBody.SeriesTraceInd.GetHorizAxis.Automatic := true;
      fraBody.SeriesVaccinated.GetHorizAxis.Automatic := true;

      fraBody.SeriesPDetection.GetHorizAxis.Automatic := true;
      fraBody.SeriesPDestroy.GetHorizAxis.Automatic := true;
      fraBody.SeriesPVaccination.GetHorizAxis.Automatic := true;
      fraBody.SeriesPOver.GetHorizAxis.Automatic := true;
    end
  ;


  function TFormDailyStatusByProdType.eventY(): double;
    var
      maxY: double;
    begin
      maxY := 0.0;

      if( cbxSusceptible.Checked ) then maxY := max( maxY, fraBody.SeriesSusc.YValues.MaxValue );
      if( cbxLatent.Checked ) then maxY := max( maxY, fraBody.SeriesLatent.YValues.MaxValue );
      if( cbxSubclinical.Checked ) then maxY := max( maxY, fraBody.SeriesSubClinical.YValues.MaxValue );
      if( cbxClinical.Checked ) then maxY := max( maxY, fraBody.SeriesClinical.YValues.MaxValue );
      if( cbxNatImmune.Checked ) then maxY := max( maxY, fraBody.SeriesNatImmune.YValues.MaxValue );
      if( cbxVacImmune.Checked ) then maxY := max( maxY, fraBody.SeriesVacImmune.YValues.MaxValue );
      if( cbxDestroyed.Checked ) then maxY := max( maxY, fraBody.SeriesDestroyed.YValues.MaxValue );

      if( cbxDetected.Checked ) then maxY := max( maxY, fraBody.SeriesDetected.YValues.MaxValue );
      if( cbxTraceDir.Checked ) then maxY := max( maxY, fraBody.SeriesTraceDir.YValues.MaxValue );
      if( cbxTraceInd.Checked ) then maxY := max( maxY, fraBody.SeriesTraceInd.YValues.MaxValue );
      if( cbxVaccinated.Checked ) then maxY := max( maxY, fraBody.SeriesVaccinated.YValues.MaxValue );

      if( 0.0 = maxY ) then maxY := 1.0;

      result := _nextEventY;
      dbcout2( _nextEventY );
      dbcout2( maxY );
      dbcout2( endl );
      _nextEventY := _nextEventY + ( maxY / 4 );
    end
  ;
//-----------------------------------------------------------------------------



  procedure TFormDailyStatusByProdType.updateChartFromDatabase();
    var
      db2: TSqlDatabase;
      res: TSqlResult;
      row: TSqlRow;
      q: string;

      displayedIteration: integer;
      selectedPTIDClause: string;

      currentDay: integer;

      firstDetDay: integer;
      firstDestrDay: integer;
      firstVaccDay: integer;
    begin
      dbcout( 'Starting iteration update chart from database', DBFORMDAILYSTATUSBYPRODTYPE );
      db2 := _smdb as TSqlDatabase;

      if ( cboIteration.ItemIndex = -1 ) then
        displayedIteration := frmMain.displayedIteration
      else
        displayedIteration := myStrToInt( cboIteration.Items[cboIteration.ItemIndex], -1 )
      ;

      _currentItr := displayedIteration;

      // Determine the ID of the currently selected production type
      //-----------------------------------------------------------
      if( nil = _selectedPT ) then
        selectedPTIDClause := ''
      else
        selectedPTIDClause := ' AND productionTypeID = ' + intToStr( _selectedPT.productionTypeID )
      ;

      // select indicated pts, all days, for last complete iteration, sorted by day
      //------------------------------------------------------------------------------
      q := 'SELECT'
          // Actual status numbers
          + ' tsdUSusc,'
          + ' tsdULat,'
          + ' tsdUSubc,'
          + ' tsdUClin,'
          + ' tsdUNImm,'
          + ' tsdUVImm,'
          + ' tsdUDest,'

          // Apparent events
          + ' detnUClin,'
          + ' trnUDir,'
          + ' trnUInd,'
          + ' vaccnUAll,'
          
          // Other relevant bits
          + ' day'
        + ' FROM'
          + ' outDailyByProductionType'
        + ' WHERE'
          + ' iteration = ' + intToStr( displayedIteration )
        + selectedPTIDClause
        + ' ORDER BY'
          + ' day'
      ;

      dbcout( q, DBFORMDAILYSTATUSBYPRODTYPE );
      res := TSqlResult.create( q, db2 );

      // Draw the chart
      //---------------
      if ( res <> nil ) then
        begin
          row := res.fetchArrayFirst();
          if ( row <> nil ) then
            begin
              currentDay := row.field('day');
              _data.clear();
              _chartFixNeeded := true; // Reset this here, because the problem may occur again when loading from the database

              while( nil <> row ) do
                begin
                  // For each day, calculate and display the number of units in each state

                  if( row.field('day') <> currentDay ) then
                    begin
                      // Draw the current values...
                      drawAllSeries( currentDay );

                      //...and prep for the next day.
                      currentDay := row.field('day');
                      _data.clear();
                    end
                  ;

                  if( null <> row.field('tsdUSusc') ) then inc( _data.tsdUSusc, longint( row.field('tsdUSusc') ) );
                  if( null <> row.field('tsdULat') ) then  inc( _data.tsdULat,  longint( row.field('tsdULat') ) );
                  if( null <> row.field('tsdUSubc') ) then inc( _data.tsdUSubc, longint( row.field('tsdUSubc') ) );
                  if( null <> row.field('tsdUClin') ) then inc( _data.tsdUClin, longint( row.field('tsdUClin') ) );
                  if( null <> row.field('tsdUNImm') ) then inc( _data.tsdUNImm, longint( row.field('tsdUNImm') ) );
                  if( null <> row.field('tsdUVImm') ) then inc( _data.tsdUVImm, longint( row.field('tsdUVImm') ) );
                  if( null <> row.field('tsdUDest') ) then inc( _data.tsdUDest, longint( row.field('tsdUDest') ) );

                  if( null <> row.field('detnUClin') ) then inc( _data.detnUClin, longint( row.field('detnUClin') ) );
                  if( null <> row.field('trnUDir') ) then inc( _data.trnUDir, longint( row.field('trnUDir') ) );
                  if( null <> row.field('trnUInd') ) then inc( _data.trnUInd, longint( row.field('trnUInd') ) );
                  if( null <> row.field('vaccnUAll') ) then inc( _data.vaccnUAll, longint( row.field('vaccnUAll') ) );

                  row := res.fetchArrayNext();
                end
              ;

              // Add the markers
              //-----------------
              if( frmMain.simIsRunning ) then
                begin
                  if( nil <> _selectedPT ) then
                    begin
                      firstDetection( _selectedPT.currentOutputs.firstDetection, _selectedPT.productionTypeID );
                      firstDestruction( _selectedPT.currentOutputs.firstDestruction, _selectedPT.productionTypeID );
                      firstVaccination( _selectedPT.currentOutputs.firstVaccination, _selectedPT.productionTypeID );
                    end
                  else // set the global first action days
                    begin
                      if( -1 < frmMain.firstOverallDetection ) then firstDetection( frmMain.firstOverallDetection, -1 );
                      if( -1 < frmMain.firstOverallDestruction ) then firstDestruction( frmMain.firstOverallDestruction, -1 );
                      if( -1 < frmMain.firstOverallVaccination ) then firstVaccination( frmMain.firstOverallVaccination, -1 );
                    end
                  ;
                end
              else
                begin
                  // Check the database for firsts

                  firstDetDay := -1;
                  firstDestrDay := -1;
                  firstVaccDay := -1;

                  q := 'SELECT'
                    + ' firstDetection,'
                    + ' firstDestruction,'
                    + ' firstVaccination'
                    + ' FROM outIterationByProductionType'
                    + ' WHERE iteration = ' + intToStr( displayedIteration )
                    + selectedPTIDClause
                  ;

                  dbcout( q, DBFORMDAILYSTATUSBYPRODTYPE );

                  res.runQuery( q );
                  if( 0 <> res.numRows ) then
                    begin
                      row := res.fetchArrayFirst();
                      while( nil <> row ) do
                        begin
                          dbcout( 'firstDetection', DBFORMDAILYSTATUSBYPRODTYPE );
                          if( null <> row.field('firstDetection') ) then
                            begin
                              if
                                ( -1 = firstDetDay )
                              or
                                ( ( -1 < firstDetDay ) and ( row.field('firstDetection') < firstDetDay ) )
                              then
                                firstDetDay := row.field('firstDetection')
                              ;
                            end
                          ;

                          dbcout( 'firstDestruction', DBFORMDAILYSTATUSBYPRODTYPE );
                          if( null <> row.field('firstDestruction') ) then
                            begin
                              if
                                ( -1 = firstDestrDay )
                              or
                                ( ( -1 < firstDestrDay ) and ( row.field('firstDestruction') < firstDestrDay ) )
                              then
                                firstDestrDay := row.field('firstDestruction')
                              ;
                            end
                          ;

                          dbcout( 'firstVaccination', DBFORMDAILYSTATUSBYPRODTYPE );
                          if( null <> row.field('firstVaccination') ) then
                            begin
                              dbcout( row.field('firstVaccination'), DBFORMDAILYSTATUSBYPRODTYPE );
                              if
                                ( -1 = firstVaccDay )
                              or
                                ( ( -1 < firstVaccDay ) and ( row.field('firstVaccination') < firstVaccDay ) )
                              then
                                 firstVaccDay := row.field('firstVaccination')
                              ;
                            end
                          ;

                          row := res.fetchArrayNext();
                        end
                      ;
                    end
                  ;

                  firstDetection( firstDetDay, -1 );
                  firstDestruction( firstDestrDay, -1 );
                  firstVaccination( firstVaccDay, -1 );

                  dbcout( 'Setting outbreakEnded', DBFORMDAILYSTATUSBYPRODTYPE );

                  q := 'SELECT outbreakEndDay, outbreakEnded'
                    + ' FROM outIteration WHERE iteration = ' + intToStr( displayedIteration )
                  ;

                  res.runQuery( q );
                  if( 0 <> res.numRows ) then
                    begin
                      row := res.fetchArrayFirst();
                      if( null <> row.field('outbreakEnded') ) then
                        begin
                          if
                            ( true = row.field('outbreakEnded') )
                          and
                            ( null <> row.field('outbreakEndDay') )
                          then
                            outbreakEnd( row.field('outbreakEndDay') )
                          ;
                        end
                      ;
                    end
                  ;
                end
              ;
            end // end if row <> nil
          ;
        end  // end if res <> nil
      ;

      // clean up
      //----------
      dbcout( 'Done drawing from database', DBFORMDAILYSTATUSBYPRODTYPE );
      res.Free();
    end
  ;


  procedure TFormDailyStatusByProdType.resetIteration( const iteration: Integer );
    begin
      if ( cboIteration.Items.IndexOf( IntToStr( iteration ) ) >= 0 ) then
        begin
          cboIteration.ItemIndex := cboIteration.Items.IndexOf( IntToStr( iteration ) );

          self._currentItr := iteration;

          if( iteration > _smdb.completedIterations ) then
            pnlCaption.Caption := tr( 'Iteration status: aborted' )
          else
            pnlCaption.Caption := tr( 'Iteration status: complete' )
          ;

          try
            screen.Cursor := crHourGlass;
            setChartControlsEnabled( false );

            reset();

            updateChartFromDatabase();
          finally
            setChartControlsEnabled( true );
            screen.Cursor := crDefault;
          end;
        end
      ;
    end
  ;

  
  procedure TFormDailyStatusByProdType.cboIterationChange(Sender: TObject);
    begin
      inherited;

      if ( assigned( frmMain ) ) then
        begin
          frmMain.displayedIteration := myStrToInt( cboIteration.Items[cboIteration.ItemIndex], -1 );
        end
      else
        begin
          try
            screen.Cursor := crHourGlass;
            setChartControlsEnabled( false );
            reset();
            updateChartFromDatabase();
          finally
            setChartControlsEnabled( true );
            screen.Cursor := crDefault;
          end;
        end
      ;
    end
  ;


  procedure TFormDailyStatusByProdType.cbxMilestonesClick(Sender: TObject);
    begin
      inherited;
      if ( frmMain.simIsRunning ) then
        begin
          fraBody.SeriesPDetection.Active := cbxMilestones.Checked;
          fraBody.SeriesPDestroy.Active := cbxMilestones.Checked;
          fraBody.SeriesPVaccination.Active := cbxMilestones.Checked;
          fraBody.SeriesPOver.Active := cbxMilestones.Checked;
        end
      else
        cbxChanged( Sender )
      ;
    end
  ;


  procedure TFormDailyStatusByProdType.cbxChanged(Sender: TObject);
    begin
      inherited;
      if ( assigned( frmMain ) ) then
        begin
          frmMain.displayedIteration := myStrToInt( cboIteration.Items[cboIteration.ItemIndex], -1 );
        end
      else
        begin
          try
            screen.Cursor := crHourGlass;
            setChartControlsEnabled( false );
            reset();
            updateChartFromDatabase();
          finally
            setChartControlsEnabled( true );
            screen.Cursor := crDefault;
          end;
        end
      ;
    end
  ;


  procedure TFormDailyStatusByProdType.cbxSusceptibleClick(Sender: TObject);
    begin
      inherited;
      if ( frmMain.simIsRunning ) then
        fraBody.SeriesSusc.Active := cbxSusceptible.Checked
      else
        cbxChanged( Sender )
      ;
    end
  ;


  procedure TFormDailyStatusByProdType.cbxNatImmuneClick(Sender: TObject);
    begin
      inherited;
      if ( frmMain.simIsRunning ) then
        fraBody.SeriesNatImmune.Active := cbxNatImmune.Checked
      else
        cbxChanged( Sender )
      ;
    end
  ;


  procedure TFormDailyStatusByProdType.cbxLatentClick(Sender: TObject);
    begin
      inherited;
      if ( frmMain.simIsRunning ) then
        fraBody.SeriesLatent.Active := cbxLatent.Checked
      else
        cbxChanged( Sender )
      ;
    end
  ;


  procedure TFormDailyStatusByProdType.cbxVacImmuneClick(Sender: TObject);
    begin
      inherited;
      if ( frmMain.simIsRunning ) then
        fraBody.SeriesVacImmune.Active := cbxVacImmune.Checked
      else
        cbxChanged( Sender )
      ;
    end
  ;


  procedure TFormDailyStatusByProdType.cbxSubClinicalClick(Sender: TObject);
    begin
      inherited;
      if ( frmMain.simIsRunning ) then
        fraBody.SeriesSubClinical.Active := cbxSubClinical.Checked
      else
        cbxChanged( Sender )
      ;
    end
  ;


  procedure TFormDailyStatusByProdType.cbxDestroyedClick(Sender: TObject);
    begin
      inherited;
      if ( frmMain.simIsRunning ) then
        fraBody.SeriesDestroyed.Active := cbxDestroyed.Checked
      else
        cbxChanged( Sender )
      ;
    end
  ;


  procedure TFormDailyStatusByProdType.cbxClinicalClick(Sender: TObject);
    begin
      inherited;
      if ( frmMain.simIsRunning ) then
        fraBody.SeriesClinical.Active := cbxClinical.Checked
      else
        cbxChanged( Sender )
      ;
    end
  ;


  procedure TFormDailyStatusByProdType.cbxDetectedClick(Sender: TObject);
    begin
      inherited;
      if ( frmMain.simIsRunning ) then
        fraBody.SeriesDetected.Active := cbxDetected.Checked
      else
        cbxChanged( Sender )
      ;
    end
  ;


  procedure TFormDailyStatusByProdType.cbxTraceDirClick(Sender: TObject);
    begin
      inherited;
      if ( frmMain.simIsRunning ) then
        fraBody.SeriesTraceDir.Active := cbxTraceDir.Checked
      else
        cbxChanged( Sender )
      ;
    end
  ;


  procedure TFormDailyStatusByProdType.cbxTraceIndClick(Sender: TObject);
    begin
      inherited;
      if ( frmMain.simIsRunning ) then
        fraBody.SeriesTraceInd.Active := cbxTraceInd.Checked
      else
        cbxChanged( Sender )
      ;
    end
  ;


  procedure TFormDailyStatusByProdType.cbxVaccinatedClick(Sender: TObject);
    begin
      inherited;
      if ( frmMain.simIsRunning ) then
        fraBody.SeriesVaccinated.Active := cbxVaccinated.Checked
      else
        cbxChanged( Sender )
      ;
    end
  ;

  
  procedure TFormDailyStatusByProdType.setCaption();
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

