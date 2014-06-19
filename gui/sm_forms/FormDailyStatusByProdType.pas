{*

FormDailyStatusByProdType.pas/dfm
---------------------------------
Begin: 2005/06/28
Last revision: $Date: 2013-06-27 19:11:25 $ $Author: areeves $
Version: $Revision: 1.40.4.7 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <aaron.reeves@naadsm.org>
Author: Ric Hupalo <Ric.Hupalo@colostate.edu>
--------------------------------------------------
Copyright (C) 2005 - 2010 Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

   This form displays several graphs showing the number of herds in each
   Disease State, the Control Status, and the Detection Status on each day
   of a single iteration. Showing Events and cumulative daily output are runtime options.
   This form has dependencies on FormMain.
}

unit FormDailyStatusByProdType;

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
    ComCtrls,


    // Units used for graphs
    TeeProcs,
    TeEngine,
    Chart,
    Series,
    StatusEnums,

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
    FrameDailyStatusByProdTypeDiseaseStats,
    FrameDailyStatusByProdTypeControlStats,
    FrameDailyStatusByProdTypeDetectionStats
  ;


  type  TFormDailyStatusByProdType = class( TFormSMOutputBase )
      pnlCaption: TPanel;
      pcCharts: TPageControl;
      tsDiseaseStatus: TTabSheet;
      tsControlStatus: TTabSheet;
      tsDetectionStatus: TTabSheet;
      fraDiseaseStatus: TFrameDailyStatusByProdTypeDiseaseStats;
      fraControlStatus: TFrameDailyStatusByProdTypeControlStats;
      fraDetectionStatus: TFrameDailyStatusByProdTypeDetectionStats;

    protected
      _data: TSMDailyOutput;     /// The daily values to display
      _itDetOccurred: integer;   // Day when first detection occurred
      _itDestrOccurred: integer; // Day when first destruction occurred
      _itVaccOccurred: integer;  // Day when first vaccination occurred
      _outbreakEnd: integer;     // Day when the outbreak ended
      _currentItr: integer;      // Currently displayed iteration

      procedure translateUI();
      procedure translateUIManual();

      { Used to create a list of all charts on the form that might be saved/copied/printed }
      procedure fillChartDict(); override;

      { Updates the contents of the form when the selected production type changes }
      procedure productionTypeChanged(); override;

      { Updates the contents of the form when the loaded database/simulation changes }
      procedure simChanged(); override;

      { Updates the contents of the form when the selected iteration changes }
      procedure iterationChanged(); override;

      { Chart drawing functions }
      procedure drawAllSeries( day: integer );

      { Calculates the total number of units in all disease states }
      function totalUnits(): longint;

      { Calculates the total number of units infected by any mechanism }
  		function totalInfected(): longint;

      { Update charts with information from the database.  All database handling is done on the form.  Only drawing takes place on the chart frames. }
      procedure updateChartsFromDatabase();

      { Show the status of the current iteration}
      procedure setCaption();

    public
      // methods
    	constructor create( AOwner: TComponent; sim: TSMSimulationInput; db: TSMDatabase ); reintroduce;
      destructor destroy(); override;

      /// Reinitializes event private members andinstructs the charting frames to clear their charts
      procedure reset();

      /// Handles chart updates for the current day while a simulation is in progress
      procedure updateForDay( day: integer );

      /// Populates the form's iteration list and other GUI features when a running simulation has completed
      procedure updateSimComplete(); override;

      /// Has frames add a point for the first Detection (across all producton types if charting all types) in an iteration.
      procedure firstDetection( const day: integer; const ptID: integer );

      /// Has frames add a point for the first Destruction (across all producton types if charting all types) in an iteration.
      procedure firstDestruction( const day: integer; const ptID: integer );

      /// Has frames add a point for the first Vaccination (across all producton types if charting all types) in an iteration.
      procedure firstVaccination( const day: integer; const ptID: integer );

      /// Has frames add a point when the Outbreak ends in an iteration.
      procedure outbreakEnd( day: integer );

      /// When an iteration ends this notifies the user and updates the GUI with the result
      procedure resetIteration( const iteration: Integer );

      /// Means for child frames to notify the container that the user has changed some charting option so a data referesh needed
      procedure cbxChanged(Sender: TObject);
    end
  ;

  var
		frmDailyStatusByProdType: TFormDailyStatusByProdType;

  const
  	DBFORMDAILYSTATUSBYPRODTYPE: boolean = false; /// set to true to enable debugging messages for this unit.

    
implementation

	{$R *.dfm}

  uses
    // Built-in Delphi units
    Math,
    StrUtils,

    // General-purpose units
    MyStrUtils,
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
      placeIterationControls();
      setDataControlsEnabled( false );

      pcCharts.ActivePageIndex := 0;

      _smSim := sim;
      _smdb := db;
      _data := TSMDailyOutput.create();
      _selectedPT := nil;
      _currentItr := frmMain.displayedIteration;

      // Size the form
      //--------------
      ClientWidth := 556 + lblIteration.Width + cboIteration.Width;
      disableIterationComboBox();

      setupIterationComboBox();
      setupProdTypeComboBox();
      updateChartsFromDatabase();
      setCaption();
      pcCharts.ActivePage.Name := 'tsDiseaseState';

      if ( frmMain.simIsRunning ) then
        disableIterationComboBox()
      ;
      //updateChartsFromDatabase(); // No reason to do this twice, is there?
    end
  ;


  destructor TFormDailyStatusByProdType.destroy();
  	begin
      _data.Free();
    	inherited destroy();
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
      // For any phrases that could not be automatically extracted by Caption Collector
    end
  ;


  procedure TFormDailyStatusByProdType.fillChartDict();
    begin
      _chartDict[fraDiseaseStatus.Name] := fraDiseaseStatus;
      _chartDict[fraControlStatus.Name] := fraControlStatus;
      _chartDict[fraDetectionStatus.Name] := fraDetectionStatus;
    end
  ;

//-----------------------------------------------------------------------------
// Iterations and Production Type Updating
//-----------------------------------------------------------------------------

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


  procedure TFormDailyStatusByProdType.productionTypeChanged();
    begin
      updateChartsFromDatabase();
    end
  ;

  
	procedure TFormDailyStatusByProdType.simChanged();
  	begin
      if ( frmMain.simIsRunning ) then
        disableIterationComboBox()
      else
        setupIterationComboBox()
      ;

      productionTypeChanged();
      setCaption();
    end
  ;


  procedure TFormDailyStatusByProdType.reset();
    begin
        fraDiseaseStatus.reset();
        fraControlStatus.reset();
        fraDetectionStatus.reset();

      _itDetOccurred := -1;
      _itDestrOccurred := -1;
      _itVaccOccurred := -1;
      _outbreakEnd := -1;
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

          updateChartsFromDatabase();
        end
      ;
    end
  ;


  procedure TFormDailyStatusByProdType.iterationChanged();
    begin
      if ( assigned( frmMain ) ) then
        begin
          frmMain.displayedIteration := myStrToInt( cboIteration.Items[cboIteration.ItemIndex], -1 );
        end
      else
        updateChartsFromDatabase()
      ;
    end
  ;

//-----------------------------------------------------------------------------
// Frame Chart(s) Data Related
//-----------------------------------------------------------------------------
  procedure TFormDailyStatusByProdType.updateSimComplete();
    begin
      setupIterationComboBox();
      updateChartsFromDatabase();
      resetIteration( StrToInt( cboIteration.Items[0] ) );
      frmMain.displayedIteration := StrToInt( cboIteration.Items[0] );
    end
  ;


  {*
    This function handles chart updates while a simulation is in progress.
    When a simulation is not running (or when the selected production type is
    changed in midstream), updateChartsFromDatabase() is used.
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


  procedure TFormDailyStatusByProdType.updateChartsFromDatabase();
    var
      res: TSqlResult;
      row: TSqlRow;
      q: string;

      selectedPTIDClause: string;

      currentDay: integer;

      firstDetDay: integer;
      firstDestrDay: integer;
      firstVaccDay: integer;
    begin
      dbcout( 'Starting iteration update chart from database', DBFORMDAILYSTATUSBYPRODTYPE );

      reset();

      if( ( not frmMain.simIsRunning ) and ( 1 > frmMain.displayedIteration ) ) then
        exit
      ;

      // Determine the iteration to be displayed
      //----------------------------------------
      if( frmMain.simIsRunning ) then
        _currentItr := frmMain.iterationInProgress
      else if ( -1 = cboIteration.ItemIndex ) then
        _currentItr := frmMain.displayedIteration
      else
        _currentItr := myStrToInt( cboIteration.Items[cboIteration.ItemIndex], -1 )
      ;

      if( 1 > _currentItr ) then
        exit
      ;


      screen.Cursor := crHourGlass;
      setChartControlsEnabled( false );

      res := TSqlResult.create( _smdb as TSqlDatabase );

      // Determine the ID of the currently selected production type
      //-----------------------------------------------------------
      if( nil = _selectedPT ) then
        selectedPTIDClause := ''
      else
        selectedPTIDClause := ' AND productionTypeID = ' + intToStr( _selectedPT.productionTypeID )
      ;


      // select pt, all days, for selected iteration, sorted by day
      //-----------------------------------------------------------
      q := 'SELECT'
          // Actual status numbers
          + ' tsdUSusc,'
          + ' tsdULat,'
          + ' tsdUSubc,'
          + ' tsdUClin,'
          + ' tsdUNImm,'
          + ' tsdUVImm,'
          + ' tsdUDest,'

          // Control status numbers
          + ' detnUClin,'
          + ' detnUTest,'
          + ' desnUAll,'
          + ' vacnUAll,'
          + ' trnUDirFwd,'
          + ' trnUIndFwd,'
          + ' trnUDirBack,'
          + ' trnUIndBack,'

          // Detection status numbers
          + ' exmnUAll,'
          + ' detnUClin,'
          + ' detnUTest,'
          + ' desnUAll,'
          + ' tstnUTruePos,'
          + ' tstnUTrueNeg,'
          + ' tstnUFalsePos,'
          + ' tstnUFalseNeg,'

          // Other relevant bits
          + ' day'
        + ' FROM'
          + ' outDailyByProductionType'
        + ' WHERE'
          + ' iteration = ' + intToStr( _currentItr )
        + selectedPTIDClause
        + ' ORDER BY'
          + ' day'
      ;
      res.runQuery( q );

      if( 0 <> res.numRows ) then
        begin
          row := res.fetchArrayFirst();

          if ( nil <> row ) then
            begin
              currentDay := row.field('day');
              _data.clear();

              while( nil <> row ) do
                begin
                  // For each day, calculate and display the number of units in each state

                  // This bit of code deals with the records for multiple production types.
                  // There will be a separate record for each day for each production type.
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

                  if( null <> row.field('vacnUAll') ) then inc( _data.vacnUAll, longint( row.field('vacnUAll') ) );
                  if( null <> row.field('trnUDirFwd') ) then inc( _data.trnUDirFwd, longint( row.field('trnUDirFwd') ) );
                  if( null <> row.field('trnUIndFwd') ) then inc( _data.trnUIndFwd, longint( row.field('trnUIndFwd') ) );
                  if( null <> row.field('trnUDirBack') ) then inc( _data.trnUDirBack, longint( row.field('trnUDirBack') ) );
                  if( null <> row.field('trnUIndBack') ) then inc( _data.trnUIndBack, longint( row.field('trnUIndBack') ) );

                  if( null <> row.field('exmnUAll') ) then inc( _data.exmnUAll, longint( row.field('exmnUAll') ) );
                  if( null <> row.field('detnUClin') ) then  inc( _data.detnUClin,  longint( row.field('detnUClin') ) );
                  if( null <> row.field('detnUTest') ) then inc( _data.detnUTest, longint( row.field('detnUTest') ) );
                  if( null <> row.field('desnUAll') ) then inc( _data.desnUAll, longint( row.field('desnUAll') ) );
                  if( null <> row.field('tstnUTruePos') ) then inc( _data.tstnUTruePos, longint( row.field('tstnUTruePos') ) );
                  if( null <> row.field('tstnUTrueNeg') ) then inc( _data.tstnUTrueNeg, longint( row.field('tstnUTrueNeg') ) );
                  if( null <> row.field('tstnUFalsePos') ) then inc( _data.tstnUFalsePos, longint( row.field('tstnUFalsePos') ) );
                  if( null <> row.field('tstnUFalseNeg') ) then inc( _data.tstnUFalseNeg, longint( row.field('tstnUFalseNeg') ) );

                  row := res.fetchArrayNext();
                end
              ;
            end
          ;
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
            + ' WHERE iteration = ' + intToStr( _currentItr )
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
            + ' FROM outIteration WHERE iteration = ' + intToStr( _currentItr )
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

      // clean up and go home
      //---------------------
      setChartControlsEnabled( true );
      screen.Cursor := crDefault;
      res.Free();
      
      dbcout( 'Done drawing from database', DBFORMDAILYSTATUSBYPRODTYPE );
    end
  ;


  procedure TFormDailyStatusByProdType.cbxChanged(Sender: TObject);
    begin
      updateChartsFromDatabase();
    end
  ;

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
// Chart drawing functions
//-----------------------------------------------------------------------------


  procedure TFormDailyStatusByProdType.drawAllSeries( day: integer );
  	var
      total: longint;
  	begin
    	total := totalUnits();

      if( 0 = total ) then
      	begin
       		dbcout( endl + 'drawAllSeries is exiting early.' + endl, DBFORMDAILYSTATUSBYPRODTYPE );
      		exit;
        end
      ;

      dbcout( 'Day ' + inttoStr( day )
          + ', # susceptible '
          + intToStr( _data.tsdUSusc )
          + ', total ' + inttoStr( total ),
        DBFORMDAILYSTATUSBYPRODTYPE
      );

      //Graphs...
      fraDiseaseStatus.drawAllSeries( day, _data );
      fraControlStatus.drawAllSeries( day, _data );
      fraDetectionStatus.drawAllSeries( day, _data );
    end
  ;


  procedure TFormDailyStatusByProdType.firstDetection( const day: integer; const ptID: integer );
  	begin
      // If all production types are shown, another pt may have been detected earlier.
      // If so, don't show another dot.
      if( -1 <> _itDetOccurred ) then
        exit
      ;

      if( ( -1 <> ptID ) and ( nil <> _selectedPT )  and ( -1 < day ) ) then
        begin
          if( ptID = _selectedPT.productionTypeID ) then
            begin
              fraDiseaseStatus.drawFirstDetection( day );
              fraControlStatus.drawFirstDetection( day );
              fraDetectionStatus.drawFirstDetection( day );
              dbcout( 'First detection on day ' + intToStr( day ), DBFORMDAILYSTATUSBYPRODTYPE );
            end
          ;
        end
      else if( -1 < day ) then
        begin
          fraDiseaseStatus.drawFirstDetection( day );
          fraControlStatus.drawFirstDetection( day );
          fraDetectionStatus.drawFirstDetection( day );
          _itDetOccurred := day;
          dbcout( 'First detection on day ' + intToStr( day ), DBFORMDAILYSTATUSBYPRODTYPE );
        end
      ;
    end
  ;


  procedure TFormDailyStatusByProdType.firstDestruction( const day: integer; const ptID: integer );
  	begin
      // If all production types are shown, another pt may have been destroyed earlier.
      // If so, don't show another dot.
      if( -1 <> _itDestrOccurred ) then
        exit
      ;

      if( ( -1 <> ptID ) and ( nil <> _selectedPT ) and ( -1 < day ) ) then
        begin
          if( ptID = _selectedPT.productionTypeID ) then
            begin
              fraDiseaseStatus.drawFirstDestruction( day );
              fraControlStatus.drawFirstDestruction( day );
              fraDetectionStatus.drawFirstDestruction( day );
            end
          ;
        end
      else if( -1 < day ) then
        begin
          fraDiseaseStatus.drawFirstDestruction( day );
          fraControlStatus.drawFirstDestruction( day );
          fraDetectionStatus.drawFirstDestruction( day );
          _itDestrOccurred := day;
        end
      ;
    end
  ;


  procedure TFormDailyStatusByProdType.firstVaccination( const day: integer; const ptID: integer );
  	begin
      // If all production types are shown, another pt may have been vaccinated earlier.
      // If so, don't show another dot.
      if( -1 <> _itVaccOccurred ) then
        exit
      ;

      if( ( -1 <> ptID ) and ( nil <> _selectedPT ) and ( -1 < day ) ) then
        begin
          if( ptID = _selectedPT.productionTypeID ) then
            begin
              fraDiseaseStatus.drawFirstVaccination( day );
              fraControlStatus.drawFirstVaccination( day );
              fraDetectionStatus.drawFirstVaccination( day );
            end
          ;
        end
      else if( -1 < day ) then
        begin
          fraDiseaseStatus.drawFirstVaccination( day );
          fraControlStatus.drawFirstVaccination( day );
          fraDetectionStatus.drawFirstVaccination( day );
          _itVaccOccurred := day;
        end
      ;
    end
  ;


  procedure TFormDailyStatusByProdType.outbreakEnd( day: integer );
  	begin
      fraDiseaseStatus.drawOutbreakEnd( day );
      fraControlStatus.drawOutbreakEnd( day );
      fraDetectionStatus.drawOutbreakEnd( day );

      _outbreakEnd := day;
    end
  ;


end.

