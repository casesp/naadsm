unit FrameEpiIterationSummary;

(*
FrameEpiIterationSummary.pas/dfm
--------------------------------
Begin: 2005/12/07
Last revision: $Date: 2013-04-01 18:54:31 $ $Author: areeves $
Version: $Revision: 1.34.2.12 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2005 - 2013 Colorado State University

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

    // Delphi units for TChart
		TeeProcs,
		TeEngine,
		Chart,
		Series,

    // General purpose units
    SqlClasses,
    PBPageControl,

    // Application-specific data structures
    SMDatabase,
    SMSimulationInput,
    Herd,
    SMSimOutByProdType,
    ProductionType,
    ProductionTypeList,

    // Application-specific widgets
    FrameChartBase,
    FrameSingleEpiCurve,
    FrameStringGridBase
	;


  {*
    This class displays summary epidemiological outputs for a single iteration.
  }
	type TFrameEpiIterationSummary = class( TFrame )

      // Controls on sbxSingleIteration
      //-------------------------------
      sgHeader: TStringGrid;

    	pnlSurvHeader: TPanel;
    	cbxSurv: TCheckBox;
    	pnlSurv: TPanel;
      fraSgSurv: TFrameStringGridBase;

    	pnlDestrHeader: TPanel;
    	cbxDestr: TCheckBox;
    	pnlDestr: TPanel;
      fraSgDestr: TFrameStringGridBase;

    	pnlVacHeader: TPanel;
    	cbxVac: TCheckBox;
      PanelVac: TPanel;
      fraSgVac: TFrameStringGridBase;

    	pnlInfHeader: TPanel;
    	cbxInf: TCheckBox;
      PanelInf: TPanel;
      fraSgInf: TFrameStringGridBase;

    	pnlApparent: TPanel;
    	cbxApparent: TCheckBox;
      pnlApparentChart: TPanel;
      fraApparent: TFrameSingleEpiCurve;

      pnlInapparent: TPanel;
      cbxInapparent: TCheckBox;
      pnlInapparentChart: TPanel;
      fraInapparent: TFrameSingleEpiCurve;
      lblAsterisk: TLabel;
      pbpGraphTableTabs: TPBPageControl;
      tabGraphs: TTabSheet;
      tabTables: TTabSheet;

      // Used by all string grids
      //-------------------------
			procedure exitStringGrid( Sender: TObject );

			procedure cbxSurvClick( Sender: TObject );
			procedure cbxDestrClick( Sender: TObject );
			procedure cbxVacClick( Sender: TObject );
      procedure cbxInfClick( Sender: TObject );
			procedure cbxApparentClick( Sender: TObject );
			procedure cbxInapparentClick( Sender: TObject );
      procedure FrameResize(Sender: TObject);

		protected
      _smSim: TSMSimulationInput;
      _smdb: TSMDatabase;
      _selectedPT: TProductionType;

      _data: TSMDailyOutput;

      _sqlRes: TSqlResult;
      
      _currentItr: Integer;

      procedure translateUI();
      procedure translateUIManual();
      
      // Initialization functions
      //-------------------------
      procedure SetupGrids();
			procedure setPanelSizes();


      // Data display functions
      //------------------------
			procedure fillGrids();
			procedure totalColumns( sg: TFrameStringGridBase; startRow, endRow, totalRow: byte );
      procedure drawSeries( day: integer; detOnDay, infOnDay: longint );


      // Minor helper functions
      //-----------------------
      procedure clearGrid( sg: TFrameStringGridBase );
      procedure ClearStringGridSelection( SG: TFrameStringGridBase );
			procedure SizeThePanel( sg: TFrameStringGridBase; P1, P2: TPanel );
      procedure resizeScrollBox();
			procedure toggleStringGrid( sg: TFrameStringGridBase; PO, PI: TPanel );
    	procedure reSizeStringGrid( sg: TFrameStringGridBase; PO, PI: TPanel );
    	procedure reSizeChart( CF: TFrame; PO, PI : TPanel );

      // Handles form updates from the database when a simulation is not running or when _selectedPT is changed
      //-------------------------------------------------------------------------------------------------------
      procedure setUpFromDatabase( Itr:Integer );

		public
      constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

      procedure reset();

      procedure resizeContents();

      procedure resetSim( db: TSMDatabase; sim: TSMSimulationInput; pt: TProductionType; newItr: Integer = -1 );
      procedure setProdType( pt: TProductionType; const simIsRunning: boolean );
     	procedure updateFromDatabase( Itr: Integer );

      // Handles chart updates while a simulation is in progress
      //--------------------------------------------------------
      procedure updateForDay( day: integer );

      // Property-like functions
      //------------------------
      function allChartBoxesUnchecked(): boolean;
      function allTextBoxesUnchecked(): boolean;
      function allBoxesUnchecked(): boolean;
		end
	;


implementation

	{$R *.dfm}

	uses
		FormMain,
    MyDialogs,
    MyStrUtils,
    RoundToXReplacement_3c,
    DebugWindow,
    I88n
	;

  const
		CHARTHEIGHT = 250;


//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
	constructor TFrameEpiIterationSummary.create( AOwner: TComponent );
		begin
      inherited create( AOwner );
      translateUI();

      _currentItr := -1;
      _data := TSMDailyOutput.create();

      _selectedPT := nil;
      _sqlRes := nil;

      setPanelSizes();
		end
	;


  procedure TFrameEpiIterationSummary.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.2.
      // Generation date: Wed Mar 12 16:13:44 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-HEAD/sm_forms/FrameEpiIterationSummary.dfm
      // File date: Wed Mar 12 15:41:54 2008

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          tabGraphs.Caption := tr( 'Graphical view' );
          cbxInapparent.Caption := tr( 'Actual epidemic curve -- includes all infections' );
          cbxApparent.Caption := tr( 'Apparent epidemic curve -- includes only detected infections' );
          tabTables.Caption := tr( 'Tabular view' );
          cbxInf.Caption := tr( 'Infection and exposure' );
          cbxVac.Caption := tr( 'Vaccination' );
          lblAsterisk.Caption := tr( '* In the course of a simulation run, these activities may occur more than once on a single unit' );
          cbxDestr.Caption := tr( 'Destruction' );
          cbxSurv.Caption := tr( 'Detection and tracing' );
        end
      ;

      // Set TChart properties
      with self do
        begin
          fraInapparent.chtCurve.Title.Text.Strings[0] := tr( 'Actual epidemic curve' );
        end
      ;

      // If any phrases are found that could not be automatically extracted by
      // Caption Collector, modify the following function to take care of them.
      translateUIManual();
    end
  ;


  procedure TFrameEpiIterationSummary.translateUIManual();
    begin
      tabGraphs.Caption := ' ' + tabGraphs.Caption;
      tabTables.Caption := ' ' + tabTables.Caption;

      fraInapparent.chtCurve.Title.Text[0] := tr( 'Actual epidemic curve' );
      fraApparent.chtCurve.Title.Text[0] := tr( 'Apparent epidemic curve' );
    end
  ;


	destructor TFrameEpiIterationSummary.destroy();
		begin
      _data.Free();
      if( nil <> _sqlRes ) then _sqlRes.Free();

      inherited destroy();
		end
	;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Reinitalization
//-----------------------------------------------------------------------------
	procedure TFrameEpiIterationSummary.resetSim( db: TSMDatabase; sim: TSMSimulationInput; pt: TProductionType; newItr: Integer = -1 );
  	var
    	db2: TSqlDatabase;
  	begin
   		_smSim := sim;
      _smdb := db;

      if ( newItr > 0 ) then
        _currentItr := newItr
      ;

			setupGrids();

      // Set up the reusable SQL query object
      if( nil <> _sqlRes ) then _sqlRes.Free();
      db2 := _smdb as TSqlDatabase;
      _sqlRes := TSqlResult.Create( db2 );

      setProdType( pt, false );

      setPanelSizes();
      resizeScrollBox();
    end
  ;


  procedure TFrameEpiIterationSummary.setProdType( pt: TProductionType; const simIsRunning: boolean );
    begin
      resizeContents();

      if( simIsRunning ) then
        _currentItr := -1
      ;

      _selectedPt := pt;
      setupFromDatabase( _currentItr );
    end
  ;


  procedure TFrameEpiIterationSummary.reset();
    begin
      _data.clear();

			fraApparent.ClearSeries();
			fraInapparent.ClearSeries();

      clearGrid( fraSgSurv );
      clearGrid( fraSgDestr );
      clearGrid( fraSgVac );
      clearGrid( fraSgInf );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Helper procedures called by initialize()
//-----------------------------------------------------------------------------

  /// Label the data rows for the Epidemiology Tabular View table, on the UI the sections follow a different order
  procedure TFrameEpiIterationSummary.SetupGrids();
    begin
      sgHeader.Cells[1,0] := tr( 'Cumulative number of units' );
      sgHeader.Cells[2,0] := tr( 'Cumulative number of animals' );

      //Detection and Tracing
      clearGrid( fraSgSurv );
      fraSgSurv.Cells[0,0] := tr( 'Clinical detections' );
      fraSgSurv.Cells[0,1] := tr( 'Diagnostic testing detections' );
      fraSgSurv.Cells[0,2] := tr( 'Dead from disease detections' );
      fraSgSurv.Cells[0,3] := tr( 'Total detected units' );
      fraSgSurv.Cells[0,4] := tr( 'Direct forward traces (successfully traced)*' );
      fraSgSurv.Cells[0,5] := tr( 'Indirect forward traces (successfully traced)*' );
      fraSgSurv.Cells[0,6] := tr( 'Direct backward traces (successfully traced)*' );
      fraSgSurv.Cells[0,7] := tr( 'Indirect backward traces (successfully traced)*' );

      // Fix Me: 20091014 Which of these labels should end in " *" ? Arron said he will revisit this week, else remind him
      // Herd Exams
      fraSgSurv.Cells[0,8] := tr( 'Herd exams from direct forward traces' );
      fraSgSurv.Cells[0,9] := tr( 'Herd exams from indirect forward traces' );
      fraSgSurv.Cells[0,10] := tr( 'Herd exams from direct backward traces' );
      fraSgSurv.Cells[0,11] := tr( 'Herd exams from indirect backward traces' );

      // Fix Me: 20091014 Which of these labels should end in " *" ? Arron said he will revisit this week, else remind him
      // Diagnostic Testing
      fraSgSurv.Cells[0,12] := tr( 'Diagnostic testing from direct forward traces' );
      fraSgSurv.Cells[0,13] := tr( 'Diagnostic testing from indirect forward traces' );
      fraSgSurv.Cells[0,14] := tr( 'Diagnostic testing from direct backward traces' );
      fraSgSurv.Cells[0,15] := tr( 'Diagnostic testing from indirect backward traces' );
      fraSgSurv.Cells[0,16] := tr( 'True positive diagnostic test result' );
      fraSgSurv.Cells[0,17] := tr( 'True negative diagnostic test result' );
      fraSgSurv.Cells[0,18] := tr( 'False positive diagnostic test result' );
      fraSgSurv.Cells[0,19] := tr( 'False negative diagnostic test result' );

      // Destruction
      clearGrid( fraSgDestr );
      frasgDestr.Cells[0,0] := tr( 'Initially destroyed' );
      fraSgDestr.Cells[0,1] := tr( 'Detection' );
      fraSgDestr.Cells[0,2] := tr( 'Direct forward traces' );
      fraSgDestr.Cells[0,3] := tr( 'Indirect forward traces' );
      fraSgDestr.Cells[0,4] := tr( 'Direct backward traces' );
      fraSgDestr.Cells[0,5] := tr( 'Indirect backward traces' );
      fraSgDestr.Cells[0,6] := tr( 'Ring' ); // FIX ME: dynamically change radius, depending on production type? //'Circle (3 km)';
      fraSgDestr.Cells[0,7] := tr( 'Disposal, cleaning, and disinfection only' );
      fraSgDestr.Cells[0,8] := tr( 'TOTAL' );

      // Vaccination
      clearGrid( fraSgVac );
      fraSgVac.Cells[0,0] := tr( 'Initially vaccinated' );
      fraSgVac.Cells[0,1] :=  tr( 'Ring*' ); // FIX ME: dynamically change radius, depending on production type? //'Ring (5 km)*';
      fraSgVac.Cells[0,2] := 'TOTAL';

      // Infection
      clearGrid( fraSgInf );
      fraSgInf.Cells[0,0] := tr( 'Initially infected' );
      fraSgInf.Cells[0,1] := tr( 'Become infected' );
      fraSgInf.Cells[0,2] := tr( 'Direct contact adequate exposures*' );
      fraSgInf.Cells[0,3] := tr( 'Indirect contact adequate exposures*' );
      fraSgInf.Cells[0,4] := tr( 'Local area adequate exposures*' );
      fraSgInf.Cells[0,5] := tr( 'Airborne adequate exposures*' );
      fraSgInf.Cells[0,6] := tr( 'TOTAL adequate exposures' );

      ClearStringGridSelection( fraSgSurv );
      ClearStringGridSelection( fraSgDestr );
      ClearStringGridSelection( fraSgVac );
      ClearStringGridSelection( fraSgInf );
    end
  ;


	procedure TFrameEpiIterationSummary.setPanelSizes();
    var
      _currentTab: Integer;
  	begin
			SizeThePanel( fraSgSurv, pnlSurv, pnlSurvHeader );
			SizeThePanel( fraSgDestr, pnlDestr, pnlDestrHeader );
			SizeThePanel( fraSgVac, PanelVac, pnlVacHeader );
			SizeThePanel( fraSgInf, PanelInf, pnlInfHeader );

			pnlApparentChart.Height := CHARTHEIGHT + pnlApparent.Height;
			pnlInapparentChart.Height := CHARTHEIGHT + pnlApparent.Height;


      _currentTab := pbpGraphTableTabs.ActivePageIndex;
      pbpGraphTableTabs.Visible := false;
			reSizeStringGrid( fraSgSurv, pnlSurv, pnlSurvHeader );
			reSizeStringGrid( fraSgDestr, pnlDestr, pnlDestrHeader );
  		reSizeStringGrid( fraSgVac, PanelVac, pnlVacHeader );
			reSizeChart( fraApparent, pnlApparentChart, pnlApparent );
	  	reSizeChart( fraInapparent, pnlInapparentChart, pnlInapparent );
		  reSizeStringGrid( fraSgInf, PanelInf, pnlInfHeader );
      pbpGraphTableTabs.ActivePageIndex := _currentTab;
      pbpGraphTableTabs.Visible := true;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Form updating functions
//-----------------------------------------------------------------------------

	procedure TFrameEpiIterationSummary.updateFromDatabase( Itr: Integer );
    begin
      setUpFromDatabase( Itr );
    end;

	procedure TFrameEpiIterationSummary.setUpFromDatabase( Itr: Integer );
    var
      row: TSqlRow;
      q: string;

      lastIteration: integer;
      day: integer;
      selectedPTIDClause: string;

      newInfOnDay: longint;
      newDetOnDay: longint;
  	begin
      reset();

      if ( Itr = -1 ) then
        begin
          // Determine the last/current iteration
          //--------------------------------------
          q := 'SELECT MAX(iteration) AS maxIt FROM outDailyByProductionType';
          _sqlRes.runQuery( q );
          row := _sqlRes.fetchArrayFirst();

          if( null = row.field('maxIt') ) then // There is no data in the database.
          		exit
    			else
          	lastIteration := row.field('maxIt');
          ;
        end
      else
        lastIteration := Itr
      ;


      // Determine the last day of the last/current iteration
      //------------------------------------------------------
      q := 'SELECT MAX(day) AS maxDay FROM outDailyByProductionType WHERE iteration = ' + intToStr( lastIteration );
      _sqlRes.runQuery( q );
      row := _sqlRes.fetchArrayFirst();

      if( null = row.field('maxDay') ) then // There is no data in the database.
      		exit
			else
      	day := row.field('maxDay')
      ;

      // Determine the ID of the currently selected production type
      //-----------------------------------------------------------
      if( nil = _selectedPT ) then
      	selectedPTIDClause := ''
      else
      	selectedPTIDClause := ' AND productionTypeID = ' + intToStr( _selectedPT.productionTypeID )
      ;

      // select CUMULATIVE data for indicated pts, last day, last/current iteration
      //----------------------------------------------------------------------------
      q :=
      	'SELECT'
          // Detection
          + ' detcUClin,'  // Number of units detected by clinical signs over the course of an iteration
          + ' detcAClin,'  // Total number of animals in all units detected by clinical signs over the course of an iteration
          + ' detcUTest,'  // Number of units detected by diagnostic testing over the course of an iteration
          + ' detcATest,'  // Total number of animals in all units detected by diagnostic testing over the course of an iteration

          + ' detcUDeadAll,'  // Number of units that were not detected until they entered the the "dead from disease" state over the course of the iteration
          + ' detcADeadAll,'  // Total number of animals in units that were not detected until they entered the "dead from disease" state over the course of the iteration

          + ' detcUAll,'   // Number of "unique" units detected from clinical signs, testing, and detected dead over the course of an iteration
          + ' detcAAll,'   // Total number of animals in all "unique" units detected from clinical signs, testing, and detected dead over the course of an iteration

        	// Tracing - Forward
          + ' trcUDirFwd,'  // Number of units directly exposed and successfully traced forward over the course of an iteration
          + ' trcADirFwd,'  // Total number of animals in all units directly exposed and successfully traced forward over the course of an iteration
          + ' trcUIndFwd,'  // Number of units indirectly exposed and successfully traced forward over the course of an iteration
          + ' trcAIndFwd,'  // Total number of animals in all units indirectly exposed and successfully traced forward over the course of an iteration
          + ' trcUDirpFwd,'  // Number of units directly exposed that could possibly have been traced forward over the course of an iteration
          + ' trcADirpFwd,'  // Total number of animals in all units directly exposed that could possibly have been traced forward over the course of an iteration
          + ' trcUIndpFwd,'  // Number of units indirectly exposed that could possibly have been traced forward over the course of an iteration
          + ' trcAIndpFwd,'  // Total number of animals in units indirectly exposed that could possibly have been traced forward over the course of an iteration

          // Tracing - Backward
          + ' trcUDirBack,'  // Number of units directly exposed and successfully traced back over the course of an iteration
          + ' trcADirBack,'  // Total number of animals in all units directly exposed and successfully traced back over the course of an iteration
          + ' trcUIndBack,'  // Number of units indirectly exposed and successfully traced back over the course of an iteration
          + ' trcAIndBack,'  // Total number of animals in all units indirectly exposed and successfully traced back over the course of an iteration
          + ' trcUDirpBack,' // Number of units directly exposed that could possibly have been traced back over the course of an iteration
          + ' trcADirpBack,' // Total number of animals in all units directly exposed that could possibly have been traced back over the course of an iteration
          + ' trcUIndpBack,' // Number of units indirectly exposed that could possibly have been traced back over the course of an iteration
          + ' trcAIndpBack,' // Total number of animals in units indirectly exposed that could possibly have been traced back over the course of an iteration

          // Tracing - Origins
          + ' tocUDirFwd,'  // Number of trace-forwards of direct contact that originate at units of the designated type over the course of an iteration
          + ' tocUIndFwd,'  // Number of trace-forwards of indirect contact that originate at units of the designated type over the course of an iteration
          + ' tocUDirBack,'  // Number of trace-backs of direct contact that originate at units of the designated type over the course of an iteration
          + ' tocUIndBack,'  // Number of trace-backs of indirect contact that originate at units of the designated type over the course of an iteration

          // Herd Exams
          + ' exmcUDirFwd,'  // Number of units subjected to a herd exam after a trace forward of direct contact over the course of an iteration
          + ' exmcADirFwd,'  // Number of animals subjected to a herd exam after a trace forward of direct contact over the course of an iteration
          + ' exmcUIndFwd,'  // Number of units subjected to a herd exam after a trace forward of indirect contact over the course of an iteration
          + ' exmcAIndFwd,'  // Number of animals subjected to a herd exam after a trace forward of indirect contact over the course of an iteration
          + ' exmcUDirBack,' // Number of units subjected to a herd exam after a trace back of direct contact over the course of an iteration
          + ' exmcADirBack,' // Number of animals subjected to a herd exam after a trace back of direct contact over the course of an iteration
          + ' exmcUIndBack,' // Number of units subjected to a herd exam after a trace back of indirect contact over the course of an iteration
          + ' exmcAIndBack,' // Number of animals subjected to a herd exam after a trace back of indirect contact over the course of an iteration

          // Diagnostic Testing
          + ' tstcUDirFwd,'  // Number of units subjected to a diagnostic testing after a trace forward of direct contact over the course of an iteration
          + ' tstcADirFwd,'  // Number of animals subjected to a diagnostic testing after a trace forward of direct contact over the course of an iteration
          + ' tstcUIndFwd,'  // Number of units subjected to a diagnostic testing after a trace forward of indirect contact over the course of an iteration
          + ' tstcAIndFwd,'  // Number of animals subjected to a diagnostic testing after a trace forward of indirect contact over the course of an iteration
          + ' tstcUDirBack,' // Number of units subjected to a diagnostic testing after a trace back of direct contact over the course of an iteration
          + ' tstcADirBack,' // Number of animals subjected to a diagnostic testing after a trace back of direct contact over the course of an iteration
          + ' tstcUIndBack,' // Number of units subjected to a diagnostic testing after a trace back of indirect contact over the course of an iteration
          + ' tstcAIndBack,' // Number of animals subjected to a diagnostic testing after a trace back of indirect contact over the course of an iteration
          + ' tstcUTruePos,' // Number of tested units with a true positive diagnostic test result over the course of an iteration
          + ' tstcATruePos,' // Number of animals in tested units with a true positive diagnostic test result over the course of an iteration
          + ' tstcUTrueNeg,' // Number of tested units with a true negative diagnostic test result over the course of an iteration
          + ' tstcATrueNeg,' // Number of animals in tested units with a true negative diagnostic test result over the course of an iteration
          + ' tstcUFalsePos,' // Number of tested units with a false positive diagnostic test result over the course of an iteration
          + ' tstcAFalsePos,' // Number of animals in tested units with a false positive diagnostic test result over the course of an iteration
          + ' tstcUFalseNeg,' // Number of tested units with a false negative diagnostic test result over the course of an iteration
          + ' tstcAFalseNeg,' // Number of animals in tested units with a false negative diagnostic test result over the course of an iteration

          // Destruction
          + ' descUIni,'  // Number of units destroyed prior to the start of the simulation
          + ' descAIni,'  // Total number of animals in units destroyed prior to the start of the simulation
          + ' descUDet,'  // Number of units destroyed because they were detected positive over the course of an iteration
          + ' descADet,'  // Total number of animals in all units destroyed because they were detected positive over the course of an iteration
          + ' descUDirFwd,'  // Number of units destroyed because they were direct traces forward over the course of an iteration
          + ' descADirFwd,'  // Total number of animals in units destroyed because they were direct traces forward over the course of an iteration
          + ' descUIndFwd,'  // Number of units destroyed because they were indirect traces forward over the course of an iteration
          + ' descAIndFwd,'  // Total number of animals in units destroyed because they were indirect traces forward over the course of an iteration
          + ' descUDirBack,'  // Number of units destroyed because they were direct traces backward over the course of an iteration
          + ' descADirBack,'  // Total number of animals in units destroyed because they were direct traces backward over the course of an iteration
          + ' descUIndBack,'  // Number of units destroyed because they were indirect traces backward over the course of an iteration
          + ' descAIndBack,'  // Total number of animals in units destroyed because they were indirect traces backward over the course of an iteration
          + ' descURing,'  // Number of units destroyed because they were in a destruction ring over the course of an iteration
          + ' descARing,'  // Total number of animals in all units destroyed because they were in a destruction ring over the course of an iteration

          //AR:deadDV
          + ' descUDcd,' // Number of units that required only disposal, cleaning, and disinfection over the course of an iteration
          + ' descADcd,' // Total number of animals in all units that required only disposal, cleaning, and disinfection over the course of an iteration

          // Vaccination
          + ' vaccUIni,' // Number of units that were vaccine immune prior to the start of the simulation
          + ' vaccAIni,' // Total number of animals in all units that were vaccine immune prior to the start of the simulation
          + ' vaccURing,'  // Number of units vaccinated in rings around detected-infected units over the course of an iteration
          + ' vaccARing,'  // Total number of animals in all units vaccinated in rings around detected-infected units over the course of an iteration

          // Infection
          + ' infcUIni,'  // Number of units that are initially infected at the beginning of an iteration
          + ' infcAIni,'  // Number of animals in units that are initially infected at the beginning of an iteration
          + ' infcUAll,'  // Number of units that become infected over the course of an iteration
          + ' infcAAll,'  // Number of animals in units that become infected over the course of an iteration

          // Adequate exposure
          + ' adqcUAir,'  // Number of units adequately exposed by airborne spread over the course of an iteration
          + ' adqcAAir,'  // Number of animals in units adequately exposed by airborne spread over the course of an iteration
          + ' adqcUDir,'  // Number of units adequately exposed by direct contact over the course of an iteration
          + ' adqcADir,'  // Number of animals in units adequately exposed by direct contact over the course of an iteration
          + ' adqcUInd,'  // Number of units adequately exposed by indirect contact over the course of an iteration
          + ' adqcAInd,'  // Number of animals in units adequately exposed by indirect contact over the course of an iteration
          + ' adqcULcl,'  // Number of units adequately exposed by local-area spread over the course of an iteration
          + ' adqcALcl,'  // Number of animals in units adequately exposed by local-area spread over the course of an iteration

          + ' zoncFoci' // Number of new zone foci created around units of the indicated type over the course of an iteration

        + ' FROM'
          + ' outDailyByProductionType'
        + ' WHERE'
          + ' iteration = ' + intToStr( lastIteration )
          + ' AND day = ' + intToStr( day )
        + selectedPTIDClause
      ;

      _sqlRes.runQuery( q );

      // The resulting recordset will have one record per selected production type.
      // Sum the appropriate records across all selected production types to generate the data to display.

      // Remember that _data has already been cleared, so it's ready for data.

      row := _sqlRes.fetchArrayFirst();
      while( nil <> row ) do
      	begin
          // Detection
          if( null <> row.field('detcUClin') ) then inc( _data.detcUClin, longint( row.field('detcUClin') ) );
          if( null <> row.field('detcAClin') ) then inc( _data.detcAClin, longint( row.field('detcAClin') ) );
          if( null <> row.field('detcUTest') ) then inc( _data.detcUTest, longint( row.field('detcUTest') ) );
          if( null <> row.field('detcATest') ) then inc( _data.detcATest, longint( row.field('detcATest') ) );

          (*
          AR: See note above with query.

          if( null <> row.field('detcUDead') ) then inc( _data.detcUDead, longint( row.field('detcUDead') ) );
          if( null <> row.field('detcADead') ) then inc( _data.detcADead, longint( row.field('detcADead') ) );

          //AR:deadDV
          if( null <> row.field('detcUDeadD') ) then inc( _data.detcUDeadD, longint( row.field('detcUDeadD') ) );
          if( null <> row.field('detcADeadD') ) then inc( _data.detcADeadD, longint( row.field('detcADeadD') ) );
          if( null <> row.field('detcUDeadV') ) then inc( _data.detcUDeadV, longint( row.field('detcUDeadV') ) );
          if( null <> row.field('detcADeadV') ) then inc( _data.detcADeadV, longint( row.field('detcADeadV') ) );
          *)

          if( null <> row.field('detcUAll') ) then inc( _data.detcUAll, longint( row.field('detcUAll') ) );
          if( null <> row.field('detcAAll') ) then inc( _data.detcAAll, longint( row.field('detcAAll') ) );

          if( null <> row.field('detcUDeadAll') ) then inc( _data.detcUDeadAll, longint( row.field('detcUDeadAll') ) );
          if( null <> row.field('detcADeadAll') ) then inc( _data.detcADeadAll, longint( row.field('detcADeadAll') ) );

          // Tracing
          if( null <> row.field('trcUDirFwd') ) then  inc( _data.trcUDirFwd,  longint( row.field('trcUDirFwd') ) );
          if( null <> row.field('trcADirFwd') ) then  inc( _data.trcADirFwd,  longint( row.field('trcADirFwd') ) );
          if( null <> row.field('trcUIndFwd') ) then  inc( _data.trcUIndFwd,  longint( row.field('trcUIndFwd') ) );
          if( null <> row.field('trcAIndFwd') ) then  inc( _data.trcAIndFwd,  longint( row.field('trcAIndFwd') ) );
          if( null <> row.field('trcUDirpFwd') ) then inc( _data.trcUDirpFwd, longint( row.field('trcUDirpFwd') ) );
          if( null <> row.field('trcADirpFwd') ) then inc( _data.trcADirpFwd, longint( row.field('trcADirpFwd') ) );
          if( null <> row.field('trcUIndpFwd') ) then inc( _data.trcUIndpFwd, longint( row.field('trcUIndpFwd') ) );
          if( null <> row.field('trcAIndpFwd') ) then inc( _data.trcAIndpFwd, longint( row.field('trcAIndpFwd') ) );
          if( null <> row.field('trcUDirBack') ) then  inc( _data.trcUDirBack,  longint( row.field('trcUDirBack') ) );
          if( null <> row.field('trcADirBack') ) then  inc( _data.trcADirBack,  longint( row.field('trcADirBack') ) );
          if( null <> row.field('trcUIndBack') ) then  inc( _data.trcUIndBack,  longint( row.field('trcUIndBack') ) );
          if( null <> row.field('trcAIndBack') ) then  inc( _data.trcAIndBack,  longint( row.field('trcAIndBack') ) );
          if( null <> row.field('trcUDirpBack') ) then inc( _data.trcUDirpBack, longint( row.field('trcUDirpBack') ) );
          if( null <> row.field('trcADirpBack') ) then inc( _data.trcADirpBack, longint( row.field('trcADirpBack') ) );
          if( null <> row.field('trcUIndpBack') ) then inc( _data.trcUIndpBack, longint( row.field('trcUIndpBack') ) );
          if( null <> row.field('trcAIndpBack') ) then inc( _data.trcAIndpBack, longint( row.field('trcAIndpBack') ) );

          // Tracing - Origins
          if( null <> row.field('tocUDirFwd') ) then inc( _data.tocUDirFwd, longint( row.field('tocUDirFwd') ) );
          if( null <> row.field('tocUIndFwd') ) then inc( _data.tocUIndFwd, longint( row.field('tocUIndFwd') ) );
          if( null <> row.field('tocUDirBack') ) then inc( _data.tocUDirBack, longint( row.field('tocUDirBack') ) );
          if( null <> row.field('tocUIndBack') ) then inc( _data.tocUIndBack, longint( row.field('tocUIndBack') ) );

          //Herd Exams
          if( null <> row.field('exmcUDirFwd') ) then inc( _data.exmcUDirFwd, longint( row.field('exmcUDirFwd') ) );
          if( null <> row.field('exmcADirFwd') ) then inc( _data.exmcADirFwd, longint( row.field('exmcADirFwd') ) );
          if( null <> row.field('exmcUIndFwd') ) then inc( _data.exmcUIndFwd, longint( row.field('exmcUIndFwd') ) );
          if( null <> row.field('exmcAIndFwd') ) then inc( _data.exmcAIndFwd, longint( row.field('exmcAIndFwd') ) );
          if( null <> row.field('exmcUDirBack') ) then inc( _data.exmcUDirBack, longint( row.field('exmcUDirBack') ) );
          if( null <> row.field('exmcADirBack') ) then inc( _data.exmcADirBack, longint( row.field('exmcADirBack') ) );
          if( null <> row.field('exmcUIndBack') ) then inc( _data.exmcUIndBack, longint( row.field('exmcUIndBack') ) );
          if( null <> row.field('exmcAIndBack') ) then inc( _data.exmcAIndBack, longint( row.field('exmcAIndBack') ) );

          // Diagnostic Testing
          if( null <> row.field('tstcUDirFwd') ) then inc( _data.tstcUDirFwd, longint( row.field('tstcUDirFwd') ) );
          if( null <> row.field('tstcADirFwd') ) then inc( _data.tstcADirFwd, longint( row.field('tstcADirFwd') ) );
          if( null <> row.field('tstcUIndFwd') ) then inc( _data.tstcUIndFwd, longint( row.field('tstcUIndFwd') ) );
          if( null <> row.field('tstcAIndFwd') ) then inc( _data.tstcAIndFwd, longint( row.field('tstcAIndFwd') ) );
          if( null <> row.field('tstcUDirBack') ) then inc( _data.tstcUDirBack, longint( row.field('tstcUDirBack') ) );
          if( null <> row.field('tstcADirBack') ) then inc( _data.tstcADirBack, longint( row.field('tstcADirBack') ) );
          if( null <> row.field('tstcUIndBack') ) then inc( _data.tstcUIndBack, longint( row.field('tstcUIndBack') ) );
          if( null <> row.field('tstcAIndBack') ) then inc( _data.tstcAIndBack, longint( row.field('tstcAIndBack') ) );
          if( null <> row.field('tstcUTruePos') ) then inc( _data.tstcUTruePos, longint( row.field('tstcUTruePos') ) );
          if( null <> row.field('tstcATruePos') ) then inc( _data.tstcATruePos, longint( row.field('tstcATruePos') ) );
          if( null <> row.field('tstcUTrueNeg') ) then inc( _data.tstcUTrueNeg, longint( row.field('tstcUTrueNeg') ) );
          if( null <> row.field('tstcATrueNeg') ) then inc( _data.tstcATrueNeg, longint( row.field('tstcATrueNeg') ) );
          if( null <> row.field('tstcUFalsePos') ) then inc( _data.tstcUFalsePos, longint( row.field('tstcUFalsePos') ) );
          if( null <> row.field('tstcAFalsePos') ) then inc( _data.tstcAFalsePos, longint( row.field('tstcAFalsePos') ) );
          if( null <> row.field('tstcUFalseNeg') ) then inc( _data.tstcUFalseNeg, longint( row.field('tstcUFalseNeg') ) );
          if( null <> row.field('tstcAFalseNeg') ) then inc( _data.tstcAFalseNeg, longint( row.field('tstcAFalseNeg') ) );

          // Destruction
          if( null <> row.field('descUIni') ) then inc( _data.descUIni, longint( row.field('descUini') ) );
          if( null <> row.field('descAIni') ) then inc( _data.descAIni, longint( row.field('descAIni') ) );
          if( null <> row.field('descUDet') ) then  inc( _data.descUDet,  longint( row.field('descUDet') ) );
          if( null <> row.field('descADet') ) then  inc( _data.descADet,  longint( row.field('descADet') ) );
          if( null <> row.field('descUDirFwd') ) then  inc( _data.descUDirFwd, longint( row.field('descUDirFwd') ) );
          if( null <> row.field('descADirFwd') ) then  inc( _data.descADirFwd, longint( row.field('descADirFwd') ) );
          if( null <> row.field('descUIndFwd') ) then  inc( _data.descUIndFwd, longint( row.field('descUIndFwd') ) );
          if( null <> row.field('descAIndFwd') ) then  inc( _data.descAIndFwd, longint( row.field('descAIndFwd') ) );
          if( null <> row.field('descUDirBack') ) then  inc( _data.descUDirBack, longint( row.field('descUDirBack') ) );
          if( null <> row.field('descADirBack') ) then  inc( _data.descADirBack, longint( row.field('descADirBack') ) );
          if( null <> row.field('descUIndBack') ) then  inc( _data.descUIndBack, longint( row.field('descUIndBack') ) );
          if( null <> row.field('descAIndBack') ) then  inc( _data.descAIndBack, longint( row.field('descAIndBack') ) );
          if( null <> row.field('descURing') ) then inc( _data.descURing, longint( row.field('descURing') ) );
          if( null <> row.field('descARing') ) then inc( _data.descARing, longint( row.field('descARing') ) );

          //AR:deadDV
          if( null <> row.field('descUDcd') ) then inc( _data.descUDcd, longint( row.field('descUDcd') ) );
          if( null <> row.field('descADcd') ) then inc( _data.descADcd, longint( row.field('descADcd') ) );

          // Vaccination
          if( null <> row.field('vaccUIni') ) then inc( _data.vaccUIni, longint( row.field('vaccUIni') ) );
          if( null <> row.field('vaccAIni') ) then inc( _data.vaccAIni, longint( row.field('vaccAIni') ) );
          if( null <> row.field('vaccURing') ) then inc( _data.vaccURing, longint( row.field('vaccURing') ) );
          if( null <> row.field('vaccARing') ) then inc( _data.vaccARing, longint( row.field('vaccARing') ) );

          // Infection
          if( null <> row.field('infcUIni') ) then inc( _data.infcUIni, longint( row.field('infcUIni') ) );
          if( null <> row.field('infcAIni') ) then inc( _data.infcAIni, longint( row.field('infcAIni') ) );
          if( null <> row.field('infcUAll') ) then inc( _data.infcUAll, longint( row.field('infcUAll') ) );
          if( null <> row.field('infcAAll') ) then inc( _data.infcAAll, longint( row.field('infcAAll') ) );

          // Adequate exposure
          if( null <> row.field('adqcUAir') ) then inc( _data.adqcUAir, longint( row.field('adqcUAir') ) );
          if( null <> row.field('adqcAAir') ) then inc( _data.adqcAAir, longint( row.field('adqcAAir') ) );
          if( null <> row.field('adqcUDir') ) then inc( _data.adqcUDir, longint( row.field('adqcUDir') ) );
          if( null <> row.field('adqcADir') ) then inc( _data.adqcADir, longint( row.field('adqcADir') ) );
          if( null <> row.field('adqcUInd') ) then inc( _data.adqcUInd, longint( row.field('adqcUInd') ) );
          if( null <> row.field('adqcAInd') ) then inc( _data.adqcAInd, longint( row.field('adqcAInd') ) );
          if( null <> row.field('adqcULcl') ) then inc( _data.adqcULcl, longint( row.field('adqcULcl') ) );
          if( null <> row.field('adqcALcl') ) then inc( _data.adqcALcl, longint( row.field('adqcALcl') ) );

          // Zone foci
          if( null <> row.field('zoncFoci') ) then inc( _data.zoncFoci, longint( row.field('zoncFoci') ) );

          row := _sqlRes.fetchArrayNext();
        end
      ;

      // Display the data in the grids
      //-------------------------------
      fillGrids();


      // Fetch data for and draw the charts
      //------------------------------------
      // Day '0' is a special case: it includes all units/animals that were initially infected.
      q := 'SELECT SUM( infcUIni ) AS allIniInfected FROM outDailyByProductionType'
        + ' WHERE  iteration = ' + intToStr( lastIteration )
        + selectedPTIDClause
        + ' AND day = 1'
      ;
      _sqlRes.runQuery( q );
      row := _sqlRes.fetchArrayFirst();

      drawSeries( 0, 0, roundDbl( row.field( 'allIniInfected' ) ) );

      // Draw all other days
      q := 'SELECT'
          + ' day,'
          + ' detnUClin,'  // Number of units detected by clinical signs on this day
          + ' detnUTest,'  // Number of units detected by diagnostic testing on this day
          + ' detnUDead,'  // Number of units detected dead from disease on this day
          + ' detnUAll,'  // Number of unique units detected by clinical signs, testing, or detected dead on this day
          + ' infcUIni,'  // Total number of units initially infected
          + ' infnUAll'  // Number of new units infected on this day

        + ' FROM'
          + ' outDailyByProductionType'
        + ' WHERE'
          + ' iteration = ' + intToStr( lastIteration )
        + selectedPTIDClause
        + ' ORDER BY day'
      ;

      _sqlRes.runQuery( q );

      row := _sqlRes.fetchArrayFirst();
      day := row.field('day');

      newDetOnDay := 0;
      newInfOnDay := 0;

      while( nil <> row ) do
      	begin
      		// For each day, determine the numbers of newly detected and newly infected units

          if( row.field('day') <> day ) then
          	begin
            	// Draw the current values...
              drawSeries( day, newDetOnDay, newInfOnDay );

              //...and prep for the next day.
              day := row.field('day');
              newDetOnDay := 0;
              newInfOnDay := 0;
            end
					;

          newDetOnDay := newDetOnDay + row.field('detnUAll');

          newInfOnDay := newInfOnDay + row.field('infnUAll');

          row := _sqlRes.fetchArrayNext();
        end
      ;
    end
  ;


  procedure TFrameEpiIterationSummary.updateForDay( day: integer );
    var
    	ptit: TProductionTypeListIterator;
      selectedPTName: string;

      newInfUOnDay: longint;
      newDetUOnDay: longint;
    begin
      newInfUOnDay := 0;
      newDetUOnDay := 0;
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
            begin
        		  _data.addCumulRecordsFrom( ptit.current().currentOutputs );

              newDetUOnDay := newDetUOnDay + ptit.current().currentOutputs.detnUAll; // unique units, no double-counting of detection mechanisms

              newInfUOnDay := newInfUOnDay + ptit.current().currentOutputs.infnUAll;

              // FIX ME: AR I don't like this "solution" to displaying initial infections.  Think about it some more.
              if( 1 = day ) then
                newInfUOnDay := newInfUOnDay + ptit.current().currentOutputs.infcUIni
              ;

            end
          ;

        	ptit.incr();
        end
      ;
      ptit.Free();

      fillGrids();
      drawSeries( day, newDetUOnDay, newInfUOnDay );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Chart drawing functions
//-----------------------------------------------------------------------------
  procedure TFrameEpiIterationSummary.drawSeries( day: integer; detOnDay, infOnDay: longint );
    var
      _maxVal: double;
    begin
      fraApparent.AddXY( day, detOnDay );
      fraInapparent.AddXY( day, infOnDay );

      _maxVal := fraApparent.series.YValues.MaxValue;
      if ( fraInapparent.series.YValues.MaxValue > _maxVal ) then
        fraApparent.series.GetVertAxis.SetMinMax( fraApparent.series.YValues.MinValue, fraInapparent.series.YValues.MaxValue )
      else
        fraInapparent.series.GetVertAxis.SetMinMax( fraInapparent.series.YValues.MinValue, _maxVal )
      ;
    end
  ;


	procedure TFrameEpiIterationSummary.fillGrids();
		begin
      // The cell array elements are formatted as column, row: [c,r]
      (* !! Adjust the rowCount number of each FrameStringGridBase as new cells are added
         in the hard-coded series below. If this is not done, then additional data rows
         greater than rowCount are ignored and the grid will be truncated in length at runtime.
         rowCount is not a setting for when to invoke a vertical scroll bar,
         but rather the absolute number of rows of the grid. Unsure why exceeding rowCount
         does not raise an exception, rather it silently truncates the grid length.
      *)

      // Detection
      //-----------
      fraSgSurv.rowCount := 20; // remember to update
			fraSgSurv.Cells[1, 0] := intToStr( _data.detcUClin );
			fraSgSurv.Cells[2, 0] := intToStr( _data.detcAClin );
      fraSgSurv.Cells[1, 1] := intToStr( _data.detcUTest );
			fraSgSurv.Cells[2, 1] := intToStr( _data.detcATest );

      //AR:deadDV
      fraSgSurv.Cells[1, 2] := intToStr( _data.detcUDeadAll );
			fraSgSurv.Cells[2, 2] := intToStr( _data.detcADeadAll );

      fraSgSurv.Cells[1, 3] := intToStr( _data.detcUAll );
      fraSgSurv.Cells[2, 3] := intToStr( _data.detcAAll );

      // Tracing
      //--------
			fraSgSurv.Cells[1, 4] := intToStr( _data.trcUDirpFwd ) + ' (' + intToStr( _data.trcUDirFwd ) + ')';
			fraSgSurv.Cells[2, 4] := intToStr( _data.trcADirpFwd ) + ' (' + intToStr( _data.trcADirFwd ) + ')';
			fraSgSurv.Cells[1, 5] := intToStr( _data.trcUIndpFwd ) + ' (' + intToStr( _data.trcUIndFwd ) + ')';
			fraSgSurv.Cells[2, 5] := intToStr( _data.trcAIndpFwd ) + ' (' + intToStr( _data.trcAIndFwd ) + ')';
      fraSgSurv.Cells[1, 6] := intToStr( _data.trcUDirpBack ) + ' (' + intToStr( _data.trcUDirBack ) + ')';
			fraSgSurv.Cells[2, 6] := intToStr( _data.trcADirpBack ) + ' (' + intToStr( _data.trcADirBack ) + ')';
			fraSgSurv.Cells[1, 7] := intToStr( _data.trcUIndpBack ) + ' (' + intToStr( _data.trcUIndBack ) + ')';
			fraSgSurv.Cells[2, 7] := intToStr( _data.trcAIndpBack ) + ' (' + intToStr( _data.trcAIndBack ) + ')';

      // Herd Exam
      //-------------
      fraSgSurv.Cells[1, 8] := intToStr( _data.exmcUDirFwd );
			fraSgSurv.Cells[2, 8] := intToStr( _data.exmcADirFwd );
      fraSgSurv.Cells[1, 9] := intToStr( _data.exmcUIndFwd );
			fraSgSurv.Cells[2, 9] := intToStr( _data.exmcAIndFwd );
      fraSgSurv.Cells[1, 10] := intToStr( _data.exmcUDirBack );
			fraSgSurv.Cells[2, 10] := intToStr( _data.exmcADirBack );
      fraSgSurv.Cells[1, 11] := intToStr( _data.exmcUIndBack );
			fraSgSurv.Cells[2, 11] := intToStr( _data.exmcAIndBack );

      // Diagnostic Testing
      //-------------
      fraSgSurv.Cells[1, 12] := intToStr( _data.tstcUDirFwd );
			fraSgSurv.Cells[2, 12] := intToStr( _data.tstcADirFwd );
      fraSgSurv.Cells[1, 13] := intToStr( _data.tstcUIndFwd );
			fraSgSurv.Cells[2, 13] := intToStr( _data.tstcAIndFwd );
      fraSgSurv.Cells[1, 14] := intToStr( _data.tstcUDirBack );
			fraSgSurv.Cells[2, 14] := intToStr( _data.tstcADirBack );
      fraSgSurv.Cells[1, 15] := intToStr( _data.tstcUIndBack );
			fraSgSurv.Cells[2, 15] := intToStr( _data.tstcAIndBack );
      fraSgSurv.Cells[1, 16] := intToStr( _data.tstcUTruePos );
			fraSgSurv.Cells[2, 16] := intToStr( _data.tstcATruePos );
      fraSgSurv.Cells[1, 17] := intToStr( _data.tstcUTrueNeg );
			fraSgSurv.Cells[2, 17] := intToStr( _data.tstcATrueNeg );
      fraSgSurv.Cells[1, 18] := intToStr( _data.tstcUFalsePos );
			fraSgSurv.Cells[2, 18] := intToStr( _data.tstcAFalsePos );
      fraSgSurv.Cells[1, 19] := intToStr( _data.tstcUFalseNeg );
			fraSgSurv.Cells[2, 19] := intToStr( _data.tstcAFalseNeg );

      // Destruction
      //-------------
      fraSgDestr.rowCount := 9; // remember to update
      fraSgDestr.Cells[1, 0] := intToStr( _data.descUIni );
      fraSgDestr.Cells[2, 0] := intToStr( _data.descAIni );
			fraSgDestr.Cells[1, 1] := intToStr( _data.descUDet );
			fraSgDestr.Cells[2, 1] := intToStr( _data.descADet );
			fraSgDestr.Cells[1, 2] := intToStr( _data.descUDirFwd );
			fraSgDestr.Cells[2, 2] := intToStr( _data.descADirFwd );
			fraSgDestr.Cells[1, 3] := intToStr( _data.descUIndFwd );
			fraSgDestr.Cells[2, 3] := intToStr( _data.descAIndFwd );
      fraSgDestr.Cells[1, 4] := intToStr( _data.descUDirBack );
			fraSgDestr.Cells[2, 4] := intToStr( _data.descADirBack );
			fraSgDestr.Cells[1, 5] := intToStr( _data.descUIndBack );
			fraSgDestr.Cells[2, 5] := intToStr( _data.descAIndBack );
			fraSgDestr.Cells[1, 6] := intToStr( _data.descURing );
			fraSgDestr.Cells[2, 6] := intToStr( _data.descARing );

      //AR:deadDV
      fraSgDestr.Cells[1, 7] := intToStr( _data.descUDcd );
      fraSgDestr.Cells[2, 7] := intToStr( _data.descADcd );

			TotalColumns( fraSgDestr, 0, 7, 8 );

      // Vaccination
      //------------
      fraSgVac.rowCount := 3; // remember to update
      fraSgVac.Cells[1, 0] := intToStr( _data.vaccUIni );
      fraSgVac.Cells[2, 0] := intToStr( _data.vaccAIni );
			fraSgVac.Cells[1, 1] := intToStr( _data.vaccURing );
			fraSgVac.Cells[2, 1] := intToStr( _data.vaccARing );
			TotalColumns( fraSgVac, 0, 1, 2 );

      // Infection
      //-----------
      fraSgInf.rowCount := 7; // remember to update
			fraSgInf.Cells[1, 0] := intToStr( _data.infcUIni );
			fraSgInf.Cells[2, 0] := intToStr( _data.infcAIni );
			fraSgInf.Cells[1, 1] := intToStr( _data.infcUAll );
			fraSgInf.Cells[2, 1] := intToStr( _data.infcAAll );

      // Adequate exposure
      //------------------
			fraSgInf.Cells[1, 2] := intToStr( _data.adqcUDir );
			fraSgInf.Cells[2, 2] := intToStr( _data.adqcADir );
			fraSgInf.Cells[1, 3] := intToStr( _data.adqcUInd );
			fraSgInf.Cells[2, 3] := intToStr( _data.adqcAInd );
			fraSgInf.Cells[1, 4] := intToStr( _data.adqcULcl );
			fraSgInf.Cells[2, 4] := intToStr( _data.adqcALcl );
			fraSgInf.Cells[1, 5] := intToStr( _data.adqcUAir );
			fraSgInf.Cells[2, 5] := intToStr( _data.adqcAAir );
			TotalColumns( fraSgInf, 2, 5, 6 );
      
		end
	;


  procedure TFrameEpiIterationSummary.totalColumns( sg: TFrameStringGridBase; startRow, endRow, totalRow: byte );
    var
      c, r: byte; // Column and row indices
      total: longint;
    begin
      for c := 1 to 2 do
        begin
          total := 0;
          for r := startRow to endRow do
            begin
              total := total + StrToInt( sg.cells[c, r] );
            end
          ;
          sg.cells[c, totalRow] := IntToStr( total );
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Minor helper functions
//-----------------------------------------------------------------------------
  procedure TFrameEpiIterationSummary.FrameResize(Sender: TObject);
    begin
      resizeScrollBox();
      
      // If both charts are visible, split the available space equally.
      if( fraInapparent.Visible and fraApparent.Visible ) then
        begin
          pnlInapparentChart.Height := tabGraphs.Height div 2;
        end

      // If only the actual (upper) chart is visible...
      else if( fraInapparent.Visible and not( fraApparent.Visible ) ) then
        begin
          // The upper panel should take all space except for the height of the lower checkbox
          pnlInapparentChart.Height := tabGraphs.Height - pnlApparent.height;
        end

      // If only the apparent (lower) chart is visible...
      else if( not( fraInapparent.Visible ) and fraApparent.Visible ) then
        begin
          // The upper panel should show only the checkbox
          pnlInapparentChart.Height := pnlInapparent.Height;
          // The lower panel should take all remaining space
          pnlApparentChart.Height := tabGraphs.Height - pnlInapparent.Height;
        end

      // If neither chart is visible...
      else
        begin
          // The upper panel should show only the check box
          pnlInapparentChart.Height := pnlInapparent.Height;
          // The lower panel should show only the check box
          pnlApparentChart.Height := pnlApparent.Height;
        end
      ;
    end
  ;

  
  procedure TFrameEpiIterationSummary.clearGrid( sg: TFrameStringGridBase );
    var
      c, r: integer; // column and row indices
    begin
      for c := 1 to 2 do
        begin
          for r := 0 to ( sg.RowCount - 1 ) do
            sg.cells[c, r] := ''
          ;
        end
      ;
    end
  ;


	procedure TFrameEpiIterationSummary.ClearStringGridSelection( SG: TFrameStringGridBase );
		var
		 tRec: TGridRect;
		begin
			FillChar( tRec, Sizeof(tRec), -1 );
			SG.stgGrid.Selection := tRec;
		end
	;


	procedure TFrameEpiIterationSummary.SizeThePanel( sg: TFrameStringGridBase; P1, P2: TPanel );
		begin
      // Show all the rows if the # of rows is small
      if (sg.RowCount <= 5) then
        P1.Height := P2.Height + (sg.stgGrid.DefaultRowHeight) * sg.RowCount + 8
      else
        // Resize panel to show 5 rows and rely on scroll bar to view additonal rows
        P1.Height := P2.Height + (sg.stgGrid.DefaultRowHeight) * 5 + 8;
		end
	;


	procedure TFrameEpiIterationSummary.resizeScrollBox();
		begin
      //dbcout( 'Resizing scrollbox...' );

{*			sbxSingleIteration.VertScrollBar.Range :=
        pnlSurv.ClientHeight
        + pnlDestr.ClientHeight
        + PanelVac.ClientHeight
        + pnlApparentChart.ClientHeight
        + PanelInf.ClientHeight
        + pnlInapparentChart.ClientHeight
        + 20
      ;*}
		end
	;



	procedure TFrameEpiIterationSummary.reSizeStringGrid( sg: TFrameStringGridBase; PO, PI: TPanel );
		begin
			if( not sg.Visible ) then
				begin
					sg.Hide();
					PO.Height := PI.Height;
				end
			else
				begin
					SizeThePanel( sg, PO, PI );
					sg.Show();
				end
			;
		end
	;


	procedure TFrameEpiIterationSummary.reSizeChart( CF: TFrame; PO, PI : TPanel );
		begin
			if( not CF.Visible ) then
				begin
					CF.Hide();
					PO.Height := PI.Height;
				end
			else
				begin
					PO.Height := CHARTHEIGHT;
					CF.Show();
				end
			;
		end
	;


	procedure TFrameEpiIterationSummary.toggleStringGrid( sg: TFrameStringGridBase; PO, PI: TPanel );
		begin
			if( sg.Visible ) then
				begin
					sg.Hide();
					PO.Height := PI.Height;
				end
			else
				begin
					SizeThePanel( sg, PO, PI );
					sg.Show();
				end
			;
		end
	;


  function TFrameEpiIterationSummary.AllBoxesUnchecked(): boolean;
    begin
      Result := AllTextBoxesUnchecked() and AllChartBoxesUnchecked();
    end
  ;


  function TFrameEpiIterationSummary.AllTextBoxesUnchecked(): boolean;
    begin
      Result := (not cbxSurv.Checked)
        and (not cbxDestr.Checked)
        and (not cbxVac.Checked)
        and (not cbxInf.Checked)
      ;
    end
  ;


  function TFrameEpiIterationSummary.AllChartBoxesUnchecked(): boolean;
    begin
      Result :=
        (not cbxApparent.Checked)
        and (not cbxInapparent.Checked)
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI-handling member functions
//-----------------------------------------------------------------------------
	procedure TFrameEpiIterationSummary.resizeContents();
    var
      baseWidth: integer;
		begin
      inherited resize();

      baseWidth := fraSgSurv.stgGrid.Canvas.TextWidth( tr( 'Diagnostic testing from indirect backward traces ' ) ) + 10;

			fraSgSurv.ColWidths[0] := baseWidth;
			fraSgSurv.ColWidths[1] := (fraSgSurv.ClientWidth - baseWidth + 5) div 2;
			fraSgSurv.ColWidths[2] := (fraSgSurv.ClientWidth - baseWidth + 5) div 2;

			fraSgDestr.ColWidths[0] := baseWidth;
			fraSgDestr.ColWidths[1] := (fraSgDestr.ClientWidth - baseWidth + 5) div 2;
			fraSgDestr.ColWidths[2] := (fraSgDestr.ClientWidth - baseWidth + 5) div 2;

			fraSgVac.ColWidths[0] := baseWidth;
			fraSgVac.ColWidths[1] := (fraSgVac.ClientWidth - baseWidth + 5) div 2;
			fraSgVac.ColWidths[2] := (fraSgVac.ClientWidth - baseWidth + 5) div 2;

			fraSgInf.ColWidths[0] := baseWidth;
			fraSgInf.ColWidths[1] := (fraSgInf.ClientWidth - baseWidth + 5) div 2;
			fraSgInf.ColWidths[2] := (fraSgInf.ClientWidth - baseWidth + 5) div 2;


			sgHeader.ColWidths[0] := baseWidth + 2;
			sgHeader.ColWidths[1] := (fraSgInf.ClientWidth - baseWidth + 2 + 5) div 2;
			sgHeader.ColWidths[2] := (fraSgInf.ClientWidth - baseWidth + 2 + 5) div 2;

      resize();
		end
	;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI event handling
//-----------------------------------------------------------------------------
	procedure TFrameEpiIterationSummary.exitStringGrid(Sender: TObject);
		begin
			ClearStringGridSelection( Sender as TFrameStringGridBase );
		end
	;


	procedure TFrameEpiIterationSummary.cbxSurvClick(Sender: TObject);
		begin
			ToggleStringGrid( fraSgSurv, pnlSurv, pnlSurvHeader );
			resizeScrollBox();
		end
	;


	procedure TFrameEpiIterationSummary.cbxDestrClick(Sender: TObject);
		begin
			ToggleStringGrid( fraSgDestr, pnlDestr, pnlDestrHeader );
			resizeScrollBox();
		end
	;


	procedure TFrameEpiIterationSummary.cbxVacClick(Sender: TObject);
    begin
      ToggleStringGrid( fraSgVac, PanelVac, pnlVacHeader );
      resizeScrollBox();
    end
	;


	procedure TFrameEpiIterationSummary.cbxInfClick(Sender: TObject);
    begin
      ToggleStringGrid( fraSgInf, PanelInf, pnlInfHeader );
      resizeScrollBox();
    end
	;


	procedure TFrameEpiIterationSummary.cbxApparentClick(Sender: TObject);
		begin
      fraApparent.Visible := ( cbChecked = cbxApparent.State );
      resize();
		end
	;


	procedure TFrameEpiIterationSummary.cbxInapparentClick(Sender: TObject);
    begin
      fraInapparent.Visible := ( cbChecked = cbxInapparent.State );
      resize();
    end
	;
//-----------------------------------------------------------------------------





end.
