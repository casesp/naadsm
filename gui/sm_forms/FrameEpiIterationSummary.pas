unit FrameEpiIterationSummary;

(*
FrameEpiIterationSummary.pas/dfm
--------------------------------
Begin: 2005/12/07
Last revision: $Date: 2008/11/25 22:00:31 $ $Author: areeves $
Version: $Revision: 1.25 $
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

    // Delphi units for TChart
		TeeProcs,
		TeEngine,
		Chart,
		Series,

    // General purpose units
    SqlClasses,

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
    FrameStringGridBase, PBPageControl
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

      _data: TSMSimOutByProdType;

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
      procedure setProdType( pt: TProductionType );
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
    GuiStrUtils,
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
      _data := TSMSimOutByProdType.create();

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
          cbxInapparent.Caption := tr( 'Actual Epidemic Curve -- includes all infections' );
          cbxApparent.Caption := tr( 'Apparent Epidemic Curve -- includes only detected infections' );
          tabTables.Caption := tr( 'Tabular view' );
          cbxInf.Caption := tr( 'Reasons for infection -- includes all infections' );
          cbxVac.Caption := tr( 'Vaccination' );
          lblAsterisk.Caption := tr( '* In the course of a simulation run, these activities may occur more than once on a single unit' );
          cbxDestr.Caption := tr( 'Destruction' );
          cbxSurv.Caption := tr( 'Detection and Tracing' );
        end
      ;

      // Set TChart properties
      with self do
        begin
          fraInapparent.chtCurve.Title.Text.Strings[0] := tr( 'Actual Epidemic Curve' );
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
        _currentItr := newItr;

			setupGrids();

      // Set up the reusable SQL query object
      if( nil <> _sqlRes ) then _sqlRes.Free();
      db2 := _smdb as TSqlDatabase;
      _sqlRes := TSqlResult.Create( db2 );

      setProdType( pt );

      setPanelSizes();
      resizeScrollBox();
    end
  ;


  procedure TFrameEpiIterationSummary.setProdType( pt: TProductionType );
    begin
      resizeContents();
      
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
  procedure TFrameEpiIterationSummary.SetupGrids();
    begin
      sgHeader.Cells[1,0] := tr( 'Cumulative number of units' );
      sgHeader.Cells[2,0] := tr( 'Cumulative number of animals' );

      clearGrid( fraSgSurv );
      fraSgSurv.Cells[0,0] := tr( 'Clinical detections*' );
      fraSgSurv.Cells[0,1] := tr( 'Direct traces (successfully traced)*' );
      fraSgSurv.Cells[0,2] := tr( 'Indirect traces (successfully traced)*' );

      clearGrid( fraSgDestr );
      frasgDestr.Cells[0,0] := tr( 'Initially destroyed' );
      fraSgDestr.Cells[0,1] := tr( 'Detection' );
      fraSgDestr.Cells[0,2] := tr( 'Direct traces' );
      fraSgDestr.Cells[0,3] := tr( 'Indirect traces' );
      fraSgDestr.Cells[0,4] := tr( 'Ring' ); // FIX ME: dynamically change radius, depending on production type? //'Circle (3 km)';
      fraSgDestr.Cells[0,5] := tr( 'TOTAL' );

      clearGrid( fraSgVac );
      fraSgVac.Cells[0,0] := tr( 'Initially vaccinated' );
      fraSgVac.Cells[0,1] :=  tr( 'Ring*' ); // FIX ME: dynamically change radius, depending on production type? //'Ring (5 km)*';
      // Total is the same as the number of ring vaccinations, for now.
      //fraSgVac.Cells[0,2] := 'TOTAL';

      clearGrid( fraSgInf );
      fraSgInf.Cells[0,0] := tr( 'Initially infected' );
      fraSgInf.Cells[0,1] := tr( 'Airborne*' );
      fraSgInf.Cells[0,2] := tr( 'Direct contact*' );
      fraSgInf.Cells[0,3] := tr( 'Indirect contact*' );
      fraSgInf.Cells[0,4] := tr( 'TOTAL' );

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
        begin
          lastIteration := Itr;
        end;


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

        	// Tracing
          + ' trcUDir,'  // Number of units directly exposed and successfully traced over the course of an iteration
          + ' trcADir,'  // Total number of animals in all units directly exposed and successfully traced over the course of an iteration
          + ' trcUInd,'  // Number of units indirectly exposed and successfully traced over the course of an iteration
          + ' trcAInd,'  // Total number of animals in all units indirectly exposed and successfully traced over the course of an iteration
          + ' trcUDirp,'  // Number of units directly exposed that could possibly have been traced over the course of an iteration
          + ' trcADirp,'  // Total number of animals in all units directly exposed that could possibly have been traced over the course of an iteration
          + ' trcUIndp,'  // Number of units indirectly exposed that could possibly have been traced over the course of an iteration
          + ' trcAIndp,'  // Total number of animals in units indirectly exposed that could possibly have been traced over the course of an iteration

          // Destruction
          + ' descUIni,'  // Number of units destroyed prior to the start of the simulation
          + ' descAIni,'  // Total number of animals in units destroyed prior to the start of the simulation
          + ' descUDet,'  // Number of units destroyed because they were detected positive over the course of an iteration
          + ' descADet,'  // Total number of animals in all units destroyed because they were detected positive over the course of an iteration
          + ' descUDir,'  // Number of units destroyed because they were direct traces over the course of an iteration
          + ' descADir,'  // Total number of animals in units destroyed because they were direct traces over the course of an iteration
          + ' descUInd,'  // Number of units destroyed because they were indirect traces over the course of an iteration
          + ' descAInd,'  // Total number of animals in units destroyed because they were indirect traces over the course of an iteration
          + ' descURing,'  // Number of units destroyed because they were in a destruction ring over the course of an iteration
          + ' descARing,'  // Total number of animals in all units destroyed because they were in a destruction ring over the course of an iteration

          // Vaccination
          + ' vaccUIni,' // Number of units that were vaccine immune prior to the start of the simulation
          + ' vaccAIni,' // Total number of animals in all units that were vaccine immune prior to the start of the simulation
          + ' vaccURing,'  // Number of units vaccinated in rings around detected-infected units over the course of an iteration
          + ' vaccARing,'  // Total number of animals in all units vaccinated in rings around detected-infected units over the course of an iteration

          // Infection
          + ' infcUIni,'  // Number of units that are initially infected at the beginning of an iteration
          + ' infcAIni,'  // Number of animals in units that are initially infected at the beginning of an iteration
          + ' infcUAir,'  // Number of units that become infected by airborne spread over the course of an iteration
          + ' infcAAir,'  // Number of animals in units that become infected by airborne spread over the course of an iteration
          + ' infcUDir,'  // Number of units that become infected by direct contact over the course of an iteration
          + ' infcADir,'  // Number of animals in units that become infected by direct contact over the course of an iteration
          + ' infcUInd,'  // Number of units that become infected by indirect contact over the course of an iteration
          + ' infcAInd,'  // Number of animals in units that become infected by indirect contact over the course of an iteration

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
          // Detection by clinical signs
          if( null <> row.field('detcUClin') ) then inc( _data.detcUClin, longint( row.field('detcUClin') ) );
          if( null <> row.field('detcAClin') ) then inc( _data.detcAClin, longint( row.field('detcAClin') ) );

          // Tracing
          if( null <> row.field('trcUDir') ) then  inc( _data.trcUDir,  longint( row.field('trcUDir') ) );
          if( null <> row.field('trcADir') ) then  inc( _data.trcADir,  longint( row.field('trcADir') ) );
          if( null <> row.field('trcUInd') ) then  inc( _data.trcUInd,  longint( row.field('trcUInd') ) );
          if( null <> row.field('trcAInd') ) then  inc( _data.trcAInd,  longint( row.field('trcAInd') ) );
          if( null <> row.field('trcUDirp') ) then inc( _data.trcUDirp, longint( row.field('trcUDirp') ) );
          if( null <> row.field('trcADirp') ) then inc( _data.trcADirp, longint( row.field('trcADirp') ) );
          if( null <> row.field('trcUIndp') ) then inc( _data.trcUIndp, longint( row.field('trcUIndp') ) );
          if( null <> row.field('trcAIndp') ) then inc( _data.trcAIndp, longint( row.field('trcAIndp') ) );

          // Destruction
          if( null <> row.field('descUIni') ) then inc( _data.descUIni, longint( row.field('descUini') ) );
          if( null <> row.field('descAIni') ) then inc( _data.descAIni, longint( row.field('descAIni') ) );
          if( null <> row.field('descUDet') ) then  inc( _data.descUDet,  longint( row.field('descUDet') ) );
          if( null <> row.field('descADet') ) then  inc( _data.descADet,  longint( row.field('descADet') ) );
          if( null <> row.field('descUDir') ) then  inc( _data.descUDir,  longint( row.field('descUDir') ) );
          if( null <> row.field('descADir') ) then  inc( _data.descADir,  longint( row.field('descADir') ) );
          if( null <> row.field('descUInd') ) then  inc( _data.descUInd,  longint( row.field('descUInd') ) );
          if( null <> row.field('descAInd') ) then  inc( _data.descAInd,  longint( row.field('descAInd') ) );
          if( null <> row.field('descURing') ) then inc( _data.descURing, longint( row.field('descURing') ) );
          if( null <> row.field('descARing') ) then inc( _data.descARing, longint( row.field('descARing') ) );

          // Vaccination
          if( null <> row.field('vaccUIni') ) then inc( _data.vaccUIni, longint( row.field('vaccUIni') ) );
          if( null <> row.field('vaccAIni') ) then inc( _data.vaccAIni, longint( row.field('vaccAIni') ) );
          if( null <> row.field('vaccURing') ) then inc( _data.vaccURing, longint( row.field('vaccURing') ) );
          if( null <> row.field('vaccARing') ) then inc( _data.vaccARing, longint( row.field('vaccARing') ) );

          // Infection
          if( null <> row.field('infcUIni') ) then inc( _data.infcUIni, longint( row.field('infcUIni') ) );
          if( null <> row.field('infcAIni') ) then inc( _data.infcAIni, longint( row.field('infcAIni') ) );
          if( null <> row.field('infcUAir') ) then inc( _data.infcUAir, longint( row.field('infcUAir') ) );
          if( null <> row.field('infcAAir') ) then inc( _data.infcAAir, longint( row.field('infcAAir') ) );
          if( null <> row.field('infcUDir') ) then inc( _data.infcUDir, longint( row.field('infcUDir') ) );
          if( null <> row.field('infcADir') ) then inc( _data.infcADir, longint( row.field('infcADir') ) );
          if( null <> row.field('infcUInd') ) then inc( _data.infcUInd, longint( row.field('infcUInd') ) );
          if( null <> row.field('infcAInd') ) then inc( _data.infcAInd, longint( row.field('infcAInd') ) );

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
      q := 'SELECT'
          + ' day,'
          + ' detnUClin,'  // Number of units detected by clinical signs on this day
          + ' infcUIni,'  // Total number of units initially infected
          + ' infnUAir,'  // Number of new units infected on this day by airborne spread
          + ' infnUDir,'  // Number of new units infected on this day by direct contact
          + ' infnUInd'  // Number of new units infected on this day by indirect contact
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

      // Day '0' is a special case: it includes all units/animals that were initially infected
      if( nil <> row ) then
        drawSeries( 0, 0, row.field( 'infcUIni' ) )
      ;

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

          newDetOnDay := newDetOnDay + row.field('detnUClin');

          newInfOnDay := newInfOnDay
            + row.field('infnUAir')
            + row.field('infnUDir')
            + row.field('infnUInd')
          ;

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

              newDetUOnDay := newDetUOnDay + ptit.current().currentOutputs.detnUClin;

              newInfUOnDay := newInfUOnDay
                + ptit.current().currentOutputs.infnUAir
                + ptit.current().currentOutputs.infnUDir
                + ptit.current().currentOutputs.infnUInd
              ;

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
      // Detection
      //-----------
			fraSgSurv.Cells[1, 0] := intToStr( _data.detcUClin );
			fraSgSurv.Cells[2, 0] := intToStr( _data.detcAClin  );


      // Tracing
      //--------
			fraSgSurv.Cells[1, 1] :=
				intToStr( _data.trcUDirp )
				+ ' ('
				+ intToStr( _data.trcUDir )
				+ ')'
			;

			fraSgSurv.Cells[2, 1] :=
				intToStr( _data.trcADirp )
				+ ' ('
				+ intToStr( _data.trcADir )
				+ ')'
			;

			fraSgSurv.Cells[1, 2] :=
				intToStr( _data.trcUIndp )
				+ ' ('
				+ intToStr( _data.trcUInd )
				+ ')'
			;

			fraSgSurv.Cells[2, 2] :=
				intToStr( _data.trcAIndp )
				+ ' ('
				+ intToStr( _data.trcAInd )
				+ ')'
			;

      // Destruction
      //-------------
      fraSgDestr.Cells[1, 0] := intToStr( _data.descUIni );
      fraSgDestr.Cells[2, 0] := intToStr( _data.descAIni );
			fraSgDestr.Cells[1, 1] := intToStr( _data.descUDet );
			fraSgDestr.Cells[2, 1] := intToStr( _data.descADet );
			fraSgDestr.Cells[1, 2] := intToStr( _data.descUDir );
			fraSgDestr.Cells[2, 2] := intToStr( _data.descADir );
			fraSgDestr.Cells[1, 3] := intToStr( _data.descUInd );
			fraSgDestr.Cells[2, 3] := intToStr( _data.descAInd );
			fraSgDestr.Cells[1, 4] := intToStr( _data.descURing );
			fraSgDestr.Cells[2, 4] := intToStr( _data.descARing );
			TotalColumns( fraSgDestr, 0, 4, 5 );


      // Vaccination
      //------------
      fraSgVac.Cells[1, 0] := intToStr( _data.vaccUIni );
      fraSgVac.Cells[2, 0] := intToStr( _data.vaccAIni );
			fraSgVac.Cells[1, 1] := intToStr( _data.vaccURing );
			fraSgVac.Cells[2, 1] := intToStr( _data.vaccARing );

			//TotalColumns( fraSgVac, 0, 1, 2 ); // There's nothing to total for vaccination at the moment


      // Infection
      //-----------
			fraSgInf.Cells[1, 0] := intToStr( _data.infcUIni );
			fraSgInf.Cells[2, 0] := intToStr( _data.infcAIni );
			fraSgInf.Cells[1, 1] := intToStr( _data.infcUAir );
			fraSgInf.Cells[2, 1] := intToStr( _data.infcAAir );
			fraSgInf.Cells[1, 2] := intToStr( _data.infcUDir );
			fraSgInf.Cells[2, 2] := intToStr( _data.infcADir );
			fraSgInf.Cells[1, 3] := intToStr( _data.infcUInd );
			fraSgInf.Cells[2, 3] := intToStr( _data.infcAInd );
			TotalColumns( fraSgInf, 0, 3, 4 );
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
			P1.Height := P2.Height + (sg.stgGrid.DefaultRowHeight) * sg.RowCount + 8;
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

      baseWidth := round( 195 * screen.PixelsPerInch / 96 );

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
