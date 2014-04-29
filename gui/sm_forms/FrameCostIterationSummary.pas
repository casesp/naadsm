unit FrameCostIterationSummary;

(*
FrameCostIterationSummary.pas/dfm
---------------------------------
Begin: 2005/01/23
Last revision: $Date: 2013-06-27 19:11:30 $ $Author: areeves $
Version number: $Revision: 1.22.4.2 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2006 - 2011 Colorado State University

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
    ExtCtrls,
    StdCtrls,
    ComCtrls,

    // Custom Delphi controls
    PBPageControl,

    // General purpose units
    QVectors,
    SqlClasses,

    // Application-specific data structures
    SMDatabase,
    SMSimulationInput,
    ProductionType,
    ProductionTypeList,

    // Application-specific widgets
    FrameSingleCostTable,
    FrameSingleCostCurve,
    FrameChartBase
  ;

  type TFrameCostIterationSummary = class( TFrame )
      pnlTable: TPanel;

      pnlChart: TPanel;
      fraChart: TFrameSingleCostCurve;
      pnlTableFrameContainer: TPanel;
      fraTable: TFrameSingleCostTable;
      pbpGraphTableTabs: TPBPageControl;
      tabGraph: TTabSheet;
      tabTable: TTabSheet;

      procedure cbxShowTableClick(Sender: TObject);
      procedure cbxShowChartClick(Sender: TObject);
      procedure Splitter1CanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
      procedure Splitter1Moved(Sender: TObject);

    protected
      _smSim: TSMSimulationInput;
      _smdb: TSMDatabase;
      _selectedPT: TProductionType;

      _sqlRes: TSqlResult;

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

      _currentItr: Integer;

      procedure translateUI();
      procedure translateUIManual();

      { Handles form updates from the database when a simulation is not running or when _selectedPT is changed }
      procedure setUpFromDatabase( Itr: Integer );

      procedure resetArrays( len: integer );

      procedure calculateDestrCostsForDay( pt: TProductionType; const day, units, animals: integer );

      procedure calculateVaccCostsForDay(
        pt: TProductionType;
        const day: integer;
        const units: integer;
        const animals: integer;
        var allAnimalsVacc: integer
      );

      procedure buildCumulArrays();
      procedure incrementArraySizes();
      procedure updateCumulArraysForDay( const day: integer );

      function getTableIsVisible(): boolean;
      function getChartIsVisible(): boolean;

      procedure resizeContents();

    public
      constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

      procedure resetSim( db: TSMDatabase; sim: TSMSimulationInput; pt: TProductionType; newItr: Integer = -1 );
      procedure setProdType( pt: TProductionType; const simIsRunning: boolean );

      procedure reset();

      { Handles chart updates while a simulation is in progress }
      procedure updateForDay( day: integer );      

      property tableIsVisible: boolean read getTableIsVisible;
      property chartIsVisible: boolean read getChartIsVisible;
    end
  ;

  const
    DBFRAMECOSTITERATIONSUMMARY: boolean = false; // Set to true to enable debugging messages for this unit

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
  constructor TFrameCostIterationSummary.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();

      dbcout( ' TFrameCostIterationSummary.Create...', DBFRAMECOSTITERATIONSUMMARY );
      _arrTotal := TQDoubleVector.Create();
      _arrDestrSubtotal := TQDoubleVector.Create();
      _arrAppraisal := TQDoubleVector.Create();
      _arrCAndD := TQDoubleVector.Create();
      _arrEuthanasia := TQDoubleVector.Create();
      _arrIndemnification := TQDoubleVector.Create();
      _arrDisposal := TQDoubleVector.Create();
      _arrVaccSetup := TQDoubleVector.Create();
      _arrVacc := TQDoubleVector.Create();
      _arrVaccSubtotal := TQDoubleVector.Create();

      _arrTotalCumul := TQDoubleVector.Create();
      _arrDestrSubtotalCumul := TQDoubleVector.Create();
      _arrAppraisalCumul := TQDoubleVector.Create();
      _arrCAndDCumul := TQDoubleVector.Create();
      _arrEuthanasiaCumul := TQDoubleVector.Create();
      _arrIndemnificationCumul := TQDoubleVector.Create();
      _arrDisposalCumul := TQDoubleVector.Create();
      _arrVaccSetupCumul := TQDoubleVector.Create();
      _arrVaccCumul := TQDoubleVector.Create();
      _arrVaccSubtotalCumul := TQDoubleVector.Create();

      _currentItr := -1;
      
      _smSim := nil;
      _smdb := nil;
      _selectedPT := nil;

      _sqlRes := nil;

      fraChart.setArrays(
        _arrTotal,

        _arrDestrSubtotal,
        _arrAppraisal,
        _arrCAndD,
        _arrEuthanasia,
        _arrIndemnification,
        _arrDisposal,
        _arrVaccSetup,
        _arrVacc,
        _arrVaccSubtotal,

        _arrTotalCumul,
        _arrDestrSubtotalCumul,
        _arrAppraisalCumul,
        _arrCAndDCumul,
        _arrEuthanasiaCumul,
        _arrIndemnificationCumul,
        _arrDisposalCumul,
        _arrVaccSetupCumul,
        _arrVaccCumul,
        _arrVaccSubtotalCumul
      );

      fraTable.setArrays(
        _arrTotal,

        _arrDestrSubtotal,
        _arrAppraisal,
        _arrCAndD,
        _arrEuthanasia,
        _arrIndemnification,
        _arrDisposal,
        _arrVaccSetup,
        _arrVacc,
        _arrVaccSubtotal,

        _arrTotalCumul,
        _arrDestrSubtotalCumul,
        _arrAppraisalCumul,
        _arrCAndDCumul,
        _arrEuthanasiaCumul,
        _arrIndemnificationCumul,
        _arrDisposalCumul,
        _arrVaccSetupCumul,
        _arrVaccCumul,
        _arrVaccSubtotalCumul
      );
      
      dbcout( 'Done  TFrameCostIterationSummary.create', DBFRAMECOSTITERATIONSUMMARY );
    end
  ;


  procedure TFrameCostIterationSummary.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.2.
      // Generation date: Wed Mar 12 16:13:44 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-HEAD/sm_forms/FrameCostIterationSummary.dfm
      // File date: Wed Mar 12 16:07:55 2008

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          tabGraph.Caption := tr( 'Graphical view' );
          tabTable.Caption := tr( 'Tabular view' );
        end
      ;

      // If any phrases are found that could not be automatically extracted by
      // Caption Collector, modify the following function to take care of them.
      translateUIManual();
    end
  ;


  procedure TFrameCostIterationSummary.translateUIManual();
    begin
      tabGraph.Caption := ' ' + tabGraph.Caption;
      tabTable.Caption := ' ' + tabTable.Caption;
    end
  ;


  destructor TFrameCostIterationSummary.destroy();
    begin
      _arrTotal.Free();
      _arrDestrSubtotal.Free();
      _arrAppraisal.Free();
      _arrCAndD.Free();
      _arrEuthanasia.Free();
      _arrIndemnification.Free();
      _arrDisposal.Free();
      _arrVaccSetup.Free();
      _arrVacc.Free();
      _arrVaccSubtotal.Free();

      _arrTotalCumul.Free();
      _arrDestrSubtotalCumul.Free();
      _arrAppraisalCumul.Free();
      _arrCAndDCumul.Free();
      _arrEuthanasiaCumul.Free();
      _arrIndemnificationCumul.Free();
      _arrDisposalCumul.Free();
      _arrVaccSetupCumul.Free();
      _arrVaccCumul.Free();
      _arrVaccSubtotalCumul.Free();

      freeAndNil( _sqlRes );

      inherited destroy();
    end
  ;



	procedure TFrameCostIterationSummary.resetSim( db: TSMDatabase; sim: TSMSimulationInput; pt: TProductionType; newItr: Integer = -1);
  	var
    	db2: TSqlDatabase;
  	begin
      dbcout( 'TFrameCostIterationSummary.resetSim...', DBFRAMECOSTITERATIONSUMMARY );

   		_smSim := sim;
      _smdb := db;
      _selectedPT := pt;
      if ( newItr > 0 ) then
        _currentItr := newItr;

      if( true = _smSim.includeCostsGlobal ) then
        begin
          // Set up the reusable SQL query object
          if( nil <> _sqlRes ) then _sqlRes.Free();
          db2 := _smdb as TSqlDatabase;
          _sqlRes := TSqlResult.Create( db2 );

          setProdType( pt, false );
        end
      ;

      dbcout( 'Done TFrameCostIterationSummary.resetSim', DBFRAMECOSTITERATIONSUMMARY );
    end
  ;


  procedure TFrameCostIterationSummary.setProdType( pt: TProductionType; const simIsRunning: boolean );
    begin
      dbcout( 'TFrameCostIterationSummary.setProdType...', DBFRAMECOSTITERATIONSUMMARY );
      _selectedPT := pt;

      if( simIsRunning ) then
        _currentItr := -1
      ;

			setUpFromDatabase( _currentItr );
      dbcout( 'Done TFrameCostIterationSummary.setProdType', DBFRAMECOSTITERATIONSUMMARY );
    end
  ;


  procedure TFrameCostIterationSummary.reset();
    begin
      resetArrays( 0 );

      fraChart.clearAllSeries();
      fraTable.clearTable();
    end
  ;


//-----------------------------------------------------------------------------
// Form updating functions
//-----------------------------------------------------------------------------
  procedure TFrameCostIterationSummary.setUpFromDatabase( Itr: Integer );
    var
      row: TSqlRow;
      q: string;

      lastIteration: integer;
      day: integer;
      selectedPTIDClause: string;

      currentPTID: integer;
      currentPT: TProductionType;

      destrUnitCount, destrAnimalCount: integer;
      vaccUnitCount, vaccAnimalCount: integer;
      animalsAlreadyVaccinated: longint;
    begin
      dbcout( 'TFrameCostIterationSummary.setUpFromDatabase...', DBFRAMECOSTITERATIONSUMMARY );

      if( false = _smSim.includeCostsGlobal ) then exit;

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
          	lastIteration := row.field('maxIt')
          ;
        end
      else
        begin
           lastIteration := Itr;
        end;

      dbcout( 'lastIteration: ' + intToStr( lastIteration ), DBFRAMECOSTITERATIONSUMMARY );

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

      dbcout( 'day: ' + intToStr( day ), DBFRAMECOSTITERATIONSUMMARY );

      // Determine the ID of the currently selected production type
      //-----------------------------------------------------------
      if( nil = _selectedPT ) then
      	selectedPTIDClause := ''
      else
      	selectedPTIDClause := ' AND productionTypeID = ' + intToStr( _selectedPT.productionTypeID )
      ;



      // select NEW COUNT data for indicated pts, last day, last/current iteration
      //--------------------------------------------------------------------------
      q :=
      	'SELECT'
          + ' productionTypeID,'
          + ' day,'

          // New daily counts for destruction for any reason
          + ' desnUAll,'
          + ' desnAAll,'

          // New daily counts for vaccination for any reason
          + ' vacnUAll,'
          + ' vacnAAll'
        + ' FROM'
          + ' outDailyByProductionType'
        + ' WHERE'
          + ' iteration = ' + intToStr( lastIteration )
        + selectedPTIDClause
        + ' ORDER BY productionTypeID, day'
      ;

      _sqlRes.runQuery( q );

      // The resulting recordset will have one record per selected production type.
      // Sum the appropriate records across all selected production types to generate the data to display.


      // Build the daily arrays
      //-----------------------
      resetArrays( day );

      currentPTID := -1;
      currentPT := nil;
      animalsAlreadyVaccinated := 0;

      row := _sqlRes.fetchArrayFirst();
      while( nil <> row ) do
      	begin
          if( null = row.field('productionTypeID') ) then
            raise exception.Create( 'ProductionTypeID is unspecified in TFrameSingleCostCurve.setUpFromDatabase' )
          ;

          if( row.field('productionTypeID') <> currentPTID ) then
            begin
              currentPTID := row.field('productionTypeID');
              currentPT := _smSim.ptList.findProdType( currentPTID );
              animalsAlreadyVaccinated := 0;
            end
          ;

          if( nil = currentPT ) then
            raise exception( 'Production type ' + intToStr( currentPTID ) + ' not found in TFrameSingleCostCurve.setUpFromDatabase' )
          ;

          if( null = row.field('day') ) then
            raise exception.Create( 'Day is unspecified in TFrameSingleCostCurve.setUpFromDatabase' )
          ;

          if( null <> row.field('desnUAll') ) then
            destrUnitCount := row.field('desnUAll')
          else
            destrUnitCount := 0
          ;

          if( null <> row.field('desnAAll') ) then
            destrAnimalCount := row.field('desnAAll')
          else
            destrAnimalCount := 0
          ;

          if( null <> row.field('vacnUAll') ) then
            vaccUnitCount := row.field('vacnUAll')
          else
            vaccUnitCount := 0
          ;

          if( null <> row.field('vacnAAll') ) then
            vaccAnimalCount := row.field('vacnAAll')
          else
            vaccAnimalCount := 0
          ;

          // Calculate daily costs, if needed
          //---------------------------------
          if( _smSim.costTrackDestruction ) then
            calculateDestrCostsForDay( currentPT, row.field('day'), destrUnitCount, destrAnimalCount )
          ;

          if( _smSim.costTrackVaccination ) then
            calculateVaccCostsForDay( currentPT, row.field('day'), vaccUnitCount, vaccAnimalCount, animalsAlreadyVaccinated )
          ;

          row := _sqlRes.fetchArrayNext();
        end
      ;


      // Make the cumulative arrays
      //---------------------------
      buildCumulArrays();


      // Now draw the charts!
      //---------------------
      fraChart.redrawAllSeries();
      fraTable.refillTable();

      dbcout( 'Done TFrameCostIterationSummary.setUpFromDatabase', DBFRAMECOSTITERATIONSUMMARY );
    end
  ;


  procedure TFrameCostIterationSummary.updateForDay( day: integer );
    var
    	ptit: TProductionTypeListIterator;
      selectedPTName: string;

      animalsAlreadyVaccinated: longint;
    begin
      if( false = _smSim.includeCostsGlobal ) then exit;

      incrementArraySizes();

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
              animalsAlreadyVaccinated := ptit.current().currentOutputs.vaccAAll - ptit.current().currentOutputs.vacnAAll;
              calculateDestrCostsForDay( ptit.current(), day, ptit.current().currentOutputs.desnUAll, ptit.current().currentOutputs.desnAAll );
              calculateVaccCostsForDay( ptit.current(), day, ptit.current().currentOutputs.vacnUAll, ptit.current().currentOutputs.vacnAAll, animalsAlreadyVaccinated );
              updateCumulArraysForDay( day );
            end
          ;

        	ptit.incr();
        end
      ;
      ptit.Free();

      fraChart.updateForDay( day );
      fraTable.updateForDay( day );
    end
  ;


  procedure TFrameCostIterationSummary.resetArrays( len: integer );
    begin
      dbcout( 'TFrameCostIterationSummary.resetArrays...', DBFRAMECOSTITERATIONSUMMARY );

      _arrTotal.Clear();
      _arrDestrSubtotal.Clear();
      _arrAppraisal.Clear();
      _arrCAndD.Clear();
      _arrEuthanasia.Clear();
      _arrIndemnification.Clear();
      _arrDisposal.Clear();
      _arrVaccSetup.Clear();
      _arrVacc.Clear();
      _arrVaccSubtotal.Clear();

      _arrTotalCumul.Clear();
      _arrDestrSubtotalCumul.Clear();
      _arrAppraisalCumul.Clear();
      _arrCAndDCumul.Clear();
      _arrEuthanasiaCumul.Clear();
      _arrIndemnificationCumul.Clear();
      _arrDisposalCumul.Clear();
      _arrVaccSetupCumul.Clear();
      _arrVaccCumul.Clear();
      _arrVaccSubtotalCumul.Clear();

      dbcout( 'Specified array length: ' + intToStr( len ), DBFRAMECOSTITERATIONSUMMARY );

      if( -1 < len ) then
        begin
          _arrTotal.resize( len );
          _arrDestrSubtotal.resize( len );
          _arrAppraisal.resize( len );
          _arrCAndD.resize( len );
          _arrEuthanasia.resize( len );
          _arrIndemnification.resize( len );
          _arrDisposal.resize( len );
          _arrVaccSetup.resize( len );
          _arrVacc.resize( len );
          _arrVaccSubtotal.resize( len );

          _arrTotalCumul.resize( len );
          _arrDestrSubtotalCumul.resize( len );
          _arrAppraisalCumul.resize( len );
          _arrCAndDCumul.resize( len );
          _arrEuthanasiaCumul.resize( len );
          _arrIndemnificationCumul.resize( len );
          _arrDisposalCumul.resize( len );
          _arrVaccSetupCumul.resize( len );
          _arrVaccCumul.resize( len );
          _arrVaccSubtotalCumul.resize( len );

          _arrTotal.Fill( 0.0 );
          _arrDestrSubtotal.Fill( 0.0 );
          _arrAppraisal.Fill( 0.0 );
          _arrCAndD.Fill( 0.0 );
          _arrEuthanasia.Fill( 0.0 );
          _arrIndemnification.Fill( 0.0 );
          _arrDisposal.Fill( 0.0 );
          _arrVaccSetup.Fill( 0.0 );
          _arrVacc.Fill( 0.0 );
          _arrVaccSubtotal.Fill( 0.0 );

          _arrTotalCumul.Fill( 0.0 );
          _arrDestrSubtotalCumul.Fill( 0.0 );
          _arrAppraisalCumul.Fill( 0.0 );
          _arrCAndDCumul.Fill( 0.0 );
          _arrEuthanasiaCumul.Fill( 0.0 );
          _arrIndemnificationCumul.Fill( 0.0 );
          _arrDisposalCumul.Fill( 0.0 );
          _arrVaccSetupCumul.Fill( 0.0 );
          _arrVaccCumul.Fill( 0.0 );
          _arrVaccSubtotalCumul.Fill( 0.0 );
        end
      ;

      dbcout( 'Done TFrameCostIterationSummary.resetArrays', DBFRAMECOSTITERATIONSUMMARY );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Cost calculations
//-----------------------------------------------------------------------------
  procedure TFrameCostIterationSummary.calculateDestrCostsForDay( pt: TProductionType; const day, units, animals: integer );
    var
      arrPos: integer;
      val: double;
    begin
      dbcout( 'TFrameCostIterationSummary.calculateDestrCostsForDay...', DBFRAMECOSTITERATIONSUMMARY );

      arrPos := day - 1;

      dbcout( 'arrPos: ' + intToStr( arrPos ), DBFRAMECOSTITERATIONSUMMARY );
      dbcout( '_arrAppraisal.count: ' + intToStr( _arrAppraisal.count ), DBFRAMECOSTITERATIONSUMMARY );

      val := pt.costParams.destrAppraisalCosts( units );;
      _arrAppraisal[arrPos] := _arrAppraisal[arrPos] + val;
      _arrDestrSubtotal[arrPos] := _arrDestrSubtotal[arrPos] + val;
      _arrTotal[arrPos] := _arrTotal[arrPos] + val;

      val := pt.costParams.destrCleaningCosts( units );
      _arrCAndD[arrPos] := _arrCAndD[arrPos] + val;
      _arrDestrSubtotal[arrPos] := _arrDestrSubtotal[arrPos] + val;
      _arrTotal[arrPos] := _arrTotal[arrPos] + val;

      val := pt.costParams.destrEuthanasiaCosts( animals );
      _arrEuthanasia[arrPos] := _arrEuthanasia[arrPos] + val;
      _arrDestrSubtotal[arrPos] := _arrDestrSubtotal[arrPos] + val;
      _arrTotal[arrPos] := _arrTotal[arrPos] + val;

      val := pt.costParams.destrIndemnificationCosts( animals );
      _arrIndemnification[arrPos] := _arrIndemnification[arrPos] + val;
      _arrDestrSubtotal[arrPos] := _arrDestrSubtotal[arrPos] + val;
      _arrTotal[arrPos] := _arrTotal[arrPos] + val;

      val := pt.costParams.destrDisposalCosts( animals );
      _arrDisposal[arrPos] := _arrDisposal[arrPos] + val;
      _arrDestrSubtotal[arrPos] := _arrDestrSubtotal[arrPos] + val;
      _arrTotal[arrPos] := _arrTotal[arrPos] + val;

      dbcout( 'Done TFrameCostIterationSummary.calculateDestrCostsForDay', DBFRAMECOSTITERATIONSUMMARY );
    end
  ;



  procedure TFrameCostIterationSummary.calculateVaccCostsForDay(
        pt: TProductionType;
        const day: integer;
        const units: integer;
        const animals: integer;
        var allAnimalsVacc: longint
      );
    var
      arrPos: integer;
      val: double;
    begin
      dbcout( 'TFrameCostIterationSummary.calculateVaccCostsForDay...', DBFRAMECOSTITERATIONSUMMARY );

      arrPos := day - 1;

      val := pt.costParams.vaccSetupCosts( units );
      _arrVaccSetup[arrPos] := _arrVaccSetup[arrPos] + val;
      _arrVaccSubtotal[arrPos] := _arrVaccSubtotal[arrPos] + val;
      _arrTotal[arrPos] := _arrTotal[arrPos] + val;

      val := pt.costParams.vaccVaccinationDailyCosts( animals, allAnimalsVacc );
      _arrVacc[arrPos] := _arrVacc[arrPos] + val;
      _arrVaccSubtotal[arrPos] := _arrVaccSubtotal[arrPos] + val;
      _arrTotal[arrPos] := _arrTotal[arrPos] + val;
      
      dbcout( 'TFrameCostIterationSummary.calculateVaccCostsForDay', DBFRAMECOSTITERATIONSUMMARY );
    end
  ;



  procedure TFrameCostIterationSummary.buildCumulArrays();
    var
      i: integer;
    begin
      dbcout( 'TFrameCostIterationSummary.buildCumulArrays...', DBFRAMECOSTITERATIONSUMMARY );

      _arrTotalCumul[0] := _arrTotal[0];
      _arrDestrSubtotalCumul[0] := _arrDestrSubtotal[0];
      _arrAppraisalCumul[0] := _arrAppraisal[0];
      _arrCAndDCumul[0] := _arrCAndD[0];
      _arrEuthanasiaCumul[0] := _arrEuthanasia[0];
      _arrIndemnificationCumul[0] := _arrIndemnification[0];
      _arrDisposalCumul[0] := _arrDisposal[0];
      _arrVaccSetupCumul[0] := _arrVaccSetup[0];
      _arrVaccCumul[0] := _arrVacc[0];
      _arrVaccSubtotalCumul[0] := _arrVaccSubtotal[0];

      for i := 1 to _arrTotal.Count - 1 do
        begin
          _arrTotalCumul[i] := _arrTotalCumul[i-1] + _arrTotal[i];
          _arrDestrSubtotalCumul[i] := _arrDestrSubtotalCumul[i-1] + _arrDestrSubtotal[i];
          _arrAppraisalCumul[i] := _arrAppraisalCumul[i-1] + _arrAppraisal[i];
          _arrCAndDCumul[i] := _arrCAndDCumul[i-1] + _arrCAndD[i];
          _arrEuthanasiaCumul[i] := _arrEuthanasiaCumul[i-1] + _arrEuthanasia[i];
          _arrIndemnificationCumul[i] := _arrIndemnificationCumul[i-1] + _arrIndemnification[i];
          _arrDisposalCumul[i] := _arrDisposalCumul[i-1] + _arrDisposal[i];
          _arrVaccSetupCumul[i] := _arrVaccSetupCumul[i-1] + _arrVaccSetup[i];
          _arrVaccCumul[i] := _arrVaccCumul[i-1] + _arrVacc[i];
          _arrVaccSubtotalCumul[i] := _arrVaccSubtotalCumul[i-1] + _arrVaccSubtotal[i];
        end
      ;

      dbcout( 'Done TFrameCostIterationSummary.buildCumulArrays', DBFRAMECOSTITERATIONSUMMARY );
    end
  ;


  procedure TFrameCostIterationSummary.incrementArraySizes();
    begin
      _arrTotal.append( 0.0 );
      _arrDestrSubtotal.append( 0.0 );
      _arrAppraisal.append( 0.0 );
      _arrCAndD.append( 0.0 );
      _arrEuthanasia.append( 0.0 );
      _arrIndemnification.append( 0.0 );
      _arrDisposal.append( 0.0 );
      _arrVaccSetup.append( 0.0 );
      _arrVacc.append( 0.0 );
      _arrVaccSubtotal.append( 0.0 );

      _arrTotalCumul.append( 0.0 );
      _arrDestrSubtotalCumul.append( 0.0 );
      _arrAppraisalCumul.append( 0.0 );
      _arrCAndDCumul.append( 0.0 );
      _arrEuthanasiaCumul.append( 0.0 );
      _arrIndemnificationCumul.append( 0.0 );
      _arrDisposalCumul.append( 0.0 );
      _arrVaccSetupCumul.append( 0.0 );
      _arrVaccCumul.append( 0.0 );
      _arrVaccSubtotalCumul.append( 0.0 );
    end
  ;

  
  procedure TFrameCostIterationSummary.updateCumulArraysForDay( const day: integer );
    begin
      if( 1 = day ) then
        begin
          _arrTotalCumul[day - 1] := _arrTotal[day - 1];
          _arrDestrSubtotalCumul[day - 1] := _arrDestrSubtotal[day - 1];
          _arrAppraisalCumul[day - 1] := _arrAppraisal[day - 1];
          _arrCAndDCumul[day - 1] := _arrCAndD[day - 1];
          _arrEuthanasiaCumul[day - 1] := _arrEuthanasia[day - 1];
          _arrIndemnificationCumul[day - 1] := _arrIndemnification[day - 1];
          _arrDisposalCumul[day - 1] := _arrDisposal[day - 1];
          _arrVaccSetupCumul[day - 1] := _arrVaccSetup[day - 1];
          _arrVaccCumul[day - 1] := _arrVacc[day - 1];
          _arrVaccSubtotalCumul[day - 1] := _arrVaccSubtotal[day - 1];
        end
      else
        begin
          _arrTotalCumul[day - 1] := _arrTotalCumul[day - 2] + _arrTotal[day - 1];
          _arrDestrSubtotalCumul[day - 1] := _arrDestrSubtotalCumul[day - 2] + _arrDestrSubtotal[day - 1];
          _arrAppraisalCumul[day - 1] := _arrAppraisalCumul[day - 2] + _arrAppraisal[day - 1];
          _arrCAndDCumul[day - 1] := _arrCAndDCumul[day - 2] + _arrCAndD[day - 1];
          _arrEuthanasiaCumul[day - 1] := _arrEuthanasiaCumul[day - 2] + _arrEuthanasia[day - 1];
          _arrIndemnificationCumul[day - 1] := _arrIndemnificationCumul[day - 2] + _arrIndemnification[day - 1];
          _arrDisposalCumul[day - 1] := _arrDisposalCumul[day - 2] + _arrDisposal[day - 1];
          _arrVaccSetupCumul[day - 1] := _arrVaccSetupCumul[day - 2] + _arrVaccSetup[day - 1];
          _arrVaccCumul[day - 1] := _arrVaccCumul[day - 2] + _arrVacc[day - 1];
          _arrVaccSubtotalCumul[day - 1] := _arrVaccSubtotalCumul[day - 2] + _arrVaccSubtotal[day - 1];
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Display
//-----------------------------------------------------------------------------
  procedure TFrameCostIterationSummary.resizeContents();
    var
      //newChartHeight: integer;
      showDebugMsg: boolean;
    begin
      showDebugMsg := false;

      dbcout( 'Form height: ' + intToStr( self.Parent.Parent.Parent.Height ), showDebugMsg );
      dbcout( 'Form client height: ' + intToStr( self.Parent.Parent.Parent.ClientHeight ), showDebugMsg );

      self.Align := alNone;
      pnlTable.Align := alNone;
//      Splitter1.Align := alNone;
      pnlChart.Align := alNone;

      self.Height := self.Parent.Parent.Parent.ClientHeight - 100;

      dbcout( 'My client height is ' + intToStr( self.ClientHeight ), showDebugMsg );

//      fraTable.Visible := cbxShowTable.checked;
//      fraChart.Visible := cbxShowChart.checked;

      if( fraChart.Visible ) then
        pnlChart.Constraints.MinHeight := fraChart.Constraints.MinHeight; // + pnlShowChart.Height
//      else
//        pnlChart.Constraints.MinHeight := pnlShowChart.Height
//      ;

      if( fraTable.Visible and fraChart.Visible ) then
        begin
          dbcout( '--- Branch A', showDebugMsg );
          pnlTable.Height := self.ClientHeight - pnlChart.Constraints.MinHeight;// - Splitter1.Height;
          pnlChart.Height := pnlChart.Constraints.minHeight;
        end
      else if( fraTable.Visible and not( fraChart.Visible ) ) then
        begin
          dbcout( '--- Branch B', showDebugMsg );
          pnlChart.Height := pnlChart.Constraints.MinHeight;
          pnlTable.Height := self.ClientHeight - pnlChart.Constraints.MinHeight; // - Splitter1.Height;
        end
      else if( not( fraTable.Visible ) and fraChart.Visible ) then
        begin
          dbcout( '--- Branch C', showDebugMsg );
          pnlTable.Height := pnlTable.Constraints.MinHeight;
          //newChartHeight := self.ClientHeight - pnlTable.Constraints.MinHeight - Splitter1.Height;
        end
      else // neither frame is visible
        begin
          dbcout( '--- Branch D', showDebugMsg );
          pnlTable.Height := pnlTable.Constraints.MinHeight;
          pnlChart.Height := pnlChart.Constraints.MinHeight;
        end
      ;

//      dbcout( 'Splitter1.Height: ' + intToStr( Splitter1.Height ), showDebugMsg );
      dbcout( 'pnlTable.Height: ' + intToStr( pnlTable.Height ), showDebugMsg );
      dbcout( 'pnlChart.Height: ' + intToStr( pnlChart.Height ), showDebugMsg );

      self.Align := alClient;
//      Splitter1.Align := alTop;
      pnlTable.Align := alTop;
      pnlChart.Align := alClient;

//      dbcout( 'Splitter1.Height: ' + intToStr( Splitter1.Height ), showDebugMsg );
      dbcout( 'pnlTable.Height: ' + intToStr( pnlTable.Height ), showDebugMsg );
      dbcout( 'pnlChart.Height: ' + intToStr( pnlChart.Height ), showDebugMsg );

//      splitter1.Enabled := ( fraTable.Visible and fraChart.Visible );

      dbcout( '*** Done with TFrameCostIterationSummary.resizeContents', DBFRAMECOSTITERATIONSUMMARY );
    end
  ;
//-----------------------------------------------------------------------------
procedure TFrameCostIterationSummary.Splitter1Moved(Sender: TObject);
//  var
//    factor: integer;
  begin
{*
    self.Align := alNone;
//    Splitter1.Align := alNone;
    pnlTable.Align := alNone;
    pnlChart.Align := alNone;
    factor := fraTable.fraGrid.stgGrid.Height div (fraTable.fraGrid.stgGrid.DefaultRowHeight +
      fraTable.fraGrid.stgGrid.GridLineWidth);
    pnlTable.Height := pnlShowTable.Height + 2*pnlShowTable.BevelWidth + 2*pnlShowTable.BevelWidth + fraTable.pnlShowCumul.Height
      + (factor) * (fraTable.fraGrid.stgGrid.DefaultRowHeight + fraTable.fraGrid.stgGrid.GridLineWidth);
    self.Align := alClient;
//    Splitter1.Align := alTop;
    pnlTable.Align := alTop;
    pnlChart.Align := alClient;
*}
  end
;


//-----------------------------------------------------------------------------
// GUI handling functions
//-----------------------------------------------------------------------------
  procedure TFrameCostIterationSummary.cbxShowTableClick(Sender: TObject);
    begin
      resizeContents();
    end
  ;


  procedure TFrameCostIterationSummary.cbxShowChartClick(Sender: TObject);
    begin
      resizeContents();
    end
  ;


  procedure TFrameCostIterationSummary.Splitter1CanResize(
        Sender: TObject;
        var NewSize: Integer;
        var Accept: Boolean
      );
    begin
      dbcout( newSize, DBFRAMECOSTITERATIONSUMMARY );
      dbcout ( self.ClientHeight - newSize, DBFRAMECOSTITERATIONSUMMARY );

      if( pnlChart.Constraints.MinHeight > self.ClientHeight - newSize ) then // - splitter1.Height ) then
        accept := false
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
  function TFrameCostIterationSummary.getTableIsVisible(): boolean;
    begin
      result := fraTable.Visible;
    end
  ;


  function TFrameCostIterationSummary.getChartIsVisible(): boolean;
    begin
      result := fraChart.Visible;
    end
  ;
//-----------------------------------------------------------------------------





end.
