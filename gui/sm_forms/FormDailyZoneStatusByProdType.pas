unit FormDailyZoneStatusByProdType;

(*
FormDailyZoneStatusByProdType.pas/dfm
-------------------------------------
Begin: 2005/06/28
Last revision: $Date: 2008/10/21 19:20:23 $ $Author: areeves $
Version: $Revision: 1.17 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Shaun Case <Shaun.Case@colostate.edu>
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2007 - 2008 Animal Population Health Institute, Colorado State University

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
    SMSimulationInput,
    SMSimOutByProdType,
    Herd,
    Zone,

    // Application-specific GUI classes
    FormSMOutputBase, // the base class!
    FrameChartBase,
    FrameDailyZoneStatusByProdType,
    QIntegerMaps,
    QLists
  ;


  type DailyZoneData = class( TObject )
    Day: Integer;
    ZoneLevel: Integer;
    Area: double;
    UnitCount: array of Integer;
    AnimalCount: array of Integer;
    UnitDays: array of Integer;
    AnimalDays: array of Integer;

    constructor create(); reintroduce;
    destructor destroy(); override;
  end;


    
  {*
    This form displays a graph showing the status of zones
    on each day of a single iteration.
  }
  type  TFormDailyZoneStatusByProdType = class( TFormSMOutputBase )
      fraBody: TFrameDailyZoneStatusByProdType;
      cboIteration: TComboBox;
      lblIteration: TLabel;
      Y_AxisSource: TRadioGroup;
      pnlCaption: TPanel;

      // Toggles chart display between 2D and 3D }
      procedure cbxThreeDClick(Sender: TObject);

      // Called on Iteration spinner changed
      procedure cboIterationChange(Sender: TObject);

      // Called when Y-Axis source is changed
      procedure Y_AxisSourceClick(Sender: TObject);
      procedure cboProdTypesChange(Sender: TObject);

    protected
      _days: TQIntegerIntegerMap;
      _daysData: array of TQObjectList;
      _numZones: Integer;

      _chartFixNeeded: boolean; // Used with the TChart bug work around.  See chart drawing functions.

      _displayedIteration: Integer;
      _currentDay: Integer;

      procedure translateUI();
      procedure translateUIManual();

      { Used to create a list of all charts on the form that might be saved/copied/printed }
      procedure fillChartDict(); override;

      { Updates the contents of the form when the selected production type changes }
      procedure productionTypeChanged(); override;

      { Updates the contents of the form when the loaded database/simulation changes }
      procedure simChanged(); override;

      { Handles form updates from the database when a simulation is not running or when _selectedPT is changed }
      procedure setupFromDatabase();

      { Chart drawing functions }
      procedure fixTChartBug();
      procedure fillOutForm();
      procedure addZones();

      procedure setCaption();

    protected
      noInterrupt:boolean;
    public
    	constructor create( AOwner: TComponent; sim: TSMSimulationInput; db: TSMDatabase ); reintroduce;
      destructor destroy(); override;

      procedure reset();

      { Handles chart updates while a simulation is in progress }
      procedure updateForDay( day: integer );
      procedure updateSimComplete();
      procedure iterationComplete();

      procedure outbreakEnd( day: integer );
      procedure resetIteration( const iteration: Integer );
    end
  ;


  var
		frmDailyZoneStatusByProdType: TFormDailyZoneStatusByProdType;


  const
  	DBFORMDAILYSTATUSBYPRODTYPE: boolean = false; // set to true to enable debugging messages for this unit.

    
implementation

	{$R *.dfm}

  uses
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

  const
    AREA        = 0;
    NUM_UNITS   = 1;
    NUM_ANIMALS = 2;
    UNIT_DAYS   = 3;
    ANIMAL_DAYS = 4;



  constructor DailyZoneData.create();
    begin
      inherited create();
     
      setLength( UnitCount, 0 );
      setLength( AnimalCount, 0 );
      setLength( UnitDays, 0 );
      setLength( AnimalDays, 0 );
    end
  ;


  destructor DailyZoneData.destroy();
    begin
      inherited destroy();

      Area := 0.0;
      ZoneLevel := 0;
      Day := -1;
      setLength( UnitCount, 0 );
      setLength( AnimalCount, 0 );
      setLength( UnitDays, 0 );
      setLength( AnimalDays, 0 );
    end
  ;


  procedure TFormDailyZoneStatusByProdType.addZones();
    var
      TNewLine:TLineSeries;
      db2: TSqlDatabase;
      res: TSqlResult;
      row: TSqlRow;
      q: string;
      zoneColors: TColorArray;
      zList: TZoneList;
    begin
      noInterrupt := true;
      fraBody.chtOutputs.RemoveAllSeries();

      zoneColors := frmMain.ZoneColors();
      zList := _smSim.zoneList;

      _numZones := zList.Count;

      try
        screen.Cursor := crHourGlass;
        setChartControlsEnabled( false );

        dbcout( 'Starting Zone Read from database', DBFORMDAILYSTATUSBYPRODTYPE );
        db2 := _smdb as TSqlDatabase;

        q := 'SELECT * from inZone order by radius';

        res := TSqlResult.create( q, db2 );

        if ( nil <> res  ) then
          begin
            row := res.fetchArrayFirst();
            while ( nil <> row ) do
              begin
                TNewLine := TLineSeries.Create( fraBody.chtOutputs.Owner );
                fraBody.chtOutputs.AddSeries( TNewLine as TChartSeries );
                fraBody.chtOutputs.Repaint();
                Application.ProcessMessages;
                TNewLine.Title := row.field( 'descr' );

                if ( nil <> zoneColors ) and (  1 <= length( zoneColors ) ) then
                  begin
                    if ( zList.find( TNewLine.Title ) <> nil ) then
                      TNewLine.SeriesColor := zoneColors[ zList.find( TNewLine.Title ).level - 1 mod length( zoneColors ) ]
                    ;
                  end
                ;

                row:= res.fetchArrayNext();
              end
            ;
          end
        ;

        dbcout( 'Done getting Zones from database', DBFORMDAILYSTATUSBYPRODTYPE );
        res.Free();
      finally
        setChartControlsEnabled( true );
        screen.Cursor := crDefault;
      end;

      noInterrupt := false;
    end
  ;


//-----------------------------------------------------------------------------
// Form construction/initialization/destruction
//-----------------------------------------------------------------------------
	constructor TFormDailyZoneStatusByProdType.create( AOwner: TComponent; sim: TSMSimulationInput; db: TSMDatabase );
    var
    i:integer;
  	begin
      inherited create( AOwner );
      translateUI();

      pnlCaption.Caption := '';
      
      setDataControlsEnabled( false );
      _days := TQIntegerIntegerMap.create();

      // Initialize pointers
      //--------------------
      _smSim := sim;
      _smdb := db;
      _selectedPT := nil;
      _displayedIteration := frmMain.displayedIteration;
      noInterrupt := false;

      // Size the form
      //--------------
      ClientWidth := 556 + lblIteration.Width + cboIteration.Width;
      cboIteration.Items.Clear();
      cboIteration.enabled := false;
      fraBody.chtOutputs.View3D := False;
      fraBody.chtOutputs.RemoveAllSeries();

      setupComboBox();

      addZones();

      setLength( _daysData, _numZones );

      for i := 0 to _numZones - 1 do
        begin
          _daysData[i] := TQObjectList.create();
        end;

      _chartFixNeeded := true;

      // Show the data
      //--------------
      if ( Y_AxisSource.ItemIndex = AREA ) then
        cboProdTypes.Enabled := false
      else
        cboProdTypes.Enabled := true;

      setUpFromDatabase();

      setCaption();

      if ( frmMain.simIsRunning ) then
        begin
          cboIteration.enabled := false;
          cboIteration.clear();
        end
      ;
    end
  ;


  procedure TFormDailyZoneStatusByProdType.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.2.
      // Generation date: Thu Feb 28 17:04:55 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormDailyZoneStatusByProdType.dfm
      // File date: Tue Feb 26 21:32:00 2008

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Daily zone status for 1 iteration' );
          lblIteration.Caption := tr( 'Iteration:' );
          Y_AxisSource.Caption := tr( 'Y axis' );
          pnlCaption.Caption := tr( 'Iteration status: completed/aborted/running' );
        end
      ;

      // Set TStrings properties
      with self do
        begin
           Y_AxisSource.Items[0] := tr( 'Area' );
           Y_AxisSource.Items[1] := tr( '# of units' );
           Y_AxisSource.Items[2] := tr( '# of animals' );
           Y_AxisSource.Items[3] := tr( 'Unit days' );
           Y_AxisSource.Items[4] := tr( 'Animal days' );
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


  procedure TFormDailyZoneStatusByProdType.translateUIManual();
    begin
    end
  ;


	destructor TFormDailyZoneStatusByProdType.destroy();
    var
    i, j, k:integer;
  	begin
      inherited destroy();

      if ( assigned( _days ) ) then
        begin
          _days.clear();
          _days.destroy();
        end;

      k := length( _daysData ) - 1;

      for i := 0 to k do
        begin
          for j := 0 to  (_daysData[i] as TQObjectList).count - 1 do
            begin
              ((_daysData[i] as TQObjectList).at(j) as DailyZoneData).destroy();
            end;
          _daysData[i].clear();
        end;

      setLength( _daysData, 0 );
    end
  ;


  procedure TFormDailyZoneStatusByProdType.fillChartDict();
    begin
      _chartDict[fraBody.Name] := fraBody;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Major chart functions
//-----------------------------------------------------------------------------
  procedure TFormDailyZoneStatusByProdType.productionTypeChanged();
    begin
      try
        screen.Cursor := crHourGlass;
        setChartControlsEnabled( false );

        reset();

        fillOutForm();
      finally
        setChartControlsEnabled( true );
        screen.Cursor := crDefault;
      end;
    end
  ;

  
	procedure TFormDailyZoneStatusByProdType.simChanged();
  	begin
      cboIteration.Items.Clear();
      cboIteration.enabled := false;
      fraBody.chtOutputs.RemoveAllSeries();
      addZones();
      
      _chartFixNeeded := true;

      // Show the data
      //--------------
      setupComboBox();

      if ( Y_AxisSource.ItemIndex = AREA ) then
        cboProdTypes.Enabled := false
      else
        cboProdTypes.Enabled := true
      ;

      setUpFromDatabase();

      if ( frmMain.simIsRunning ) then
        begin
          cboIteration.enabled := false;
          cboIteration.clear();
        end
      ;

      setCaption();
    end
  ;


  procedure TFormDailyZoneStatusByProdType.reset();
    var
      i:integer;
    begin
      if ( noInterrupt ) then exit;

      noInterrupt := true;
      for i := 0 to (fraBody.chtOutputs.SeriesCount() - 1) do
        begin
          fraBody.chtOutputs.Series[i].Clear();
          if ( frmMain.displayedIteration = -1 ) or ( frmMain.simIsRunning ) then
            fraBody.chtOutputs.Series[i].GetHorizAxis.SetMinMax( 1, 2 )
          else
            fraBody.chtOutputs.Series[i].GetHorizAxis.Automatic := true
          ;

          fraBody.chtOutputs.Series[i].Repaint();
        end;

      if( assigned( _smSim ) ) then
        begin
          case Y_AxisSource.ItemIndex of
            AREA: fraBody.chtOutputs.LeftAxis.Title.Caption := tr( 'Zone Area in Square Kilometers' );
            UNIT_DAYS: fraBody.chtOutputs.LeftAxis.Title.Caption := tr( 'Unit Days' );
            ANIMAL_DAYS: fraBody.chtOutputs.LeftAxis.Title.Caption := tr( 'Animals Days' );
            NUM_UNITS: fraBody.chtOutputs.LeftAxis.Title.Caption := tr( 'Number of Units' );
            NUM_ANIMALS: fraBody.chtOutputs.LeftAxis.Title.Caption := tr( 'Number of Animals' );
          else
            fraBody.chtOutputs.LeftAxis.Title.Caption := tr( 'Y axis' );
          end;
        end
      else
      	fraBody.chtOutputs.LeftAxis.Title.Caption := tr( '(Simulation is not loaded)' )
      ;

      noInterrupt := false;
    end
  ;


  {*
    This function handles chart updates from the database: either when
    a simulation is not running, or when the selected production type is
    changed in midstream.  Function updateForDay() is used for dynamic
    updates.
  }
  procedure TFormDailyZoneStatusByProdType.setupFromDatabase();
    var
      res: TSqlResult;
      row: TSqlRow;
      q: string;
    begin
      if( not( _smSim.includeZonesGlobal ) ) then
        exit
      ;

      try
        screen.Cursor := crHourGlass;
        setChartControlsEnabled( false );

        reset();

        dbcout( 'Starting update chart from database', DBFORMDAILYSTATUSBYPRODTYPE );

        // Fill iteration ComboBox
        //------------------------
        cboIteration.Items.Clear();

        q := 'SELECT distinct(iteration) from outDailyByProductionType order by iteration desc';

        res := TSqlResult.create( q, ( _smdb as TSqlDatabase ) );

        if( 0 < res.numRows ) then
          begin
            cboIteration.Enabled := true;

            row := res.fetchArrayFirst();

            while ( row <> nil ) do
              begin
                 cboIteration.Items.Add(row.field('iteration'));
                 row := res.fetchArrayNext();
              end
            ;

            cboIteration.ItemIndex := 0;
          end
        ;
        res.Free();

        // Determine the iteration to display
        //-----------------------------------
        dbcout2( _displayedIteration );

        if( -1 <> frmMain.displayedIteration ) then
          begin
            // Determine the iteration to display from the main form, and select the appropriate item in the combo box.
            _displayedIteration := frmMain.displayedIteration;
            cboIteration.ItemIndex := cboIteration.Items.IndexOf( IntToStr( _displayedIteration ) );
            if( 0 > cboIteration.ItemIndex ) then
              raise exception.create( 'Iteration not found (' + intToStr( _displayedIteration ) + ') in TFormDailyZoneStatusByProductionType' )
            ;
          end
        else // Select the last item from the newly created list of iterations
          begin
            if( 0 < cboIteration.Items.Count ) then
              begin
                _displayedIteration := strToInt( cboIteration.Items[0] );
                cboIteration.ItemIndex := 0;
              end
            else
              _displayedIteration := -1
            ;
            frmMain.displayedIteration := _displayedIteration;
          end
        ;

        dbcout2( _displayedIteration );

        // Display the data for the appropriate iteration
        //-----------------------------------------------
        if( -1 < _displayedIteration ) then // There is data in the database
          fillOutForm()
        else
          dbcout( 'No database data to display in TFormDailyZoneStatusByProductionType', DBFORMDAILYSTATUSBYPRODTYPE )
        ;

        // clean up
        //----------
        dbcout( 'Done drawing from database', DBFORMDAILYSTATUSBYPRODTYPE );

      finally
        setChartControlsEnabled( true );
        screen.Cursor := crDefault;
      end;
    end
  ;


  procedure TFormDailyZoneStatusByProdType.updateSimComplete();
    begin
      if( _smSim.includeZonesGlobal ) then
        begin
          self.setupFromDatabase();
          self.resetIteration( StrToInt( cboIteration.Items[0] ) );
          frmMain.displayedIteration := StrToInt( cboIteration.Items[0] );

          fixTChartBug();
        end
      ;
    end
  ;


  {*
    This function handles chart updates while a simulation is in progress.
    When a simulation is not running (or when the selected production type is
    changed in midstream), setUpFromDatabase() is used.
  }
	procedure TFormDailyZoneStatusByProdType.updateForDay( day: integer );
  	var
      pType:integer;
      zList:TZoneListIterator;
      tempZone:TZone;
      count:integer;
      i, j, k:integer;
      DailyData: DailyZoneData;
    begin
      if( not( _smSim.zoneList.focusCreated ) ) then
        exit
      ;

      if ( day = 1 ) then
        reset()
      ;

      if ( _currentDay > day ) then
        begin
          reset();
          _days.clear();

          k := length( _daysData ) - 1;

          for i := 0 to k do
            begin
              for j := 0 to  (_daysData[i] as TQObjectList).count - 1 do
                begin
                  ((_daysData[i] as TQObjectList).at(j) as DailyZoneData).destroy();
                end;
              _daysData[i].clear();
            end
          ;

          setLength( _daysData, 0 );
          setLength( _daysData, _numZones );

          for i := 0 to _numZones - 1 do
            begin
              _daysData[i] := TQObjectList.create();
            end
          ;
        end
      ;
          
      _currentDay := day;
      
      noInterrupt := true;
      cboIteration.Enabled := false;

    	dbcout( endl + 'Updating Zone chart for day' + endl, DBFORMDAILYSTATUSBYPRODTYPE );

      pType := -1;

      if( nil <> _selectedPT ) then
        begin
          pType := _selectedPT.productionTypeID;
        end
      ;

      zList := TZoneListIterator.create( _smSim.zoneList );
      count := zList.count;
      zList.toFirst();


      _days.insert( _days.count,  _currentDay );

      for i := 0 to (count - 1) do
        begin
          tempZone := zList.current();
          if ( day > 1 ) then
            fraBody.chtOutputs.Series[tempZone.level - 1].GetHorizAxis.SetMinMax( 1, day )
          ;

          // Set cummulative chart data structures
          //--------------------------------------
          DailyData := DailyZoneData.create();
          DailyData.Area := tempZone.area;
          DailyData.ZoneLevel := tempZone.level;
          DailyData.Day := _currentDay;
          setLength( DailyData.UnitCount, cboProdTypes.Items.Count - 1 );
          setLength( DailyData.AnimalCount, cboProdTypes.Items.Count - 1 );
          setLength( DailyData.UnitDays, cboProdTypes.Items.Count - 1 );
          setLength( DailyData.AnimalDays, cboProdTypes.Items.Count - 1 );

          for j := 0 to cboProdTypes.Items.Count - 2 do
            begin
              k := (cboProdTypes.Items.Objects[j + 1] as TProductionType).productionTypeId;
              DailyData.UnitCount[j] := tempZone.getUnitCountById( k );
              DailyData.AnimalCount[j] := tempZone.getAnimalCountById( k );
              DailyData.UnitDays[j] := tempZone.getUnitDaysById( k );
              DailyData.AnimalDays[j] := tempZone.getAnimalDaysById( k );
            end
          ;
            
          _daysData[i].append( DailyData );
          
          case Y_AxisSource.ItemIndex of
            AREA:
              begin
                if ( tempZone.level <= fraBody.chtOutputs.SeriesCount() ) then
                  fraBody.chtOutputs.Series[tempZone.level - 1].AddXY( day, tempZone.area )
                ;
              end
            ;

            NUM_UNITS:
              begin
                if ( pType = -1 ) then
                  fraBody.chtOutputs.Series[tempZone.level - 1].AddXY( day, tempZone.unitCount )
                else
                  fraBody.chtOutputs.Series[tempZone.level - 1].AddXY( day, tempZone.getUnitCountById( pType ) )
                ;
              end
            ;

            NUM_ANIMALS:
              begin
                if ( pType = -1 ) then
                  fraBody.chtOutputs.Series[tempZone.level - 1].AddXY( day, tempZone.animalCount )
                else
                  fraBody.chtOutputs.Series[tempZone.level - 1].AddXY( day, tempZone.getAnimalCountById( pType ) )
                ;
              end
            ;

            UNIT_DAYS:
              begin
                if ( pType = -1 ) then
                  fraBody.chtOutputs.Series[tempZone.level - 1].AddXY( day, tempZone.unitDays )
                else
                  fraBody.chtOutputs.Series[tempZone.level - 1].AddXY( day, tempZone.getUnitDaysById( pType ) )
                ;
              end
            ;

            ANIMAL_DAYS:
              begin
                if ( pType = -1 ) then
                  fraBody.chtOutputs.Series[tempZone.level - 1].AddXY( day, tempZone.animalDays )
                else
                  fraBody.chtOutputs.Series[tempZone.level - 1].AddXY( day, tempZone.getAnimalDaysById( pType ) )
                ;
              end
            ;

            else
              begin
                dbcout( 'No radio button is checked on the FormDailyZoneStatusByProdType form.  Ensure that one is set to be default checked in form editor...', DBFORMDAILYSTATUSBYPRODTYPE );
                Y_AxisSource.ItemIndex := 0;  //Set it to Area.
              end
            ;
          end; // end CASE

          zList.incr();
        end
      ;  // end For loop

      zList.Free();
      noInterrupt := false;
      fixTChartBug();
    end
  ;
//-----------------------------------------------------------------------------


  procedure TFormDailyZoneStatusByProdType.iterationComplete();
    //var
      //i: integer;
    begin
    {*
      reset();
      fraBody.chtOutputs.Series[ fraBody.chtOutputs.SeriesCount - 1 ].AddNullXY(0, 0, '');
      fraBody.chtOutputs.Repaint();
      fraBody.chtOutputs.RemoveAllSeries();
      addZones();
      reset();
      fixTChartBug();
      fraBody.chtOutputs.Refresh();
      fraBody.chtOutputs.Repaint();
     //reset();
     // fraBody.chtOutputs.Repaint();
     // fixTChartBug();
     *}
    end;


//-----------------------------------------------------------------------------



  procedure TFormDailyZoneStatusByProdType.outbreakEnd( day: integer );
  	begin
      //fixTChartBug();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI events and housekeeping
//-----------------------------------------------------------------------------
  procedure TFormDailyZoneStatusByProdType.cbxThreeDClick( Sender: TObject );
    begin
      fraBody.chtOutputs.View3D := not fraBody.chtOutputs.View3D;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Functions for dealing with TChart
//-----------------------------------------------------------------------------
  procedure TFormDailyZoneStatusByProdType.fixTChartBug();
    var
      i: integer;
      min, max: double;
      adjustment: double;
    begin
      min := 0.0;
      max := 0.0;

      for i := 0 to fraBody.chtOutputs.SeriesCount - 1 do
        begin
          if ( fraBody.chtOutputs.Series[i].MinYValue < min ) then
            min := fraBody.chtOutputs.Series[i].MinYValue
          ;

          if ( fraBody.chtOutputs.Series[i].MaxYValue > max ) then
            max := fraBody.chtOutputs.Series[i].MaxYValue
          ;
        end
      ;

      adjustment := (max - min) * 0.05;

      for i := 0 to fraBody.chtOutputs.SeriesCount - 1 do
        fraBody.chtOutputs.Series[i].GetVertAxis.SetMinMax( min, max + adjustment )
      ;

      min := 1.0;
      max := 2.0;

      for i := 0 to fraBody.chtOutputs.SeriesCount - 1 do
        begin
          if ( fraBody.chtOutputs.Series[i].MinXValue < min ) then
            min := fraBody.chtOutputs.Series[i].MinXValue
          ;

          if ( fraBody.chtOutputs.Series[i].MaxXValue > max ) then
            max := fraBody.chtOutputs.Series[i].MaxXValue
          ;
        end
      ;

      for i := 0 to fraBody.chtOutputs.SeriesCount - 1 do
        fraBody.chtOutputs.Series[i].GetHorizAxis.SetMinMax( min, max )
      ;
    end
  ;
//-----------------------------------------------------------------------------



  procedure TFormDailyZoneStatusByProdType.fillOutForm();
    var
      db2: TSqlDatabase;
      res: TSqlResult;
      row: TSqlRow;
      q: string;

      selectedPTIDClause: string;

      currentDay: integer;
      currentZone: integer;
      sum:integer;
      first:  boolean;
      i:integer;
      maxDay:integer;
      needFix: boolean;
      zList: TZoneList;
    begin
      // Set up the horizontal axis of the chart, based on the last day of the iteration
      //--------------------------------------------------------------------------------
      db2 := _smdb as TSqlDatabase;
      q := 'SELECT outbreakEndDay from outIteration WHERE iteration = ' + intToStr( _displayedIteration );
      res := TSqlResult.create( q, db2 );

      dbcout( q, DBFORMDAILYSTATUSBYPRODTYPE );

      res.runQuery( q );

      needFix := true;
      row := res.fetchArrayFirst();
      if ( nil <> row ) then
        begin
          maxDay := row.field( 'outbreakEndDay' );
          for i := 0 to fraBody.chtOutputs.SeriesCount - 1 do
            begin
              fraBody.chtOutputs.Series[i].GetHorizAxis.SetMinMax( 1, maxDay );
              fraBody.chtOutputs.Series[i].Repaint();
            end
          ;
          needFix := false;
        end
      ;

      // Determine the ID of the currently selected production type
      //-----------------------------------------------------------
      if( nil = _selectedPT ) then
        selectedPTIDClause := ''
      else
        selectedPTIDClause := ' AND productionTypeID = ' + intToStr( _selectedPT.productionTypeID )
      ;

      // select indicated pts, all days, for last complete iteration, sorted by day
      //----------------------------------------------------------------------------
      if ( AREA = Y_AxisSource.ItemIndex ) then
        begin
          q := 'SELECT iteration, day, zoneID, area FROM outDailyByZone'
            + ' WHERE iteration = ' + intToStr( _displayedIteration )
            + ' ORDER BY day, zoneID'
          ;
        end
      else
        begin
          q := 'SELECT'
            + ' iteration, day, zoneID, productionTypeID, unitDaysInZone, animalDaysInZone, unitsInZone, animalsInZone'
            + ' FROM outDailyByZoneAndProductionType'
            + ' WHERE iteration = ' + intToStr( _displayedIteration )
            + selectedPTIDClause
            + ' ORDER BY day, zoneID, productionTypeID'
          ;
        end
      ;

      dbcout( q, DBFORMDAILYSTATUSBYPRODTYPE );

      res.runQuery( q );

      _chartFixNeeded := true; // Reset this here, because the problem may occur again when loading from the database
      zlist := _smSim.zoneList;

       row := res.fetchArrayFirst();
       first := true;

       if ( nil <> row ) then
         begin
          needFix := true;

          // If "All production types" is selected, then generate a sum for the requested output.
          // (Note that there is no sum for zone area, so zone area can be treated below.)
          //-------------------------------------------------------------------------------------
          if ( ( nil = _selectedPT ) and ( AREA <> Y_AxisSource.ItemIndex ) )then
            begin
              while( nil <> row ) do
                begin
                  currentDay := row.field( 'day' );
                  currentZone := zlist.find( row.field( 'zoneID') ).level;
                  sum := 0;

                  while( ( currentDay = row.field( 'day' ) ) and ( currentZone = zlist.find( row.field( 'zoneID' )).level )) do
                    begin
                      case Y_AxisSource.ItemIndex of
                        UNIT_DAYS:   sum := sum + row.field( 'unitDaysInZone' );
                        ANIMAL_DAYS: sum := sum + row.field( 'animalDaysInZone' );
                        NUM_ANIMALS: sum := sum + row.field( 'animalsInZone' );
                        NUM_UNITS:   sum := sum + row.field( 'unitsInZone' );
                      end;
                      row := res.fetchArrayNext();
                      if ( row = nil ) then
                        begin
                          break;
                        end
                      ;
                    end
                  ;  //  End currentDay = current row and currentZone = current row....

                  // Write sum to chart here....
                  if ( ( currentZone - 1 ) < fraBody.chtOutputs.SeriesCount() ) then
                    begin
                      if ( first ) then
                        begin
                          first := false;
                          if ( currentDay > 1 ) then
                            begin
                              for i := 0 to fraBody.chtOutputs.SeriesCount - 1 do
                                fraBody.chtOutputs.Series[i].AddXY( currentDay - 1 , 0 )
                              ;
                            end
                          ;
                        end
                      ;
                      fraBody.chtOutputs.Series[currentZone - 1].AddXY( currentDay, sum );
                    end
                  ;

                end
              ;
            end

          // If this statement is reached, then either an output for an
          // individual production type was requested, or area was requested.
          //-----------------------------------------------------------------
          else
            begin
              currentDay := row.field( 'day' );

              while( nil <> row ) do
                begin
                  while( currentDay = row.field( 'day' ) ) do
                    begin
                      currentZone := zlist.find(row.field( 'zoneID')).level - 1;

                      if ( currentZone < fraBody.chtOutputs.SeriesCount() )  then
                        begin
                          if ( first ) then
                            begin
                              first := false;
                              if ( currentDay > 1 ) then
                                begin
                                  for i := 0 to fraBody.chtOutputs.SeriesCount - 1 do
                                    begin
                                      case Y_AxisSource.ItemIndex of
                                        AREA:  fraBody.chtOutputs.Series[i].AddXY( currentDay - 1, 0 );
                                        NUM_UNITS: fraBody.chtOutputs.Series[i].AddXY( currentDay - 1, 0 );
                                        NUM_ANIMALS: fraBody.chtOutputs.Series[i].AddXY( currentDay - 1, 0 );
                                        UNIT_DAYS: fraBody.chtOutputs.Series[i].AddXY( currentDay - 1, 0 );
                                        ANIMAL_DAYS: fraBody.chtOutputs.Series[i].AddXY( currentDay - 1, 0 );
                                      end;
                                    end
                                  ;
                                end
                              ;
                            end
                          ;

                          case Y_AxisSource.ItemIndex of
                            AREA:  fraBody.chtOutputs.Series[currentZone].AddXY( currentDay, row.field( 'area' ));
                            NUM_UNITS: fraBody.chtOutputs.Series[currentZone].AddXY( currentDay, row.field( 'unitsInZone' ));
                            NUM_ANIMALS: fraBody.chtOutputs.Series[currentZone].AddXY( currentDay, row.field( 'animalsInZone' ));
                            UNIT_DAYS: fraBody.chtOutputs.Series[currentZone].AddXY( currentDay, row.field( 'unitDaysInZone' ));
                            ANIMAL_DAYS: fraBody.chtOutputs.Series[currentZone].AddXY( currentDay, row.field( 'animalDaysInZone' ));
                          end;
                        end
                      ;

                      row := res.fetchArrayNext();
                      if ( row = nil ) then
                        break
                      ;
                    end
                   ; // END while( currentDay = row.field( 'day' ) )

                  if ( row <> nil ) then
                   currentDay := row.field( 'day' )
                  ;
              end
              ; // END while( nil <> row )

            end
          ; // END indiv production type or area output
        end
      ; // END if( nil <> row )

      // Clean up and go home
      //---------------------
      res.Free();

      if ( needFix ) then
        fixTChartBug()
      ;
    end
  ;


  procedure TFormDailyZoneStatusByProdType.resetIteration( const iteration: Integer );
    begin
      if ( cboIteration.Items.IndexOf( IntToStr( iteration ) ) >= 0 ) then
        begin
          cboIteration.ItemIndex := cboIteration.Items.IndexOf( IntToStr( iteration ) );

          _displayedIteration := iteration;

          if( iteration > _smdb.completedIterations ) then
            pnlCaption.Caption := tr( 'Iteration status: aborted' )
          else
            pnlCaption.Caption := tr( 'Iteration status: complete' )
          ;

          try
            screen.Cursor := crHourGlass;
            setChartControlsEnabled( false );

            reset();

            fillOutForm();
          finally
            setChartControlsEnabled( true );
            screen.Cursor := crDefault;
          end;
        end
      ;
    end
  ;

  
  procedure TFormDailyZoneStatusByProdType.cboIterationChange(Sender: TObject);
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
            fillOutForm();

          finally
            setChartControlsEnabled( true );
            screen.Cursor := crDefault;
          end;
        end
      ;
    end
  ;


  procedure TFormDailyZoneStatusByProdType.Y_AxisSourceClick(Sender: TObject);
    var
      i,j,k,sum: Integer;
      Daily: DailyZoneData;
      pType: Integer;
    begin
      inherited;

      if( assigned( _smSim ) ) then
        begin
          case Y_AxisSource.ItemIndex of
            AREA: fraBody.chtOutputs.LeftAxis.Title.Caption := tr( 'Zone Area in Square Kilometers' );
            UNIT_DAYS: fraBody.chtOutputs.LeftAxis.Title.Caption := tr( 'Unit Days' );
            ANIMAL_DAYS: fraBody.chtOutputs.LeftAxis.Title.Caption := tr( 'Animals Days' );
            NUM_UNITS: fraBody.chtOutputs.LeftAxis.Title.Caption := tr( 'Number of Units' );
            NUM_ANIMALS: fraBody.chtOutputs.LeftAxis.Title.Caption := tr( 'Number of Animals' );
          else
            fraBody.chtOutputs.LeftAxis.Title.Caption := tr( 'Y axis' );
          end;
        end
      else
        fraBody.chtOutputs.LeftAxis.Title.Caption := tr( '(Simulation is not loaded)' )
      ;


      if ( Y_AxisSource.ItemIndex = AREA ) then
        cboProdTypes.Enabled := false
      else
        cboProdTypes.Enabled := true
      ;

      try
        screen.Cursor := crHourGlass;
        setChartControlsEnabled( false );

        addZones();
        reset();
        Application.ProcessMessages();
        if ( not frmMain.simIsRunning ) then
          fillOutForm()
        else
          begin
            pType := -1;

            if( nil <> _selectedPT ) then
              begin
                pType := _selectedPT.productionTypeID;
              end
            ;

            for i := 0 to length(_daysData ) - 1 do
              begin
                for j := 0 to _daysData[i].count - 1 do
                  begin
                    Daily := ((_daysData[i] as TQObjectList).at(j) as DailyZoneData);                

                    case Y_AxisSource.ItemIndex of
                      AREA:
                        begin
                          fraBody.chtOutputs.Series[Daily.ZoneLevel -1].AddXY( Daily.Day, Daily.Area );
                          fraBody.chtOutputs.repaint();
                        end
                      ;

                      NUM_UNITS:
                        begin
                          if ( pType = -1 ) then
                            begin
                              sum := 0;
                              for k := 0 to length( Daily.UnitCount ) - 1 do
                                sum := sum + Daily.UnitCount[k]
                              ;
                              fraBody.chtOutputs.Series[ Daily.ZoneLevel - 1].AddXY( Daily.Day, sum );
                            end
                          else
                            fraBody.chtOutputs.Series[Daily.ZoneLevel - 1].AddXY( Daily.Day, Daily.UnitCount[ cboProdTypes.ItemIndex - 1] )
                          ;
                        end
                      ;

                      NUM_ANIMALS:
                        begin
                          if ( pType = -1 ) then
                            begin
                              sum := 0;
                              for k := 0 to length( Daily.AnimalCount ) - 1 do
                                sum := sum + Daily.AnimalCount[k]
                              ;
                              fraBody.chtOutputs.Series[ Daily.ZoneLevel - 1].AddXY( Daily.Day, sum );
                            end
                          else
                            fraBody.chtOutputs.Series[Daily.ZoneLevel - 1].AddXY( Daily.Day, Daily.AnimalCount[ cboProdTypes.ItemIndex - 1] )
                          ;
                        end
                      ;


                      UNIT_DAYS:
                        begin
                          if ( pType = -1 ) then
                            begin
                              sum := 0;
                              for k := 0 to length( Daily.UnitDays ) - 1 do
                                sum := sum + Daily.UnitDays[k];
                              fraBody.chtOutputs.Series[ Daily.ZoneLevel - 1].AddXY( Daily.Day, sum );
                            end
                          else
                            fraBody.chtOutputs.Series[Daily.ZoneLevel - 1].AddXY( Daily.Day, Daily.UnitDays[ cboProdTypes.ItemIndex - 1] )
                          ;
                        end;

                      ANIMAL_DAYS:
                        begin
                          if ( pType = -1 ) then
                            begin
                              sum := 0;
                              for k := 0 to length( Daily.AnimalDays ) - 1 do
                                sum := sum + Daily.AnimalDays[k]
                              ;
                              fraBody.chtOutputs.Series[ Daily.ZoneLevel - 1].AddXY( Daily.Day, sum );
                            end
                          else
                            fraBody.chtOutputs.Series[Daily.ZoneLevel - 1].AddXY( Daily.Day, Daily.AnimalDays[ cboProdTypes.ItemIndex - 1] )
                          ;
                        end
                      ;
                    end;  // End case

                  end
                ;  // End For j
              end
            ;  // End For i
          end
        ;

        fixTChartBug();
        Application.ProcessMessages();
      finally
        setChartControlsEnabled( true );
        screen.Cursor := crDefault;
      end;
    end
  ;


  procedure TFormDailyZoneStatusByProdType.cboProdTypesChange( Sender: TObject);
    begin
      inherited;

      Y_AxisSourceClick( nil );
    end
  ;


  procedure TFormDailyZoneStatusByProdType.setCaption();
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
