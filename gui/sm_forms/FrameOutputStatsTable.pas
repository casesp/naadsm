unit FrameOutputStatsTable;

(*
FrameOutputStatsTable.pas/dfm
-----------------------------
Begin: 2005/12/17
Last revision: $Date: 2008/11/25 22:00:31 $ $Author: areeves $
Version number: $Revision: 1.16 $
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
    Grids,
    ExtCtrls,
    
    ARSyncGrid,

    QOrderedDictionaries,

		FrameStringGridBase,

    Points,

    ProductionType,
    Zone,
    
    SMSimulationStats,
    SMZoneStats,
    OutputDescriptions
  ;
  

  type TFrameOutputStatsTable = class( TFrameStringGridBase )
  	published
      procedure stgGridMouseMove(
        Sender: TObject;
        Shift: TShiftState;
        X, Y: Integer
      );

      procedure stgGridMouseDown(
        Sender: TObject;
        Button: TMouseButton;
        Shift: TShiftState;
        X, Y: Integer
      );

    protected
      _nameColumn: integer;
      d,e: integer;

      _pt: TProductionType;
      _z: TZone;

      _superList: TSMIterationOutputSuperList;
      _superCostStats: TProductionTypeOutputSet;
      _superZoneStats: TZoneOutputSet;
      _rowHeaders: TQOrderedStringStringDictionary;

      _lastMousePoint: RIntPoint;

      _statsType: TStatsType;

      procedure populateEpi();
      procedure setHeadersEpi();

      procedure populateCosts();
      procedure setHeadersCosts();

      procedure populateZones();
      procedure setHeadersZones();

    public
      constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

 			function csvText(): string; override;

      procedure setProdType( pt: TProductionType );
      procedure setZone( z: TZone );
      procedure setSuperList( sl: TSMIterationOutputSuperList );
      procedure setZoneStats( val: TZoneOutputSet );
      procedure setCostStats( val: TProductionTypeOutputSet );
      procedure setOutputType( val: TStatsType );

      property nameColumn: integer read _nameColumn;
    end
  ;


  const
    DBFRAMEOUTPUTSTATSTABLE: boolean = false; // Set to true to enable debugging message for this unit


implementation

  {$R *.dfm}

  uses
    Printers,
    
    SqlClasses,
    CStringList,
    MyStrUtils,
    GuiStrUtils,
    DebugWindow,
    AppLog
  ;

  constructor TFrameOutputStatsTable.create( AOwner: TComponent );
    begin
      inherited create( AOwner );

      dbcout( 'Creating TFrameOutputStatsTable', DBFRAMEOUTPUTSTATSTABLE  );

      _nameColumn := 1;
      _pt := nil;
      _z := nil;
      _superList := nil;
      _superCostStats := nil;
      _superZoneStats := nil;

      _lastMousePoint.x := -1;
      _lastMousePoint.y := -1;

      _statsType := StatsUnspecified;

      application.HintPause := 10;
      application.HintHidePause := 30000;

      dbcout( 'Done creating TFrameOutputStatsTable', DBFRAMEOUTPUTSTATSTABLE );
    end
  ;


  destructor TFrameOutputStatsTable.destroy();
    begin
      dbcout( 'Destroying TFrameOutputStatsTable', DBFRAMEOUTPUTSTATSTABLE );
      _rowHeaders.free();

      application.HintPause := 500;
      application.HintHidePause := 2500;
      dbcout( 'Done destroying TFrameOutputStatsTable', DBFRAMEOUTPUTSTATSTABLE );

      inherited destroy();
    end
  ;


  procedure TFrameOutputStatsTable.setOutputType( val: TStatsType );
    begin
      _statsType := val;

      case _statsType of
        StatsEpi: setHeadersEpi();
        StatsCosts: setHeadersCosts();
        StatsZones: setHeadersZones();
      end;
    end
  ;


  procedure TFrameOutputStatsTable.setSuperList( sl: TSMIterationOutputSuperList );
    begin
      _superList := sl;

      if( DBFRAMEOUTPUTSTATSTABLE ) then
        _superList.arraysForProductionType( 0 ).descUTotal.debug()
      ;
    end
  ;


  procedure TFrameOutputStatsTable.setZoneStats( val: TZoneOutputSet );
    begin
      _superZoneStats := val;
    end
  ;


  procedure TFrameOutputStatsTable.setCostStats( val: TProductionTypeOutputSet );
    begin
      _superCostStats := val;
    end
  ;


  procedure TFrameOutputStatsTable.setProdType( pt: TProductionType );
    begin
      _pt := pt;

      case _statsType of
        StatsEpi: populateEpi();
        StatsCosts: populateCosts();
        StatsZones: populateZones();
      end;
    end
  ;


  procedure TFrameOutputStatsTable.setZone( z: TZone );
    begin
      _z := z;

      case _statsType of
        StatsEpi: { do nothing };
        StatsCosts: { do nothing };
        StatsZones: populateZones();
      end;
    end
  ;


  function TFrameOutputStatsTable.csvText(): string;
    var
      s: string;
      c, r: integer;
    begin
      dbcout( '++++++++++ Calling TFrameOutputStatsTable.csvText()', DBFRAMEOUTPUTSTATSTABLE );

      result := '';

      for r := 0 to stgGrid.RowCount - 1 do
        begin
          s := '';

          // NOTE: starting at column 1 is different from the typical planned behavior for a base class
          for c := 1 to stgGrid.ColCount - 1 do
            begin
              s := s + stgGrid.Cells[ c, r ];

              if( stgGrid.ColCount - 1 > c ) then
                s := s + ', '
              else
                s := s + endl
              ;
            end
          ;

          result := result + s;
        end
      ;
    end
  ;


  {*
   When the cursor is over the column that contains output names, a click will trigger
   a hint that shows the definition of the output.

   Remember that cell selection must be handled by the owner: instances of this class
   don't know what to do when a cell/column/row is selected.
  }
  procedure TFrameOutputStatsTable.stgGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    var
      c, r: integer;
    begin
      addAppLog( 'TFrameOutputStatsTable.stgGridMouseDown()' );

      try
        stgGrid.MouseToCell( x, y, c, r );
        d:=c;
        e:=r;
        if
          ( 0 > c )
        or
          ( 0 > r )
        or
          ( stgGrid.ColCount - 1 < c )
        or
          ( stgGrid.RowCount - 1 < r )
        then
          exit
        ;

        if(( 1 = c ) or (0 = c) and ( 0 < r )) then
          begin
            _lastMousePoint.x := x;
            _lastMousePoint.y := y;
            Application.CancelHint();
            stgGrid.Hint := _rowHeaders.GetItemByIndex(r-1);
          end
        else
          stgGrid.Hint := ''
        ;
      except
        addAppLog( 'EXCEPTION: caught in TFrameOutputStatsTable.stgGridMouseDown()' );
        // The cursor is probably not over a cell.
        // Do nothing and fail silently.
      end;
      addAppLog( 'Done with TFrameOutputStatsTable.stgGridMouseDown()' );
    end
  ;


  {*
   For reasons that I don't understand, a mouseMove event is triggered immediately after
   a mouseDown event, even if the cursor hasn't moved.  The approach used here will hide
   a hint if the cursor is  moved too far from the point where the initial click occurred.
  }
  procedure TFrameOutputStatsTable.stgGridMouseMove( Sender: TObject; Shift: TShiftState; X, Y: Integer );
    begin
    //dbcout2( 'Start stgGridMouseMove' );
    inherited;
    
    if( ( 3 < abs( x - _lastMousePoint.x ) ) or ( 3 < abs( y - _lastMousePoint.y ) ) ) then
      begin
        _lastMousePoint.x := -1;
        _lastMousePoint.y := -1;
        application.CancelHint();
      end
    ;

    //dbcout2( 'Leave stgGridMouseMove' );

    // This approach for showing hints almost works, but has problems dealing with
    // fixed columns.  Keep the code for potential future use, in case the
    // "bug" in TStringGrid is ever fixed.
    //
    // From http://www.greatis.com/delphicb/tips/lib/components-strgridhint.html
    (*
    var
      R, C: Integer;
    begin
      StringGrid1.MouseToCell(X, Y, C, R);
      with StringGrid1 do
      begin
        if ((Row<>R)or(Col<>C)) then
        begin
          Row:=R;
          Col:=C;
          Application.CancelHint;
          StringGrid1.Hint:=IntToStr(R)+#32+IntToStr(C);
        end;
      end;
    *)
    end
  ;






  procedure TFrameOutputStatsTable.setHeadersEpi();
    var
      colHeaders: TCStringList;
      c, r: integer;
    begin
      dbcout( 'Setting headers', DBFRAMEOUTPUTSTATSTABLE );
      _rowHeaders := TSMIterationOutputArraySet.createEpiOutputDictionary();

      colHeaders := TSimulationResultSet.createCalculationNameList();

      stgGrid.RowCount := _rowHeaders.Count + stgGrid.FixedRows;
      stgGrid.ColCount := colHeaders.Count + stgGrid.FixedCols;

      stgGrid.Cells[ 0, 0 ] := ' ';
      stgGrid.Cells[ 1, 0 ] := 'Output';
      for c := 0 to colHeaders.Count - 1 do
        stgGrid.Cells[ c + stgGrid.FixedCols, 0 ] := colHeaders.at(c)
      ;


      for r := 0 to _rowHeaders.count - 1 do
        stgGrid.cells[ 1, r + stgGrid.FixedRows ] := _rowHeaders.GetKeyByIndex(r)
      ;

      stgGrid.ColWidths[0] := 15;
      stgGrid.ColWidths[1] := stgGrid.ColWidths[1] + 15;

      colHeaders.Free();
    end
  ;

  
  procedure TFrameOutputStatsTable.populateEpi();
    var
      c, r: integer;
      arrSet: TSMIterationOutputArraySet;
      ptID: integer;
    begin
      dbcout( 'Populating output epi stats', DBFRAMEOUTPUTSTATSTABLE );

      stgGrid.Enabled := false;

      clearGrid();

      if( ( nil = _superList ) and not( StatsZones = _statsType ) ) then
        exit
      ;

      if( nil = _pt ) then
        ptID := 0
      else
        ptID := _pt.productionTypeID
      ;

      arrSet := _superList.arraysForProductionType( ptID );

      if( nil <> arrSet ) then
        begin
          for c := 0 to TSimulationResultSet.calculationCount() -1 do
            begin
              for r := 0 to TSMIterationOutputArraySet.epiOutputCount() - 1 do
                stgGrid.Cells[c + stgGrid.FixedCols, r + stgGrid.FixedRows] := arrSet.valueAsString( r, c ) // Yes, r and c are intentionally reversed here.
              ;
            end
          ;
        end
      ;

      stgGrid.Enabled := true;
    end
  ;


  procedure TFrameOutputStatsTable.populateCosts();
    var
      c, r: integer;
      outputSet: TVariableOutputSet;
      ptID: integer;
    begin
      stgGrid.Enabled := false;

      clearGrid();

      if( nil = _superCostStats ) then
        exit
      ;

      if( nil = _pt ) then
        ptID := 0
      else
        ptID := _pt.productionTypeID
      ;

      outputSet := _superCostStats.item( ptID );

      if( nil <> outputSet ) then
        begin
          for c := 0 to TSimulationResultSet.calculationCount() -1 do
            begin
              for r := 0 to TProductionTypeOutputSet.costsOutputCount() - 1 do
                stgGrid.Cells[c + stgGrid.FixedCols, r + stgGrid.FixedRows] :=
                  outputSet.valueAsString( r + TZoneOutputSet.zoneOutputCount(), c ) // Yes, r and c are intentionally reversed here.
              ;
            end
          ;
        end
      ;

      stgGrid.Enabled := true;
    end
  ;


  procedure TFrameOutputStatsTable.populateZones();
    var
      c, r: integer;
      outputSet: TVariableOutputSet;
      ptID: integer;
      zoneID: integer;
    begin
      stgGrid.Enabled := false;

      clearGrid();

      if( nil = _superZoneStats ) then
        exit
      ;

      if( nil = _pt ) then
        ptID := 0
      else
        ptID := _pt.productionTypeID
      ;

      if( nil = _z ) then
        zoneID := 0
      else
        zoneID := _z.id
      ;

      outputSet := _superZoneStats.item( zoneID ).item( ptID );

      if( nil <> outputSet ) then
        begin
          for c := 0 to TSimulationResultSet.calculationCount() -1 do
            begin
              for r := 0 to TZoneOutputSet.zoneOutputCount() - 1 do
                stgGrid.Cells[c + stgGrid.FixedCols, r + stgGrid.FixedRows] := outputSet.valueAsString( r, c ) // Yes, r and c are intentionally reversed here.
              ;
            end
          ;
        end
      ;

      stgGrid.Enabled := true;
    end
  ;


  procedure TFrameOutputStatsTable.setHeadersCosts();
    var
      colHeaders: TCStringList;
      c, r: integer;
    begin
      dbcout( 'Setting headers', DBFRAMEOUTPUTSTATSTABLE );
      _rowHeaders := TProductionTypeOutputSet.createCostsOutputDictionary();

      colHeaders := TSimulationResultSet.createCalculationNameList();

      stgGrid.RowCount := _rowHeaders.Count + stgGrid.FixedRows;
      stgGrid.ColCount := colHeaders.Count + stgGrid.FixedCols;

      stgGrid.Cells[ 0, 0 ] := ' ';
      stgGrid.Cells[ 1, 0 ] := 'Output';
      for c := 0 to colHeaders.Count - 1 do
        stgGrid.Cells[ c + stgGrid.FixedCols, 0 ] := colHeaders.at(c)
      ;


      for r := 0 to _rowHeaders.count - 1 do
        stgGrid.cells[ 1, r + stgGrid.FixedRows ] := _rowHeaders.GetKeyByIndex(r)
      ;

      stgGrid.ColWidths[0] := 15;
      stgGrid.ColWidths[1] := stgGrid.ColWidths[1] + 15;

      colHeaders.Free();
    end
  ;


  procedure TFrameOutputStatsTable.setHeadersZones();
    var
      colHeaders: TCStringList;
      c, r: integer;
    begin
      dbcout( 'Setting headers', DBFRAMEOUTPUTSTATSTABLE );
      _rowHeaders := TZoneOutputSet.createZoneOutputDictionary();

      colHeaders := TSimulationResultSet.createCalculationNameList();

      stgGrid.RowCount := _rowHeaders.Count + stgGrid.FixedRows;
      stgGrid.ColCount := colHeaders.Count + stgGrid.FixedCols;

      stgGrid.Cells[ 0, 0 ] := ' ';
      stgGrid.Cells[ 1, 0 ] := 'Output';
      for c := 0 to colHeaders.Count - 1 do
        stgGrid.Cells[ c + stgGrid.FixedCols, 0 ] := colHeaders.at(c)
      ;


      for r := 0 to _rowHeaders.count - 1 do
        stgGrid.cells[ 1, r + stgGrid.FixedRows ] := _rowHeaders.GetKeyByIndex(r)
      ;

      stgGrid.ColWidths[0] := 15;
      stgGrid.ColWidths[1] := stgGrid.ColWidths[1] + 15;

      colHeaders.Free();
    end
  ;


end.
