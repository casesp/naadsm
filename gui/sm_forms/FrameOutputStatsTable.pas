unit FrameOutputStatsTable;

(*
FrameOutputStatsTable.pas/dfm
-----------------------------
Begin: 2005/12/17
Last revision: $Date: 2013-06-27 19:11:32 $ $Author: areeves $
Version number: $Revision: 1.21.4.6 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <aaron.reeves@naadsm.org>
--------------------------------------------------
Copyright (C) 2005 - 2011 Colorado State University

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
    Menus,
    
    ARSyncGrid,

    QOrderedDictionaries,

		FrameStringGridBase,

    Points,

    ProductionType,
    Zone,
    
    IterationOutputs,
    OutputDescriptions,
    SMDatabase
  ;
  

  type TFrameOutputStatsTable = class( TFrameStringGridBase )
      SaveDialog1: TSaveDialog;
      PopupMenu1: TPopupMenu;
      Exportrawdatatofile1: TMenuItem;
      Copyrawdatatoclipboard1: TMenuItem;
      
      procedure stgGridMouseWheelDown( Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean );
      procedure stgGridMouseWheelUp( Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean );

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

      procedure Exportrawdatatofile1Click(Sender: TObject);
      procedure Copyrawdatatoclipboard1Click(Sender: TObject);

    protected
      _nameColumn: integer;

      _pt: TProductionType;
      _z: TZone;
      _smdb: TSMDatabase;

      _superEpiStats: TProductionTypeOutputSet;
      _superCostStats: TProductionTypeOutputSet;
      _superZonePTStats: TZonePTOutputSet;
      _superZoneStats: TZoneOutputSet;
      _rowHeaders: TQOrderedStringStringDictionary;
      _lastMousePoint: RIntPoint;
      _statsType: TStatsType;

      procedure populateEpi();
      procedure setHeadersEpi();

      procedure populateCosts();
      procedure setHeadersCosts();

      procedure populatePTZones();
      procedure setHeadersPTZones();

      procedure populateZones();
      procedure setHeadersZones();

      procedure translateUI();

      function fileHeader( const isZoneOutput: boolean ): string;
      function resultsAsString( var isZoneOutput: boolean ): string;

    public
      constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

 			function csvText(): string; override;

      procedure writeOutputToFile();
      procedure writeOutputToClipboard();

      procedure setProdType( pt: TProductionType );
      procedure setZone( z: TZone );
      procedure setEpiStats( val: TProductionTypeOutputSet );
      procedure setZonePTStats( val: TZonePTOutputSet );
      procedure setCostStats( val: TProductionTypeOutputSet );
      procedure setZoneStats( val: TZoneOutputSet );
      procedure setOutputType( val: TStatsType );
      procedure setDatabase( db: TSMDatabase );

      property nameColumn: integer read _nameColumn;
    end
  ;


implementation

  {$R *.dfm}

  uses
    Math,
    Printers,
    StrUtils,
    TypInfo,
    Clipbrd,
    
    SqlClasses,
    CStringList,
    MyStrUtils,
    MyDialogs,
    DebugWindow,
    AppLog,
    I88n,

    StringConsts
  ;


  const
    DBSHOWMSG: boolean = false; // Set to true to enable debugging message for this unit


  constructor TFrameOutputStatsTable.create( AOwner: TComponent );
    begin
      inherited create( AOwner );

      dbcout( 'Creating TFrameOutputStatsTable', DBSHOWMSG  );

      _nameColumn := 1;
      _pt := nil;
      _z := nil;

      _superEpiStats := nil;
      _superCostStats := nil;
      _superZonePTStats := nil;
      _superZoneStats := nil;

      _smdb := nil;

      _lastMousePoint.x := -1;
      _lastMousePoint.y := -1;

      _statsType := StaNAADSMStateUnspecified;

      application.HintPause := 10;
      application.HintHidePause := 30000;

      translateUI();

      dbcout( 'Done creating TFrameOutputStatsTable', DBSHOWMSG );
    end
  ;


  procedure TFrameOutputStatsTable.translateUI();
    begin
      SaveDialog1.Filter := tr( 'Plain text file (*.txt)|*.txt|All files (*.*)|*.*' );
      Exportrawdatatofile1.Caption := tr(  'Export raw data for selected output to file...' );
      Copyrawdatatoclipboard1.Caption := tr( 'Copy &raw data for selected output to clipboard' );
    end
  ;


  destructor TFrameOutputStatsTable.destroy();
    begin
      dbcout( 'Destroying TFrameOutputStatsTable', DBSHOWMSG );
      _rowHeaders.free();

      application.HintPause := 500;
      application.HintHidePause := 2500;
      dbcout( 'Done destroying TFrameOutputStatsTable', DBSHOWMSG );

      inherited destroy();
    end
  ;


  procedure TFrameOutputStatsTable.setOutputType( val: TStatsType );
    begin
      _statsType := val;

      case _statsType of
        StatsEpi: setHeadersEpi();
        StatsCosts: setHeadersCosts();
        StatsPTZones: setHeadersPTZones();
        StatsZones: setHeadersZones();
      end;
    end
  ;


  procedure TFrameOutputStatsTable.setEpiStats( val: TProductionTypeOutputSet );
    begin
      _superEpiStats := val;
    end
  ;


  procedure TFrameOutputStatsTable.setZonePTStats( val: TZonePTOutputSet );
    begin
      _superZonePTStats := val;
    end
  ;


  procedure TFrameOutputStatsTable.setCostStats( val: TProductionTypeOutputSet );
    begin
      _superCostStats := val;
    end
  ;


  procedure TFrameOutputStatsTable.setZoneStats( val: TZoneOutputSet );
    begin
      _superZoneStats := val;
    end
  ;


  procedure TFrameOutputStatsTable.setDatabase( db: TSMDatabase );
    begin
      _smdb := db;
    end
  ;


  procedure TFrameOutputStatsTable.setProdType( pt: TProductionType );
    begin
      _pt := pt;

      case _statsType of
        StatsEpi: populateEpi();
        StatsCosts: populateCosts();
        StatsPTZones: populatePTZones();
        StatsZones: { Do nothing };
      end;
    end
  ;


  procedure TFrameOutputStatsTable.setZone( z: TZone );
    begin
      _z := z;

      case _statsType of
        StatsEpi: { do nothing };
        StatsCosts: { do nothing };
        StatsPTZones: populatePTZones();
        StatsZones: populateZones();
      end;
    end
  ;


  function TFrameOutputStatsTable.csvText(): string;
    var
      s, s2: string;
      c, r: integer;
    begin
      dbcout( '++++++++++ Calling TFrameOutputStatsTable.csvText()', DBSHOWMSG );

      result := '';

      for r := 0 to stgGrid.RowCount - 1 do
        begin
          s := '';

          // NOTE: starting at column 1 is different from the typical planned behavior for a base class
          for c := 1 to stgGrid.ColCount - 1 do
            begin
              s2 := stgGrid.Cells[ c, r ];
              if( not( isNan( uiStrToFloat( s2, NaN ) ) ) ) then
                s2 := ansiReplaceStr( s2, SysUtils.DecimalSeparator, csvDecPt )
              ;
              s := s + s2;

              if( stgGrid.ColCount - 1 > c ) then
                s := s + csvListSep + ' '
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


  function TFrameOutputStatsTable.resultsAsString( var isZoneOutput: boolean ): string;
    var
      outputSet: TIterationOutputSet;
      ptID: integer;
      zoneID: integer;
      resultSet: TIterationOutputArray;
      idx: integer;
    begin
      idx := self.stgGrid.Row - 1;

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

      case _statsType of
        StatsEpi:
          begin
            isZoneOutput := false;
            idx := idx + TZoneOutputSet.zoneOutputCount() + TZonePTOutputSet.zonePTOutputCount() + TProductionTypeOutputSet.costsOutputCount();
            outputSet := _superEpiStats.item( ptID );
          end
        ;
        StatsCosts:
          begin
            isZoneOutput := false;
            idx := idx + TZoneOutputSet.zoneOutputCount() + TZonePTOutputSet.zonePTOutputCount();
            outputSet := _superCostStats.item( ptID );
          end
        ;
        StatsPTZones:
          begin
            isZoneOutput := true;
            idx := idx + TZoneOutputSet.zoneOutputCount();
            outputSet := _superZonePTStats.item( zoneID ).item( ptID );
          end
        ;
        StatsZones:
          begin
            isZoneOutput := true;
            // Don't change idx
            outputSet := _superZoneStats.item( zoneID );
          end
        ;
        else
          raise exception.Create( 'There is a problem in TFrameOutputStatsTable.writeOutputToFile()' )
        ;
      end;

      resultSet := outputSet.resultSet( TIterationOutputType( idx ) );

      if
        ( TIterationOutputType( idx ) in [ ZZonesOccurred ] )
      or
        ( TIterationOutputType( idx ) in [ EpiDetOccurred, EpiVaccOccurred, EpiDestrOccurred, EpiDiseaseEnded, EpiOutbreakEnded ] )
      then
        result := intToStr( trunc( resultSet.calculationAtPosition( 0 ) ) ) + ' ' +  tr( 'of' ) + ' ' + intToStr( _smdb.completedIterations ) + ' ' + tr( 'iterations' )
      else
        result := resultSet.asString()
      ;
    end
  ;


  procedure TFrameOutputStatsTable.writeOutputToFile();
    var
      fileSuccess: boolean;
      f: TextFile;
      output: string;
      isZoneOutput: boolean;
    begin
      if( SaveDialog1.Execute() ) then
        begin
          try
            try
              output := resultsAsString( isZoneOutput );

              AssignFile( f, SaveDialog1.FileName );
              Rewrite( f );
              write( f, fileHeader( isZoneOutput ) );
              write( f, output );
              fileSuccess := true;
            except
              fileSuccess := false;
            end;
          finally
            closeFile( f );
          end;

          if( not fileSuccess ) then
            msgOK(
              ansiReplaceStr( tr( 'File xyz' ), 'xyz', abbrevpath( SaveDialog1.FileName ) ) + ' ' + tr( 'could not be written.' ),
              '',
              IMGWarning,
              self
            )
          ;
        end
      ;
    end
  ;


  procedure TFrameOutputStatsTable.writeOutputToClipboard();
    var
      isZoneOutput: boolean;
      str: string;
    begin
      str := resultsAsString( isZoneOutput );
      str := fileHeader( isZoneOutput ) + str;

      ClipBoard.SetTextBuf( PChar( str ) );
    end
  ;


  function TFrameOutputStatsTable.fileHeader( const isZoneOutput: boolean ): string;
    begin
      result := ''
        + '## ' + tr( 'NAADSM raw data for all iterations' ) + endl
        + '## ' + tr( 'Application version:' ) + ' ' + MAJORVERSIONNUMBER + '.' + MINORVERSIONNUMBER + '-' + BUILDNUMBER + endl
        + '## ' + tr( 'Date:' ) + ' ' + dateTimeToStr( now() ) + endl
        + '## ' + tr( 'Scenario file:' ) + ' ' + _smdb.permanentDBFileName + endl
      ;

      if( nil <> _pt ) then
        result := result + '## ' + tr( 'Production type:' ) + ' ' + _pt.productionTypeDescr + endl
      else
        result := result + '## ' + tr( 'Production type:' ) + ' ' + tr( 'All production types' ) + endl
      ;

      if( isZoneOutput ) then
        begin
          if( nil <> _z ) then
            result := result + '## ' + tr( 'Zone:' ) + ' ' + _z.descr + endl
          else
            result := result + '## ' + tr( 'Zone:' ) + ' ' + tr( 'All zones' ) + endl
          ;
        end
      ;

      result := result + '## ' + tr( 'Output name:' ) + ' ' + stgGrid.Cells[ 1, stgGrid.row ] + endl;
      result := result + endl;
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
      try
        stgGrid.MouseToCell( x, y, c, r );

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

        if( ( 2 > c ) and ( 0 < r ) and ( mbRight <> button ) ) then
          begin
            _lastMousePoint.x := x;
            _lastMousePoint.y := y;
            Application.CancelHint();
            stgGrid.Hint := _rowHeaders.GetItemByIndex( r - 1 );
          end
        else
          stgGrid.Hint := ''
        ;

      except
        dbcout( 'EXCEPTION: caught in TFrameOutputStatsTable.stgGridMouseDown()', true );
        // The cursor is probably not over a cell.
        // Do nothing and fail silently.
      end;
    end
  ;


  {*
   For reasons that I don't understand, a mouseMove event is triggered immediately after
   a mouseDown event, even if the cursor hasn't moved.  The approach used here will hide
   a hint if the cursor is  moved too far from the point where the initial click occurred.
  }
  procedure TFrameOutputStatsTable.stgGridMouseMove( Sender: TObject; Shift: TShiftState; X, Y: Integer );
    begin
      inherited;
    
      if( ( 3 < abs( x - _lastMousePoint.x ) ) or ( 3 < abs( y - _lastMousePoint.y ) ) ) then
        begin
          _lastMousePoint.x := -1;
          _lastMousePoint.y := -1;
          application.CancelHint();
        end
      ;

      // This approach for showing hints almost works, but has problems dealing with
      // fixed columns.  Keep the code for potential future use, in case the
      // "bug" in TStringGrid is ever fixed.
      //
      // From http://www.greatis.com/delphicb/tips/lib/components-strgridhint.html
      (*
      var
        R, C: Integer;
      begin
        stgGrid.MouseToCell(X, Y, C, R);
        with stgGrid do
        begin
          if ((Row<>R)or(Col<>C)) then
          begin
            Row:=R;
            Col:=C;
            Application.CancelHint;
            stgGrid.Hint:=IntToStr(R)+#32+IntToStr(C);
          end;
        end;
      *)
    end
  ;






  procedure TFrameOutputStatsTable.setHeadersEpi();
    var
      colHeaders: TCStringList;
      c, r: integer;
      colWidth: integer;
      tmp: integer;
    begin
      dbcout( 'Setting headers', DBSHOWMSG );
      
      _rowHeaders := TProductionTypeOutputSet.createEpiOutputDictionary();

      colHeaders := TIterationOutputArray.createCalculationNameList();

      stgGrid.RowCount := _rowHeaders.Count + stgGrid.FixedRows;
      stgGrid.ColCount := colHeaders.Count + stgGrid.FixedCols;

      stgGrid.Cells[ 0, 0 ] := ' ';
      stgGrid.Cells[ 1, 0 ] := tr( 'Output' );
      for c := 0 to colHeaders.Count - 1 do
        stgGrid.Cells[ c + stgGrid.FixedCols, 0 ] := colHeaders.at(c)
      ;

      colWidth := 0;
      for r := 0 to _rowHeaders.count - 1 do
        begin
          stgGrid.cells[ 1, r + stgGrid.FixedRows ] := _rowHeaders.GetKeyByIndex(r);
          tmp := stgGrid.Canvas.TextWidth( _rowHeaders.GetKeyByIndex(r) );
          if( tmp > colWidth ) then
            colWidth := tmp
          ;
        end
      ;

      stgGrid.ColWidths[0] := 15;
      stgGrid.ColWidths[1] := colWidth + 10;

      colHeaders.Free();
    end
  ;

  
  procedure TFrameOutputStatsTable.populateEpi();
    var
      c, r: integer;
      outputSet: TIterationOutputSet;
      ptID: integer;
    begin
      stgGrid.Enabled := false;
      clearGrid();

      if( nil = _superEpiStats ) then
        exit
      ;

      if( nil = _pt ) then
        ptID := 0
      else
        ptID := _pt.productionTypeID
      ;

      outputSet := _superEpiStats.item( ptID );

      if( nil <> outputSet ) then
        begin
          for c := 0 to TIterationOutputArray.calculationCount() -1 do
            begin
              for r := 0 to TProductionTypeOutputSet.epiOutputCount() - 1 do
                begin
                  stgGrid.Cells[c + stgGrid.FixedCols, r + stgGrid.FixedRows] :=
                    outputSet.valueAsString(
                      r + TZoneOutputSet.zoneOutputCount() + TZonePTOutputSet.zonePTOutputCount() + TProductionTypeOutputSet.costsOutputCount(),
                      c
                    ) // Yes, r and c are intentionally reversed here.
                  ;
                end
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
      outputSet: TIterationOutputSet;
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
          for c := 0 to TIterationOutputArray.calculationCount() -1 do
            begin
              for r := 0 to TProductionTypeOutputSet.costsOutputCount() - 1 do
                stgGrid.Cells[c + stgGrid.FixedCols, r + stgGrid.FixedRows] :=
                  outputSet.valueAsString( r + TZoneOutputSet.zoneOutputCount() + TZonePTOutputSet.zonePTOutputCount(), c ) // Yes, r and c are intentionally reversed here.
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
      outputSet: TIterationOutputSet;
      zoneID: integer;
    begin
      stgGrid.Enabled := false;

      clearGrid();

      if( nil = _superZoneStats ) then
        exit
      ;

      if( nil = _z ) then
        zoneID := 0
      else
        zoneID := _z.id
      ;

      outputSet := _superZoneStats.item( zoneID );

      if( nil <> outputSet ) then
        begin
          for c := 0 to TIterationOutputArray.calculationCount() -1 do
            begin
              for r := 0 to TZoneOutputSet.zoneOutputCount() - 1 do
                // r = 0 is zonesOccurred.  Go ahead and show it for "all zones".
                // Other outputs apply only to specific zones.
                if
                  ( 0 = zoneID )
                and
                  ( not( TIterationOutputType( r ) in [ ZZonesOccurred ] ) )
                then
                  stgGrid.Cells[c + stgGrid.FixedCols, r + stgGrid.FixedRows] := tr( 'n/a' )
                else
                  stgGrid.Cells[c + stgGrid.FixedCols, r + stgGrid.FixedRows] := outputSet.valueAsString( r, c ) // Yes, r and c are intentionally reversed here.
              ;
            end
          ;
        end
      ;

      stgGrid.Enabled := true;
    end
  ;


  procedure TFrameOutputStatsTable.populatePTZones();
    var
      c, r: integer;
      outputSet: TIterationOutputSet;
      ptID: integer;
      zoneID: integer;
    begin
      stgGrid.Enabled := false;

      clearGrid();

      if( nil = _superZonePTStats ) then
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

      outputSet := _superZonePTStats.item( zoneID ).item( ptID );

      if( nil <> outputSet ) then
        begin
          for c := 0 to TIterationOutputArray.calculationCount() -1 do
            begin
              for r := 0 to TZonePTOutputSet.zonePTOutputCount() - 1 do
                stgGrid.Cells[c + stgGrid.FixedCols, r + stgGrid.FixedRows] := outputSet.valueAsString( r + TZoneOutputSet.zoneOutputCount(), c ) // Yes, r and c are intentionally reversed here.
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
      colWidth: integer;
      tmp: integer;
    begin
      dbcout( 'Setting headers', DBSHOWMSG );
      _rowHeaders := TProductionTypeOutputSet.createCostsOutputDictionary();

      colHeaders := TIterationOutputArray.createCalculationNameList();

      stgGrid.RowCount := _rowHeaders.Count + stgGrid.FixedRows;
      stgGrid.ColCount := colHeaders.Count + stgGrid.FixedCols;

      stgGrid.Cells[ 0, 0 ] := ' ';
      stgGrid.Cells[ 1, 0 ] := tr( 'Output' );
      for c := 0 to colHeaders.Count - 1 do
        stgGrid.Cells[ c + stgGrid.FixedCols, 0 ] := colHeaders.at(c)
      ;

      colWidth := 0;
      for r := 0 to _rowHeaders.count - 1 do
        begin
          stgGrid.cells[ 1, r + stgGrid.FixedRows ] := _rowHeaders.GetKeyByIndex(r);
          tmp := stgGrid.Canvas.TextWidth( _rowHeaders.GetKeyByIndex(r) );
          if( tmp > colWidth ) then
            colWidth := tmp
          ;
        end
      ;

      stgGrid.ColWidths[0] := 15;
      stgGrid.ColWidths[1] := colWidth + 10;

      colHeaders.Free();
    end
  ;


  procedure TFrameOutputStatsTable.setHeadersPTZones();
    var
      colHeaders: TCStringList;
      c, r: integer;
      colWidth: integer;
      tmp: integer;
    begin
      dbcout( 'Setting headers', DBSHOWMSG );
      _rowHeaders := TZonePTOutputSet.createZonePTOutputDictionary();

      colHeaders := TIterationOutputArray.createCalculationNameList();

      stgGrid.RowCount := _rowHeaders.Count + stgGrid.FixedRows;
      stgGrid.ColCount := colHeaders.Count + stgGrid.FixedCols;

      stgGrid.Cells[ 0, 0 ] := ' ';
      stgGrid.Cells[ 1, 0 ] := tr( 'Output' );
      for c := 0 to colHeaders.Count - 1 do
        stgGrid.Cells[ c + stgGrid.FixedCols, 0 ] := colHeaders.at(c)
      ;

      colWidth := 0;
      for r := 0 to _rowHeaders.count - 1 do
        begin
          stgGrid.cells[ 1, r + stgGrid.FixedRows ] := _rowHeaders.GetKeyByIndex(r);
          tmp := stgGrid.Canvas.TextWidth( _rowHeaders.GetKeyByIndex(r) );
          if( tmp > colWidth ) then
            colWidth := tmp
          ;
        end
      ;

      stgGrid.ColWidths[0] := 15;
      stgGrid.ColWidths[1] := colWidth + 10;

      colHeaders.Free();
    end
  ;


  procedure TFrameOutputStatsTable.setHeadersZones();
    var
      colHeaders: TCStringList;
      c, r: integer;
      colWidth: integer;
      tmp: integer;
    begin
      dbcout( 'Setting headers', DBSHOWMSG );
      _rowHeaders := TZoneOutputSet.createZoneOutputDictionary();

      colHeaders := TIterationOutputArray.createCalculationNameList();

      stgGrid.RowCount := _rowHeaders.Count + stgGrid.FixedRows;
      stgGrid.ColCount := colHeaders.Count + stgGrid.FixedCols;

      stgGrid.Cells[ 0, 0 ] := ' ';
      stgGrid.Cells[ 1, 0 ] := tr( 'Output' );
      for c := 0 to colHeaders.Count - 1 do
        stgGrid.Cells[ c + stgGrid.FixedCols, 0 ] := colHeaders.at(c)
      ;

      colWidth := 0;
      for r := 0 to _rowHeaders.count - 1 do
        begin
          stgGrid.cells[ 1, r + stgGrid.FixedRows ] := _rowHeaders.GetKeyByIndex(r);
          tmp := stgGrid.Canvas.TextWidth( _rowHeaders.GetKeyByIndex(r) );
          if( tmp > colWidth ) then
            colWidth := tmp
          ;
        end
      ;

      stgGrid.ColWidths[0] := 15;
      stgGrid.ColWidths[1] := colWidth + 10;

      colHeaders.Free();
    end
  ;


  procedure TFrameOutputStatsTable.stgGridMouseWheelDown(Sender: TObject;
    Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    begin
      inherited;
      {
        Prevents "Grid index out of range" error. When the selected row is the
        first or last the control sets it's column to it's col count  when the
        mouse wheel is used. This raises Grid.pas EInvalidGridOperation because
        col must less than col count. This odd behavior only happens when
        the makeSyncGridPair method is used, not on single grids.
      }
      if stgGrid.Col >= stgGrid.ColCount then stgGrid.Col := (stgGrid.ColCount - 1);
    end
  ;


  procedure TFrameOutputStatsTable.stgGridMouseWheelUp(Sender: TObject;
    Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    begin
      inherited;
      {
        Prevents "Grid index out of range" error. When the selected row is the
        first or last the control sets it's column to it's col count  when the
        mouse wheel is used. This raises Grid.pas EInvalidGridOperation because
        col must less than col count. This odd behavior only happens when
        the makeSyncGridPair method is used, not on single grids.
      }

      if stgGrid.Col >= stgGrid.ColCount then stgGrid.Col := (stgGrid.ColCount - 1);
    end
  ;

  
  procedure TFrameOutputStatsTable.Exportrawdatatofile1Click( Sender: TObject );
    begin
      inherited;
      writeOutputToFile();
    end
  ;


  procedure TFrameOutputStatsTable.Copyrawdatatoclipboard1Click( Sender: TObject );
    begin
      inherited;
      writeOutputToClipboard();
    end
  ;



end.
