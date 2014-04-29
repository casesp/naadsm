unit GlobalControlParamsList;

(*
GlobalControlParamsList.pas
----------------------------
Begin: 2005/06/10
Last revision: $Date: 2010-06-15 01:12:06 $ $Author: areeves $
Version number: $Revision: 1.16.12.1 $
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
  	Models,
  	Contnrs,
    GlobalControlParams,
    FunctionEnums,
    ChartFUnction
  ;


  type TGlobalControlParamsList = class( TModelList )
      private
        procedure setObject( index: integer; item: TGlobalControlParams );
        function getObject( index: integer ): TGlobalControlParams;

      public
      	constructor create(); overload;
        destructor destroy(); override;

        //Inherited functions
        procedure removeChart( const chartName: string ); override;

        procedure changeChart(
          const whichChart: TSMChart;
          const oldChartName: string;
          newChart: TChartFunction;
          addlInfo: integer = -1
        ); override;

        function functionsAreValid(): boolean; override;

        // Typical list functions
        function append( dm: TGlobalControlParams ): integer; reintroduce;
        procedure insert( index: integer; dm: TGlobalControlParams );
        property objects[ index: integer]: TGlobalControlParams read getObject write setObject; default;

        function at( i: word ): TGlobalControlParams;
    end
  ;


  type TGlobalControlParamsListIterator = class( TModelListIterator )
    public
      function toFirst(): TGlobalControlParams;
      function toLast(): TGlobalControlParams;
      function current(): TGlobalControlParams;
  	end
  ;

implementation

	uses
  	SysUtils
  ;



//-----------------------------------------------------------------------------
// Iterator: Basic functions
//-----------------------------------------------------------------------------
	function TGlobalControlParamsListIterator.toFirst(): TGlobalControlParams;
  	begin
      result := _toFirst() as TGlobalControlParams;
    end
  ;

	function TGlobalControlParamsListIterator.toLast(): TGlobalControlParams;
  	begin
      result := _toLast() as TGlobalControlParams;
    end
  ;

  function TGlobalControlParamsListIterator.current(): TGlobalControlParams;
  	begin
      result := _current() as TGlobalControlParams;
    end
  ;
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// List: Construction/initialization/destruction
//-----------------------------------------------------------------------------
	constructor TGlobalControlParamsList.create();
  	begin
    	inherited create( true );
    end
  ;


  destructor TGlobalControlParamsList.destroy();
  	begin
    	inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------

  procedure TGlobalControlParamsList.removeChart( const chartName: string );
    var
      it: TGlobalControlParamsListIterator;
    begin
      it := TGlobalControlParamsListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          it.current().removeChart( chartName );
          it.incr();
        end
      ;

      it.Free();
    end
  ;


  procedure TGlobalControlParamsList.changeChart(
        const whichChart: TSMChart;
        const oldChartName: string;
        newChart: TChartFunction;
        addlInfo: integer = -1
      );
    var
      it: TGlobalControlParamsListIterator;
    begin

      it := TGlobalControlParamsListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          it.current().changeChart( whichChart, oldChartName, newChart, addlInfo );
          it.incr();
        end
      ;

      it.Free();
    end
  ;


  function TGlobalControlParamsList.functionsAreValid(): boolean;
    var
      it: TGlobalControlParamsListIterator;
    begin
      result := true;

      it := TGlobalControlParamsListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          if( not it.current().functionsAreValid() ) then
            result := false
          ;
          it.incr();
        end
      ;

      it.free();
    end
  ;


//-----------------------------------------------------------------------------
// List: Typical list functions
//-----------------------------------------------------------------------------
  function TGlobalControlParamsList.append( dm: TGlobalControlParams ): integer;
    begin
      result := inherited Add( dm );
    end
  ;


  procedure TGlobalControlParamsList.setObject( index: integer; item: TGlobalControlParams );
    begin
      inherited SetItem( index, item );
    end
  ;


  function TGlobalControlParamsList.getObject( index: integer ): TGlobalControlParams;
    begin
      result := inherited GetItem( index ) as TGlobalControlParams;
    end
  ;


  procedure TGlobalControlParamsList.insert(index: integer; dm: TGlobalControlParams);
    begin
      inherited Insert(index, dm);
    end
  ;


  (*
  function TGlobalControlParamsList.first() : TGlobalControlParams;
    begin
      _currentIndex := 0;
      if( self.Count = 0 ) then
      	result := nil
      else
      	result := getObject( _currentIndex )
      ;
    end
  ;


  function TGlobalControlParamsList.last() : TGlobalControlParams;
    begin
      if( self.Count = 0 ) then
      	result := nil
      else
        begin
          _currentIndex := self.Count - 1;
          result := getObject( _currentIndex );
        end
      ;
    end
  ;


  function TGlobalControlParamsList.next() : TGlobalControlParams;
    begin
      _currentIndex := _currentIndex + 1;
      if( _currentIndex > (self.Count - 1) ) then
      	result := nil
      else
      	result := getObject( _currentIndex )
      ;
    end
  ;


  function TGlobalControlParamsList.current() : TGlobalControlParams;
    begin
      if( _currentIndex > (self.Count - 1) ) then
      	result := nil
      else
      	result := getObject( _currentIndex )
      ;
    end
  ;
  *)

  function TGlobalControlParamsList.at( i: word ): TGlobalControlParams;
  	begin
      if( i > self.Count-1 ) then
      	raise exception.Create( 'Index out of bounds in TGlobalControlParamsList' )
      else
      	result := getObject( i )
      ;
    end
  ;
//-----------------------------------------------------------------------------



end.


