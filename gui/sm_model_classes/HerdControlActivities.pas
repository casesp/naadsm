unit HerdControlActivities;

(*
HerdControlActivities.pas
-------------------------
Begin: 2009/09/02
Last revision: $Date: 2010-06-26 00:11:28 $ $Author: areeves $
Version number: $Revision: 1.2.4.3 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2009 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

{$INCLUDE ../Defs.inc}

interface

  uses
    QIntegerMaps
  ;

  type THerdControlActivities = class
    protected
      // Outputs for single iterations
      //------------------------------
      _itInfections: integer;
      _itDetections: integer;
      _itDestructions: integer;
      _itVaccinations: integer;

      _isQueuedForDestr: boolean;
      _cumulDaysInDestrQueue: integer;

      _vaccQueueDayList: TQIntegerIntegerMap;
      _cumulDaysInVaccQueue: integer;

      // Outputs for multiple iterations
      //--------------------------------
      _simVaccinations: integer;
      _simInfections: integer;
      _simDetections: integer;
      _simDestructions: integer;

      // Properties for one iteration
      //-----------------------------
      function getIsQueuedForVacc(): boolean;

      // Properties for multiple iterations
      //-----------------------------------
      function getCumulVaccinations(): integer;
      function getCumulInfections(): integer;
      function getCumulDetections(): integer;
      function getCumulDestructions(): integer;

      procedure setCumulVaccinations( val: integer );
      procedure setCumulInfections( val: integer );
      procedure setCumulDetections( val: integer );
      procedure setCumulDestructions( val: integer );


      // Houskeeping properties
      //-----------------------
      function getUpdated(): boolean;

    public
      constructor create(); overload;
      constructor create( const src: THerdControlActivities ); overload;
      destructor destroy(); override;

      procedure assign( const src: THerdControlActivities );

      procedure initialize();

      procedure initializeAllOutputRecords();
      procedure prepareForIteration();
      procedure processIterationRecords();

      // FIX ME: Think about how to handle this.
      procedure infectHerd( const wasInitialEvent: boolean );
      procedure detectHerd();

      procedure queueForDestruction( const day: integer );
      procedure destroyHerd( const day, queueDay: integer );

      procedure queueForVaccination( const day: integer );
      procedure vaccinateHerd( const day, queueDay: integer );
      procedure cancelHerdVaccination( const queueDay: integer );

      property itInfections: integer read _itInfections;
      property itDetections: integer read _itDetections;
      property itDestructions: integer read _itDestructions;
      property itVaccinations: integer read _itVaccinations;

      // Output properties
      //------------------
      property cumulVaccinations: integer read getCumulVaccinations write setCumulVaccinations;
      property cumulInfections: integer read getCumulInfections write setCumulInfections;
      property cumulDetections: integer read getCumulDetections write setCumulDetections;
      property cumulDestructions: integer read getCumulDestructions write setCumulDestructions;

      property isQueuedForVacc: boolean read getIsQueuedForVacc;
      property isQueuedForDestr: boolean read _isQueuedForDestr;

      property updated: boolean read getUpdated;
    end
  ;

implementation

  uses
    SysUtils,

    DebugWindow
  ;

  constructor THerdControlActivities.create();
    begin
      inherited create();
      initialize();
    end
  ;


  constructor THerdControlActivities.create( const src: THerdControlActivities );
    begin
      inherited create();
      assign( src );
    end
  ;

  procedure THerdControlActivities.assign( const src: THerdControlActivities );
    begin
      _itInfections := src._itInfections;
      _itDetections := src._itDetections;
      _itDestructions := src._itDestructions;
      _itVaccinations := src._itVaccinations;

      _isQueuedForDestr := src._isQueuedForDestr;
      _cumulDaysInDestrQueue := src._cumulDaysInDestrQueue;

      _vaccQueueDayList:= TQIntegerIntegerMap.create( src._vaccQueueDayList );
      _cumulDaysInVaccQueue := src._cumulDaysInVaccQueue;

      _simVaccinations := src._simVaccinations;
      _simInfections := src._simInfections;
      _simDetections := src._simDetections;
      _simDestructions := src._simDestructions;
    end
  ;


  procedure THerdControlActivities.initialize();
    begin
      // Outputs for single iterations
      _itInfections := 0;
      _itDetections := 0;
      _itDestructions := 0;
      _itVaccinations := 0;

      _isQueuedForDestr := false;

      _cumulDaysInDestrQueue := 0;

      _vaccQueueDayList := TQIntegerIntegerMap.create();
      _cumulDaysInVaccQueue := 0;

      // Outputs for multiple iterations
      _simVaccinations := 0;
      _simInfections := 0;
      _simDetections := 0;
      _simDestructions := 0;
    end
  ;


  destructor THerdControlActivities.destroy();
    begin
      _vaccQueueDayList.free();

      inherited destroy();
    end
  ;


  procedure THerdControlActivities.initializeAllOutputRecords();
    begin
      _simInfections := 0;
      _simDetections := 0;
      _simDestructions := 0;
      _simVaccinations := 0;
    end
  ;


  procedure THerdControlActivities.prepareForIteration();
    begin
      _itInfections := 0;
      _itDetections := 0;
      _itDestructions := 0;
      _itVaccinations := 0;

      _isQueuedForDestr := false;
      _cumulDaysInDestrQueue := 0;

      _vaccQueueDayList.clear();
      _cumulDaysInVaccQueue := 0;
    end
  ;


  procedure THerdControlActivities.processIterationRecords();
    begin
      inc( _simInfections, _itInfections );
      inc( _simDetections, _itDetections );
      inc( _simVaccinations, _itVaccinations );
      inc( _simDestructions, _itDestructions );
    end
  ;


  procedure THerdControlActivities.infectHerd( const wasInitialEvent: boolean );
    begin
      if( wasInitialEvent ) then
        inc( _simInfections )
      else
        inc( _itInfections )
      ;
    end
  ;


  procedure THerdControlActivities.detectHerd();
    begin
      inc( _itDetections );
    end
  ;


  procedure THerdControlActivities.destroyHerd( const day, queueDay: integer );
    begin
      inc( _itDestructions );

      if( 1 < _itDestructions ) then
        raise exception.Create( 'A herd has been destroyed multiple times during an iteration.' )
      ;

      // Deal with time in destruction queue
      //------------------------------------
      if( ( not isQueuedForDestr ) ) then
        raise exception.create( 'A unit is being destroyed but was never in the destruction queue' )
      else
        begin
          //dbcout2( '+++ Herd destroyed on day ' + intToStr( day ) + ', queue day was ' + intToStr( _destrQueueDay ) );
          _isQueuedForDestr := false;
          inc( _cumulDaysInDestrQueue, day - queueDay );
        end
      ;
    end
  ;


  procedure THerdControlActivities.queueForVaccination( const day: integer );
    begin
      _vaccQueueDayList.Add( day, day );
    end
  ;


  procedure THerdControlActivities.vaccinateHerd( const day, queueDay: integer );
    begin
      inc( _itVaccinations );

      // Deal with time in vaccination queue.  Recall that units can be vaccinated several times.
      //-----------------------------------------------------------------------------------------
      if( not isQueuedForVacc ) then
        raise exception.create( 'A unit is being vaccinated that is not in the vaccination queue' )
      else
        begin
          inc( _cumulDaysInVaccQueue, day - queueDay );
          _vaccQueueDayList.remove( queueDay );
        end
      ;
    end
  ;


  procedure THerdControlActivities.cancelHerdVaccination( const queueDay: integer );
    begin
      _vaccQueueDayList.remove( queueDay );
    end
  ;


  procedure THerdControlActivities.queueForDestruction( const day: integer );
    begin
      if( _isQueuedForDestr ) then
        raise exception.Create( 'Unit is already queued for destruction' )
      else
        _isQueuedForDestr := true
      ;
    end
  ;


  function THerdControlActivities.getUpdated(): boolean;
    begin
      result :=
           ( 0 <> _itInfections )
        or ( 0 <> _itDetections )
        or ( 0 <> _itDestructions )
        or ( 0 <> _itVaccinations )
      ;
    end
  ;


//-----------------------------------------------------------------------------
// Properties for one iteration
//-----------------------------------------------------------------------------
  function THerdControlActivities.getIsQueuedForVacc(): boolean;
    begin
      // If there are any items in the queue day list, then this unit is in the queue.
      // If the queue is empty, then this unit is not in the queue.
      result := not( _vaccQueueDayList.isEmpty );
    end
  ;
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// Outputs for multiple iterations
//-----------------------------------------------------------------------------
  procedure THerdControlActivities.setCumulVaccinations( val: integer ); begin _simVaccinations := val; end;
  procedure THerdControlActivities.setCumulInfections( val: integer ); begin _simInfections := val; end;
  procedure THerdControlActivities.setCumulDetections( val: integer ); begin _simDetections := val; end;
  procedure THerdControlActivities.setCumulDestructions( val: integer ); begin _simDestructions := val; end;

  function THerdControlActivities.getCumulVaccinations(): integer; begin result := _simVaccinations; end;
  function THerdControlActivities.getCumulInfections(): integer; begin result := _simInfections; end;
  function THerdControlActivities.getCumulDetections(): integer; begin result := _simDetections; end;
  function THerdControlActivities.getCumulDestructions(): integer; begin result := _simDestructions; end;
//-----------------------------------------------------------------------------

end.