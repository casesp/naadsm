unit FunctionEnums;

(*
FunctionEnums.pas
-----------------
Begin: 2005/06/08
Last revision: $Date: 2008/11/25 22:04:42 $ $Author: areeves $
Version number: $Revision: 1.1 $
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

  {*
   The definitions in this type must be specified for every simulation model application
   that uses either a probability density function or a relational function.

   There is nothing in the functions below or in the implementation section that
   should require application-specific changes.
  }
  type TSMChart = (
    // Standard unknown/unspecified type.
        // This type MUST be the first one in the list!
  	XNoChart,

    // Length of the various disease states (TProductionType)
    DLatent, // PDF
    DSubclinical, // PDF
    DClinical, // PDF
    DImmune, // PDF
    DPrevalence, // Rel

    // Direct and indirect contact (TContactModel)
    CMDistanceDirect,  // PDF
    CMDelayDirect, // PDF
    CMMovementControlDirect, // Rel
    CMDistanceIndirect, // PDF
    CMDelayIndirect, // PDF
    CMMovementControlIndirect, // Rel

    // Airborne spread
    AIRDelay, // PDF

    // Detection parameters (TDetectionParams)
    DetProbObsVsTimeClinical, // Rel
    DetProbReportVsFirstDetection, // Rel

    // Vaccination parameters
    VacImmunePeriod, // PDF

    // Global control parameters
    DestrCapacityGlobal, // Rel
    VaccCapacityGlobal,	// Rel

    // Zone movement restrictions
    ZONMovementDirect, // Rel
    ZONMovementIndirect, // Rel

    // This type MUST be the last one in the list!
    XLastChartInList
  );


  { Returns the string definition (name) that corresponds to the designated chart type. }
  function smChartStr( val: TSMChart ): string;

  { Returns the chart type that corresponds to the indicated string definition/name. }
  function strToSMChart( val: string ): TSMChart;

implementation

	uses
  	SysUtils,
  	TypInfo
  ;

//-----------------------------------------------
// Helper functions
//-----------------------------------------------
  function smChartStr( val: TSMChart ): string;
  	begin
			result := getEnumName( TypeInfo( TSMChart ), ord( val ) );
    end
  ;


  function strToSMChart( val: string ): TSMChart;
    var
      i: TSMChart;
      chartTypeName: string;
    begin
      result := XNoChart;

      for i := XNoChart to XLastChartInList do
        begin
          chartTypeName := getEnumName( TypeInfo( TSMChart ), ord( i ) );

          if( val = chartTypeName ) then
            begin
              result := i;
              break;
            end
          ;
        end
      ;

      if( XNoChart = result ) then
        raise exception.Create( 'Unrecognized TSMChart in strToSMChart: ' + val )
      ;
    end
  ;

(*
  function strToSMChart( val: string ): TSMChart;
    begin
    	if( val = 'DLatent' ) then
      	result := DLatent
      else if( val = 'DSubclinical' ) then
      	result := DSubclinical
      else if( val = 'DClinical' ) then
      	result := DClinical
      else if( val = 'DImmune' ) then
      	result := DImmune
      else if( val = 'DPrevalence' ) then
        result := DPrevalence
      else if( val = 'CMDistanceDirect' ) then
      	result := CMDistanceDirect
      else if( val = 'CMDelayDirect' ) then
      	result := CMDelayDirect
      else if( val = 'CMMovementControlDirect' ) then
      	result := CMMovementControlDirect
      else if( val = 'CMDistanceIndirect' ) then
      	result := CMDistanceIndirect
      else if( val = 'CMDelayIndirect' ) then
      	result := CMDelayIndirect
      else if( val = 'CMMovementControlIndirect' ) then
      	result := CMMovementControlIndirect
      else if( val = 'AIRDelay' ) then
      	result := AIRDelay
			else if( val = 'DetProbObsVsTimeClinical' ) then
      	result := DetProbObsVsTimeClinical
    	else if( val = 'DetProbReportVsFirstDetection' ) then
      	result := DetProbReportVsFirstDetection
      else if( val = 'VacImmunePeriod' ) then
      	result := VacImmunePeriod
      else if( val = 'DestrCapacityGlobal' ) then
      	result := DestrCapacityGlobal
      else if( val = 'VaccCapacityGlobal' ) then
      	result := VaccCapacityGlobal
      else if( val = 'ZONMovementDirect' ) then
        result := ZONMovementDirect
      else if( val = 'ZONMovementIndirect' ) then
        result := ZONMovementIndirect
      else
      	begin
      		raise exception.Create( 'Unrecognized TSMChart in strToSMChart: ' + val );
          result := XNoChart;
        end
      ;
    end
  ;
*)
//-----------------------------------------------

end.
