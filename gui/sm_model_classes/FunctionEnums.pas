unit FunctionEnums;

(*
FunctionEnums.pas
-----------------
Begin: 2005/06/08
Last revision: $Date: 2009-11-13 16:41:26 $ $Author: areeves $
Version number: $Revision: 1.4.4.1 $
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

    // Direct and indirect contact (TContactSpreadParams)
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
    VaccCapacityGlobal, // Rel

    // Zone movement restrictions
    ZONMovementDirect, // Rel
    ZONMovementIndirect, // Rel

    // Tracing
    TrDelay, // PDF

    // Diagnostic testing
    TeDelay, // PDF

    // This type MUST be the last one in the list!
    XLastChartInList
  );

  type TChartSet = set of TSMChart;

  { Returns the string definition (name) that corresponds to the designated chart type. }
  function smChartStr( val: TSMChart ): string;

  { Returns the chart type that corresponds to the indicated string definition/name. }
  function strToSMChart( val: string ): TSMChart;

implementation

  uses
    SysUtils,
    TypInfo
  ;

//-----------------------------------------------------------------------------
// Helper functions
//-----------------------------------------------------------------------------
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
//-----------------------------------------------------------------------------

end.
