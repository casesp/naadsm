unit AirborneSpreadParams;

(*
AirborneSpreadParams.pas
------------------------
Begin: 2005/01/21
Last revision: $Date: 2011-06-14 14:46:45 $ $Author: rhupalo $
Version number: $Revision: 1.7.6.1 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2005 - 2009 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
    SysUtils,
    Dialogs,
    SMDatabase,
    SqlClasses,
    Models,
    LocalAreaSpreadParams
  ;

  type TAirborneSpreadParams = class( TLocalAreaSpreadBase )
    public
      constructor create(); overload;
      constructor create( sim: TObject; toProdTypeID, fromProdTypeID: integer ); overload;
      constructor create( const src: TAirborneSpreadParams; sim: TObject ); overload;

      constructor create(
        db: TSMDatabase;
        modelID: integer;
        sim: TObject;
        srcID: integer;
        destID: integer;
        populateFromDB: boolean
      ); overload;

      property useAirborne: boolean read getThisModelIsUsed write setThisModelIsUsed;
    end
  ;


implementation

  uses
    StrUtils,
    Variants,

    MyStrUtils,
    DebugWindow,
    I88n,

    SMSimulationInput,
    ProductionType,
    FunctionDictionary
  ;
  
  const
    DBSHOWMSG: boolean = false; // set to true to enable debugging messages for this unit.

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TAirborneSpreadParams.create();
    begin
      inherited create();
      _modelType := MTAirborne;
    end
  ;


  constructor TAirborneSpreadParams.create( sim: TObject; toProdTypeID, fromProdTypeID: integer );
    begin
      inherited create( sim, toProdTypeID, fromProdTypeID );
      _modelType := MTAirborne;
      //rbh20110614 Fix Me! This hack addresses a problem caused by TLocalAreaSpreadBase.createXmlModelList()
      // Having both local-area and airborne models in the list creates BOTH models when importing local-area only.
      self.xmlModelList.clear;
      self.xmlModelList.Append( 'airborne-spread-model' );
    end
  ;


  constructor TAirborneSpreadParams.create( const src: TAirborneSpreadParams; sim: TObject );
    begin
      inherited create( src, sim );
      _modelType := MTAirborne;
    end
  ;


  constructor TAirborneSpreadParams.create(
        db: TSMDatabase;
        modelID: integer;
        sim: TObject;
        srcID: integer;
        destID: integer;
        populateFromDB: boolean
      );
    begin
      inherited create( db, modelID, sim, srcID, destID, populateFromDB );
      _modelType := MTAirborne;
    end
  ;

//-----------------------------------------------------------------------------




end.
