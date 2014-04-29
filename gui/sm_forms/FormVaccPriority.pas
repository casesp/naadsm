unit FormVaccPriority;

(*
FormVaccPriority.pas/dfm
-------------------------
Begin: 2005/06/08
Last revision: $Date: 2008/11/25 22:00:30 $ $Author: areeves $
Version: $Revision: 1.20 $
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
    StdCtrls,
    ExtCtrls,
    Buttons,
    Menus,
    ActnPopupCtrl,

    FormSMWizardBase,
    FormPriorityBase,
    ProductionType,
    ProductionTypeList
  ;

  type TFormVaccPriority = class( TFormPriorityBase )
		protected
  		procedure fillReasons(); override;
      procedure updateReasons(); override;
      procedure updateScenarioData(); override;
      procedure fillProdTypes(); override;

    protected
      procedure translateUI();

		public
			constructor create( AOwner: TComponent ); override;

			function showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer; override;

		end
	 ;

implementation

{$R *.dfm}

	uses
    DebugWindow,
    SqlClasses,
    CStringList,
    MyStrUtils,
    GuiStrUtils,
    QStringMaps,
    QIntegerMaps,
    I88n
  ;


  constructor TFormVaccPriority.create( AOwner: TComponent );
  	begin
      inherited create( AOwner );
      translateUI();

      ReasonForActivity := tr( 'Reason for vaccination' );
      //sortOrder := OrderVaccPriority;
      //prodTypePriorityColumn := 'vaccPriority';
    end
  ;


  procedure TFormVaccPriority.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:54 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormVaccPriority.dfm
      // File date: Wed Oct 25 14:50:18 2006

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Scenario parameters: Vaccination priorities' );
          pnlCaption.Caption := tr( 'Vaccination priorities' );
        end
      ;

    end
  ;
  

  procedure TFormVaccPriority.fillReasons();
    var
      list: TCStringList;
      str: pchar;
    begin
      if( length( _smScenarioCopy.simInput.controlParams.vaccPriorityOrder ) > 0 ) then
      	begin
      		// figure out the previously specified order
          list := TCStringList.create( _smScenarioCopy.simInput.controlParams.vaccPriorityOrder, ',' );

          str := list.first();
          while( nil <> str ) do
          	begin
              if( 'production type' = str ) then lbxPrimary.AddItem( tr( 'Production type' ), nil )
              else if( 'reason' = str ) then lbxPrimary.AddItem( tr( 'Reason for vaccination' ), nil )
              else if( 'time waiting' = str ) then lbxPrimary.AddItem( tr( 'Days holding' ), nil )
              else raise exception.Create( 'FormVaccPriority: unrecognized primary reason ''' +  str + ''' in fillReasons().');
              ;
            	str := list.next();
            end
          ;
          list.free();
        end
      else
        begin
        	// use default order
          lbxPrimary.AddItem( tr( 'Reason for vaccination' ), nil );
          lbxPrimary.AddItem( tr( 'Production type' ), nil );
          lbxPrimary.AddItem( tr( 'Days holding' ), nil );
        end
      ;

      lbxReason.AddItem( tr( 'Ring' ), nil );
    end
  ;


  procedure TFormVaccPriority.fillProdTypes();
  	var
    	it: TProductionTypeListIterator;
      orderList: Array of String;
      str: String;
      Index: Integer;

      maxIdx: integer;
    begin
      maxIdx := -1;
			_ptList := _smScenarioCopy.simInput.ptList;
      setLength( orderList, _ptList.Count );
      str := 'ring';

      it := TProductionTypeListIterator.create( _ptList );
      it.toFirst();

      while ( nil <> it.current() ) do
        begin
          Index := (_smScenarioCopy.simInput.controlParams.ssVaccPriorities.Item[ it.current().productionTypeDescr + '+' + str ] - 1);

          if( index > maxIdx ) then
            maxIdx := index
          ;
          // This ensures that newly added production types are tacked on to the end of the
          // priority list.
          if( -1 = index ) or ( -2 = index ) then
            begin
              inc( maxIdx );
              index := maxIdx;
              _smScenarioCopy.simInput.controlParams.ssVaccPriorities[it.current().productionTypeDescr + '+' + str] := index;
              it.current().ringVaccParams.updated := true;
            end
          ;

          orderList[Index] := it.current().productionTypeDescr;
          it.incr();
        end;

      for Index := 0 to length( orderList ) - 1 do
      	begin
          lbxProdType.AddItem( orderList[Index], nil );
        end
      ;

      it.free();
    end
  ;

  procedure TFormVaccPriority.updateScenarioData();
    var
      pt: TProductionType;
      reasonList: TCStringList;
      reason: pchar;
      i,j: integer;
      vaccOrderList:TQStringLongIntMap;
      vaccReasonOrder:String;
      vaccPriorityOrder:String;
    begin
      updateReasons();

      vaccReasonOrder := 'ring'; //_smScenarioCopy.simInput.controlParams.vaccReasonOrder;
      vaccPriorityOrder :=_smScenarioCopy.simInput.controlParams.vaccPriorityOrder;

      vaccOrderList := TQStringLongIntMap.create();

      reasonList := TCStringList.create( vaccReasonOrder, ',' );

      i := 1;

       if( pos( 'production type', vaccPriorityOrder ) < pos( 'reason', vaccPriorityOrder ) ) then
      	begin
        	// Production type is higher priority than vaccination reason
          // So production type is the outer loop

           for j := 0 to lbxProdType.Items.Count-1 do
             begin
               pt := _ptList.findProdType( lbxProdType.Items.Strings[j] );
               if ( nil <> pt ) then
               	begin
                	reason := reasonList.first();
                  while( nil <> reason ) do
                  	begin
    									vaccOrderList[ pt.productionTypeDescr + '+' + reason ] := i;
                    	inc( i );
                  		reason := reasonList.next();
                    end
                  ;
                  pt.ringVaccParams.vaccPriority := j + 1;
                end
             ;
          end;  // End For loop
        end
      else
      	begin
        	// Vaccination reason is higher priority than production type
          // So destruction reason is the outer loop
          reason := reasonList.first();
          while( nil <> reason ) do
          	begin
              for j := 0 to lbxProdType.Items.Count-1 do
                begin
                  pt := _ptList.findProdType( lbxProdType.Items.Strings[j] );
                  if ( nil <> pt ) then
                    begin
                    	vaccOrderList[ pt.productionTypeDescr + '+' + reason ] := i;
                    	inc( i );
                      pt.ringVaccParams.vaccPriority := j + 1;
                    end;
                end
              ;
            	reason := reasonList.next();
            end
          ;
        end
      ;
      _smScenarioCopy.simInput.controlParams.ssVaccPriorities := vaccOrderList;
      freeAndNil( reasonList );
      inherited updateScenarioData();
    end
  ;


	function TFormVaccPriority.showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer;
  	begin
    	if( _smScenarioCopy.simInput.includeVaccinationGlobal ) then
    		result := inherited showModal( nextFormToShow, formDisplayed, currentFormIndex )
      else
      	begin
          formDisplayed := false;
      		nextForm := nextFormToShow;
          result := 0;
        end
      ;
    end
  ;


  procedure TFormVaccPriority.updateReasons();
    var
      str: string;
      i: integer;
		begin
      // primary vaccination priorities
      str := '';

      for i := 0 to lbxPrimary.Count-1 do
        begin
          if( tr( 'Reason for vaccination' ) = lbxPrimary.Items[i] ) then str := str + 'reason'
          else if( tr( 'Production type' ) = lbxPrimary.Items[i] ) then str := str + 'production type'
          else if( tr( 'Days holding' ) = lbxPrimary.Items[i] ) then str := str + 'time waiting'
          else raise exception.create( 'FormVaccPriority: unrecognized primary reason ''' + lbxPrimary.Items[i] + ''' in updateReasons().' )
          ;

          if( lbxPrimary.Count-1 <> i ) then str := str + ',';
        end
      ;

       _smScenarioCopy.simInput.controlParams.vaccPriorityOrder := str;
    end
  ;


initialization
	RegisterClass( TFormVaccPriority );

end.
