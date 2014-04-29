unit FormVaccPriority;

(*
FormVaccPriority.pas/dfm
-------------------------
Begin: 2005/06/08
Last revision: $Date: 2010-09-09 14:34:02 $ $Author: rhupalo $
Version: $Revision: 1.26.10.2 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2005 - 2010 Animal Population Health Institute, Colorado State University

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
    QIntegerMaps,
    I88n
  ;


  constructor TFormVaccPriority.create( AOwner: TComponent );
  	begin
      inherited create( AOwner );
      translateUI();

      ReasonForActivity := tr( 'Reason for vaccination' );
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
          lblNoProdTypes.Caption := tr( '(Vaccination is not used with any production type)' );
          lblNoReasons.Caption := tr( '(Vaccination is never used)' );
        end
      ;

    end
  ;
  

  procedure TFormVaccPriority.fillReasons();
    var
      list: TCStringList;
      str: pchar;
      it: TProductionTypeListIterator;
      nRing: integer;
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

      // Before filling in the reasons list box, determine which reasons are actually used.
      // Show only these reasons in use.
      nRing := 0;

      it := TProductionTypeListIterator.create( _ptList );

      it.toFirst();
      while( nil <> it.current() ) do
        begin
          if( nil = it.current().vaccinationParams ) then
            raise exception.Create( 'pt.vaccinationParams is nil in TFormVaccPriority.fillReasons()' )
          ;

          if( it.current().vaccinationParams.useVaccination ) then
            inc( nRing )
          ;
          it.incr();
        end
      ;

      it.Free();

      // Now fill in the reasons list box.
      if( length( _smScenarioCopy.simInput.controlParams.vaccReasonOrder ) > 0 ) then
      	begin
      		// figure out the previously specified order
          list := TCStringList.create( _smScenarioCopy.simInput.controlParams.vaccReasonOrder, ',' );

          str := list.first();
          while( nil <> str ) do
          	begin
              if( ( 0 < nRing ) and ( 'ring' = str ) ) then
                lbxReason.AddItem( tr( 'Ring' ), nil )
              ;
            	str := list.next();
            end
          ;
          list.free();
        end
      else
      	begin
        	// use default order
          if( 0 < nRing ) then
            lbxReason.AddItem( tr( 'Ring' ), nil )
          ;
        end
      ;

      if( 0 = lbxReason.Count ) then
        begin
          lbxReason.Hide();
          lblNoReasons.show();
        end
      ;
    end
  ;


  procedure TFormVaccPriority.fillProdTypes();
    var
      dm: TProductionType;
      it: TProductionTypeListIterator;
      map: TQIntegerObjectMap;
      i: integer;
    begin
      _ptList := _smScenarioCopy.simInput.ptList;

      map := TQIntegerObjectMap.create();
      it := TProductionTypeListIterator.create( _ptList );

      // Loop over all production types to determine which ones already have a valid priority value.
      // Store those that do have a valid value in the map.
      it.toFirst();
      while( nil <> it.current() ) do
        begin
          dm := it.current();

          // All production types should have ringVaccParams, whether units are vaccinated or not.
          if( nil = dm.ringVaccParams ) then
            raise exception.Create( 'dm.ringVaccParams is nil in TFormVaccPriority.fillProdTypes()' )
          ;

          if( dm.isVaccTarget ) then
            begin
              // If the priority value is a zero or a negative number, it is invalid: don't save it in the map.
              if( 0 >= dm.ringVaccParams.vaccPriority ) then
                begin
                  // Do nothing
                end
              // If the map already contains the priority value, then it is a duplicate that must be fixed: don't save it in the map.
              else if( map.contains( dm.ringVaccParams.vaccPriority ) ) then
                dm.ringVaccParams.vaccPriority := -1
              // If neither of these conditions are met, then add an item to the map.
              else
                map.insert( dm.ringVaccParams.vaccPriority, dm )
              ;
            end
          ;

          it.incr();
        end
      ;

      // Once we have the map, we can use it to fill in the production type selection box.
      // Production types that have valid priority values should be listed first, in priority order,
      // followed by any other production types that are targets of vaccination.

      // Recall that items stored in a QMap are sorted by key. It won't matter if priority
      // values aren't sequential, ptroduction types will still be listed in the right order.
      for i := 0 to map.count - 1 do
        begin
          dm := map.itemAtIndex( i ) as TProductionType;
          lbxProdType.AddItem( dm.productionTypeDescr, nil );
        end
      ;

      map.Free();

      // Now we can append the remaining production types that are vaccination targets but don't (yet) have
      // a valid priority value.
      it.toFirst();
      while( nil <> it.current() ) do
        begin
          dm := it.current();
          if( ( dm.isVaccTarget ) and ( 0 >= dm.ringVaccParams.vaccPriority ) ) then
            lbxProdType.AddItem( dm.productionTypeDescr, nil )
          ;
          it.incr();
        end
      ;

      it.Free();
    end
  ;


  procedure TFormVaccPriority.updateScenarioData();
    var
      pt: TProductionType;
      reasonList: TCStringList;
      reason: pchar;
      i,j: integer;
      vaccReasonOrder:String;
      vaccPriorityOrder:String;
    begin
      updateReasons();

      vaccReasonOrder := 'ring'; //_smScenarioCopy.simInput.controlParams.vaccReasonOrder;
      vaccPriorityOrder :=_smScenarioCopy.simInput.controlParams.vaccPriorityOrder;

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
                      _smScenarioCopy.simInput.controlParams.ssVaccPriorities[ pt.productionTypeDescr + '+' + reason ] := i;
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
                      _smScenarioCopy.simInput.controlParams.ssVaccPriorities[ pt.productionTypeDescr + '+' + reason ] := i;
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

      freeAndNil( reasonList );
      inherited updateScenarioData();
    end
  ;


	function TFormVaccPriority.showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer;
  	begin
    	// need detection of disease in order to conduct vaccination campaign
      if (( _smScenarioCopy.simInput.includeVaccinationGlobal ) and ( _smScenarioCopy.simInput.includeDetectionGlobal )) then
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
