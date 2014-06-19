unit FormDestrPriority;

(*
FormDestrPriority.pas/dfm
--------------------------
Begin: 2005/06/08
Last revision: $Date: 2013-06-27 19:11:25 $ $Author: areeves $
Version: $Revision: 1.27.4.2 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <aaron.reeves@naadsm.org>
--------------------------------------------------
Copyright (C) 2005 - 2009 Colorado State University

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

	type TFormDestrPriority = class( TFormPriorityBase )
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
    SqlClasses,
    CStringList,
    MyStrUtils,
    DebugWindow,
    QStringMaps,
    QIntegerMaps,
    QLists,
    I88n
  ;

  
  constructor TFormDestrPriority.create( AOwner: TComponent );
  	begin
      inherited create( AOwner );
      translateUI();

      ReasonForActivity := tr( 'Reason for destruction' );
    end
  ;
  
  
  procedure TFormDestrPriority.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:55 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormDestrPriority.dfm
      // File date: Wed Oct 25 14:50:17 2006

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Scenario parameters: Destruction priorities' );
          pnlCaption.Caption := tr( 'Destruction priorities' );
          lblNoProdTypes.Caption := tr( '(Destruction is not used with any production type)' );
          lblNoReasons.Caption := tr( '(Destruction is never used)' );
        end
      ;

    end
  ;  
  

  procedure TFormDestrPriority.updateScenarioData();
    var
      pt: TProductionType;
      reasonList: TCStringList;
      reason: pchar;
      i,j: integer;
      _destrOrderList:TQStringLongIntMap;
      destrReasonOrder:String;
      destrPriorityOrder:String;
    begin
      updateReasons();

      destrReasonOrder := _smScenarioCopy.simInput.controlParams.destrReasonOrder;
      destrPriorityOrder :=_smScenarioCopy.simInput.controlParams.destrPriorityOrder;


      if( length( 'basic,direct-forward,indirect-forward,ring,direct-back,indirect-back' ) <> length( destrReasonOrder ) ) then
      	destrReasonOrder := 'basic,direct-forward,indirect-forward,ring,direct-back,indirect-back'
      ;

      _destrOrderList := TQStringLongIntMap.Create();

      reasonList := TCStringList.create( destrReasonOrder, ',' );

      i := 1;

      if( pos( 'production type', destrPriorityOrder ) < pos( 'reason', destrPriorityOrder ) ) then
      	begin
        	// Production type is higher priority than destruction reason
          // So production type is the outer loop

          for j := 0 to lbxProdType.Items.Count-1 do
            begin
              pt := _ptList.findProdType( lbxProdType.Items.Strings[j] );
              if ( nil <> pt ) then
                begin
                  reason := reasonList.first();
                  while( nil <> reason ) do
                    begin
                      _destrOrderList[ pt.productionTypeDescr + '+' + reason ] := i;
                      inc( i );
                      reason := reasonList.next();
                    end
                  ;
                  pt.destructionParams.destrPriority := j + 1;
                end
              ;
            end
          ;  // End For loop
        end
      else
      	begin
        	// Destruction reason is higher priority than production type
          // So destruction reason is the outer loop
          reason := reasonList.first();
          while( nil <> reason ) do
          	begin
              for j := 0 to lbxProdType.Items.Count-1 do
                begin
                  pt := _ptList.findProdType( lbxProdType.Items.Strings[j] );
                  if ( nil <> pt ) then
                    begin
                    	_destrOrderList[ pt.productionTypeDescr + '+' + reason ] := i;
                      pt.destructionParams.destrPriority := j + 1;
                      inc( i );
                    end;
                end
              ;
            	reason := reasonList.next();
            end
          ;
        end
      ;
      _smScenarioCopy.simInput.controlParams.ssDestrPriorities := _destrOrderList;
      freeAndNil( reasonList );

      inherited updateScenarioData();
    end
  ;


  procedure TFormDestrPriority.fillReasons();
    var
      list: TCStringList;
      str: pchar;
      it: TProductionTypeListIterator;
      nDetected, nDirectFwd, nIndirectFwd, nRing, nDirectBack, nIndirectBack: integer;
    begin
      if( length( _smScenarioCopy.simInput.controlParams.destrPriorityOrder ) > 0 ) then
      	begin
      		// figure out the previously specified order
          list := TCStringList.create( _smScenarioCopy.simInput.controlParams.destrPriorityOrder, ',' );

          str := list.first();
          while( nil <> str ) do
          	begin
              if( 'production type' = str ) then
                lbxPrimary.AddItem( tr( 'Production type' ), nil )
              else if( 'reason' = str ) then
                lbxPrimary.AddItem( tr( 'Reason for destruction' ), nil )
              else if( 'time waiting' = str ) then
                lbxPrimary.AddItem( tr( 'Days holding' ), nil )
              else
                raise exception.Create( 'FormDestrPriority: unrecognized primary reason ''' +  str + ''' in fillReasons().')
              ;
            	str := list.next();
            end
          ;
          list.free();
        end
      else
        begin
        	// use default order
          lbxPrimary.AddItem( tr( 'Reason for destruction' ), nil );
          lbxPrimary.AddItem( tr( 'Production type' ), nil );
          lbxPrimary.AddItem( tr( 'Days holding' ), nil );
        end
      ;


      // Before filling in the reasons list box, determine which reasons are actually used.
      // Show only these reasons in use.
      nDetected := 0;
      nDirectFwd := 0;
      nIndirectFwd := 0;
      nRing := 0;
      nDirectBack := 0;
      nIndirectBack := 0;

      it := TProductionTypeListIterator.create( _ptList );

      it.toFirst();
      while( nil <> it.current() ) do
        begin
          if( nil = it.current().destructionParams ) then
            raise exception.Create( 'pt.destructionParams is nil in TFormDestrPriority.fillReasons()' )
          ;

          if( it.current().destructionParams.destroyDetectedUnits ) then
            inc( nDetected )
          ;
          if( it.current().destructionParams.destroyDirectForwardTraces ) then
            inc( nDirectFwd )
          ;
          if( it.current().destructionParams.destroyIndirectForwardTraces ) then
            inc( nIndirectFwd )
          ;
          if( it.current().destructionParams.isRingTarget ) then
            inc( nRing )
          ;
          if( it.current().destructionParams.destroyDirectBackTraces ) then
            inc( nDirectBack )
          ;
          if( it.current().destructionParams.destroyIndirectBackTraces ) then
            inc( nIndirectBack )
          ;
          it.incr();
        end
      ;

      it.Free();

      // Now fill in the reasons list box.
      if( length( _smScenarioCopy.simInput.controlParams.destrReasonOrder ) > 0 ) then
      	begin
      		// figure out the previously specified order
          list := TCStringList.create( _smScenarioCopy.simInput.controlParams.destrReasonOrder, ',' );

          str := list.first();
          while( nil <> str ) do
          	begin
              if( ( 0 < nDetected ) and ( 'basic' = str ) ) then
                lbxReason.AddItem( tr( 'Detected' ), nil )
              ;
              if( ( 0 < nDirectFwd ) and ( 'direct-forward' = str ) ) then
                lbxReason.AddItem( tr( 'Trace forward of direct contact' ), nil )
              ;
              if( ( 0 < nIndirectFwd ) and ( 'indirect-forward' = str ) ) then
                lbxReason.AddItem( tr( 'Trace forward of indirect contact' ), nil )
              ;
              if( ( 0 < nRing ) and ( 'ring' = str ) ) then
                lbxReason.AddItem( tr( 'Ring' ), nil )
              ;
              if( ( 0 < nDirectBack ) and ( 'direct-back' = str ) ) then
                lbxReason.AddItem( tr( 'Trace back of direct contact' ), nil )
              ;
              if( ( 0 < nIndirectBack ) and ( 'indirect-back' = str ) ) then
                lbxReason.AddItem( tr( 'Trace back of indirect contact' ), nil )
              ;
            	str := list.next();
            end
          ;
          list.free();
        end
      else
      	begin
        	// use default order
        	if( 0 < nDetected ) then
            lbxReason.AddItem( tr( 'Detected' ), nil )
          ;
          if( 0 < nDirectFwd ) then
            lbxReason.AddItem( tr( 'Trace forward of direct contact' ), nil )
          ;
          if( 0 < nIndirectFwd ) then
            lbxReason.AddItem( tr( 'Trace forward of indirect contact' ), nil )
          ;
          if( 0 < nRing ) then
            lbxReason.AddItem( tr( 'Ring' ), nil )
          ;
          if( 0 < nDirectBack ) then
            lbxReason.AddItem( tr( 'Trace back of direct contact' ), nil )
          ;
          if( 0 < nIndirectBack ) then
            lbxReason.AddItem( tr( 'Trace back of indirect contact' ), nil )
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


  procedure TFormDestrPriority.fillProdTypes();
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

          // All production types should have destructionParams, whether units are destroyed or not.
          if( nil = dm.destructionParams ) then
            raise exception.Create( 'dm.destructionParams is nil in TFormDestrPriority.fillProdTypes()' )
          ;

          if( dm.isDestrTarget ) then
            begin
              // If the priority value is a zero or a negative number, it is invalid: don't save it in the map.
              if( 0 >= dm.destructionParams.destrPriority ) then
                begin
                  // Do nothing
                end
              // If the map already contains the priority value, then it is a duplicate that must be fixed: don't save it in the map.
              else if( map.contains( dm.destructionParams.destrPriority ) ) then
                dm.destructionParams.destrPriority := -1
              // If neither of these conditions are met, then add an item to the map.
              else
                map.insert( dm.destructionParams.destrPriority, dm )
              ;
            end
          ;

          it.incr();
        end
      ;                                           

      // Once we have the map, we can use it to fill in the production type selection box.
      // Production types that have valid priority values should be listed first, in priority order,
      // followed by any other production types that are targets of destruction.

      // Recall that items stored in a QMap are sorted by key. It won't matter if priority
      // values aren't sequential, production types will still be listed in the right order.
      for i := 0 to map.count - 1 do
        begin
          dm := map.itemAtIndex( i ) as TProductionType;
          lbxProdType.AddItem( dm.productionTypeDescr, nil );
        end
      ;

      map.Free();

      // Now we can append the remaining production types that are destruction targets but don't (yet) have
      // a valid priority value.
      it.toFirst();
      while( nil <> it.current() ) do
        begin
          dm := it.current();
          if( ( dm.isDestrTarget ) and ( 0 >= dm.destructionParams.destrPriority ) ) then
            lbxProdType.AddItem( dm.productionTypeDescr, nil )
          ;
          it.incr();
        end
      ;

      it.Free();
    end
  ;


  procedure TFormDestrPriority.updateReasons();
    var
      str: string;
      i: integer;
      basicFound, directFwdFound, indirectFwdFound, ringFound, directBackFound, indirectBackFound: boolean;
      strList: TQStringList;
		begin
      // Primary destruction priorities
      //-------------------------------
      str := '';

      for i := 0 to lbxPrimary.Count-1 do
        begin
          if( tr( 'Reason for destruction' ) = lbxPrimary.Items[i] ) then
            str := str + 'reason'
          else if( tr( 'Production type' ) = lbxPrimary.Items[i] ) then
            str := str + 'production type'
          else if( tr( 'Days holding' ) = lbxPrimary.Items[i] ) then
            str := str + 'time waiting'
          else
            raise exception.create( 'FormDestrPriority: unrecognized primary reason ''' + lbxPrimary.Items[i] + ''' in updateReasons().' )
          ;

          if( lbxPrimary.Count-1 <> i ) then str := str + ',';
        end
      ;

      _smScenarioCopy.simInput.controlParams.destrPriorityOrder := str;

      // Destruction reasons
      //--------------------
      // Note that one or more destruction reasons may be missing from the list box
      // (if a reason is never used, it won't be listed).
      // Eventually, though, all six reasons must be given in the string that will
      // be stored in the database.
      basicFound := false;
      directFwdFound := false;
      indirectFwdFound := false;
      ringFound := false;
      directBackFound := false;
      indirectBackFound := false;

      strList := TQStringList.create();

      for i := 0 to lbxReason.Count-1 do
      	begin
       		if( tr( 'Detected' ) = lbxReason.Items[i] ) then
            begin
              basicFound := true;
              strList.append( 'basic' );
            end
          else if( tr( 'Trace forward of direct contact' ) = lbxReason.Items[i] ) then
            begin
              directFwdFound := true;
              strList.append( 'direct-forward' );
            end
          else if( tr( 'Trace forward of indirect contact' ) = lbxReason.Items[i] ) then
            begin
              indirectFwdFound := true;
              strList.append( 'indirect-forward' );
            end
          else if( tr( 'Ring' ) = lbxReason.Items[i] ) then
            begin
              ringFound := true;
              strList.append( 'ring' );
            end
          else if( tr( 'Trace back of direct contact' ) = lbxReason.Items[i] ) then
            begin
              directBackFound := true;
              strList.append( 'direct-back' );
            end
          else if( tr( 'Trace back of indirect contact' ) = lbxReason.Items[i] ) then
            begin
              indirectBackFound := true;
              strList.append( 'indirect-back' );
            end
          else
            raise exception.create( 'FormDestrPriority: unrecognized reason ''' + lbxReason.Items[i] + ''' in updateReasons().' )
        	;
        end
      ;

      if( not basicFound ) then
        strList.append( 'basic' )
      ;
      if( not directFwdFound ) then
        strList.append( 'direct-forward' )
      ;
      if( not indirectFwdFound ) then
        strList.append( 'indirect-forward' )
      ;
      if( not ringFound ) then
        strList.append( 'ring' )
      ;
      if( not directBackFound ) then
        strList.append( 'direct-back' )
      ;
      if( not indirectBackFound ) then
        strList.append( 'indirect-back' )
      ;

      if( 6 <> strList.count ) then
        raise exception.Create( 'Missing reason for destruction in TFormDestrPriority.updateReasons()')
      ;

      _smScenarioCopy.simInput.controlParams.destrReasonOrder := strList.join( ',' );

      strList.Free();
    end
  ;


	function TFormDestrPriority.showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer;
  	begin
      // need detection of disease in order to conduct destruction campaign
    	if (( _smScenarioCopy.simInput.includeDestructionGlobal ) and ( _smScenarioCopy.simInput.includeDetectionGlobal )) then
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


initialization
	RegisterClass( TFormDestrPriority );

end.
