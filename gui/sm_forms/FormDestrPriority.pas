unit FormDestrPriority;

(*
FormDestrPriority.pas/dfm
--------------------------
Begin: 2005/06/08
Last revision: $Date: 2008/11/25 22:00:29 $ $Author: areeves $
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
    GuiStrUtils,
    DebugWindow,
    QStringMaps,
    QIntegerMaps,
    I88n
  ;

  
  constructor TFormDestrPriority.create( AOwner: TComponent );
  	begin
      inherited create( AOwner );
      translateUI();

      ReasonForActivity := tr( 'Reason for destruction' );
      //sortOrder := OrderDestrPriority;
      //prodTypePriorityColumn := 'destrPriority';
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


      if( length( 'basic,direct,indirect,ring' ) <> length( destrReasonOrder ) ) then
      	destrReasonOrder := 'basic,direct,indirect,ring'
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
          end;  // End For loop
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
    begin
      if( length( _smScenarioCopy.simInput.controlParams.destrPriorityOrder ) > 0 ) then
      	begin
      		// figure out the previously specified order
          list := TCStringList.create( _smScenarioCopy.simInput.controlParams.destrPriorityOrder, ',' );

          str := list.first();
          while( nil <> str ) do
          	begin
              if( 'production type' = str ) then lbxPrimary.AddItem( tr( 'Production type' ), nil )
              else if( 'reason' = str ) then lbxPrimary.AddItem( tr( 'Reason for destruction' ), nil )
              else if( 'time waiting' = str ) then lbxPrimary.AddItem( tr( 'Days holding' ), nil )
              else raise exception.Create( 'FormDestrPriority: unrecognized primary reason ''' +  str + ''' in fillReasons().');
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

      if( length( _smScenarioCopy.simInput.controlParams.destrReasonOrder ) > 0 ) then
      	begin
      		// figure out the previously specified order
          list := TCStringList.create( _smScenarioCopy.simInput.controlParams.destrReasonOrder, ',' );

          str := list.first();
          while( nil <> str ) do
          	begin
              if( 'basic' = str ) then lbxReason.AddItem( tr( 'Detected' ), nil )
              else if( 'direct' = str ) then lbxReason.AddItem( tr( 'Direct contact' ), nil )
              else if( 'indirect' = str ) then lbxReason.AddItem( tr( 'Indirect contact' ), nil )
              else if( 'ring' = str ) then lbxReason.AddItem( tr( 'Ring' ), nil )
              ;
            	str := list.next();
            end
          ;
          list.free();
        end
      else
      	begin
        	// use default order
        	lbxReason.AddItem( tr( 'Detected' ), nil );
          lbxReason.AddItem( tr( 'Direct contact' ), nil );
          lbxReason.AddItem( tr( 'Indirect contact' ), nil );
          lbxReason.AddItem( tr( 'Ring' ), nil );
        end
      ;
    end
  ;

  procedure TFormDestrPriority.fillProdTypes();
  	var
    	it: TProductionTypeListIterator;
      orderDict: TQIntegerStringMap;
      list: TCStringList;
      destrReason: string;
      index: integer;
      maxIdx: integer;
    begin
      dbcout2( endl + '================= TFormDestrPriority.fillProdTypes()' );

      // This list lasts just long enough to grab the first reason for destruction
      // (i.e. basic, ring, direct trace, indirect trace).
      dbcout2( 'Expected destruction reason order:' );
      dbcout2( _smScenarioCopy.simInput.controlParams.destrReasonOrder );
      list := TCStringList.create( _smScenarioCopy.simInput.controlParams.destrReasonOrder, ',' );
      destrReason := list.first();
      freeAndNil( list );

      orderDict := TQIntegerStringMap.create();

			_ptList := _smScenarioCopy.simInput.ptList;

      maxIdx := -1;

      it := TProductionTypeListIterator.create( _ptList );
      it.toFirst();

      while ( nil <> it.current() ) do
        begin
          index := (_smScenarioCopy.simInput.controlParams.ssDestrPriorities.Item[ it.current().productionTypeDescr + '+' + destrReason ] - 1);

          // Each production type can appear in the destruction priority list more than once,
          // because there is more than one reason for destruction that can be applied to a production type.
          // Within each reason, however, production types will always be given in the same order.
          // This mechanism should assure that all production types are listed in the correct relative order,
          // regardless of the destruction reason.
          dbcout2( 'Original index: ' + intToStr( index ) );
          index := index mod _ptList.Count;

          if( index > maxIdx ) then maxIdx := index;

          // If there's already a production type with that index, put it at the end of the list.
          // Likewise, make sure newly added production types are tacked on to the end of the list.
          // (New types won't be surprising.  Duplicate indices may not even be possible, but if they
          // do occur, it probably means that someone was screwing around with the database.)
          if
            ( orderDict.contains( index ) )
          or
            ( -1 = index )
          then
            begin
              dbcout2( '---- New or duplicate index found!!' );
              inc( maxIdx );
              index := maxIdx;
              _smScenarioCopy.simInput.controlParams.ssDestrPriorities[it.current().productionTypeDescr + '+' + destrReason] := index;
              it.current().destructionParams.updated := true;
            end
          ;

          // Insert the indexed production type name into the dictionary
          dbcout2( 'Index: ' + intToStr( index ) + ', type: ' + it.current().productionTypeDescr );
          orderDict.insert( index, it.current().productionTypeDescr );

          it.incr();
        end
      ;

      dbcout2( endl );

      // Now that all of the production types are in the list,
      // put their descriptions in the list box.
      // Take advantage of the fact that items in a QMap are always sorted by key.
      for index := 0 to orderDict.count - 1 do
      	begin
          lbxProdType.AddItem( orderDict.itemAtIndex( index ), nil );
        end
      ;

      it.Free();
      orderDict.Free();

      dbcout2( '================= Done.' + endl );
    end
  ;


  procedure TFormDestrPriority.updateReasons();
    var
      str: string;
      i: integer;
		begin
      // primary destruction priorities
      str := '';

      for i := 0 to lbxPrimary.Count-1 do
        begin
          if( tr( 'Reason for destruction' ) = lbxPrimary.Items[i] ) then str := str + 'reason'
          else if( tr( 'Production type' ) = lbxPrimary.Items[i] ) then str := str + 'production type'
          else if( tr( 'Days holding' ) = lbxPrimary.Items[i] ) then str := str + 'time waiting'
          else raise exception.create( 'FormDestrPriority: unrecognized primary reason ''' + lbxPrimary.Items[i] + ''' in updateReasons().' )
          ;

          if( lbxPrimary.Count-1 <> i ) then str := str + ',';
        end
      ;

      _smScenarioCopy.simInput.controlParams.destrPriorityOrder := str;

      // destruction reasons
      str := '';

      for i := 0 to lbxReason.Count-1 do
      	begin
       		if( tr( 'Detected' ) = lbxReason.Items[i] ) then str := str + 'basic'
          else if( tr( 'Direct contact' ) = lbxReason.Items[i] ) then str := str + 'direct'
          else if( tr( 'Indirect contact' ) = lbxReason.Items[i] ) then str := str + 'indirect'
          else if( tr( 'Ring' ) = lbxReason.Items[i] ) then str := str + 'ring'
          else raise exception.create( 'FormDestrPriority: unrecognized reason ''' + lbxReason.Items[i] + ''' in updateReasons().' )
        	;

         if( lbxReason.Count-1 <> i ) then str := str + ',';
        end
      ;
      _smScenarioCopy.simInput.controlParams.destrReasonOrder := str;
    end
  ;


	function TFormDestrPriority.showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer;
  	begin
    	if( _smScenarioCopy.simInput.includeDestructionGlobal ) then
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
