unit SMI88nSettings;

(*
SMI88nSettings.pas
------------------
Begin: 2009/04/14
Begin: 2009/4/14
Last revision: $Date: 2013-06-27 19:11:18 $ $Author: areeves $
Version number: $Revision: 1.2.8.1 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2009 Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
    I88n
  ;

  type TSMI88nSettings = class( TI88nSettings )
    public
      constructor create(); override;
      destructor destroy(); override;

      procedure updateRegistry();
    end
  ;
  
implementation

  uses
    Registry,
    Windows,
    
    WindowsUtils
  ;

  constructor TSMI88nSettings.create();
    var
      regValue: integer;
      regStr: string;
    begin
      inherited create();
      
      try
        regValue := getRegistryData( HKEY_CURRENT_USER, 'Software\NAADSM','UseCustomSeparators' );
      except
        regValue := 0; // Default to false
      end;
      _useCustomListSeparator := ( 0 <> regValue );
      _useCustomDecimalSymbol := ( 0 <> regValue );
      
      try
        regStr := getRegistryData( HKEY_CURRENT_USER, 'Software\NAADSM','CustomListSeparator' );
        if( 0 < length( regStr ) ) then
          _customListSeparator := regStr[1]
        ;
      except
        // Do nothing
      end;
      
      try
        regStr := getRegistryData( HKEY_CURRENT_USER, 'Software\NAADSM','CustomDecimalSymbol' );
        if( 0 < length( regStr ) ) then
          _customDecimalSymbol := regStr[1]
        ;
      except
        // Do nothing
      end;      
    end
  ;
  
 
  destructor TSMI88nSettings.destroy();
    begin
      inherited destroy(); 
    end
  ;


  procedure TSMI88nSettings.updateRegistry();
    begin
      if( _useCustomListSeparator ) then
        begin
          setRegistryData( HKEY_CURRENT_USER, 'Software\NAADSM','UseCustomSeparators', rdInteger, 1 );
          setRegistryData( HKEY_CURRENT_USER, 'Software\NAADSM','CustomListSeparator', rdString, _customListSeparator );
          setRegistryData( HKEY_CURRENT_USER, 'Software\NAADSM','CustomDecimalSymbol', rdString, _customDecimalSymbol );
        end
      else
        setRegistryData( HKEY_CURRENT_USER, 'Software\NAADSM','UseCustomSeparators', rdInteger, 0 )
      ;
    end
  ;

end.