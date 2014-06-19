# RivertonInst.nsi
# ----------------
# Begin: 2008/10/21
# Last revision: $Date: 2011-11-24 00:05:07 $ $Author: areeves $
# Version: $Revision: 1.3.4.2 $
# Project: NSIS installer script for NAADSM, experimental version "Riverton"
# Website: http://www.naadsm.org
# Author: Snehal Shetye <snehal@goku.engr.colostate.edu>
# Author: Aaron Reeves <aaron.reeves@naadsm.org>
# --------------------------------------------------
# Copyright (C) 2008 - 2013 NAADSM Development Team
# 
# This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
# Public License as published by the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.


#Name of the Program being installed
Name "NAADSM-Riverton 3.3.2"

#NOTE: Can use the 'Icon' command to specify an icon file(.ico) for the .exe file.
Icon installerIcon.ico

#Installer Name
outFile "RivertonSetup.exe"

#Default Installation Directory
installDir "$PROGRAMFILES\NAADSM-Riverton 3.3.2"

#Sets the text to be shown in the license page
LicenseData ../license.txt

#creates a custom page to ask install confirmation from the user.
page custom startDialog "" ": Start Dialog"
 Function startDialog
 
  MessageBox MB_OKCANCEL "This application will install \
  NAADSM 'Riverton' version 3.3.2 \ 
  on your computer. Click OK to continue." \
  IDCANCEL NoCancelAbort 
  Abort
  NoCancelAbort: Quit  
 FunctionEnd 

#brings up the license page(pre-made in nSIS)
page license

#Asks the user to select an installation Directory(pre-made in nSIS)
page directory

#brings up the components page(pre-made in nSIS)
page components

#brings up the installation dialog(pre-made in NSIS)
page instfiles


page custom endDialog "" ": End Dialog"
 Function endDialog
  MessageBox MB_OK "Installation is complete! \
  Please check http://www.naadsm.org for updates. "
 FunctionEnd

#the "-" before MainSection text indicates that this section
#	cannot be selectively installed by the user, i.e., it does not 
#	show up in the components page 
section "-MainSection"

#sets the installation directory
setOutPath $INSTDIR

#copies each file in the installation directory
file "C:\WINDOWS\system32\msvcrt.dll"
file "C:\QtSDK\Desktop\Qt\4.7.4\mingw\bin\mingwm10.dll"
file "C:\QtSDK\Desktop\Qt\4.7.4\mingw\bin\libgcc_s_dw2-1.dll"
file "C:\QtSDK\Desktop\Qt\4.7.4\mingw\bin\QtCore4.dll"
file "C:\QtSDK\Desktop\Qt\4.7.4\mingw\bin\QtNetwork4.dll"
file "C:\libs\C_libs\libiconv-1.9.1\bin\iconv.dll"
file "C:\libs\C_libs\gettext-0.13.1\bin\intl.dll"
file "C:\libs\C_libs\libaphi\bin\libaphi.dll"
file "C:\libs\C_libs\gmp-5.0.1\bin\libgmp-10.dll"
file "C:\libs\C_libs\glib-2.22.2\bin\libglib-2.0-0.dll"
file "C:\libs\C_libs\gsl-1.8\bin\libgsl.dll"
file "C:\libs\C_libs\gsl-1.8\bin\libgslcblas.dll" 
file "C:\libs\C_libs\libiconv-1.8-1\bin\libiconv-2.dll"
file "C:\libs\C_libs\libintl-0.14.4\bin\libintl-2.dll"
file "C:\libs\C_libs\sprng-2.0a_naadsm\bin\sprng.dll"
file "C:\libs\C_libs\popt-1.8.1\bin\popt1.dll"
file "C:\libs\C_libs\proj-4.6.1\bin\proj.dll"
file "C:\libs\Delphi_libs\sdew\dll\sdew.dll"
file "C:\libs\Delphi_libs\qclasses\dll\qclasses.dll"
file "C:\libs\Delphi_libs\zipmaster\dll\UnzDll.dll"
file "C:\libs\Delphi_libs\zipmaster\dll\ZipDll.dll"
file "..\riverton.exe"
file "..\rivertonconsole.exe"
file "..\remote.dll"
file "..\riverton.dll"
file "..\license.txt"

#creates an uninstaller
writeUninstaller $INSTDIR\RivertonUninstall.exe

#the following 2 commands add uninstall functionality in Add/Remove Programs
WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NAADSM-Riverton-3.3.2" \
                 "DisplayName" "NAADSM-Riverton 3.3.2"
WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NAADSM-Riverton-3.3.2" \
                 "UninstallString" "$INSTDIR\RivertonUninstall.exe"
sectionEnd

#this section can also be selectively installed by the user
#	and it is checked by default
section "Install Start menu shortcut"
createShortCut "$SMPROGRAMS\NAADSM-Riverton 3.3.2.lnk" "$INSTDIR\riverton.exe"
sectionEnd

#this section can be selectively installed by the user
#	and the /o means that it is unchecked by default(optional)
section /o "Install desktop shortcut"
setOutPath $DESKTOP
#creates a shortcut to the executable file
createShortCut "$DESKTOP\NAADSM-Riverton 3.3.2.lnk" "$INSTDIR\riverton.exe"
sectionEnd

#brings up the uninstall confirmation page before uninstalling
uninstPage uninstConfirm "" "" ""

uninstPage instfiles

section "Uninstall"
  Delete "$INSTDIR\NAADSMUninstall.exe"
  Delete "$INSTDIR\sdew.dll"
  Delete "$INSTDIR\license.txt"
  Delete "$INSTDIR\iconv.dll"
  Delete "$INSTDIR\intl.dll"
  Delete "$INSTDIR\libaphi.dll"
  Delete "$INSTDIR\libgmp-10.dll"
  Delete "$INSTDIR\libglib-2.0-0.dll"
  Delete "$INSTDIR\libgcc_s_dw2-1.dll"
  Delete "$INSTDIR\libgsl.dll"
  Delete "$INSTDIR\libgslcblas.dll"
  Delete "$INSTDIR\libiconv-2.dll"
  Delete "$INSTDIR\libintl-2.dll"
  Delete "$INSTDIR\mingwm10.dll"
  Delete "$INSTDIR\msvcrt.dll"
  Delete "$INSTDIR\popt1.dll"
  Delete "$INSTDIR\proj.dll"
  Delete "$INSTDIR\qclasses.dll"
  Delete "$INSTDIR\QtCore4.dll"
  Delete "$INSTDIR\QtNetwork4.dll"
  Delete "$INSTDIR\sprng.dll"
  Delete "$INSTDIR\riverton.dll"
  Delete "$INSTDIR\UnzDll.dll"
  Delete "$INSTDIR\remote.dll"
  Delete "$INSTDIR\riverton.exe"
  Delete "$INSTDIR\rivertonconsole.exe"  
  Delete "$INSTDIR\ZipDll.dll"
  Delete "$INSTDIR\libgcc_s_dw2-1.dll"  
  Delete "$INSTDIR\spreadmodel.ini"

  ifFileExists "$SMPROGRAMS\NAADSM-Riverton 3.3.2.lnk" DeleteSMlink DoNothingSM
  DeleteSMlink:
  Delete "$SMPROGRAMS\NAADSM-Riverton 3.3.2.lnk"
  
  DoNothingSM:

  ifFileExists "$DESKTOP\NAADSM-Riverton 3.3.2.lnk" DeleteDesktopLink DoNothingDESK
  DeleteDesktopLink:
  Delete "$DESKTOP\NAADSM-Riverton 3.3.2.lnk"

  DoNothingDESK:

  RMDir $INSTDIR

  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NAADSM-Riverton-3.3.2"

sectionEnd
