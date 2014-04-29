# NAADSMInst.nsi
# --------------
# Begin: 2007/01/10
# Last revision: $Date: 2011-11-24 00:05:07 $ $Author: areeves $
# Version: $Revision: 1.25.4.20 $
# Project: NSIS installer script for NAADSM
# Website: http://www.naadsm.org
# Author: Snehal Shetye <snehal@goku.engr.colostate.edu>
# Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
# --------------------------------------------------
# Copyright (C) 2007 - 2011 Animal Population Health Institute, Colorado State University
# 
# This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
# Public License as published by the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.


#Name of the Program being installed
Name "NAADSM 3.2.18"

#NOTE: Can use the 'Icon' command to specify an icon file(.ico) for the .exe file.
Icon installerIcon.ico

#Installer Name
outFile "NAADSMSetup.exe"

#Default Installation Directory
installDir "$PROGRAMFILES\NAADSM 3.2.18"

#Sets the text to be shown in the license page
LicenseData ../license.txt

#creates a custom page to ask install confirmation from the user.
page custom startDialog "" ": Start Dialog"
 Function startDialog
 
  MessageBox MB_OKCANCEL "This application will install \
  NAADSM version 3.2.18 on your computer. \ 
  Click OK to continue." \
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
file "..\smconsole.exe"
file "..\SpreadModel.exe"
file "..\remote.dll"
file "..\naadsm.dll"
file "..\license.txt"

#creates an uninstaller
writeUninstaller $INSTDIR\NAADSMUninstall.exe

#the following 2 commands add uninstall functionality in Add/Remove Programs
WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NAADSM3.2.18" \
                 "DisplayName" "NAADSM 3.2.18"
WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NAADSM3.2.18" \
                 "UninstallString" "$INSTDIR\NAADSMUninstall.exe"
sectionEnd

#this section can also be selectively installed by the user
#	and it is checked by default
section "Install Start menu shortcut"
createShortCut "$SMPROGRAMS\NAADSM 3.2.18.lnk" "$INSTDIR\SpreadModel.exe"
sectionEnd

#this section can be selectively installed by the user
#	and the /o means that it is unchecked by default(optional)
section /o "Install desktop shortcut"
setOutPath $DESKTOP
#creates a shortcut to the executable file
createShortCut "$DESKTOP\NAADSM 3.2.18.lnk" "$INSTDIR\SpreadModel.exe"
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
  Delete "$INSTDIR\libgcc_s_dw2-1.dll"
  Delete "$INSTDIR\sprng.dll"
  Delete "$INSTDIR\naadsm.dll"
  Delete "$INSTDIR\UnzDll.dll"
  Delete "$INSTDIR\remote.dll"
  Delete "$INSTDIR\smconsole.exe"
  Delete "$INSTDIR\SpreadModel.exe"
  Delete "$INSTDIR\ZipDll.dll"
  Delete "$INSTDIR\spreadmodel.ini"

  ifFileExists "$SMPROGRAMS\NAADSM 3.2.18.lnk" DeleteSMlink DoNothingSM
  DeleteSMlink:
  Delete "$SMPROGRAMS\NAADSM 3.2.18.lnk"
  
  DoNothingSM:

  ifFileExists "$DESKTOP\NAADSM 3.2.18.lnk" DeleteDesktopLink DoNothingDESK
  DeleteDesktopLink:
  Delete "$DESKTOP\NAADSM 3.2.18.lnk"

  DoNothingDESK:

  RMDir $INSTDIR

  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\NAADSM3.2.18"

sectionEnd
