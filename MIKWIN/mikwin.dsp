# Microsoft Developer Studio Project File - Name="mikwin" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** NICHT BEARBEITEN **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=mikwin - Win32 Debug
!MESSAGE Dies ist kein gültiges Makefile. Zum Erstellen dieses Projekts mit NMAKE
!MESSAGE verwenden Sie den Befehl "Makefile exportieren" und führen Sie den Befehl
!MESSAGE 
!MESSAGE NMAKE /f "mikwin.mak".
!MESSAGE 
!MESSAGE Sie können beim Ausführen von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "mikwin.mak" CFG="mikwin - Win32 Debug"
!MESSAGE 
!MESSAGE Für die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "mikwin - Win32 Release" (basierend auf  "Win32 (x86) Dynamic-Link Library")
!MESSAGE "mikwin - Win32 Debug" (basierend auf  "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "mikwin - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "MIKWIN_EXPORTS" /YX /FD /c
# ADD CPP /nologo /MT /GX /O2 /I ".\include" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "MIKWIN_EXPORTS" /FD /c
# SUBTRACT CPP /YX
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "NDEBUG"
# ADD RSC /l 0x407 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib dsound.lib /nologo /dll /machine:I386

!ELSEIF  "$(CFG)" == "mikwin - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "MIKWIN_EXPORTS" /YX /FD /GZ /c
# ADD CPP /nologo /MTd /W2 /Gm /GX /ZI /Od /I ".\include" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "MIKWIN_EXPORTS" /FD /GZ /c
# SUBTRACT CPP /YX
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "_DEBUG"
# ADD RSC /l 0x407 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib dsound.lib /nologo /dll /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "mikwin - Win32 Release"
# Name "mikwin - Win32 Debug"
# Begin Group "Source files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Group "loaders"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\loaders\load_669.c
# End Source File
# Begin Source File

SOURCE=.\loaders\load_amf.c
# End Source File
# Begin Source File

SOURCE=.\loaders\load_dsm.c
# End Source File
# Begin Source File

SOURCE=.\loaders\load_far.c
# End Source File
# Begin Source File

SOURCE=.\loaders\load_imf.c
# End Source File
# Begin Source File

SOURCE=.\loaders\load_it.c
# End Source File
# Begin Source File

SOURCE=.\loaders\load_m15.c
# End Source File
# Begin Source File

SOURCE=.\loaders\load_med.c
# End Source File
# Begin Source File

SOURCE=.\loaders\load_mod.c
# End Source File
# Begin Source File

SOURCE=.\loaders\load_mtm.c
# End Source File
# Begin Source File

SOURCE=.\loaders\load_s3m.c
# End Source File
# Begin Source File

SOURCE=.\loaders\load_stm.c
# End Source File
# Begin Source File

SOURCE=.\loaders\load_stx.c
# End Source File
# Begin Source File

SOURCE=.\loaders\load_ult.c
# End Source File
# Begin Source File

SOURCE=.\loaders\load_uni.c
# End Source File
# Begin Source File

SOURCE=.\loaders\load_xm.c
# End Source File
# Begin Source File

SOURCE=.\playercode\mloader.c
# End Source File
# Begin Source File

SOURCE=.\playercode\mlreg.c
# End Source File
# Begin Source File

SOURCE=.\playercode\mlutil.c
# End Source File
# Begin Source File

SOURCE=.\playercode\munitrk.c
# End Source File
# Begin Source File

SOURCE=.\playercode\mwav.c
# End Source File
# Begin Source File

SOURCE=.\playercode\npertab.c
# End Source File
# Begin Source File

SOURCE=.\playercode\sloader.c
# End Source File
# End Group
# Begin Group "drivers"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\drivers\drv_ds_raw.c
# End Source File
# Begin Source File

SOURCE=.\drivers\drv_nos.c
# End Source File
# Begin Source File

SOURCE=.\drivers\drv_wav.c
# End Source File
# Begin Source File

SOURCE=.\playercode\mdreg.c
# End Source File
# Begin Source File

SOURCE=.\playercode\mdriver.c
# End Source File
# End Group
# Begin Group "tools"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\mmio\mmalloc.c
# End Source File
# Begin Source File

SOURCE=.\mmio\mmerror.c
# End Source File
# Begin Source File

SOURCE=.\mmio\mmio.c
# End Source File
# End Group
# Begin Source File

SOURCE=.\mikwin.c
# End Source File
# Begin Source File

SOURCE=.\playercode\mplayer.c
# End Source File
# Begin Source File

SOURCE=.\playercode\virtch.c
# End Source File
# Begin Source File

SOURCE=.\playercode\virtch2.c
# End Source File
# Begin Source File

SOURCE=.\playercode\virtch_common.c
# End Source File
# End Group
# Begin Group "Header files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\include\mikmod.h
# End Source File
# Begin Source File

SOURCE=.\include\mikmod.pas
# End Source File
# Begin Source File

SOURCE=.\include\mikmod_internals.h
# End Source File
# Begin Source File

SOURCE=.\include\mikwin.h
# End Source File
# End Group
# Begin Group "Documentation"

# PROP Default_Filter "*.txt"
# Begin Source File

SOURCE=.\Authors
# End Source File
# Begin Source File

SOURCE=.\docs\mikmod.html
# End Source File
# Begin Source File

SOURCE=.\docs\mikmod_foot.html
# End Source File
# Begin Source File

SOURCE=.\docs\mikmod_toc.html
# End Source File
# Begin Source File

SOURCE=.\mikwin.html
# End Source File
# End Group
# Begin Source File

SOURCE=.\mikwin.def
# End Source File
# Begin Source File

SOURCE=.\mikwin.rc
# End Source File
# End Target
# End Project
