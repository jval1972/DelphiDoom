# Microsoft Developer Studio Generated NMAKE File, Based on mikwin.dsp
!IF "$(CFG)" == ""
CFG=mikwin - Win32 Debug
!MESSAGE Keine Konfiguration angegeben. mikwin - Win32 Debug wird als Standard verwendet.
!ENDIF 

!IF "$(CFG)" != "mikwin - Win32 Release" && "$(CFG)" != "mikwin - Win32 Debug"
!MESSAGE UngÅltige Konfiguration "$(CFG)" angegeben.
!MESSAGE Sie kînnen beim AusfÅhren von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "mikwin.mak" CFG="mikwin - Win32 Debug"
!MESSAGE 
!MESSAGE FÅr die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "mikwin - Win32 Release" (basierend auf  "Win32 (x86) Dynamic-Link Library")
!MESSAGE "mikwin - Win32 Debug" (basierend auf  "Win32 (x86) Dynamic-Link Library")
!MESSAGE 
!ERROR Eine ungÅltige Konfiguration wurde angegeben.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "mikwin - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\mikwin.dll"


CLEAN :
	-@erase "$(INTDIR)\drv_ds_raw.obj"
	-@erase "$(INTDIR)\drv_nos.obj"
	-@erase "$(INTDIR)\drv_wav.obj"
	-@erase "$(INTDIR)\load_669.obj"
	-@erase "$(INTDIR)\load_amf.obj"
	-@erase "$(INTDIR)\load_dsm.obj"
	-@erase "$(INTDIR)\load_far.obj"
	-@erase "$(INTDIR)\load_imf.obj"
	-@erase "$(INTDIR)\load_it.obj"
	-@erase "$(INTDIR)\load_m15.obj"
	-@erase "$(INTDIR)\load_med.obj"
	-@erase "$(INTDIR)\load_mod.obj"
	-@erase "$(INTDIR)\load_mtm.obj"
	-@erase "$(INTDIR)\load_s3m.obj"
	-@erase "$(INTDIR)\load_stm.obj"
	-@erase "$(INTDIR)\load_stx.obj"
	-@erase "$(INTDIR)\load_ult.obj"
	-@erase "$(INTDIR)\load_uni.obj"
	-@erase "$(INTDIR)\load_xm.obj"
	-@erase "$(INTDIR)\mdreg.obj"
	-@erase "$(INTDIR)\mdriver.obj"
	-@erase "$(INTDIR)\mikwin.obj"
	-@erase "$(INTDIR)\mikwin.res"
	-@erase "$(INTDIR)\mloader.obj"
	-@erase "$(INTDIR)\mlreg.obj"
	-@erase "$(INTDIR)\mlutil.obj"
	-@erase "$(INTDIR)\mmalloc.obj"
	-@erase "$(INTDIR)\mmerror.obj"
	-@erase "$(INTDIR)\mmio.obj"
	-@erase "$(INTDIR)\mplayer.obj"
	-@erase "$(INTDIR)\munitrk.obj"
	-@erase "$(INTDIR)\mwav.obj"
	-@erase "$(INTDIR)\npertab.obj"
	-@erase "$(INTDIR)\sloader.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\virtch.obj"
	-@erase "$(INTDIR)\virtch2.obj"
	-@erase "$(INTDIR)\virtch_common.obj"
	-@erase "$(OUTDIR)\mikwin.dll"
	-@erase "$(OUTDIR)\mikwin.exp"
	-@erase "$(OUTDIR)\mikwin.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MT /GX /O2 /I ".\include" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "MIKWIN_EXPORTS" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

MTL=midl.exe
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /win32 
RSC=rc.exe
RSC_PROJ=/l 0x407 /fo"$(INTDIR)\mikwin.res" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\mikwin.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib dsound.lib /nologo /dll /incremental:no /pdb:"$(OUTDIR)\mikwin.pdb" /machine:I386 /def:".\mikwin.def" /out:"$(OUTDIR)\mikwin.dll" /implib:"$(OUTDIR)\mikwin.lib" 
DEF_FILE= \
	".\mikwin.def"
LINK32_OBJS= \
	"$(INTDIR)\load_669.obj" \
	"$(INTDIR)\load_amf.obj" \
	"$(INTDIR)\load_dsm.obj" \
	"$(INTDIR)\load_far.obj" \
	"$(INTDIR)\load_imf.obj" \
	"$(INTDIR)\load_it.obj" \
	"$(INTDIR)\load_m15.obj" \
	"$(INTDIR)\load_med.obj" \
	"$(INTDIR)\load_mod.obj" \
	"$(INTDIR)\load_mtm.obj" \
	"$(INTDIR)\load_s3m.obj" \
	"$(INTDIR)\load_stm.obj" \
	"$(INTDIR)\load_stx.obj" \
	"$(INTDIR)\load_ult.obj" \
	"$(INTDIR)\load_uni.obj" \
	"$(INTDIR)\load_xm.obj" \
	"$(INTDIR)\mloader.obj" \
	"$(INTDIR)\mlreg.obj" \
	"$(INTDIR)\mlutil.obj" \
	"$(INTDIR)\munitrk.obj" \
	"$(INTDIR)\mwav.obj" \
	"$(INTDIR)\npertab.obj" \
	"$(INTDIR)\sloader.obj" \
	"$(INTDIR)\drv_ds_raw.obj" \
	"$(INTDIR)\drv_nos.obj" \
	"$(INTDIR)\drv_wav.obj" \
	"$(INTDIR)\mdreg.obj" \
	"$(INTDIR)\mdriver.obj" \
	"$(INTDIR)\mmalloc.obj" \
	"$(INTDIR)\mmerror.obj" \
	"$(INTDIR)\mmio.obj" \
	"$(INTDIR)\mikwin.obj" \
	"$(INTDIR)\mplayer.obj" \
	"$(INTDIR)\virtch.obj" \
	"$(INTDIR)\virtch2.obj" \
	"$(INTDIR)\virtch_common.obj" \
	"$(INTDIR)\mikwin.res"

"$(OUTDIR)\mikwin.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "mikwin - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\mikwin.dll"


CLEAN :
	-@erase "$(INTDIR)\drv_ds_raw.obj"
	-@erase "$(INTDIR)\drv_nos.obj"
	-@erase "$(INTDIR)\drv_wav.obj"
	-@erase "$(INTDIR)\load_669.obj"
	-@erase "$(INTDIR)\load_amf.obj"
	-@erase "$(INTDIR)\load_dsm.obj"
	-@erase "$(INTDIR)\load_far.obj"
	-@erase "$(INTDIR)\load_imf.obj"
	-@erase "$(INTDIR)\load_it.obj"
	-@erase "$(INTDIR)\load_m15.obj"
	-@erase "$(INTDIR)\load_med.obj"
	-@erase "$(INTDIR)\load_mod.obj"
	-@erase "$(INTDIR)\load_mtm.obj"
	-@erase "$(INTDIR)\load_s3m.obj"
	-@erase "$(INTDIR)\load_stm.obj"
	-@erase "$(INTDIR)\load_stx.obj"
	-@erase "$(INTDIR)\load_ult.obj"
	-@erase "$(INTDIR)\load_uni.obj"
	-@erase "$(INTDIR)\load_xm.obj"
	-@erase "$(INTDIR)\mdreg.obj"
	-@erase "$(INTDIR)\mdriver.obj"
	-@erase "$(INTDIR)\mikwin.obj"
	-@erase "$(INTDIR)\mikwin.res"
	-@erase "$(INTDIR)\mloader.obj"
	-@erase "$(INTDIR)\mlreg.obj"
	-@erase "$(INTDIR)\mlutil.obj"
	-@erase "$(INTDIR)\mmalloc.obj"
	-@erase "$(INTDIR)\mmerror.obj"
	-@erase "$(INTDIR)\mmio.obj"
	-@erase "$(INTDIR)\mplayer.obj"
	-@erase "$(INTDIR)\munitrk.obj"
	-@erase "$(INTDIR)\mwav.obj"
	-@erase "$(INTDIR)\npertab.obj"
	-@erase "$(INTDIR)\sloader.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\virtch.obj"
	-@erase "$(INTDIR)\virtch2.obj"
	-@erase "$(INTDIR)\virtch_common.obj"
	-@erase "$(OUTDIR)\mikwin.dll"
	-@erase "$(OUTDIR)\mikwin.exp"
	-@erase "$(OUTDIR)\mikwin.ilk"
	-@erase "$(OUTDIR)\mikwin.lib"
	-@erase "$(OUTDIR)\mikwin.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W2 /Gm /GX /ZI /Od /I ".\include" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "MIKWIN_EXPORTS" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

MTL=midl.exe
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /win32 
RSC=rc.exe
RSC_PROJ=/l 0x407 /fo"$(INTDIR)\mikwin.res" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\mikwin.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib dsound.lib /nologo /dll /incremental:yes /pdb:"$(OUTDIR)\mikwin.pdb" /debug /machine:I386 /def:".\mikwin.def" /out:"$(OUTDIR)\mikwin.dll" /implib:"$(OUTDIR)\mikwin.lib" /pdbtype:sept 
DEF_FILE= \
	".\mikwin.def"
LINK32_OBJS= \
	"$(INTDIR)\load_669.obj" \
	"$(INTDIR)\load_amf.obj" \
	"$(INTDIR)\load_dsm.obj" \
	"$(INTDIR)\load_far.obj" \
	"$(INTDIR)\load_imf.obj" \
	"$(INTDIR)\load_it.obj" \
	"$(INTDIR)\load_m15.obj" \
	"$(INTDIR)\load_med.obj" \
	"$(INTDIR)\load_mod.obj" \
	"$(INTDIR)\load_mtm.obj" \
	"$(INTDIR)\load_s3m.obj" \
	"$(INTDIR)\load_stm.obj" \
	"$(INTDIR)\load_stx.obj" \
	"$(INTDIR)\load_ult.obj" \
	"$(INTDIR)\load_uni.obj" \
	"$(INTDIR)\load_xm.obj" \
	"$(INTDIR)\mloader.obj" \
	"$(INTDIR)\mlreg.obj" \
	"$(INTDIR)\mlutil.obj" \
	"$(INTDIR)\munitrk.obj" \
	"$(INTDIR)\mwav.obj" \
	"$(INTDIR)\npertab.obj" \
	"$(INTDIR)\sloader.obj" \
	"$(INTDIR)\drv_ds_raw.obj" \
	"$(INTDIR)\drv_nos.obj" \
	"$(INTDIR)\drv_wav.obj" \
	"$(INTDIR)\mdreg.obj" \
	"$(INTDIR)\mdriver.obj" \
	"$(INTDIR)\mmalloc.obj" \
	"$(INTDIR)\mmerror.obj" \
	"$(INTDIR)\mmio.obj" \
	"$(INTDIR)\mikwin.obj" \
	"$(INTDIR)\mplayer.obj" \
	"$(INTDIR)\virtch.obj" \
	"$(INTDIR)\virtch2.obj" \
	"$(INTDIR)\virtch_common.obj" \
	"$(INTDIR)\mikwin.res"

"$(OUTDIR)\mikwin.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("mikwin.dep")
!INCLUDE "mikwin.dep"
!ELSE 
!MESSAGE Warning: cannot find "mikwin.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "mikwin - Win32 Release" || "$(CFG)" == "mikwin - Win32 Debug"
SOURCE=.\loaders\load_669.c

"$(INTDIR)\load_669.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\loaders\load_amf.c

"$(INTDIR)\load_amf.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\loaders\load_dsm.c

"$(INTDIR)\load_dsm.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\loaders\load_far.c

"$(INTDIR)\load_far.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\loaders\load_imf.c

"$(INTDIR)\load_imf.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\loaders\load_it.c

"$(INTDIR)\load_it.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\loaders\load_m15.c

"$(INTDIR)\load_m15.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\loaders\load_med.c

"$(INTDIR)\load_med.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\loaders\load_mod.c

"$(INTDIR)\load_mod.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\loaders\load_mtm.c

"$(INTDIR)\load_mtm.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\loaders\load_s3m.c

"$(INTDIR)\load_s3m.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\loaders\load_stm.c

"$(INTDIR)\load_stm.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\loaders\load_stx.c

"$(INTDIR)\load_stx.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\loaders\load_ult.c

"$(INTDIR)\load_ult.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\loaders\load_uni.c

"$(INTDIR)\load_uni.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\loaders\load_xm.c

"$(INTDIR)\load_xm.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\playercode\mloader.c

"$(INTDIR)\mloader.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\playercode\mlreg.c

"$(INTDIR)\mlreg.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\playercode\mlutil.c

"$(INTDIR)\mlutil.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\playercode\munitrk.c

"$(INTDIR)\munitrk.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\playercode\mwav.c

"$(INTDIR)\mwav.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\playercode\npertab.c

"$(INTDIR)\npertab.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\playercode\sloader.c

"$(INTDIR)\sloader.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\drivers\drv_ds_raw.c

"$(INTDIR)\drv_ds_raw.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\drivers\drv_nos.c

"$(INTDIR)\drv_nos.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\drivers\drv_wav.c

"$(INTDIR)\drv_wav.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\playercode\mdreg.c

"$(INTDIR)\mdreg.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\playercode\mdriver.c

"$(INTDIR)\mdriver.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\mmio\mmalloc.c

"$(INTDIR)\mmalloc.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\mmio\mmerror.c

"$(INTDIR)\mmerror.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\mmio\mmio.c

"$(INTDIR)\mmio.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\mikwin.c

"$(INTDIR)\mikwin.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\playercode\mplayer.c

"$(INTDIR)\mplayer.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\playercode\virtch.c

"$(INTDIR)\virtch.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\playercode\virtch2.c

"$(INTDIR)\virtch2.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\playercode\virtch_common.c

"$(INTDIR)\virtch_common.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\mikwin.rc

"$(INTDIR)\mikwin.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) $(RSC_PROJ) $(SOURCE)



!ENDIF 

