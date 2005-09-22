# Microsoft Developer Studio Project File - Name="swt2k" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=swt2k - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "swt2k.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "swt2k.mak" CFG="swt2k - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "swt2k - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "swt2k - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "swt2k - Win32 Release"

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
# ADD BASE F90 /compile_only /nologo /warn:nofileopt
# ADD F90 /assume:underscore /check:bounds /compile_only /iface:cref /names:lowercase /nologo /real_size:64 /traceback /warn:argument_checking /warn:nofileopt
# SUBTRACT F90 /browser
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /FR /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /stack:0x989680 /subsystem:console /machine:I386
# SUBTRACT LINK32 /incremental:yes

!ELSEIF  "$(CFG)" == "swt2k - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /check:bounds /compile_only /debug:full /fpe:0 /nologo /real_size:64 /traceback /warn:argument_checking /warn:nofileopt
# SUBTRACT F90 /warn:truncated_source /warn:unused
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "swt2k - Win32 Release"
# Name "swt2k - Win32 Debug"
# Begin Source File

SOURCE=.\ctime.f
# End Source File
# Begin Source File

SOURCE=.\daf1.f
DEP_F90_DAF1_=\
	".\ground.com"\
	".\params.inc"\
	".\startdaf.com"\
	
# End Source File
# Begin Source File

SOURCE=.\de45.f
# End Source File
# Begin Source File

SOURCE=.\glo1bas6.f
DEP_F90_GLO1B=\
	".\openspec.inc"\
	".\parallel.inc"\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\gmg1.f
# End Source File
# Begin Source File

SOURCE=.\gutsdaf.f
DEP_F90_GUTSD=\
	".\params.inc"\
	".\startdaf.com"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf1bas6.f
# End Source File
# Begin Source File

SOURCE=.\gwf1bcf6.f
# End Source File
# Begin Source File

SOURCE=.\gwf1chd6.f
# End Source File
# Begin Source File

SOURCE=.\gwf1drn6.f
# End Source File
# Begin Source File

SOURCE=.\gwf1drt1.f
DEP_F90_GWF1D=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf1ets1.f
# End Source File
# Begin Source File

SOURCE=.\gwf1evt6.f
# End Source File
# Begin Source File

SOURCE=.\gwf1fhb1.f
# End Source File
# Begin Source File

SOURCE=.\gwf1gag5.f
# End Source File
# Begin Source File

SOURCE=.\gwf1ghb6.f
# End Source File
# Begin Source File

SOURCE=.\gwf1hfb6.f
DEP_F90_GWF1H=\
	".\openspec.inc"\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf1huf2.f
DEP_F90_GWF1HU=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf1ibs6.f
# End Source File
# Begin Source File

SOURCE=.\gwf1lak3.f
# End Source File
# Begin Source File

SOURCE=.\gwf1lpf1.f
DEP_F90_GWF1L=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf1mnw1.f
# End Source File
# Begin Source File

SOURCE=.\gwf1rch6.f
# End Source File
# Begin Source File

SOURCE=.\gwf1res1.f
# End Source File
# Begin Source File

SOURCE=.\gwf1riv6.f
# End Source File
# Begin Source File

SOURCE=.\gwf1sfr1.f
DEP_F90_GWF1S=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf1str6.f
# End Source File
# Begin Source File

SOURCE=.\gwf1sub1.f
# End Source File
# Begin Source File

SOURCE=.\gwf1wel6.f
# End Source File
# Begin Source File

SOURCE=.\hufutl2.f
DEP_F90_HUFUT=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\hydmod.f
DEP_F90_HYDMO=\
	".\hydmod.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\imt1adv5.for
# End Source File
# Begin Source File

SOURCE=.\imt1btn5.for
DEP_F90_IMT1B=\
	".\FILESPEC.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\imt1dsp5.for
# End Source File
# Begin Source File

SOURCE=.\imt1fmi5.for
# End Source File
# Begin Source File

SOURCE=.\imt1gcg5.for
# End Source File
# Begin Source File

SOURCE=.\imt1rct5.for
# End Source File
# Begin Source File

SOURCE=.\imt1ssm5.for
# End Source File
# Begin Source File

SOURCE=.\imt1tob5.for
# End Source File
# Begin Source File

SOURCE=.\imt1utl5.for
DEP_F90_IMT1U=\
	".\FILESPEC.INC"\
	
# End Source File
# Begin Source File

SOURCE=.\lmg1.f
# End Source File
# Begin Source File

SOURCE=.\lmt6.f
DEP_F90_LMT6_=\
	".\openspec.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\memchk.f
# End Source File
# Begin Source File

SOURCE=.\obs1adv2.f
DEP_F90_OBS1A=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\obs1bas6.f
DEP_F90_OBS1B=\
	".\parallel.inc"\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\obs1bas6vd.f
DEP_F90_OBS1BA=\
	".\param.inc"\
	".\vdf.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\obs1drn6.f
DEP_F90_OBS1D=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\obs1drn6vd.f
DEP_F90_OBS1DR=\
	".\param.inc"\
	".\vdf.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\obs1drt1.f
DEP_F90_OBS1DRT=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\obs1ghb6.f
DEP_F90_OBS1G=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\obs1ghb6vd.f
DEP_F90_OBS1GH=\
	".\param.inc"\
	".\vdf.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\obs1riv6.f
DEP_F90_OBS1R=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\obs1riv6vd.f
DEP_F90_OBS1RI=\
	".\param.inc"\
	".\vdf.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\obs1str6.f
DEP_F90_OBS1S=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=".\para-non.f"
DEP_F90_PARA_=\
	".\parallel.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\parutl1.f
DEP_F90_PARUT=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\pcg2.f
# End Source File
# Begin Source File

SOURCE=.\pes1bas6.f
DEP_F90_PES1B=\
	".\openspec.inc"\
	".\parallel.inc"\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\pes1gau1.f
DEP_F90_PES1G=\
	".\parallel.inc"\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\rtedaf.f
DEP_F90_RTEDA=\
	".\params.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\sen1bas6.f
DEP_F90_SEN1B=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\sen1chd6.f
DEP_F90_SEN1C=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\sen1drn6.f
DEP_F90_SEN1D=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\sen1drt1.f
DEP_F90_SEN1DR=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\sen1ets1.f
DEP_F90_SEN1E=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\sen1evt6.f
DEP_F90_SEN1EV=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\sen1ghb6.f
DEP_F90_SEN1G=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\sen1hfb6.f
DEP_F90_SEN1H=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\sen1huf2.f
DEP_F90_SEN1HU=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\sen1lpf1.f
DEP_F90_SEN1L=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\sen1rch6.f
DEP_F90_SEN1R=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\sen1riv6.f
DEP_F90_SEN1RI=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\sen1str6.f
DEP_F90_SEN1S=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\sen1wel6.f
DEP_F90_SEN1W=\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\sip5.f
# End Source File
# Begin Source File

SOURCE=.\sor5.f
# End Source File
# Begin Source File

SOURCE=.\swt2k.f
DEP_F90_SWT2K=\
	".\lmt6vd.inc"\
	".\openspec.inc"\
	".\parallel.inc"\
	".\param.inc"\
	".\vdf.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\utl6.f
DEP_F90_UTL6_=\
	".\openspec.inc"\
	".\param.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\vdf1.f
DEP_F90_VDF1_=\
	".\vdf.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\vdf1bas6.for
# End Source File
# Begin Source File

SOURCE=.\vdf1bcf6.f
DEP_F90_VDF1B=\
	".\vdf.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\vdf1chd6.f
# End Source File
# Begin Source File

SOURCE=.\vdf1drn6.f
DEP_F90_VDF1D=\
	".\vdf.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\vdf1evt6.f
DEP_F90_VDF1E=\
	".\vdf.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\vdf1fhb1.f
DEP_F90_VDF1F=\
	".\vdf.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\vdf1ghb6.f
DEP_F90_VDF1G=\
	".\vdf.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\vdf1huf2.f
DEP_F90_VDF1H=\
	".\param.inc"\
	".\vdf.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\vdf1lmt6.f
DEP_F90_VDF1L=\
	".\vdf.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\vdf1lpf1.for
DEP_F90_VDF1LP=\
	".\vdf.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\vdf1mnw1.f
DEP_F90_VDF1M=\
	".\vdf.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\vdf1rch6.f
DEP_F90_VDF1R=\
	".\vdf.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\vdf1riv6.f
DEP_F90_VDF1RI=\
	".\vdf.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\vdf1wel6.f
DEP_F90_VDF1W=\
	".\vdf.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\gmg1.2.lib
# End Source File
# End Target
# End Project
