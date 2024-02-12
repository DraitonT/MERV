Dear Venus-GRAM user,

Venus-GRAM 2004 is not a US ITAR item.  Export of Venus-GRAM 2004 is 
allowed under Export Control Classification EAR-99.  However, no recipient
of this code should forward copies outside the United States without
explicit approval by NASA Marshall Space Flight Center (see contact
information, below).

The Venus-GRAM 2004 Version 0 directory contains the following files:


               SOURCE CODE FOR STAND-ALONE Venus-GRAM PROGRAM

venusgrm_V05.f   - source code for the "stand-alone" version main program
venssubs_V05.f   - subroutines used by both venusgrm and dumytraj versions
setup_V05.f      - setup routines used by both venusgrm and dumytraj versions


    SOURCE CODE FOR AUXILIARY PROGRAMS (SEE README2.TXT AND README4.TXT)

bldtraj.f        - program to build pseudo-trajectory file for using in Venus-
                    GRAM to compute output for maps or cross-sections
dumytraj_V05.f   - source code for the dummy trajectory version main program.
                    To be compiled with venssubs_V05.f, setup_V05.f, and wrapper_V05.f, 
                    this program illustrates use of Venus-GRAM as a subroutine 
                    in trajectory programs or orbit propagator programs.
wrapper_V05.f    - code for Venustraj "wrapper" subroutine
multtraj_V05.f   - source code for a dummy trajectory main program that
                    computes multiple trajectories during one run.
                    To be compiled with venssubs_V05.f, setup_V05.f, and wrapper_V05.f, 
                    this program illustrates use of Venus-GRAM as a subroutine 
                    in trajectory programs or orbit propagator programs that
                    compute positions of multiple spacecraft during one run.
finddate.f       - utility to find Earth date/time for desired Ls or Venus time                


    DATA FILES (ASCII FORMAT) FOR Venus ATMOSPHERE DATA (SEE README3.TXT)

VIRALow.txt  - data file for Venus International Reference Atmosphere
                (VIRA) low-altitude data (0-100 km)
VIRAMid.txt  - data file for VIRA middle-altitude data (100-150 km)
VIRAHi.txt   - data file for VIRA high-altitude data (150-250 km)

                       SAMPLE INPUT AND OUTPUT FILES

inputref.txt - commented test input file for reference case
listref.txt  - LIST output file for reference case
               

                  TEXT FILES DOCUMENTING PROGRAM FEATURES

headers.txt  - list of output files and file header definitions
Venusfix.txt - history of code changes since first posting of Version 0
venusgrm.hst - history file summarizing various versions and changes
README1.txt  - this general program introduction file
README2.txt  - discussion of dumytraj and multtraj dummy trajectory 
                programs and Venustraj "wrapper" routine
README3.txt  - discussion of input atmospheric data files
README4.txt  - discussion of auxiliary programs bldtraj and finddate
README5.txt  - discussion of Venus-GRAM sample input and output files,
                including reference test case
xycodes.txt  - list of x-y plot codes (also given below)
VenusSources.txt - List of some data sources used to develop Venus-GRAM


Plotable output files can be generated with data given versus
several selected parameters.  Generation of LIST file output and
plotable output files is controlled by the value of iup on input.
For Venus-GRAM 2004, a number of plotable output files are generated,
each containing several parameters suitable for plotting.  These 
plotable files have headers to help identify parameters in the files.
File names and definitions of headers are given in the file headers.txt.

Plotable x and y parameters and their code values (set by input variables
NVARX and NVARY) are as follows:

 Code              Parameter
 ----   ------------------------------------------------------------ 
   1    Height (planetocentric, above reference ellipsoid, km)               
   2    Radius (planetocentric radius = planet radius + height, km)
   3    Latitude (deg.)                                  
   4    Longitude (deg.) East if LonEast=1; West if LonEast=0 
   5    Time from start (seconds: ERT/VET/UTC/TT per input)
   6    Time from start (Venus days)
   7    Planetocentric longitude of Sun (Ls, degrees)
   8    Local true solar time (Venus hours = 1/24th Titan day)
   9    Pressure (mb)                                         
  10    Pressure Height [-log(Pres/PresSurf) = -log(sigma)]   
  11    Sigma coordinate [sigma=Pressure/(Surface Pressure)]
  12    Solar zenith angle (degrees)
  13    Longitude in range -180 to +180 (East or West, controlled by LonEast)
  
To compile venusgrm_V05, dumytraj_V05, and multtraj_V05 under UNIX, to produce 
executable files venusgrm_V05.x, dumytraj_V05.x, and multtraj_V05.x, you can use 
the commands:

f77 -o venusgrm_V05.x venusgrm_V05.f venssubs_V05.f setup_V05.f
f77 -o dumytraj_V05.x dumytraj_V05.f wrapper_V05.f venssubs_V05.f setup_V05.f
f77 -o multtraj_V05.x multtraj_V05.f wrapper_V05.f venssubs_V05.f setup_V05.f

To compile venusgrm_V05, dumytraj_V05, and multtraj_V05 under PC-DOS (for 
example, with Microsoft FORTRAN Powerstation), to produce executable 
files venusgrm_V05.exe, dumytraj_V05.exe, and multtraj_V05.exe, you can use the 
commands:

fl32 venusgrm_V05.f venssubs_V05.f setup_V05.f
fl32 dumytraj_V05.f wrapper_V05.f venssubs_V05.f setup_V05.f
fl32 multtraj_V05.f wrapper_V05.f venssubs_V05.f setup_V05.f

To compile the auxiliary program bldtraj.f, just use the FORTRAN compile 
statement for this source code file

f77 -o bldtraj.x bldtraj.f

or

fl32 bldtraj.f


For technical questions or problems, please contact:

Dr. C. G. (Jere) Justus          
NASA MSFC ED44/CSC
Marshall Space Flight Center, AL 35812
phone:  (256)-544-3260           
fax:    (256)-544-5754
e-mail: Carl.G.Justus@nasa.gov      

For programmatic or policy questions, please contact

Hilary Justh
NASA MSFC EV13
Marshall Space Flight Center, AL 35812
phone: (256)-544-3694
fax:   (256)-544-5754
e-mail: Hilary.L.Justh@nasa.gov
