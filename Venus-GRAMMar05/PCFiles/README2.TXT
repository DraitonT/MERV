             DUMMY TRAJECTORY PROGRAMS (dumytraj.f and multtraj.f)


	In dumytraj.f and multtraj.f, interaction with Venus-GRAM 2004 is 
via three calls to one "wrapper" subroutine (subroutine Venustraj_V05 in file 
wrapper_V05.f), but with different values of three control parameters (isetup, 
jmonte, and istep) -

                Call Venustraj_V05(...)      with isetup=1
                Call Venustraj_V05(...)      with isetup=0, jmonte>0, istep=0
                Call Venustraj_V05(...)      with isetup=0, jmonte=0, istep>0

where isetup = 1 triggers the call to the Setup_V05 subroutine, jmonte>0
triggers the call to the reinitialization process (including the call to
the Randinit_V05 subroutine), and istep = 1 to MAXNUM is a counter for steps
along the trajectory (with a call to the Datastep_V05 subroutine at each step).
Venustraj_V05 is a subroutine in the dumytraj_V05.f or multtraj_V05.f code, and 
must be included (along with the basic Venus-GRAM code setup_V05.f and 
venssubs_V05.f ) as a subroutine in the user's calling trajectory program.
	To compile dumytraj_V05 or multtraj_V05 under UNIX, to produce 
executable files dumytraj_V05.x, or multtraj_V05.x, you can use the commands:

f77 -o dumytraj_V05.x dumytraj_V05.f wrapper_V05.f venssubs_V05.f setup_V05.f
f77 -o multtraj_V05.x multtraj_V05.f wrapper_V05.f venssubs_V05.f setup_V05.f

To compile dumytraj_V05 or multtraj_V05 under PC-DOS (for example, with 
Microsoft FORTRAN Powerstation), to produce executable file dumytraj_V05.exe, 
or multtraj_V05.exe, you can use the commands:

fl32 dumytraj_V05.f wrapper_V05.f venssubs_V05.f setup_V05.f
fl32 multtraj_V05.f wrapper_V05.f venssubs_V05.f setup_V05.f

        In dumytraj_V05.f or multtraj_V05.f, input variables to the 
Venustraj_V05 subroutine are current and next (double precision) position 
values (height, latitude, and longitude).  Position increments to be passed 
to the Datastep_V05 subroutine (increments of height, latitude, and longitude) 
are computed within the Venustraj_V05 subroutine. 

        It is worthwhile to read the comments embedded in the dumytraj_V05.f 
code.  These comments give more explicit descriptions of the functions 
that are being performed.  They also provide better hints about what to do 
if you are using predictor-corrector (or other) trajectory approaches that 
require mid-point corrections along trajectory steps and/or the use of 
density variations that occur within each trajectory step.



For technical questions or problems, please contact

Dr. C. G. (Jere) Justus                
NASA MSFC  ED44/CSC
Marshall Space Flight Center, AL 35812
phone: (256)-544-3260                  
fax:   (256)-544-5754
e-mail: Carl.G.Justus@nasa.gov         

For programmatic or policy questions, please contact

Hilary Justh
NASA MSFC EV13
Marshall Space Flight Center, AL 35812
phone: (256)-544-3694
fax:   (256)-544-5754
e-mail: Hilary.L.Justh@nasa.gov

