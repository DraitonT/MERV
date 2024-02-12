                      Input Data Files for Venus-GRAM

VIRA INPUT DATA FILES

Input (ASCII-format) atmospheric data files for Venus-GRAM are from Kliore, 
A. J., V. I. Moroz, and G. M. Keating (1986), "The Venus International 
Reference Atmosphere", Advances in Space Research, vol. 5, no. 11, 1985, 
pages 1-304, Pergamon Press, Oxford, referred to here as VIRA.

In the low-altitude VIRA file (VIRALow.txt, 0-100 km) dependence is on 
height and latitude.  The middle-atmosphere (100-150 km) data file
(VIRAMid.txt) has dependence on height and local solar time (LST=0 Venus
hours or LST=12 Venus hours).  Upper-altitude (150-250 km) data
(VIRAHi.txt) depends on height and solar zenith angle.  Venus-GRAM
insures smooth variation between height regions by averaging values at
the two transition heights (i.e. low and middle data are averaged at 100
km, and middle and high data are averaged at 150 km.

Parameters given in these files include height (km), pressure (N/m**2),
density (kg/m**3), temperature (K), CO2 number density (#/m**3), N2 number 
density (#/m**3), O number density (#/m**3), CO number density (#/m**3),
He number density (#/m**3), N number density (#/m**3), and H number 
density (#/m**3).  Venus-GRAM also reads, from the low-altitude data file
only, atmospheric gas constant (R, in SI units) and dimensionless 
compressibility factor [ p/(rho*R*T) ]. 


OPTIONAL TRAJECTORY INPUT FILES

An optional trajectory input file is read from a file name given by input
parameter TRAJFILE. if the number of positions to be calculated (NPOS) is
set to 0.  Each line of the trajectory file consists of: (1) time, in
seconds past the start time specified in the NAMELIST input, (2) height, in
km, (3) latitude in degrees, and (4) longitude in degrees. Longitudes are 
East or West, as set by input parameter LonEast.  Any additional REFERENCE 
information included on each line (e.g. orbit number, measured density, etc.) 
is ignored.  Trajectory positions in these files do not have to be at small 
time or space steps.  For example, a trajectory file may consist of successive 
periapsis times and positions for a simulated (or observed) aerobraking 
operation. Trajectory files may also contain arrays of locations used for 
computing height-latitude cross sections or latitude-longitude cross sections.  
Such trajectory input files can be as built by program bldtraj.f (see 
README4.txt).


OPTIONAL AUXILIARY PROFILE INPUT FILE

As an option, data read from an auxiliary profile may be used to replace
data from the VIRA data files.  This option is controlled by setting parameters
"profile", profnear, and proffar in the NAMELIST input file.  Parameter profile
gives the file name containing the profile data values.  Parameter profnear is 
the latitude-longitude radius (in degrees) within which weight for the 
auxiliary profile is 1.0.  Parameter proffar is the latitude-longitude radius 
(in degrees) beyond which weight for the auxiliary profile is 0.0.  Each line 
of the auxiliary profile input file consists of: (1) height, in km, (2) 
latitude, in degrees, (3) longitude, in degrees, (4) temperature, in K, (5) 
pressure, in N/m**2, (6) density, in kg/m**3, (7) Eastward wind, in m/s, and 
(8) Northward wind, in m/s.  Longitudes are East or West, as set by input 
parameter LonEast. VIRA temperature, pressure, and density data are used if 
any of the profile inputs for temperature, pressure, or density are 0.  
VIRA winds are used if BOTH wind components are 0 on the profile file.  A 
weighting factor for the profile data (profwgt), having values between 0 and 1, 
is applied between radii proffar and profnear.  Mean conditions are as given in 
the profile file if the desired point is within a lat-lon radius of profnear 
from the profile lat-lon at the given altitude; mean conditions are as given by 
the original VIRA data if the desired point is beyond a lat-lon radius of 
proffar from the lat-lon of the profile at the given altitude.  If profnear = 
0, then profile data are NOT used. The profile weight factor (profwgt) for the 
auxiliary profile also varies between 0 at the first profile altitude level and 
1 at the second profile altitude level (and between 1 at the next-to-last 
profile altitude level and 0 at the last profile altitude level).  First and 
second profile points (and next-to-last and last profile points) should 
therefore be selected widely enough apart in altitude that a smooth transition 
can occur as profwgt changes form 0 to 1 near these profile end points.  
