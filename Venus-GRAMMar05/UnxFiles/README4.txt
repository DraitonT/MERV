              Auxiliary Programs for Use with Venus-GRAM

This file discusses two auxiliary programs, provided for use with
Venus-GRAM: bldtraj.f and finddate.f.  The stand-alone version Venus-GRAM 
program files (venusgrm_V05.f, venssubs_V05.f, and setup_V05.f) are discussed 
in file README1.txt.   Files README1.txt and README2.txt discuss dummy 
trajectory programs dumytraj_V05.f and multtraj_V05.f, which provide examples 
of how to use Venus-GRAM as a subroutine in trajectory programs or orbit 
propagator programs.  


                            PROGRAM BLDTRAJ

bldtraj.f    - program to build pseudo-trajectory file for using in Venus-
                GRAM to compute output for maps or cross-sections

It is frequently desirable to produce Venus-GRAM output for graphing as a
map (i.e. lat-lon cross section at a given height) or other cross-section
(e.g. height-lat cross section at a given longitude).  Program bldtraj.f
generates a "trajectory" file (with input lines containing time, height,
latitude, and longitude) that can be used as Venus-GRAM input for
generation of such maps or cross-sections.  Program bldtraj is interactive
and prompts the user to input starting values, ending values, and step
sizes for height (z1,z2,dz), latitude (lat1,lat2,dlat), and longitude
(lon1,lon2,dlon).  The program also prompts for a value of time increment
which is applied between each "trajectory" step (the time increment may
be 0, if all trajectory points are at the same time). Time values in the
trajectory file are time (seconds) from the start time specified by date
and time information provided in the Venus-GRAM NAMELIST-format input file.

Example:
For a lat-lon map at height 10 km, between latitudes -30 and 30 degrees 
(in steps of 5 degrees), and longitudes 0 to 180 degrees (in steps of 20 
degrees), enter 10 10 0 for z1, z2, dz; enter -30 30 5 for lat1, lat2, 
dlat; and enter 0 180 20 for lon1, lon2, dlon.  All of these input 
quantities are of type real, and can be entered to one or more significant 
digits beyond the decimal.


                            PROGRAM FINDDATE

finddate.f   - utility to find Earth date/time for desired Ls or Venus time

Program finddate.f allows calculation of planetocentric longitude of the Sun
(Ls) and Venus local true solar time (LTST) for a given Earth date and time.
It also computes the next occurrence (beyond the initial input date and
time) of the Earth date and time for which Ls and LTST are any desired
values.  Accuracy information and other documentary comments are given 
within the source code.  The program is interactive and prompts for all 
necessary inputs. Options allow time to be expressed in either Terrestrial 
Dynamical Time (TT) or Coordinated Universal Time (UTC) and in either 
Venus-Event time (VET) or Earth-Receive time (ERT).
