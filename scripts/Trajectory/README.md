# Trajectory
This folder contains essential scripts and functions utilized by the trajectory team to design the interplanetary journey between Earth and Venus, and allowing for the study of several departure and arrival dates. 

# Functions/Packages

## ARO3090_Porkchop_Plots
This script generates porkchop plots, which layout the relationship between launch & arrival dates and their associated energy. This tool is crucial for determining the energy required for launch, $C_3$, which is directly linked to the amount of useful spacecraft mass for your mission, making it essential for sizing any spacecraft during the Space Mission Design Process.

### Inputs
The user is allowed to adjust the following parameters:

1. $Planet_1 \rightarrow$ Departure Planet
2. $Planet_2 \rightarrow$ Arrival Planet
3. $TOF_{min,max} \rightarrow$ Desired bounds for the time of flight
4. $dJD \rightarrow$ Arrival date increment in Julian days
5. $dTOF \rightarrow$ Time of flight increment

### Outputs
The script will iterate between several launch and arrival dates, based on the chosen inputs, and produce contours of energy and time of flight. Two plots are returned, one for the departure, and the other for arrival. 


## Interplanetary Transfer
This function uses the Two-Body Patched Conics method to calculate the Hohmann Transfer's characteristics between two celestial bodies within the solar system.

### Inputs
1. Target Planet, passed as a string
2. Arrival Planet, passed as a string
3. Specific Impulse, $I_{sp}$, of the engine use for both the departure and arrival burns
4. Parking and capture orbit apoapses and periapses in km.

### Outputs 
1. Total $\Delta V \left[km/s\right]$ for departure and arrival burns
2. Transfer time between planets $[days]$

## guessAndCheck
This script numerically integrates the Two-Body equations of motion, given a set of initial conditions. As a first-order approximation, this script was instrumental in choosing between several parking orbit trajectories a Venus-orbiting crew module.