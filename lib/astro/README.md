# Astro
This folder contains a number of essential orbital mechanic functions used within a number of scripts by MERV. 

# Functions/Packages

## Orbital
At the current state, Orbital acts as cumulation of a number of orbital mechanics related functions developed by MERV ranging from:

1. Orbital Determination 
2. Propagation 
3. Visualization 
4. Conversions 
5. API Calls 

The team plans to seperate this large package into a number of smaller specialized packages and functions in the near future. 

## anglesOfStates 
To calculate the entry angle of the trajectory along with the angle along it, the team developed this function: 

$$ 

\theta = \cos^{-1} \frac{\vec{v_{1}} \cdot \vec{v_{2}}}{|| v_{1} \times v_{2} ||}

$$

where 
1. $v_{1}$ is the first vector of interest 
2. $v_{2}$ is the second vector of interest
3. $\theta$ is the angle between the two vectors

## ap2rv
Calculates the state vectors of the orbit based on the provided radius of apoapsis and radius of periapsis.

$$

a = \frac{(r_a + r_p)}{2} 

$$

$$

e = \frac{(r_a - r_p)}{(r_a + r_p)}

$$

$$

[r_I, v_I] = coe2rv(e, a, 0, 0, 0, 0, \mu);

$$

## coe2rv
