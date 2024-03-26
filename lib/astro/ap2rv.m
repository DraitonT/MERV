function [r_I, v_I] = ap2rv(r_a, r_p)
% AP2RV calculates the state vectors of the orbit based on the radius of apoapsis and radius of periapsis
% 
% Inputs:
% r_a - Radius of apoapsis (km) [1x1]
% r_p - Radius of periapsis (km) [1x1]
% 
% Outputs:
% r_I - Position vector (km) [3x1]
% v_I - Velocity vector (km/s) [3x1]


% Calculating known COEs from r_a and r_p
mu = 324859; % [km^2/s^2] Gravitational parameter of Venus
a = (r_a + r_p) / 2; % Semi-major axis in meters
e = (r_a - r_p) / (r_a + r_p); % Eccentricity
% v_p = sqrt(mu_venus * (2 / r_p - 1 / a)); % Velocity at periapsis in m/s

% State vectors at periapsis 
[r_I, v_I] = coe2rv(e, a, 0, 0, 0, 0, mu);

end
