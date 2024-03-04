function [r_vector, v_vector] = rarp2rv(r_a, r_p)
% Calculations
mu_venus = 324859;
a = (r_a + r_p) / 2; % Semi-major axis in meters
e = (r_a - r_p) / (r_a + r_p); % Eccentricity
v_p = sqrt(mu_venus * (2 / r_p - 1 / a)); % Velocity at periapsis in m/s

% State Vector at Periapsis
r_vector = [r_p, 100, 200]; % Position vector at periapsis in meters
v_vector = [90, v_p, 50]; % Velocity vector at periapsis in m/s
end
