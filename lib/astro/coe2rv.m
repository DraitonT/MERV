function [r_I, v_I] = coe2rv(e, a, i, w, Om, TA, mu)
    % COE2RV calculates the state vector (position and velocity vector) from the 
    % classicial orbital elements
    % Inputs:
    % e - Eccentricity [1x1]
    % a - Semi-major axis (km) [1x1]
    % i - Inclination (deg) [1x1]
    % w - Argument of perigee (deg) [1x1]
    % Om - Right Ascension of Ascending Node (deg) [1x1]
    % TA - True Anomaly (deg) [1x1]
    %
    % Outputs:
    % r_I - Position vector (km) [3x1]
    % v_I - Velocity vector (km) [3x1]

    % Calculate the specific angular momentum
    h = sqrt(a * mu * (1 - e^2));
    
    % Calculate the magnitude of the radius vector
    norm_r = h^2 / mu / (1 + e * cosd(TA));
    
    % Position vector in the perifocal frame
    r_PF = norm_r * [cosd(TA), sind(TA), 0]';
    
    % Velocity vector in the perifocal frame
    v_PF = mu / h * [-sind(TA), e + cosd(TA), 0]';
    
    % Rotation matrix from the inertial frame to the frame defined by the
    % longitude of ascending node (Om)
    R_O = [cosd(Om) sind(Om) 0; -sind(Om) cosd(Om) 0; 0 0 1];
    
    % Rotation matrix from the frame defined by Om to the frame defined by
    % the inclination (i)
    R_i = [1, 0, 0; 0 cosd(i) sind(i); 0 -sind(i) cosd(i)];
    
    % Rotation matrix from the frame defined by the inclination to the
    % perifocal frame, which is defined by the argument of periapsis (w)
    R_w = [cosd(w) sind(w) 0; -sind(w) cosd(w) 0; 0 0 1];
    
    % Combined rotation matrix from the perifocal frame to the inertial frame
    R_PI = R_w * R_i * R_O;
    
    % The transpose of R_PI gives the rotation matrix from the inertial
    % frame to the perifocal frame
    R_IP = R_PI';
    
    % Convert position and velocity vectors from the perifocal frame to the
    % inertial frame
    r_I = R_IP * r_PF;
    v_I = R_IP * v_PF;
end
