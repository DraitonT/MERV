function [a, e, TA, RAAN, AOP, i, h_vec] = rv2coe(r, v)
        mu = 132712440018; 
        r_mag = vecnorm(r);

        v_mag = vecnorm(v);

        v_r = dot(r,v)/r_mag;
            
        h_vec = cross(r,v);
        h_mag = vecnorm(h_vec);
    
        h_z = h_vec(3);
        i = acos(h_z/h_mag);
        N_vector = cross([0,0,1] , h_vec);
        N_mag = norm(N_vector);

        RAAN = acos(N_vector(1)/vecnorm(N_vector));

        E = 1/mu * ((v_mag^2 - (mu/r_mag))*r - r_mag * v_r * v);
        e = norm(E);

        AOP = 2 * pi - acos(dot(N_vector, E)/(N_mag * e));

        TA = acos((dot(E, r))/(e * norm(r)));

        r_p = h_mag^2/mu * (1/(1+e*cos(0)));
        r_a = h_mag^2/mu * (1/(1+e*cos(180)));
        % 
        a = (r_a + r_p)/2;
        % 
        % 
        % a = (rp + ra)/2;                        % Semi-major axis [km] (2.71, pg. 81 | Curtis)
        % e = -((rp/a) - 1);                      % Eccentricity [unitless](2.73, pg. 82 | Curtis)
        % V_c = sqrt(mu / r_analytical);
        % T_c = (2*pi/sqrt(mu)) * a^(3/2);        % Orbital Period [seconds] (2.83, pg. 84 | Curtis)
        % epsilon = -mu/(2*a);                    % Specific Energy [km^2/s^2] (2.80, pg. 83) | Curtis)
        % h = sqrt(2*mu) * sqrt((ra*rp)/(ra+rp)); % Angular momentum [kg*km^2/s] (6.2, pg. 290 | Curtis)
        % n = (2 * pi)/ T_c;                      % Mean motion [rad/s] (3.9, pg. 144 | Curtis)
        % v_p = h/rp;                             % Velocity at perigee [km/s] (2.31, pg. 69 | Curtis)
        % v_a = h/ra;                             % Velocity at apogee [km/s] (2.31, pg. 69 | Curtis)
end