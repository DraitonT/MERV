classdef Orbital
    methods(Static)

    %% 1.0 Numerical Two-body Equation Solver (ODE45)
        function [t, state, csvFileName] = numericalTwoBody(initialConditions, T, numOrbits, optionsCorrections, toleranceMin, toleranceMax)
            r_x = initialConditions(1);
            r_y = initialConditions(2);
            r_z = initialConditions(3);
            v_x = initialConditions(4);
            v_y = initialConditions(5);
            v_z = initialConditions(6);

            r = [r_x, r_y, r_z]; % [km] Initial position vector 
            v = [v_x, v_y, v_z]; % [km/s] Initial velocity vector
            % Combine initial position and velocity into the state vector
            
            initialState = [r, v]; 
        
        % Integrating 2nd Order ODE
        
            % Define the time span for integration (e.g., 0 to 2 hours)
            tspan = [0, T * numOrbits]; % Integration time in seconds (2 hours)
            
            % Use ode45 to integrate the equations of motion
            if optionsCorrections
                options = odeset('RelTol', toleranceMin, 'AbsTol', toleranceMax);
                [t, state] = ode45(@(t, state) Orbital.twoBodyEOMHelio(t, state), tspan, initialState, options);
                tolerance = "Tolerance";
                csvFileName = sprintf("%s %s (%d_%d).csv", t(end),tolerance, toleranceMin, toleranceMax); %Add ODE name (45 and 115)
                writematrix([t, state], csvFileName)
            else
                [t, state] = ode45(@(t, state) Orbital.twoBodyEOMHelio(t, state), tspan, initialState);
                tolerance = "No tolerance";
                csvFileName = sprintf("%s.csv", tolerance); %Add ODE name (45 and 115)
                writematrix([t, state], csvFileName)
            end

        end

        function [t, state, csvFileName] = numericalTwoBodyEarth(initialConditions, T, numOrbits, optionsCorrections, toleranceMin, toleranceMax)
            r_x = initialConditions(1);
            r_y = initialConditions(2);
            r_z = initialConditions(3);
            v_x = initialConditions(4);
            v_y = initialConditions(5);
            v_z = initialConditions(6);

            r = [r_x, r_y, r_z]; % [km] Initial position vector 
            v = [v_x, v_y, v_z]; % [km/s] Initial velocity vector
            % Combine initial position and velocity into the state vector
            
            initialState = [r, v]; 
        
        % Integrating 2nd Order ODE
        
            % Define the time span for integration (e.g., 0 to 2 hours)
            tspan = [0, T * numOrbits]; % Integration time in seconds (2 hours)
            
            % Use ode45 to integrate the equations of motion
            if optionsCorrections
                options = odeset('RelTol', toleranceMin, 'AbsTol', toleranceMax);
                [t, state] = ode45(@(t, state) Orbital.twoBodyEOMEarth(t, state), tspan, initialState, options);
                tolerance = "Tolerance";
                csvFileName = sprintf("%s %s (%d_%d).csv", t(end),tolerance, toleranceMin, toleranceMax); %Add ODE name (45 and 115)
                writematrix([t, state], csvFileName)
            else
                [t, state] = ode45(@(t, state) Orbital.twoBodyEOMEarth(t, state), tspan, initialState);
                tolerance = "No tolerance";
                csvFileName = sprintf("%s.csv", tolerance); %Add ODE name (45 and 115)
                writematrix([t, state], csvFileName)
            end

        end
        
        %% 1.1 Numerical Two-body Equation Solver (ODE45)
            function [t, state] = numericalTwoBody113(initialConditions, T, numOrbits, optionsCorrections, toleranceMin, toleranceMax)
            

            r_x = initialConditions(1);
            r_y = initialConditions(2);
            r_z = initialConditions(3);
            v_x = initialConditions(4);
            v_y = initialConditions(5);
            v_z = initialConditions(6);

            r = [r_x, r_y, r_z]; % [km] Initial position vector 
            v = [v_x, v_y, v_z]; % [km/s] Initial velocity vector
            % Combine initial position and velocity into the state vector
            
            initialState = [r, v]; 
        
        % Integrating 2nd Order ODE
        
            % Define the time span for integration (e.g., 0 to 2 hours)
            tspan = [0, T * numOrbits]; % Integration time in seconds (2 hours)
            
            % Use ode45 to integrate the equations of motion
            if optionsCorrections
                options = odeset('RelTol', toleranceMin, 'AbsTol', toleranceMax);
                [t, state] = ode113(@(t, state) Orbital.twoBodyEOM(t, state), tspan, initialState, options);
                tolerance = "Tolerance";
                csvFileName = sprintf("%s (%d_%d).csv", tolerance, toleranceMin, toleranceMax); %Add ODE name (45 and 115)
                writematrix([t, state], csvFileName)
            else
                [t, state] = ode113(@(t, state) Orbital.twoBodyEOM(t, state), tspan, initialState);
                tolerance = "No tolerance (ODE113)";
                csvFileName = sprintf("%s.csv", tolerance); %Add ODE name (45 and 115)
                writematrix([t, state], csvFileName)
            end

        end

        %% 2.0 Plot Orbits using CSV
         function plot2BodyCSV(mu, csvFileName, plotOrbit, analyticalVsNumericalPlot, orbitsEarthFileName, analyticalVsNumericalRFileName, correction, multipleOrbits, multipleVelocity, velocityVsTime, velocityVsTimeFileName)
            earthTextureImage = append(pwd, '\..\..\data\earth_texture.png');
            state = csvread(csvFileName);
            % Extract the position vectors from the state variable
            t = state(:, 1);
            x = state(:, 2);
            y = state(:, 3);
            z = state(:, 4);
            v_x = state(:,5);
            v_y = state(:,6);
            v_z = state(:,7);
            velocityMagnitude = sqrt(v_x.^2 + v_y.^2 + v_z.^2);
            
            % Define the radius of the Earth (in kilometers)
            earthRadius = 6371; % Approximate radius of the Earth
            
            % Create a sphere for Earth
            [xEarth, yEarth, zEarth] = sphere;
            xEarth = xEarth * earthRadius;
            yEarth = yEarth * earthRadius;
            zEarth = zEarth * earthRadius;
        
            earthTexture = imread(earthTextureImage); % Load your own Earth texture image
            earthTexture = flipud(earthTexture);
        

            
            % Plot the Earth as a sphere at the origin
            if plotOrbit
            % Plot the satellite's trajectory
                plot3(x, y, z, 'r');
                xlabel('X (km)');
                ylabel('Y (km)');
                zlabel('Z (km)');
                title('Satellite Circular Orbit');
                grid on;
                axis equal;
                
                hold on
                if multipleOrbits
                    surf(xEarth, yEarth, zEarth, 'CData', earthTexture, 'FaceColor', 'texturemap', 'EdgeColor', 'none');
                    axis equal;
                    xlabel('X (km)');
                    ylabel('Y (km)');
                    zlabel('Z (km)');
                    grid on
                    title(sprintf('Orbit Visualization'));
                    saveas(gcf, (append(pwd, '\', orbitsEarthFileName)));
                else
                    surf(xEarth, yEarth, zEarth, 'CData', earthTexture, 'FaceColor', 'texturemap', 'EdgeColor', 'none');
                    axis equal;
                    xlabel('X (km)');
                    ylabel('Y (km)');
                    zlabel('Z (km)');
                    grid on
                    title(sprintf('Orbit Visualization'));
                    saveas(gcf, (append(pwd, '\', orbitsEarthFileName)));
                    hold off
                end
            else
            end
        
        % Analytical Radius vs Numerical Radius
            for i = 1: length(t)
                r_numerical(i) = sqrt((x(i))^2 + (y(i))^2 + (z(i))^2);
            end
            rp = min(r_numerical);
            ra = max(r_numerical);

            h = sqrt(2*mu) * sqrt((ra*rp)/(ra+rp));
            r_analytical = round(((norm(h))^2/mu)); % Analytical radius [km] (2.62, pg. 76 | Curtis)
        
            for i = 1: length(t)
                r_analyticalArray(i) = r_analytical;
            end

            if analyticalVsNumericalPlot
                figure;
                plot(t, r_analyticalArray)
                hold on
                plot(t, r_numerical)
                xlabel('Time (in seconds)', 'Interpreter', 'latex')
                ylabel('Radius (in km)', 'Interpreter', 'latex')
                title(sprintf('Analytical vs Numerical Radius (%s)', correction), 'Interpreter', 'latex')
                legend ({'Analytical Radius', 'Numerical Radius'}, 'Location', 'southeast')
                grid on
                saveas(gcf, (append(pwd, '\', analyticalVsNumericalRFileName)));
                hold off
            else
            end

            if velocityVsTime
                if multipleVelocity
                    plot(t(1:end-100), velocityMagnitude(1:end-100))
                    hold on
                    xlabel('Time (in seconds)', 'Interpreter', 'latex')
                    ylabel('Velocity (in km/s)', 'Interpreter', 'latex')
                    title(sprintf('Time vs Velocity'), 'Interpreter', 'latex')
    %                 legend ({'Analytical Radius', 'Numerical Radius'}, 'Location', 'southeast')
                    grid on
                    saveas(gcf, (append(pwd, '\', velocityVsTimeFileName)));
                    
                else
                    figure;
                    plot(t, velocityMagnitude(1:end-100))
                    hold on
                    xlabel('Time (in seconds)', 'Interpreter', 'latex')
                    ylabel('Velocity (in km/s)', 'Interpreter', 'latex')
                    title(sprintf('Time vs Velocity'), 'Interpreter', 'latex')
    %                 legend ({'Analytical Radius', 'Numerical Radius'}, 'Location', 'southeast')
                    grid on
                    saveas(gcf, (append(pwd, '\', velocityVsTimeFileName)));
                    hold off
                end
                
            end

        
     
        % Conic Equation Parameters
            a = (ra + rp)/2;
            e = (ra - rp)/(ra + rp);
            e = -((rp/a) - 1);                      % Eccentricity [unitless](2.73, pg. 82 | Curtis)
            p = h^2/mu;                             % Semiparameter [kg^2*km] (2.53, pg. 74 | Curtis)
            a = (rp + ra)/2;                        % Semi-major axis [km] (2.71, pg. 81 | Curtis)
            V_c = sqrt(mu / r_analytical);
            T_c = (2*pi/sqrt(mu)) * a^(3/2);        % Orbital Period [seconds] (2.83, pg. 84 | Curtis)
            epsilon = -mu/(2*a);                    % Specific Energy [km^2/s^2] (2.80, pg. 83) | Curtis)
            h = sqrt(2*mu) * sqrt((ra*rp)/(ra+rp)); % Angular momentum [kg*km^2/s] (6.2, pg. 290 | Curtis)
            n = (2 * pi)/ T_c;                      % Mean motion [rad/s] (3.9, pg. 144 | Curtis)
            v_p = h/rp;                             % Velocity at perigee [km/s] (2.31, pg. 69 | Curtis)
            v_a = h/ra;                             % Velocity at apogee [km/s] (2.31, pg. 69 | Curtis)
%             fprintf('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n');
%             fprintf('                  Problem 3 \n');
%             fprintf('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n\n');
%             
%             fprintf('Part A\n');
%             fprintf('-----------------------------\n');
%             fprintf('Eccentricity:   e = %.5e  \n', e);
%             fprintf('-----------------------------\n');
%             
%             fprintf('Part B\n');
%             fprintf('-----------------------------\n');
%             fprintf('Semiparameter:  p = %.5e kg*m^2*km\n', p);
%             fprintf('-----------------------------\n');
%             
%             fprintf('Part C\n');
%             fprintf('-----------------------------\n');
%             fprintf('Semimajor Axis: a = %.5e   \n', a);
%             fprintf('-----------------------------\n');
%             
%             fprintf('Part D\n');
%             fprintf('-----------------------------\n');
%             fprintf('Periapsis range:       rp = %.5e km  \n', rp);
%             fprintf('-----------------------------\n');
%             
%             fprintf('Part E\n');
%             fprintf('-----------------------------\n');
%             fprintf('Apoapsis range:         ra = %.5e km  \n', ra);
%             fprintf('-----------------------------\n');
%             
%             fprintf('Part F\n');
%             fprintf('-----------------------------\n');
%             fprintf('Velocity at Periapsis: V_p = %.5e km/s  \n', v_p);
%             fprintf('-----------------------------\n');
%             
%             fprintf('Part G\n');
%             fprintf('-----------------------------\n');
%             fprintf('Velocity at Apoapsis: V_a = %.5e km/s  \n', v_a);
%             fprintf('-----------------------------\n');
%             
%             fprintf('Part H\n');
%             fprintf('-----------------------------\n');
%             fprintf('Orbiter period: T_c = %.5e  seconds or %.5e minutes \n', T_c, T_c/60);
%             fprintf('-----------------------------\n');    
% 
%             fprintf('Part I\n');
%             fprintf('-----------------------------\n');
%             fprintf('Specific Mechanical Energy: epsilon = %.5e km^2/s^2  \n', epsilon);
%             fprintf('-----------------------------\n');
% 
%             fprintf('Part J\n');
%             fprintf('-----------------------------\n');
%             fprintf('Angular Momentum: h = %.5e kg*km^2/s  \n', h);
%             fprintf('-----------------------------\n');
%             
%             fprintf('Part K\n');
%             fprintf('-----------------------------\n');
%             fprintf('Mean Motion: n = %.5e  rad/s\n', n);
%             fprintf('-----------------------------\n');
         end
    %% 2.1 Plots multiple radius using CSVs
         function plotMultipleRadius(csvFileName)
                state = csvread(csvFileName);
                % Extract the position vectors from the state variable
                t = state(:, 1);
                x = state(:, 2);
                y = state(:, 3);
                z = state(:, 4);
                mu = 398600;

                % Analytical Radius vs Numerical Radius
                for i = 1: length(t)
                    r_numerical(i) = sqrt((x(i))^2 + (y(i))^2 + (z(i))^2);
                end
                rp = min(r_numerical);
                ra = max(r_numerical);
    
                h = sqrt(2*mu) * sqrt((ra*rp)/(ra+rp));
                r_analytical = round(((norm(h))^2/mu)); % Analytical radius [km] (2.62, pg. 76 | Curtis)
            
                for i = 1: length(t)
                    r_analyticalArray(i) = r_analytical;
                end

                plot(t, r_numerical)
                hold on
                xlabel('Time (in seconds)', 'Interpreter', 'latex')
                ylabel('Radius (in km)', 'Interpreter', 'latex')
                title('Multiple Radius')
                legend ({'No Tolerance (ODE113)', 'No Tolerance (ODE45)','Tolerance 1 (ODE45)', 'Tolerance 2 (ODE45)'}, 'Location', 'southeast')
                grid on
         end
    %% 2.2 Plotting Orbit using time and state vectors (position and velocity)
        function plot2Body(state, color)
            earthTextureImage = append(pwd, '\..\data\earth_texture.png');
            % Extract the position vectors from the state variable
            x = state(:, 1);
            y = state(:, 2);
            z = state(:, 3);
            
            % Define the radius of the Earth (in kilometers)
            earthRadius = 6371; % Approximate radius of the Earth
            
            % Create a sphere for Earth
            [xEarth, yEarth, zEarth] = sphere;
            xEarth = xEarth * earthRadius;
            yEarth = yEarth * earthRadius;
            zEarth = zEarth * earthRadius;
        
            earthTexture = imread(earthTextureImage); % Load your own Earth texture image
            earthTexture = flipud(earthTexture);
        
            % Plot the Earth as a sphere at the origin
            % Plot the satellite's trajectory
                plot3(x, y, z, color);
                xlabel('X (km)', 'FontWeight','bold','Color','w');
                ylabel('Y (km)', 'FontWeight','bold','Color','w');
                zlabel('Z (km)', 'FontWeight','bold','Color','w');
                title('Satellite Circular Orbit', 'FontWeight','bold','Color','w');
                grid on;
                axis equal;
                set(gca,'Color',[0,0,0])
                set(gcf,'Color',[0,0,0])
                hold on
                surf(xEarth, yEarth, zEarth, 'CData', earthTexture, 'FaceColor', 'texturemap', 'EdgeColor', 'none');
                axis equal;
                xlabel('X (km)');
                ylabel('Y (km)');
                zlabel('Z (km)');
                grid on
                title(sprintf('Orbit Visualization'));
%                 saveas(gcf, (append(pwd, '\', orbitsEarthFileName)));
        
        end
    %% 2.2.1 Calculate Parameters
    function [a, e, h] = calculateOrbitalParameters(mu, t, state)
            % Extract the position vectors from the state variable
            x = state(:, 1);
            y = state(:, 2);
            z = state(:, 3);
            v_x = state(:,4);
            v_y = state(:,5);
            v_z = state(:,6);
            velocityMagnitude = sqrt(v_x.^2 + v_y.^2 + v_z.^2);

            for i = 1: length(t)
                r_numerical(i) = sqrt((x(i))^2 + (y(i))^2 + (z(i))^2);
            end
            rp = min(r_numerical);
            ra = max(r_numerical);

            h = sqrt(2*mu) * sqrt((ra*rp)/(ra+rp));
            r_analytical = round(((norm(h))^2/mu)); % Analytical radius [km] (2.62, pg. 76 | Curtis)
        
            for i = 1: length(t)
                r_analyticalArray(i) = r_analytical;
            end

        % Conic Equation Parameters
            a = (ra + rp)/2;
            e = -((rp/a) - 1);                      % Eccentricity [unitless](2.73, pg. 82 | Curtis)
            p = h^2/mu;                             % Semiparameter [kg^2*km] (2.53, pg. 74 | Curtis)
            a = (rp + ra)/2;                        % Semi-major axis [km] (2.71, pg. 81 | Curtis)
            V_c = sqrt(mu / r_analytical);
            T_c = (2*pi/sqrt(mu)) * a^(3/2);        % Orbital Period [seconds] (2.83, pg. 84 | Curtis)
            epsilon = -mu/(2*a);                    % Specific Energy [km^2/s^2] (2.80, pg. 83) | Curtis)
            h = sqrt(2*mu) * sqrt((ra*rp)/(ra+rp)); % Angular momentum [kg*km^2/s] (6.2, pg. 290 | Curtis)
            n = (2 * pi)/ T_c;                      % Mean motion [rad/s] (3.9, pg. 144 | Curtis)
            v_p = h/rp;                             % Velocity at perigee [km/s] (2.31, pg. 69 | Curtis)
            v_a = h/ra;                             % Velocity at apogee [km/s] (2.31, pg. 69 | Curtis)

%             fprintf('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n');
%             fprintf('                  Problem 3 \n');
%             fprintf('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n\n');
%             
%             fprintf('Part A\n');
%             fprintf('-----------------------------\n');
%             fprintf('Eccentricity:   e = %.5e  \n', e);
%             fprintf('-----------------------------\n');
%             
%             fprintf('Part B\n');
%             fprintf('-----------------------------\n');
%             fprintf('Semiparameter:  p = %.5e kg*m^2*km\n', p);
%             fprintf('-----------------------------\n');
%             
%             fprintf('Part C\n');
%             fprintf('-----------------------------\n');
%             fprintf('Semimajor Axis: a = %.5e   \n', a);
%             fprintf('-----------------------------\n');
%             
%             fprintf('Part D\n');
%             fprintf('-----------------------------\n');
%             fprintf('Periapsis range:       rp = %.5e km  \n', rp);
%             fprintf('-----------------------------\n');
%             
%             fprintf('Part E\n');
%             fprintf('-----------------------------\n');
%             fprintf('Apoapsis range:         ra = %.5e km  \n', ra);
%             fprintf('-----------------------------\n');
%             
%             fprintf('Part F\n');
%             fprintf('-----------------------------\n');
%             fprintf('Velocity at Periapsis: V_p = %.5e km/s  \n', v_p);
%             fprintf('-----------------------------\n');
%             
%             fprintf('Part G\n');
%             fprintf('-----------------------------\n');
%             fprintf('Velocity at Apoapsis: V_a = %.5e km/s  \n', v_a);
%             fprintf('-----------------------------\n');
%             
%             fprintf('Part H\n');
%             fprintf('-----------------------------\n');
%             fprintf('Orbiter period: T_c = %.5e  seconds or %.5e minutes \n', T_c, T_c/60);
%             fprintf('-----------------------------\n');    
% 
%             fprintf('Part I\n');
%             fprintf('-----------------------------\n');
%             fprintf('Specific Mechanical Energy: epsilon = %.5e km^2/s^2  \n', epsilon);
%             fprintf('-----------------------------\n');
% 
%             fprintf('Part J\n');
%             fprintf('-----------------------------\n');
%             fprintf('Angular Momentum: h = %.5e kg*km^2/s  \n', h);
%             fprintf('-----------------------------\n');
%             
%             fprintf('Part K\n');
%             fprintf('-----------------------------\n');
%             fprintf('Mean Motion: n = %.5e  rad/s\n', n);
%             fprintf('-----------------------------\n');
    end
  
    %% 2.3 TwoBody Equation of Motion for ODEs
        % Function defining the differential equations
        function dydt = twoBodyEOMHelio(~, state)
            % Extract state variables from state vector
            % mu needs to be changed based on type of Orbit, may want to
            % make a new function for different reference frames
            mu = 132712440018;
            x = state(1);
            y = state(2);
            z = state(3);
            vx = state(4);
            vy = state(5);
            vz = state(6);
        
            % Calculate acceleration components
            r = norm([x, y, z]);
            ax = -mu * x / r^3;
            ay = -mu * y / r^3;
            az = -mu * z / r^3;
        
            % Derivatives of state variables
            dydt = [vx; vy; vz; ax; ay; az];
        end
    % Function defining the differential equations
        function dydt = twoBodyEOMEarth(~, state)
            % Extract state variables from state vector
            % mu needs to be changed based on type of Orbit, may want to
            % make a new function for different reference frames
            mu = 398600;
            x = state(1);
            y = state(2);
            z = state(3);
            vx = state(4);
            vy = state(5);
            vz = state(6);
        
            % Calculate acceleration components
            r = norm([x, y, z]);
            ax = -mu * x / r^3;
            ay = -mu * y / r^3;
            az = -mu * z / r^3;
        
            % Derivatives of state variables
            dydt = [vx; vy; vz; ax; ay; az];
        end
    %% 2.4 Calculates the transfer time based on true anomaly input
    function [t, E, ME] = transferTimeFromDeltaTrueAnomaly(csvFileName, tA)
            state = csvread(csvFileName);
            mu = 132712440018;
            % Extract the position vectors from the state variable
            t = state(:, 1);
            x = state(:, 2);
            y = state(:, 3);
            z = state(:, 4);
            v_x = state(:,5);
            v_y = state(:,6);
            v_z = state(:,7);
            velocityMagnitude = sqrt(v_x.^2 + v_y.^2 + v_z.^2);
           

            for i = 1: length(t)
                r_numerical(i) = sqrt((x(i))^2 + (y(i))^2 + (z(i))^2);
            end

            rp = min(r_numerical);
            ra = max(r_numerical);

            h = sqrt(2*mu) * sqrt((ra*rp)/(ra+rp));
            
            r_analytical = round(((norm(h))^2/mu)); % Analytical radius [km] (2.62, pg. 76 | Curtis)

            a = (ra + rp)/2;
            e = (ra - rp)/(ra + rp);
            e = -((rp/a) - 1);                      % Eccentricity [unitless](2.73, pg. 82 | Curtis)
            p = h^2/mu;                             % Semiparameter [kg^2*km] (2.53, pg. 74 | Curtis)
            a = (rp + ra)/2;                        % Semi-major axis [km] (2.71, pg. 81 | Curtis)
            V_c = sqrt(mu / r_analytical);
            T = (2*pi/sqrt(mu)) * a^(3/2);        % Orbital Period [seconds] (2.83, pg. 84 | Curtis)
            epsilon = -mu/(2*a);                    % Specific Energy [km^2/s^2] (2.80, pg. 83) | Curtis)
            h = sqrt(2*mu) * sqrt((ra*rp)/(ra+rp)); % Angular momentum [kg*km^2/s] (6.2, pg. 290 | Curtis)
            n = (2 * pi)/ T;                      % Mean motion [rad/s] (3.9, pg. 144 | Curtis)
            v_p = h/rp;                             % Velocity at perigee [km/s] (2.31, pg. 69 | Curtis)
            v_a = h/ra;                             % Velocity at apogee [km/s] (2.31, pg. 69 | Curtis)
            h = sqrt(mu * rp * (1 + e));
            E = 2 * atan(sqrt((1-e)/(1+e)) * tan(tA/2));
            ME = E - e*sin(E);
            t = (ME/(2*pi)) * T; % Transfer Time (3.8 | Curtis)
       

%             fprintf('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n');
%             fprintf('                  Problem 2 \n');
%             fprintf('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n\n');
%             
%             fprintf('Part A\n');
%             fprintf('-----------------------------\n');
%             fprintf('Eccentricity:   e = %.5e  \n', e);
%             fprintf('-----------------------------\n');
%             
%             fprintf('Part B\n');
%             fprintf('-----------------------------\n');
%             fprintf('Periapsis Radius:  rp = %.5e km \n', rp);
%             fprintf('-----------------------------\n');
%             
%             fprintf('Part C\n');
%             fprintf('-----------------------------\n');
%             fprintf('Angular Momentum: h = %.5e kg*km^2/s  \n', h);
%             fprintf('-----------------------------\n');
%             
%             fprintf('Part D\n');
%             fprintf('-----------------------------\n');
%             fprintf('Eccentric Anomaly:       E = %.5e deg  \n', E * 180/pi);
%             fprintf('-----------------------------\n');
%             
%             fprintf('Part E\n');
%             fprintf('-----------------------------\n');
%             fprintf('Semi-major axis:         a = %.5e km  \n', a);
%             fprintf('-----------------------------\n');
%             
%             fprintf('Part F\n');
%             fprintf('-----------------------------\n');
%             fprintf('Orbit Period: T = %.5e days  \n', T/86400);
%             fprintf('-----------------------------\n');
%             
%             fprintf('Part G\n');
%             fprintf('-----------------------------\n');
%             fprintf('Mean Anomaly: M = %.5e deg  \n', ME * 180/pi);
%             fprintf('-----------------------------\n');
%             
%             fprintf('Part H\n');
%             fprintf('-----------------------------\n');
%             fprintf('Transfer Time: t = %.5e  seconds or %.5e minutes \n', t, t/60);
%             fprintf('-----------------------------\n');    

    end
    %% 2.4.1 Plot Radius vs Time
    function [t, a, e] = radiusTimePlot(csvFileName)
            state = csvread(csvFileName);
            mu = 398600;
            % Extract the position vectors from the state variable
            t = state(:, 1);
            x = state(:, 2);
            y = state(:, 3);
            z = state(:, 4);
            v_x = state(:,5);
            v_y = state(:,6);
            v_z = state(:,7);
            for i = 1: length(t)
                r_numerical(i) = sqrt((x(i))^2 + (y(i))^2 + (z(i))^2);
            end
            rp = min(r_numerical);
            ra = max(r_numerical);

            plot(t, r_numerical)
            xlabel('Time (in seconds)', 'Interpreter', 'latex')
            ylabel('Radius (in km)', 'Interpreter', 'latex')
            title(sprintf('Time vs Radius'), 'Interpreter', 'latex')
            grid on

            a = (ra + rp)/2;
            e = (ra - rp)/(ra + rp);
    end 
    %% 2.5 Calculates transfer angle based on time input
    function tA = transferAngleFromDeltaTime(csvFileName, tGiven)
         state = csvread(csvFileName);
        mu = 398600;
        % Extract the position vectors from the state variable
        t = state(:, 1);
        x = state(:, 2);
        y = state(:, 3);
        z = state(:, 4);
        v_x = state(:,5);
        v_y = state(:,6);
        v_z = state(:,7);
        velocityMagnitude = sqrt(v_x.^2 + v_y.^2 + v_z.^2);
        

        for i = 1: length(t)
            r_numerical(i) = sqrt((x(i))^2 + (y(i))^2 + (z(i))^2);
        end

        rp = min(r_numerical);
        ra = max(r_numerical);

        h = sqrt(2*mu) * sqrt((ra*rp)/(ra+rp));
        
        r_analytical = round(((norm(h))^2/mu)); % Analytical radius [km] (2.62, pg. 76 | Curtis)

        a = (ra + rp)/2;
        e = (ra - rp)/(ra + rp);
        e = -((rp/a) - 1);                      % Eccentricity [unitless](2.73, pg. 82 | Curtis)
        p = h^2/mu;                             % Semiparameter [kg^2*km] (2.53, pg. 74 | Curtis)
        a = (rp + ra)/2;                        % Semi-major axis [km] (2.71, pg. 81 | Curtis)
        V_c = sqrt(mu / r_analytical);
        T = (2*pi/sqrt(mu)) * a^(3/2);        % Orbital Period [seconds] (2.83, pg. 84 | Curtis)
        T = 2*pi/mu^2 * (h/(sqrt(1-e^2)))^3
        epsilon = -mu/(2*a);                    % Specific Energy [km^2/s^2] (2.80, pg. 83) | Curtis)
        h = sqrt(2*mu) * sqrt((ra*rp)/(ra+rp)); % Angular momentum [kg*km^2/s] (6.2, pg. 290 | Curtis)
        n = (2 * pi)/ T;                      % Mean motion [rad/s] (3.9, pg. 144 | Curtis)
        v_p = h/rp;                             % Velocity at perigee [km/s] (2.31, pg. 69 | Curtis)
        v_a = h/ra;                             % Velocity at apogee [km/s] (2.31, pg. 69 | Curtis)
        h = sqrt(mu * rp * (1 + e));

        ME = (2*pi*tGiven)/T;
        
        % Initial guess for Eccentric Anomaly (E) using ME
        if ME * 180 / pi < 180
            E = ME + e/2;
        else
            E = ME - e/2;
        end
        
        % Newton-Raphson Method
        iteration = 0;
        convergenceThreshold = 1e-8; % Adjust this threshold as needed
        maxIterations = 100;
        
        while iteration < maxIterations
            f = E - e * sin(E) - ME;
            fprime = 1 - e * cos(E);
            
            % Update the value of E based on the Newton-Raphson method
            E = E - f / fprime;
            
            % Check for convergence
            if abs(f) < convergenceThreshold
                disp(['Converged after ', num2str(iteration), ' iterations']);
                break;
            end
            
            iteration = iteration + 1;
        end
        
        % Calculate the true anomaly (tA) based on the eccentric anomaly (E)
        tA = 2 * atan(sqrt((1 + e) / (1 - e)) * tan(E / 2));
        
        % Ensure tA is in the range [0, 2*pi)
        if tA < 0
            tA = tA + 2 * pi;
        end

        fprintf('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n');
        fprintf('                  Problem 3 \n');
        fprintf('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n\n');
        
            fprintf('Part A\n');
            fprintf('-----------------------------\n');
            fprintf('Eccentricity:   e = %.5e  \n', e);
            fprintf('-----------------------------\n');
            
            fprintf('Part B\n');
            fprintf('-----------------------------\n');
            fprintf('Periapsis Radius:  rp = %.5e km \n', rp);
            fprintf('-----------------------------\n');
            
            fprintf('Part C\n');
            fprintf('-----------------------------\n');
            fprintf('Angular Momentum: h = %.5e kg*km^2/s  \n', h);
            fprintf('-----------------------------\n');
            
            
            fprintf('Part D\n');
            fprintf('-----------------------------\n');
            fprintf('Semi-major axis:         a = %.5e km  \n', a);
            fprintf('-----------------------------\n');
            
            fprintf('Part E\n');
            fprintf('-----------------------------\n');
            fprintf('Orbit Period: T = %.5e days  \n', T/86400);
            fprintf('-----------------------------\n');

            fprintf('Part F \n');
            fprintf('-----------------------------\n');
            fprintf('Mean Anomaly: M = %.5e deg \n', ME * 180/pi);
            fprintf('-----------------------------\n');
    
            fprintf('Part G\n');
            fprintf('-----------------------------\n');
            fprintf('Eccentric Anomaly:       E = %.5e deg  \n', E * 180/pi);
            fprintf('-----------------------------\n');
            
            fprintf('Part H\n');
            fprintf('-----------------------------\n');
            fprintf('Transfer Angle: theta = %.5e deg \n', tA);
            fprintf('-----------------------------\n');    
    end
    %% 2.6 Calculates Orbital Parameters given position and velocity vectors
    function [a, h_mag, e_mag, i, RAAN, AOP, trueAnomaly] = rvToOE(mu, r_vec,v_vec)
        % Referenced Algorithm 4.2 of Curtis (pg.191)
        r_mag = norm(r_vec);
        v_mag = norm(v_vec);
        v_radial = dot(r_vec, v_vec)/(r_mag);
        h_vec = cross(r_vec,v_vec);
        h_mag = norm(h_vec); 
        i = acos(h_vec(3)/h_mag);

 
        
        K_unit = [0, 0, 1];
        N_vec = cross(K_unit, h_vec);
        N_mag = norm(N_vec);
        if N_vec(2) >= 0
            RAAN = acos(N_vec(1)/N_mag);
        else % N_vec(2) <= 0
            RAAN = 2*pi - acos(N_vec(1)/N_mag);
        end

        e_vec = 1/mu * (((v_mag^2 - (mu/r_mag)) * r_vec) - r_mag * v_radial * v_vec); %(4.10, pg. 192)
        e_mag = norm(e_vec);
        
        if e_vec(3) >= 0
            AOP = acos((dot(N_vec,e_vec)/(N_mag * e_mag))); % (4.11, pg. 192)
        else %e_vector(3) <= 0
            AOP = 2 * pi - acos((dot(N_vec,e_vec)/(N_mag * e_mag))); % (4.11, pg. 192)
        end
        
        if v_radial >= 0
            trueAnomaly = acos(dot(e_vec, r_vec)/(e_mag*r_mag)); % (4.13, pg. 193)
        else % v_radial < 0
            trueAnomaly = 2* pi - acos(dot(e_vec, r_vec)/(e_mag*r_mag)); % (4.13, pg. 193)
        end
        rp = h_mag^2/mu * (1/(1+e_mag*cos(0)));
        ra = h_mag^2/mu * (1/(1+e_mag*cos(pi)));
        a = 1/2 * (rp + ra);
        r_vec_per = ((h_mag^2/mu)/(1+ e_mag*cos(trueAnomaly))) * [cos(trueAnomaly), sin(trueAnomaly), 0]';
        v_vec_per = (mu/h_mag) * [-sin(trueAnomaly), e_mag + cos(trueAnomaly), 0]';
    end
    %% 2.7 Equatorial to Perifocal Frame via 3-2-1 Matrix (xyz -> pqr)
    function [r_vec_per, v_vec_per] = equatorialToPerifocal (r_vec_eq, v_vec_eq,angle_vec_eq)
        RAAN = angle_vec_eq(1);
        AOP = angle_vec_eq(2);
        TA = angle_vec_eq(3);
        i = angle_vec_eq(4);

        R1 = [cos(RAAN), sin(RAAN), 0;
            -sin(RAAN), cos(RAAN), 0;
            0, 0, 1];

        R2 = [1, 0, 0;
            0, cos(i), sin(i); 
            0, -sin(i), cos(i)];

        R3 = [cos(AOP), sin(AOP), 0; 
            -sin(AOP), cos(AOP) 0; 
            0, 0, 1];

        R_IP = R3 * R2 * R1;
        R_PI = R_IP';

        r_vec_per = R_IP * r_vec_eq';
        v_vec_per = R_IP * v_vec_eq';

        r_I = R_PI * r_vec_per;
        v_I = R_PI * v_vec_per;
    end
    %% 2.8 J2 Perturbations
    function [RAAN_dot, AOP_dot] = J2Perturbations(mu, a, J2, radiusPlanet, i, e)
%         if i >=0 && i < 90
        RAAN_dot = -1.5 * ((sqrt(mu/a^7)) * ((J2 * radiusPlanet^2)/(1-e^2)^2)) * cos(i);
%         end
        AOP_dot = -1.5 * ((sqrt(mu/a^7)) * ((J2 * radiusPlanet^2)/(1-e^2)^2)) * (5/2 * (sin(i))^2 - 2);
    end
    %% 3.0 Interplanetary Transfer Functions (from ARO 3090)
    function [DV_dep, DV_arr, transferTime] = interplantarySolver(departurePlanet, targetPlanet, Isp_dep, Isp_arr, z_parking_per_dep, z_parking_apo_dep, z_parking_per_arr,z_parking_apo_arr)
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   EDITABLE   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  DO NOT EDIT  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        departurePlanet = Orbital.getPlanetProps(departurePlanet);  %Retrieves the needed fields for the departure planet    
        targetPlanet = Orbital.getPlanetProps(targetPlanet);        %Retrieves the needed fields for the arrival planet
    
        muSun = 132712440018;                               %[km^3/s^2] gravitational parameter of the sun
        nDepature = 2 * pi / (departurePlanet.OSP * 86400); %[rad/s] for the departure planets
        arrivalplanetTheta = 0;                             %[degs] initial position of arrival planet
        depaturePlanetTheta = 0;                            %[degs] initial position of departure planet
    
        g0 = 9.81E-3;                                       %[km/s^2] sea level standard acceleration of gravity 
    
    %% 2.0 Calculations
            %% 2.1 Arrival Phase and Departure Phase (Populating arrays) 
                t = (2 * pi / sqrt(muSun)) * ((departurePlanet.semiMajor + targetPlanet.semiMajor)/2)^(3/2);
                t12 = (pi/sqrt(muSun)) * ((departurePlanet.semiMajor + targetPlanet.semiMajor)/2)^(3/2); % Same result as above

                vHelioPlanetArrival = sqrt(muSun/targetPlanet.semiMajor);
                vHelioPlanetDeparture = sqrt(muSun/departurePlanet.semiMajor);
                n = (2 * pi/ targetPlanet.OSP) / 86400;                                                     %[radians/sec] | Mean Motions of planets
                vD = sqrt(2 * muSun) * sqrt(targetPlanet.semiMajor / ...
                    (departurePlanet.semiMajor * (departurePlanet.semiMajor + targetPlanet.semiMajor)));    %[km/s] | velocity of S/C departure 
                vA = (sqrt(2 * muSun) * sqrt((targetPlanet.semiMajor * departurePlanet.semiMajor) /...
                    (targetPlanet.semiMajor + departurePlanet.semiMajor))) / targetPlanet.semiMajor;        %[km/s] | velocity of S/C arrival
                
                phiRadDepart0 = pi * (1 - ((departurePlanet.semiMajor + targetPlanet.semiMajor) /...        %[radians]
                    (2 * targetPlanet.semiMajor))^(3/2)); % ARO 3090 Derivation
                phiRadDepart0  = pi - n*t12; % [radians] Equation 8.12 from Curtis (pg. 390)
                phiDegDepart0 = phiRadDepart0 * (180/pi);                                                   %[deg]
                phiArrivalRad = phiRadDepart0 + (n - nDepature) * t12;                                        %[radians] Equation 8.8, pg. 388 from Curtis
                if abs(phiArrivalRad) > 2*pi
                   phiArrivalRad = mod(phiArrivalRad,(sign(phiArrivalRad))*2*pi);                           %[radians] | Correction for radians over 2 * pi
                end
                phiArrivalDeg = phiArrivalRad * (180/pi);                                                   %[deg]
                planetTheta2 = arrivalplanetTheta + n * t;                                                  %[deg]
                earthTheta2 = depaturePlanetTheta + n * t;                                                  %[deg]
                semiMajor = (targetPlanet.semiMajor + departurePlanet.semiMajor)/2;                         %[km] | Semi-major of the Hohmann transfer
%                 targetPlane
%                 if targetPlanet == "Earth's Moon"
%                     transferTime = (pi / sqrt(398600)) * ((targetPlanet.semiMajor)/2)^(3/2); %[sec] | Transfer time of the Hohmann transfer
%                 else
                transferTime = (pi / sqrt(muSun)) * (((targetPlanet.semiMajor + z_parking_apo_arr)  + (departurePlanet.semiMajor + z_parking_per_dep))/2)^(3/2); %[sec] | Transfer time of the Hohmann transfer

      
                h = sqrt(2 * muSun) * sqrt((targetPlanet.semiMajor * departurePlanet.semiMajor)/(targetPlanet.semiMajor + departurePlanet.semiMajor)); %[km^2/s] | Angular momentum of the Hohmann Transfer
                e = (targetPlanet.semiMajor-departurePlanet.semiMajor) / (targetPlanet.semiMajor + departurePlanet.semiMajor);                         %[] | Eccentricity of the Hohmann Transfer
                tSyn = 2*pi/ (nDepature - n);                   
                vDWRTDeparture = abs(vD - sqrt(muSun / departurePlanet.semiMajor));                                                                    %[km/s] | Velocity of S/C departure WRT to the depature planet
                vAWRTTarget = abs(vA - sqrt(muSun / targetPlanet.semiMajor));                                                                          %[km/s] | Velocity of S/C arrival WRT to the arrival planet
            %% 2.2 Departure Parking Orbit
                departureParkingOrbitPer = z_parking_per_dep + departurePlanet.R;   %[km] | Radius of departure periapsis parking orbit to S/C                                                                                                   
                depatureParkingOrbitApo = z_parking_apo_dep + departurePlanet.R;    %[km] | Radius of arrival apopapsis parking orbit to S/C
                departureParkingOrbitSemiMajor = (z_parking_apo_dep + departurePlanet.R + z_parking_per_dep + departurePlanet.R) / 2;   %[km] | Semi-major of the parking orbit wrt Departure planet
                departureParkingOrbitH = sqrt(2 * departurePlanet.mu) * sqrt((departureParkingOrbitPer * depatureParkingOrbitApo)/(departureParkingOrbitPer + depatureParkingOrbitApo));    %[km^2/s] | Angular momomentum of the departure parking orbit
                V_park_dep_per = sqrt(departurePlanet.mu/departureParkingOrbitPer); %[km/s] | Velocity of parking orbit of the periapsis
                V_park_dep_apo = sqrt(departurePlanet.mu/depatureParkingOrbitApo);  %[km/s] | Velocity of parking orbit of the apoapsis 
                e_park_dep = 0;                                                     %[] | Eccentricity of the parking orbit
                %% 2.2.1 Departure Hyperbolic Trajectory
                    V_inf_dep = vDWRTDeparture;                                                         %[km/s] | V infinity (delta V or excessive velocity from departure planet)
                    V_hyp_dep = sqrt(V_inf_dep^2 + 2 * (departurePlanet.mu/departureParkingOrbitPer));  %[km/s] | Velocity of the hyperbolic transfer orbit 
                    DV_dep = abs(V_park_dep_apo-V_hyp_dep);                                             %[km/s] | Difference between parking orbit and hyperbolic velocity 
                    e_dep = ((V_hyp_dep^2 * departureParkingOrbitPer) / departurePlanet.mu) - 1;        %[] | Eccentricity of the departure hyperbolic trajectory 
                    Beta_dep_deg = acosd(1 / e_dep);                                                    %[deg] | Direction of periapsis 
                    Beta_dep_rad = Beta_dep_deg * (pi/180);                                             %[rad] | Direction of periapsis
                    delta_dep_deg = 180 - 2 * Beta_dep_deg;                                             %[deg] | Turn angle 
                    delta_dep_rad = delta_dep_deg * (pi/180);                                           %[rad] | Turn angle
                    aimingRadiusDep = departureParkingOrbitPer * sqrt(1 + (2 * departurePlanet.mu / (departureParkingOrbitPer * V_inf_dep^2))); %[km] | Aiming radius
                    percentageOfMassDep = (1 - exp(1) ^ (-DV_dep/(Isp_dep * g0))) * 100;                                                        %[%] | Percentage of fuel remaining after maneuver
                    h_hyp_dep = departurePlanet.mu/V_inf_dep * (sqrt(e_dep^2-1));                                                               %[km^2/s] | Angular momentum of the departure hypberbolic trajectory 
            %% 2.3 Arrival Parking Orbit
                arrivalParkingOrbitPer = z_parking_per_arr + targetPlanet.R;                            %[km] | Periapsis of the parking orbit of the arrival planet
                arrivalParkingOrbitApo = z_parking_apo_arr + targetPlanet.R;                            %[km] | Apoapsis of the parking orbit of the arrival planet
                arrivalParkingOrbitSemiMajor = (z_parking_apo_arr + targetPlanet.R + z_parking_per_arr + targetPlanet.R) / 2; %[km] | Semi-major axis of the arrival planet
                arrivalParkingOrbitH = sqrt(2 * targetPlanet.mu) * sqrt((arrivalParkingOrbitPer * arrivalParkingOrbitApo)/(arrivalParkingOrbitPer + arrivalParkingOrbitApo)); %[km^2/s] | Angular momentum of arrival planet's orbit
                V_park_arr_per = (arrivalParkingOrbitH/arrivalParkingOrbitPer);                         %[km/s] | Velocity of the arrival planet's orbit (Periapsis)
                V_park_arr_apo = (arrivalParkingOrbitH/arrivalParkingOrbitApo);                         %[km/s] | Velocity of the arrival planet's orbit (Apoapsis)       
                e_arr_cap = (arrivalParkingOrbitApo/arrivalParkingOrbitSemiMajor) - 1;                      %[] | Eccentricity of arrival planet
                %% 2.3.1 Arrival Hyperbolic Trajectory
                    V_inf_arr = vAWRTTarget;                                                            %[km/s] | Velocity excessive of the S/C respected to the arrival hyperbolic trajectory
                    V_hyp_arr = sqrt(V_inf_arr^2 + 2 * (targetPlanet.mu/arrivalParkingOrbitPer));       %[km/s] | Hyerbolic arrival orbit velocity
                    DV_arr = abs(V_hyp_arr - V_park_arr_per);                                           %[km/s] | Difference between the hyperbolic velocity and the perapsis of the arrival planet's parking orbit
                    e_arr = ((V_hyp_arr^2 * arrivalParkingOrbitPer) / targetPlanet.mu) - 1;             %[km/s] | Eccentricity of the arrival planet's orbit 
                    Beta_arr_deg = acosd(1 / e_arr);                                                    %[deg] | Direction of periapsis of the arrival planet's orbit
                    Beta_arr_rad = Beta_arr_deg * (pi/180);                                             %[rad] | Direction of periapsis of the arrival planet's orbit
                    delta_arr_deg = 180 - 2 * Beta_arr_deg;                                             %[deg] | Turn angle of the arrival planet's orbit
                    delta_arr_rad = delta_arr_deg * (pi/180);                                           %[rad] | Turn angle of the arrival planet's orbit
                    aimingRadiusArr = arrivalParkingOrbitPer * sqrt(1 + (2 * targetPlanet.mu / (arrivalParkingOrbitPer * V_inf_arr^2))); %[km] | Aiming radius of the arrival planet's radius
                    percentageOfMassArr = (1 - exp(1) ^ (-DV_arr/(Isp_arr * g0))) * 100;                %[%] | Percentage of fuel remaining after maneuver (Arrival orbit)            
                    h_hyp_arr = targetPlanet.mu/V_inf_arr * (sqrt(e_arr^2-1));                          %[km^2/s] | Target planet orbit's angular momentum
            %% 2.4 Fly-by Option
                %% 2.4.1 Entrance
                    V_entry = sqrt(vHelioPlanetArrival^2 + V_inf_arr^2 + 2 * vHelioPlanetArrival * V_inf_arr * cosd(targetPlanet.phi_1));   %[km/s] | Entry velocity of the fly-by
                    phi_2_deg = targetPlanet.phi_1 + delta_arr_deg;                                                                         %[deg] | SOI Entry Angle                                                                         
                    phi_2_rad = phi_2_deg * (pi/180);                                                                                       %[rad] | SPO Entry Angle
                    if targetPlanet.OSP > departurePlanet.OSP
                        V_exit = sqrt(vHelioPlanetArrival^2 + V_inf_arr^2 - 2 * vHelioPlanetArrival * V_inf_arr * cosd(delta_arr_deg));                         %[km/s] | Exit velocity, if the arrival planet is an outer planet
                    else
                        V_exit = sqrt(vHelioPlanetArrival^2 + V_inf_arr^2 + 2 * vHelioPlanetArrival * V_inf_arr * cosd(delta_arr_deg - targetPlanet.phi_1));    %[km/s] | Exit velocity, if the arrival planet is an inner planet
                    end
            %% 2.4.5 Patched Conics Test
                Vc_dep = sqrt(departurePlanet.mu/(departurePlanet.R + z_parking_per_dep));
                deltaV_dep = Vc_dep * (sqrt(2+(V_inf_dep/Vc_dep)^2) - 1);
                Vc_cap = sqrt(targetPlanet.mu/(targetPlanet.R + z_parking_apo_arr));
                deltaV_cap = Vc_cap * (sqrt(2+(V_inf_arr/Vc_cap)^2) - 1);
                
                totalPatchedConicDV = deltaV_dep + deltaV_cap;
                     %% 2.4.5 Patched Conics Test
              % Departure burn to achieve hyperbolic excess velocity for departure
                V_park_orbit_dep = sqrt(departurePlanet.mu / departureParkingOrbitPer); % velocity in parking orbit at periapsis
                deltaV_departure = sqrt(V_inf_dep^2 + 2*departurePlanet.mu/departureParkingOrbitPer) - V_park_orbit_dep; % delta-v for departure
                
                % Arrival burn to capture into parking orbit at arrival planet
                V_park_orbit_arr = sqrt(targetPlanet.mu / arrivalParkingOrbitPer); % velocity in parking orbit at periapsis
                deltaV_arrival = sqrt(V_inf_arr^2 + 2*targetPlanet.mu/arrivalParkingOrbitPer) - V_park_orbit_arr; % delta-v for arrival
                
                % Output the delta-v's for departure and arrival
                fprintf('Delta-v for departure maneuver: %f km/s\n', deltaV_departure);
                fprintf('Delta-v for arrival maneuver: %f km/s\n', deltaV_arrival);
            %% 2.5 Displaying results
            
                fprintf('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
                fprintf('                  Hohmann Transfer from %s to %s \n', departurePlanet.name, targetPlanet.name)
                fprintf('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n\n')
                
                fprintf('%s''s Physical Parameters:\n', departurePlanet.name)
                fprintf('-----------------------------\n')
                fprintf('Mean Motion:                     n = %.5e  rad/s\n', nDepature)
                fprintf('Gravitational Parameter:        mu = %.0f  km^3/s^2\n', departurePlanet.mu)
                fprintf('Orbital Period:          T2(P1/Sun) = %.2f days = %.2f year(s)\n\n', departurePlanet.OSP, departurePlanet.OSP/departurePlanet.OSP)
            
                fprintf('%s''s Physical Parameters:\n', targetPlanet.name)
                fprintf('-----------------------------\n')
                fprintf('Mean Motion:                     n = %.5e  rad/s\n', n)
                fprintf('Gravitational Parameter:        mu = %.0f  km^3/s^2\n', targetPlanet.mu)
                fprintf('Orbital Period:          T2(P1/Sun) = %.2f days = %.2f year(s)\n\n', targetPlanet.OSP, targetPlanet.OSP/departurePlanet.OSP)
            
                fprintf(' \n')
                fprintf('                       Hohmann Transfer:\n')
                fprintf('--------------------------------------------------------------\n')
                fprintf('Phase Angle at Departure:                   phi_dep = %.2f deg = %.4f rad\n', phiDegDepart0, phiRadDepart0)
                fprintf('Phase Angle at Arrival:                     phi_arr = %.2f deg = %.4f rad\n', phiArrivalDeg, phiArrivalRad)
                fprintf('Transfer Time:                              t = %.1f days\n', transferTime/86400)
                fprintf('Semi-Major Axis:                            a = %.1f Mkm\n', semiMajor/1E6)
                fprintf('Eccentricity:                               e = %.3f \n',e)
                fprintf('Angular Momentum:                           h = %.1e  km^2/s\n', h)
                fprintf('Perhihelion:                                perH = %.5e  km\n', departurePlanet.semiMajor)
                fprintf('Aphelion:                                   apoH = %.5e  km\n', targetPlanet.semiMajor)
            
                fprintf(' \n')
                fprintf('                       Depature Phase:\n')
                fprintf('--------------------------------------------------------------\n')
                fprintf('Velocity of Earth wrt Sun:                  V_P1/S = %.4f  km/s\n', vHelioPlanetDeparture)
                fprintf('Velocity of  S/C wrt Earth:                 V_inf_dep = %.4f  km/s\n', vDWRTDeparture)
                fprintf('Velocity of  S/C  wrt Sun:                  V_SC/S = %.4f  km/s\n', vD)
    
                fprintf(' \n')
                fprintf('                       Departure Pab  rking Orbit:\n')
                fprintf('--------------------------------------------------------------\n')
                fprintf('Periapsis:                                  rp = %.1f  km, vp = %.3f km/s \n', departureParkingOrbitPer, V_park_dep_per)
                fprintf('Apoapsis                                    ra = %.1f  km, va = %.3f km/s \n', depatureParkingOrbitApo, V_park_dep_apo)
                fprintf('Eccentricity:                               e = %.4f \n', e_park_dep)
                fprintf('Semi-Major Axis:                            a = %.1f km \n', departureParkingOrbitSemiMajor)
                fprintf('Angular Momentum:                           h = %.1f km^2/s \n', departureParkingOrbitH) 
    
                fprintf(' \n')
                fprintf('                       Departure Hyperbolic Trajectory:\n')
                fprintf('--------------------------------------------------------------\n')
                fprintf('Delta-v at %.5e alt: DV_dep = |V_hyp_dep - V_park_dep| = |%.4f - %.4f| = %.4f km/s \n', z_parking_per_dep, V_hyp_dep, V_park_dep_per, DV_dep)
                fprintf('Location of periapsis                   Beta_dep = %.2f deg = %.4f rad \n', Beta_dep_deg, Beta_dep_rad)
                fprintf('Turn Angle:                             delta_dep = %.2f deg = %.4f rad \n', delta_dep_deg, delta_dep_rad)
                fprintf('Eccentricity:                           e = %.3f \n', e_dep)
                fprintf('Angular Momentum:                       h_dep = %.1f km^2/s \n', h_hyp_dep)
                fprintf('Aiming Radius:                          D = %.3f km \n', aimingRadiusDep)
                fprintf(' \n')
                fprintf('Percentage of mass used for burn:       %.2f Percent  (Isp = %.0f s) %\n', percentageOfMassDep, Isp_dep)
            
                fprintf(' \n')
                fprintf('                       Arrival Phase:\n')
                fprintf('--------------------------------------------------------------\n')
                fprintf('Velocity of Mars wrt Sun:                   V_P2/S = %.4f  km/s\n', vHelioPlanetArrival)
                fprintf('Velocity of  S/C wrt %s:                    V_inf_dep = %.4f  km/s\n', targetPlanet.name, vAWRTTarget)
                fprintf('Velocity of  S/C  wrt Sun:                  V_SC/S = %.4f  km/s\n', vA)
                
                fprintf(' \n')
                fprintf('                       Arrival Hyperbolic Trajectory:\n')
                fprintf('--------------------------------------------------------------\n')
                fprintf('Delta-v at %.5e alt: DV_dep = |V_park_arr - V_hyp_arr| = |%.5f - %.5f| = %.5f km/s \n', z_parking_per_arr, V_park_arr_per, V_hyp_arr, DV_arr)
                fprintf('Location of periapsis                   Beta_arr = %.2f deg = %.4f rad \n', Beta_arr_deg, Beta_arr_rad)
                fprintf('Turn Angle:                             delta_arr = %.2f deg = %4f rad \n', delta_arr_deg, delta_arr_rad)
                fprintf('Eccentricity:                           e = %.4f \n', e_arr)
                fprintf('Angular Momentum:                       h_arr = %.1f km^2/s \n', h_hyp_arr)
                fprintf('Aiming Radius:                          D = %.3f km \n', aimingRadiusArr)
    
                fprintf(' \n')
                fprintf('                       Arrival Parking Orbit: \n')
                fprintf('--------------------------------------------------------------\n')
                fprintf('Periapsis:                                  rp = %.1f  km, vp = %.3f km/s^2 \n', arrivalParkingOrbitPer, V_park_arr_per)
                fprintf('Apoapsis                                    ra = %.1f  km, va = %.3f km/s^2 \n', arrivalParkingOrbitApo, V_park_arr_apo)
                fprintf('Eccentricity:                               e = %.4f \n',e_arr_cap);
                fprintf('Semi-Major Axis:                            a = %.1f km \n', departureParkingOrbitSemiMajor)
                fprintf('Angular Momentum:                           h = %.1f km^2/s \n', arrivalParkingOrbitH) 
                fprintf(' \n')
                fprintf('Percentage of mass used for burn:       %.2f Percent  (Isp = %.0f s) %\n', percentageOfMassArr, Isp_arr)
    
                fprintf(' \n')
                fprintf('                       Fly-by Option:\n')
                fprintf('--------------------------------------------------------------\n')
                fprintf('SOI Entry Angle:                                phi_1 = %.2f deg = %.4f rad \n', targetPlanet.phi_1, targetPlanet.phi_1 * (pi/180))
                fprintf('Entry Heliocentric Speed:                       V_entry = %.4f km/s \n', V_entry)
    
                fprintf(' \n')
                fprintf('SOI Exit Angle:                                 phi_2 = %.2f deg = %.4f rad \n', phi_2_deg, phi_2_rad)
                fprintf('Exit Heliocentric Speed:                        V_exit = %.4f km/s \n', V_exit)
    
    end
        %% 3.1 Planet Properties Database
        function TTP = getPlanetProps(targetPlanet)
                sideRealDays = 365.26;                                                                                                         % [days] Days in a sidereal year
                T = [87.97, 224.7, 365.25, 27.322, (1.881 * sideRealDays), (11.86 * sideRealDays), (29.46 * sideRealDays), ...  
                    (84.01 * sideRealDays), (164.8 * sideRealDays), (247.9 * sideRealDays)];                                                   % [days] Orbit side real days of planets
                mu = [22032, 324859, 398600, 4905, 42828, 126686534, 37931187, 5793939, 6836529, 871];                                         % [km^3/s^2] gravitational parameter 
                aPlanets = [57.91E6, 108.2E6, 149.6E6, 384.4E3, 227.9E6, 778.6E6, 1.433E9, 2.872E9, 4.495E9, 5.906E9];                         % [km] Semi-Major Axis of orbit
                eccentricity = [0.2056, 0.0067, 0.0167, 0.0549, 0.0935, 0.0489, 0.0565, 0.0457, 0.0113, 0.2488];
                R = [2440, 6052, 6378, 1737, 3396, 71490, 60270, 25560, 24764, 1187];
                planetName = ["Mercury", "Venus", "Earth", "Earth's Moon", "Mars", "Jupiter", "Saturn", ...
                    "Uranus", "Neptune", "Pluto"];
                phi_1 = [0, 0, 0, 0, 180, 180, 180, 180, 180, 180];
                for i = 1:length(planetName)
                    TP.name(i) = planetName(i);                                                                                                %Creates a row of the names to planets
                    TP.semiMajor(i) = aPlanets(i);                                                                                             %Creates a row of all the semi-major axis to desired planets
                    TP.mu(i) = mu(i);                                                                                                          %Creates a row for all the gravitational parameters of the planets
                    TP.e(i) = eccentricity(i);                                                                                                 %Creates a row for all the eccentricities of the planets 
                    TP.OSP(i) = T(i);                                                                                                          %Orbit sidereal period (in days) 
                    TP.phi_1(i) = phi_1(i);                                                                                                    %Assigns the respective angles to the inner and outer planets
                    TP.R(i) = R(i);                                                                                                            %Creates a row for radius of each of the planets
    
                end
                switch targetPlanet
                    case "Mercury"
                        TTP.name = TP.name(1);
                        TTP.semiMajor = TP.semiMajor(1);
                        TTP.mu = TP.mu(1);
                        TTP.e = TP.e(1);
                        TTP.OSP = TP.OSP(1);
                        TTP.phi_1 = TP.phi_1(1);
                        TTP.R = TP.R(1);
                    case "Venus"
                        TTP.name = TP.name(2);
                        TTP.semiMajor = TP.semiMajor(2);
                        TTP.mu = TP.mu(2);
                        TTP.e = TP.e(2);
                        TTP.OSP = TP.OSP(2);
                        TTP.phi_1 = TP.phi_1(2);
                        TTP.R = TP.R(2);
                    case "Earth"
                        TTP.name = TP.name(3);
                        TTP.semiMajor = TP.semiMajor(3);
                        TTP.mu = TP.mu(3);
                        TTP.e = TP.e(3);
                        TTP.OSP = TP.OSP(3);
                        TTP.phi_1 = TP.phi_1(3);
                        TTP.R = TP.R(3);
                    case "Earth's Moon"
                        TTP.name = TP.name(4);
                        TTP.semiMajor = TP.semiMajor(3) + TP.semiMajor(4);
                        TTP.mu = TP.mu(4);
                        TTP.e = TP.e(4);
                        TTP.OSP = TP.OSP(4);
                        TTP.phi_1 = TP.phi_1(4);
                        TTP.R = TP.R(4);
                    case "Mars"
                        TTP.name = TP.name(5);
                        TTP.semiMajor = TP.semiMajor(5);
                        TTP.mu = TP.mu(5);
                        TTP.e = TP.e(5);
                        TTP.OSP = TP.OSP(5);
                        TTP.phi_1 = TP.phi_1(5);
                        TTP.R = TP.R(5);
                    case "Jupiter"
                        TTP.name = TP.name(6);
                        TTP.semiMajor = TP.semiMajor(6);
                        TTP.mu = TP.mu(6);
                        TTP.e = TP.e(6);
                        TTP.OSP = TP.OSP(6);
                        TTP.phi_1 = TP.phi_1(6);
                        TTP.R = TP.R(6);
                    case "Saturn"
                        TTP.name = TP.name(7);
                        TTP.semiMajor = TP.semiMajor(7);
                        TTP.mu = TP.mu(7);
                        TTP.e = TP.e(7);
                        TTP.OSP = TP.OSP(7);
                        TTP.phi_1 = TP.phi_1(7);
                        TTP.R = TP.R(7);
                    case "Uranus"
                        TTP.name = TP.name(8);
                        TTP.semiMajor = TP.semiMajor(8);
                        TTP.mu = TP.mu(8);
                        TTP.e = TP.e(8);
                        TTP.OSP = TP.OSP(8);
                        TTP.phi_1 = TP.phi_1(8);
                        TTP.R = TP.R(8);
                    case "Neptune"
                        TTP.name = TP.name(9);
                        TTP.semiMajor = TP.semiMajor(9);
                        TTP.mu = TP.mu(9);
                        TTP.e = TP.e(9);
                        TTP.OSP = TP.OSP(9);
                        TTP.phi_1 = TP.phi_1(9);
                        TTP.R = TP.R(9);
                    case "Pluto"
                        TTP.name = TP.name(10);
                        TTP.semiMajor = TP.semiMajor(10);
                        TTP.mu = TP.mu(10);
                        TTP.e = TP.e(10);
                        TTP.OSP = TP.OSP(10);
                        TTP.phi_1 = TP.phi_1(10);
                        TTP.R = TP.R(10);
                    otherwise
                        warning("Planet not found, please check spelling and ensure it's a major body is within solar system")
                end
              
        end
    
    %% 4.0 Lambert's Problem 
    function S = stumpS(Z)   
    % Stumpff Function for S(Z) - Equation 3.52 on page 168 on Curtis)
    if Z>0
        S = (sqrt(Z) - sin(sqrt(Z)))./(sqrt(Z)).^3;
    elseif Z<0
        S = (sinh(sqrt(-Z)) - sqrt(-Z))./(sqrt(-Z)).^3;
    else
        S = 1/6;
    end
    end
    
    function C = stumpC(Z)
        % Stumpff Function for C(Z) - Equation 3.53 on page 169 on Curtis)
        if Z>0
            C = (1 - cos(sqrt(Z)))./Z;
        elseif Z<0
            C = (cosh(sqrt(-Z)) - 1)./(-Z);
        else
            C = 1/2;
        end
    end
    
    function F = Fun(z,t,mu,r1,r2,A)
        % Function defined in 5.40 on page 242 of Curtis to solve for z using
        % Newton Raphson Method
        r1_mag = vecnorm(r1);
        r2_mag = vecnorm(r2);
        y = r1_mag + r2_mag + A*((z * Orbital.stumpS(z) -1)/sqrt(Orbital.stumpS(z)));
        F = (y/Orbital.stumpS(z))^(3/2) * Orbital.stumpS(z) + A * sqrt(y) - sqrt(mu) * t; % Eq. 5.39, pg. 242 of Curtis
        F = F';
    end
    %% Lamber Fit
    function [v1_vec, v2_vec, vinf_dep_vec, vinf_arr_vec] = lambertFit (r1_vec, v1_vec, r2_vec, v2_vec, TOF, Orbit)
        mu = 132712440018; %[km^3/s^2] Gravitational parameter of the sun 
        Options = optimoptions('fsolve','Display','off','TolFun',1E-12,'TolX',1E-12);
        TOF = TOF * 86400;
        Planet_1 = 'Earth';
        Planet_2 = 'Mars';
        
        
        VP1_dep = v1_vec';
        VP2_arr = v2_vec';
        
        r1_mag = norm(r1_vec);
        r2_mag = norm(r2_vec);
        
        r1Crossr2 = cross(r1_vec,r2_vec);
        
        %% 1.0 True Anomaly Calculation and A 
            if strcmp(Orbit, 'Prograde')
                if r1Crossr2(3) >= 0
                    deltaTheta = acos((dot(r1_vec,r2_vec))/(r1_mag * r2_mag));
                else %r2Crossr2(3) < 0
                    deltaTheta = 2* pi - acos((dot(r1_vec,r2_vec))/(r1_mag * r2_mag));
                end
            else % Retrograde
                 if r1Crossr2(3) < 0
                    deltaTheta = acos((dot(r1_vec,r2_vec))/(r1_mag * r2_mag));
                else %r2Crossr2(3) < 0
                    deltaTheta = 2* pi - acos((dot(r1_vec,r2_vec))/(r1_mag * r2_mag));
                end  
            end
            
            A = (sin(deltaTheta)) * sqrt((r1_mag * r2_mag)/(1-cos(deltaTheta)));
        %% 1.1 Newton's Raphson to find Z
            r1_vec=r1_vec';   
            r1_mag = vecnorm(r1_vec);
            r2_vec=r2_vec';   
            r2_mag = vecnorm(r2_vec);
            
            % True Anomaly Difference
            DTA_S = acos(dot(r1_vec,r2_vec)./(r1_mag.*r2_mag));  
            DTA_L = 2*pi-DTA_S;    
        
            A_S = sin(DTA_S).*sqrt(r1_mag.*r2_mag./(1-cos(DTA_S)));
            A_L = sin(DTA_L).*sqrt(r1_mag.*r2_mag./(1-cos(DTA_L)));
        
            z0_S =-100;
            while Orbital.Fun(z0_S,TOF,mu,r1_mag,r2_mag,A_S) < 0
                z0_S = z0_S + 0.01;
            end
        
            z0_L = -100;
            while Orbital.Fun(z0_L,TOF,mu,r1_mag,r2_mag,A_L) < 0
                z0_L = z0_L + 0.1;
            end
            
            z_S = fsolve(@(z_S)Orbital.Fun(z_S,TOF,mu,r1_mag,r2_mag,A_S),z0_S,Options); 
            z_L = fsolve(@(z_L)Orbital.Fun(z_L,TOF,mu,r1_mag,r2_mag,A_L),z0_L,Options);
        
            y_S = r1_mag + r2_mag + A_S.*(z_S.*Orbital.stumpS(z_S) - 1)./sqrt(Orbital.stumpC(z_S));
            y_L = r1_mag + r2_mag + A_L.*(z_L.*Orbital.stumpS(z_L) - 1)./sqrt(Orbital.stumpC(z_L));
        %% 1.2 Coefficients for Lambert's Problem 
            f_S = 1 - y_S./r1_mag;
            g_S = A_S.*sqrt(y_S/mu);
            gdot_S = 1 - y_S./r2_mag;
            v1_vec_S = (r2_vec - f_S.*r1_vec)./g_S;
            v2_vec_S = 1./g_S.*(gdot_S.*r2_vec - r1_vec);
        
            f_L = 1 - y_L./r1_mag;
            g_L = A_L.*sqrt(y_L/mu);
            gdot_L = 1 - y_L./r2_mag;
            v1_vec_L = (r2_vec - f_L.*r1_vec)./g_L;
            v2_vec_L = 1/g_L.*(gdot_L.*r2_vec - r1_vec);
        
            fprintf('The short path velocities are the following: \n')
            fprintf('v1_vec_S (%s): [%.5f,%.5f,%.5f] km/s\n ', Planet_1, v1_vec_S)
            fprintf('v2_vec_S (%s): [%.5f,%.5f,%.5f] km/s\n ', Planet_2, v2_vec_S)
        
        
            fprintf('The long path velocities are the following: \n')
            fprintf('v1_vec_L (%s): [%.5f,%.5f,%.5f] km/s\n ', Planet_1, v1_vec_L)
            fprintf('v2_vec_L (%s): [%.5f,%.5f,%.5f] km/s\n ', Planet_2, v2_vec_L)
        %% Short or long path
         % Departure
            vinf_dep_vec_S = v1_vec_S - VP1_dep;
            vinf_dep_S = vecnorm(vinf_dep_vec_S);
            C3_dep_S = vinf_dep_S^2;
            
            vinf_dep_vec_L = v1_vec_L - VP1_dep;
            vinf_dep_L = vecnorm(vinf_dep_vec_L);
            C3_dep_L = vinf_dep_L^2;
            
            % Arrival
            vinf_arr_vec_S = v2_vec_S-VP2_arr;
            vinf_arr_S = vecnorm(vinf_arr_vec_S);
            C3_arr_S = vinf_arr_S^2;
             
            vinf_arr_vec_L = v2_vec_L-VP2_arr;
            vinf_arr_L = vecnorm(vinf_arr_vec_L);
            C3_arr_L = vinf_arr_L^2;
        
        %     fprintf('The short path deltaV are the following: \n')
        %     fprintf('vinf_dep_vec_S (%s): [%.5f,%.5f,%.5f] km/s\n ', Planet_1, vinf_dep_vec_S)
        %     fprintf('vinf_dep_S (%s): [%.5f] km/s\n ', Planet_1, vinf_dep_S)
        % 
        % 
        %     fprintf('The short path deltaV are the following: \n')
        %     fprintf('vinf_dep_vec_S (%s): [%.5f,%.5f,%.5f] km/s\n ', Planet_2, vinf_dep_vec_L)
        %     fprintf('vinf_dep_S (%s): [%.5f] km/s\n ', Planet_2, vinf_dep_L)
        
            if isnan(C3_dep_S) && isnan(C3_dep_L)
                fprintf('\nTransfer physically unrealizable. ')
                fprintf('Select a different time of flight (TOF).\n')
                return
            end
            if C3_dep_S<C3_dep_L || isnan(C3_dep_L)
                orbit_choice = sprintf('              The SHORT-Way Orbit is the viable solution.\n');
                C3_dep = C3_dep_S;
                C3_arr = C3_arr_S;
                color_traj = [0,0.6,1];
                vinf_dep_vec = vinf_dep_vec_S;
                vinf_arr_vec = vinf_arr_vec_S;
                v1_vec = v1_vec_S;
                v2_vec = v2_vec_S;
            elseif C3_dep_S>=C3_dep_L || isnan(C3_dep_S)
                orbit_choice = sprintf('              The  LONG-Way Orbit is the viable solution.\n');
                C3_dep = C3_dep_L;
                C3_arr = C3_arr_L;
                color_traj = [.9,0,.9];
                vinf_dep_vec = vinf_dep_vec_L;
                vinf_arr_vec = vinf_arr_vec_L;
                v1_vec = v1_vec_L;
                v2_vec = v2_vec_L;
            end



    end
    %%
    function [v1_vec, v2_vec, C3_dep, C3_arr] = lamShortOrLong(v1_vec_S, v2_vec_S, v1_vec_L, v2_vec_L, VP1_dep, VP2_arr)
        %% 1.4 Solving for Delta V Required
           % Departure
            vinf_dep_vec_S = v1_vec_S - VP1_dep;
            vinf_dep_S = vecnorm(vinf_dep_vec_S);
            C3_dep_S = vinf_dep_S^2;
            
            vinf_dep_vec_L = v1_vec_L - VP1_dep;
            vinf_dep_L = vecnorm(vinf_dep_vec_L);
            C3_dep_L = vinf_dep_L^2;
            
            % Arrival
            vinf_arr_vec_S = v2_vec_S-VP2_arr;
            vinf_arr_S = vecnorm(vinf_arr_vec_S);
            C3_arr_S = vinf_arr_S^2;
             
            vinf_arr_vec_L = v2_vec_L-VP2_arr;
            vinf_arr_L = vecnorm(vinf_arr_vec_L);
            C3_arr_L = vinf_arr_L^2;
        
%             fprintf('The short path deltaV are the following: \n')
%             fprintf('vinf_dep_vec_S (%s): [%.5f,%.5f,%.5f] km/s\n ', Planet_1, vinf_dep_vec_S)
%             fprintf('vinf_dep_S (%s): [%.5f] km/s\n ', Planet_1, vinf_dep_S)
%         
%         
%             fprintf('The short path deltaV are the following: \n')
%             fprintf('vinf_dep_vec_S (%s): [%.5f,%.5f,%.5f] km/s\n ', Planet_2, vinf_dep_vec_L)
%             fprintf('vinf_dep_S (%s): [%.5f] km/s\n ', Planet_2, vinf_dep_L)
%         
            if isnan(C3_dep_S) && isnan(C3_dep_L)
                fprintf('\nTransfer physically unrealizable. ')
                fprintf('Select a different time of flight (TOF).\n')
                return
            end
            if C3_dep_S<C3_dep_L || isnan(C3_dep_L)
                orbit_choice = sprintf('              The SHORT-Way Orbit is the viable solution.\n');
                C3_dep = C3_dep_S;
                C3_arr = C3_arr_S;
                color_traj = [0,0.6,1];
                vinf_dep_vec = vinf_dep_vec_S;
                vinf_arr_vec = vinf_arr_vec_S;
                v1_vec = v1_vec_S;
                v2_vec = v2_vec_S;
            elseif C3_dep_S>=C3_dep_L || isnan(C3_dep_S)
                orbit_choice = sprintf('              The  LONG-Way Orbit is the viable solution.\n');
                C3_dep = C3_dep_L;
                C3_arr = C3_arr_L;
                color_traj = [.9,0,.9];
                vinf_dep_vec = vinf_dep_vec_L;
                vinf_arr_vec = vinf_arr_vec_L;
                v1_vec = v1_vec_L;
                v2_vec = v2_vec_L;
            end
    end
    %%
    %% 5.0 External Data 
        %% 5.1 Pulling Data from JPL Horizon from API Call
    function jplHorizonRequest(params, textFileName)
        %% JPLHORIZONREQUEST
        % This function sends a request to the JPL Horizons system and saves the response to a file.
        %
        % Inputs:
        %   params - A structure containing all the necessary parameters for the API request.
        %   textFileName - The name of the text file where the API response will be saved.
        %
        % Example usage:
        %   params = struct('COMMAND', '399', 'OBJ_DATA', 'NO', 'MAKE_EPHEM', 'YES', ...
        %                   'EPHEM_TYPE', 'VECTORS', 'CENTER', '500@0', ...
        %                   'START_TIME', '2022-08-25', 'STOP_TIME', '2022-08-26', ...
        %                   'STEP_SIZE', '1 d', 'QUANTITIES', '2');
        %   jplHorizonRequest(params, 'output.txt');
    
        baseUrl = 'https://ssd.jpl.nasa.gov/api/horizons.api?format=text';
        
        % Dynamically build the query string from the parameters structure
        queryParams = fieldnames(params);
        for i = 1:length(queryParams)
            baseUrl = [baseUrl, '&', queryParams{i}, '=', urlencode(params.(queryParams{i}))];
        end
        
        try
            % Send a GET request to the constructed URL
            api_response = webread(baseUrl);
            
            % Display the response
            disp(api_response);
            
            % Write the response to a text file
            fileID = fopen(textFileName, 'w');
            fprintf(fileID, '%s', api_response);
            fclose(fileID);
            
            disp(['API response has been saved to ' textFileName]);
        catch exception
            disp(['Error occurred: ' exception.message]);
        end
    end

    %% 5.1 Parse States and Dates from JPL Horizon Files 
    function [outputStates] = parseJPLHorizonStates(textFileName)
        % Read the text file
        fileID = fopen(textFileName, 'r');
        data = textscan(fileID, '%s', 'Delimiter', '\n');
        fclose(fileID);
        
        % Extract position and velocity data
        startIdx = find(strcmp(data{1}, '$$SOE')) + 1;
        endIdx = find(strcmp(data{1}, '$$EOE')) - 1;
        rowIndex = 1;
        
            for i = startIdx:4:endIdx
                % Assuming data is a cell array containing the input strings
                
                % Extract the position vector string
                date_vector = data{1, 1}{i, 1};
                position_vector = data{1, 1}{i + 1, 1};
                velocity_vector = data{1, 1}{i + 2, 1};

                % Split the input string based on spaces and '='
                parts_date = strsplit(date_vector, {' ', '='});
                parts_pos = strsplit(position_vector, {' ', '='});
                parts_vel = strsplit(velocity_vector, {' ', '='});
                
                % Find the indices of X, Y, and Z in the parts cell array
                julianDate = str2double(parts_date{1, 1});
                xIndex = find(strcmp(parts_pos, 'X'));
                yIndex = find(strcmp(parts_pos, 'Y'));
                zIndex = find(strcmp(parts_pos, 'Z'));

                velxIndex = find(strcmp(parts_vel, 'VX'));
                velyIndex = find(strcmp(parts_vel, 'VY'));
                velzIndex = find(strcmp(parts_vel, 'VZ'));
                
                % Extract values for X, Y, and Z and convert to double
                X = str2double(parts_pos{xIndex + 1});
                Y = str2double(parts_pos{yIndex + 1});
                Z = str2double(parts_pos{zIndex + 1});

                VX = str2double(parts_vel{velxIndex + 1});
                VY = str2double(parts_vel{velyIndex + 1});
                VZ = str2double(parts_vel{velzIndex + 1});
                
                % Store values in the positions matrix
                outputStates(rowIndex, 1) = julianDate;
                outputStates(rowIndex, 2) = X;
                outputStates(rowIndex, 3) = Y;
                outputStates(rowIndex, 4) = Z;
                outputStates(rowIndex, 5) = VX;
                outputStates(rowIndex, 6) = VY;
                outputStates(rowIndex, 7) = VZ;
                
                % Increment the row index
                rowIndex = rowIndex + 1;
            end
    end
%% Julian Date to Calendar year, month, and date
            function [years, months, days] = julianDateToYMD(julianDate)
                % Extracting year
                J = julianDate;
                 % Adjusting for half-day offset
                Z = floor(J);
                F = J - Z;
            
                A = Z;
                if Z >= 2299161
                    alpha = floor((Z - 1867216.25) / 36524.25);
                    A = Z + 1 + alpha - floor(alpha / 4);
                end
            
                B = A + 1524;
                C = floor((B - 122.1) / 365.25);
                D = floor(365.25 * C);
                E = floor((B - D) / 30.6001);
            
                % Extracting month and day
                day = B - D - floor(30.6001 * E) + F;
                if E < 14
                    month = E - 1;
                else
                    month = E - 13;
                end
                if month > 2
                    year = C - 4716;
                else
                    year = C - 4715;
                end
            
                % Output
                years = year;
                months = month;
                days = day;
            end
        %% 6.0 Optimization Related
         function [critical_x, critical_y] = gradientMethod(f)
            syms x y;
            % Calculate the gradient of the function
            grad_f = [diff(f, x); diff(f, y)];
            
            % Solve the gradient equations for critical points
            [critical_x, critical_y] = solve(grad_f == 0, [x y]);
            
            % Convert the solutions to numeric values
            critical_x = double(critical_x);
            critical_y = double(critical_y);
            
            % Display the critical points
            disp(['Critical point at x = ', num2str(critical_x), ', y = ', num2str(critical_y)]);
            
            % Calculate the Hessian matrix at the critical point
            H = [diff(f, x, 2), diff(f, x, y); diff(f, y, x), diff(f, y, 2)];
            
            % Evaluate the Hessian matrix at the critical point
            H_at_critical = double(subs(H, [x, y], [critical_x, critical_y]));
            
            % Calculate the determinant of the Hessian
            det_H = det(H_at_critical);
            
            % Calculate the magnitude of the Hessian
            magnitude_H = sqrt(trace(H_at_critical'*H_at_critical));
            
            % Display the determinant and magnitude of the Hessian
            disp(['Determinant of Hessian: ', num2str(det_H)]);
            disp(['Magnitude of Hessian: ', num2str(magnitude_H)]);
            
            % Determine if the critical point is a minimum or maximum
            eigenvalues = eig(H_at_critical);
            if all(eigenvalues > 0)
                disp('The critical point is a minimum.');
            elseif all(eigenvalues < 0)
                disp('The critical point is a maximum.');
            else
                disp('The critical point is a saddle point or inconclusive.');
            end
        
        end
        
        function surfPlot(f, critical_x, critical_y)
            % Plot the function
            syms x y;
            fplot = matlabFunction(f);
            [x_vals, y_vals] = meshgrid(linspace(-10, 10, 400), linspace(-10, 10, 400));
            z_vals = fplot(x_vals, y_vals);
            surf_handle = surf(x_vals, y_vals, z_vals); % Create the surface plot
            
            % Change the colormap to 'jet' or any other desired colormap
            colormap(surf_handle.Parent, 'jet'); % This will apply the 'jet' colormap
            
            hold on;
            
            % Plot the critical point
            cp = plot3(critical_x, critical_y, fplot(critical_x, critical_y), 'r*', 'MarkerSize', 10);
            
            title('Surface Plot of f(x, y)');
            xlabel('x-axis');
            ylabel('y-axis');
            zlabel('f(x, y)');

            % Label the critical point
            % text(critical_x, critical_y, fplot(critical_x, critical_y), ...
            %     sprintf('Critical Point (%.2f, %.2f)', critical_x, critical_y), ...
            %     'VerticalAlignment', 'bottom', 'HorizontalAlignment', 'left', ...
            %     'BackgroundColor', 'white', 'EdgeColor', 'black');
            % Create a legend for the critical point
            legend(cp, 'Critical Point');

            shading interp; % Interpolates colors across surfaces and edges
            light; % Adds lighting
            lighting gouraud; % Use Gouraud lighting, which can make the color variation more apparent
        end
        
        function steepestDescent2D(f)
            syms x y h;
            % Calculate the gradient of the function
            grad_f(x, y) = [diff(f, x); diff(f, y)];
            
            % Define the starting point
            x_k = 0;
            y_k = 5;
            % Set the convergence criterion (epsilon)
            eps = 1e-6;
            % Set the maximum number of iterations to prevent an infinite loop
            max_iters = 1000;
            
            % Initialize iteration counter and arrays to hold the path
            iter = 0;
            x_points = x_k;
            y_points = y_k;
            
            % Steepest descent method
            while true
                % Increment iteration counter
                iter = iter + 1;
                
                % Calculate the gradient at the current point
                grad_current = double(grad_f(x_k, y_k));
                
                % Find the optimal h by minimizing the function along the gradient direction
                g(h) = f(x_k - h*grad_current(1), y_k - h*grad_current(2));
                g_prime = diff(g, h);
                h_opt = solve(g_prime == 0, h);
                h_opt = double(h_opt);
                
                % Filter out complex solutions and negative h
                h_opt = h_opt(imag(h_opt) == 0 & h_opt > 0);
                % Choose the h that minimizes the function g(h)
                [~, idx] = min(subs(g(h), h, h_opt));
                h_star = h_opt(idx);
                
                % Update the current point
                x_k1 = x_k - h_star*grad_current(1);
                y_k1 = y_k - h_star*grad_current(2);
                
                % Store the path
                x_points(end+1) = x_k1;
                y_points(end+1) = y_k1;
                
                % Check for convergence
                if norm([x_k1 - x_k, y_k1 - y_k]) < eps
                    fprintf('Converged at iteration %d with x = %.4f, y = %.4f\n', iter, x_k1, y_k1);
                    break;
                end
                
                % Update the point for the next iteration
                x_k = x_k1;
                y_k = y_k1;
                
                % Check if maximum number of iterations reached
                if iter >= max_iters
                    fprintf('Maximum iterations reached.\n');
                    break;
                end
            end
            
            % Output the number of iterations
            fprintf('Number of iterations: %d\n', iter);
            
            % Create a grid of points for x and y
            [x_grid, y_grid] = meshgrid(-2:0.1:2, -2:0.1:5);
            % Evaluate the function on the grid
            z_grid = double(f(x_grid, y_grid));
            % Create a contour plot
            figure;
            contour(x_grid, y_grid, z_grid, 50);
            hold on;
            
            % Plot the path taken with iteration numbers
            % Plot the points
            plot(x_points, y_points, 'rx');
            % Connect the points with lines
            plot(x_points, y_points, 'r-'); % This line connects the points
            
            % Annotate the points with their iteration numbers
            for i = 1:length(x_points)
                text(x_points(i), y_points(i), [' ', num2str(i)], ...
                    'VerticalAlignment', 'bottom', 'HorizontalAlignment', 'right');
            end
            
            title('Contour Plot of the Function with Steepest Descent Path');
            xlabel('x');
            ylabel('y');
            hold off;
        end
        
        function steepestDescent3D(f, x_k, y_k)
            syms x y h;
            % Calculate the gradient of the function
            grad_f(x, y) = [diff(f, x); diff(f, y)];
            
            % Define the starting point
            % x_k = 0;
            % y_k = 5;
            % Set the convergence criterion (epsilon)
            eps = 1e-6;
            % Set the maximum number of iterations to prevent an infinite loop
            max_iters = 1000;
            
            % Initialize iteration counter and arrays to hold the path
            iter = 0;
            x_points = x_k;
            y_points = y_k;
            
            % Steepest descent method
            while true
                % Increment iteration counter
                iter = iter + 1;
                
                % Calculate the gradient at the current point
                grad_current = double(grad_f(x_k, y_k));
                
                % Find the optimal h by minimizing the function along the gradient direction
                g(h) = f(x_k - h*grad_current(1), y_k - h*grad_current(2));
                g_prime = diff(g, h);
                h_opt = solve(g_prime == 0, h);
                h_opt = double(h_opt);
                
                % Filter out complex solutions and negative h
                h_opt = h_opt(imag(h_opt) == 0 & h_opt > 0);
                % Choose the h that minimizes the function g(h)
                [~, idx] = min(subs(g(h), h, h_opt));
                h_star = h_opt(idx);
                
                % Update the current point
                x_k1 = x_k - h_star*grad_current(1);
                y_k1 = y_k - h_star*grad_current(2);
                
                % Store the path
                x_points(end+1) = x_k1;
                y_points(end+1) = y_k1;
                
                % Check for convergence
                if norm([x_k1 - x_k, y_k1 - y_k]) < eps
                    fprintf('Converged at iteration %d with x = %.4f, y = %.4f\n', iter, x_k1, y_k1);
                    break;
                end
                
                % Update the point for the next iteration
                x_k = x_k1;
                y_k = y_k1;
                
                % Check if maximum number of iterations reached
                if iter >= max_iters
                    fprintf('Maximum iterations reached.\n');
                    break;
                end
            end
            
            % Output the number of iterations
            fprintf('Number of iterations: %d\n', iter);
            
            % Plot the function and the path taken
            % Create a grid of points for x and y
            [x_grid, y_grid] = meshgrid(-2:0.1:2, -2:0.1:5);
            % Evaluate the function on the grid
            z_grid = double(f(x_grid, y_grid));
            % Create a 3D contour plot
            figure;
            contour3(x_grid, y_grid, z_grid, 50);
            hold on;
            
            % Plot the path taken with iteration numbers
            fplot = matlabFunction(f, 'vars', [x y]);
            z_path = fplot(x_points, y_points);
            
            % Plot the points on the 3D contour plot
            plot3(x_points, y_points, z_path, 'ro-', 'LineWidth', 2, 'MarkerSize', 4);
            
            % Annotate the points with their iteration numbers
            for i = 1:length(x_points)
                text(x_points(i), y_points(i), z_path(i), [' ', num2str(i)], ...
                    'HorizontalAlignment', 'left', 'FontSize', 8, 'Color', 'k');
            end
            
            title('3D Contour Plot of the Function with Steepest Descent Path');
            xlabel('x');
            ylabel('y');
            zlabel('Function value');
            hold off;
        end
    end
    
end
