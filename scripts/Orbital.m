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
                [t, state] = ode45(@(t, state) Orbital.twoBodyEOM(t, state), tspan, initialState, options);
                tolerance = "Tolerance";
                csvFileName = sprintf("%s (%d_%d).csv", tolerance, toleranceMin, toleranceMax); %Add ODE name (45 and 115)
                writematrix([t, state], csvFileName)
            else
                [t, state] = ode45(@(t, state) Orbital.twoBodyEOM(t, state), tspan, initialState);
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
            fprintf('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n');
            fprintf('                  Problem 3 \n');
            fprintf('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n\n');
            
            fprintf('Part A\n');
            fprintf('-----------------------------\n');
            fprintf('Eccentricity:   e = %.5e  \n', e);
            fprintf('-----------------------------\n');
            
            fprintf('Part B\n');
            fprintf('-----------------------------\n');
            fprintf('Semiparameter:  p = %.5e kg*m^2*km\n', p);
            fprintf('-----------------------------\n');
            
            fprintf('Part C\n');
            fprintf('-----------------------------\n');
            fprintf('Semimajor Axis: a = %.5e   \n', a);
            fprintf('-----------------------------\n');
            
            fprintf('Part D\n');
            fprintf('-----------------------------\n');
            fprintf('Periapsis range:       rp = %.5e km  \n', rp);
            fprintf('-----------------------------\n');
            
            fprintf('Part E\n');
            fprintf('-----------------------------\n');
            fprintf('Apoapsis range:         ra = %.5e km  \n', ra);
            fprintf('-----------------------------\n');
            
            fprintf('Part F\n');
            fprintf('-----------------------------\n');
            fprintf('Velocity at Periapsis: V_p = %.5e km/s  \n', v_p);
            fprintf('-----------------------------\n');
            
            fprintf('Part G\n');
            fprintf('-----------------------------\n');
            fprintf('Velocity at Apoapsis: V_a = %.5e km/s  \n', v_a);
            fprintf('-----------------------------\n');
            
            fprintf('Part H\n');
            fprintf('-----------------------------\n');
            fprintf('Orbiter period: T_c = %.5e  seconds or %.5e minutes \n', T_c, T_c/60);
            fprintf('-----------------------------\n');    

            fprintf('Part I\n');
            fprintf('-----------------------------\n');
            fprintf('Specific Mechanical Energy: epsilon = %.5e km^2/s^2  \n', epsilon);
            fprintf('-----------------------------\n');

            fprintf('Part J\n');
            fprintf('-----------------------------\n');
            fprintf('Angular Momentum: h = %.5e kg*km^2/s  \n', h);
            fprintf('-----------------------------\n');
            
            fprintf('Part K\n');
            fprintf('-----------------------------\n');
            fprintf('Mean Motion: n = %.5e  rad/s\n', n);
            fprintf('-----------------------------\n');
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
        function plot2Body(mu, t, state, plotOrbit, analyticalVsNumericalPlot, orbitsEarthFileName, analyticalVsNumericalRFileName, correction, multipleOrbits, multipleVelocity, velocityVsTime, velocityVsTimeFileName, toleranceMin, toleranceMax)
            earthTextureImage = append(pwd, '\..\..\data\earth_texture.png');
            % Extract the position vectors from the state variable
            x = state(:, 1);
            y = state(:, 2);
            z = state(:, 3);
            v_x = state(:,4);
            v_y = state(:,5);
            v_z = state(:,6);
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
            fprintf('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n');
            fprintf('                  Problem 3 \n');
            fprintf('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n\n');
            
            fprintf('Part A\n');
            fprintf('-----------------------------\n');
            fprintf('Eccentricity:   e = %.5e  \n', e);
            fprintf('-----------------------------\n');
            
            fprintf('Part B\n');
            fprintf('-----------------------------\n');
            fprintf('Semiparameter:  p = %.5e kg*m^2*km\n', p);
            fprintf('-----------------------------\n');
            
            fprintf('Part C\n');
            fprintf('-----------------------------\n');
            fprintf('Semimajor Axis: a = %.5e   \n', a);
            fprintf('-----------------------------\n');
            
            fprintf('Part D\n');
            fprintf('-----------------------------\n');
            fprintf('Periapsis range:       rp = %.5e km  \n', rp);
            fprintf('-----------------------------\n');
            
            fprintf('Part E\n');
            fprintf('-----------------------------\n');
            fprintf('Apoapsis range:         ra = %.5e km  \n', ra);
            fprintf('-----------------------------\n');
            
            fprintf('Part F\n');
            fprintf('-----------------------------\n');
            fprintf('Velocity at Periapsis: V_p = %.5e km/s  \n', v_p);
            fprintf('-----------------------------\n');
            
            fprintf('Part G\n');
            fprintf('-----------------------------\n');
            fprintf('Velocity at Apoapsis: V_a = %.5e km/s  \n', v_a);
            fprintf('-----------------------------\n');
            
            fprintf('Part H\n');
            fprintf('-----------------------------\n');
            fprintf('Orbiter period: T_c = %.5e  seconds or %.5e minutes \n', T_c, T_c/60);
            fprintf('-----------------------------\n');    

            fprintf('Part I\n');
            fprintf('-----------------------------\n');
            fprintf('Specific Mechanical Energy: epsilon = %.5e km^2/s^2  \n', epsilon);
            fprintf('-----------------------------\n');

            fprintf('Part J\n');
            fprintf('-----------------------------\n');
            fprintf('Angular Momentum: h = %.5e kg*km^2/s  \n', h);
            fprintf('-----------------------------\n');
            
            fprintf('Part K\n');
            fprintf('-----------------------------\n');
            fprintf('Mean Motion: n = %.5e  rad/s\n', n);
            fprintf('-----------------------------\n');
        end
    
    %% 2.3 TwoBody Equation of Motion for ODEs
        % Function defining the differential equations
        function dydt = twoBodyEOM(~, state)
            % Extract state variables from state vector
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
    
%% 3.0 Interplanetary Transfer Functions
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
                vHelioPlanetArrival = sqrt(muSun/targetPlanet.semiMajor);
                vHelioPlanetDeparture = sqrt(muSun/departurePlanet.semiMajor);
                n = (2 * pi/ targetPlanet.OSP) / 86400;                                                     %[radians/sec] | Mean Motions of planets
                vD = sqrt(2 * muSun) * sqrt(targetPlanet.semiMajor / ...
                    (departurePlanet.semiMajor * (departurePlanet.semiMajor + targetPlanet.semiMajor)));    %[km/s] | velocity of S/C departure 
                vA = (sqrt(2 * muSun) * sqrt((targetPlanet.semiMajor * departurePlanet.semiMajor) /...
                    (targetPlanet.semiMajor + departurePlanet.semiMajor))) / targetPlanet.semiMajor;        %[km/s] | velocity of S/C arrival
                phiRadDepart0 = pi * (1 - ((departurePlanet.semiMajor + targetPlanet.semiMajor) /...        %[radians]
                    (2 * targetPlanet.semiMajor))^(3/2));t/365
                phiDegDepart0 = phiRadDepart0 * (180/pi);                                                   %[deg]
                phiArrivalRad = phiRadDepart0 + (n - nDepature) * t;                                        %[radians]
                if abs(phiArrivalRad) > 2*pi
                   phiArrivalRad = mod(phiArrivalRad,(sign(phiArrivalRad))*2*pi);                           %[radians] | Correction for radians over 2 * pi
                end
                phiArrivalDeg = phiArrivalRad * (180/pi);                                                   %[deg]
                planetTheta2 = arrivalplanetTheta + n * t;                                                  %[deg]
                earthTheta2 = depaturePlanetTheta + n * t;                                                  %[deg]
                semiMajor = (targetPlanet.semiMajor + departurePlanet.semiMajor)/2;                         %[km] | Semi-major of the Hohmann transfer
                transferTime = (pi / sqrt(muSun)) * ((targetPlanet.semiMajor + departurePlanet.semiMajor)/2)^(3/2); %[sec] | Transfer time of the Hohmann transfer
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
    end
end
