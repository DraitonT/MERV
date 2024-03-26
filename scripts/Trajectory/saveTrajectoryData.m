clc, clear, close all
txtFileEarth = append(pwd, '\..\data\', 'earth.txt');
txtFileVenus = append(pwd, '\..\data\', 'Venus.txt');

paramsEarth = struct(...
    'COMMAND', '399', ...
    'OBJ_DATA', 'NO', ...
    'MAKE_EPHEM', 'YES', ...
    'EPHEM_TYPE', 'VECTORS', ...
    'CENTER', '500@0', ...
    'START_TIME', '2035-07-01', ...
    'STOP_TIME', '2035-07-02', ...
    'STEP_SIZE', '1', ...
    'QUANTITIES', '2' ...
);

paramsVenus = struct(...
    'COMMAND', '299', ...
    'OBJ_DATA', 'NO', ...
    'MAKE_EPHEM', 'YES', ...
    'EPHEM_TYPE', 'VECTORS', ...
    'CENTER', '500@0', ...
    'START_TIME', '2036-01-17', ...
    'STOP_TIME', '2036-01-18', ...
    'STEP_SIZE', '1', ...
    'QUANTITIES', '2' ...
);

dt = 1;
Orbit = 'Prograde';     % Type of orbit 
mu = 132712440018; %[km^3/s^2] Gravitational parameter of the sun
%% Call to JPL Horizons
Orbital.jplHorizonRequest(paramsEarth, txtFileEarth)
Orbital.jplHorizonRequest(paramsVenus, txtFileVenus)

[outputStatesEarth] = Orbital.parseJPLHorizonStates(txtFileEarth);
[outputStatesMars] = Orbital.parseJPLHorizonStates(txtFileVenus);

%% 1.1 Initialize matrices for speed
    % Calculate the number of iterations and round up to the nearest integer


    [rowsEarth, columnsEarth] = size(outputStatesEarth);
    [rowsMars, columnsMars] = size(outputStatesMars);
%% 2.0 Main loop with Lambert's Problem
    %% 2.1 Retrieves the Julian date and states of Earth from URL stored in text file
        for i = 1:dt:rowsEarth
            t1 = (outputStatesEarth(i, 1)); % Time state [Julian days]
            departureDate = datetime(t1,'convertfrom','juliandate');

            fprintf('The departure date will be:\n')
            disp(departureDate)
            t1_matrix(ceil(i / dt)) = t1;   

            X = outputStatesEarth(i,2);     % X position [km]
            Y = outputStatesEarth(i,3);     % Y position [km]
            Z = outputStatesEarth(i,4);     % Z position [km]
            VX = outputStatesEarth(i,5);    % Velocity in x-direction [km/s]
            VY = outputStatesEarth(i,6);    % Velocity in y-direction [km/s]
            VZ = outputStatesEarth(i,7);    % Velocity in z-direction [km/s]
            
            r1_vec = [X, Y, Z]';
            v1_vec = [VX, VY, VZ]';
        
            rowIndex = ceil(i / dt);
      %% 2.2 Retrieves the Julian date and states of Mars from URL stored in text file  
            for j = 1:dt:rowsMars
                t2 = (outputStatesMars(j, 1));  % Time state [Julian days]
                arrivalDate = datetime(t2,'convertfrom','juliandate');

                fprintf('The arrival date will be:\n')
                disp(arrivalDate)

                t2_matrix(ceil(j / dt)) = t2;
                X = outputStatesMars(j,2);      % X position [km]
                Y = outputStatesMars(j,3);      % Y position [km] 
                Z = outputStatesMars(j,4);      % Z position [km]
                VX = outputStatesMars(j,5);     % Velocity in x-direction [km/s]
                VY = outputStatesMars(j,6);     % Velocity in y-direction [km/s]
                VZ = outputStatesMars(j,7);     % Velocity in z-direction [km/s]
                
                TOF = t2 - t1;
                r2_vec = [X, Y, Z]';
                v2_vec = [VX, VY, VZ]';
        
                [v1_vecOrb, v2_vecOrb, vinf_dep_vec, vinf_arr_vec] = Orbital.lambertFit(r1_vec, v1_vec, r2_vec, v2_vec, TOF, Orbit);
            %% 2.2.1 Calculatesd the C3s
                C3_dep = (norm(vinf_dep_vec))^2; % Characteristic energy of departure [km^2/s^2]
                C3_arr = (norm(vinf_arr_vec))^2; % Characteristic energy of arrival[km^2/s^2]
                
                % Store C3_dep and C3_arr values using indices
                columnIndex = ceil(j / dt);
                C3_dep_matrix(rowIndex, columnIndex) = C3_dep;
                C3_arr_matrix(rowIndex, columnIndex) = C3_arr;
            end
        end

    %% 1.5 Orbits Orbital Elements
    % Orbital Elements of Planet 1 from Ephemeris Data
    [a, h, e, i, RAAN, AOP, TA1] = Orbital.rvToOE(mu, r1_vec, v1_vec);
    % Orbital Elements of Planet 2 from Ephemeris Data
    [a2, h_mag2, e_mag2, i2, RAAN2, AOP2, TA2] = Orbital.rvToOE(mu, r2_vec, v2_vec);
    % Orbital Elements of Planet 1 at Departure 
    [a_P1, h_P1,e_P1,i_P1, Om_P1, w_P1,~] = Orbital.rvToOE(mu, r1_vec',VP1_dep');
    % Orbital Elements of Planet 2 at Arrival
    [a_P2, h_P2,e_P2,i_P2, Om_P2, w_P2,~] = Orbital.rvToOE(mu, r2_vec',VP2_arr');

    a_transfer = (a_P2 + a_P1)/2;
%% 1.6 Setting for Trajectory Plotter
    if nominalPlots
        if multipleOrbits
            plotOrbit = true;
            velocityVsTime = false;
            multipleVelocity = false;
        else
            velocityVsTime = false;
            multipleVelocity = false;
            plotOrbit = true;
        end
    end
    
%% 1.7 Initial Conditions for Numerical Methods Solver (2-Body)
    initialConditionEarth = [earthR, earthV];
    initialConditionDeparture = [earthR, v1_vec'];
    initialConditionMars = [marsR, marsV];
%% 1.8 Trajector Plotters
    T_dep = TOF;
   [~, stateDeparture,~] = Orbital.numericalTwoBody(initialConditionDeparture, T_dep, numOrbits, optionsCorrections, toleranceMin, toleranceMax);
   Orbital.plot2Body(stateDeparture, green);

   T_transfer = (2*pi/sqrt(mu)) * a_P1^(3/2);
   [~, stateEarth,~] = Orbital.numericalTwoBody(initialConditionEarth, T_transfer-10000, numOrbits, optionsCorrections, toleranceMin, toleranceMax);
   Orbital.plot2Body(stateEarth, blue);

   T_arr = (2*pi/sqrt(mu)) * a_P2^(3/2);
   [~, stateMars,~] = Orbital.numericalTwoBody(initialConditionMars, T_arr, numOrbits, optionsCorrections, toleranceMin, toleranceMax);
   Orbital.plot2Body(stateMars, red);
   
%% 1.8.1 Adding additional features (Celestial Bodies) onto the plots
    l_S = plot3(0,0,0,'markeredgecolor',[1,1,0.4],'markerfacecolor',[1,1,0.4],'marker','o','markersize',10,'color','none');
    l_P1     = plot3(r1_vec(1,1),r1_vec(2,1),r1_vec(3,1),'markeredgecolor',blue,'markerfacecolor',blue,'marker','o','markersize',8,'color',blue);
    l_P2     = plot3(r2_vec(1,1),r2_vec(2,1),r2_vec(3,1),'markeredgecolor',red,'markerfacecolor',red,'marker','o','markersize',8,'color',red);
    
    leg = legend([l_S,l_P1,l_P2],'Sun',sprintf('%s',Planet_1),sprintf('%s',Planet_2));
    leg.TextColor=[1 1 1];
    text_P1 = sprintf('   %s on %d-%d-%d',Planet_1,yyyyD,mmD,ddD);
    text_P2 = sprintf('   %s on %d-%d-%d',Planet_2,yyyyA,mmA,ddA);
    text(r1_vec(1,1)+.1,r1_vec(2,1)+.1,r1_vec(3,1)+.1,text_P1,'Color','w')%,'FontSize',16,'interpreter','latex');
    text(r2_vec(1,1)+.1,r2_vec(2,1)+.1,r2_vec(3,1)+.1,text_P2,'Color','w')%,'FontSize',16,'interpreter','latex');
