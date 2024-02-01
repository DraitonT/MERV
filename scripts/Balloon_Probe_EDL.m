clc, clear, close all

% -------------MATLAB Script Information-------------
% Author Names: Rene Holmes
% Team Number: 1
% Date: Spring 2024
% Tool Version: R2022a
% Purpose of Script: Pressure and terminal velocity calculations from
% altitude values from Genesis
% other .m files required: None
% other files required (not .m): output.csv

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   EDITABLE   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Inputs for the balloon probe system
balloon_mass = 718.6953082; % [kg] mass of the balloon probe system
spacecraft_mass = 3423; % [kg]
system_mass = balloon_mass + spacecraft_mass; % [kg]

% Venusian atmospheric properties
gravity = 8.87; % [m/s^2] gravity on Venus

% Define the relative path to the CSV file
relativePathToFile = '..\Genesis-v0.3.0\Project\multi_vehicle\';
fileName = 'output.csv';

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   EDITABLE   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  DO NOT EDIT  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fullPath = fullfile(relativePathToFile, fileName);

% Data Table of Results from Genesis
dataTable = readtable(fullPath);

% Initialize arrays to hold interpolated data and speed results
pressure_at_altitude = [];
temperature_at_altitude = [];
v_terminal = [];

% Given atmospheric data for Venus
height_km = [0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 80, 90, 100]; % Heights in km
temperature_C = [462, 424, 385, 348, 306, 264, 222, 180, 143, 110, 75, 27, -10, -30, -43, -76, -104, -112]; % Temperatures in degrees Celsius
pressure_atm = [92.10, 66.65, 47.39, 33.04, 22.52, 14.93, 9.851, 5.917, 3.501, 1.979, 1.066, 0.5314, 0.2357, 0.09765, 0.03690, 0.004760, 0.0003736, 0.00002660]; % Pressures in atm

% Convert heights to meters and pressures to Pascals (1 atm = 101325 Pa)
height_m = height_km * 1000;
pressure_Pa = pressure_atm * 101325;

% Process each column in the table
for i = 1:width(dataTable)
    varName = dataTable.Properties.VariableNames{i};
    
    if contains(varName, 'h_') % Process only altitude-related columns
        altitude_data = dataTable.(varName);
        
        for j = 1:length(altitude_data)
            % Interpolate atmospheric pressure and temperature based on altitude
            pressure_at_altitude(j,i) = interp1(height_m, pressure_Pa, altitude_data(j), 'linear', 'extrap');
            temperature_at_altitude(j,i) = interp1(height_m, temperature_C, altitude_data(j), 'linear', 'extrap') + 273.15; % Convert to Kelvin
            
            % Calculate the atmospheric density using the Ideal Gas Law: p = rho * R * T
            R = 188.92; % Specific gas constant for CO2 in J/(kg*K)
            rho_at_altitude = pressure_at_altitude(j,i) / (R * temperature_at_altitude(j,i));
            
            % Calculate the drag coefficient (Placeholder value, replace with actual data)
            Co_D = 2.2; % Drag coefficient
            
            % Calculate terminal velocity
            v_terminal(j,i) = (sqrt((2 * gravity * system_mass) / (rho_at_altitude * Co_D)))/1000;
        end
        
        % Plot results for the current altitude data
        plot((dataTable.t * 100)/60, altitude_data/1000, 'DisplayName', varName);
        hold on;
        title('Altitude vs. Time','Interpreter','latex');
        xlabel('Time (minutes)','Interpreter','latex');
        ylabel('Altitude (km)','Interpreter','latex');
        ylim([0, max(altitude_data/1000)])
        legend({'Aeroshell', 'Heatshield', 'Payload'}, 'Location', 'southeast');
        grid on
        
        % Display the terminal velocity at the last calculated point
        fprintf('Terminal velocity for %s at %.2f s: %.2f m/s\n', varName, dataTable.t(end), v_terminal(end));
    end
end

% Pressure at Alitutde Plot
figure;
hold on
plot((dataTable.t * 100)/60,pressure_at_altitude(:,2)) 
plot((dataTable.t * 100)/60,pressure_at_altitude(:,3)) 
plot((dataTable.t * 100)/60,pressure_at_altitude(:,4)) 
title('Pressure vs Time','Interpreter','latex')
xlabel('Time (minutes)','Interpreter','latex');
ylabel('Pressure (Pa)','Interpreter','latex');
legend({'Aeroshell', 'Heatshield', 'Payload'}, 'Location', 'southeast','Interpreter','latex');
grid on

% Terminal Velocity at Altitude Plot
figure;
hold on
plot((dataTable.t * 100)/60,v_terminal(:,2)) 
plot((dataTable.t * 100)/60,v_terminal(:,3)) 
plot((dataTable.t * 100)/60,v_terminal(:,4)) 
title('Terminal Velocity vs Time','Interpreter','latex')
xlabel('Time (minutes)','Interpreter','latex');
ylabel('Terminal velocity (km/s)','Interpreter','latex');
legend({'Aeroshell', 'Heatshield', 'Payload'}, 'Location', 'southeast','Interpreter','latex');
grid on

% Temperature at Altitude Plot
figure;
hold on
plot((dataTable.t * 100)/60,temperature_at_altitude(:,2)) 
plot((dataTable.t * 100)/60,temperature_at_altitude(:,3)) 
plot((dataTable.t * 100)/60,temperature_at_altitude(:,4)) 
title('Temperature vs Time','Interpreter','latex')
xlabel('Time (minutes)','Interpreter','latex');
ylabel('Temperature (K)','Interpreter','latex');
legend({'Aeroshell', 'Heatshield', 'Payload'}, 'Location', 'southeast','Interpreter','latex');
grid on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  DO NOT EDIT  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
