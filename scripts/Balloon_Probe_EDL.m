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

% Inflation gas 
density_gas = 0.08988; % [kg/m^3] Hydrogen
volume = 650; % [m^3] Volume of the balloon

% Inputs for the balloon probe system
balloon_mass = 718.6953082; % [kg] mass of the balloon probe system
spacecraft_mass = 3423; % [kg]
system_mass = balloon_mass + spacecraft_mass; % [kg]

% Venusian atmospheric properties
gravity = 8.87; % [km/s^2] gravity on Venus
R = 188.92; % Specific gas constant for CO2 in J/(kg*K)

% Define the relative path to the CSV file
relativePathToFile = '..\Genesis-v0.3.0\Project\multi_vehicle\';
fileName = 'output.csv';

inflationRate = 10; % [m^3/s] Placeholder inflation rate
inflationRateActual = (inflationRate * 100)/60;

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
v_initial = 0; % [m^3] Initial volume of the balloon

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
            rho_at_altitude(j,i) = pressure_at_altitude(j,i) / (R * temperature_at_altitude(j,i)); % [kg/m^3]
    
            % Calculating buoyancy force (Lifting capacity)
            % Inflation rate of gas or system will make the volume dynamic
            % based on the inflation rate and such
            volumeAtTime(j,i) = v_initial + (inflationRateActual * (dataTable.t(j) * 100)/60);
            
            % Check to see if the volumeAtTime is greater than the max
            % possible volume for the balloon
            if volumeAtTime(j,i) > volume
                volumeAtTime(j,i) = volume;
            end

            F_buoyancy(j,i) = rho_at_altitude(j,i) * volumeAtTime(j,i) * gravity; % [N]
            F_gravity(j,i) = system_mass * gravity; % [N]

            % Calculate the drag coefficient (Placeholder value, replace with actual data)
            Co_D = 2.2; % [Unitless] Drag coefficient
            
            % Calculate terminal velocity
            v_terminal(j,i) = (sqrt((2 * gravity * system_mass) / (rho_at_altitude(j,i) * Co_D)))/1000; % [km/s] Terminal velocity
        end
        
        % Plot results for the current altitude data
        plot((dataTable.t * 100)/60, altitude_data/1000, 'DisplayName', varName);
        hold on;
        title('Altitude vs. Time','Interpreter','latex');
        xlabel('Time (minutes)','Interpreter','latex');
        ylabel('Altitude (km)','Interpreter','latex');
        ylim([0, max(altitude_data/1000)])
        legend({'Back shell', 'Heatshield', 'Payload'}, 'Location', 'southeast');
        grid on
        
        % Display the terminal velocity at the last calculated point
        fprintf('Terminal velocity for %s at %.2f s: %.2f m/s\n', varName, dataTable.t(end), v_terminal(end));
    end
    xline((20 * 100)/60, 'k--', 'Label', 'Separation Child 1');
    xline((50 * 100)/60, 'k--', 'Label', 'Separation Child 1');
end

% Pressure at Alitutde Plot
figure;
hold on
plot((dataTable.t * 100)/60,pressure_at_altitude(:,2),'LineWidth',2,'Color','blue') 
plot((dataTable.t * 100)/60,pressure_at_altitude(:,3),'LineWidth',2,'Color',[0, 0.5, 0]) 
plot((dataTable.t * 100)/60,pressure_at_altitude(:,4),'LineWidth',2,'Color','red') 
title('Pressure vs Time','Interpreter','latex')
xlabel('Time (minutes)','Interpreter','latex');
ylabel('Pressure (Pa)','Interpreter','latex');
legend({'Back shell', 'Heatshield', 'Payload'}, 'Location', 'southeast','Interpreter','latex');
grid on
saveas(gcf, 'pressureVsAltitude.png');

% Altitude vs Time Plot
figure;
hold on
plot((dataTable.t * 100)/60,dataTable.h_parent/1000,'LineWidth',2,'Color','blue') 
plot((dataTable.t * 100)/60,dataTable.h_child1/1000,'LineWidth',2,'Color',[0, 0.5, 0]) 
plot((dataTable.t * 100)/60,dataTable.h_child2/1000,'LineWidth',2,'Color','red') 
title('Altitude vs. Time','Interpreter','latex');
xlabel('Time (minutes)','Interpreter','latex');
ylabel('Altitude (km)','Interpreter','latex');
ylim([60,100])
grid on
xline((5 * 100)/60, 'k--', 'Label', 'Separation of Heatshield');
xline((50 * 100)/60, 'k--', 'Label', 'Separation of Back shell');
legend({'Back shell', 'Heatshield', 'Payload', 'Sep (Heat shield)', 'Sep (Back shell)'}, 'Location', 'southeast','Interpreter','latex');
saveas(gcf, 'AltitudeVsTime.png');

% Terminal Velocity at Altitude Plot
figure;
hold on
plot((dataTable.t * 100)/60,v_terminal(:,2),'LineWidth',2,'Color','blue') 
plot((dataTable.t * 100)/60,v_terminal(:,3),'LineWidth',2,'Color',[0, 0.5, 0]) 
plot((dataTable.t * 100)/60,v_terminal(:,4),'LineWidth',2,'Color','red') 
title('Terminal Velocity vs Time','Interpreter','latex')
xlabel('Time (minutes)','Interpreter','latex');
ylabel('Terminal velocity (km/s)','Interpreter','latex');
legend({'Back shell', 'Heatshield', 'Payload'}, 'Location', 'southeast','Interpreter','latex');
grid on
saveas(gcf, 'velocityVsAltitude.png');

% Temperature at Altitude Plot
figure;
hold on
plot((dataTable.t * 100)/60,temperature_at_altitude(:,2),'LineWidth',2,'Color','blue') 
plot((dataTable.t * 100)/60,temperature_at_altitude(:,3),'LineWidth',2,'Color',[0, 0.5, 0]) 
plot((dataTable.t * 100)/60,temperature_at_altitude(:,4),'LineWidth',2,'Color','red') 
title('Temperature vs Time','Interpreter','latex')
xlabel('Time (minutes)','Interpreter','latex');
ylabel('Temperature (K)','Interpreter','latex');
legend({'Back shell', 'Heatshield', 'Payload'}, 'Location', 'southeast','Interpreter','latex');
grid on
saveas(gcf, 'temperatureVsAltitude.png');

% Buoyancy Force at Altitude Plot 
figure;
hold on
plot((dataTable.t * 100)/60,F_buoyancy(:,4),'LineWidth',2,'Color','blue') 
plot((dataTable.t * 100)/60,F_gravity(:,4),'LineWidth',2,'Color',[0, 0.5, 0]) 
title('Buoyancy force vs Gravity Force (Over time)','Interpreter','latex')
xlabel('Time (minutes)','Interpreter','latex');
ylabel('Force (N)','Interpreter','latex');
legend({'Buoyancy Force','Gravitational Force'}, 'Location', 'southeast','Interpreter','latex');
grid on
saveas(gcf, 'temperatureVsAltitude.png');

grid on 

% Altitude vs. Velocity
figure;
hold on
plot(v_terminal(:,4), dataTable.h_child2/1000,'LineWidth',2,'Color','blue') 
title('Altitude vs Velocity','Interpreter','latex')
xlabel('Velocity (km/s)','Interpreter','latex');
ylabel('Altitude (km)','Interpreter','latex');
% legend({'Buoyancy Force','Gravitational Force'}, 'Location', 'southeast','Interpreter','latex');
grid on
saveas(gcf, 'velocityVsAltitude.png');

grid on

%% Saving all the new data to a CSV with the altitude and time values
% After all calculations are done, create a table with the generated data
resultsTable = table(dataTable.t, altitude_data, pressure_at_altitude, temperature_at_altitude, v_terminal, F_buoyancy, F_gravity, ... % Add other arrays as needed
    'VariableNames', {'Time', 'Altitude', 'Pressure_at_Altitude', 'Temperature_at_Altitude', 'Terminal_Velocity', 'Buoyancy force', 'Gravity Force'}); % Update with actual variable names

% Define the name and path for the new CSV file
newFileName = 'resultsData.csv';
newFullPath = fullfile(relativePathToFile, newFileName);

% Write the table to a new CSV file
writetable(resultsTable, newFullPath);

fprintf('Data saved to %s\n', newFullPath);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  DO NOT EDIT  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
