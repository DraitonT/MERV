clc, clear, close all
tic
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
system_mass = 3901.24; % [kg]

% Venusian atmospheric properties
R = 188.92; % Specific gas constant for CO2 in J/(kg*K)

inflationRate = 10; % [m^3/s] Placeholder inflation rate
inflationRateActual = (inflationRate * 100)/60;

%% Venus Scale Height Givens
gravity = 8.87; % [m/s^2] Gravity on surface of Venus
M_Venus = 0.04401; % [kg/mol] Molar mass of The Venusian atmosphere, mainly CO2
R_Venus = 8.314; % JK/(Mol*K) Universal gas constant - Venus
S_Venus = R_Venus/M_Venus; % Specific gas constant - Venus
T_Venus = 737; % [K] Venus surface temperature

%% Snatch Force Related
m_p = balloon_mass; % Mass of the parachute
delta_e = 0.5; % Change in length of suspension lines (this is a placeholder, you need to determine the actual value)

%% Erosion related
E = 0.34E-24; % [cm^3] Erosion yield of Twflon FEP, table 12.8 of Pisacane
phi = 100;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   EDITABLE   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  DO NOT EDIT  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

altitude_data = linspace(45,55,750);

% Generate a random permutation of indices
idx = randperm(length(altitude_data));

% Use the indices to shuffle the list
altitude_data = altitude_data(idx);

time = linspace(0,750,750);
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


for i = 1:length(altitude_data)
    % Interpolate atmospheric pressure and temperature based on altitude
    pressure_at_altitude(i) = interp1(height_m, pressure_Pa, altitude_data(i), 'linear', 'extrap');
    temperature_at_altitude(i) = interp1(height_m, temperature_C, altitude_data(i), 'linear', 'extrap') + 273.15; % Convert to Kelvin

    % Calculate the atmospheric density using the Ideal Gas Law: p = rho * R * T
    rho_at_altitude(i) = pressure_at_altitude(i) / (R * temperature_at_altitude(i)); % [kg/m^3]
    H_Venus(i) = (S_Venus * temperature_at_altitude(i))/(gravity); % [m] Scale height (Equation 12.55, pg. 545)

    % Calculating buoyancy force (Lifting capacity)
    % Inflation rate of gas or system will make the volume dynamic
    % based on the inflation rate and such
    volumeAtTime(i) = v_initial + (inflationRateActual * (time(i) * 100)/60);

    % Check to see if the volumeAtTime is greater than the max
    % possible volume for the balloon
    if volumeAtTime(i) > volume
        volumeAtTime(i) = volume;
    end

    F_buoyancy(i) = rho_at_altitude(i) * volumeAtTime(i) * gravity; % [N]
    F_gravity(i) = system_mass * gravity; % [N]

    % Calculate the drag coefficient (Placeholder value, replace with actual data)
    Co_D = 2.2; % [Unitless] Drag coefficient

    % Calculate terminal velocity
    v_terminal(i) = (sqrt((2 * gravity * system_mass) / (rho_at_altitude(i) * Co_D)))/1000; % [km/s] Terminal velocity\

    % Calculate the required parachute area (A) to achieve the desired descent speed
    A(i) = (2 * F_gravity(i)) / (Co_D * rho_at_altitude(i) * (v_terminal(i))^2);

    % Assuming the parachute is a hemisphere (common for parachute shapes)
    % Calculate the radius of a hemisphere with area A
    r(i) = sqrt(A(i) / (2 * pi));

    % Calculate the volume of the hemisphere (How much air is trapped beneath
    % the parachute
    V(i) = (2/3) * pi * (r(i))^3;

    Vp1 = v_terminal(i); % Initial velocity of the parachute at altitude j
    delta_V(i) = -v_terminal(i); % Assuming the parachute stops (final velocity is zero), change in velocity is the negative of initial velocity

    % Calculate the change in kinetic energy AKE
    AKE(i) = 0.5 * m_p * (2 * Vp1 * delta_V(i) + delta_V(i)^2); % Equation 2 from the paper

    % Calculate snatch force P using the derived equation (3)
    A = 113.32; % [m^2] Total cross-sectional area of suspension lines (based on parachute radius)
    P(i) = 2 * AKE(i) / delta_e; % Snatch force equation

    F = phi .* time(i) .* v_terminal; % [atoms/cm^2] Atomic oxygen fluence
    d = E .* F; % [cm] Depth of material loss (Equation 12.92, pg. 562)

end

%% Saving all the new data to a CSV with the altitude and time values
% After all calculations are done, create a table with the generated data
resultsTable = table(time', altitude_data', pressure_at_altitude', temperature_at_altitude', v_terminal', F_buoyancy', F_gravity', ... % Add other arrays as needed
    'VariableNames', {'Time', 'Altitude', 'Pressure_at_Altitude', 'Temperature_at_Altitude', 'Terminal_Velocity', 'Buoyancy force', 'Gravity Force'}); % Update with actual variable names

% Define the name and path for the new CSV file
newFileName = 'resultsData_float.csv';
newFullPath = append(pwd,'\..\data\', newFileName);

% Write the table to a new CSV file
writetable(resultsTable, newFullPath);

fprintf('Data saved to %s\n', newFullPath);

toc

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  DO NOT EDIT  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
