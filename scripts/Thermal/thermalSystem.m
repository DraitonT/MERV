clc, clear, close all
tic

%%%%%%%%%%%%%%%%%%%%%%%%% EDITABLE %%%%%%%%%%%%%%%%%%%%%%%%%

% Constants
emissivity = 0.02; % Emissivity
stefan_boltzmann_constant = 5.67e-8; % Stefan-Boltzmann constant in W/m^2K^4
diameter_m = 3.04; % Diameter of the sphere in meters

temp_hot_C = 477; % Highest temperature in Celsius
temp_cold_C = 27; % Lowest temperature in Celsius
passive_system_W = 2608; % [K] Sodium -> Table 12.10, pg. 382
num_heat_pipes = 4; % Number of heat pipes

kg_meter = .33; % [kg/meter]
pipe_D = 12.7E-3; % [meter] Diameter of the pipe

h_convective = 65; % [W/(m^2 * K)]
T_base = 40; % [C] Temperature of the base 

%%%%%%%%%%%%%%%%%%%%%%%%% EDITABLE %%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%% NON-EDITABLE %%%%%%%%%%%%%%%%%%%%%%%
% Convert diameter to radius
radius_m = diameter_m / 2;
circumference_m = 2 * pi * radius_m;
lengthOfHeatPipes = circumference_m/2 * num_heat_pipes;
weightOfHeatPipes = kg_meter * lengthOfHeatPipes;

% Calculate the surface area of the sphere
surface_area_m2 = 4 * pi * radius_m^2;

% Convert temperatures from Celsius to Kelvin
temp_hot_K = temp_hot_C + 273.15;
temp_cold_K = temp_cold_C + 273.15;

% Calculate the radiative heat transfer using Newton's Cooling law (Convection)
heat_transfer_radiation = h_convective * surface_area_m2 * (temp_hot_K)

% Calculate the radiative heat transfer using Stefan-Boltzmann law (Radiation)
heat_transfer_W_radiation = emissivity * stefan_boltzmann_constant * surface_area_m2 * ...
                  (temp_hot_K^4 - temp_cold_K^4);

heat_transfer_W_radiation = heat_transfer_W_radiation - passive_system_W * num_heat_pipes;

% Display the result
fprintf('The radiative power required to maintain the temperature range is: %.2f W\n', ...
        heat_transfer_W_radiation);
fprintf('Length of Heat Pipes: %0.8f meters\n Weight of heat pipes: %0.8f kg', lengthOfHeatPipes, weightOfHeatPipes)

tic
%%%%%%%%%%%%%%%%%%%%%%% NON-EDITABLE %%%%%%%%%%%%%%%%%%%%%%%