clc
clear

% Rene J. Holmes II
% ARO 4821L SP'24

% Given atmospheric data for Venus
height_km = [0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 80, 90, 100]; % Heights in km
temperature_C = [462, 424, 385, 348, 306, 264, 222, 180, 143, 110, 75, 27, -10, -30, -43, -76, -104, -112]; % Temperatures in degrees Celsius
pressure_atm = [92.10, 66.65, 47.39, 33.04, 22.52, 14.93, 9.851, 5.917, 3.501, 1.979, 1.066, 0.5314, 0.2357, 0.09765, 0.03690, 0.004760, 0.0003736, 0.00002660]; % Pressures in atm

% Convert heights to meters and pressures to Pascals (1 atm = 101325 Pa)
height_m = height_km * 1000;
pressure_Pa = pressure_atm * 101325;

% Inputs for the balloon probe system
balloon_mass = 2400; % kg, mass of the balloon probe system
spacecraft_mass = 3423; % kg
system_mass = balloon_mass + spacecraft_mass; % kg
altitude = 50000; % m, altitude at which the speed is to be calculated

% Venusian atmospheric properties
gravity = 8.87; % m/s^2, gravity on Venus

% Interpolate atmospheric pressure and temperature based on altitude
pressure_at_altitude = interp1(height_m, pressure_Pa, altitude, 'linear', 'extrap');
temperature_at_altitude = interp1(height_m, temperature_C, altitude, 'linear', 'extrap') + 273.15; % Convert to Kelvin

% Calculate the atmospheric density using the Ideal Gas Law: p = rho * R * T
% where R for Venusian atmosphere is approx. R = 188.92 J/(kg*K)
R = 188.92; % Specific gas constant for CO2 in J/(kg*K)
rho_at_altitude = pressure_at_altitude / (R * temperature_at_altitude);

% Calculate atmospheric density at the given altitude
% rho_at_altitude = atmospheric_density(altitude);

% Calculate the drag coefficient (Placeholder value, replace with actual data)
Co_D = 2.2; % Drag coefficient

% Speed calculation
% Assuming terminal velocity for simplicity (can be replaced with a more complex model)
v_terminal = sqrt((2 * gravity * system_mass) / (rho_at_altitude * Co_D));

% Display the results
fprintf('At an altitude of %d m:\n', altitude);
fprintf('Atmospheric Pressure: %.5f MPa\n', pressure_at_altitude / 1e6); % Convert Pa to MPa
fprintf('Atmospheric Temperature: %.2f K\n', temperature_at_altitude);
fprintf('Speed of the balloon probe: %.2f m/s\n', v_terminal);


% Display the results
% fprintf('Speed of the balloon probe at an altitude of %d m: %.2f m/s\n', altitude, v_terminal);

% Equation for atmospheric density at Venus
% function rho = atmospheric_density(h)
%     % Placeholder model for atmospheric density (replace with accurate data)
%     rho0 = 67; % kg/m^3, base density at a reference altitude
%     H = 15000; % m, scale height (example value)
%     rho = rho0 * exp(-h/H);
% end