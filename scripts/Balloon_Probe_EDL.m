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
system_mass = 3901.24; % [kg]

% Venusian atmospheric properties
R = 188.92; % Specific gas constant for CO2 in J/(kg*K)

% Define the relative path to the CSV file
relativePathToFile = '..\Genesis-v0.3.0\Project\multi_vehicle\';
fileName = 'output.csv';

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
            H_Venus(j,i) = (S_Venus * temperature_at_altitude(j,i))/(gravity); % [m] Scale height (Equation 12.55, pg. 545)

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
            v_terminal(j,i) = (sqrt((2 * gravity * system_mass) / (rho_at_altitude(j,i) * Co_D)))/1000; % [km/s] Terminal velocity\

            % Calculate the required parachute area (A) to achieve the desired descent speed
            A(j,i) = (2 * F_gravity(j,i)) / (Co_D * rho_at_altitude(j,i) * (v_terminal(j,i))^2);

            % Assuming the parachute is a hemisphere (common for parachute shapes)
            % Calculate the radius of a hemisphere with area A
            r(j,i) = sqrt(A(j,i) / (2 * pi)); 

            % Calculate the volume of the hemisphere (How much air is trapped beneath
            % the parachute
            V(j,i) = (2/3) * pi * (r(j,i))^3;

            Vp1 = v_terminal(j,i); % Initial velocity of the parachute at altitude j
            delta_V(j,i) = -v_terminal(j,i); % Assuming the parachute stops (final velocity is zero), change in velocity is the negative of initial velocity

            % Calculate the change in kinetic energy AKE
            AKE(j,i) = 0.5 * m_p * (2 * Vp1 * delta_V(j,i) + delta_V(j,i)^2); % Equation 2 from the paper

            % Calculate snatch force P using the derived equation (3)
            A = 11; % [m^2] Total cross-sectional area of suspension lines (based on parachute radius) 
            P(j,i) = 2 * AKE(j,i) / delta_e; % Snatch force equation
            
            % max(max(P))/32 to calculate snatch force as a function of the
            % shock
            % acceleration(j,i) = dataTable.t * 100)/60

            D(j,i) =  (rho_at_altitude(j,i) * (v_terminal(j,i) * 1000)^2)/2 * Co_D * A;
            m_air(j,i) = V(j,i) * rho_at_altitude(j,i);
            v_drag(j,i) = D(j,i)/m_air(j,i) * (dataTable.t(i)* 100);

            v_new(j,i) = v_terminal(j,i) * 1000 - v_drag(j,i);
        end

        F = phi .* dataTable.t .* v_terminal; % [atoms/cm^2] Atomic oxygen fluence
        d = E .* F; % [cm] Depth of material loss (Equation 12.92, pg. 562)

        % Plot results for the current altitude data
        plot((dataTable.t * 100)/60, altitude_data/1000, 'DisplayName', varName);
        hold on;
        title('Altitude vs. Time','Interpreter','latex');
        xlabel('Time (minutes)','Interpreter','latex');
        ylabel('Altitude (km)','Interpreter','latex');
        ylim([0, max(altitude_data/1000)])
        legend({'Back shell', 'Heatshield', 'Payload', 'Chute'}, 'Location', 'southeast');
        grid on
        
        % Display the terminal velocity at the last calculated point
        fprintf('Terminal velocity for %s at %.2f s: %.2f m/s\n', varName, dataTable.t(end), v_terminal(end));
    end
    xline((20 * 100)/60, 'k--', 'Label', 'Separation Child 1');
    xline((50 * 100)/60, 'k--', 'Label', 'Separation Child 1');
end

P = abs(P);
% Pressure at Alitutde Plot
figure;
hold on
plot((dataTable.t * 100)/60,pressure_at_altitude(:,2),'LineWidth',2,'Color','blue') 
plot((dataTable.t * 100)/60,pressure_at_altitude(:,3),'LineWidth',2,'Color',[0, 0.5, 0]) 
plot((dataTable.t * 100)/60,pressure_at_altitude(:,4),'LineWidth',2,'Color','red') 
plot((dataTable.t * 100)/60,pressure_at_altitude(:,5),'LineWidth',2,'Color','g') 
title('Pressure vs Time','Interpreter','latex')
xlabel('Time (minutes)','Interpreter','latex');
ylabel('Pressure (Pa)','Interpreter','latex');
legend({'Back shell', 'Heatshield', 'Payload', 'Chute'}, 'Location', 'southeast','Interpreter','latex');
grid on
saveas(gcf, 'pressureVsAltitude.png');

% Altitude vs Time Plot
figure;
hold on
plot((dataTable.t * 100)/60,dataTable.h_parent/1000,'LineWidth',2,'Color','blue') 
plot((dataTable.t * 100)/60,dataTable.h_child1/1000,'LineWidth',2,'Color',[0, 0.5, 0]) 
plot((dataTable.t * 100)/60,dataTable.h_child2/1000,'LineWidth',2,'Color','red') 
plot((dataTable.t * 100)/60,dataTable.h_child3/1000,'LineWidth',2,'Color','g') 
title('Altitude vs. Time','Interpreter','latex');
xlabel('Time (minutes)','Interpreter','latex');
ylabel('Altitude (km)','Interpreter','latex');
ylim([60,100])
grid on
xline(5, 'k--', 'Label', 'Separation of Heatshield');
xline(50, 'k--', 'Label', 'Separation of Back shell');
xline(80, 'k--', 'Label', 'Chute');
legend({'Back shell', 'Heatshield', 'Payload', 'Chute', 'Sep (Heat shield)', 'Sep (Back shell)'}, 'Location', 'southeast','Interpreter','latex');
saveas(gcf, 'AltitudeVsTime.png');

% Terminal Velocity at Altitude Plot
figure;
hold on
plot((dataTable.t * 100)/60,v_terminal(:,2),'LineWidth',2,'Color','blue') 
plot((dataTable.t * 100)/60,v_terminal(:,3),'LineWidth',2,'Color',[0, 0.5, 0]) 
plot((dataTable.t * 100)/60,v_terminal(:,4),'LineWidth',2,'Color','red') 
plot((dataTable.t * 100)/60,v_terminal(:,5),'LineWidth',2,'Color','g') 
title('Terminal Velocity vs Time','Interpreter','latex')
xlabel('Time (minutes)','Interpreter','latex');
ylabel('Terminal velocity (km/s)','Interpreter','latex');
legend({'Back shell', 'Heatshield', 'Payload', 'Chute'}, 'Location', 'southeast','Interpreter','latex');
grid on
saveas(gcf, 'velocityVsAltitude.png');

% Temperature at Altitude Plot
figure;
hold on
plot((dataTable.t * 100)/60,temperature_at_altitude(:,2),'LineWidth',2,'Color','blue') 
plot((dataTable.t * 100)/60,temperature_at_altitude(:,3),'LineWidth',2,'Color',[0, 0.5, 0]) 
plot((dataTable.t * 100)/60,temperature_at_altitude(:,4),'LineWidth',2,'Color','red') 
plot((dataTable.t * 100)/60,temperature_at_altitude(:,5),'LineWidth',2,'Color','g') 
title('Temperature vs Time','Interpreter','latex')
xlabel('Time (minutes)','Interpreter','latex');
ylabel('Temperature (K)','Interpreter','latex');
legend({'Back shell', 'Heatshield', 'Payload', 'Chute'}, 'Location', 'southeast','Interpreter','latex');
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
ylim([0, 1E5])
xlim([0,150])
grid on
saveas(gcf, 'buoyancyVsTime.png');

grid on 

% Altitude vs. Velocity
figure;
hold on
plot(v_new(:,4)/1000, dataTable.h_child2/1000,'LineWidth',2,'Color','blue') 
title('Altitude vs Velocity','Interpreter','latex')
xlabel('Velocity (km/s)','Interpreter','latex');
ylabel('Altitude (km)','Interpreter','latex');
% legend({'Buoyancy Force','Gravitational Force'}, 'Location', 'southeast','Interpreter','latex');
grid on
saveas(gcf, 'velocityVsAltitude.png');

grid on

%%
% Scale Height at Altitude Plot 
figure;
hold on;

% Plotting the Scale Height vs Altitude on the left y-axis
yyaxis left;
plot(dataTable.h_child2/1000, H_Venus(:,4)/1000, 'LineWidth', 2, 'Color', 'blue');
xlabel('Altitude (km)', 'Interpreter', 'latex');
ylabel('Scale height (km)', 'Interpreter', 'latex');

% Now, create the right y-axis for time
yyaxis right;
plot(dataTable.h_child2/1000, (dataTable.t * 100)/60, 'LineWidth', 2, 'Color', 'red', 'LineStyle', '--');
ylabel('Time (minutes)', 'Interpreter', 'latex');

% Set the x-axis limit
xlim([0, 100]);

% Adding title and grid
title('Scale Height vs Altitude (Time on right axis)', 'Interpreter', 'latex');
grid on;

% Save the figure
saveas(gcf, 'ScaleHeightVsAltitude_Time.png');


%% Density
figure;
hold on
plot(rho_at_altitude(:,4), dataTable.h_child2/1000, 'LineWidth',2,'Color','blue') 
title('Density vs Altitude (Over time)','Interpreter','latex')
xlabel('Density (kg/m^3)','Interpreter','latex');
ylabel('Altitude (km)','Interpreter','latex');
ylim([40,100])
grid on
saveas(gcf, 'DensityVsAltitude.png');

%% Snatch Force
figure;
hold on
plot((dataTable.t * 100)/60, P(:,2), 'LineWidth',2,'Color','blue') 
title('Snatch force vs. Time','Interpreter','latex')
xlabel('Time (minutes)','Interpreter','latex');
ylabel('Snatch Force (N)','Interpreter','latex');
grid on
saveas(gcf, 'Snatch Force vs. Time.png');

%% Depth of Material Loss
mu_Venus = 324859; % [km^3/s^2] 
v_Venus = sqrt(mu_Venus/6097);
phi = 100; % [atoms/(cm^2*s)] Solar flux

F = phi .* (30 * 86400) * (v_Venus * 1E5); % [atoms/cm^2] Atomic oxygen fluence
d_30 = E * F;
d_30_nano = d_30 * 1E7;
% %% Orbital Elements to RVs 
% mu = 324859; % [km^3/s^2]
% dataTable.Semimajor = dataTable.Semimajor + 6052 * 1000;
% % [r_I, v_I] = coe2rv(e, a, i, w, Om, TA, mu)
% [r_I, v_I] = coe2rv(dataTable.Eccentricity, dataTable.Semimajor, dataTable.inclination, dataTable.AOP, dataTable.RAAN, dataTable.trueAnomaly, mu);
% %% Plot Orbit 
% figure;
% r_I = r_I/1000;
% v_I = v_I/1000;
% % Define the radius of the Earth (in kilometers)
% venusRadius = 6052; % Approximate radius of the VenusTexture = imread(earthTextureImage); % Load your own Venus texture image
% 
% venusTextureImage = append(pwd, '\..\data\venusTexture.jpg');
% venusTexture = imread(venusTextureImage); % Load your own Venus texture image
% venusTexture = flipud(venusTexture);
% 
% % Create a sphere for Earth
% [xVenus, yVenus, zVenus] = sphere;
% xVenus = xVenus * venusRadius;
% yVenus = yVenus * venusRadius;
% zVenus = zVenus * venusRadius;
% 
% plot3(r_I(:,1), r_I(:,2), r_I(:,3), 'b');
% xlabel('X (km)', 'FontWeight','bold','Color','w');
% ylabel('Y (km)', 'FontWeight','bold','Color','w');
% zlabel('Z (km)', 'FontWeight','bold','Color','w');
% title('Satellite Circular Orbit', 'FontWeight','bold','Color','w');
% grid on;
% axis equal;
% set(gca,'Color',[0,0,0])
% set(gcf,'Color',[0,0,0])
% hold on
% surf(xVenus, yVenus, zVenus, 'CData', venusTexture, 'FaceColor', 'texturemap', 'EdgeColor', 'none');
% axis equal;
% xlabel('X (km)');
% ylabel('Y (km)');
% zlabel('Z (km)');
% grid on
% title(sprintf('Orbit Visualization'));

%% Saving all the new data to a CSV with the altitude and time values
% After all calculations are done, create a table with the generated data
resultsTable = table((dataTable.t * 100)/60, altitude_data, pressure_at_altitude, temperature_at_altitude, v_terminal, F_buoyancy, F_gravity, ... % Add other arrays as needed
    'VariableNames', {'Time', 'Altitude', 'Pressure_at_Altitude', 'Temperature_at_Altitude', 'Terminal_Velocity', 'Buoyancy force', 'Gravity Force'}); % Update with actual variable names

% Define the name and path for the new CSV file
newFileName = 'resultsData.csv';
newFullPath = append(pwd,'\..\data\', newFileName);

% Write the table to a new CSV file
writetable(resultsTable, newFullPath);

fprintf('Data saved to %s\n', newFullPath);

function [r_I, v_I] = coe2rv(e, a, i, w, Om, TA, mu)
    % Ensure inputs are column vectors for consistent dimensions
    if isrow(e); e = e'; end
    if isrow(a); a = a'; end
    if isrow(i); i = i'; end
    if isrow(w); w = w'; end
    if isrow(Om); Om = Om'; end
    if isrow(TA); TA = TA'; end
    if isrow(mu); mu = mu'; end
    
    % Calculate the specific angular momentum for each set of elements
    h = sqrt(a .* mu .* (1 - e.^2));
    
    % Calculate the magnitude of the radius vector
    norm_r = h.^2 ./ mu ./ (1 + e .* cosd(TA));
    
    % Position vector in the perifocal frame
    r_PF = [norm_r .* cosd(TA), norm_r .* sind(TA), zeros(size(norm_r))];
    
    % Velocity vector in the perifocal frame
    v_PF = [mu ./ h .* (-sind(TA)), mu ./ h .* (e + cosd(TA)), zeros(size(norm_r))];
    
    % Preallocate arrays for the output
    r_I = zeros(size(r_PF));
    v_I = zeros(size(v_PF));
    
    % Loop through each set of elements to apply the rotation
    for k = 1:size(e, 1)
        % Rotation matrix from the inertial frame to the frame defined by the
        % longitude of ascending node (Om)
        R_O = [cosd(Om(k)) sind(Om(k)) 0; -sind(Om(k)) cosd(Om(k)) 0; 0 0 1];
        
        % Rotation matrix from the frame defined by Om to the frame defined by
        % the inclination (i)
        R_i = [1 0 0; 0 cosd(i(k)) sind(i(k)); 0 -sind(i(k)) cosd(i(k))];
        
        % Rotation matrix from the frame defined by the inclination to the
        % perifocal frame, which is defined by the argument of periapsis (w)
        R_w = [cosd(w(k)) sind(w(k)) 0; -sind(w(k)) cosd(w(k)) 0; 0 0 1];
        
        % Combined rotation matrix from the perifocal frame to the inertial frame
        R_PI = R_w * R_i * R_O;
        
        % Convert position and velocity vectors from the perifocal frame to the
        % inertial frame for each set
        r_I(k, :) = (R_PI' * r_PF(k, :)')';
        v_I(k, :) = (R_PI' * v_PF(k, :)')';
    end
end



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  DO NOT EDIT  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
