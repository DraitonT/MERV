clc, clear, close all
addpath(genpath(append(pwd,'\..\..\lib'))) %Adds Orbital.m library to the working path
tic
% -------------MATLAB Script Information-------------
% Author Names: Michael Quach
% Date: 10/14/23
% Tool Version: R2023a
% Purpose of Script: Homework 5 for ARO 5090 (Lambert's Problem)
% other .m files required: Orbital.m
% other files required (not .m): None

%%%%%%%%%%%%%%%%%%%%%%%%% EDITABLE %%%%%%%%%%%%%%%%%%%%%%%%%

%% 0.0 Initial Conditions for Lambert Solver and Plotter
mu = 132712440018; %[km^3/s^2] Gravitational parameter of the sun
TOF = 320; % [days] Time of Flight
numOrbits = 1;

earthR = [1.315386533115567E8, -7.200148094172376E7, 3.259147626853362E4];
earthV = [1.374271621633860E1, 2.605497393397116E1, -1.421352374887164E-3];

marsR =[-2.478882664224494E8, 2.502285057293286E7, 6.610234028189430E6];
marsV = [-1.559223594672144, -2.204943915362882E1, -4.234349764409195E-1];

plotOrbit = true;
dt = 10;

%% 0.1 Trajectory Launch Date and Targets
yyyy = 2022;  mm = 8;  dd = 25;
Planet_1 = 'Earth';
Planet_2 = 'Mars';

%% 0.2 Orbit Settings
Orbit = 'Prograde';
blue = 'b'; %Earth
green = 'g'; % Transfer Orbit
red = 'r'; % Mars

%% 0.3 Numerical Methods Settings
optionsCorrections = true;
toleranceMin = 1e-6;
toleranceMax = 1e-9;
Options = optimoptions('fsolve','Display','off','TolFun',1E-12,'TolX',1E-12);

%% 0.4 Plot Settings for Trajectory Plotter
    nominalPlots = true;
    multipleOrbits = true; % True to see Multiple Orbits, False to see Velocity comparsion
   
    Orbit = 'Prograde';
    % yyyyH = 2022;  mmH = 8;  ddH = 25;
    Planet_1 = 'Earth';
    Planet_2 = 'Mars';
    columnIndex =1;

%% 0.5 Specify the URL and text file name
    url1 = 'https://ssd.jpl.nasa.gov/api/horizons.api?format=text&COMMAND=%27399%27&OBJ_DATA=%27NO%27&MAKE_EPHEM=%27YES%27&EPHEM_TYPE=%27VECTORS%27&CENTER=%27500@0%27&START_TIME=%272022-08-25%27&STOP_TIME=%272022-08-26%27&STEP_SIZE=%271%20d%27&QUANTITIES=%272%27';
    url2 = 'https://ssd.jpl.nasa.gov/api/horizons.api?format=text&COMMAND=%27399%27&OBJ_DATA=%27NO%27&MAKE_EPHEM=%27YES%27&EPHEM_TYPE=%27VECTORS%27&CENTER=%27500@0%27&START_TIME=%272037-12-31%27&STOP_TIME=%272039-07-08%27&STEP_SIZE=%271%20d%27&QUANTITIES=%272%27';
    url3 = 'https://ssd.jpl.nasa.gov/api/horizons.api?format=text&COMMAND=%27299%27&OBJ_DATA=%27NO%27&MAKE_EPHEM=%27YES%27&EPHEM_TYPE=%27VECTORS%27&CENTER=%27500@0%27&START_TIME=%272038-05-26%27&STOP_TIME=%272039-12-01%27&STEP_SIZE=%271%20d%27&QUANTITIES=%272%27';
    
    textFileName1 = 'Earth_t1.txt';
    textFileName2 = '01012022_12322022-Earth.txt';
    textFileName3 = '12312022_03012022-Mars.txt';
    
    Orbital.jplHorizonRequest(url1, textFileName1);
    Orbital.jplHorizonRequest(url2, textFileName2);
    Orbital.jplHorizonRequest(url3, textFileName3);
    
%% 1.0 Parsing the Requested Text Files
    [outputStatesEarth] = Orbital.parseJPLHorizonStates(textFileName2);
    [outputStatesMars] = Orbital.parseJPLHorizonStates(textFileName3);
%% 1.1 Initialize matrices for speed
    % Calculate the number of iterations and round up to the nearest integer
    numIterationsEarth = ceil(length(outputStatesEarth) / dt);
    numIterationsMars = ceil(length(outputStatesMars) / dt);
    
    % Initialize matrices to store C3_dep and C3_arr values using indices
    C3_dep_matrix = zeros(numIterationsEarth, numIterationsMars);
    C3_arr_matrix = zeros(numIterationsEarth, numIterationsMars);
    
    % Initialize matrices to store t1 and t2 values using indices
    t1_matrix = zeros(numIterationsEarth, 1);
    t2_matrix = zeros(1, numIterationsMars);

%% 2.0 Main loop with Lambert's Problem
    %% 2.1 Retrieves the Julian date and states of Earth from URL stored in text file
        for i = 1:dt:length(outputStatesEarth)
            t1 = (outputStatesEarth(i, 1));
            t1_matrix(ceil(i / dt)) = t1;
            X = outputStatesEarth(i,2);
            Y = outputStatesEarth(i,3);
            Z = outputStatesEarth(i,4);
            VX = outputStatesEarth(i,5);
            VY = outputStatesEarth(i,6);
            VZ = outputStatesEarth(i,7);
            
            r1_vec = [X, Y, Z]';
            v1_vec = [VX, VY, VZ]';
        
            rowIndex = ceil(i / dt);
      %% 2.2 Retrieves the Julian date and states of Mars from URL stored in text file  
            for j = 1:dt:length(outputStatesMars)
                t2 = (outputStatesMars(j, 1));
                t2_matrix(ceil(j / dt)) = t2;
                X = outputStatesMars(j,2);
                Y = outputStatesMars(j,3);
                Z = outputStatesMars(j,4);
                VX = outputStatesMars(j,5);
                VY = outputStatesMars(j,6);
                VZ = outputStatesMars(j,7);
                
                r2_vec = [X, Y, Z]';
                v2_vec = [VX, VY, VZ]';
        
                [vinf_dep_vec, vinf_arr_vec] = Orbital.lambertFit(r1_vec, v1_vec, r2_vec, v2_vec, TOF, Orbit);
            %% 2.2.1 Calculatesd the C3s
                C3_dep = (norm(vinf_dep_vec))^2;
                C3_arr = (norm(vinf_arr_vec))^2;
                
                % Store C3_dep and C3_arr values using indices
                columnIndex = ceil(j / dt);
                C3_dep_matrix(rowIndex, columnIndex) = C3_dep;
                C3_arr_matrix(rowIndex, columnIndex) = C3_arr;
            end
        end

%% 3.0 Porkchop Plots

%% 3.1 C3 Departure Porkchop Plot
% Define contour levels for C3_dep_matrix

[T2, T1] = meshgrid(t2_matrix, t1_matrix);

% Calculate TOF matrix (difference between arrival and departure times)
TOF_matrix = T2 - T1;

% Convert Julian dates to 'mm/dd/yyyy' format
departureDates = datestr(datetime(t1_matrix(:), 'ConvertFrom', 'juliandate'), 'mm/dd/yyyy');
arrivalDates = datestr(datetime(t2_matrix(:), 'ConvertFrom', 'juliandate'), 'mm/dd/yyyy');

% Create meshgrid using converted dates
[T2, T1] = meshgrid(datenum(arrivalDates), datenum(departureDates));

C3_levels = 5:5:400;
cmap = jet(length(C3_levels));

% Plot Porkchop plot with black TOF_matrix contours
figure('Position', [200, 200, 800, 600]);
[C, h] = contour(T1, T2, TOF_matrix, 'k'); % TOF contour in black

hold on

% Plot C3_dep_matrix contours with specified colors
[C2, h2] = contour(T1, T2, C3_dep_matrix, C3_levels);

% Color the C3_dep_matrix contours using the specified colormap
colormap(cmap);

% Label TOF values directly on the C3_dep_matrix contours
clabel(C, h, 'FontSize', 10, 'Color', 'red');

% Find indices of the minimum C3 and maximum TOF combination
[minC3, minTOF] = min(C3_dep_matrix(:));
[minTOF_row, minTOF_col] = ind2sub(size(C3_dep_matrix), minTOF);

% Find indices of the maximum C3 within the specified range and minimum TOF combination
[maxC3InRange, maxTOF] = max(C3_dep_matrix(:));
[maxTOF_row, maxTOF_col] = ind2sub(size(C3_dep_matrix), maxTOF);

% Labels and title
caxis([min(C3_levels), max(C3_levels)]);
colorbar;
xlabel('Departure Date (Earth)', 'FontSize', 12);
ylabel('Arrival Date (Mars)', 'FontSize', 12);
title('C3 Departure Contour Plot', 'FontSize', 14);
set(gca, 'FontSize', 10);

datetick('x', 'mm/dd/yyyy');
datetick('y', 'mm/dd/yyyy');

% Limits to remove white space
xlim([min(T1(:)), max(T1(:))]);
ylim([min(T2(:)), max(T2(:))]);

% Plot a marker at the point with the lowest C3 and highest TOF combination
plot(T1(minTOF_row, minTOF_col), T2(minTOF_row, minTOF_col), 'ro', 'MarkerSize', 10, 'MarkerFaceColor', 'red');

% Display values of the combinations as text annotations
text(T1(minTOF_row, minTOF_col), T2(minTOF_row, minTOF_col), ...
    sprintf('Min C3\n%.2f\nTOF: %.2f', minC3, TOF_matrix(minTOF_row, minTOF_col)), 'HorizontalAlignment', 'left', 'VerticalAlignment', 'bottom', 'FontSize', 10, 'Color', 'red');

% Check if the maximum C3 value found is within the specified range
if maxC3InRange <= max(C3_levels)
    % Plot a marker at the point with the highest C3 within the specified range and lowest TOF combination
    plot(T1(maxTOF_row, maxTOF_col), T2(maxTOF_row, maxTOF_col), 'go', 'MarkerSize', 10, 'MarkerFaceColor', 'green');
    % Display the maximum C3 value within the specified range
    text(T1(maxTOF_row, maxTOF_col), T2(maxTOF_row, maxTOF_col), ...
        sprintf('Max C3\n%.2f\nTOF: %.2f', maxC3InRange, TOF_matrix(maxTOF_row, maxTOF_col)), 'HorizontalAlignment', 'right', 'VerticalAlignment', 'top', 'FontSize', 10, 'Color', 'green');
end

%%
% Find indices of the specific C3_dep and C3_arr values
[C3_dep_row, C3_dep_col] = find(C3_dep_matrix == C3_dep);
[C3_arr_row, C3_arr_col] = find(C3_arr_matrix == C3_arr);

% Plot markers at the specific C3_dep and C3_arr points
plot(T1(C3_dep_row, C3_dep_col), T2(C3_dep_row, C3_dep_col), 'mo', 'MarkerSize', 10, 'MarkerFaceColor', 'm');
plot(T1(C3_arr_row, C3_arr_col), T2(C3_arr_row, C3_arr_col), 'co', 'MarkerSize', 10, 'MarkerFaceColor', 'c');

% Display values of the specific C3_dep and C3_arr points as text annotations
text(T1(C3_dep_row, C3_dep_col), T2(C3_dep_row, C3_dep_col), ...
    sprintf('C3_{dep}=%.2f\nTOF=%.2f', C3_dep, TOF_matrix(C3_dep_row, C3_dep_col)), 'HorizontalAlignment', 'left', 'VerticalAlignment', 'bottom', 'FontSize', 10, 'Color', 'm');
text(T1(C3_arr_row, C3_arr_col), T2(C3_arr_row, C3_arr_col), ...
    sprintf('C3_{arr}=%.2f\nTOF=%.2f', C3_arr, TOF_matrix(C3_arr_row, C3_arr_col)), 'HorizontalAlignment', 'left', 'VerticalAlignment', 'bottom', 'FontSize', 10, 'Color', 'c');

%%
grid on
hold off

%% 3.2 C3 Arrival Porkchop 
% Define contour levels for C3_dep_matrix
C3_levels = 5:5:400;
cmap = jet(length(C3_levels));

% Plot Porkchop plot with black TOF_matrix contours
figure('Position', [200, 200, 800, 600]);
[C, h] = contour(T1, T2, TOF_matrix, 'k'); % TOF contour in black

hold on

% Plot C3_arr_matrix contours with specified colors
[C2, h2] = contour(T1, T2, C3_arr_matrix, C3_levels);

% Color the C3_arr_matrix contours using the specified colormap
colormap(cmap);

% Label TOF values directly on the C3_arr_matrix contours
clabel(C, h, 'FontSize', 10, 'Color', 'red');

% Find indices of the minimum C3 and maximum TOF combination
[minC3, minTOF] = min(C3_arr_matrix(:));
[minTOF_row, minTOF_col] = ind2sub(size(C3_arr_matrix), minTOF);

% Find indices of the maximum C3 within the specified range and minimum TOF combination
[maxC3InRange, maxTOF] = max(C3_arr_matrix(:));
[maxTOF_row, maxTOF_col] = ind2sub(size(C3_arr_matrix), maxTOF);

% Labels and title
caxis([min(C3_levels), max(C3_levels)]);
colorbar;
xlabel('Departure Date (Earth)', 'FontSize', 12);
ylabel('Arrival Date (Mars)', 'FontSize', 12);
title('C3 Departure Contour Plot', 'FontSize', 14);
set(gca, 'FontSize', 10);

datetick('x', 'mm/dd/yyyy');
datetick('y', 'mm/dd/yyyy');

% Limits to remove white space
xlim([min(T1(:)), max(T1(:))]);
ylim([min(T2(:)), max(T2(:))]);

% Plot a marker at the point with the lowest C3 and highest TOF combination
plot(T1(minTOF_row, minTOF_col), T2(minTOF_row, minTOF_col), 'ro', 'MarkerSize', 10, 'MarkerFaceColor', 'red');

% Display values of the combinations as text annotations
text(T1(minTOF_row, minTOF_col), T2(minTOF_row, minTOF_col), ...
    sprintf('Min C3\n%.2f\nTOF: %.2f', minC3, TOF_matrix(minTOF_row, minTOF_col)), 'HorizontalAlignment', 'left', 'VerticalAlignment', 'bottom', 'FontSize', 10, 'Color', 'red');

% Check if the maximum C3 value found is within the specified range
if maxC3InRange <= max(C3_levels)
    % Plot a marker at the point with the highest C3 within the specified range and lowest TOF combination
    plot(T1(maxTOF_row, maxTOF_col), T2(maxTOF_row, maxTOF_col), 'go', 'MarkerSize', 10, 'MarkerFaceColor', 'green');
    % Display the maximum C3 value within the specified range
    text(T1(maxTOF_row, maxTOF_col), T2(maxTOF_row, maxTOF_col), ...
        sprintf('Max C3\n%.2f\nTOF: %.2f', maxC3InRange, TOF_matrix(maxTOF_row, maxTOF_col)), 'HorizontalAlignment', 'right', 'VerticalAlignment', 'top', 'FontSize', 10, 'Color', 'green');
end

grid on
hold off
%%
%%
% Define contour levels for C3_arr_matrix
C3_levels = 5:5:400;
cmap = jet(length(C3_levels));

% Specific C3_arr and TOF values
C3_dep = 20.1207;
specific_TOF = 320;

% Plot Porkchop plot with black TOF_matrix contours
figure('Position', [200, 200, 800, 600]);
[C, h] = contour(T1, T2, TOF_matrix, 'k'); % TOF contour in black

hold on

% Plot C3_dep_matrix contours with specified colors
[C2, h2] = contour(T1, T2, C3_dep_matrix, C3_levels);

% Color the C3_dep_matrix contours using the specified colormap
colormap(cmap);

% Label TOF values directly on the C3_dep_matrix contours
clabel(C, h, 'FontSize', 10, 'Color', 'red');

% Find indices of the closest values to specific C3_arr and TOF
[~, closest_C3_index] = min(abs(C3_dep_matrix(:) - C3_dep));
[~, closest_TOF_index] = min(abs(TOF_matrix(:) - specific_TOF));

% Labels and title
caxis([min(C3_levels), max(C3_levels)]);
colorbar;
xlabel('Departure Date (Earth)', 'FontSize', 12);
ylabel('Arrival Date (Mars)', 'FontSize', 12);
title('C3 Arrival Contour Plot', 'FontSize', 14);
set(gca, 'FontSize', 10);

datetick('x', 'mm/dd/yyyy');
datetick('y', 'mm/dd/yyyy');

% Limits to remove white space
xlim([min(T1(:)), max(T1(:))]);
ylim([min(T2(:)), max(T2(:))]);

% Plot marker at the closest point to specified C3_arr and TOF
plot(T1(closest_C3_index), T2(closest_C3_index), 'mo', 'MarkerSize', 10, 'MarkerFaceColor', 'r');

% Display values of the closest points as text annotations
text(T1(closest_C3_index), T2(closest_C3_index), ...
    sprintf('C3_{arr}=%.2f\n TOF = %.2f', C3_dep_matrix(closest_C3_index), TOF_matrix(closest_C3_index)), ...
    'HorizontalAlignment', 'left', 'VerticalAlignment', 'bottom', 'FontSize', 10, 'Color', 'r');

grid on
hold off


%%
% Define contour levels for C3_arr_matrix
C3_levels = 5:5:400;
cmap = jet(length(C3_levels));

% Specific C3_arr and TOF values
C3_dep = 7.5497;
specific_TOF = 320;

% Plot Porkchop plot with black TOF_matrix contours
figure('Position', [200, 200, 800, 600]);
[C, h] = contour(T1, T2, TOF_matrix, 'k'); % TOF contour in black

hold on

% Plot C3_arr_matrix contours with specified colors
[C2, h2] = contour(T1, T2, C3_arr_matrix, C3_levels);

% Color the C3_arr_matrix contours using the specified colormap
colormap(cmap);

% Label TOF values directly on the C3_arr_matrix contours
clabel(C, h, 'FontSize', 10, 'Color', 'red');

% Find indices of the closest values to specific C3_arr and TOF
[~, closest_C3_index] = min(abs(C3_arr_matrix(:) - C3_dep));
[~, closest_TOF_index] = min(abs(TOF_matrix(:) - specific_TOF));

% Labels and title
caxis([min(C3_levels), max(C3_levels)]);
colorbar;
xlabel('Departure Date (Earth)', 'FontSize', 12);
ylabel('Arrival Date (Mars)', 'FontSize', 12);
title('C3 Arrival Contour Plot', 'FontSize', 14);
set(gca, 'FontSize', 10);

datetick('x', 'mm/dd/yyyy');
datetick('y', 'mm/dd/yyyy');

% Limits to remove white space
xlim([min(T1(:)), max(T1(:))]);
ylim([min(T2(:)), max(T2(:))]);

% Plot marker at the closest point to specified C3_arr and TOF
plot(T1(closest_C3_index), T2(closest_C3_index), 'mo', 'MarkerSize', 10, 'MarkerFaceColor', 'r');

% Display values of the closest points as text annotations
text(T1(closest_C3_index), T2(closest_C3_index), ...
    sprintf('C3_{arr}=%.2f\n TOF = %.2f', C3_arr_matrix(closest_C3_index), TOF_matrix(closest_C3_index)), ...
    'HorizontalAlignment', 'left', 'VerticalAlignment', 'bottom', 'FontSize', 10, 'Color', 'r');

grid on
hold off


%%
toc
%%%%%%%%%%%%%%%%%%%%%%% NON-EDITABLE %%%%%%%%%%%%%%%%%%%%%%