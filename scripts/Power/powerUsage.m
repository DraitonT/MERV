clc, clear, close all
addpath(genpath(append(pwd,'\..\..\..\lib'))) %Adds TwinAnalyticsToolKit.m library to the working path
tic
% -------------MATLAB Script Information-------------
% Author Names: Michael Quach
% Date: 11/17/23
% Tool Version: R2023a
% Purpose of Script: Displays the Interpolated results from all combined
% runs of specific cut
% other .m files required: None
% other files required (not .m): Folder with the individual results

%%%%%%%%%%%%%%%%%%%%%%%%% EDITABLE %%%%%%%%%%%%%%%%%%%%%%%%%

% Define power consumption for subsystems in Watts (from the uploaded file)
subsystemPower.normalOperation = [3.15, 92.04, 217.65, 86, 159.60, 375]; % all subsystems active
subsystemPower.lowPowerMonitor = [3.15, 0, 0, 0, 97, 0]; % only Thermal Control and COMMs
subsystemPower.duplex = [0, 0, 0, 0, 73.09, 0]; % only COMMs
subsystemPower.shutdown = [0, 0, 0, 0, 0, 0]; % all subsystems inactive

% Define total energy and simulation parameters
totalEnergyWh = 16800 * 2; % Maximum available energy in Watt-hours
simulationTimeHours = 7 * 24; % Total simulation time in hours
timeStepHours = 1; % Time step for simulation in hours

%%%%%%%%%%%%%%%%%%%%%%%%% EDITABLE %%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%% NON-EDITABLE %%%%%%%%%%%%%%%%%%%%%%%
% Map the power modes to subsystem activation states
powerModeMapping = containers.Map({'normalOperation', 'lowPowerMonitor', 'duplex', 'shutdown'}, ...
                                  {'normalOperation', 'lowPowerMonitor', 'duplex', 'shutdown'});

numTimeSteps = simulationTimeHours / timeStepHours;

% Initialize simulation arrays
powerAvailableWh = totalEnergyWh; % Initialize available power
cumulativePowerNeededWh = zeros(1, numTimeSteps); % Initialize cumulative power needed array
modeChanges = strings(1, numTimeSteps); % Initialize mode changes array

% ... [previous definitions and initialization] ...

% Initial cumulative power needed calculation for the mission
initialPowerNeededWh = sum(subsystemPower.normalOperation) * simulationTimeHours;

% Determine initial mode based on power budget
if initialPowerNeededWh > totalEnergyWh
    % Start in low power monitor mode to save energy if we don't have enough
    currentMode = 'lowPowerMonitor';
else
    currentMode = 'normalOperation';
end

% Simulation loop
for step = 1:numTimeSteps
    % Check if we need to change modes to save power
    if powerAvailableWh <= 0
        currentMode = 'shutdown';
        % Once we enter shutdown mode, we stop consuming power
        activeSubsystemPower = subsystemPower.(currentMode);
    else
        % Get the active subsystems based on current power mode
        activeSubsystemPower = subsystemPower.(currentMode);
    end

    % Calculate power needed for this step
    powerNeededThisStep = sum(activeSubsystemPower) * timeStepHours;
    
    % Update cumulative power needed
    if step == 1
        cumulativePowerNeededWh(step) = powerNeededThisStep;
    else
        cumulativePowerNeededWh(step) = cumulativePowerNeededWh(step - 1);
        if powerAvailableWh > 0
            cumulativePowerNeededWh(step) = cumulativePowerNeededWh(step) + powerNeededThisStep;
        end
    end
    
    % Update available energy
    powerAvailableWh = max(powerAvailableWh - powerNeededThisStep, 0);

    % Record the current mode
    modeChanges(step) = currentMode;
end


% Plotting the results
figure;
stairs(1:numTimeSteps, totalEnergyWh - cumulativePowerNeededWh, 'b-', 'LineWidth', 2); % Stairs plot for the available power
hold on;
stairs(1:numTimeSteps, cumulativePowerNeededWh, 'r-', 'LineWidth', 2); % Stairs plot for the

% Plotting the results
figure;
hold on;

% Fill the area for power available
fill1_x = [1:numTimeSteps, fliplr(1:numTimeSteps)];
fill1_y = [totalEnergyWh - cumulativePowerNeededWh, zeros(1, numTimeSteps)];
fill(fill1_x, fill1_y, 'b', 'FaceAlpha', 0.1, 'EdgeColor', 'none');

% Fill the area for cumulative power needed
fill2_x = [1:numTimeSteps, fliplr(1:numTimeSteps)];
fill2_y = [cumulativePowerNeededWh, zeros(1, numTimeSteps)];
fill(fill2_x, fill2_y, 'r', 'FaceAlpha', 0.1, 'EdgeColor', 'none');

% Plot the stairs for power available and cumulative power needed on top of the filled area
stairs(1:numTimeSteps, totalEnergyWh - cumulativePowerNeededWh, 'b-', 'LineWidth', 2);
stairs(1:numTimeSteps, cumulativePowerNeededWh, 'r-', 'LineWidth', 2);

% Enhance plot with labels and title
xlabel('Time (hours)', 'FontSize', 12);
ylabel('Power (Wh)', 'FontSize', 12);
title('Power Available vs Cumulative Power Needed', 'FontSize', 14);

% Add grid lines for better readability
grid on;

% Add a legend to describe the plots
legend({'Power Available', 'Cumulative Power Needed'}, 'Location', 'best', 'FontSize', 10);

% Set axes properties for better visualization
xlim([0, simulationTimeHours]);
ylim([0, totalEnergyWh]);
set(gca, 'Box', 'on');

% % Annotations for mode changes
% for step = 1:numTimeSteps
%     if modeChanges(step) ~= ""
%         text(step, totalEnergyWh - cumulativePowerNeededWh(step), ...
%             ['\leftarrow ' char(modeChanges(step))], ...
%             'FontSize', 8, 'HorizontalAlignment', 'left', 'VerticalAlignment', 'middle', ...
%             'BackgroundColor', 'white', 'Margin', 1, 'EdgeColor', 'black');
%     end
% end

hold off;
toc
%%%%%%%%%%%%%%%%%%%%%%% NON-EDITABLE %%%%%%%%%%%%%%%%%%%%%%%

