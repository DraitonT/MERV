clc;clear;close all
tic
% -------------MATLAB Script Information-------------
% Author Names: Michael Quach
% Date: 9/1/23
% Tool Version: R2022a
% Purpose of Script: Homework 1 for ARO 5090 for Numerical vs Analytical
% other .m files required: None
% other files required (not .m): None

%%%%%%%%%%%%%%%%%%%%%%%%% EDITABLE %%%%%%%%%%%%%%%%%%%%%%%%%

%% 0.0  Initial conditions
    r = [6452, 1000, 0]; % Initial position vector in kilometers
    mu = 324859; % Standard gravitational parameter in km^3/s^2
    
    V_c = 1.3 * sqrt(mu / norm(r)); % Velocity of circular orbit in km/s
    v_x = V_c/20;
    v_y = V_c;
    v_z = V_c/4;
    
    numOrbits = 1;          % Specified number of orbits

    optionsCorrections = false; % Boolean to allow for corrections to ode45
    analyticalVsNumericalPlot = true;

    orbitsEarthFileName = 'orbitsAroundEarth.png'; % Name of file for problem 1
    analyticalVsNumericalRFileName = 'analytical_r_vs_numerical_r.png'; % Name of file for problem 2

%%%%%%%%%%%%%%%%%%%%%%%%% EDITABLE %%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%% NON-EDITABLE %%%%%%%%%%%%%%%%%%%%%%%

    earthTextureImage = append(pwd, '\..\data\Textures\earth_texture.png');
    v = [v_x, v_y, v_z]; % Initial velocity vector in km/s
    % Combine initial position and velocity into the state vector
    initialState = [r, v]; 

%% 1.0 Integrating 2nd Order ODE
    rp = r(1);
    ra = r(1);
    
    a = (rp + ra)/2;               % Semimajor Axis [km] (2.71, pg. 81 | Curtis)
    T = (2*pi/sqrt(mu)) * a^(3/2) * 5; % Orbital Period [seconds] (2.83, pg. 84 | Curtis)

    % Define the time span for integration (e.g., 0 to 2 hours)
    tspan = [0, T * numOrbits]; % Integration time in seconds (2 hours)
    
    % Use ode45 to integrate the equations of motion
    if optionsCorrections
        options = odeset('RelTol', 1e-6, 'AbsTol', 1e-9);
        [t, state] = ode45(@(t, state) twqBodyEOM(t, state), tspan, initialState, options);
        correction = "Corrected";
    else
        [t, state] = ode45(@(t, state) twqBodyEOM(t, state), tspan, initialState);
        correction = "Not Corrected";
    end

    % Extract the position vectors from the state variable
    x = state(:, 1);
    y = state(:, 2);
    z = state(:, 3);
    
    % Plot the satellite's trajectory
    plot3(x, y, z, 'r');
    xlabel('X (km)');
    ylabel('Y (km)');
    zlabel('Z (km)');
    title('Satellite Circular Orbit');
    grid on;
    axis equal;
    
    hold on
    
    % Define the radius of the Earth (in kilometers)
    earthRadius = 6052; % Approximate radius of the Earth
    
    % Create a sphere for Earth
    [xEarth, yEarth, zEarth] = sphere;
    xEarth = xEarth * earthRadius;
    yEarth = yEarth * earthRadius;
    zEarth = zEarth * earthRadius;

    earthTexture = imread(earthTextureImage); % Load your own Earth texture image
    earthTexture = flipud(earthTexture);

    % Plot the Earth as a sphere at the origin
    surf(xEarth, yEarth, zEarth, 'CData', earthTexture, 'FaceColor', 'texturemap', 'EdgeColor', 'none');
    axis equal;
    xlabel('X (km)');
    ylabel('Y (km)');
    zlabel('Z (km)');
    grid on
    title(sprintf('Problem 1 Plot: Orbit Visualization (%s)', correction));
    saveas(gcf, (append(pwd, '\', orbitsEarthFileName)));
    hold off

%% 2.0 Problem 2 (Analytical Radius vs Numerical Radius)
    h = sqrt(2*mu) * sqrt((ra*rp)/(ra+rp));

    r_analytical = ((norm(h))^2/mu); % Analytical radius [km] (2.62, pg. 76 | Curtis)
    for i = 1: length(t)
        r_analyticalArray(i) = r_analytical;
        r_numerical(i) = sqrt((x(i))^2 + (y(i))^2 + (z(i))^2);
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

%% 3.0 Problem 3
    e = -((rp/a) - 1);                      % Eccentricity [unitless](2.73, pg. 82 | Curtis)
    p = h^2/mu;                             % Semiparameter [kg^2*km] (2.53, pg. 74 | Curtis)
    a = (rp + ra)/2;                        % Semi-major axis [km] (2.71, pg. 81 | Curtis)
    V_c = sqrt(mu / norm(r));
    T_c = (2*pi/sqrt(mu)) * a^(3/2);        % Orbital Period [seconds] (2.83, pg. 84 | Curtis)
    epsilon = -mu/(2*a);                    % Specific Energy [km^2/s^2] (2.80, pg. 83) | Curtis)
    h = sqrt(2*mu) * sqrt((ra*rp)/(ra+rp)); % Angular momentum [kg*km^2/s] (6.2, pg. 290 | Curtis)
    n = (2 * pi)/ T_c;                      % Mean motion [rad/s] (3.9, pg. 144 | Curtis)

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
    fprintf('Velocity:       V_c = %.5e km/s  \n', V_c);
    fprintf('-----------------------------\n');
    
    fprintf('Part E\n');
    fprintf('-----------------------------\n');
    fprintf('Period:         T_c = %.5e seconds  \n', T_c);
    fprintf('-----------------------------\n');
    
    fprintf('Part F\n');
    fprintf('-----------------------------\n');
    fprintf('Specific Mechanical Energy: epsilon = %.5e km^2/s^2  \n', epsilon);
    fprintf('-----------------------------\n');
    
    fprintf('Part G\n');
    fprintf('-----------------------------\n');
    fprintf('Angular Momentum: h = %.5e kg*km^2/s  \n', h);
    fprintf('-----------------------------\n');
    
    fprintf('Part H\n');
    fprintf('-----------------------------\n');
    fprintf('Mean Motion: n = %.5e  rad/s\n', n);
    fprintf('-----------------------------\n');



%%%%%%%%%%%%%%%%%%%%%%% NON-EDITABLE %%%%%%%%%%%%%%%%%%%%%%%

[rowStateVenus, ~] = size(state);

for i = 1:max(rowStateVenus)
    r_1 = state(i,1);
    r_2 = state(i,2);
    r_3 = state(i,3);
    v_1 = state(i,4);
    v_2 = state(i,5);
    v_3 = state(i,6);

    r_vec(i,:) = [r_1, r_2, r_3];
    v_vec(i,:) = [v_1, v_2, v_3];
    
    [a(i,:), e(i,:), TA(i,:), RAAN(i,:), AOP(i,:), inc(i,:), h_vec(i,:)] = rv2coe(r_vec(i,:), v_vec(i,:));
end
toc

% Function defining the differential equations
function dydt = twqBodyEOM(~, state)
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
