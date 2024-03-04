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
TOF = 200; % [days] Time of Flight
numOrbits = 1;

% 2.206137794001645E7 Y =-1.513068137493977E8 Z = 2.937558597297221E4
% 2.899097886494714E1 VY= 4.295936298511452E0 VZ=-2.831381940802125E-4

earthR = [2.206137794001645E7, -1.513068137493977E8, 2.937558597297221E4];
earthV = [2.899097886494714E1, 4.295936298511452E0, -2.831381940802125E-4];


% X = 9.090695194550528E7 Y = 5.712879960773074E7 Z =-4.452148913925059E6
%  VX=-1.892000813643908E1 VY= 2.937495866605382E1 VZ= 1.496477507020145E0

venusR =[9.090695194550528E7, 5.712879960773074E7, -4.452148913925059E6];
venusV = [-1.892000813643908E1, 2.937495866605382E1, 1.496477507020145E0];

%% 0.1 Trajectory Launch Date and Targets
yyyy = 2035;  mm = 7;  dd = 1;
Planet_1 = 'Earth';
Planet_2 = 'Venus';

%% 0.2 Orbit Settings
Orbit = 'Prograde';
blue = 'b'; %Earth
green = 'g'; % Transfer Orbit
red = 'r'; % venus

%% 0.3 Numerical Methods Settings
optionsCorrections = true;
toleranceMin = 1e-6;
toleranceMax = 1e-9;
Options = optimoptions('fsolve','Display','off','TolFun',1E-12,'TolX',1E-12);

%% 0.4 Plot Settings for Trajectory Plotter
nominalPlots = true;
multipleOrbits = true; % True to see Multiple Orbits, False to see Velocity comparsion

%%%%%%%%%%%%%%%%%%%%%%%%% EDITABLE %%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%% NON-EDITABLE %%%%%%%%%%%%%%%%%%%%%%%

TOF = TOF * 86400;
r1_vec = earthR;
r2_vec = venusR;

VP1_dep = earthV';
VP2_arr = venusV';

r1_mag = norm(r1_vec);
r2_mag = norm(r2_vec);

r1Crossr2 = cross(r1_vec,r2_vec);

%% 1.0 True Anomaly Calculation and A
if strcmp(Orbit, 'Prograde')
    if r1Crossr2(3) >= 0
        deltaTheta = acos((dot(r1_vec,r2_vec))/(r1_mag * r2_mag));
    else %r2Crossr2(3) < 0
        deltaTheta = 2* pi - acos((dot(r1_vec,r2_vec))/(r1_mag * r2_mag));
    end
else % Retrograde
    if r1Crossr2(3) < 0
        deltaTheta = acos((dot(r1_vec,r2_vec))/(r1_mag * r2_mag));
    else %r2Crossr2(3) < 0
        deltaTheta = 2* pi - acos((dot(r1_vec,r2_vec))/(r1_mag * r2_mag));
    end
end

A = (sin(deltaTheta)) * sqrt((r1_mag * r2_mag)/(1-cos(deltaTheta)));
%% 1.1 Newton's Raphson to find Z
r1_vec=r1_vec';
r1_mag = vecnorm(r1_vec);
r2_vec=r2_vec';
r2_mag = vecnorm(r2_vec);

% True Anomaly Difference
DTA_S = acos(dot(r1_vec,r2_vec)./(r1_mag.*r2_mag));
DTA_L = 2*pi-DTA_S;

A_S = sin(DTA_S).*sqrt(r1_mag.*r2_mag./(1-cos(DTA_S)));
A_L = sin(DTA_L).*sqrt(r1_mag.*r2_mag./(1-cos(DTA_L)));

z0_S =-100;
while Orbital.Fun(z0_S,TOF,mu,r1_mag,r2_mag,A_S) < 0
    z0_S = z0_S + 0.01;
end

z0_L = -100;
while Orbital.Fun(z0_L,TOF,mu,r1_mag,r2_mag,A_L) < 0
    z0_L = z0_L + 0.1;
end

z_S = fsolve(@(z_S)Orbital.Fun(z_S,TOF,mu,r1_mag,r2_mag,A_S),z0_S,Options);
z_L = fsolve(@(z_L)Orbital.Fun(z_L,TOF,mu,r1_mag,r2_mag,A_L),z0_L,Options);

y_S = r1_mag + r2_mag + A_S.*(z_S.*Orbital.stumpS(z_S) - 1)./sqrt(Orbital.stumpC(z_S));
y_L = r1_mag + r2_mag + A_L.*(z_L.*Orbital.stumpS(z_L) - 1)./sqrt(Orbital.stumpC(z_L));
%% 1.2 Coefficients for Lambert's Problem
f_S = 1 - y_S./r1_mag;
g_S = A_S.*sqrt(y_S/mu);
gdot_S = 1 - y_S./r2_mag;
v1_vec_S = (r2_vec - f_S.*r1_vec)./g_S;
v2_vec_S = 1./g_S.*(gdot_S.*r2_vec - r1_vec);

f_L = 1 - y_L./r1_mag;
g_L = A_L.*sqrt(y_L/mu);
gdot_L = 1 - y_L./r2_mag;
v1_vec_L = (r2_vec - f_L.*r1_vec)./g_L;
v2_vec_L = 1/g_L.*(gdot_L.*r2_vec - r1_vec);

fprintf('The short path velocities are the following: \n')
fprintf('v1_vec_S (%s): [%.5f,%.5f,%.5f] km/s\n ', Planet_1, v1_vec_S)
fprintf('v2_vec_S (%s): [%.5f,%.5f,%.5f] km/s\n ', Planet_2, v2_vec_S)


fprintf('The long path velocities are the following: \n')
fprintf('v1_vec_L (%s): [%.5f,%.5f,%.5f] km/s\n ', Planet_1, v1_vec_L)
fprintf('v2_vec_L (%s): [%.5f,%.5f,%.5f] km/s\n ', Planet_2, v2_vec_L)
%% 1.3 Julian Dates for Departure
JD_Dep = juliandate(yyyy,mm,dd);

Options = optimoptions('fsolve','Display','off','TolFun',1E-12,'TolX',1E-12);
JD_Arr = JD_Dep + TOF/86400; % Julian Day

Dep = datetime(JD_Dep,'convertfrom','juliandate');
yyyyD=year(Dep); mmD=month(Dep); ddD=day(Dep);
Arr = datetime(JD_Arr,'convertfrom','juliandate');
yyyyA=year(Arr); mmA=month(Arr); ddA=day(Arr);
%% 1.4 Solving for Delta V Required
% Departure
vinf_dep_vec_S = v1_vec_S - VP1_dep;
vinf_dep_S = vecnorm(vinf_dep_vec_S);
C3_dep_S = vinf_dep_S^2;

vinf_dep_vec_L = v1_vec_L - VP1_dep;
vinf_dep_L = vecnorm(vinf_dep_vec_L);
C3_dep_L = vinf_dep_L^2;

% Arrival
vinf_arr_vec_S = v2_vec_S-VP2_arr;
vinf_arr_S = vecnorm(vinf_arr_vec_S);
C3_arr_S = vinf_arr_S^2;

vinf_arr_vec_L = v2_vec_L-VP2_arr;
vinf_arr_L = vecnorm(vinf_arr_vec_L);
C3_arr_L = vinf_arr_L^2;

if isnan(C3_dep_S) && isnan(C3_dep_L)
    fprintf('\nTransfer physically unrealizable. ')
    fprintf('Select a different time of flight (TOF).\n')
    return
end
if C3_dep_S<C3_dep_L || isnan(C3_dep_L)
    orbit_choice = sprintf('              The SHORT-Way Orbit is the viable solution.\n');
    C3_dep = C3_dep_S;
    C3_arr = C3_arr_S;
    color_traj = [0,0.6,1];
    vinf_dep_vec = vinf_dep_vec_S;
    vinf_arr_vec = vinf_arr_vec_S;
    v1_vec = v1_vec_S;
    v2_vec = v2_vec_S;
elseif C3_dep_S>=C3_dep_L || isnan(C3_dep_S)
    orbit_choice = sprintf('              The  LONG-Way Orbit is the viable solution.\n');
    C3_dep = C3_dep_L;
    C3_arr = C3_arr_L;
    color_traj = [.9,0,.9];
    vinf_dep_vec = vinf_dep_vec_L;
    vinf_arr_vec = vinf_arr_vec_L;
    v1_vec = v1_vec_L;
    v2_vec = v2_vec_L;
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
initialConditionvenus = [venusR, venusV];
%% 1.8 Trajector Plotters
T_dep = TOF;
T_Venus = (2*pi/sqrt(mu)) * a_P2^(3/2);
T_Earth = (2*pi/sqrt(mu)) * a_P1^(3/2);

[t_Dep, stateDeparture,~] = Orbital.numericalTwoBody(initialConditionDeparture, T_dep/2, numOrbits, optionsCorrections, toleranceMin, toleranceMax);
Orbital.plot2Body(stateDeparture, green);

[t_Earth, stateEarth,~] = Orbital.numericalTwoBody(initialConditionEarth, T_Earth, numOrbits, optionsCorrections, toleranceMin, toleranceMax);
Orbital.plot2Body(stateEarth, blue);

[t_Venus, stateVenus,~] = Orbital.numericalTwoBody(initialConditionvenus, T_Venus, numOrbits, optionsCorrections, toleranceMin, toleranceMax);
Orbital.plot2Body(stateVenus, red);

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

%% 2.0 Print Statements
TOF = TOF/86400;
fprintf('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
fprintf('        Interplanetary %s-to-%s Orbit Characteristics:\n',Planet_1,Planet_2)
fprintf('\n%s', Orbit)
fprintf('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
fprintf('       Semi-Major Axis:    a  = %.3f \n',a )
fprintf('          Eccentricity:    e  = %.4f\n'   ,e)
fprintf('      Periapsis Radius:   rp = %.3f \n',abs(a*(1-e)) )
fprintf('        Apopsis Radius:   ra = %.3f \n',abs(a*(1+e)) )
fprintf('                Period:    T = %.0f days = %.1f years\n\n',2*pi/sqrt(mu)*a^1.5/86400,2*pi/sqrt(mu)*a^1.5/86400/365.25)

fprintf('R.A. of Ascending Node:   RAAN = %.1f  deg\n', RAAN * 180/pi)
fprintf('           Inclination:    i = %.1f  deg\n', i * 180/pi)
fprintf(' Argument of Periapsis:    AOP = %.1f  deg\n', AOP * 180/pi)

fprintf('~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ \n')

fprintf('True Anomaly of S/C at Departure:  TA_1 = %3.1f  deg\n'  ,TA1 * 180/pi)
fprintf('True Anomaly of S/C at   Arrival:  TA_2 = %3.1f  deg\n',TA2 * 180/pi)
fprintf('              Delta True Anomaly:         %3.1f  deg\n',mod((TA2 * 180/pi)-(TA1* 180/pi),360))
fprintf('                  Time of Flight:   TOF = %3.0f  days = %.1f  years\n\n',TOF,TOF/365.25)

fprintf('At Departure (%s):\n                    ',Planet_1);
fprintf('V(S/C) = [%.5f,%.5f,%.5f] km/s\n                    ',v1_vec)
fprintf('v_inf  = [%.5f,%.5f,%.5f] km/s\n              ',vinf_dep_vec)
fprintf('     |v_inf| = %.3f  km/s,\n                   C3 = %.1f km^2/s^2\n\n', sqrt(C3_dep),C3_dep)

fprintf('At Arrival (%s):\n                    ',Planet_2);
fprintf('V(S/C) = [%.5f,%.5f,%.5f] km/s\n                    ',v2_vec)
fprintf('v_inf  = [%.5f,%.5f,%.5f] km/s\n              ',vinf_arr_vec)
fprintf('     |v_inf| = %.3f  km/s,\n                   C3 = %.1f km^2/s^2\n\n', sqrt(C3_arr),C3_arr)

%% 2.0 Save the states of each stage in their own CSVs
% For Earth
earth_table = table(t_Earth, stateEarth(:,1), stateEarth(:,2), stateEarth(:,3), ...
    stateEarth(:,4), stateEarth(:,5), stateEarth(:,6), ...
    'VariableNames', {'Time', 'X', 'Y', 'Z', 'VX', 'VY', 'VZ'});
writetable(earth_table, append(pwd,'\..\data\Earth_States.csv'));

% For Venus
venus_table = table(t_Venus, stateVenus(:,1), stateVenus(:,2), stateVenus(:,3), ...
    stateVenus(:,4), stateVenus(:,5), stateVenus(:,6), ...
    'VariableNames', {'Time', 'X', 'Y', 'Z', 'VX', 'VY', 'VZ'});
writetable(venus_table, append(pwd,'\..\data\Venus_States.csv'));

% For the Transfer State
transfer_table = table(t_Dep, stateDeparture(:,1), stateDeparture(:,2), stateDeparture(:,3), ...
    stateDeparture(:,4), stateDeparture(:,5), stateDeparture(:,6), ...
    'VariableNames', {'Time', 'X', 'Y', 'Z', 'VX', 'VY', 'VZ'});
writetable(transfer_table, append(pwd,'\..\data\Transfer_States.csv'));

[rowStateVenus, ~] = size(stateVenus);

for i = 1:max(rowStateVenus)
    r_1 = stateVenus(i,1);
    r_2 = stateVenus(i,2);
    r_3 = stateVenus(i,3);
    v_1 = stateVenus(i,4);
    v_2 = stateVenus(i,5);
    v_3 = stateVenus(i,6);

    r_vec(i,:) = [r_1, r_2, r_3] - venusR;
    v_vec(i,:) = [v_1, v_2, v_3] - venusV;
    
    [a(i,:), e(i,:), TA(i,:), RAAN(i,:), AOP(i,:), inc(i,:), h_vec(i,:)] = rv2coe(r_vec(i,:), v_vec(i,:));

end

toc
%%%%%%%%%%%%%%%%%%%%%%% NON-EDITABLE %%%%%%%%%%%%%%%%%%%%%%%