clc;clear;close all
tic
% -------------MATLAB Script Information-------------
% Author Name: V^3
% Date: 11/4/23
% Tool Version: R2022a
% Purpose of Script: Nonlinear vs Linear System
% other .m files required: None
% other files required (not .m): None
%{

Function inputs:
func_nonLinear: Initial angular velocity components

Example Output:
3x3 plot contiaining the following specified by the requested Part (i.e A or B):
Angular velocity components over time (linearized)
Angular velocity components over time (linearized vs nonlinearized)
%}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   EDITABLE   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nonLinear = '1'; %Select if you would like to see the non-linear plot onto the linear plot (i.e 0 for off, 1 for on)
Case = 'a'; %Display which case you would like to see (i.e a or b)

t0 = 0;
tf = 20;
tSpan = [t0:0.1:tf];
n_d = 100; %[deg/s]
n_r = n_d * (pi/180); %[rad/s]
eplison_x_d_0 = 5; %[deg/s]
eplison_y_d_0 = 1; %[deg/s]
eplison_z_d_0 = 3; %[deg/s]

eplison_x_r_0 = eplison_x_d_0 * (pi/180); %[rad/s]
eplison_y_r_0 = eplison_y_d_0 * (pi/180); %[rad/s]
eplison_z_r_0 = eplison_z_d_0 * (pi/180); %[rad/s]

p0=0;
q0=0;
r0=n_r;

if Case == 'a'
    Ixx = 17.8; %[kgm^2]
    Iyy = 14.6; %[kgm^2]
    Izz = 6.8;  %[kgm^2]

    p0_ds=5;
    q0_ds=1;
    r0_ds=103;
end

if Case == 'b'
    Ixx = 17.8; %[kgm^2]
    Iyy = 6.8; %[kgm^2]
    Izz = 14.6;  %[kgm^2]

    p0_ds=0;
    q0_ds=0;
    r0_ds=103;
end

 Ixx = 44451.50; %[kg*m^2]
Iyy = 45161.45; %[kg*m^2]
Izz = 44017.00; %[kg*m^2]

Ixy = 378.08; %[kg*m^2]
Ixz = 108.92; %[kg*m^2]
Iyz = -3273.58; %[kg*m^2]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   EDITABLE   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DO NOT EDIT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
p0_rs=p0_ds*(pi/180);
q0_rs=q0_ds*(pi/180);
r0_rs=r0_ds*(pi/180);
x_0 = [p0_rs;q0_rs;r0_rs];

sigma_x = (Izz-Iyy)/Ixx;
sigma_y = (Ixx-Izz)/Iyy;
sigma_z = (Iyy-Ixx)/Izz;
interiaVector = [Ixx;Iyy;Izz];

if sigma_x*sigma_y>0
    for i = 1:length(tSpan)
        eplison_x(i) = 1/2 * (eplison_x_r_0 - eplison_y_r_0 * sign(n_r*sigma_x) * sqrt(sigma_x/sigma_y))*((exp(1))^(sqrt(n_r^2*sigma_x * sigma_y) * tSpan(i))) + 1/2 * (eplison_x_r_0 + eplison_y_r_0 * sign(n_r*sigma_x) * sqrt(sigma_x/sigma_y))*((exp(1))^(-sqrt(n_r^2*sigma_x * sigma_y) * tSpan(i)));
        eplsion_y(i) = 1/2 * (eplison_y_r_0 - eplison_x_r_0 * sign(n_r*sigma_y) * sqrt(sigma_y/sigma_x))*((exp(1))^(sqrt(n_r^2*sigma_y * sigma_x) * tSpan(i))) + 1/2 * (eplison_y_r_0 + eplison_x_r_0 * sign(n_r*sigma_y) * sqrt(sigma_y/sigma_x))*((exp(1))^(-sqrt(n_r^2*sigma_x * sigma_y) * tSpan(i)));
        eplsion_z(i) = eplison_z_r_0;

        p_t=eplison_x+p0;
        q_t=eplsion_y+q0;
        r_t=eplsion_z+r0;
    end
    disp("Case 1")
end

if sigma_x*sigma_y<0
    for i = 1:length(tSpan)
        eplison_x(i) = eplison_x_r_0 * cos(sqrt(-1*(n_r^2)*sigma_x*sigma_y) * tSpan(i)) - eplison_y_r_0*sign(n_r*sigma_x)*sqrt(-sigma_x/sigma_y)*sin(sqrt(-1*(n_r)^2*sigma_x*sigma_y)*tSpan(i));
        eplsion_y(i) = eplison_y_r_0 * cos(sqrt(-1*(n_r^2)*sigma_x*sigma_y) * tSpan(i)) - eplison_x_r_0*sign(n_r*sigma_y)*sqrt(-sigma_y/sigma_x)*sin(sqrt(-1*(n_r)^2*sigma_x*sigma_y)*tSpan(i));
        eplsion_z(i) = eplison_z_r_0;

        p_t=eplison_x+p0;
        q_t=eplsion_y+q0;
        r_t=eplsion_z+r0;
    end
    disp("Case 3")
end
% 
% figure(1)
% plot(tSpan, p_t)
% figure(2)
% plot(tSpan, q_t)
% figure(3)
% plot(tSpan, r_t)



%% PLOTS: Quantities vs. Time   
set(gcf,'units','normalized','outerposition',[0 0 1 1])
l = tiledlayout(3,1,'TileSpacing','Compact','Padding','Compact');
title (l, convertStringsToChars("Homework 7: Linearized Response Case " + convertStringsToChars(Case)))
% Quaternions
nexttile(1),hold on,grid on
    plot(tSpan,p_t* 180/pi,'linewidth',2,'Color',[0,0.6,1])
    xlabel('t'); ylabel('p(t)'); 
nexttile(2),hold on,grid on
    plot(tSpan,q_t* 180/pi,'linewidth',2,'Color',[1,0.6,0])
    xlabel('t'); ylabel('q(t)')
nexttile(3),hold on,grid on
    plot(tSpan,r_t* 180/pi,'linewidth',2,'Color',[0,0.8,0])
    xlabel('t'); ylabel('r(t)')

if nonLinear == '1'
    options = odeset('RelTol',1E-7,'AbsTol',1E-9); % Tolerances 
    [t, X] = ode45(@(tSpan, X, interiaVector)func_nonLinear(tSpan, X, interiaVector),tSpan,x_0, options,interiaVector);
%     [thetaSpan,X, te, ye, ie] = ode45(@(thetaSpan,X,gamma)func_taylorMaccoll(thetaSpan,X,gamma),thetaSpan,vInitials,options,gamma);
    
    %% Nonlinear vs Linear
    set(gcf,'units','normalized','outerposition',[0 0 1 1])
    g = tiledlayout(3,1,'TileSpacing','Compact','Padding','Compact');
    title (g, convertStringsToChars("Homework 7: Linearized vs Non-Linear Response Case " + convertStringsToChars(Case)))
 
    % Quaternions
    nexttile(1),hold on,grid on
        plot(tSpan,p_t * 180/pi,'linewidth',2,'Color',[0,0.6,1])
        hold on
        plot(tSpan,X(:,1)* 180/pi,'linewidth',2,'Color',[1,0.6,1])
        hold off
        xlabel('t'); ylabel('pt(t)'); 
    nexttile(2),hold on,grid on
        plot(tSpan,q_t * 180/pi,'linewidth',2,'Color',[1,0.6,0])
        xlabel('t'); ylabel('q(t)')
        hold on
        plot(tSpan,X(:,2)* 180/pi,'linewidth',2,'Color',[0.8,0.6,1])
        hold off
    nexttile(3),hold on,grid on
        plot(tSpan,r_t * 180/pi,'linewidth',2,'Color',[0,0.8,0])
        xlabel('t'); ylabel('r(t)')
        hold on
        plot(tSpan,X(:,3)* 180/pi,'linewidth',2,'Color',[0.2,0.6,1])
        hold off
end

toc
%% Non-linear
function x_dot = func_nonLinear(~, xVec, interiaVector)
    Ixx = interiaVector(1);
    Iyy = interiaVector(2);
    Izz = interiaVector(3);
%     Ixx = 17.8; % [kgm^2]
%     Iyy = 14.6; % [kgm^2]
%     Izz = 6.8;  % [kgm^2]

%     Ixx = 17.8; %[kgm^2]
%     Iyy = 6.8; %[kgm^2]
%     Izz = 14.6;  %[kgm^2]

    p = xVec(1);
    q = xVec(2);
    r = xVec(3);

    p_dot = ((Iyy - Izz) / Ixx) * q * r;
    q_dot = ((Izz - Ixx) / Iyy) * r * p;
    r_dot = ((Ixx - Iyy) / Izz) * p * q;

    x_dot = [p_dot; q_dot; r_dot];
end
