clc;clear;close all
tic
% -------------MATLAB Script Information-------------
% Author Name: V^3
% Date: 11/4/23
% Tool Version: R2022a
% Purpose of Script: Single Spin System
% other .m files required: None
% other files required (not .m): None

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   EDITABLE   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Set up
% Initial Conditions

% Initial Euler Angles
psi_0   = 100;   %[deg] Yaw
theta_0 = 50;    %[deg] Pitch
phi_0   = 30;    %[deg] Roll
% Initial Angular Velocity in B-RF Coords
p_0_B = -1000;     %[deg/s]
q_0_B = 10;     %[deg/s]
r_0_B = 300;    %[deg/s] 

% Set Up Mass and Dimensions
m = 0.132;   %[kg] Mass
l = 5.6E-2;   %[m] Side Length

% Inertia Moments
Ixx = 1/6*m*(l^2); %[kg*m^2]
Iyy = 1/6*m*(l^2); %[kg*m^2]
Izz = 1/6*m*(l^2); %[kg*m^2]
% Inertia Products
Ixy = 0; %[kg*m^2]
Ixz = 0; %[kg*m^2]
Iyz = 0; %[kg*m^2]

Ixx = 44451.50; %[kg*m^2]
Iyy = 45161.45; %[kg*m^2]
Izz = 44017.00; %[kg*m^2]
%% 0.6 Inertia Products
Ixy = 378.08; %[kg*m^2]
Ixz = 108.92; %[kg*m^2]
Iyz = -3273.58; %[kg*m^2]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   EDITABLE   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DO NOT EDIT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Inertia Matrix wrt to C.M. in B-RF coords.
I_B = [ Ixx  -Ixy  -Ixz;
       -Ixy   Iyy  -Iyz;
       -Ixz  -Iyz   Izz];

THETA_0 = [psi_0,theta_0,phi_0]';      %[deg]
R_BI_0  = EulerAngles_to_R(THETA_0);   %[Euler Angles to R]
BETA_0  = R_to_Quaternions(R_BI_0);    %[R to Quaternions]
w_BI_B0 = [p_0_B,q_0_B,r_0_B]'/180*pi; %[rad/s]

% Integration Time
tspan = 0:0.01:40; %[s]

%% Integration of ODE
Options = odeset('RelTol',1E-7,'AbsTol',1E-9); % Tolerances
[t,X] = ode45(@(t,X)func_DYNAMICS(t,X,I_B),tspan,[BETA_0;w_BI_B0],Options);

%% Elaboration of Results
BETA   = X(:,1:4);    % Quaternions
w_BI_B = X(:,5:7);   % Angular Velocity in B-RF coords

for j=1:size(X,1)
    % Rotation Matrix
    R_BI(:,:,j) = Quaternions_to_R(BETA(j,:));
    % Euler Angles
    [psi(j,1),theta(j,1),phi(j,1)] = R_to_EulerAngles(R_BI(:,:,j)); 
    % Quaternions
    b0(j) = BETA(j,1);
    b1(j) = BETA(j,2);
    b2(j) = BETA(j,3);
    b3(j) = BETA(j,4);
    norm_B(j) = norm(BETA(j,:));
    % Ang. Velocity
    p(j) = w_BI_B(j,1);
    q(j) = w_BI_B(j,2);
    r(j) = w_BI_B(j,3);
    norm_w(j) = norm(w_BI_B(j,:));
end

%% PLOTS: Quantities vs. Time
figure(1)%,hold on
set(gcf,'units','normalized','position',[0 0 0.5 .9])
tiledlayout(4,3,'TileSpacing','Compact','Padding','Compact');
% Quaternions
nexttile(1),hold on,grid on
    plot(t,BETA(:,1),'linewidth',2,'Color',[0,0.6,1])
    xlabel('t'); ylabel('\beta_0'); 
nexttile(4),hold on,grid on
    plot(t,BETA(:,2),'linewidth',2,'Color',[1,0.6,0])
    xlabel('t'); ylabel('\beta_1')
nexttile(7),hold on,grid on
    plot(t,BETA(:,3),'linewidth',2,'Color',[0,0.8,0])
    xlabel('t'); ylabel('\beta_2')
nexttile(10),hold on,grid on
    plot(t,BETA(:,4),'linewidth',2,'Color',[1,0.8,0])
    xlabel('t'); ylabel('\beta_3')
nexttile(11),hold on,grid on
    plot(t,norm_B,'linewidth',1.0,'Color',[.3,.3,.3],'linestyle','-')
    xlabel('t'); ylabel('$||\hat{\beta}||$','Interpreter','Latex')
    ytickformat('%.3f')
% Euler Angles
nexttile(2),hold on,grid on
    plot(t,phi,'linewidth',2,'Color',[0,0.6,1])
    xlabel('t'); ylabel('\phi [deg]')
    ytickformat('%.0f')
nexttile(5),hold on,grid on
    plot(t,theta,'linewidth',2,'Color',[1,0.6,0])
    xlabel('t'); ylabel('\theta [deg]')
    ytickformat('%.0f')
nexttile(8),hold on,grid on
    plot(t,psi,'linewidth',2,'Color',[0,0.8,0])
    xlabel('t'); ylabel('\psi [deg]')
    ytickformat('%.0f')
% Angular Velocity in B-RF coords.
nexttile(3),hold on,grid on
    plot(t,p./norm_w,'linewidth',2,'Color',[0,0.9,0.9])
    xlabel('t'); ylabel('$p/||\vec{\omega}||$','Interpreter','Latex')
    ytickformat('%.1f'); ylim([-1,1]);
nexttile(6),hold on,grid on
    plot(t,q./norm_w,'linewidth',2,'Color',[0,0.6,1])
    xlabel('t'); ylabel('$q/||\vec{\omega}||$','Interpreter','Latex')
    ytickformat('%.1f'); ylim([-1,1]);
nexttile(9),hold on,grid on
    plot(t,r./norm_w,'linewidth',2,'Color',[0,0,1])
    xlabel('t'); ylabel('$r/||\vec{\omega}||$','Interpreter','Latex')
    ytickformat('%.1f'); ylim([-1,1]);
nexttile(12),hold on,grid on
    plot(t,norm_w*180/pi,'linewidth',2,'Color',[1 0.85 0])
    xlabel('t'); ylabel('$||\vec{\omega}||$ [deg/s]','Interpreter','Latex') 
    ytickformat('%.1f');

    toc
%% ODE function
function dX = func_DYNAMICS(t,X,I_B)
    BETA   = X(1:4);
    w_BI_B = X(5:7);

    p = w_BI_B(1);
    q = w_BI_B(2);
    r = w_BI_B(3);

    % External Moments
    M_B = 1E-6*[-6*p,0,-5*r]'; % Part (a)
    %M_B = 1E-5*[5,0,0]';       % Part (b)

    % Quaternions
    b0 = BETA(1); b1 = BETA(2); 
    b2 = BETA(3); b3 = BETA(4);
    B = [ b1  b2  b3
         -b0 -b3  b2
          b3 -b0 -b1
         -b2  b1 -b0];
    
    % Rotational Dynamics
    dX(1:4,1) = 0.5*B*w_BI_B;
    dX(5:7,1) = I_B\(M_B - skew(w_BI_B)*I_B*w_BI_B);
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Other Functions
function R = Quaternions_to_R(q)
    q=q/norm(q);
    if norm(q)-1>1E-6 || norm(q)-1<-1E-6
        fprintf('Quaternions might be not accurate\n')
    end
    q0=q(1); q1=q(2); q2=q(3); q3=q(4);

    R(1,1)=q0^2+q1^2-q2^2-q3^2;
    R(2,2)=q0^2-q1^2+q2^2-q3^2;
    R(3,3)=q0^2-q1^2-q2^2+q3^2;
    
    R(1,2)=2*(q1*q2-q0*q3);
    R(2,1)=2*(q1*q2+q0*q3);
    
    R(1,3)=2*(q1*q3+q0*q2);
    R(3,1)=2*(q1*q3-q0*q2);
    
    R(2,3)=2*(q2*q3-q0*q1);
    R(3,2)=2*(q2*q3+q0*q1);
end

function R = EulerAngles_to_R(THETA)

    psi   = THETA(1);
    theta = THETA(2);
    phi   = THETA(3);
    
    R =[                                cosd(psi)*cosd(theta),                                 cosd(theta)*sind(psi),          -sind(theta)
        cosd(psi)*sind(phi)*sind(theta) - cosd(phi)*sind(psi), sind(phi)*sind(psi)*sind(theta) + cosd(psi)*cosd(phi), sind(phi)*cosd(theta)
        cosd(psi)*cosd(phi)*sind(theta) + sind(phi)*sind(psi), cosd(phi)*sind(psi)*sind(theta) - cosd(psi)*sind(phi), cosd(phi)*cosd(theta)];
end

function q = R_to_Quaternions(R)
    q0sq = 0.25*(1+R(1,1)+R(2,2)+R(3,3));
    q1sq = 0.25*(1+R(1,1)-R(2,2)-R(3,3));
    q2sq = 0.25*(1+R(2,2)-R(1,1)-R(3,3));
    q3sq = 0.25*(1+R(3,3)-R(1,1)-R(2,2));
    if q0sq==max([q0sq,q1sq,q2sq,q3sq])
        q0 = sqrt(q0sq);
        q1 = 1/(4*q0)*(R(3,2)-R(2,3));
        q2 = 1/(4*q0)*(R(1,3)-R(3,1));
        q3 = 1/(4*q0)*(R(2,1)-R(1,2));
    elseif q1sq==max([q0sq,q1sq,q2sq,q3sq])
        q1 = sqrt(q1sq);
        q0 = 1/(4*q1)*(R(3,2)-R(2,3));
        q2 = 1/(4*q1)*(R(2,1)+R(1,2));
        q3 = 1/(4*q1)*(R(1,3)+R(3,1));
    elseif q2sq==max([q0sq,q1sq,q2sq,q3sq])
        q2 = sqrt(q2sq);
        q0 = 1/(4*q2)*(R(1,3)-R(3,1));
        q1 = 1/(4*q2)*(R(2,1)+R(1,2));
        q3 = 1/(4*q2)*(R(3,2)+R(2,3));
    else
        q3 = sqrt(q3sq);
        q0 = 1/(4*q3)*(R(2,1)-R(1,2));
        q1 = 1/(4*q3)*(R(1,3)+R(3,1));
        q2 = 1/(4*q3)*(R(3,2)+R(2,3));
    end
    q = [q0,q1,q2,q3]';
    q = q/norm(q);
end
function [psi,theta,phi] = R_to_EulerAngles(R)
    % Euler Angles
    phi   = atan2d(R(2,3),R(3,3)); % Roll  [-180,180] deg
    theta = asind(-R(1,3));        % Pitch [-90,90]   deg
    psi   = atan2d(R(1,2),R(1,1)); % Yaw   [0,360]    deg
    if psi<0
        psi=mod(psi+360,360);
    end
    if theta==-90 || theta==90
        fprintf('Singularity encountered. Values for roll and yaw angles are not reliable.\n')
        fprintf('Quarternions should be used instead.\n\n')
    end
end
function s = skew(v)
    s = [    0,-v(3), v(2);
          v(3),    0,-v(1);
         -v(2), v(1),    0];
end