%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Written by Dr. Marco Maggia, December 2022.        %
% Aerospace Engineering Department                   %
% California Polytechnic State University, Pomona    %
% Orbital Mechanics - ARO 3090                       %
%                                                    %
% Before sharing the code, please ask for permission.%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clc;clear;close all
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%% EDITABLE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Departure & Arrival Planets
Planet_1 = 'Earth'; % Departure Planet
Planet_2 = 'Mars';   % Arrival Planet

%% Launch Date 
yyyy = 2022;  mm = 8;  dd = 25;
Additional_Days = 0; %Additional days added (or subtracted to Launch Date)

%% Time of Flight (TOF)
TOF = 320;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% %%%%%%%%%%%%%%%%%%%%%%%%% DO NOT EDIT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mu = 132712440018; % Sun's gravitational parameter
AU = 1.49597871E8; % Astronomical Unit
%% Hohmann Transfer
[R1,~,mu1,T1,n1,V1,r_SOI1] = func_parameters_Planet(Planet_1);
[R2,~,mu2,T2,n2,V2,r_SOI2] = func_parameters_Planet(Planet_2);
T_syn = 2*pi/(abs(n1-n2));
at = (R2 + R1)/2;
et = abs(R2 - R1)/(R2 + R1);
ht = sqrt(mu*(1-et^2)*at);
Tt = 2*pi/sqrt(mu)*at^1.5;
phi_dep = mod(180 - (360/T2)*(Tt/2),360);
if phi_dep>180
    phi_dep = 360-phi_dep;
end

V_dep     = ht/R1;
V_inf_dep = abs(V_dep - V1);
V_arr     = ht/R2;
V_inf_arr = abs(V2 - V_arr);
if R1==R2
    fprintf('Please select two different planets.\n\n')
    return
end
fprintf('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
fprintf('             Hohmann Transfer from %s to %s \n\n',Planet_1,Planet_2)
fprintf('     Observer (O): %s, Primary (P): Sun, Target (T): %s\n',Planet_1,Planet_2)
fprintf('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
fprintf('O-P-T Angle at Departure:      phi = %.1f deg   (0 < phi < 180 deg)\n',phi_dep)    
fprintf('          Time of Flight:      TOF = %.0f days\n',Tt/86400/2)  
fprintf('          Synodic Period:    T_Syn = %.0f days = %.1f years\n\n',T_syn/86400,T_syn/86400/365.25)  
fprintf('V_inf (dep)  = %.3f km/s,  C3 (dep) =  %.1f km^2/s^2\n',V_inf_dep, V_inf_dep^2)
fprintf('V_inf (arr)  = %.3f km/s,  C3 (arr) =  %.1f km^2/s^2\n\n',V_inf_arr, V_inf_arr^2)

tic
%% Julian Dates for Departure
JD_Dep = juliandate(yyyy,mm,dd)+Additional_Days;

Options = optimoptions('fsolve','Display','off','TolFun',1E-12,'TolX',1E-12);
% Using 432t as it is the latest Ephemeris available for the function
[r1_vec,VP1_dep] = planetEphemeris(JD_Dep','Sun',Planet_1,'432t');  
 % Creating column vectors
r1_vec=[1.315386533115567E8, -7.200148094172376E7, 3.259147626853362E4]';   VP1_dep=[1.374271621633860E1, 2.605497393397116E1, -1.421352374887164E-3]';   r1 = vecnorm(r1_vec);
JD_Arr = JD_Dep + TOF; % Julian Day

Dep = datetime(JD_Dep,'convertfrom','juliandate');
yyyyD=year(Dep); mmD=month(Dep); ddD=day(Dep);
Arr = datetime(JD_Arr,'convertfrom','juliandate');
yyyyA=year(Arr); mmA=month(Arr); ddA=day(Arr);

% Using 432t as it is the latest Ephemeris available for the function
[r2_vec,VP2_arr] = planetEphemeris(JD_Arr','Sun',Planet_2,'432t');

% Creating column vectors
r2_vec=[-2.480149682310914E8, 2.311697912825998E7, 6.573435869818931E6]';   VP2_arr=[-1.373649815999955, -2.206771889922939E1, -4.283702423805122E-1]';   r2 = vecnorm(r2_vec);

% True Anomaly Difference
DTA_S = acos(dot(r1_vec,r2_vec)./(r1.*r2));
DTA_L = 2*pi-DTA_S;    

A_S = sin(DTA_S).*sqrt(r1.*r2./(1-cos(DTA_S)));
A_L = sin(DTA_L).*sqrt(r1.*r2./(1-cos(DTA_L)));

z0_S =-100;
while Fun(z0_S,TOF*86400,mu,r1,r2,A_S) < 0
    z0_S = z0_S + 0.1;
end
z0_L = -100;
while Fun(z0_L,TOF*86400,mu,r1,r2,A_L) < 0
    z0_L = z0_L + 0.1;
end

z_S = fsolve(@(z_S)Fun(z_S,TOF*86400,mu,r1,r2,A_S),z0_S,Options);
z_L = fsolve(@(z_L)Fun(z_L,TOF*86400,mu,r1,r2,A_L),z0_L,Options);

y_S = r1 + r2 + A_S.*(z_S.*stumpS(z_S) - 1)./sqrt(stumpC(z_S));
y_L = r1 + r2 + A_L.*(z_L.*stumpS(z_L) - 1)./sqrt(stumpC(z_L));

f_S = 1 - y_S./r1;
g_S = A_S.*sqrt(y_S/mu);
gdot_S = 1 - y_S./r2;
v1_vec_S = (r2_vec - f_S.*r1_vec)./g_S;
v2_vec_S = 1./g_S.*(gdot_S.*r2_vec - r1_vec);

f_L = 1 - y_L./r1;
g_L = A_L.*sqrt(y_L/mu);
gdot_L = 1 - y_L./r2;
v1_vec_L = (r2_vec - f_L.*r1_vec)./g_L;
v2_vec_L = 1/g_L.*(gdot_L.*r2_vec - r1_vec);

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

[e,a,i,w,Om,TA1,gam1,h] = func_RVtoOE(r1_vec,v1_vec,mu);
[~,~,~,~, ~,TA2,gam2,~] = func_RVtoOE(r2_vec,v2_vec,mu);

% Orbital Elements of Planet 1 at Departure
[e_P1,a_P1,i_P1,w_P1,Om_P1,~,~,h_P1] = func_RVtoOE(r1_vec,VP1_dep,mu);
% Orbital Elements of Planet 2 at Arrival
[e_P2,a_P2,i_P2,w_P2,Om_P2,~,~,h_P2] = func_RVtoOE(r2_vec,VP2_arr,mu);

fprintf('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
fprintf('        Interplanetary %s-to-%s Orbit Characteristics:\n',Planet_1,Planet_2)
fprintf('\n%s',orbit_choice)
fprintf('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
fprintf('       Semi-Major Axis:    a  = %.3f AU\n',a/AU)
fprintf('          Eccentricity:    e  = %.4f\n'   ,e)
fprintf('      Periapsis Radius:   rp = %.3f AU\n',abs(a*(1-e))/AU)
fprintf('        Apopsis Radius:   ra = %.3f AU\n',abs(a*(1+e))/AU)
fprintf('                Period:    T = %.0f days = %.1f years\n\n',2*pi/sqrt(mu)*a^1.5/86400,2*pi/sqrt(mu)*a^1.5/86400/365.25)

fprintf('R.A. of Ascending Node:   Om = %.1f  deg\n',Om)
fprintf('           Inclination:    i = %.1f  deg\n',i)
fprintf(' Argument of Periapsis:    w = %.1f  deg\n',w)

fprintf('~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ \n')

fprintf('True Anomaly of S/C at Departure:  TA_1 = %3.1f  deg\n'  ,TA1)
fprintf('True Anomaly of S/C at   Arrival:  TA_2 = %3.1f  deg\n',TA2)
fprintf('              Delta True Anomaly:         %3.1f  deg\n',mod(TA2-TA1,360))
fprintf('                  Time of Flight:   TOF = %3.0f  days = %.1f  years\n\n',TOF,TOF/365.25)

fprintf('At Departure (%s):\n                    ',Planet_1);
fprintf('V(S/C) = [%.5f,%.5f,%.5f] km/s\n                    ',v1_vec)
fprintf('v_inf  = [%.5f,%.5f,%.5f] km/s\n              ',vinf_dep_vec)
fprintf('     |v_inf| = %.3f  km/s,\n                   C3 = %.1f km^2/s^2\n\n', sqrt(C3_dep),C3_dep)

fprintf('At Arrival (%s):\n                    ',Planet_2);
fprintf('V(S/C) = [%.5f,%.5f,%.5f] km/s\n                    ',v2_vec)
fprintf('v_inf  = [%.5f,%.5f,%.5f] km/s\n              ',vinf_arr_vec)
fprintf('     |v_inf| = %.3f  km/s,\n                   C3 = %.1f km^2/s^2\n\n', sqrt(C3_arr),C3_arr)

%% S/C Orbit
R_PI = func_RPI(Om,i,w);
R_IP = R_PI';

if TA1<TA2
    TA   = TA1:0.01:TA2;
    r_PF = zeros(3,size(TA,2));
    r_I  = zeros(3,size(TA,2));
    for j=1:size(TA,2)
        r_PF(:,j) = h^2/mu/(1+e*cosd(TA(j)))*[cosd(TA(j)),sind(TA(j)),0]';
        r_I(:,j)  = R_IP*r_PF(:,j);
    end
else
    TAa   = 0:0.01:TA2;
    r_PFa = zeros(3,size(TAa,2));
    r_Ia  = zeros(3,size(TAa,2));
    for j=1:size(TAa,2)
        r_PFa(:,j) = h^2/mu/(1+e*cosd(TAa(j)))*[cosd(TAa(j)),sind(TAa(j)),0]';
        r_Ia(:,j)  = R_IP*r_PFa(:,j);
    end
    TAb = TA1:0.01:360;
    r_PFb = zeros(3,size(TAb,2));
    r_Ib  = zeros(3,size(TAb,2));
    for j=1:size(TAb,2)
        r_PFb(:,j) = h^2/mu/(1+e*cosd(TAb(j)))*[cosd(TAb(j)),sind(TAb(j)),0]';
        r_Ib(:,j)  = R_IP*r_PFb(:,j);
    end
end

%% Planet_1 Orbit
color_P1 = [0,1,0];
R_PI_P1 = func_RPI(Om_P1,i_P1,w_P1);
R_IP_P1 = R_PI_P1';

TA_P1   = 0:0.1:360;
r_PF_P1 = zeros(3,size(TA_P1,2));
r_I_P1  = zeros(3,size(TA_P1,2));
for j=1:size(TA_P1,2)
    r_PF_P1(:,j) = (h_P1^2/mu)/(1+e_P1*cosd(TA_P1(j)))*[cosd(TA_P1(j)),sind(TA_P1(j)),0]';
    r_I_P1(:,j)  = R_IP_P1*r_PF_P1(:,j);
end

%% Planet_2 Orbit
color_P2 = [1,.6,0];
R_PI_P2 = func_RPI(Om_P2,i_P2,w_P2);
R_IP_P2 = R_PI_P2';

TA_P2   = 0:0.1:360;
r_PF_P2 = zeros(3,size(TA_P2,2));
r_I_P2  = zeros(3,size(TA_P2,2));
for j=1:size(TA_P2,2)
    r_PF_P2(:,j) = (h_P2^2/mu)/(1+e_P2*cosd(TA_P2(j)))*[cosd(TA_P2(j)),sind(TA_P2(j)),0]';
    r_I_P2(:,j)  = R_IP_P2*r_PF_P2(:,j);
end

%% Plots
figure(3),hold on
set(gcf,'units','normalized','position',[0,0.06,0.5,0.83]) 
set(gca,'Color',[0,0,0])
set(gcf,'Color',[0,0,0])
view([Om,25])
strt1 = sprintf('Launch Date: %d-%d-%d,    Arrival Date: %d-%d-%d,    [TOF: %.0f days]\n',yyyyD,mmD,ddD,yyyyA,mmA,ddA,TOF);
strt2 = sprintf('C3(dep.)=%.1f km^2/s^2,  v_{inf}(arr.)=%.2f km/s',C3_dep,sqrt(C3_arr));
tlt_t = title([strt1,strt2]);
tlt_t.Color = 'w'; 
tlt_t.FontSize = 13; 

l_S = plot3(0,0,0,'markeredgecolor',[1,1,0.4],'markerfacecolor',[1,1,0.4],'marker','o','markersize',10,'color','none');
if TA1<TA2
    l = plot3(r_I(1,:)/AU,r_I(2,:)/AU,r_I(3,:)/AU,'color',color_traj,'linewidth',1.5);
else
    l = plot3(r_Ia(1,:)/AU,r_Ia(2,:)/AU,r_Ia(3,:)/AU,'color',color_traj,'linewidth',1.5);
        plot3(r_Ib(1,:)/AU,r_Ib(2,:)/AU,r_Ib(3,:)/AU,'color',color_traj,'linewidth',1.5);
end
plot3(r_I_P1(1,:)/AU,r_I_P1(2,:)/AU,r_I_P1(3,:)/AU,'color',color_P1,'linewidth',0.5);
plot3(r_I_P2(1,:)/AU,r_I_P2(2,:)/AU,r_I_P2(3,:)/AU,'color',color_P2,'linewidth',0.5);

l_P1     = plot3(r1_vec(1,1)/AU,r1_vec(2,1)/AU,r1_vec(3,1)/AU,'markeredgecolor',color_P1,'markerfacecolor',color_P1,'marker','o','markersize',8,'color',color_P1);
l_P2     = plot3(r2_vec(1,1)/AU,r2_vec(2,1)/AU,r2_vec(3,1)/AU,'markeredgecolor',color_P2,'markerfacecolor',color_P2,'marker','o','markersize',8,'color',color_P2);
text_P1 = sprintf('   %s on %d-%d-%d',Planet_1,yyyyD,mmD,ddD);
text_P2 = sprintf('   %s on %d-%d-%d',Planet_2,yyyyA,mmA,ddA);
text(r1_vec(1,1)/AU+.1,r1_vec(2,1)/AU+.1,r1_vec(3,1)/AU+.1,text_P1,'Color','w')%,'FontSize',16,'interpreter','latex');
text(r2_vec(1,1)/AU+.1,r2_vec(2,1)/AU+.1,r2_vec(3,1)/AU+.1,text_P2,'Color','w')%,'FontSize',16,'interpreter','latex');
    
axis('equal')

leg = legend([l_S,l_P1,l_P2,l],'Sun',sprintf('%s',Planet_1),sprintf('%s',Planet_2),'Interplanetary Transfer Orbit');
leg.TextColor=[1 1 1];

function [r,R,mu,T,n,v,r_SOI] = func_parameters_Planet(Planet)
    % Planetary Parameters
    mu_Sun = 1.3271244e11; %[km^3/s^2]
    
    if strcmp(Planet,'Mercury')
        R = 2440;
        mu = 22032;
        r = 57.91e6;
    elseif strcmp(Planet,'Venus')
        R = 6052;
        mu = 324900;
        r = 108.2e6;
    elseif strcmp(Planet,'Earth')    
        R = 6378;
        r = 149.6E6;
        mu = 398600;
    elseif strcmp(Planet,'Mars')   
        R = 3396;
        mu = 42828;
        r = 227.9e6;
    elseif strcmp(Planet,'Jupiter')
        R = 71490;
        mu = 126686000;
        r = 778.6e6;
    elseif strcmp(Planet,'Saturn') 
        R = 60270;
        mu = 37931000;
        r = 1433e6;
    elseif strcmp(Planet,'Uranus') 
        R = 25560;
        mu = 5794000;
        r = 2872e6;
    elseif strcmp(Planet,'Neptune')
        R = 24760;
        mu = 6835100;
        r = 4495e6; 
    elseif strcmp(Planet,'Pluto')
        R = 1188;
        mu = 869.61;
        r = 5.90638E9;
    end
    
    T = 2*pi/sqrt(mu_Sun)*r^1.5; %[s]
    n = 2*pi/T;                  %[rad/s]
    v = sqrt(mu_Sun/r);          %[km/s]
    r_SOI = r*(mu/mu_Sun)^(2/5); %[km]
end
function R_PI = func_RPI(Om,i,w)
    % Computes the rotation matrix from Earth-Centered Inertial RF (I) to Perifocal RF (P).
    
    ROm = [cosd(Om),sind(Om),0; -sind(Om),cosd(Om), 0;0,0,1];
    Ri  = [1,0,0; 0,cosd(i),sind(i); 0,-sind(i),cosd(i)];
    Rw  = [cosd(w),sind(w),0; -sind(w),cosd(w),0; 0,0,1]; 
        
    R_PI = Rw*Ri*ROm;
end
function [norm_e,a,i,w,Om,TA,gam,norm_h] = func_RVtoOE(r,v,mu)
    % Calculates Orbital Elements from Position and Inertial Velocity Vectors.
    i1 = [1,0,0]';     i3 = [0,0,1]';
    
    h = cross(r,v);
    p3 = h/norm(h);
    e = cross(v,h)/mu - r/norm(r);
    norm_e=norm(e);
    p1 = e/norm(e);
    N = cross(i3,p3)/norm(cross(i3,p3));
    if N(2)>=0
        Om = acosd(i1'*N);
    else
        Om = 360-acosd(i1'*N);
    end
    i = acosd(i3'*p3);
    if p1(3)>=0
        w = acosd(p1'*N);
    else
        w = 360-acosd(p1'*N);
    end
    vr = r'*v;
    if vr>=0
        TA = acosd(p1'*(r/norm(r)));
    else
        TA = 360-acosd(p1'*(r/norm(r)));
    end
    gam = 90 - acosd((r/norm(r))'*(v/norm(v)));
    a = norm(h)^2/mu/abs(1-norm_e^2);
    norm_h = norm(h);
end
function S = stumpS(Z)
    % This function evaluates the Stumpff function S(z).
    if size(Z,1)==1
        Z=Z';
    end
    S=zeros(size(Z));    
    for i=1:size(Z)
        z=Z(i,1);
        if z>0
            S(i,1) = (sqrt(z) - sin(sqrt(z)))./(sqrt(z)).^3;
        elseif z<0
            S(i,1) = (sinh(sqrt(-z)) - sqrt(-z))./(sqrt(-z)).^3;
        else
            S(i,1) = 1/6;
        end
    end
end
function C = stumpC(Z)
    % This function evaluates the Stumpff function C(z).
    if size(Z,1)==1
        Z=Z';
    end
    C=zeros(size(Z));
    for i=1:size(Z)
        z=Z(i,1);
        if z>0
            C(i,1) = (1 - cos(sqrt(z)))./z;
        elseif z<0
            C(i,1) = (cosh(sqrt(-z)) - 1)./(-z);
        else
            C(i,1) = 1/2;
        end
    end
end
function F = Fun(z,t,mu,r1,r2,A)
    if size(z,1)==1
        z=z';
    end
    if size(A,1)==1
        A=A';
    end
    if size(r1,1)==1
        r1=r1';
    end
    if size(r2,1)==1
        r2=r2';
    end
    AU = 1.49597871E8; % Astronomical Unit
    TU = 5022643;
    y = r1/AU + r2/AU + A./AU.*(z.*stumpS(z) - 1)./sqrt(stumpC(z));
    F = (y./stumpC(z)).^1.5.*stumpS(z) + A./AU.*sqrt(y) - sqrt(mu/AU^3*TU^2)*(t/TU);
    F = F';
end