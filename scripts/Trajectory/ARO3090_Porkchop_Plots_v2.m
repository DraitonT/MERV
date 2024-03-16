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
Planet_1 = 'Earth';   % Departure Planet
Planet_2 = 'Mars'; % Arrival Planet

%% Launch Dates 
% First and Last possible launch dates
Before_ILD = -50; %[<0 days] before Hohmann     (Recommended: Mercury=-50, Venus=-90, Mars=-120, Jupiter,Saturn,Uranus,Neptune=-50)
After_ILD  = +50; %[>0 days] after Hohmann      (Recommended: Mercury=+50, Venus=+90, Mars=+120, Jupiter,Saturn,Uranus,Neptune=+50)  
dJD = 1;          %[days] Launch date increment (Recommended: All Planets<=1)

%% Time of Flight (TOF)
TOF_min = 0;   %[days] Minimum TOF   (Recommended: Mercury=50,  Venus=80,  Mars=80, Jupiter=400,  Saturn=900,  Uranus=2500, Neptune=5000)
TOF_max = 320;  %[days] Maximum TOF   (Recommended: Mercury=150, Venus=200, Mars=500, Jupiter=1500, Saturn=3500, Uranus=8000, Neptune=10000)
dTOF = 10;       %[days] TOF increment (Recommended: Mercury<=1,  Venus<=2,  Mars<=5,  Jupiter<=10,  Saturn<=40,  Uranus<=100, Neptune<=100)

%% Maximum C3 Capability
C3_max = 130;    %[km^2/s^2]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% %%%%%%%%%%%%%%%%%%%%%%%%% DO NOT EDIT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if TOF_min>TOF_max
    dTOF = -dTOF;
end
mu = 132712440018; % Sun's gravitational parameter
Options = optimoptions('fsolve','Display','off','TolFun',1E-12,'TolX',1E-12);
%% Hohmann Transfer
[R1,~,mu1,T1,n1,V1,~] = func_parameters_Planet(Planet_1);
[R2,~,mu2,T2,n2,V2,~] = func_parameters_Planet(Planet_2);
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
fprintf('          Synodic Period:    T_Syn = %.0f days\n\n',T_syn/86400)  
fprintf('V_inf (dep)  = %.3f km/s,  C3 (dep) =  %.1f km^2/s^2\n',V_inf_dep, V_inf_dep^2)
fprintf('V_inf (arr)  = %.3f km/s,  C3 (arr) =  %.1f km^2/s^2\n',V_inf_arr, V_inf_arr^2)
fprintf('                            C3 (tot) = %.1f km^2/s^2\n\n',V_inf_dep^2+V_inf_arr^2)

fprintf('Insert Hohmann Transfer Ideal Launch Date (ILD):\n\n')
if strcmp(Planet_1,'Earth')
    if strcmp(Planet_2,'Mercury') 
        fprintf('For %s-%s transfer select one of the following dates:\n\n',Planet_1,Planet_2)
        fprintf('yyyy = 2035;  mm = 12;  dd = 17\n');
        fprintf('yyyy = 2035;  mm = 08;  dd = 09\n');
        fprintf('yyyy = 2035;  mm = 04;  dd = 20\n');
        fprintf('yyyy = 2035;  mm = 01;  dd = 03\n');
        fprintf('yyyy = 2034;  mm = 08;  dd = 28\n');
        fprintf('yyyy = 2034;  mm = 05;  dd = 06\n');
        fprintf('yyyy = 2034;  mm = 01;  dd = 20\n');
        fprintf('yyyy = 2033;  mm = 09;  dd = 17\n');
        fprintf('yyyy = 2033;  mm = 05;  dd = 22\n');
        fprintf('yyyy = 2033;  mm = 02;  dd = 05\n');
        fprintf('yyyy = 2032;  mm = 10;  dd = 08\n');
        fprintf('yyyy = 2032;  mm = 06;  dd = 07\n');
        fprintf('yyyy = 2032;  mm = 02;  dd = 22\n');            
        fprintf('yyyy = 2031;  mm = 10;  dd = 29\n');
        fprintf('yyyy = 2031;  mm = 06;  dd = 24\n');
        fprintf('yyyy = 2031;  mm = 03;  dd = 10\n');
        fprintf('yyyy = 2030;  mm = 11;  dd = 18\n');
        fprintf('yyyy = 2030;  mm = 07;  dd = 11\n');
        fprintf('yyyy = 2030;  mm = 03;  dd = 26\n');
        fprintf('yyyy = 2029;  mm = 12;  dd = 06\n');    
        fprintf('yyyy = 2029;  mm = 07;  dd = 29\n');
        fprintf('yyyy = 2029;  mm = 04;  dd = 10\n');
        fprintf('yyyy = 2028;  mm = 12;  dd = 24\n');
        fprintf('yyyy = 2028;  mm = 08;  dd = 16\n');
        fprintf('yyyy = 2028;  mm = 04;  dd = 26\n');
        fprintf('yyyy = 2028;  mm = 01;  dd = 11\n');
        fprintf('yyyy = 2027;  mm = 09;  dd = 06\n');
        fprintf('yyyy = 2027;  mm = 05;  dd = 13\n');
        fprintf('yyyy = 2027;  mm = 01;  dd = 27\n');
        fprintf('yyyy = 2026;  mm = 09;  dd = 26\n');
        fprintf('yyyy = 2026;  mm = 05;  dd = 29\n');
        fprintf('yyyy = 2026;  mm = 02;  dd = 13\n');
        fprintf('yyyy = 2025;  mm = 10;  dd = 17\n');
        fprintf('yyyy = 2025;  mm = 06;  dd = 14\n');
        fprintf('yyyy = 2025;  mm = 02;  dd = 28\n');
        fprintf('yyyy = 2024;  mm = 11;  dd = 06\n');
        fprintf('yyyy = 2024;  mm = 07;  dd = 01\n');
        fprintf('yyyy = 2024;  mm = 03;  dd = 16\n\n');
    elseif strcmp(Planet_2,'Venus')
        fprintf('For %s-%s transfer select one of the following dates:\n\n',Planet_1,Planet_2)
        fprintf('yyyy = 2034;  mm = 07;  dd = 26\n');
        fprintf('yyyy = 2032;  mm = 12;  dd = 21\n');
        fprintf('yyyy = 2031;  mm = 05;  dd = 18\n');
        fprintf('yyyy = 2029;  mm = 10;  dd = 10\n');
        fprintf('yyyy = 2028;  mm = 03;  dd = 08\n');
        fprintf('yyyy = 2026;  mm = 07;  dd = 28\n');
        fprintf('yyyy = 2024;  mm = 12;  dd = 24\n\n');
    elseif strcmp(Planet_2,'Mars')
        fprintf('For %s-%s transfer select one of the following dates:\n\n',Planet_1,Planet_2)
        fprintf('yyyy = 2035;  mm = 05;  dd = 09\n');
        fprintf('yyyy = 2033;  mm = 03;  dd = 18\n');
        fprintf('yyyy = 2031;  mm = 02;  dd = 09\n');
        fprintf('yyyy = 2029;  mm = 01;  dd = 06\n');
        fprintf('yyyy = 2026;  mm = 12;  dd = 04\n');
        fprintf('yyyy = 2024;  mm = 10;  dd = 24\n\n');
    elseif strcmp(Planet_2,'Jupiter')
        fprintf('For %s-%s transfer select one of the following dates:\n\n',Planet_1,Planet_2)
        fprintf('yyyy = 2035;  mm = 07;  dd = 21\n');
        fprintf('yyyy = 2034;  mm = 06;  dd = 12\n');
        fprintf('yyyy = 2033;  mm = 05;  dd = 05\n');
        fprintf('yyyy = 2032;  mm = 03;  dd = 31\n');
        fprintf('yyyy = 2031;  mm = 02;  dd = 27\n');
        fprintf('yyyy = 2030;  mm = 01;  dd = 27\n');
        fprintf('yyyy = 2028;  mm = 12;  dd = 29\n');
        fprintf('yyyy = 2027;  mm = 11;  dd = 30\n');
        fprintf('yyyy = 2026;  mm = 10;  dd = 30\n');
        fprintf('yyyy = 2025;  mm = 09;  dd = 27\n');
        fprintf('yyyy = 2024;  mm = 08;  dd = 22\n\n');
    elseif strcmp(Planet_2,'Saturn')
        fprintf('For %s-%s transfer select one of the following dates:\n\n',Planet_1,Planet_2)
        fprintf('yyyy = 2035;  mm = 10;  dd = 19\n');
        fprintf('yyyy = 2034;  mm = 10;  dd = 05\n');
        fprintf('yyyy = 2033;  mm = 09;  dd = 20\n');
        fprintf('yyyy = 2032;  mm = 09;  dd = 05\n');
        fprintf('yyyy = 2031;  mm = 08;  dd = 23\n');
        fprintf('yyyy = 2030;  mm = 08;  dd = 08\n');
        fprintf('yyyy = 2029;  mm = 07;  dd = 24\n');
        fprintf('yyyy = 2028;  mm = 07;  dd = 09\n');
        fprintf('yyyy = 2027;  mm = 06;  dd = 26\n');
        fprintf('yyyy = 2026;  mm = 06;  dd = 12\n');
        fprintf('yyyy = 2025;  mm = 05;  dd = 30\n');
        fprintf('yyyy = 2024;  mm = 05;  dd = 17\n\n');
    elseif strcmp(Planet_2,'Uranus')
        fprintf('For %s-%s transfer select one of the following dates:\n\n',Planet_1,Planet_2)
        fprintf('yyyy = 2035;  mm = 09;  dd = 13\n');
        fprintf('yyyy = 2034;  mm = 09;  dd = 08\n');
        fprintf('yyyy = 2033;  mm = 09;  dd = 04\n');
        fprintf('yyyy = 2032;  mm = 08;  dd = 30\n');
        fprintf('yyyy = 2031;  mm = 08;  dd = 26\n');
        fprintf('yyyy = 2030;  mm = 08;  dd = 21\n');
        fprintf('yyyy = 2029;  mm = 08;  dd = 16\n');
        fprintf('yyyy = 2028;  mm = 08;  dd = 12\n');
        fprintf('yyyy = 2027;  mm = 08;  dd = 08\n');
        fprintf('yyyy = 2026;  mm = 08;  dd = 03\n');
        fprintf('yyyy = 2025;  mm = 07;  dd = 30\n');
        fprintf('yyyy = 2024;  mm = 07;  dd = 25\n\n'); 
    elseif strcmp(Planet_2,'Neptune')
        fprintf('For %s-%s transfer select one of the following dates:\n\n',Planet_1,Planet_2)
        fprintf('yyyy = 2035;  mm = 06;  dd = 21\n');
        fprintf('yyyy = 2034;  mm = 06;  dd = 18\n');
        fprintf('yyyy = 2033;  mm = 06;  dd = 16\n');
        fprintf('yyyy = 2032;  mm = 06;  dd = 13\n');
        fprintf('yyyy = 2031;  mm = 06;  dd = 11\n');
        fprintf('yyyy = 2030;  mm = 06;  dd = 09\n');
        fprintf('yyyy = 2029;  mm = 06;  dd = 06\n');
        fprintf('yyyy = 2028;  mm = 06;  dd = 03\n');
        fprintf('yyyy = 2027;  mm = 06;  dd = 02\n');
        fprintf('yyyy = 2026;  mm = 05;  dd = 30\n');
        fprintf('yyyy = 2025;  mm = 05;  dd = 28\n');
        fprintf('yyyy = 2024;  mm = 05;  dd = 25\n\n'); 
    end
    fprintf('Or go to: <a href = "https://ssd.jpl.nasa.gov/horizons.cgi">NASA JPL Horizons</a>\n\n')
else
    fprintf('To determine the Ideal Launch Date (ILD) go to: ')
    fprintf('<a href = "https://ssd.jpl.nasa.gov/horizons.cgi">NASA JPL Horizons</a>\n\n')
    fprintf('    Ephemeris Type: OBSERVER TABLE\n')
    fprintf('       Target Body: %s\n',Planet_2)
    fprintf(' Observer Location: @%s\n',Planet_1)
    fprintf('Time Specification: custom\n')
    fprintf('    Table Settings: custom [Check only quantity #26: "Observer-Primary-Target angle"  ]\n')
    fprintf('                           [   ILD occurs when OPT = %3.1f deg & OPT is DECREASING     ]\n\n',phi_dep)
    web https://ssd.jpl.nasa.gov/horizons.cgi"
end
yyyyH = input('Year  (yyyy):  ');
mmH   = input('Month (1-12):  ');
ddH   = input('Day   (1-31):  ');

% yyyyH = 2024;
% mmH   = 6;
% ddH   = 9;

fprintf('\n[Hohmann ILD: %d/%d/%d]\n\n',yyyyH,mmH,ddH)
tic
%% Julian Dates for Departure
JD_DepH = juliandate(yyyyH,mmH,ddH);
JD_Dep0 = JD_DepH+Before_ILD;
JD_DepF = JD_DepH+After_ILD;
Dep0 = datetime(JD_Dep0,'convertfrom','juliandate');
yyyy0=year(Dep0); mm0=month(Dep0); dd0=day(Dep0);

if JD_DepF<JD_Dep0
    fprintf('Last possible launch date must be later than: %d/%d/%d (y/m/d).\n\n',yyyy0,mm0,dd0)
    return
end

JD_Dep = JD_Dep0:dJD:JD_DepF;
Dt_Dep = JD_Dep-JD_DepH;
TOF    = TOF_min:dTOF:TOF_max;

J = size(JD_Dep,2);
T = size(TOF,2);

C3_dep_S=NaN(T,J); C3_arr_S=NaN(T,J);
C3_dep_L=NaN(T,J); C3_arr_L=NaN(T,J);
C3_dep  =NaN(T,J); C3_arr  =NaN(T,J);
z0_S    =NaN(T,J);    z0_L =NaN(T,J);
JD_Arr  =NaN(T,J);

% Using 432t as it is the latest Ephemeris available for the function
[r1_vec,VP1_dep] = planetEphemeris(JD_Dep','Sun',Planet_1,'432t');  
 % Creating column vectors
r1_vec=r1_vec';   VP1_dep=VP1_dep';   r1 = vecnorm(r1_vec);
%%
wb = waitbar(0,'Pork Chops Grilling in Progress...');
for k=1:T
    % Waitbar
    waitbar(k/T,wb);
    wbc = allchild(wb);
    wbc(1).JavaPeer.setForeground(wbc(1).JavaPeer.getBackground.green)
    wbc(1).JavaPeer.setStringPainted(true)

    JD_Arr(k,:) = JD_Dep + ones(1,J)*TOF(k); % Julian Day
    
    % Using 432t as it is the latest Ephemeris available for the function
    [r2_vec,VP2_arr] = planetEphemeris(JD_Arr(k,:)','Sun',Planet_2,'432t');
   
    % Creating column vectors
    r2_vec=r2_vec';   VP2_arr=VP2_arr';   r2 = vecnorm(r2_vec);

    %% SHORT-way Orbits
    % True Anomaly Difference
    DTA_S = acos(dot(r1_vec,r2_vec)./(r1.*r2));
    A_S = sin(DTA_S).*sqrt(r1.*r2./(1-cos(DTA_S)));

    z0_S(k,1)=-100;
    for j=1:J
        if rem(j,2)==1
            while Fun(z0_S(k,j),TOF(k)*86400,mu,r1(j),r2(j),A_S(j)) < 0
                z0_S(k,j) = z0_S(k,j) + 0.1;
            end
        else
            while Fun(z0_S(k,j),TOF(k)*86400,mu,r1(j),r2(j),A_S(j)) > 0
                z0_S(k,j) = z0_S(k,j) - 0.1;
            end
        end
        if j<J
            z0_S(k,j+1) = z0_S(k,j);
        end
    end
    z_S = fsolve(@(z_S)Fun(z_S,TOF(k)*86400,mu,r1,r2,A_S),z0_S(k,:),Options);
    y_S = r1 + r2 + A_S.*(z_S.*stumpS(z_S)' - 1)./sqrt(stumpC(z_S)');
    f_S = 1 - y_S./r1;
    g_S = A_S.*sqrt(y_S/mu);
    gdot_S = 1 - y_S./r2;

    % Departure
    v1_vec_S = (r2_vec - f_S.*r1_vec)./g_S;
    vinf_dep_vec_S = v1_vec_S - VP1_dep;
    vinf_dep_S = vecnorm(vinf_dep_vec_S);
    C3_dep_S(k,:) = vinf_dep_S.^2;
    % Arrival
    v2_vec_S = 1./g_S.*(gdot_S.*r2_vec - r1_vec);
    vinf_arr_vec_S = v2_vec_S - VP2_arr;
    vinf_arr_S = vecnorm(vinf_arr_vec_S);
    C3_arr_S(k,:) = vinf_arr_S.^2;

    %~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    %% LONG-way Orbits
    % True Anomaly Difference
    DTA_L = 2*pi-DTA_S;    
    A_L = sin(DTA_L).*sqrt(r1.*r2./(1-cos(DTA_L)));

    z0_L(k,1)=-100;
    for j=1:J
        if rem(j,2)==1
            while Fun(z0_L(k,j),TOF(k)*86400,mu,r1(j),r2(j),A_L(j)) < 0
                z0_L(k,j) = z0_L(k,j) + 0.1;
            end
        else
            while Fun(z0_L(k,j),TOF(k)*86400,mu,r1(j),r2(j),A_L(j)) > 0
                z0_L(k,j) = z0_L(k,j) - 0.1;
            end
        end
        if j<J
            z0_L(k,j+1) = z0_L(k,j);
        end
    end
    z_L = fsolve(@(z_L)Fun(z_L,TOF(k)*86400,mu,r1,r2,A_L),z0_L(k,:),Options);  
    y_L = r1 + r2 + A_L.*(z_L.*stumpS(z_L)' - 1)./sqrt(stumpC(z_L)');       
    f_L = 1 - y_L./r1;
    g_L = A_L.*sqrt(y_L/mu);
    gdot_L = 1 - y_L./r2;  
    
    % Departure   
    v1_vec_L = (r2_vec - f_L.*r1_vec)./g_L;
    vinf_dep_vec_L = v1_vec_L - VP1_dep;
    vinf_dep_L = vecnorm(vinf_dep_vec_L);
    C3_dep_L(k,:) = vinf_dep_L.^2;
    % Arrival  
    v2_vec_L = 1./g_L.*(gdot_L.*r2_vec - r1_vec);
    vinf_arr_vec_L = v2_vec_L - VP2_arr;
    vinf_arr_L = vecnorm(vinf_arr_vec_L);
    C3_arr_L(k,:) = vinf_arr_L.^2;

    C3_dep(k,:) = min(C3_dep_S(k,:),C3_dep_L(k,:));
    C3_arr(k,:) = min(C3_arr_S(k,:),C3_arr_L(k,:));
end
delete(wb)

%% Printing min C3 at Departure
[~,J_C3] = min(min(C3_dep));
[~,T_C3] = min(C3_dep(:,J_C3));

min_C3_dep = C3_dep(T_C3,J_C3);
TOF_min_C3 = TOF(T_C3);
JD_min_C3  = JD_Dep(J_C3);

Dep_C3 = datetime(JD_min_C3,'convertfrom','juliandate');
y_C3=year(Dep_C3); m_C3=month(Dep_C3); d_C3=day(Dep_C3);

if JD_min_C3>=JD_DepH
    fprintf('Minimum Departure C3:   %.1f km^2/s^2, Launch Date: %d/%d/%d  (+%d days after ILD), TOF: %d days.\n',min_C3_dep,y_C3,m_C3,d_C3,abs(JD_min_C3-JD_DepH),TOF_min_C3)
else
    fprintf('Minimum Departure C3:   %.1f km^2/s^2, Launch Date: %d/%d/%d  (-%d days before ILD), TOF: %d days.\n',min_C3_dep,y_C3,m_C3,d_C3,abs(JD_min_C3-JD_DepH),TOF_min_C3)
end
%% Printing min v_inf at Arrival
[~,J_vinf] = min(min(C3_arr));
[~,T_vinf] = min(C3_arr(:,J_vinf));

min_vinf_arr = sqrt(C3_arr(T_vinf,J_vinf));
TOF_min_vinf = TOF(T_vinf);
JD_min_vinf  = JD_Dep(J_vinf);

Arr_vinf = datetime(JD_min_vinf,'convertfrom','juliandate');
y_vinf=year(Arr_vinf); m_vinf=month(Arr_vinf); d_vinf=day(Arr_vinf);

if JD_min_vinf>=JD_DepH
    fprintf('Minimum Arrival v_inf:  %.2f km/s, Launch Date: %d/%d/%d  (+%d days after ILD), TOF: %d days.\n\n',min_vinf_arr,y_vinf,m_vinf,d_vinf,abs(JD_min_vinf-JD_DepH),TOF_min_vinf)
else
    fprintf('Minimum Arrival v_inf:  %.2f km/s, Launch Date: %d/%d/%d  (-%d days before ILD), TOF: %d days.\n\n',min_vinf_arr,y_vinf,m_vinf,d_vinf,abs(JD_min_vinf-JD_DepH),TOF_min_vinf)
end

%% %%%%%%%%%%%%%%%%%%%%   Saving data on File   %%%%%%%%%%%%%%%%%%%%%%%%%%%

% File with proper name
FileName = sprintf('%s to %s [ILD %d-%d-%d].csv',Planet_1,Planet_2,yyyyH,mmH,ddH);
writematrix([TOF',C3_dep],FileName) 
writematrix([TOF',C3_arr],FileName,'WriteMode','append')
writematrix([NaN,JD_Dep],FileName,'WriteMode','append')

% Temporary file (C3.csv) for plotting purposes
writematrix([TOF',C3_dep],'C3.csv') 
writematrix([TOF',C3_arr],'C3.csv','WriteMode','append')
writematrix([NaN,JD_Dep],'C3.csv','WriteMode','append')

C3 = load(FileName);
C3_dep = C3(0*T+1:1*T,2:J+1);
C3_arr = C3(1*T+1:2*T,2:J+1);
JD_Dep = C3(end,2:J+1);
TOF    = C3(1:T,1)';

%% %%%%%%%%%%%%%%%%%%%%%%%%%%   DEPARTURE   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
figure(1),hold on,grid on
set(gcf,'units','normalized','position',[0.5,0.06,1,0.83]) 
subplot(1,2,1),hold on,grid on 
title(sprintf('%s to %s: Departure C_3  [km^2/s^2]',Planet_1,Planet_2))

ax1 = gca;
[~,h1a]  = contour(ax1,Dt_Dep,TOF,C3_dep,'Showtext','on');
[~,h1b]  = contour(ax1,Dt_Dep,TOF,C3_dep,'Showtext','on');
ax1.XLabel.String = sprintf('Launch Epoch from %s [Days since ILD %d/%d/%d]',Planet_1, yyyyH,mmH,ddH);
ax1.YLabel.String = 'Time of Flight  [Days]';
ax1.Color = 'none';

ax2 = axes('position', get(ax1, 'position'),'Color','none'); 
[~,h2] = contour(ax2,Dt_Dep,TOF,JD_Arr-JD_DepH,'Showtext','on');
ax2.Color = 'none';

h1a.LineWidth = 1.5;
h1a.LineStyle = '-';

h1b.LineWidth = 3.0;
h1b.LineStyle = '-';

h2.LineWidth = 1.0;
h2.LineStyle = '--';

if strcmp(Planet_1,'Earth')
    h1a.LevelList = 0:5:C3_max;
    h1b.LevelList = C3_max;
    h1b.Color = 'k';
    h2.LevelList = 300:50:1500;      
elseif strcmp(Planet_1,'Jupiter') 
    if strcmp(Planet_2,'Neptune')
        h1a.LevelList = 0:5:C3_max;
        h1b.LevelList = C3_max;
        h1b.Color = 'k';
        h2.LevelList = 2000:100:5500;
    elseif strcmp(Planet_2,'Pluto')
        h1a.LevelList = [0:1:35,37.5,40:5:60,60:10:150,150:20:400];
        h1b.LevelList = 0:5:C3_max;
        h1b.Color = 'k';
%         h2.LevelList = 3000:250:10000;
    end
end
grid(ax1,'minor')
cax1 = colormap(ax1,'turbo');
cax2 = colormap(ax2,[(.1:.05:.8)',(.1:.05:.8)',(.1:.05:.8)']);
linkaxes([ax1,ax2])

%% %%%%%%%%%%%%%%%%%%%%%%%%%%    ARRIVAL   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subplot(1,2,2),hold on,grid on 
title(sprintf('%s to %s: Arrival C_3  [km^2/s^2]',Planet_1,Planet_2))

ax1 = gca;
[~,h1a]  = contour(ax1,Dt_Dep,TOF,C3_arr,'Showtext','on');
[~,h1b]  = contour(ax1,Dt_Dep,TOF,C3_dep);
ax1.XLabel.String = sprintf('Launch Epoch from %s [Days since ILD %d/%d/%d]',Planet_1,yyyyH,mmH,ddH);
ax1.YLabel.String = 'Time of Flight  [Days]';
ax1.Color = 'none';

ax2 = axes('position', get(ax1, 'position'),'Color','none'); 
[~,h2] = contour(ax2,Dt_Dep,TOF,JD_Arr-JD_DepH,'Showtext','on');
ax2.Color = 'none';

h1a.LineWidth = 1.5;
h1a.LineStyle = '-';

h1b.LineWidth = 3.0;
h1b.LineStyle = '-';

h2.LineWidth = 1.0;
h2.LineStyle = '--';


if strcmp(Planet_1,'Earth')
    if strcmp(Planet_2,'Jupiter') || strcmp(Planet_2,'Saturn')
        h1a.LevelList = [0:1:35,37.5,40:5:60,60:10:150,150:20:400];
        h1b.LevelList = C3_max;
        h1b.Color = 'k';
        h2.LevelList = 300:50:1500;       
    end
elseif strcmp(Planet_1,'Jupiter') 
    if strcmp(Planet_2,'Neptune')
        h1a.LevelList = [0:5:100,100:10:150,150:25:600];
        h1b.LevelList = C3_max;
        h1b.Color = 'k';
        h2.LevelList = 2000:100:5500;
    elseif strcmp(Planet_2,'Pluto')
        h1a.LevelList = [0:5:100,100:10:150,150:25:600];
        h1b.Color = 'none';
%         h2.LevelList = 3000:250:10000;
    end
end
grid(ax1,'minor')
cax1 = colormap(ax1,'turbo');
cax2 = colormap(ax2,[(.1:.05:.8)',(.1:.05:.8)',(.1:.05:.8)']);
linkaxes([ax1,ax2])
toc
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