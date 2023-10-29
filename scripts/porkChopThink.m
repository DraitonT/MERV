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
% [r1_vec,VP1_dep] = planetEphemeris(JD_Dep','Sun',Planet_1,'432t');  
 % Creating column vectors
 %% Replace with Parsed Out
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
%     [r2_vec,VP2_arr] = planetEphemeris(JD_Arr(k,:)','Sun',Planet_2,'432t');
  %% Replace with parsed out 
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