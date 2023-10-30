clc, clear, close all
% -------------MATLAB Script Information-------------
% Author Names: Team Venus
% Team Number: 1
% Date: 09/23/23
% Tool Version: R2022a
% Purpose of Script: Utilizing patch conics method to calculate the Hohmann Transfer's characteristics between two celestial bodies within the solar systems
% other .m files required: 
% other files required (not .m):

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   EDITABLE   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 0.0 Seledct Design to View
    designA = true; % Earth to Gateway to Venus and back
    designB = true;  % Earth to Venus directly and back
%% 1.0 Initial Conditions
    departurePlanetPhase1= "Venus"; %Departure planet
    targetPlanetPhase1 = "Venus";  %Arrival planet

%     departurePlanetPhase2= "Venus"; %Departure planet
%     targetPlanetPhase2 = "Venus";  %Arrival planet
% 
%     departurePlanetPhase3 = "Venus"; %Departure planet
%     targetPlanetPhase3 = "Earth's Moon";  %Arrival planet
% 
%     departurePlanetPhase4 = "Earth's Moon"; %Departure planet
%     targetPlanetPhase4 = "Earth";  %Arrival planet

    % Specific Impulse
    Isp_dep = 340;                  %[s] (Departure)
    Isp_arr = 400;                  %[s] (Arrival)

    % Earth parameters
    zp_Venus_Parking = 200;        %[km] (@ periapsis)
    za_Venus_Parking = 200;        %[km] (@ apoapsis)

    % Moon parameters
    zp_Venus_Mission = 50;      %[km] (@ periapsis)
    za_Venus_Mission = 50;     %[km] (@ apoapsis)
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   EDITABLE   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  DO NOT EDIT  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if designA
    [DV_dep_earthMoon, DV_arr_earthMoon, transferTime_earthMoon] = Orbital.interplantarySolver2(departurePlanetPhase1, targetPlanetPhase1, Isp_dep, Isp_arr, zp_Venus_Parking, za_Venus_Parking, zp_Venus_Mission, za_Venus_Mission);
%     [DV_dep_moonVenus, DV_arr_moonVenus, transferTime_moonVenus] = Orbital.interplantarySolver(departurePlanetPhase2, targetPlanetPhase2, Isp_dep, Isp_arr, zp_Venus_Mission, za_Venus_Mission, za_Venus, zp_Venus);
%     [DV_dep_venusMoon, DV_arr_venusMoon, transferTime_venusMoon] = Orbital.interplantarySolver(departurePlanetPhase3, targetPlanetPhase3, Isp_dep, Isp_arr, zp_Venus, za_Venus, za_Venus_Mission, zp_Venus_Mission);
%     [DV_dep_moonEarth, DV_arr_moonEarth, transferTime_moonEarth] = Orbital.interplantarySolver(departurePlanetPhase4, targetPlanetPhase4, Isp_dep, Isp_arr, zp_Venus_Mission, za_Venus_Mission, za_Venus_Parking, zp_Venus_Parking);
%     
%     total_dv = DV_dep_earthMoon + DV_arr_earthMoon + DV_dep_moonVenus + DV_arr_moonVenus + DV_dep_venusMoon + DV_arr_venusMoon + DV_dep_moonEarth + DV_arr_moonEarth;
%     totalTime = transferTime_earthMoon + transferTime_moonVenus + transferTime_venusMoon + transferTime_moonEarth;
    
    fprintf('Phase 1: Time from %s to %s \n', departurePlanetPhase1, targetPlanetPhase1);
    fprintf('-----------------------------\n');
    fprintf('Delta V = %.5e km/s \n', DV_dep_earthMoon);
    fprintf('Transfer Time = %.5e days \n', transferTime_earthMoon/86400)
    fprintf('-----------------------------\n');
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  DO NOT EDIT  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%