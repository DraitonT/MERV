clc, clear, close all
% -------------MATLAB Script Information-------------
% Author Names: Michael Quach
% Team Number: 1
% Date: 09/23/23
% Tool Version: R2022a
% Purpose of Script: Utilizing patch conics method to calculate the Hohmann Transfer's characteristics between two celestial bodies within the solar systems
% other .m files required: 
% other files required (not .m):

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   EDITABLE   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 0.0 Seledct Design to View
    designA = true; % Earth to Gateway to Venus and back
    designB = false;  % Earth to Venus directly and back
%% 1.0 Initial Conditions
    departurePlanetPhase1= "Earth"; %Departure planet
    targetPlanetPhase1 = "Earth's Moon";  %Arrival planet

    departurePlanetPhase2= "Earth's Moon"; %Departure planet
    targetPlanetPhase2 = "Venus";  %Arrival planet

    departurePlanetPhase3 = "Venus"; %Departure planet
    targetPlanetPhase3 = "Earth's Moon";  %Arrival planet

    departurePlanetPhase4 = "Earth's Moon"; %Departure planet
    targetPlanetPhase4 = "Earth";  %Arrival planet

    % Specific Impulse
    Isp_dep = 340;                  %[s] (Departure)
    Isp_arr = 400;                  %[s] (Arrival)

    % Earth parameters
    zp_Earth = 700;        %[km] (@ periapsis)
    za_Earth = 800;        %[km] (@ apoapsis)

    % Moon parameters
    zp_Moon = 400;      %[km] (@ periapsis)
    za_Moon = 500;     %[km] (@ apoapsis)

    % Venus parameters
    zp_Venus = 800;      %[km] (@ periapsis)
    za_Venus = 1000;     %[km] (@ apoapsis)
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   EDITABLE   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  DO NOT EDIT  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if designA
    [DV_dep_earthMoon, DV_arr_earthMoon, transferTime_earthMoon] = Orbital.interplantarySolver(departurePlanetPhase1, targetPlanetPhase1, Isp_dep, Isp_arr, zp_Earth, za_Earth, zp_Moon, za_Moon);
    [DV_dep_moonVenus, DV_arr_moonVenus, transferTime_moonVenus] = Orbital.interplantarySolver(departurePlanetPhase2, targetPlanetPhase2, Isp_dep, Isp_arr, zp_Moon, za_Moon, za_Venus, zp_Venus);
    [DV_dep_venusMoon, DV_arr_venusMoon, transferTime_venusMoon] = Orbital.interplantarySolver(departurePlanetPhase3, targetPlanetPhase3, Isp_dep, Isp_arr, zp_Venus, za_Venus, za_Moon, zp_Moon);
    [DV_dep_moonEarth, DV_arr_moonEarth, transferTime_moonEarth] = Orbital.interplantarySolver(departurePlanetPhase4, targetPlanetPhase4, Isp_dep, Isp_arr, zp_Moon, za_Moon, za_Earth, zp_Earth);
    
    total_dv = DV_dep_earthMoon + DV_arr_earthMoon + DV_dep_moonVenus + DV_arr_moonVenus + DV_dep_venusMoon + DV_arr_venusMoon + DV_dep_moonEarth + DV_arr_moonEarth;
    totalTime = transferTime_earthMoon + transferTime_moonVenus + transferTime_venusMoon + transferTime_moonEarth;
    
    fprintf('Phase 1: Time from %s to %s \n', departurePlanetPhase1, targetPlanetPhase1);
    fprintf('-----------------------------\n');
    fprintf('Delta V = %.5e km/s \n', DV_dep_earthMoon);
    fprintf('Transfer Time = %.5e days \n', transferTime_earthMoon/86400)
    fprintf('-----------------------------\n');
    
    fprintf('Phase 2: Time from %s to %s \n', departurePlanetPhase2, targetPlanetPhase2);
    fprintf('-----------------------------\n');
    fprintf('Delta V = %.5e km/s \n', DV_dep_moonVenus);
    fprintf('Transfer Time = %.5e days \n', transferTime_moonVenus/86400)
    fprintf('-----------------------------\n');
    
    fprintf('Phase 3: Time from %s to %s \n', departurePlanetPhase3, targetPlanetPhase3);
    fprintf('-----------------------------\n');
    fprintf('Delta V = %.5e km/s \n', DV_arr_venusMoon);
    fprintf('Transfer Time = %.5e days \n', transferTime_venusMoon/86400)
    fprintf('-----------------------------\n');
    
    fprintf('Phase 4: Time from %s to %s \n', departurePlanetPhase4, targetPlanetPhase4);
    fprintf('-----------------------------\n');
    fprintf('Delta V = %.5e km/s \n', DV_arr_moonEarth);
    fprintf('Transfer Time = %.5e days \n', transferTime_moonEarth/86400)
    fprintf('-----------------------------\n');
    
    fprintf('\n -----------------------------\n');
    fprintf('Total Delta V: %.5e km/s \n', total_dv)
    fprintf('Total Transfer Time: %.5e days\n', totalTime/86400)
end

if designB
    [DV_dep_earthVenus, DV_arr_earthVenus, transferTime_earthVenus] = Orbital.interplantarySolver(departurePlanetPhase1, targetPlanetPhase2, Isp_dep, Isp_arr, zp_Earth, za_Earth, zp_Venus, za_Venus);
    [DV_dep_venusEarth, DV_arr_venusEarth, transferTime_venusEarth] = Orbital.interplantarySolver(targetPlanetPhase2, departurePlanetPhase1, Isp_dep, Isp_arr, zp_Venus, za_Venus, zp_Earth, za_Earth);
    
    total_dv = DV_dep_earthVenus+DV_arr_earthVenus+DV_dep_venusEarth+DV_arr_venusEarth;
    totalTime = (transferTime_earthVenus + transferTime_venusEarth)/86400;

    fprintf('\n -----------------------------\n');
    fprintf('Total Delta V: %.5e km/s \n', total_dv)
    fprintf('Total Transfer Time: %.5e days\n', totalTime)
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  DO NOT EDIT  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%