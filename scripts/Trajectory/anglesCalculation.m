clc, clear, close all

tic

transferStatePath = append(pwd, '\..\data\', 'Transfer_States.csv');
venusStatePath = append(pwd, '\..\data\', 'Venus_States.csv');
dataTableTransfer = readtable(transferStatePath);
dataTableVenus = readtable(venusStatePath);

endStatesTransfer = [dataTableTransfer.X(end,end)...
dataTableTransfer.Y(end,end)... 
dataTableTransfer.Z(end,end)]';

endStatesVenus = [dataTableVenus.X(end,end)...
dataTableVenus.Y(end,end)...
dataTableVenus.Z(end,end)]';

angle = anglesOfStates(endStatesTransfer, endStatesVenus);

fprintf('The angle is %0.8f degrees \n', angle)

toc