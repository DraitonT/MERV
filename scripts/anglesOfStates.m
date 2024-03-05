function angle = anglesOfStates(vector1, vector2)
% ANGLESOFSTATES 
%
% Example:
% transferStatePath = append(pwd, '\..\data\', 'Transfer_States.csv');
% venusStatePath = append(pwd, '\..\data\', 'Venus_States.csv');
% dataTableTransfer = readtable(transferStatePath);
% dataTableVenus = readtable(venusStatePath);
% 
% endStatesTransfer = [dataTableTransfer.X(end,end)...
% dataTableTransfer.Y(end,end)... 
% dataTableTransfer.Z(end,end)]';
% 
% endStatesVenus = [dataTableVenus.X(end,end)...
% dataTableVenus.Y(end,end)...
% dataTableVenus.Z(end,end)]';

angle = acosd(dot(vector1, vector2) / (vecnorm(vector1) * vecnorm(vector2)));

end