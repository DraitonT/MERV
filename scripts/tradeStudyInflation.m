% -------------MATLAB Script Information-------------
% Author Names: Trajectory Team
% Team Number: 1
% Date: Spring 2024
% Tool Version: R2022a
% Purpose of Script: Trade study to solve for the ideal inflation piping
% system (i.e., volume of balloon and inflation rate)
% Genetic algorithm 
% other .m files required: None
% other files required (not .m): output.csv

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   EDITABLE   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Inputs for the genetic algorithms 
popSize = 100;
numVars = 3; % Number of variables 
lowerBound = [0, 300]; % Lower bounds for the buoyancy force 
upperBound = [400, 700]; % Upper bounds for the buoyancy force 
maxGenerations = 100; 
crossOverRate = 0.7; 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   EDITABLE   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  DO NOT EDIT  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Initialize Population 
population = initializePopulation(popSize, numVars, lowerBound, upperBound); 

for gen = 1:maxGenerations
    % Evaluate fitness 
    fitness = evaluateFitness(population);

    % Selection 

    % Crossover 

    % Mutation 

    % Optional

    % Report 

end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  DO NOT EDIT  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%