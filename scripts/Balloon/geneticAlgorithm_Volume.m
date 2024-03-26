clc, clear, close all

% -------------MATLAB Script Information-------------
% Author Names: Trajectory Team 
% Team Number: 1
% Date: Spring 2024
% Tool Version: R2022a
% Purpose of Script: Genetic algorithm to calculate for idea volume 
% other .m files required: None
% other files required (not .m): output.csv

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   EDITABLE   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Genetic Algorithm for Optimizing Balloon Volume to Maximize Buoyancy Force on Venus

% Clear environment
clc;
clear;

% GA Parameters
populationSize = 50;
maxGenerations = 1000;
minVolume = 100; % Minimum volume in m^3
maxVolume = 950; % Maximum volume in m^3
tournamentSize = 2; % Tournament size for selection
mutationRate = 0.1; % Probability of mutation
mutationRange = 50; % Range of mutation
system_mass = 718.6953082 + 3423; % Total system mass in kg

% Initialize population
population = minVolume + (maxVolume - minVolume) .* rand(populationSize, 1);

% Main GA loop
for gen = 1:maxGenerations
    % Evaluate fitness
    fitnessScores = arrayfun(@(v) calculateBuoyancyForce(v, system_mass), population);
    
    % Selection
    selectedIndices = tournamentSelection(fitnessScores, populationSize / 2, tournamentSize);
    selectedPopulation = population(selectedIndices);
    
    % Crossover
    offspring = uniformCrossover(selectedPopulation, populationSize - length(selectedPopulation));
    
    % Mutation
    mutatedOffspring = mutate(offspring, minVolume, maxVolume, mutationRate, mutationRange);
    
    % Create new population
    population = [selectedPopulation; mutatedOffspring];
    
    % Check for convergence (optional, could implement a check here)
    
    % Logging for visibility
    [maxFitness, idx] = max(fitnessScores);
    disp(['Generation ', num2str(gen), ': Max Fitness = ', num2str(maxFitness), ' for Volume = ', num2str(population(idx))]);
    
    % Optional: Implement convergence check here
end

% Final result
[bestFitness, bestIndex] = max(fitnessScores);
bestVolume = population(bestIndex);
disp(['Best Volume: ', num2str(bestVolume), ' m^3, Best Fitness: ', num2str(bestFitness)]);

% Objective function: Calculate buoyancy force
function F_buoyancy = calculateBuoyancyForce(volume, system_mass)
    gravity = 8.87; % Gravity on Venus in m/s^2
    R = 188.92; % Specific gas constant for CO2 in J/(kg*K)
    P_surface = 92.10 * 101325; % Surface pressure in Pa
    T_surface = 737; % Surface temperature in Kelvin
    rho_surface = P_surface / (R * T_surface); % Density in kg/m^3
    F_buoyancy = rho_surface * gravity * volume; % Buoyancy force in N
    F_gravity = system_mass * gravity; % Gravitational force in N
    F_net = F_buoyancy - F_gravity; % Net buoyancy force in N
    F_buoyancy = F_net; % Using net buoyancy force as fitness
end

% Tournament Selection
function indices = tournamentSelection(fitnessScores, selectionSize, tournamentSize)
    populationSize = length(fitnessScores);
    indices = zeros(selectionSize, 1);
    for i = 1:selectionSize
        contestants = randi(populationSize, tournamentSize, 1);
        [~, bestIdx] = max(fitnessScores(contestants));
        indices(i) = contestants(bestIdx);
    end
end

% Uniform Crossover
function offspring = uniformCrossover(selectedPopulation, offspringSize)
    numParents = length(selectedPopulation);
    offspring = zeros(offspringSize, 1);
    for i = 1:offspringSize
        parent1 = selectedPopulation(randi(numParents));
        parent2 = selectedPopulation(randi(numParents));
        if rand() < 0.5
            offspring(i) = parent1;
        else
            offspring(i) = parent2;
        end
    end
end

% Mutation
function mutatedOffspring = mutate(offspring, minVolume, maxVolume, mutationRate, mutationRange)
    mutatedOffspring = offspring;
    for i = 1:length(offspring)
        if rand() < mutationRate
            mutation = (rand() - 0.5) * 2 * mutationRange;
            mutatedOffspring(i) = max(minVolume, min(mutatedOffspring(i) + mutation, maxVolume));
        end
    end
end
