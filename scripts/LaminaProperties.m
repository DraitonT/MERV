% -------------MATLAB Script Information-------------
% Author Names: Eric Ruf
% Team Number: 1
% Date: Spring 2024
% Tool Version: R2022a
% Purpose of Script: Calculates the composite longitudal and longitude
% characterisitics (laminar properties)
% other .m files required: None
% other files required (not .m): 

function LaminaProperties(Vf, Ef, Gf, vf, Vm, Em, Gm, vm)
    %% LAMINAPROPERTIES
    % Inputs:
    % Vf: Volume fraction of fiber
    % Ef: Fiber modulus 
    % Gf: Shear fiber modulus 
    % vf: Poisson Ratio
    % Vm: Volume fraction of matrix (i.e., epoxy, binding agent)
    % Em: Matrix modulus
    % Gm: Shear matrix modulus
    % vm: Poission Ration matrix modulus

    % Formulas

    E1 = (Ef * Vf) + (Em * Vm);                % Msi
    E2 = (Ef * Em) / ((Vm * Ef) + (Vf * Em));  % Msi
    G12 = (Gm * Gf) / ((Vm * Gf) + (Vf * Gm)); % Msi
    v12 = (Vm * vm) + (Vf * vf);               
    v21 = (v12 / E1) * E2;

    % Display calculated lamina properties

    fprintf('\n');
    fprintf('Calculated Lamina Properties\n');
    fprintf('\n');
    fprintf('Longitudinal Modulus (E1): %.4f\n', E1);
    fprintf('Transverse Modulus (E2): %.4f\n', E2);
    fprintf('Shear Modulus (G12): %.4f\n', G12);
    fprintf('Poisson''s Ratio (ν12): %.4f\n', v12);
    fprintf('Poisson''s Ratio (ν21): %.4f\n', v21);

end