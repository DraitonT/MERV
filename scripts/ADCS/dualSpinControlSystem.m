clc;clear;close all
tic
% -------------MATLAB Script Information-------------
% Author Names: V^3
% Date: 11/4/23
% Tool Version: R2022a
% Purpose of Script: Dual-spin bias momentum satellite control system design (Pitch)
% other .m files required: None
% other files required (not .m): dualSpinCSSimulink.slx

%{
Initial Conditons Inputs:
Location of Satellite and Orbital Properities
Initial S/C Attitude
Rol and Yaw Controller Va;ues
Initial Angular Velocity 
Interia Moments
Interia Products
Disturbance Torque
Control Torque
Misc. Control System Variables

Example Output:
6 Plots:
Idealized Pitch Actuator Root Locus
Actual POitch Actuator Root Locus
Time vs. Uncontrolled Response
Time vs. Pitch
Time vs. Roll and Yaw
Time vs. Idealized and Actual Pitch

%}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   EDITABLE   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 0.0 Setting-up Initial Conditions
    %% 0.1 Location of Satellite and orbital properties
        R_e = 6052;                         %[km]
        z_a = 50;                          %[km]
        z_p = 50;                          %[km]
        R_c = R_e + z_a;                    %[km] Location of satellite
        mu = 324859;                      %[km^3/s^2] Gravitational Constant of Earth
        w_0_b1 = 0;                         %tD-Intitial angular velocity
        w_0_b2 = sqrt(mu/R_c^3);            %tD-Intitial angular velocity
        w_0_b3 = 0;                         %tD-Intitial angular velocity
        w_B = [w_0_b1; w_0_b2; w_0_b3];
    %% 0.2 Initial S/C Attitude
        psi_0   = 0;    %[deg] Yaw
        theta_0 = 0;    %[deg] Pitch
        phi_0   = 0;    %[deg] Roll
        phi_dot = 0;    %[deg/s] 
        psi_dot = 0;
        theta_dot = 0;
    %% 0.3 Roll and Yaw Controller Values
        kd = 1.2;       %Derivative gain
        kp = 3;         %Proportional gain
    %% 0.4 Initial Angular Velocity in B-RF Coords
        p_0_B = 0;     %[deg/s]
        q_0_B = 10;     %[deg/s]
        r_0_B = 300;    %[deg/s] 
%     %% 0.5 Inertia Moments
%         Ixx = 44451.50/10000; %[kg*m^2]
%         Iyy = 45161.45/10000; %[kg*m^2]
%         Izz = 44017.00/10000; %[kg*m^2]
%     %% 0.6 Inertia Products
%         Ixy = 378.08/10000; %[kg*m^2]
%         Ixz = 108.92/10000; %[kg*m^2]
%         Iyz = -3273.58/10000; %[kg*m^2]

    %% 0.5 Inertia Moments
        Ixx = 44451.50; %[kg*m^2]
        Iyy = 45161.45; %[kg*m^2]
        Izz = 44017.00; %[kg*m^2]
    %% 0.6 Inertia Products
        Ixy = 378.08/10000; %[kg*m^2]
        Ixz = 108.92/10000; %[kg*m^2]
        Iyz = -3273.58/10000; %[kg*m^2]
    %% 0.7 Inertia Matrix wrt to C.M. in B-RF coords.
        I_B = [ Ixx  -Ixy  -Ixz;
       -Ixy   Iyy  -Iyz;
       -Ixz  -Iyz   Izz];
    %% 0.8 Disturbance Torque
        tD_x = 1E-5; %[Nm]
        tD_y = 1E-5; %[Nm]
        tD_z = 1E-5; %[Nm]
    %% 0.9 Control Torque
        tC_x = 0; %[Nm]
        tC_y = 0; %[Nm]
        tC_z = 0; %[Nm]
    %% 0.10 Misc. Control System Variables 
        zeta = 0.7;     %Damping ratio 
        t_s = 299;      %[seconds]
        wN = 4/(zeta * t_s);
        mP = 25;        %[in degrees]
        p0 = 100+mP; 
        ss_Pitch = 0.1; %[in degrees]
        alpha = 0.9;
        T = 5;          %Time constant
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   EDITABLE   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        kg = 3 * w_0_b2^2 * (Ixx-Izz);
    %% 0.11 Defining Momentum Wheel Characteristics
        h_s = I_B * w_B;
        h_dot = 0;

%% 1.0  Pitch Controller
    %% 1.1 Gain Values, Damping Ratio, Natural Freq.
        zeta_pitch = log(p0/100)/(sqrt(pi^2 + (log(p0/100))^2));
        zeta_pitch = 0.4;
        wN_pitch = sqrt((tD_y/(Iyy*(ss_Pitch*pi/180))) + 6 * w_0_b2^2 * ((Ixx-Izz)/Iyy));
        kp_p = wN_pitch^2*Iyy - 3*w_0_b2^2 * (Ixx - Izz);
        kd_p = 2 * Iyy * zeta_pitch * wN_pitch;
    %% 1.2 Root Locus (Problem 2 | Idealized)
        figure(1)
        s0TermsO = 3*w_0_b2^2*(Ixx-Izz)+kp_p;
        sysOriginal = tf([kd kp], [Iyy, kd_p, s0TermsO]);
        hold on
        rlocus(sysOriginal)
        title("Idealized Pitch Actuator Root Locus", 'Interpreter', 'latex')
        hold off
    %% 1.3 Root Locus (Problem 4 | Actual) 
        zeta_pitch = log(p0/100)/(sqrt(pi^2 + (log(p0/100))^2));
        zeta_pitch = 0.7;
        wN_pitch = sqrt((tD_y/(Iyy*(ss_Pitch*pi/180))) + 6 * w_0_b2^2 * ((Ixx-Izz)/Iyy));
        kp_p = wN_pitch^2*Iyy - 3*w_0_b2^2 * (Ixx - Izz);
        kd_p = 2 * Iyy * zeta_pitch * wN_pitch;
        
        s3Terms = Iyy *T;
        s2Terms = Iyy;
        s1Terms = 3*w_0_b2^2*(Ixx-Izz)*T +kd_p;
        s0Terms = 3*w_0_b2^2*(Ixx-Izz) + kp_p;
        
        sys = tf([1], [T 1]);
        figure(2)
        sys = tf([kd_p kp_p], [s3Terms, s2Terms, s1Terms, s0Terms]);
        hold on
        rlocus(sys)
        title("Actual Pitch Actuator Root Locus", 'Interpreter', 'latex')
        hold off
%% 2.0 Simulink Model Outputs
    out = sim('dualSpinCSSimulink.slx');
    phi = out.eulerAngles2.signals(1).values;
    theta = out.eulerAngles2.signals(2).values;
    psi = out.eulerAngles2.signals(3).values;

    pitchIdeal = out.problem5Comparsion.signals(1).values;
    pitchActual = out.problem5Comparsion.signals(2).values;
    %%
        figure(3)
        hold on
        plot(out.UnControlled.time,out.UnControlled.signals(1).values)
        plot(out.UnControlled.time,out.UnControlled.signals(2).values)
        plot(out.UnControlled.time,out.UnControlled.signals(3).values)
        title("Time vs Uncontrolled Response", 'Interpreter', 'latex')
        ylabel("Degrees", 'Interpreter', 'latex')
        xlabel("Time (in seconds)", 'Interpreter', 'latex')
        grid on
        legend ("$\phi(t)$","$\theta(t)$","$\psi(t)$", 'Interpreter', 'latex', 'Location', 'southeast')
        hold off
    %% 2.1 Time vs Pitch Plot
        figure(4)
        hold on
        plot(out.tout,theta)
        title("Time vs. $\theta(t)$", 'Interpreter', 'latex')
        ylabel("Degrees", 'Interpreter', 'latex')
        xlabel("Time (in seconds)", 'Interpreter', 'latex')
        grid on
    
        legend ("$\theta(t)$", 'Interpreter', 'latex', 'Location', 'southeast')
        hold off
    %% 2.2 Time vs Roll and Yaw Plot
        figure(5)
        title("Time vs. $\phi(t)$ and $\psi(t)$", 'Interpreter', 'latex')
        ylabel("Degrees", 'Interpreter', 'latex')
        xlabel("Time (in seconds)", 'Interpreter', 'latex')
        grid on
    
        hold on
        plot(out.tout, phi)
        plot (out.tout, psi)
        legend ("$\phi(t)$","$\psi(t)$", 'Interpreter', 'latex', 'Location', 'southeast')
        hold off
    %% 2.2 Idealized vs Actual Pitch
        figure(6)
        hold on
        title("Time vs. Ideal and Actual Pitch")
        grid on
        plot(out.tout,pitchIdeal)
        plot(out.tout, pitchActual)
        ylabel("Degrees", 'Interpreter', 'latex')
        xlabel("Time (in seconds)", 'Interpreter', 'latex')
        legend ("Idealized", "Actual", 'Location', 'southeast')

    toc