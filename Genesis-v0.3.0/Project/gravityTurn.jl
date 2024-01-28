using Genesis
using LinearAlgebra
using Rotations
using StaticArrays

mutable struct GravityTurnGuidance
    R_command::RotationMatrix{Float64}
end

GravityTurnGuidance() = GravityTurnGuidance(RotationMatrix(I))

function run()
    time = Time()

    integ = DP5(time)

    executive = Executive(integ,
                          stop_time=350)

    moon = Moon(time)

    vehicle = Vehicle(moon)
    add!(executive, vehicle)

    set_mass_properties!(vehicle,
                         mass=1000)

    configure!(vehicle,
               ComputedAcceleration(vehicle))

    guidance = GravityTurnGuidance()

    configure!(vehicle,
               PrescribedAttitude(vehicle,
                                  rotation=() -> guidance.R_command,
                                  frame=PDNED(vehicle)))

    set_orbital_elements!(vehicle,
                          radius_of_periapsis=equatorial_radius(moon) + 16e3,
                          radius_of_apoapsis=equatorial_radius(moon) + 100e3,
                          inclination=(90 - 27.1) / 180 * pi,
                          longitude_of_ascending_node=-6.0845 / 180 * pi,
                          argument_of_periapsis=16.5 / 180 * pi,
                          true_anomaly=0)

    tank = PointMassTank(vehicle,
                         propellant_mass=1000)

    rocket = MonoPropellantRocket(vehicle,
                                  vacuum_thrust=5000 * 0.8,
                                  specific_impulse=300,
                                  thrust_direction=[-1, 0, 0],
                                  thrust_frame=Structure(vehicle),
                                  propellant_source=tank)

    set_firing_command!(rocket, true)

    configure!(executive,
               FlightSoftware([FlightSoftwareRateGroup(dt=1//50) do
                                   ψ = planetodetic_inertial_azimuth(vehicle)
                                   γ = planetodetic_inertial_flight_path_angle(vehicle)
                                   guidance.R_command = RotationMatrix(RotationSequence{:ZY}(ψ, γ))
                               end]))

    time_history = TimeHistoryLoggingGroup(
        "t" => () -> dynamic_time(time),
        "m" => () -> total_mass(vehicle),
        "mp" => () -> propellant_mass(tank),
        "h" => () -> planetodetic_altitude(vehicle),
        "r" => () -> Genesis.position(vehicle, frame=PCI(moon)),
        "vi" => () -> inertial_velocity(vehicle, frame=PCI(moon)),
        "T" => () -> thrust(rocket),
        "a" => () -> sensed_acceleration(vehicle, frame=PCI(moon)))
    add!(executive, time_history)

    touchdown = Event(condition=() -> planetodetic_altitude(vehicle) <= 1e3,
                      action=function()
                          @info "Touchdown" time=dynamic_time(time) vi=norm(inertial_velocity(vehicle, frame=PCI(moon)))
                          stop!(executive)
                      end)
    add!(executive, touchdown)

    run!(executive)

    data(time_history)
end
