using Genesis
using Genesis: density
using LinearAlgebra
using Rotations
using StaticArrays

function run(inputs)
    time = Time()

    integ = DP5(time)

    executive = Executive(integ,
                          stop_time=1000)

    earth = Earth(time)

    vehicle = Vehicle(earth)
    add!(executive, vehicle)

    set_mass_properties!(vehicle,
                         mass=1000)

    configure!(vehicle,
               ComputedAcceleration(vehicle))

    configure!(vehicle,
               PrescribedAngularVelocity(vehicle,
                                         angular_velocity=SVector(0.1, 0.0, 0.0) / 180 * pi))

    set_position!(vehicle,
                  planetodetic_altitude=100 * 1000,
                  planetodetic_latitude=0,
                  longitude=0)

    set_velocity!(vehicle,
                  planet_relative_velocity=6000,
                  planetodetic_planet_relative_flight_path_angle=inputs["initial_fpa"] / 180 * pi,
                  planetodetic_planet_relative_azimuth=0)

    configure!(vehicle, StandardAtmosphere(vehicle))

    configure!(vehicle,
               NonsymmetricAerodynamics(vehicle,
                                        reference_area=1.0,
                                        cd=1.0,
                                        cl=0,
                                        cs=0))

    add!(executive,
         Event(condition=() -> planetodetic_altitude(vehicle) <= 0,
               action=() -> stop!(executive)))

    time_history = TimeHistoryLoggingGroup(
        "t" => () -> dynamic_time(time),
        "h" => () -> planetodetic_altitude(vehicle),
        "vr" => () -> planet_relative_velocity(vehicle, frame=PCPF(earth)),
        "γr" => () -> planetodetic_planet_relative_flight_path_angle(vehicle),
        "euler_zyx" => () -> angles(RotationSequence{:ZYX}(rotation(PCI(earth), Body(vehicle)))),
        "ω" => () -> angular_velocity(vehicle, frame=Body(vehicle)),
        "ρ" => () -> density(atmosphere(vehicle)))
    add!(executive, time_history)

    run!(executive)

    data(time_history)
end
