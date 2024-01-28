using Genesis

function run(inputs)
    time = Time()

    integ = DP5(time)

    executive = Executive(integ,
                          stop_time = 1000)

    earth = Earth(time)

    vehicle = Vehicle(earth)
    add!(executive, vehicle)

    S = 1.0
    cd = 1.0
    m = inputs.ballistic_coefficient * cd * S
    cl = inputs.lift_to_drag_ratio * cd

    set_mass_properties!(vehicle,
                         mass = m)

    configure!(vehicle,
               ComputedAcceleration(vehicle))

    set_position!(vehicle,
                  planetodetic_altitude = 125 * 1000,
                  planetodetic_latitude = 0,
                  longitude = 0)

    set_velocity!(vehicle,
                  planet_relative_velocity = 10 * 1000,
                  planetodetic_planet_relative_flight_path_angle = inputs.initial_flight_path_angle,
                  planetodetic_planet_relative_azimuth = 0)

    configure!(vehicle,
               PrescribedAttitude(vehicle,
                                  angle_of_attack = 55.0 / 180 * pi,
                                  sideslip_angle = 0.0,
                                  bank_angle = inputs.bank_angle))

    configure!(vehicle,
               StandardAtmosphere(vehicle))

    configure!(vehicle,
               NonsymmetricAerodynamics(vehicle,
                                        reference_area = S,
                                        cd = cd,
                                        cl = cl,
                                        cs = 0))

    add!(executive,
         Event(condition = function()
                   γ = planetodetic_planet_relative_flight_path_angle(vehicle)
                   h = planetodetic_altitude(vehicle)
                   γ > 0 && h > 125 * 1000
               end,
               action = () -> stop!(executive)))

    add!(executive,
         Event(condition = () -> planetodetic_altitude(vehicle) <= 0,
               action = () -> stop!(executive)))

    time_history = TimeHistoryLoggingGroup(
        "t" => () -> dynamic_time(time),
        "h" => () -> planetodetic_altitude(vehicle),
        "vr" => () -> planet_relative_velocity(vehicle, frame = PCPF(earth)),
        "ha" => () -> radius_of_apoapsis(vehicle) - equatorial_radius(earth),
        "ε" => () -> specific_energy(vehicle))
    add!(executive, time_history)

    run!(executive)

    data(time_history)
end

default_inputs = (
    lift_to_drag_ratio = 0.5,
    ballistic_coefficient = 300,
    initial_flight_path_angle = -7/180*pi,
)
