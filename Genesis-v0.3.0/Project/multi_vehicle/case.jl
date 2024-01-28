using Genesis
using StaticArrays
using LinearAlgebra

function run()
    time = Time()

    integ = DP5(time)

    executive = Executive(integ,
                          stop_time=100)

    earth = Earth(time)

    parent = Vehicle(earth)
    add!(executive, parent)

    set_mass_properties!(parent,
                         mass=10677.78)

    configure!(parent,
               ComputedAcceleration(parent))

    configure!(parent,
               PrescribedAttitude(parent,
                                  rotation=I,
                                  frame=PCI(earth)))

    set_position!(parent,
                  planetodetic_altitude=10 * 1000,
                  planetodetic_latitude=0,
                  longitude=0)

    set_velocity!(parent,
                  planet_relative_velocity=100,
                  planetodetic_planet_relative_flight_path_angle=-10/180*pi,
                  planetodetic_planet_relative_azimuth=0)

    child = Vehicle(earth)
    add!(executive, child)

    set_mass_properties!(child,
                         mass=1389.26)

    attach!(child, parent=parent)

    tank = PointMassTank(child,
                         propellant_mass=900)

    rocket = MonoPropellantRocket(child,
                                  vacuum_thrust=10000,
                                  specific_impulse=300,
                                  thrust_direction=[1, 0, 0],
                                  thrust_frame=Structure(child),
                                  propellant_source=tank)

    set_firing_command!(rocket, true)

    separation = TimeEvent(50) do
        @info "Separation"
        detach!(child)
        configure!(child,
                   PrescribedAttitude(child,
                                      rotation=I,
                                      frame=PCI(earth)))
    end
    add!(executive, separation)

    time_history = TimeHistoryLoggingGroup(
        "t" => () -> dynamic_time(time),
        "h_parent" => () -> planetodetic_altitude(parent),
        "h_child" => () -> planetodetic_altitude(child))
    add!(executive, time_history)

    run!(executive)

    data(time_history)
end
