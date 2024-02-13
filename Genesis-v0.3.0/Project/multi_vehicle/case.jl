using Genesis
using StaticArrays
using LinearAlgebra

function run()
    time = Time()

    integ = DP5(time)

    executive = Executive(integ, stop_time=200 )
    
    venus = Europa(time)

    parent = Vehicle(venus)
    add!(executive, parent)

    set_mass_properties!(parent, mass=10677.78)

    configure!(parent, ComputedAcceleration(parent))

    configure!(parent,
               PrescribedAttitude(parent,
                                  rotation=I,
                                  frame=PCI(venus)))

    set_position!(parent,
                  planetodetic_altitude=100 * 1000,
                  planetodetic_latitude=0,
                  longitude=0)

    set_velocity!(parent,
                  planet_relative_velocity=3029,
                  planetodetic_planet_relative_flight_path_angle=-10/180*pi,
                  planetodetic_planet_relative_azimuth=0)

    # First child setup
    child1 = Vehicle(venus)
    add!(executive, child1)

    set_mass_properties!(child1, mass=1389.26)

    attach!(child1, parent=parent)

    tank1 = PointMassTank(child1, propellant_mass=900)

    rocket1 = MonoPropellantRocket(child1,
                                   vacuum_thrust=10000,
                                   specific_impulse=300,
                                   thrust_direction=[1, 0, 0],
                                   thrust_frame=Structure(child1),
                                   propellant_source=tank1)

    set_firing_command!(rocket1, true)

    separation1 = TimeEvent(50) do
        @info "Separation of child1"
        detach!(child1)
        configure!(child1,
                   PrescribedAttitude(child1,
                                      rotation=I,
                                      frame=PCI(venus)))
    end
    add!(executive, separation1)

    # Additional child setup
    child2 = Vehicle(venus)
    add!(executive, child2)

    set_mass_properties!(child2, mass=500)  # Set the mass for child2

    attach!(child2, parent=parent)

    # Adding a rocket to child2 just like child1
    tank2 = PointMassTank(child2, propellant_mass=900)  # Assuming same mass as child1's tank

    rocket2 = MonoPropellantRocket(child2,
                                   vacuum_thrust=10000,  # Assuming same thrust as child1's rocket
                                   specific_impulse=300,  # Assuming same specific impulse as child1's rocket
                                   thrust_direction=[1, 0, 0],  # Assuming same thrust direction as child1's rocket
                                   thrust_frame=Structure(child2),
                                   propellant_source=tank2)

    set_firing_command!(rocket2, true)  # Assuming we want to fire child2's rocket immediately as well

    # Separation event for the second child
    separation2 = TimeEvent(100) do
        @info "Separation of child2"
        detach!(child2)
        configure!(child2,
                   PrescribedAttitude(child2,
                                      rotation=I,
                                      frame=PCI(venus)))
    end
    add!(executive, separation2)

     # Additional child setup 3
     child3 = Vehicle(venus)
     add!(executive, child3)
 
     set_mass_properties!(child3, mass=500)  # Set the mass for child3
 
     attach!(child3, parent=parent)
 
     # Adding a rocket to child3 just like child1
     tank2 = PointMassTank(child3, propellant_mass=900)  # Assuming same mass as child1's tank
 
     rocket2 = MonoPropellantRocket(child3,
                                    vacuum_thrust=10000,  # Assuming same thrust as child1's rocket
                                    specific_impulse=300,  # Assuming same specific impulse as child1's rocket
                                    thrust_direction=[1, 0, 0],  # Assuming same thrust direction as child1's rocket
                                    thrust_frame=Structure(child3),
                                    propellant_source=tank2)
 
     set_firing_command!(rocket2, true)  # Assuming we want to fire child3's rocket immediately as well
 
     # Separation event for the third child
     separation3 = TimeEvent(120) do
         @info "Separation of child3"
         detach!(child3)
         configure!(child3,
                    PrescribedAttitude(child3,
                                       rotation=I,
                                       frame=PCI(venus)))
     end
     add!(executive, separation3)

    # Time history setup
    time_history = TimeHistoryLoggingGroup(
        "t" => () -> dynamic_time(time),
        "Aeroshell" => () -> planetodetic_altitude(parent),
        "Heat_shield" => () -> planetodetic_altitude(child1),
        "Payload" => () -> planetodetic_altitude(child2), # Logging altitude of child2
        "Chute" => () -> planetodetic_altitude(child3))  # Logging altitude of child3
    add!(executive, time_history)

    # Running the simulation
    run!(executive)

    # Gather and return the data
    data(time_history)
end

# Call the run function and capture the output
output = run()