# Activate a Julia project (environment) and then run this script to configure the project to
# use this version of Genesis.

using Pkg

let package_path = (name) -> abspath(@__DIR__, "packages", name)
    for name in ["NextTimes", "Quaternions", "Rotations", "Subtend", "Schedulers", "Ellipsoids", "AngleEmulators", "MonitorLogging", "MassElements", "ThrottleEmulators", "Events", "GravityModels", "TimeEvents", "UserParameters", "TimeHistoryLogging", "Geodesy", "AerothermalIndicators", "ConicOrbits", "Integrators", "Dispersions", "Genesis"]
        Pkg.develop(path=package_path(name))
    end
end

Pkg.build()
