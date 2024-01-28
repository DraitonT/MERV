# Create a local registry containing the packages from this release of Genesis.

using Pkg
using LocalRegistry

registry_path = abspath(@__DIR__, "Genesis-v0.3.0")

create_registry(registry_path,
                registry_path,
                description = "Genesis v0.3.0 release registry")

for name in ["AngleEmulators", "MonitorLogging", "MassElements", "Dispersions", "ThrottleEmulators", "Rotations", "Subtend", "Events", "Genesis", "GravityModels", "TimeEvents", "UserParameters", "TimeHistoryLogging", "Geodesy", "Schedulers", "AerothermalIndicators", "ConicOrbits", "NextTimes", "Ellipsoids", "Quaternions", "Integrators"]
    package_path = abspath(@__DIR__, "packages", name)
    register(package_path,
             repo = package_path,
             push = false,
             registry = registry_path)
end
