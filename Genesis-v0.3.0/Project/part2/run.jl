using LinearAlgebra
using CairoMakie

include("cases.jl")

inputs = merge(
    default_inputs,
    (
        bank_angle = 0,
    )
)

outputs = run(inputs)

@show outputs["ha"][end]

f = Figure()
ax = Axis(f[1, 1],
          xlabel = "Planet-Relative Velocity [km/s]",
          ylabel = "Altitude [km]")
lines!(ax,
       norm.(outputs["vr"]) ./ 1000,
       outputs["h"] ./ 1000)
f
