using CairoMakie
using Dispersions

include("case.jl")

dispersions = Dict(
    "initial_fpa" => RandomDispersion(Uniform(-40.0, -5.0), seed=1)
)

results = monte_carlo(SequentialEx(), dispersions, runs=1:10) do run_number, inputs
    run(inputs)
end

f = Figure()

ax = Axis(f[1, 1],
          xlabel="Planet-Relative Velocity [m/s]",
          ylabel="Altitude [m]")
for (run, data) in results
    lines!(ax,
           norm.(data["vr"]),
           data["h"],
           label="Run $run")
end
Legend(f[1, 2], ax)

f

display(f)
