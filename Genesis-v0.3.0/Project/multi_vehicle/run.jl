using CairoMakie

include("case.jl")

out = run()

f = Figure()

ax = Axis(f[1, 1],
          xlabel="Time [s]",
          ylabel="Altitude [m]")
lines!(ax,
       out["t"],
       out["h_parent"],
       label="Parent")
lines!(ax,
       out["t"],
       out["h_child"],
       label="Child")
axislegend()

f

display(f)
