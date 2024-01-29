using CairoMakie
using LaTeXStrings

include("case.jl")

out = run()

f = Figure()

ax = Axis(f[1, 1],
          xlabel=L"Time \ [s]",  # Using LaTeXStrings for LaTeX-like labels
          ylabel=L"Altitude \ [m]")
lines!(ax,
       out["t"],
       out["h_parent"],
       label=L"Parent")  # Using LaTeXStrings for LaTeX-like labels
lines!(ax,
       out["t"],
       out["h_child"],
       label=L"Child")  # Using LaTeXStrings for LaTeX-like labels
axislegend()

display(f)
