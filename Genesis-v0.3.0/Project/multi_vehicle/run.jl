using CairoMakie
using LaTeXStrings
using CSV
using DataFrames

include("case.jl")

out = run()

# Makes a dataframe from "out" dictionary
time_history_data = DataFrame(
    t = out["t"],
    h_parent = out["h_parent"],
    h_child1 = out["h_child1"],
    h_child2 = out["h_child2"]
)

# Write the DataFrame to a CSV file
CSV.write("M:\\dev\\MATLAB\\ARO 4811L\\Genesis-v0.3.0\\Project\\multi_vehicle\\output.csv", time_history_data)

f = Figure()

ax = Axis(f[1, 1],
          xlabel=L"Time \ [s]",  # Using LaTeXStrings for LaTeX-like labels
          ylabel=L"Altitude \ [m]")
          
# Plot for the parent
lines!(ax,
       out["t"],
       out["h_parent"],
       label=L"Parent")  # Using LaTeXStrings for LaTeX-like labels

# Plot for the first child
lines!(ax,
       out["t"],
       out["h_child1"],  # This should match the key for the first child's altitude
       label=L"Child 1")  # Using LaTeXStrings for LaTeX-like labels

# Plot for the second child
lines!(ax,
       out["t"],
       out["h_child2"],  # This should match the key for the second child's altitude
       label=L"Child 2")  # Using LaTeXStrings for LaTeX-like labels

# Add vertical lines for separation times
vlines!(ax, [50], color=:black, linestyle=:dash, label=L"Separation Child 1")  # Separation of child 1 at 50 seconds
vlines!(ax, [100], color=:black, linestyle=:dash, label=L"Separation Child 2")  # Separation of child 2 at 100 seconds

axislegend()  # This will automatically pick up the labels from the lines! calls

display(f)