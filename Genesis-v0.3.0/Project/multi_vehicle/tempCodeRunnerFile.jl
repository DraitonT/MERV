# Add vertical lines for separation times
vlines!(ax, [50], color=:black, linestyle=:dash, label=L"Separation (Heatshield)")  # Separation of child 1 at 50 seconds
vlines!(ax, [100], color=:black, linestyle=:dash, label=L"Separation (Aeroshell)")  # Separation of child 2 at 100 seconds