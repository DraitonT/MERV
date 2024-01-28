### A Pluto.jl notebook ###
# v0.19.9

using Markdown
using InteractiveUtils

# This Pluto notebook uses @bind for interactivity. When running this notebook outside of Pluto, the following 'mock version' of @bind gives bound variables a default value (instead of an error).
macro bind(def, element)
    quote
        local iv = try Base.loaded_modules[Base.PkgId(Base.UUID("6e696c72-6542-2067-7265-42206c756150"), "AbstractPlutoDingetjes")].Bonds.initial_value catch; b -> missing; end
        local el = $(esc(element))
        global $(esc(def)) = Core.applicable(Base.get, el) ? Base.get(el) : iv(el)
        el
    end
end

# ╔═╡ 3d77c44a-3984-11eb-0da6-a70bcef1cb38
begin
    import Pkg
    Pkg.activate(Base.current_project())
	using LinearAlgebra
	using CairoMakie
	using PlutoUI
	include("cases.jl")
end

# ╔═╡ 1910e500-3984-11eb-01b3-315b3b47375a
md"# A Pluto Notebook for Running Genesis

An alternative to a Genesis run script, like we wrote in the first part of this tutorial, would be to use a Pluto notebook. Pluto provides a much better 
environment for viewing and analyzing the trajectory data than a plain command-line. In addition, its ability to display Markdown text strings (like this one) enables you to record comments and observations for future use."

# ╔═╡ 86f3f01c-3984-11eb-3e4c-17633ca883a0
md"## Trajectory Integration"

# ╔═╡ aba5af36-55c5-11eb-3669-29f5e96630fe
md"""In addition to setting values in code cells, we can set them with UI elements. For example, let's set the bank angle as: $(@bind bank_angle NumberField(-180:1:180, default = 0)) °"""

# ╔═╡ 69b46ee5-b65c-4227-98ae-8e52e8c6a814
inputs = merge(
	default_inputs,
	(
		bank_angle = bank_angle,
	)
)

# ╔═╡ a1b88158-55c7-11eb-2adc-e967741035f0
md"Now we are ready to call Genesis to integrate a trajectory. Note that you can click the arrow icon to the left of `Dict` to explore the returned dictionary."

# ╔═╡ a8e7b014-3984-11eb-0f59-df542673c6ad
outputs = run(inputs)

# ╔═╡ c756d1a6-3984-11eb-3d0c-e73bf45958b1
md"""We can use string interpolation to embed values into your markdown cells. For example, we can note that the final altitude from the trajectory is $(outputs["h"][end]) m."""

# ╔═╡ 5af2f2c4-55c7-11eb-39ce-77e1130fe3ec
md"And of course, we can also make a plot."

# ╔═╡ f4fc6c9c-3984-11eb-30d3-d939309e6ded
begin
    f = Figure()
    ax = Axis(f[1, 1],
              xlabel = "Planet-Relative Velocity [km/s]",
              ylabel = "Altitude [km]")
    lines!(ax,
           norm.(outputs["vr"]) ./ 1000,
           outputs["h"] ./ 1000)
    f
end

# ╔═╡ Cell order:
# ╟─1910e500-3984-11eb-01b3-315b3b47375a
# ╠═3d77c44a-3984-11eb-0da6-a70bcef1cb38
# ╟─86f3f01c-3984-11eb-3e4c-17633ca883a0
# ╟─aba5af36-55c5-11eb-3669-29f5e96630fe
# ╠═69b46ee5-b65c-4227-98ae-8e52e8c6a814
# ╟─a1b88158-55c7-11eb-2adc-e967741035f0
# ╠═a8e7b014-3984-11eb-0f59-df542673c6ad
# ╟─c756d1a6-3984-11eb-3d0c-e73bf45958b1
# ╟─5af2f2c4-55c7-11eb-39ce-77e1130fe3ec
# ╟─f4fc6c9c-3984-11eb-30d3-d939309e6ded
