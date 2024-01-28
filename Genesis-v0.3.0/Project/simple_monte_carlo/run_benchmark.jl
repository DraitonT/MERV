using Dispersions

include("case.jl")

dispersions = Dict(
    "initial_fpa" => RandomDispersion(Uniform(-40.0, -5.0), seed=1)
)

function run_benchmark()
    monte_carlo(SequentialEx(), dispersions, runs=1:10) do run_number, inputs
        run(inputs)
    end
end
