# Genesis v0.3.0 Release

Genesis is a flight mechanics simulation for mission design. It is applicable to
planetary trajectories, including atmospheric and powered flight.

We are still actively developing Genesis. There are significant changes planned,
which may not be backwards compatible. However, a number of groups have
expressed interest in learning more about what we are doing with Genesis, so we
have released this beta version as a preview. We would love to talk with you
more about Genesis and get your feedback.

## Installing

There are two ways to use this release. For either method, you'll need to move
this release to a good permanent location on your system.

### Using `Pkg.develop`

The first method simply adds every package in the release to a user's project by
calling `Pkg.develop` on all of the packages. This is the simplest method.

The user simply activates his project and then runs the `include` function,
passing the path to the `setup.jl` script that is located at the root of this
release.

### Using a local registry

The second method is a little more complicated to set up, but should prove
easier for users in the end. With this technique, the person who installs this
release in its permanent location can create a local registry containing all of
the packages from this release. Users then need only add the registry, and then
they can add Genesis as they would any other package.

To create the local registry:
1. Navigate to the root of this release
2. Start Julia
3. Enter the package manager REPL mode by hitting the `]` key
4. Activate that project in the root of the release by running the `activate .`
   command
5. Install the project's packages by running the `instantiate` command
6. Hit backspace to return to the normal Julia REPL mode
7. Run `include("create-registry.jl")` to create the local registry

Users can then use the Julia package manager to add the registry:
```julia
pkg> registry add /path/to/Genesis-v0.3.0/Genesis-v0.3.0
```
Then, Genesis and its dependencies can easily be added to the user's active
Julia project:
```julia
pkg> add Genesis
```

## Learning

The release includes HTML documentation for the various packages. We suggest you
start with the
[Genesis documentation](packages/Genesis/docs/build/external/index.html).

## Changes

See a summary of [recent changes](packages/Genesis/CHANGELOG.md).

If you are updating from the previous release, please run Julia with the `--depwarn=yes` flag.
This will cause Julia to print warnings if you are using any deprecated features.
