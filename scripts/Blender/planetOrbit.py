import bpy
import csv
import os

# Function to load CSV data and extract positions
def load_csv_data(csv_file_path):
    positions = []
    with open(csv_file_path, newline='') as csvfile:
        datareader = csv.reader(csvfile, delimiter=',')
        next(datareader)  # Skip the header row
        for row in datareader:
            # Extract the position data (assuming it is in columns 1, 2, 3)
            x = float(row[1]) / 1e6  # Scale the position down by a factor of 1e6
            y = float(row[2]) / 1e6
            z = float(row[3]) / 1e6
            positions.append((x, y, z))
    return positions

# Function to create a sphere
def create_planet(name, radius, location):
    bpy.ops.mesh.primitive_uv_sphere_add(radius=radius, location=location)
    planet = bpy.context.active_object
    planet.name = name
    return planet

# Clear existing objects in the scene
bpy.ops.object.select_all(action='SELECT')
bpy.ops.object.delete(use_global=False)

# Get the directory of the current script file
script_dir = os.path.dirname(os.path.abspath(__file__))

# File paths to the CSV files
earth_csv_path = os.path.join(script_dir, '..', '..', 'data', 'Earth_states.csv')
venus_csv_path = os.path.join(script_dir, '..', '..', 'data', 'Venus_states.csv')

# Load state data from CSV files
earth_positions = load_csv_data(earth_csv_path)
venus_positions = load_csv_data(venus_csv_path)

# Use the first position for the initial placement
earth_location = earth_positions[0]
venus_location = venus_positions[0]

# Real-world radii of the planets in kilometers
# Scale down by a factor (e.g., 1,000,000) to make them visible in Blender
scale_factor = 1e3
earth_radius = 6371 / scale_factor  # Earth's radius in km
venus_radius = 6052 / scale_factor  # Venus's radius in km

# Create Earth and Venus to scale
earth = create_planet('Earth', radius=earth_radius, location=earth_location)
venus = create_planet('Venus', radius=venus_radius, location=venus_location)

# Set the shading to smooth
bpy.ops.object.shade_smooth()

# Add material to Earth (blue)
earth_material = bpy.data.materials.new(name="Earth_Material")
earth_material.diffuse_color = (0.0, 0.0, 1.0, 1)
earth.data.materials.append(earth_material)

# Add material to Venus (orange)
venus_material = bpy.data.materials.new(name="Venus_Material")
venus_material.diffuse_color = (1, 0.5, 0, 1)
venus.data.materials.append(venus_material)

# Assuming a very simplistic orbit just to show movement
# This does not represent actual celestial mechanics
def animate_orbit(planet, positions, frame_start, frame_end):
    for i, position in enumerate(positions):
        frame = frame_start + i * ((frame_end - frame_start) / len(positions))
        planet.location = position
        planet.keyframe_insert(data_path="location", frame=frame)

# Animate the orbits
animate_orbit(earth, earth_positions, frame_start=1, frame_end=250)
animate_orbit(venus, venus_positions, frame_start=1, frame_end=250)

# Set the end frame for the animation
bpy.context.scene.frame_end = 250
