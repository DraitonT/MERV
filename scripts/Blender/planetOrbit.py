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

def create_textured_planet(name, radius, location, texture_path):
    # Create the planet sphere
    bpy.ops.mesh.primitive_uv_sphere_add(segments=64, ring_count=32, radius=radius, location=location)
    bpy.ops.object.shade_smooth()  # Set smooth shading
    planet = bpy.context.active_object
    planet.name = name

    # Set up the material with the texture
    mat = bpy.data.materials.new(name=name + "_Material")
    mat.use_nodes = True
    bsdf = mat.node_tree.nodes["Principled BSDF"]
    
    # Add the texture node
    texImage = mat.node_tree.nodes.new('ShaderNodeTexImage')
    texImage.image = bpy.data.images.load(texture_path)
    mat.node_tree.links.new(bsdf.inputs['Base Color'], texImage.outputs['Color'])

    # Set up the UV map for a sphere
    bpy.context.view_layer.objects.active = planet
    bpy.ops.object.mode_set(mode='EDIT')
    bpy.ops.uv.sphere_project()
    bpy.ops.object.mode_set(mode='OBJECT')

    planet.data.materials.append(mat)
    
    return planet

# Function to create a satellite
def create_satellite(name, radius, location):
    bpy.ops.mesh.primitive_uv_sphere_add(radius=radius, location=location)
    satellite = bpy.context.active_object
    satellite.name = name
    
    # Create a simple material for the satellite
    mat = bpy.data.materials.new(name=name + "_Material")
    mat.diffuse_color = (0.8, 0.8, 0.8, 1)  # Grey color, you can customize it
    satellite.data.materials.append(mat)

    bpy.ops.object.shade_smooth()  # Set smooth shading
    return satellite

# Function to propagatet the orbit
def animate_orbit(planet, positions, frame_start, frame_end):
    for i, position in enumerate(positions):
        frame = frame_start + i * ((frame_end - frame_start) / len(positions))
        planet.location = position
        planet.keyframe_insert(data_path="location", frame=frame)

# Function to import an STL file and scale it
def import_stl(filepath, object_name, scale_factor):
    bpy.ops.import_mesh.stl(filepath=filepath)
    imported_object = bpy.context.selected_objects[0]
    imported_object.name = object_name
    imported_object.scale *= scale_factor  # Apply scale factor
    bpy.ops.object.transform_apply(location=False, rotation=False, scale=True)  # Apply the scaling
    return imported_object

# Clear existing objects in the scene
bpy.ops.object.select_all(action='SELECT')
bpy.ops.object.delete(use_global=False)

# Get the directory of the current script file
script_dir = os.path.dirname(os.path.abspath(__file__))

# File paths to the CSV files
earth_csv_path = os.path.join(script_dir, '..', '..', 'data', 'Earth_states.csv')
venus_csv_path = os.path.join(script_dir, '..', '..', 'data', 'Venus_states.csv')

# Build the path to the texture files
earth_texture_path = os.path.join(script_dir, '..', '..', 'data', 'Textures', 'earth_texture.png')
venus_texture_path = os.path.join(script_dir, '..', '..', 'data', 'Textures', 'venusTexture.jpg')

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

# # Create Earth and Venus to scale
# earth = create_planet('Earth', radius=earth_radius, location=earth_location)
# venus = create_planet('Venus', radius=venus_radius, location=venus_location)

# Create Earth and Venus to scale with textures
earth = create_textured_planet('Earth', radius=earth_radius, location=earth_location, texture_path=earth_texture_path)
venus = create_textured_planet('Venus', radius=venus_radius, location=venus_location, texture_path=venus_texture_path)

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

# Animate the orbits 
animate_orbit(earth, earth_positions, frame_start=1, frame_end=250)
animate_orbit(venus, venus_positions, frame_start=1, frame_end=250)

# File path to the CSV file for the satellite's orbit
transfer_csv_path = os.path.join(script_dir, '..', '..', 'data', 'Transfer_States.csv')

# Load state data from CSV file for the satellite
transfer_positions = load_csv_data(transfer_csv_path)

# Use the first position for the initial placement of the satellite
satellite_location = transfer_positions[0]

# Define a radius for the satellite, smaller than the planets
satellite_radius = 5 / scale_factor  # Adjust as needed for visualization

# Create the satellite to scale
satellite = create_satellite('Satellite', radius=satellite_radius, location=satellite_location)

# Animate the satellite's orbit using the loaded positions
animate_orbit(satellite, transfer_positions, frame_start=1, frame_end=250)

# Set the end frame for the animation
bpy.context.scene.frame_end = 250

# Build paths to the STL files for the satellite components
aeroshell_path = os.path.join(script_dir, '..', '..', 'Models', 'Aeroshell - Aeroshell-1.STL')
heatshield_path = os.path.join(script_dir, '..', '..', 'Models', 'Aeroshell - Heatshield-2.STL')

satellite_scale_factor = 1/(scale_factor * 5)  # Adjust this as needed to make the satellite components visible

# Import the satellite components with adjusted scaling
aeroshell = import_stl(aeroshell_path, 'Aeroshell', satellite_scale_factor)
heatshield = import_stl(heatshield_path, 'Heatshield', satellite_scale_factor)

# Position the satellite components using the first entry from the transfer positions
aeroshell.location = satellite_location
heatshield.location = satellite_location

# # Parent satellite components to the main satellite object if needed
# aeroshell.parent = satellite
# heatshield.parent = satellite

# Apply the same animation to the satellite components
animate_orbit(aeroshell, transfer_positions, frame_start=1, frame_end=250)
animate_orbit(heatshield, transfer_positions, frame_start=1, frame_end=250)