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

# Function to create a marker at the specified location
def create_marker(name, position, marker_color):
    # Create a sphere at the given position
    bpy.ops.mesh.primitive_uv_sphere_add(location=position, radius=0.1)  # Adjust radius as needed
    marker = bpy.context.object
    marker.name = name
    
    # Create a new material with the given color
    mat = bpy.data.materials.new(name=name + "_Material")
    mat.diffuse_color = marker_color + (1,)  # RGBA, A needs to be 1 for full opacity
    marker.data.materials.append(mat)

def create_glowing_orbit_material():
    mat = bpy.data.materials.new(name="GlowingOrbitMaterial")
    mat.use_nodes = True
    nodes = mat.node_tree.nodes
    # Remove default
    for node in nodes:
        nodes.remove(node)

    # Add an emission node
    emission = nodes.new(type='ShaderNodeEmission')
    emission.inputs['Color'].default_value = (0, 1, 1, 1)  # Bright cyan color
    emission.inputs['Strength'].default_value = 10  # Adjust the strength as needed

    # Add material output
    material_output = nodes.new(type='ShaderNodeOutputMaterial')

    # Link nodes
    links = mat.node_tree.links
    link = links.new(emission.outputs['Emission'], material_output.inputs['Surface'])
    
    return mat

def animate_orbit(planet, positions, frame_start, frame_end):
    # Create a new curve
    bpy.ops.curve.primitive_nurbs_path_add()
    curve = bpy.context.object
    curve.name = planet.name + "_Orbit"
    
    # Switch to 'EDIT' mode to modify the curve
    bpy.ops.object.mode_set(mode='EDIT')
    
    # Get the curve's spline to add points to
    spline = curve.data.splines[0]
    spline.type = 'POLY'  # Changing the spline type to 'POLY'
    spline.points.add(len(positions) - 1)  # Adding the necessary number of points

    # Add the positions to the spline points
    for i, position in enumerate(positions):
        spline.points[i].co = (position[0], position[1], position[2], 1)

    # Switch back to 'OBJECT' mode
    bpy.ops.object.mode_set(mode='OBJECT')
    
    # Animate the planet along the path
    for i, position in enumerate(positions):
        frame = frame_start + i * ((frame_end - frame_start) / len(positions))
        planet.location = position
        planet.keyframe_insert(data_path="location", frame=frame)
    
    # Create markers for start and end positions
    create_marker(planet.name + "_Start", positions[0], marker_color=(1, 0, 0))
    create_marker(planet.name + "_End", positions[-1], marker_color=(0, 1, 0))
    
    # After the curve is created and named:
    orbit_curve_name = planet.name + "_Orbit"
    curve = bpy.data.objects[orbit_curve_name]  # Get the curve object using its name
    
    # Add a glowing material to the curve
    mat = bpy.data.materials.new(name="GlowingOrbitMaterial")
    mat.use_nodes = True
    nodes = mat.node_tree.nodes
    nodes.clear()

    # Create emission node
    emission = nodes.new(type='ShaderNodeEmission')  
    emission.inputs['Color'].default_value = (0.8, 0.5, 0.2, 1)  # Orange color, you can change this
    emission.inputs['Strength'].default_value = 10  # Glow strength

    # Attach emission node to material output
    material_output = nodes.new(type='ShaderNodeOutputMaterial')
    mat.node_tree.links.new(emission.outputs['Emission'], material_output.inputs['Surface'])

    # Assign material to curve
    if curve.data.materials:
        curve.data.materials[0] = mat
    else:
        curve.data.materials.append(mat)

    # Increase the curve's bevel to make it more visible
    curve.data.bevel_depth = 0.05  # Adjust this value as needed
    curve.data.bevel_resolution = 0  # Smoothness of the bevel

    # Optional: enable 'Bloom' in Eevee for a glowing effect
    bpy.context.scene.eevee.use_bloom = True

    return curve


# Function to import an STL file and scale it
def import_stl(filepath, object_name, scale_factor):
    bpy.ops.import_mesh.stl(filepath=filepath)
    imported_object = bpy.context.selected_objects[0]
    imported_object.name = object_name
    imported_object.scale *= scale_factor  # Apply scale factor
    bpy.ops.object.transform_apply(location=False, rotation=False, scale=True)  # Apply the scaling
    return imported_object

# Function to set up the space background
def setup_background(backgroundFilePath):
    # Use an environment texture for the world background
    bpy.context.scene.world.use_nodes = True
    bg = bpy.context.scene.world.node_tree.nodes.new('ShaderNodeTexEnvironment')
    bg.image = bpy.data.images.load(backgroundFilePath)  # Update the path to your space background image
    bg.location = -300, 0
    output = bpy.context.scene.world.node_tree.nodes.get('World Output')
    bpy.context.scene.world.node_tree.links.new(bg.outputs['Color'], output.inputs['Surface'])

# Function to create and set up a camera that follows an object
def setup_follow_camera(target_object, radius, location=(0, 0, 10)):
    # Create the camera
    bpy.ops.object.camera_add(location=location)
    camera = bpy.context.active_object
    camera.name = 'FollowCamera'
    
    # Add a constraint to make the camera follow the target object
    constraint = camera.constraints.new(type='TRACK_TO')
    constraint.target = target_object
    constraint.track_axis = 'TRACK_NEGATIVE_Z'
    constraint.up_axis = 'UP_Y'
    
    # Parent the camera to an empty at the target location to control the distance
    bpy.ops.object.empty_add(type='PLAIN_AXES', location=target_object.location)
    empty = bpy.context.active_object
    empty.name = 'CameraTargetEmpty'
    camera.parent = empty
    
    # Set the radius (distance to the target)
    camera.location.x = radius
    camera.location.y = radius
    camera.location.z = radius
    
    # Set the camera as the active camera for the scene
    bpy.context.scene.camera = camera

# Function to create a sun lamp
def create_sun(location, strength=5, sun_color=(1, 1, 1)):
    bpy.ops.object.light_add(type='SUN', location=location)
    sun = bpy.context.active_object
    sun.data.energy = strength
    sun.data.color = sun_color
    return sun

# Function to keyframe the visibility of an object
def keyframe_visibility(obj, start_frame, end_frame, is_visible):
    # Hide the object in the viewport
    obj.hide_viewport = not is_visible
    obj.keyframe_insert(data_path="hide_viewport", frame=start_frame - 1)
    obj.hide_viewport = is_visible
    obj.keyframe_insert(data_path="hide_viewport", frame=start_frame)
    
    # Hide the object in the render
    obj.hide_render = not is_visible
    obj.keyframe_insert(data_path="hide_render", frame=start_frame - 1)
    obj.hide_render = is_visible
    obj.keyframe_insert(data_path="hide_render", frame=start_frame)

    # Keyframe the end of the visibility transition
    obj.hide_viewport = not is_visible
    obj.keyframe_insert(data_path="hide_viewport", frame=end_frame)
    obj.hide_render = not is_visible
    obj.keyframe_insert(data_path="hide_render", frame=end_frame)

# Function to animate the camera's location
def animate_camera_location(camera, start_location, end_location, start_frame, end_frame):
    camera.location = start_location
    camera.keyframe_insert(data_path="location", frame=start_frame)
    camera.location = end_location
    camera.keyframe_insert(data_path="location", frame=end_frame)

# Set the end frame for the animation
bpy.context.scene.frame_end = 750

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

backgroundFilePath = os.path.join(script_dir, '..', '..', 'data', 'Textures', 'Space_background.jpg')

setup_background(backgroundFilePath)

# Set up the follow camera to track the aeroshell
setup_follow_camera(aeroshell, radius=10)  # 'radius' is the distance from the target object

# Create the sun lamp
sun_location = (100, 100, 100)  # Position the sun lamp at a suitable location
sun_strength = 5  # Adjust the strength as needed
sun_color = (1, 1, 0.95)  # Sunlight color can be slightly yellow
sun = create_sun(sun_location, strength=sun_strength, sun_color=sun_color) 

# (obj, start_frame, end_frame, is_visible)
scene1_start_frame = 1
scene1_end_frame = 250

keyframe_visibility(aeroshell, scene1_start_frame, scene1_end_frame, False)
keyframe_visibility(heatshield, scene1_start_frame, scene1_end_frame, False)
keyframe_visibility(earth, scene1_start_frame, scene1_end_frame, False)
keyframe_visibility(venus, scene1_start_frame, scene1_end_frame, False)

# At frame 250, switch to the new Venus surface and background
new_scene_start_frame = 251



