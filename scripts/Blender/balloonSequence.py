import bpy
import csv
import os
from mathutils import Vector
import random
import math
from bpy_extras.image_utils import load_image

# Function to import an OBJ file and scale it
def import_obj(filepath, object_name, scale_factor, location=None):
    bpy.ops.wm.obj_import(filepath=filepath)
    imported_object = bpy.context.selected_objects[0]
    imported_object.name = object_name
    imported_object.scale *= scale_factor

    # Apply the scale
    bpy.ops.object.transform_apply(location=False, rotation=False, scale=True)

    if location is not None:
        # Assuming that the object's origin is where it should be in the local space
        # If not, you might need to set the origin to the geometry or a specific point first
        imported_object.location = Vector(location)
        
        # Now apply the location
        bpy.ops.object.transform_apply(location=True, rotation=False, scale=False)
  
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

# Function to load CSV data
def load_csv_data(filepath):
    with open(filepath, 'r') as csvfile:
        csvreader = csv.reader(csvfile)
        data = list(csvreader)
    return data

# Function to update text object per frame
def update_text(scene):
    frame = scene.frame_current
    try:
        # Assuming first column is frame number, second is the value to display
        value = csv_data[frame][1]
        text_object = bpy.data.objects['Text']
        text_object.data.body = value
        print(f"Frame {frame}: {value}")  # For debugging purposes
    except IndexError as e:
        # No data for this frame, or end of data reached
        print(f"IndexError for frame {frame}: {e}")  # For debugging purposes
    except Exception as e:
        print(f"Unexpected error for frame {frame}: {e}")  # For debugging purposes

# Function to create a particle system to simulate dust
def create_dust_particle_system(emitter_object, particle_count, start_frame, end_frame, dust_particle_shape):
    # Create a new particle system for the emitter object
    emitter_object.modifiers.new(name="DustParticles", type='PARTICLE_SYSTEM')
    particle_system = emitter_object.particle_systems[-1]  # get the newly created particle system
    ps_settings = particle_system.settings
    ps_settings.count = particle_count
    ps_settings.frame_start = start_frame
    ps_settings.frame_end = end_frame
    ps_settings.lifetime = 400  # Adjust as needed
    ps_settings.lifetime_random = 0.5
    ps_settings.factor_random = 1
    ps_settings.physics_type = 'NEWTON'
    ps_settings.use_rotations = True
    ps_settings.use_dynamic_rotation = True
    ps_settings.render_type = 'OBJECT'  # Set render type to OBJECT
    ps_settings.instance_object = dust_particle_shape  # Set the instance object to the dust particle shape

    # Adjust the scale and randomness of the particles
    ps_settings.particle_size = 3  # Set the scale of the particles
    ps_settings.size_random = 1  # Set the scale randomness

    # Modify gravity influence and other settings for a floating dust effect
    ps_settings.effector_weights.gravity = 0  # Reducing gravity's influence
    # Additional settings like brownian, drag, etc. can be set here

    return particle_system


# Function to add a turbulence force field
def add_turbulence_force_field(strength=1, size=1):
    bpy.ops.object.effector_add(type='FORCE', enter_editmode=False, align='WORLD', location=(0, 0, 0))
    turbulence_field = bpy.context.object
    turbulence_field.field.type = 'TURBULENCE'
    turbulence_field.field.strength = strength
    turbulence_field.field.size = size
    turbulence_field.name = 'TurbulenceField'
    return turbulence_field

# Function to create a dust particle shape
def create_dust_particle_shape():
    bpy.ops.mesh.primitive_ico_sphere_add(subdivisions=1, radius=0.02)
    dust_particle = bpy.context.object
    dust_particle.name = 'DustParticleShape'
    return dust_particle

# Function to create a dust material with variable transparency and color
def create_dust_material():
    mat = bpy.data.materials.new(name="DustMaterial")
    mat.use_nodes = True
    nodes = mat.node_tree.nodes
    links = mat.node_tree.links
    
    # Remove default nodes
    for node in nodes:
        nodes.remove(node)

    # Create a Mix Shader Node to mix Transparent and Diffuse shaders
    mix_shader = nodes.new(type='ShaderNodeMixShader')
    
    # Create a Transparent BSDF Node
    transparent_bsdf = nodes.new(type='ShaderNodeBsdfTransparent')
    transparent_bsdf.inputs['Color'].default_value = (1, 1, 1, 0.1)  # Almost fully transparent

    # Create a Diffuse BSDF Node
    diffuse_bsdf = nodes.new(type='ShaderNodeBsdfDiffuse')
    diffuse_bsdf.inputs['Color'].default_value = (0.8, 0.65, 0.4, 1)  # Dust color

    # Create a Noise Texture Node to vary the factor of the mix shader
    noise_tex = nodes.new(type='ShaderNodeTexNoise')
    noise_tex.inputs['Scale'].default_value = 500  # High scale for fine-grained noise

    # Create Material Output Node
    material_output = nodes.new(type='ShaderNodeOutputMaterial')

    # Link nodes together
    links.new(noise_tex.outputs['Fac'], mix_shader.inputs['Fac'])
    links.new(transparent_bsdf.outputs['BSDF'], mix_shader.inputs[1])
    links.new(diffuse_bsdf.outputs['BSDF'], mix_shader.inputs[2])
    links.new(mix_shader.outputs['Shader'], material_output.inputs['Surface'])

    return mat
# Function to create a sun lamp
def create_sun(location, strength=5, sun_color=(1, 1, 1)):
    bpy.ops.object.light_add(type='SUN', location=location)
    sun = bpy.context.active_object
    sun.data.energy = strength
    sun.data.color = sun_color
    return sun

# Define a function to create a dashboard with multiple data points
def create_dashboard(csv_data, start_frame, end_frame):
    # Create a dictionary to hold the text objects
    dashboard_texts = {}
    
    # Get the number of data points
    num_data_points = len(csv_data[0]) - 1  # assuming the first column is the frame number
    
    for i in range(num_data_points):
        # Create text object for each data point
        bpy.ops.object.text_add(location=(0, -i * 0.3, 0))  # Adjust Y position for each text
        text_obj = bpy.context.object
        text_obj.name = f"DataText_{i}"
        text_obj.data.body = f"Data Point {i}: 0"
        text_obj.data.align_x = 'LEFT'
        text_obj.data.size = 0.2  # Adjust to fit the display screen
        dashboard_texts[i] = text_obj
    
    # Function to update text objects per frame
    def update_dashboard(scene):
        frame = scene.frame_current - start_frame
        if frame >= 0 and frame < len(csv_data):
            # Update each text object with the corresponding data point
            for i, text_obj in dashboard_texts.items():
                # Assuming the CSV columns are in the correct order
                text_obj.data.body = f"Data Point {i}: {csv_data[frame][i+1]}"
    
    # Remove existing handlers that start with "update_dashboard"
    for handler in bpy.app.handlers.frame_change_pre:
        if "update_dashboard" in str(handler):
            bpy.app.handlers.frame_change_pre.remove(handler)
    
    # Append the handler
    bpy.app.handlers.frame_change_pre.append(update_dashboard)

def update_balloon_altitude(scene):
    frame = scene.frame_current
    try:
        # Retrieve the balloon object
        balloon = bpy.data.objects['Balloon']
        
        # Retrieve the altitude value from the CSV for the current frame and scale it
        # Assuming the altitude in the CSV is given in meters and the Blender scene scale is 1 Blender unit = 1 meter
        altitude = float(csv_data[frame][1])  # Convert from m to Blender units if needed

        # Introduce horizontal movement based on noise or other algorithms
        # This is a simple random walk example; replace with your own logic
        balloon.location.x += -0.5 # Random change in x
        balloon.location.y += 0  # Random change in y

        # Set the altitude
        balloon.location.z = altitude

        # Insert keyframes for location to record the movement
        balloon.keyframe_insert(data_path="location", frame=frame)

    except IndexError as e:
        # Handle the case where the frame number exceeds
        #  the number of entries in the CSV
        pass
    except Exception as e:
        # Handle other exceptions
        print(f"An error occurred: {e}")

# Function to create and set up a camera that follows an object
def setup_follow_camera(target_object, radius, location=(-200, -30, 31), fov_degrees=90):
    # Create the camera
    bpy.ops.object.camera_add(location=location)
    camera = bpy.context.active_object
    camera.name = 'FollowCamera'
    
    # Add a constraint to make the camera follow the target object
    # constraint = camera.constraints.new(type='TRACK_TO')
    # constraint.target = target_object
    # constraint.track_axis = 'TRACK_NEGATIVE_Z'
    # constraint.up_axis = 'UP_Y'
    
    # Parent the camera to an empty at the target location to control the distance
    # bpy.ops.object.empty_add(type='PLAIN_AXES', location=target_object.location)
    # empty = bpy.context.active_object
    # empty.name = 'CameraTargetEmpty'
    # camera.parent = empty
    
    # # Set the radius (distance to the target)
    # camera.location.x = radius
    # camera.location.y = radius
    # camera.location.z = radius
    
    # Set the FOV of the camera
    camera.data.lens = camera.data.sensor_width / (2 * math.tan(math.radians(fov_degrees) / 2))

    # Set the camera as the active camera for the scene
    bpy.context.scene.camera = camera

    return camera

# Function to set up the dashboard elements
def setup_dashboard(camera, csv_data):
    # Create an empty object at the camera's location
    bpy.ops.object.empty_add(type='PLAIN_AXES', location=camera.location)
    dashboard_empty = bpy.context.active_object
    dashboard_empty.name = 'DashboardEmpty'

    # Create text objects and other dashboard elements here
    # For example, create a text object for altitude
    bpy.ops.object.text_add(location=(0, 0, 0))  # Adjust location as needed
    text_obj = bpy.context.object
    text_obj.name = 'AltitudeText'
    text_obj.data.body = 'Altitude: 0'
    text_obj.parent = dashboard_empty  # Parent to dashboard empty

    # Repeat for other dashboard elements, positioning them relative to the empty

    # Position the dashboard in front of the camera
    dashboard_empty.parent = camera
    dashboard_empty.location = (0, 0, -5)  # Move 5 units in front of the camera

    return dashboard_empty

def create_and_setup_dashboard(csv_data, camera, start_frame, end_frame, text_color=(0, 1, 0, 1)):  # RGBA color, white by default
    # Create an empty object at the camera's location for the dashboard
    bpy.ops.object.empty_add(type='PLAIN_AXES', location=camera.location)
    dashboard_empty = bpy.context.active_object
    dashboard_empty.name = 'DashboardEmpty'
    
    # Create a new material for the text color
    text_mat = bpy.data.materials.new(name="TextMaterial")
    text_mat.use_nodes = True
    bsdf = text_mat.node_tree.nodes.get('Principled BSDF')
    bsdf.inputs['Base Color'].default_value = text_color  # Set the color of the material
    
    # Create a dictionary to hold the text objects
    dashboard_texts = {}
    
    # Get the number of data points
    num_data_points = len(csv_data[0])  # assuming the first column is the frame number
    flightCharacteristicList = csv_data[0]
    for i in range(num_data_points):
        # Create text object for each data point and parent to dashboard empty
        bpy.ops.object.text_add(location=(0, -i * 0.3, 0))  # Adjust Y position for each text
        text_obj = bpy.context.object
        currentChar = flightCharacteristicList[i]
        text_obj.name = f"{currentChar}"
        text_obj.data.body = f"{currentChar}: 0"
        text_obj.data.align_x = 'LEFT'
        text_obj.data.size = 0.4  # Adjust to fit the display screen
        text_obj.parent = dashboard_empty  # Parent to dashboard empty

        # Assign the color material to the text object
        if text_obj.data.materials:
            text_obj.data.materials[0] = text_mat
        else:
            text_obj.data.materials.append(text_mat)

        dashboard_texts[i] = text_obj
    
    # Position the dashboard in front of the camera
    dashboard_empty.parent = camera
    dashboard_empty.location = (-7, 0, -7)  # Move 5 units in front of the camera
    
    # Function to update text objects per frame
    def update_dashboard(scene):
        frame = scene.frame_current - start_frame
        flightCharacteristicList = csv_data[0]
        if frame >= 0 and frame < len(csv_data):
            # Update each text object with the corresponding data point
            for i, text_obj in dashboard_texts.items():
                # Assuming the CSV columns are in the correct order
                currentChar = flightCharacteristicList[i]
                text_obj.data.body = f"{currentChar}: {csv_data[frame][i]}"
    
    # Remove existing handlers that start with "update_dashboard"
    for handler in bpy.app.handlers.frame_change_pre:
        if "update_dashboard" in str(handler):
            bpy.app.handlers.frame_change_pre.remove(handler)
    
    # Append the handler
    bpy.app.handlers.frame_change_pre.append(update_dashboard)

    return dashboard_empty

def import_obj_and_apply_texture(filepath, object_name, scale_factor, location, texture_path):
    # Import the OBJ file
    bpy.ops.wm.obj_import(filepath=filepath)
    imported_object = bpy.context.selected_objects[0]
    imported_object.name = object_name
    imported_object.scale *= scale_factor

    # Apply the scale
    bpy.ops.object.transform_apply(location=False, rotation=False, scale=True)

    if location is not None:
        imported_object.location = Vector(location)
        bpy.ops.object.transform_apply(location=True, rotation=False, scale=False)

    # Create a new material with a texture
    mat = bpy.data.materials.new(name=object_name + "_Material")
    mat.use_nodes = True
    bsdf = mat.node_tree.nodes.get('Principled BSDF')
    texture_node = mat.node_tree.nodes.new('ShaderNodeTexImage')
    
    # Load the texture image
    texture_image = load_image(texture_path)
    texture_node.image = texture_image
    
    # Link the texture node to the BSDF node
    mat.node_tree.links.new(bsdf.inputs['Base Color'], texture_node.outputs['Color'])
    
    # Assign the material to the object
    if imported_object.data.materials:
        imported_object.data.materials[0] = mat
    else:
        imported_object.data.materials.append(mat)

    return imported_object

# Function to create and set up a static camera
def setup_static_camera(location=(100, 5, 10), fov_degrees=90):
    # Create the camera
    bpy.ops.object.camera_add(location=location)
    camera = bpy.context.active_object
    camera.name = 'StaticCamera'
    
    # Set the FOV of the camera
    camera.data.lens = camera.data.sensor_width / (2 * math.tan(math.radians(fov_degrees) / 2))

    # Set the camera as the active camera for the scene
    bpy.context.scene.camera = camera

    return camera

   
def import_obj_follow_camera(filepath, camera, location, scale_factor=1):
    # Step 1: Import the OBJ file
    bpy.ops.wm.obj_import(filepath=filepath)
    imported_obj = bpy.context.selected_objects[0]  # Assuming the OBJ only contains one object
    imported_obj.scale *= scale_factor  # Adjust scale if necessary

    # Apply the scale
    bpy.ops.object.transform_apply(location=False, rotation=False, scale=True)

    # Step 2: Create an empty object at the camera's location
    bpy.ops.object.empty_add(type='PLAIN_AXES', location=camera.location)
    follow_empty = bpy.context.active_object
    follow_empty.name = 'FollowEmpty'

    # Step 3: Parent the imported OBJ to the empty object
    imported_obj.parent = follow_empty

    # Step 4: Make the empty object follow the camera
    follow_empty.parent = camera
    follow_empty.location = location  # Adjust the relative location

    return imported_obj

def apply_texture_to_object(object_name, texture_path):
    # Check if the object exists in the scene
    if object_name not in bpy.data.objects:
        print(f"Object '{object_name}' not found in the scene.")
        return

    # Get the object
    obj = bpy.data.objects[object_name]

    # Create a new material
    mat = bpy.data.materials.new(name=f"{object_name}_Material")
    mat.use_nodes = True
    nodes = mat.node_tree.nodes

    # Clear existing nodes
    nodes.clear()

    # Create a Principled BSDF shader node
    shader = nodes.new(type='ShaderNodeBsdfPrincipled')
    shader.location = (0, 0)

    # Create an Image Texture node
    tex_image = nodes.new('ShaderNodeTexImage')
    tex_image.location = (-300, 0)

    # Load and assign the image to the texture node
    tex_image.image = load_image(texture_path)

    # Create a Material Output node
    output = nodes.new(type='ShaderNodeOutputMaterial')
    output.location = (200, 0)

    # Link the texture node to the Principled BSDF shader node
    links = mat.node_tree.links
    links.new(tex_image.outputs['Color'], shader.inputs['Base Color'])

    # Link the shader node to the material output
    links.new(shader.outputs['BSDF'], output.inputs['Surface'])

    # Assign the material to the object
    if len(obj.data.materials):
        # Replace the first material
        obj.data.materials[0] = mat
    else:
        # No materials present, add the new one
        obj.data.materials.append(mat)

    print(f"Texture applied to '{object_name}'.")

# Clear existing objects in the scene
bpy.ops.object.select_all(action='SELECT')
bpy.ops.object.delete(use_global=False)

# Create the sun lamp
sun_location = (100, 100, 100)  # Position the sun lamp at a suitable location
sun_strength = 5  # Adjust the strength as needed
sun_color = (1, 1, 0.95)  # Sunlight color can be slightly yellow
sun = create_sun(sun_location, strength=sun_strength, sun_color=sun_color) 

# Get the directory of the current script file
script_dir = os.path.dirname(os.path.abspath(__file__))

backgroundFilePath = os.path.join(script_dir, '..', '..', 'data', 'Textures', 'venusAtmo.jpg')
setup_background(backgroundFilePath)

scale_factor = 1e3

low_res_venus_path = os.path.join(script_dir, '..', '..', 'Models', 'LowResMesh.obj')  # Update this to your correct file path

new_venus_scale_factor = 1/(scale_factor /1000)  # Adjust as needed
# new_venus = import_obj(low_res_venus_path, 
#                        'LowResVenus', 
#                        new_venus_scale_factor, 
#                        location=(500, 500, 0.0))


texture_path = "M:\\dev\\MATLAB\\ARO 4811L\\data\\Textures\\venusUVMap.jpg"
dashBoard_Path = os.path.join(script_dir, '..', '..', 'data', 'Textures' , 'venusUVMap.jpg')
low_res_venus_path = os.path.join(script_dir, '..', '..', 'Models', 'LowResMesh.obj')
new_venus = import_obj_and_apply_texture(
    filepath=low_res_venus_path,
    object_name='LowResVenus',
    scale_factor=new_venus_scale_factor,
    location=(500, 500, 0.0),
    texture_path=texture_path
)

balloon_Path = os.path.join(script_dir, '..', '..', 'Models', 'balloon_Sphercial.obj')  # Update this to your correct file path
balloon = import_obj(balloon_Path, 'Balloon', 1)

# Load CSV data
csv_filepath = os.path.join(script_dir, '..', '..', 'data', 'resultsData_float.csv')
csv_data = load_csv_data(csv_filepath)

# Create or find the display object
display_object_name = 'DisplayScreen'  # Replace with your display object's name
if display_object_name not in bpy.data.objects:
    # If a display object doesn't exist, create one (e.g., a plane)
    bpy.ops.mesh.primitive_plane_add(size=2, location=(0, 0, 0))  # Adjust size and location
    display_object = bpy.context.object
    display_object.name = display_object_name
else:
    display_object = bpy.data.objects[display_object_name]

# Create or find the text object
if 'Text' not in bpy.data.objects:
    bpy.ops.object.text_add(location=(0, 0, 0))
    text_object = bpy.context.object
    text_object.name = 'Text'
else:
    text_object = bpy.data.objects['Text']
    text_object.data.body = ''

# Set the font size and alignment as needed
text_object.data.align_x = 'CENTER'
text_object.data.size = 0.2  # Adjust to fit the display screen

# Parent the text to the display
text_object.parent = display_object
# Reset location to center on the display, and adjust as needed
text_object.location.x = 0
text_object.location.y = 0
text_object.location.z = 0.01  # Slightly in front of the display

# Set the animation start and end frames
bpy.context.scene.frame_start = 0
bpy.context.scene.frame_end = 750

# # Update the text for the current frame
# update_text(bpy.context.scene)

# # Remove existing handlers that start with "update_text"
# for handler in bpy.app.handlers.frame_change_pre:
#     if "update_text" in str(handler):
#         bpy.app.handlers.frame_change_pre.remove(handler)

# # Append the handler
# bpy.app.handlers.frame_change_pre.append(update_text)

# Add a turbulence field to the scene
add_turbulence_force_field(strength=5, size=2)  # Adjust the strength and size as needed

# Call the function to add a particle system to the Venus object
dust_Path = os.path.join(script_dir, '..', '..', 'Models', 'dust.obj')  # Update this to your correct file path
dust_obj = import_obj(dust_Path, 
                       'Dust', 
                       1, 
                       location=(0,0,0))
# Create the dust particle shape
dust_particle_shape = create_dust_particle_shape()

# Create the dust material
dust_material = create_dust_material()

# Assign the dust material to the particle shape
dust_particle_shape.data.materials.append(dust_material)

# Call the function to add a particle system to the Venus object
# Pass the dust_particle_shape to the create_dust_particle_system function
create_dust_particle_system(dust_obj, 1000, 0, 750, dust_particle_shape)  # Adjust particle_count as needed

# Create the dust particle shape
dust_particle = create_dust_particle_shape()

# Create the dust material
dust_material = create_dust_material()

# Assign the dust material to the particle shape
dust_particle.data.materials.append(dust_material)

# Make sure to set the particle system to use the dust particle shape
if 'DustParticles' in bpy.context.object.modifiers:
    particle_system = bpy.context.object.particle_systems['DustParticles']
    particle_system.settings.render_type = 'OBJECT'
    particle_system.settings.instance_object = dust_particle

    # Load CSV data
csv_filepath = os.path.join(script_dir, '..', '..', 'data', 'resultsData_float.csv')
csv_data = load_csv_data(csv_filepath)

# Create the dashboard
# create_dashboard(csv_data, bpy.context.scene.frame_start, bpy.context.scene.frame_end)

# Append the handler to update balloon's altitude
bpy.app.handlers.frame_change_pre.append(update_balloon_altitude)

# Camera set-up
camera = setup_follow_camera(balloon, radius=10)

# # Create the dashboard
# dashboard = setup_dashboard(camera, csv_data)

# Now call the function to create and set up the dashboard
dashboard_empty = create_and_setup_dashboard(csv_data, camera, bpy.context.scene.frame_start, bpy.context.scene.frame_end)

dashBoard_TexturePath = os.path.join(script_dir, '..', '..', 'data', 'Textures' , 'display.png')
dashboard_obj = os.path.join(script_dir, '..', '..', 'Models', 'dashboard.obj')

dashBoard = import_obj_follow_camera(filepath=dashboard_obj, camera=camera, location=(0, 0, -5), scale_factor=0.1)
apply_texture_to_object('Plane', dashBoard_TexturePath)