import bpy
import sys

# # Get the directory of the current script
# current_script_dir = os.path.dirname(os.path.abspath(__file__))
# sys.path.append('lib\Blender')

# # Calculate the path to the 'lib\Blender' directory
# lib_blender_path = os.path.abspath(os.path.join(current_script_dir, '..', '..'))


# # Add the 'lib\Blender' directory to sys.path
# if lib_blender_path not in sys.path:
#     sys.path.append(lib_blender_path)

# Now you can import from the 'object' subpackage
from object.create_marker import create_marker

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