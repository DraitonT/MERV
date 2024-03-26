import bpy

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