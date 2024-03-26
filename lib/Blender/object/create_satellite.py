import bpy

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