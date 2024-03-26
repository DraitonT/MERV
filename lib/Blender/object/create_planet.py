import bpy

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