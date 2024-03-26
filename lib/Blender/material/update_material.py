import bpy

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