import bpy

def create_cloud_with_noise(name='Cloud', location=(0, 0, 0), scale=(1, 1, 1), density=0.1, color=(1, 1, 1, 1)):
    # Create a domain for the cloud
    bpy.ops.mesh.primitive_cube_add(size=2, location=location)
    cloud_domain = bpy.context.object
    cloud_domain.scale = scale
    cloud_domain.name = name

    # Create a new material
    cloud_mat = bpy.data.materials.new(name=f"{name}Material")
    cloud_domain.data.materials.append(cloud_mat)

    # Enable use of nodes for the material
    cloud_mat.use_nodes = True
    nodes = cloud_mat.node_tree.nodes
    nodes.clear()

    # Create the nodes
    material_output = nodes.new(type='ShaderNodeOutputMaterial')
    volume_scatter = nodes.new(type='ShaderNodeVolumeScatter')
    volume_absorption = nodes.new(type='ShaderNodeVolumeAbsorption')
    mix_shader = nodes.new(type='ShaderNodeMixShader')
    noise_texture = nodes.new(type='ShaderNodeTexNoise')
    math_node = nodes.new(type='ShaderNodeMath')
    math_node.operation = 'MULTIPLY'
    math_node.inputs[1].default_value = density  # Multiply the noise texture by the density

    # Connect the nodes
    links = cloud_mat.node_tree.links
    links.new(noise_texture.outputs['Fac'], math_node.inputs[0])
    links.new(math_node.outputs['Value'], mix_shader.inputs['Fac'])
    links.new(volume_scatter.outputs['Volume'], mix_shader.inputs[1])
    links.new(volume_absorption.outputs['Volume'], mix_shader.inputs[2])
    links.new(mix_shader.outputs['Shader'], material_output.inputs['Volume'])

    # Set the material to be volumetric
    cloud_mat.blend_method = 'BLEND'

    # Adjust the noise texture settings for variation
    noise_texture.inputs['Scale'].default_value = 10.0
    noise_texture.inputs['Detail'].default_value = 2.0
    noise_texture.inputs['Distortion'].default_value = 0.5

    # Volume scatter and absorption settings
    volume_scatter.inputs['Color'].default_value = color
    volume_scatter.inputs['Density'].default_value = 0.2  # Adjust as needed
    volume_absorption.inputs['Color'].default_value = color
    volume_absorption.inputs['Density'].default_value = 0.1  # Adjust as needed

    return cloud_domain

def add_area_light(name='AreaLight', location=(5, 5, 5), energy=1000, size=10):
    # Add an area light to the scene
    bpy.ops.object.light_add(type='AREA', location=location)
    area_light = bpy.context.object
    area_light.data.energy = energy
    area_light.data.size = size
    area_light.name = name

    return area_light

def hex_to_rgb(value):
    """Converts a hex color string to an RGB tuple."""
    value = value.lstrip('#')
    lv = len(value)
    return tuple(int(value[i:i + lv // 3], 16) / 255.0 for i in range(0, lv, lv // 3))

# Clear existing objects in the scene
bpy.ops.object.select_all(action='SELECT')
bpy.ops.object.delete(use_global=False)

# Example usage:
cloud_color = hex_to_rgb('FFB86E')
cloud = create_cloud_with_noise(name='MyCloud', location=(0, 0, 2), scale=(20, 20, 10), density=0.15, color=(0.8, 0.479, 0.156, 1))
light = add_area_light(name='MyAreaLight', location=(5, 5, 5), energy=1000, size=10)
# Manually input the following for the color of the cloud (FFB86E)