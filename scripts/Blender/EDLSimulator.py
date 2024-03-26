import bpy
import csv
from bpy_extras.image_utils import load_image


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

def create_balloon(location=(0, 0, 0), scale=(1, 1, 1)):
    # Create a UV Sphere
    bpy.ops.mesh.primitive_uv_sphere_add(location=location)
    balloon_obj = bpy.context.active_object
    balloon_obj.scale = scale
    balloon_obj.name = 'Balloon'

    # Enter Edit Mode to adjust the shape to more closely resemble a balloon
    bpy.ops.object.mode_set(mode='EDIT')
    bpy.ops.mesh.select_all(action='DESELECT')
    
    # Assume the top vertex of the sphere is the tip of the balloon
    bpy.ops.mesh.select_all(action='SELECT')
    bpy.ops.transform.resize(value=(1.0, 1.0, 1.5))  # Scale to elongate the balloon shape
    bpy.ops.object.mode_set(mode='OBJECT')

    # Add a shape key for the basis shape (the uninflated balloon)
    balloon_obj.shape_key_add(name='Basis', from_mix=False)

    # Add another shape key for the inflated shape
    bpy.ops.object.shape_key_add(from_mix=False)
    balloon_obj.active_shape_key.name = 'Inflated'

    # Switch to Edit Mode to model the inflated balloon
    bpy.ops.object.mode_set(mode='EDIT')
    bpy.ops.mesh.select_all(action='SELECT')
    bpy.ops.transform.resize(value=(1.2, 1.2, 1.4))  # Inflate the balloon a bit
    bpy.ops.object.mode_set(mode='OBJECT')

    return balloon_obj

def animate_balloon_inflate_deflate(object_name, start_frame, end_frame):
    # Retrieve the object
    obj = bpy.data.objects.get(object_name)
    if not obj:
        print("Object not found.")
        return

    # Insert keyframes for the inflation and deflation
    # Frame at which the balloon is fully inflated
    inflation_frame = (end_frame - start_frame) // 2 + start_frame

    # Defining keyframes for the 'Inflated' shape key value
    obj.data.shape_keys.key_blocks['Inflated'].value = 0
    obj.data.shape_keys.key_blocks['Inflated'].keyframe_insert('value', frame=start_frame)
    
    obj.data.shape_keys.key_blocks['Inflated'].value = 1
    obj.data.shape_keys.key_blocks['Inflated'].keyframe_insert('value', frame=inflation_frame)
    
    obj.data.shape_keys.key_blocks['Inflated'].value = 0
    obj.data.shape_keys.key_blocks['Inflated'].keyframe_insert('value', frame=end_frame)
 
# Clear existing objects in the scene
bpy.ops.object.select_all(action='SELECT')
bpy.ops.object.delete(use_global=False)

# Create the balloon object
balloon = create_balloon(location=(0, 0, 0), scale=(1, 1, 1))

# Animate the balloon to inflate and deflate
animate_balloon_inflate_deflate(balloon.name, 1, 240)
# Gondola and Aeroshell 
# Parachute 
# Clouds 
# Balloon 