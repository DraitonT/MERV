import bpy
import csv

# Path to the CSV file
csv_file_path = 'M:\\dev\\MATLAB\\ARO 4811L\\Genesis-v0.3.0\\Project\\multi_vehicle\\output.csv'

# Open the CSV file
with open(csv_file_path, newline='') as csvfile:
    csvreader = csv.reader(csvfile, delimiter=',')
    
    # Read the header row and find the index of 'h_child2'
    headers = next(csvreader)
    h_child2_index = headers.index('h_child2')

    # Define your frame step
    frame_step = 10  # For example, change to your desired frame step

    # Loop through the rows in the CSV
    for frame_number, row in enumerate(csvreader):
        altitude = float(row[h_child2_index])  # Use the index of 'h_child2'
        frame_number += 1  # Frame numbers typically start at 1
        frame_number *= frame_step  # Calculate the actual frame number
        
        # Assuming 'Balloon' is the name of your balloon object in Blender
        balloon = bpy.data.objects['Balloon']
        
        # Set the location of the balloon at this frame
        balloon.location.z = altitude
        
        # Insert a keyframe
        balloon.keyframe_insert(data_path="location", frame=frame_number, index=2)  # '2' is the index for 'z' location
