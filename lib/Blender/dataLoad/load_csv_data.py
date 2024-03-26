import csv

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