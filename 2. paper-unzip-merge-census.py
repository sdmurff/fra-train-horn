import os
import zipfile
import geopandas as gpd
from shapely.geometry import MultiPolygon

# Define directories
zip_folder = "~/Downloads/State Census Files"  # Change to your ZIP file folder
extract_folder = "~/Downloads/State Census Files - Extracted"  # Change to where you want to extract files
output_gpkg = "~/Downloads/US Tabblock/nationwide_tabblock20.gpkg"  # Change to output path

# Ensure extract folder exists
os.makedirs(extract_folder, exist_ok=True)

# Step 1: Unzip all files
for file in os.listdir(zip_folder):
    if file.endswith(".zip"):
        zip_path = os.path.join(zip_folder, file)
        try:
            with zipfile.ZipFile(zip_path, "r") as zip_ref:
                zip_ref.extractall(extract_folder)
                print(f"Extracted: {file}")
        except zipfile.BadZipFile:
            print(f"Error: {file} is a corrupted ZIP file.")
        except Exception as e:
            print(f"Unexpected error extracting {file}: {e}")
print("Done")

# Step 2: Read and merge shapefiles
first_file = True  # Track if it's the first file
# Initialize counter
file_count = 0

# Process shapefiles
for root, dirs, files in os.walk(extract_folder):
    for file in files:
        if file.endswith(".shp"):
            shp_path = os.path.join(root, file)
            try:
                gdf = gpd.read_file(shp_path)

                # Convert POLYGON to MULTIPOLYGON
                gdf["geometry"] = gdf["geometry"].apply(
                    lambda geom: MultiPolygon([geom]) if geom.geom_type == "Polygon" else geom
                )

                # Append or create new GeoPackage
                gdf.to_file(output_gpkg, driver="GPKG", layer="blocks", mode="w" if first_file else "a")
                first_file = False  # Only overwrite for the first file

                file_count += 1
                print(f"[{file_count}] Appended: {file} to {output_gpkg}")
                print(file_count)

            except Exception as e:
                print(f"Error reading {file}: {e}")

print(f"Successfully processed {file_count} shapefiles and saved to {output_gpkg}")

