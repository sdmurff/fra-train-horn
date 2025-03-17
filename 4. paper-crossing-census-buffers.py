import geopandas as gpd
import pandas as pd
from shapely.geometry import Point

  # Read in Train Crossings
  data = pd.read_csv("~/crossings_usa_clean1.csv")
  
  # Create lat/long points for geoprocessing
  geometry = [Point(xy) for xy in zip(data['Longitude'], data['Latitude'])]
  crossings = gpd.GeoDataFrame(data, geometry=geometry)
  
  # Set the CRS to WGS84 (EPSG:4326) since the data is in lat/long
  crossings.set_crs("EPSG:4326", inplace=True)
  
  # Transform to a projected CRS
  crossings = crossings.to_crs("EPSG:26912")
  
  # Create 1,000-foot buffers (convert feet to meters: 1 foot = 0.3048 meters)
  crossings['buffer'] = crossings.geometry.buffer(500 * 0.3048)
  
  # Rename geometry column to avoid conflicts
  crossings_us = crossings.rename(columns={"geometry": "latLong", "buffer": "geometry"})
  
  # Read in Census shape file for Utah
  #census_gdf = gpd.read_file("~/tl_2024_49_tabblock20/tl_2024_49_tabblock20.shp")
  census_gdf = gpd.read_file("~/US Tabblock/nationwide_tabblock20.gpkg", layer="blocks")
  #census_gdf.drop(columns=["geometry"]).to_csv("~/tl_2024_49_tabblock20.csv", index=False)
  
  # Perform a spatial intersection to get actual overlap areas
  census_gdf = census_gdf.to_crs("EPSG:26912")
  crossings_with_blocks = gpd.overlay(crossings_us, census_gdf, how='intersection')
  
  # Calculate the overlap area
  crossings_with_blocks['overlap_area'] = crossings_with_blocks.geometry.area
  
  crossings_with_blocks['proportion'] = crossings_with_blocks['overlap_area']  / crossings_with_blocks['ALAND20'] 
  
  # Step 4: Adjust the population by the proportion
  crossings_with_blocks['adjusted_population'] = crossings_with_blocks['POP20'] * crossings_with_blocks['proportion']
  
  # Save the result as a CSV file
  crossings_with_blocks.to_csv("~/crossings_census_join_US1.csv", index=False)
