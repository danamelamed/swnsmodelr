# Prepare MODIS EVI for analyses in SWNS
import os
import arcpy
from arcpy import env
from shutil import copyfile


directory = r'Z:/Dana/EVI'
os.chdir(directory)
files = os.listdir(directory)

for i in range(0,len(files)):

    # get date chars
    year = files[i][9:13]
    yday = files[i][13:16]
    # Rename files
    new_file_name = "MOD13Q1_%s_%s.hdf"%(year,yday)
    print(new_file_name)
    os.rename(files[i], new_file_name)

os.chdir(directory)    
files = os.listdir(directory)


arcpy.env.mask = r'F:/Packages/swnsmodelr/Rasters/dem.tif'
arcpy.env.cellSize = r'F:/Packages/swnsmodelr/Rasters/dem.tif'
arcpy.env.snapRaster = r'F:/Packages/swnsmodelr/Rasters/dem.tif'
for i in range(0,len(files)):
    # Extract EVI tiff from .hdf file
    year = files[i][8:12]
    yday = files[i][13:16]
    
    arcpy.management.ExtractSubDataset(files[i], \
                                       r'Z:/Dana/rTemp.evi_temp.tif', 1)
    # Project area to NAD83 UTM zone 20N
    arcpy.management.ProjectRaster( r'Z:/Dana/rTemp.evi_temp.tif', \
                                    r'Z:/Dana/rTemp.evi_temp2.tif', \
                                   "PROJCS['NAD_1983_UTM_Zone_20N',GEOGCS['GCS_North_American_1983',DATUM['D_North_American_1983',SPHEROID['GRS_1980',6378137.0,298.257222101]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['False_Easting',500000.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',-63.0],PARAMETER['Scale_Factor',0.9996],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0]]", "NEAREST", "231.65635826375 231.65635826375", None, None, "PROJCS['Unknown_datum_based_upon_the_custom_spheroid_Sinusoidal',GEOGCS['GCS_Unknown_datum_based_upon_the_custom_spheroid',DATUM['D_Not_specified_based_on_custom_spheroid',SPHEROID['Custom_spheroid',6371007.181,0.0]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Sinusoidal'],PARAMETER['false_easting',0.0],PARAMETER['false_northing',0.0],PARAMETER['central_meridian',0.0],UNIT['Meter',1.0]]")

    # Resample and mask area to SWNS
    out_raster = arcpy.sa.ExtractByMask(r'Z:/Dana/rTemp.evi_temp2.tif', \
                                        r"F:\Packages\swnsmodelr\Rasters\dem.tif")
    out_raster.save(r"Z:/Dana/EVI/EVI_%s_%s.tif"%(year,yday))
