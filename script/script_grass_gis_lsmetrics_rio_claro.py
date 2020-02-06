#" -----
#" objetivos: lsmetrics in script
#" autor: mauricio vancine
#" data: 31-01-2020
#" -----

# create grassdb
# mkdir grassdb
# cd grassdb
# grass -c epsg:31983 -e /home/mude/Downloads/guilherme/metricas_test/grassdb/utm23ssirgas2000

# initiate grass
grass /home/mude/data/github/lsmetrics/grassdb/utm23ssirgas2000/PERMANENT --exec

# iniciar o python
python

# importar bibliotecas
import os
import grass.script as gs
from grass.pygrass.modules.shortcuts import general as g
from grass.pygrass.modules.shortcuts import vector as v
from grass.pygrass.modules.shortcuts import raster as r
import time

# gui
gs.run_command("g.gui")

#-------------------------------

# diretorio
# os.chdir(r"/home/mude/data/github/lsmetrics/vector")

# import vector of land use
# v.in.ogr(input = "SP_3543907_USO.shp", output = "uso", overwrite = True)

# define region and resolution
# g.region(flags = "ap", vector = "uso", res = 30)

# vector to raster
# v.to.rast(input = "uso", output = "uso", type = "area", use = "cat", \
# 	label_column = "CLASSE_USO", overwrite = True)

# import limit
# v.import(input = "limite_rio_claro_wgs84.shp", output = "li", overwrite = True)

#-------------------------------

## metrics

# output directory
output_dir = r"/home/mude/data/github/lsmetrics/output"

# ls metrics directory
ls_metrics_dir = r"/home/mude/data/github/lsmetrics/lsmetrics"
os.chdir(ls_metrics_dir)

# import ls metrics functions
from LSMetrics_v1_0_0 import create_binary, createtxt, patch_size, edge_core, functional_connectivity, dist_edge

# define parameters
map_list = ["uso"]
habitat = [4] 
edge_depths_list = [30]
gap_crossing_list = [60, 120]


# region
g.region(flags = "ap", raster = "uso")


#-------------------------------

# 0. create binary maps
map_list_bin = create_binary(list_maps = map_list, 
                             list_habitat_classes = habitat, 
                             zero = True, 
                             prefix = "bin_zero_")

#-------------------------------


#-------------------------------
# 1. number of patch
# 2. patch size
patch_size(input_maps = map_list_bin, 
           zero = False, 
           diagonal = True,
           prepare_biodim = False, 
           calc_statistics = True, 
           remove_trash = True,
           prefix = "02_patch_size_", 
           add_counter_name = False, 
           export = True, 
           export_pid = True, 
           dirout = output_dir)

#-------------------------------


#-------------------------------
# 3. edge core
edge_core(input_maps = map_list_bin, 
	      list_edge_depths = edge_depths_list,
	      diagonal = True, 
	      diagonal_neighbors = True,
          calc_edge_core_area = True,
          calc_percentage = False, 
          window_size = [], 
          method_percentage = "average",
          calc_statistics = True, 
          remove_trash = True,
          prefix = "03_edge_core_", 
          add_counter_name = False, 
          export = True, 
          export_pid = False,
          dirout = output_dir)

#-------------------------------


#-------------------------------
# 4. functional connectivity
functional_connectivity(input_maps = map_list_bin, 
						list_gap_crossing = gap_crossing_list,
                        zero = False, 
                        diagonal = True, 
                        diagonal_neighbors = True,
                        functional_connec = False,
                        functional_area_complete = False,
                        prepare_biodim = False, 
                        calc_statistics = True, 
                        remove_trash = True,
                        prefix = "04_functional_connectivity_", 
                        add_counter_name = False, 
                        export = True, 
                        export_pid = False, 
                        dirout = output_dir)

#-------------------------------


#-------------------------------
# 5. edge dist
dist_edge(input_maps = map_list_bin,
          classify_edge_as_zero = False,
          prepare_biodim = False, 
          remove_trash = True,
          prefix = "05_edge_distance_", 
          add_counter_name = False, 
          export = True, 
          dirout = output_dir)

#-------------------------------


#-------------------------------
# create hexagons
# gs.run_command("v.mkgrid", flags = "h", map = "hex_1000", box = [1000, 1000], overwrite = True)

# select hexagon
# gs.run_command("v.overlay", ainput = "hex_1000", binput = "li", output = "hex_1000_uso", operator = "and", overwrite = True)

# rasterize
# gs.run_command("v.to.rast", input = "hex_1000_uso", output = "hex_1000_uso", use = "cat", overwrite = True)

#-------------------------------


#-------------------------------
# zonal statistics
# 1. number of patch
# Change to the script folder!
script_dir = r'/home/mude/data/github/lsmetrics/generalized_zonal_stats'
os.chdir(script_dir)
os.listdir(script_dir)

# Import GeneralizedZonalStats class and functions
from GeneralizedZonalStats_v001 import GeneralizedZonalStats, proportion_habitat, number_patches

# Initialize and select maps to be used in zonal stats
np = GeneralizedZonalStats(input_shape = "hex_1000_uso", input_rasters = ["02_patch_size_bin_zero_uso_HABMAT_pid"])

# Create new cols
cols = ["number_pat"] # Col names
col_type = ['int'] # Col type

# Create cols
np.create_new_column(column_names = cols, type_col = col_type)

# Monitoring time
start = time.time()

# Calculate number of patches (clumps) of eucalyptus in each feature using number_patches function
np.run_zonal_stats(number_patches, mask = True)

# Monitoring time
end = time.time()

# Print total time
print 'The number of patches took us '+str((end - start)/60)+' minutes.'

# region
g.region(flags = "ap", raster = "uso")

# 2. patch size
v.rast_stats(map = "hex_1000_uso", raster = "02_patch_size_bin_zero_uso_HABMAT_patch_AreaHA", \
	column_prefix = "patch_size_median", method = "median")

# 3. core area
v.rast_stats(map = "hex_1000_uso", raster = "03_edge_core_bin_zero_uso_HABMAT_CORE_0030m_AreaHA", \
	column_prefix = "core_area_median", method = "median")

# 4. edge area
v.rast_stats(map = "hex_1000_uso", raster = "03_edge_core_bin_zero_uso_HABMAT_EDGE_0030m_AreaHA", \
	column_prefix = "edge_area_median", method = "median")

# 5. functional connectivity 60 m
v.rast_stats(map = "hex_1000_uso", raster = "04_functional_connectivity_bin_zero_uso_HABMAT_0060m_func_connect_AreaHA", \
	column_prefix = "func_con_60m_median", method = "median")

# 6. functional connectivity 120 m
v.rast_stats(map = "hex_1000_uso", raster = "04_functional_connectivity_bin_zero_uso_HABMAT_0120m_func_connect_AreaHA", \
	column_prefix = "func_con_120m_median", method = "median")

# 7. isolation
r.mapcalc("05_edge_distance_bin_zero_uso_HABMAT_edge_dist_isolation = if(05_edge_distance_bin_zero_uso_HABMAT_edge_dist > 0, 05_edge_distance_bin_zero_uso_HABMAT_edge_dist, null())", overwrite = True)
v.rast_stats(map = "hex_1000_uso", raster = "05_edge_distance_bin_zero_uso_HABMAT_edge_dist_isolation", \
	column_prefix = "isolation_median", method = "median")

# 8. interior
r.mapcalc("05_edge_distance_bin_zero_uso_HABMAT_edge_dist_interior = if(05_edge_distance_bin_zero_uso_HABMAT_edge_dist < 0, 05_edge_distance_bin_zero_uso_HABMAT_edge_dist, null())", overwrite = True)
v.rast_stats(map = "hex_1000_uso", raster = "05_edge_distance_bin_zero_uso_HABMAT_edge_dist_interior", \
	column_prefix = "interior_median", method = "median")

#-------------------------------