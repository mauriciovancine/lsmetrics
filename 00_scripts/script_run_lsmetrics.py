#' ---
#' title: ls metrics
#' author: mauricio vancine
#' date: 2021-05-08
#' ---

# ---------------------------------------------------------------------------------

# python
python3

# import libs
import os
import grass.script as gs 

# dir
path = r"/home/mude/data/github/lsmetrics/"

# import generalized
os.chdir(path + "lsmetrics")

# import lsmetrics
from LSMetrics_v0_9_1 import create_binary, createtxt, patch_size, edge_core, functional_connectivity, dist_edge

# --------------------------------------------------------------------------------

# import vector data
os.chdir(path + "data")
gs.run_command("v.in.ogr", input = "SP_3543907_USO.shp", output = "uso", snap = 0.0001, overwrite = True)

# limit
gs.run_command("v.import", input = "rio_claro_limit.shp", output = "rio_claro_limit", overwrite = True)

# define region and resolution
gs.run_command("g.region", flags = "ap", vector = "uso", res = 30)

# rasterize
gs.run_command("v.to.rast", input = "uso", output = "uso", type = "area", \
	use = "cat", label_column = "CLASSE_USO", overwrite = True)

# create hexagons
gs.run_command("v.mkgrid", flags = "h", map = "hex", overwrite = True)

# --------------------------------------------------------------------------------

# define parameters
map_list = ["uso"] 
edge_depths_list = [30]
gap_crossing_list = [60, 120]
output_dir = path + "metrics"

# definir a region
gs.run_command("g.region", flags = "ap", raster = "uso")
gs.run_command("r.mask", flags = "r")
gs.run_command("r.mask", raster = "uso")

#-------------------------------

# 0. create binary maps
map_list_bin_forest = create_binary(list_maps = map_list, 
                             		list_habitat_classes = [4], 
                             		zero = True, 
                             		prefix = "bin_zero_forest_")

#-------------------------------

#-------------------------------
## forest
# 1. number of patch
# 2. patch size
patch_size(input_maps = map_list_bin_forest, 
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
edge_core(input_maps = map_list_bin_forest, 
          list_edge_depths = edge_depths_list,
          diagonal = True, 
          diagonal_neighbors = True,
          calc_edge_core_area = False,
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

# --------------------------------------------------

# number of the patchs
# region
gs.run_command("g.region", flags = "ap", raster = "uso")
gs.run_command("r.mask", flags = "r")
gs.run_command("r.mask", vector = "uso")

# clip
gs.run_command("v.overlay", ainput = "hex", binput = "rio_claro_limit", \
	output = "hex_clip", operator = "and", overwrite = True)

# parameters
vector = "hex"
raster_id_list = ["02_patch_size_bin_zero_forest_uso_HABMAT_pid"]
col_names_list = ["np"]

# function
def number_patches_to_polygon(vector, raster_id_list, col_names_list):
	# column names
	cols = [str(i) + " int" for i in col_names_list]
	# add columns
	gs.run_command("v.db.addcolumn", map = vector, column = cols, quiet = True)
	# cats
	cats = gs.read_command("db.select", flags = "c", sql = "SELECT cat FROM " + vector, quiet = True).split('\n')
	# remove absent values ''
	cats = [i for i in cats if i != '']
	# selection, region, mask and area
	for i in cats:
		# information
		print("Complete " + i + " of " + str(len(cats)) + " features")
		# select feature
		gs.run_command("v.extract", flags = "t", input = vector, output = "vector_cat", where = "cat = " + i, overwrite = True, quiet = True)
		# define region to feature
		gs.run_command("g.region", vector = "vector_cat", align = raster_id_list[0], quiet = True)
		# define mask to feature
		gs.run_command("r.mask", vector = "vector_cat", overwrite = True, quiet = True)
		# for to raster
		for j in list(range(len(raster_id_list))):
			# mask raster
			gs.mapcalc("raster_id_cat = " + raster_id_list[j], overwrite = True, quiet = True) # retirar e add dentro do r.category
			# assess patch ids
			ids = gs.read_command("r.category", map = "raster_id_cat").split('\n')
			# remove absent values ''
			ids = [i for i in ids if i != '']
			# number of patches 
			np = len(ids)
			# add value to column
			gs.run_command("v.db.update", map = vector, column = col_names_list[j], value = str(np), where = "cat = " + i, quiet = True)
	# return mask for vector
	gs.run_command("g.region", flags = "a", vector = vector, quiet = True)
	gs.run_command("r.mask", vector = vector, overwrite = True, quiet = True)
	# remove temp files
	gs.run_command("g.remove", flags = "f", type = "vector", name = "vector_cat", quiet = True)
	gs.run_command("g.remove", flags = "f", type = "raster", name = "raster_id_cat", quiet = True)


# use
number_patches_to_polygon(vector = "hex",
	raster_id_list = ["02_patch_size_bin_zero_forest_uso_HABMAT_pid"],
	col_names_list = ["np"])

# --------------------------------------------------

# percentage
# region
gs.run_command("g.region", flags = "ap", raster = "uso")
gs.run_command("r.mask", flags = "r")
gs.run_command("r.mask", vector = "uso")

# parameters
vector = "hex_clip"
raster_bin_list = ["03_edge_core_bin_zero_forest_uso_HABMAT_EDGE_0030m", 
                   "03_edge_core_bin_zero_forest_uso_HABMAT_CORE_0030m"]
col_names_list = ["pe", "pc"]

# function
def raster_percentage_to_polygon(vector, raster_bin_list, col_names_list):
	# add columns
	cols = [str(i) + " double precision" for i in col_names_list]
	gs.run_command("v.db.addcolumn", map = vector, column = cols, quiet = True)
	# cats
	cats = gs.read_command("db.select", flags = "c", sql = "SELECT cat FROM " + vector, quiet = True).split('\n')
	# remove absent values ''
	cats = [i for i in cats if i != '']
	# selection, region, mask and area
	for i in cats:
		# information
		print("Complete " + i + " of " + str(len(cats)) + " features")
		# select feature
		gs.run_command("v.extract", flags = "t", input = vector, output = "vector_cat", where = "cat = " + i, overwrite = True, quiet = True)
		# define region to feature
		gs.run_command("g.region", vector = "vector_cat", align = raster_id_list[0], quiet = True)
		# define mask to feature
		gs.run_command("r.mask", vector = "vector_cat", overwrite = True, quiet = True)
		# rasterize the feature
		gs.run_command("v.to.rast", input = "vector_cat", output = "raster_cat", use = "val", val = 1, overwrite = True, quiet = True)
		# calculate the area to feature
		area_cat = gs.read_command("r.stats", flags = "an", input = "raster_cat", quiet = True)
		# for to raster
		for j in list(range(len(raster_bin_list))):
			# reclassify
			gs.mapcalc("raster_null = if(" + raster_bin_list[j] + " == 1, 1, null())", overwrite = True, quiet = True)
			# calculate area to raster
			area_raster = gs.read_command("r.stats", flags = "an", input = "raster_null", quiet = True)
			# calculate proportion
			if area_raster == "":
				val = 0
			else:
				val = round((float(str(area_raster).replace("\n","").replace("1 ", "")) / 
                float(str(area_cat).replace("\n","").replace("1 ", ""))) * 100, 3)
			# add value to column
			gs.run_command("v.db.update", map = vector, column = col_names_list[j], value = str(val), where = "cat = " + i, quiet = True)
	# return mask for vector
	gs.run_command("g.region", flags = "a", vector = vector, quiet = True)
	gs.run_command("r.mask", vector = vector, overwrite = True, quiet = True)
	# remove temp files
	gs.run_command("g.remove", flags = "f", type = "vector", name = "vector_cat", quiet = True)
	gs.run_command("g.remove", flags = "f", type = "raster", name = "raster_null", quiet = True)
	gs.run_command("g.remove", flags = "f", type = "raster", name = "raster_cat", quiet = True)


# use
percentage_to_polygon(vector = "hex",
	raster_bin_list = ["03_edge_core_bin_zero_forest_uso_HABMAT_EDGE_0030m", "03_edge_core_bin_zero_forest_uso_HABMAT_CORE_0030m"],
	col_names_list = ["pe1", "pc1"])

# color
gs.run_command("v.colors", map = "hex", use = "attr", column = "pe", color = "viridis")
gs.run_command("v.colors", map = "hex", use = "attr", column = "pc", color = "viridis")

# --------------------------------------------------

# export
os.chdir(path)
os.mkdir("vector")
os.chdir(path + "vector")
gs.run_command("v.out.ogr", input = "hex", output = "hex.gpkg")

# --------------------------------------------------
