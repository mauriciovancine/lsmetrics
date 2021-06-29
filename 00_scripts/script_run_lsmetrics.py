#' ---
#' title: lsmetrics
#' author: mauricio vancine
#' date: 2021-05-12
#' ---

# ---------------------------------------------------------------------------------

# python
python3

# import libs
import os
import grass.script as gs 

# directory
path = r"/home/mude/data/github/lsmetrics/"

# import lsmetrics
os.chdir(path + "lsmetrics")
from LSMetrics_v0_9_1 import create_binary, createtxt, patch_size, fragment_area, percentage, edge_core, functional_connectivity, dist_edge, landscape_diversity

# add-ons
# sudo apt install -y grass-dev
# sudo grass -text -c EPSG:4326 --exec g.extension -s extension=r.pi operation=add
# gs.run_command("g.extension", flags = "s", extension = "r.pi") # sudo
# gs.run_command("g.extension", extension = "r.forestfrag")
# gs.run_command("g.extension", extension = "r.diversity")

# open gui
# gs.run_command("g.gui")

# --------------------------------------------------------------------------------

## import data

# directory
os.chdir(path + "01_data")

# import vector
gs.run_command("v.in.ogr", 
  input = "rio_claro_limit_sirgas2000_utm23s.shp",
	output = "limit_rc", 
  overwrite = True)

# import raster
gs.run_command("r.import", 
  input = "mapbiomas_c5_2019_mataatlantica_rio_claro_wgs84_geo.tif",
	output = "mapbiomas_af" ,
  overwrite = True)

# import raster
gs.run_command("r.import", 
  input = "mapbiomas_c5_2019_cerrado_rio_claro_wgs84_geo.tif",
	output = "mapbiomas_ce",
  overwrite = True)

# --------------------------------------------------------------------------------

## prepare data

# define region and resolution
gs.run_command("g.region", flags = "ap", vector = "limit_rc", res = 30)

# create maps with resolution from region
gs.mapcalc("mapbiomas_af_30m = mapbiomas_af", overwrite = True)
gs.mapcalc("mapbiomas_ce_30m = mapbiomas_ce", overwrite = True)

# merge rasters
gs.run_command("r.patch", flags = "z", 
	input = ["mapbiomas_af_30m", "mapbiomas_ce_30m"], 
	output = "mapbiomas_30m", overwrite = True)

# create hexagons
gs.run_command("v.mkgrid", flags = "ha", 
  map = "hex", box = [2000, 2000], overwrite = True)

# select hexagons
gs.run_command("v.select", ainput = "hex", binput = "limit_rc",
  operator = "overlap", output = "hex_rc", overwrite = True)

# export hexagons
gs.run_command("v.out.ogr", input = "hex_rc", 
  output = "hex_2000.gpkg", overwrite = True)

# list maps
# mapbiomas = gs.list_grouped(type = "raster", pattern = "*mapbiomas*")["PERMANENT"]

# --------------------------------------------------------------------------------

# remove all metrics
# gs.run_command("g.list", type = "raster", pattern = "*HABMAT*")
gs.run_command("g.remove", flags = "f", type = "raster", pattern = "*HABMAT*")

## metrics

# define parameters
map_list = ["mapbiomas_30m"] 
scale_list = [900]
edge_depths_list = [30]
gap_crossing_list = [31]
output_dir = path + "02_metrics"


# 0. create binary maps
create_binary(list_maps = map_list, 
              list_habitat_classes = [3, 4], 
              zero = True,
              prepare_biodim = False, 
              calc_statistics = True, 
              prefix = '', 
              add_counter_name = False, 
              export = True, 
              dirout = output_dir)

# 1. patch size
patch_size(input_maps = ["mapbiomas_30m_HABMAT"], 
           zero = True, 
           diagonal = True,
           prepare_biodim = False, 
           calc_statistics = True, 
           remove_trash = True,
           prefix = "", 
           add_counter_name = False, 
           export = True, 
           export_pid = True, 
           dirout = output_dir)

# 2. fragment area and structural connectivity
fragment_area(input_maps = ["mapbiomas_30m_HABMAT"], 
              list_edge_depths = edge_depths_list,
              zero = True, 
              diagonal = True,
              diagonal_neighbors = False,
              struct_connec = True, 
              patch_size_map_names = ["mapbiomas_30m_HABMAT_patch_AreaHA"],
              prepare_biodim = False, 
              calc_statistics = True, 
              remove_trash = True,
              prefix = "", 
              add_counter_name = False, 
              export = True, 
              export_fid = True, 
              export_struct_connec = True,
              dirout = output_dir)

# 3. proportion of habitat
percentage(input_maps = ["mapbiomas_30m_HABMAT"], 
           scale_list = scale_list, 
           method = "average", 
           append_name = "",
           diagonal_neighbors = False, 
           result_float = False,
           remove_trash = True, 
           export = True, 
           dirout = output_dir)

# 4. functional connectivity
functional_connectivity(input_maps = ["mapbiomas_30m_HABMAT"], 
                        list_gap_crossing = gap_crossing_list,
                        zero = True, 
                        diagonal = True, 
                        diagonal_neighbors = False,
                        functional_connec = True,
                        functional_area_complete = True,
                        prepare_biodim = False, 
                        calc_statistics = True, 
                        remove_trash = True,
                        prefix = '', 
                        add_counter_name = False, 
                        export = True, 
                        export_pid = True, 
                        dirout = output_dir)

# 5. edge core
edge_core(input_maps = ["mapbiomas_30m_HABMAT"], 
          list_edge_depths = edge_depths_list,
          diagonal = True, 
          diagonal_neighbors = False,
          calc_edge_core_area = True,
          calc_percentage = True, 
          window_size = scale_list, 
          method_percentage = 'average',
          calc_statistics = True, 
          remove_trash = True,
          prefix = '', 
          add_counter_name = False, 
          export = True, 
          export_pid = True, 
          dirout = output_dir)

# 6. distance of edge
dist_edge(input_maps = ["mapbiomas_30m_HABMAT"],
          classify_edge_as_zero = False,
          prepare_biodim = False, 
          remove_trash = True,
          prefix = '', 
          add_counter_name = False, 
          export = True, 
          dirout = output_dir)

# ---------------------------------------------------------------------------------------------

# r.forestfrag
gs.run_command("r.forestfrag", 
  flags = "s",
  input = "mapbiomas_30m_HABMAT", 
  output = "mapbiomas_30m_HABMAT_frag", 
  pf = "mapbiomas_30m_HABMAT_pf",
  pff = "mapbiomas_30m_HABMAT_pff",
  size = 5, 
  overwrite = True)

# ---------------------------------------------------------------------------------------------

# r.diversity
gs.run_command("r.diversity",
  input = "mapbiomas_30m", 
  prefix = "mapbiomas_30m", 
  alpha = .3,
  size = 5,
  method = ["simpson,shannon,pielou,renyi"],
  overwrite = True)

# ---------------------------------------------------------------------------------------------

# r.li

### 0 GUI tools

# 0.1 g.gui.rlisetup 
# g.gui.rlisetup: Configuration editor for the r.li.* module where * is name of the index
gs.run_command("g.gui.rlisetup")


# 0.2 forest and null
gs.mapcalc("mapbiomas_30m_HABMAT_null = if(mapbiomas_30m_HABMAT == 0, null(), mapbiomas_30m_HABMAT)", overwrite = True)


### 1 Patch indices

## 1.1 Patch number: Indices based on patch number

# 1.1.1 Patch density index
# r.li.patchdensity: Calculates patch density index on a raster map, using a 4 neighbour algorithm
gs.run_command("r.li.patchdensity",
  input = "mapbiomas_30m_HABMAT_null",
  conf = "mov5",
  output = "mapbiomas_30m_HABMAT_null_patchdensity",
  overwrite = True)

# 1.1.2 Patch number index
# r.li.patchnum: Calculates patch number index on a raster map, using a 4 neighbour algorithm
gs.run_command("r.li.patchnum", 
  input = "mapbiomas_30m_HABMAT_null@PERMANENT", 
  conf = "mov5",
  output = "mapbiomas_30m_HABMAT_null_patchnum", 
  overwrite = True)


## 1.2 Patch dimension: Indices based on patch dimension

# 1.2.1 Mean patch size index
# r.li.mps: Calculates mean patch size index on a raster map, using a 4 neighbour algorithm
gs.run_command("r.li.mps", 
  input = "mapbiomas_30m_HABMAT_null", 
  conf = "mov5",
  output = "mapbiomas_30m_HABMAT_null_mps", 
  overwrite = True)

# 1.2.2 Coefficient of variation of patch area
# r.li.padcv: Calculates coefficient of variation of patch area on a raster map
gs.run_command("r.li.padcv", 
  input = "mapbiomas_30m_HABMAT_null", 
  conf = "mov5",
  output = "mapbiomas_30m_HABMAT_null_padcv", 
  overwrite = True)

# 1.2.3 Range of patch area size
# r.li.padrange: Calculates range of patch area size on a raster map
gs.run_command("r.li.padrange", 
  input = "mapbiomas_30m_HABMAT_null", 
  conf = "mov5",
  output = "mapbiomas_30m_HABMAT_null_padrange", 
  overwrite = True)

# 1.2.4 Standard deviation of patch area
# r.li.padsd: Calculates standard deviation of patch area a raster map
gs.run_command("r.li.padsd", 
  input = "mapbiomas_30m_HABMAT_null", 
  conf = "mov5",
  output = "mapbiomas_30m_HABMAT_null_padsd", 
  overwrite = True)


## 1.3 Patch shape: Indices based on patch shape
 
# 1.3.1 Shape index
# r.li.shape: Calculates shape index on a raster map
gs.run_command("r.li.shape", 
  input = "mapbiomas_30m_HABMAT_null", 
  conf = "mov5",
  output = "mapbiomas_30m_HABMAT_null_shape", 
  overwrite = True)

## 1.4 Patch edge: Indices based on patch edge

# 1.4.1 Edge density index
# r.li.edgedensity: Calculates edge density index on a raster map, using a 4 neighbour algorithm
gs.run_command("r.li.edgedensity", 
  input = "mapbiomas_30m_HABMAT_null", 
  conf = "mov5",
  output = "mapbiomas_30m_HABMAT_null_edgedensity", 
  overwrite = True)

## 1.5 Patch attributes: Indices based on patch attributes

# 1.5.1 Weighted Edge Density index
# r.li.cwed: Calculates contrast Weighted Edge Density index on a raster map
gs.run_command("r.li.cwed", 
  input = "mapbiomas_30m_HABMAT_null", 
  conf = "mov5",
  path = "/home/mude/data/github/lsmetrics/01_data/weights.csv",
  output = "mapbiomas_30m_HABMAT_null_cwed", 
  overwrite = True)

# 1.5.2 Mean pixel attribute index
# r.li.mpa: Calculates mean pixel attribute index on a raster map
gs.run_command("r.li.mpa", 
  input = "mapbiomas_30m", 
  conf = "mov5",
  output = "mapbiomas_30m_mpa", 
  overwrite = True)



### 2. Diversity indices

# 2.1 Dominance diversity index
# r.li.dominance: Calculates dominance diversity index on a raster map
gs.run_command("r.li.dominance", 
  input = "mapbiomas_30m", 
  conf = "mov5",
  output = "mapbiomas_30m_div_dominance", 
  overwrite = True)

# 2.2 Pielou eveness index
# r.li.pielou: Calculates Pielou eveness index on a raster map
gs.run_command("r.li.pielou", 
  input = "mapbiomas_30m", 
  conf = "mov5",
  output = "mapbiomas_30m_div_pielow", 
  overwrite = True)

# 2.3 Renyi entropy
# r.li.renyi: Calculates Renyi entropy on a raster map
gs.run_command("r.li.renyi", 
  input = "mapbiomas_30m", 
  conf = "mov5",
  alpha = 0,
  output = "mapbiomas_30m_div_renyi", 
  overwrite = True)

# 2.4 Richness diversity index
# r.li.richness: Calculates richness diversity index on a raster map
gs.run_command("r.li.richness", 
  input = "mapbiomas_30m", 
  conf = "mov5",
  output = "mapbiomas_30m_div_richness", 
  overwrite = True)

# 2.5 Shannon diversity index
# r.li.shannon: Calculates Shannon diversity index on a raster map
gs.run_command("r.li.shannon", 
  input = "mapbiomas_30m", 
  conf = "mov5",
  output = "mapbiomas_30m_div_shannon", 
  overwrite = True)

# 2.6 Simpson diversity index
# r.li.simpson: Calculates Simpson diversity index on a raster map
gs.run_command("r.li.simpson", 
  input = "mapbiomas_30m", 
  conf = "mov5",
  output = "mapbiomas_30m_div_simpson", 
  overwrite = True)


# r.pi ---------------------------------------------------------------------------------------------

# 1. r.pi.corearea
gs.run_command("r.pi.corearea", 
  flags = "a", 
  input = "mapbiomas_30m_HABMAT", 
  output = "mapbiomas_30m_ENN",
  method = "ENN",
  keyval = 1,
  overwrite = True)

# 2. r.pi.index
gs.run_command("r.pi.index", 
  flags = "a", 
  input = "mapbiomas_30m_HABMAT", 
  output = "mapbiomas_30m_HABMAT_rpiindex_area",
  method = "area",
  keyval = 1,
  overwrite = True)

gs.run_command("r.pi.index", 
  flags = "a", 
  input = "mapbiomas_30m_HABMAT", 
  output = "mapbiomas_30m_HABMAT_rpiindex_perimeter",
  method = "perimeter",
  keyval = 1,
  overwrite = True)

gs.run_command("r.pi.index", 
  flags = "a", 
  input = "mapbiomas_30m_HABMAT", 
  output = "mapbiomas_30m_HABMAT_rpiindex_shape",
  method = "shape",
  keyval = 1,
  overwrite = True)



 , , border, compactness, asymmetry, area-perimeter, fractal, ENN

# 3. r.pi.enn

# 4. r.pi.fnn



# creates a random generated map
gs.run_command("r.pi.nlm",
	output = "nlm",
	landcover = 50)

gs.run_command("r.pi.nlm",
	input = "mapbiomas_30m_HABMAT",
	output = "nlm_rc",
	keyval = 1,
	landcover = 50,
	overwrite = True)	


# --------------------------------------------------