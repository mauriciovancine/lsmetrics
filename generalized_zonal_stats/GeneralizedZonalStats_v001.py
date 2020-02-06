#---------------------------------------------------------------------------------------
"""
 Generalized zonal statistics for GRASS GIS
 
 Bernardo B. S. Niebuhr - bernardo_brandaum@yahoo.com.br
 
 Laboratorio de Ecologia Espacial e Conservacao
 Universidade Estadual Paulista - UNESP
 Rio Claro - SP - Brasil
 
 This script runs within GRASS GIS 7.X environment.
"""
#---------------------------------------------------------------------------------------

# Import modules
import os
import grass.script as grass
import grass.script.vector as v
import grass.script.raster as r
import grass.script.db as db

# Function proportion_habitat
def proportion_habitat(input_rast):
    '''
    input_rast must be a binary 1/0 map
    '''
        
    # Count number of cells of each category with r.stats -c
    cell_counts = grass.read_command('r.stats', flags = 'c', input = input_rast).split('\n')
    # Take only values != ''
    valid_cell_counts = [val for val in cell_counts if val != '']
    
    # Check values of raster
    if len(valid_cell_counts) > 3:
        # If len > 3 (it has more than 0, 1, and null *), return and error
        grass.error('There is a problem with the input raster '+input_rast+'. Raster values must be either 0, 1, or null.')
    
    # If it is ok, go on
    else:
        # Zeros
        zero_count_aux = valid_cell_counts[0].split(' ')
        zero = zero_count_aux[0]
        zero_count = zero_count_aux[1]
        
        # Ones
        one_count_aux = valid_cell_counts[1].split(' ')
        one = one_count_aux[0]
        one_count = one_count_aux[1]
        
        # If len == 3, null may be also present
        null = ''
        if len(valid_cell_counts) == 3:
            # Nulls
            null_count_aux = valid_cell_counts[2].split(' ')
            null = null_count_aux[0]
            null_count = null_count_aux[1]
            
            # If the third value (besides 0 and 1) is not *, return error
            if null != '*':
                grass.error('There is a problem with the input raster '+input_rast+'. The value '+null+' should not be present in the raster.')
            
        # If zero is really zero, when 0 is present
        if zero == '0':
            # Number of zero cells
            zero_count_float =  float(zero_count)
        # When zero is absent (eg 100% habitat)
        elif zero == '1':
            # Number of one cells
            zero_count_float = 0.0
            one_count_float =  float(zero_count)
        else:
            raise Exception('Error in raster cell values n.1.')
         
        # If one is really one, when 1 is present   
        if one == '1':
            # Number of one cells
            one_count_float =  float(one_count)
        # If 1 is absent (eg there is not habitat)
        elif one == '*' and zero == '0':
            one_count_float = 0.0
        
        # If there are null cells, warn the user
        if one == '*' or null == '*':
            if one == '*':
                null_count = one_count
            #grass.warning('There are '+null_count+' null values in this region of the map.')
            # here we should warn about the number of null cells within the mask, but it seems the values outside the mask are being considered too. Find out what to do later.
        
        try: 
            # Proportion of 1's
            proportion_1 = one_count_float/(one_count_float + zero_count_float)
            
            return(100*proportion_1)
        except:
            #raise Exception('Error in raster cell values n.2.')
            grass.error('There is a problem with the input raster '+input_rast+'. It presents the values '+zero+' and '+one+', but should present only 0 and 1.')


# Function number_patches
def number_patches(input_raster_pid, mask = False):
    '''
    input_raster_pid - patch id map
    mask - raster used as mask
    '''
    
    if mask:
        temp_map_name = 'pid_mask'
        grass.mapcalc(temp_map_name+' = '+input_raster_pid)
    else:
        temp_map_name = input_raster_pid
    
    # Assess the number of categories (patch IDs) in the Patch ID input map
    maps_vals_aux = grass.read_command('r.category', map = temp_map_name).split('\n')
    
    # Remove absent values ''
    map_vals = [val for val in maps_vals_aux if val != '']
    
    # NP = length of the list of pid values
    number_of_patches = len(map_vals)
    
    # Delete pid map for the mask region, if applicable
    if mask:
        grass.run_command('g.remove', type = 'raster', pattern = temp_map_name, flags = 'f')
    
    # Return NP
    return number_of_patches
    
# Class generalized_zonal_stats
class GeneralizedZonalStats():
    
    # Function init - load shape and raster maps
    def __init__(self, input_shape, overwrite_shape = False, input_rasters = [], overwrite_rasters = False, folder = ''):
        '''
        Function init
        
        This function initializes the variables (map names) used to perform the Zonal calculations, 
        and loads input vector and raster maps, if either they are not present within GRASS or the 
        overwrite option == True.
        
        Parameters
        ----------
        input_shape: string
            Name of the input shapefile/vector, without extension (.shp), to be used as the base for zonal
            statistics (masks are features/polygons of this vector layer).
        overwrite_shape: bool (True/False)
            If there is already a vector with the same name as the input shapefile in the current mapset,
            this variable states whether this map should be overwritten (the map is re-imported) or not 
            (the same map is kept).
        input_rasters: list with strings
            List with the name of one or multiple input rasters, without extension (.tif). These rasters are
            used to calculate metrics or statistics.
        overwrite_rasters: bool (True/False)
            If there are already rasters with the same name as the ones in the input raster list in the 
            current mapset, this variable states whether these maps should be overwritten 
            (the map is re-imported) or not (the same map is kept).
        folder: string
            Path of the folder where the maps are located.
            
        Returns
        -------
        None.
        Maps may be loaded in the process. This function is always needed to initialize the 
        zonal calculation process.
        '''
        
        # Initialize variable to assess if loading maps was successful
        self.load_ok = False
        # Initialize variable to assess if setting columns was successful
        self.set_cols = False        
        
        # Names of input maps
        self.input_shape = input_shape
        self.input_rasters = input_rasters
        
        # List of maps to be used in zonal statistics
        to_be_used = 'Vector map to be used in zonal statistics:\n'
        
        # Get current mapset
        current_mapset = grass.read_command('g.mapset', flags = 'p').replace('\n','').replace('\r','')
        
        # Get list of imported shape files
        shape_list = grass.list_grouped('vector', pattern = input_shape) [current_mapset]
        shape_exists = len(shape_list) # Length of the list
        
        # If the map does not exist within GRASS mapset or if the user wants to overwrite it, import it
        if (not shape_exists) or (overwrite_shape):
            
            # Change to input folder
            os.chdir(folder)
            # Import shapefile
            grass.run_command('v.in.ogr', input = input_shape+'.shp', output = input_shape, overwrite = True)
            # Message to prompt
            grass.message('The vector map '+input_shape+' was successfully imported to GRASS GIS.')

            # Update list of maps to be used
            to_be_used = to_be_used + input_shape+'\n\n'
            
        else:
            # Message to prompt
            grass.warning('The vector map '+input_shape+' was already present in the mapset and was not imported again; if necessary, please check the overwrite option and retry.')
            
            # If the shapefile already exists, it may be possible to just use it
            if shape_exists:
                # Update list of maps to be used
                to_be_used = to_be_used + input_shape+'\n\n'            
            
        # Get list of imported raster maps
        raster_list = grass.list_grouped('raster') [current_mapset]
        
        # Update list of maps to be used
        to_be_used = to_be_used + 'Raster map(s) to be used in zonal statistics:\n'        
        
        # For each map in the input rasters
        for i in input_rasters:
            
            # If the map does not exist within GRASS or if the user wants to overwrite it, import it
            if (not i in raster_list) or (overwrite_rasters):
                        
                # Change to input folder
                os.chdir(folder)
                # Import raster
                grass.run_command('r.in.gdal', input = i+'.tif', output = i, overwrite = True)
                # Message to prompt
                grass.message('The raster map '+i+' was successfully imported to GRASS GIS.')                
                
                # Update list of maps to be used
                to_be_used = to_be_used + i+'\n'
                
            else:
                # Message to prompt
                grass.warning('The raster map '+i+' was already present in the mapset and was not imported; if necessary, please check the overwrite option and retry.')
                
                # If the raster already exists, it may be possible to just use it
                if i in raster_list:
                    # Update list of maps to be used
                    to_be_used = to_be_used + i+'\n'
                    
        # Print the name of the maps to be used
        grass.message(to_be_used)
        
        # Ok, maps were imported or at least the the list of maps to be considered for zonal stats was loaded successfully
        self.load_ok = True
        
    
    # Function create_new_column - create new columns where zonal stats will be written
    def create_new_column(self, column_names, type_col = ['int']):
        '''
        Function create_new_column
        
        This function crates new columns in the input vector map, to be filled with information from the
        zonal statistics/metrics.
        
        Parameters
        ----------
        column_names: list with strings
            List with the name of the columns to be added. Names should be shorter than 10-characters in length,
            to avoid problems when exporting vector maps as an ESRI shapefile.
            Each column name must be equivalent to one of the input raster maps, in the same order. Therefore,
            both lists must have the same number of elements.
        type_col: list with strings; {'int', 'float', 'string', 'date'}
            List with the type of variable (possible values: 'int', 'double precision', 'varchar', 'date') for each
            column to be added - integer, float, string, or date.
            Each data type must be equivalent to one of the input raster maps, in the same order. Therefore,
            both lists must have the same number of elements.
            
        Returns
        -------
        None.
        '''
        
        # Names of columns
        self.column_names = column_names
        
        # Test if column name is greater than 10 characters and raise error if True
        # This is done to avoid errors with dbf while exporting the vector map as an ESRI shapefile later
        if any([len(i) > 10 for i in column_names]):
            raise ValueError('Column names should be up to 10 characters. Please choose other names and retry.')
        
        # If the length on column names and types is correct, go on
        if len(column_names) == len(self.input_rasters) and len(type_col) == len(self.input_rasters):
            
            # Get list of existing cols in the attribute table of the input shapefile
            existing_cols = v.vector_columns(self.input_shape, getDict = False)
            
            # Create string with col names and col types to be created
            list_cols = ''
            # String with col names to be shown in the end
            list_cols_str = 'The following column(s) were created:\n'
            # String with col names to be used:
            list_cols_use = 'The following column(s) will be filled by zonal statistics:\n'
            
            # For each column, check if it does not exist already and increase the list
            for i in range(len(column_names)):
                
                # Check if column exists
                if column_names[i] in existing_cols:
                    
                    # If the column exists, check whether to overwrite it.
                    overwrite = raw_input('The column '+column_names[i]+' already exists in the attribute table of the input shapefile. Should we overwrite its values (Y/N)?')
                    if overwrite == 'Y' or overwrite == 'y':
                        # Update list to be used
                        list_cols_use = list_cols_use + 'Column: '+column_names[i]+'\n'                    
                    elif overwrite == 'N' or overwrite == 'n':
                        grass.error('Ok. The process was stopped. Please select another column name.\n')
                        raise Exception('')
                    else:
                        grass.error('You did not type a valid option. The process was stopped.\n')
                        raise Exception('')
                # if the column does not exist, create it
                else:
                    
                    # Check variable type
                    if type_col[i] == 'int':
                        type_col_str = 'integer'
                    elif type_col[i] == 'float':
                        type_col_str = 'double precision'
                    elif type_col[i] == 'string':
                        type_col_str = 'varchar(20)' # increase this number if necessary
                        # Type date not implemented here
                    else:
                        g.error('Hey, one of your variables is neither int, float or string!')
                    
                    # Add variable and type to the list of columns to be created
                    list_cols = list_cols+column_names[i]+' '+type_col_str
                    if i < (len(column_names)-1):
                        list_cols = list_cols+', '
                    
                    # Update list to be imported
                    list_cols_str = list_cols_str + 'Column: '+column_names[i]+'; type: '+type_col_str+'\n'
                    
                    # Update list to be used
                    list_cols_use = list_cols_use + 'Column: '+column_names[i]+'\n'                    
                    
                
            # Create columns
            if list_cols != '':
                grass.run_command("v.db.addcolumn", map = self.input_shape, columns = list_cols)
            
            # Message to prompt
            # List to be imported
            grass.message(list_cols_str)
            # List to be used
            grass.message(list_cols_use)
            
            # Initialize variable to assess if setting columns was successful
            self.set_cols = True
        
        # If the length on column names and types is different from the length of the list of input rasters, stop
        else:
            # Message to prompt
            grass.error('The lists of column names and types and the list of input rasters do not have the same length. Columns were not created.')
        
        
    # Function run_zonal_stats - run the the function passed as argument for each zone/feature
    def run_zonal_stats(self, function, select_cats = 'all', *args, **kwargs):
        '''
        Function run_zonal_stats
        
        This function ...
        
        Parameters
        ----------
        function: Python function
            Name of the function to be used to calculate over masks/vetor features.
        cats: list with integers in character form ('1' and not 1)
            List with values of lines of the input vector/shape (cats), representing the polygons/features of
            this vector to be processed. The default is the string 'all', in case all polygons will be processed.
        *args: several
            Argument of the function, not named (only value, e.g. 30, 'int')
        **kwargs: several
            Optional arguments of the function, named (option = value, e.g. threshold = 50)
        '''
        
        # If the previous steps (load/select maps, create/set columns) were done with success, go on
        if self.set_cols and self.load_ok:
            # Get row names (cat)
            cat_info = grass.read_command('db.select', sql = 'SELECT cat FROM '+self.input_shape).split('\n')
            # exclude last line and header
            cats = [val for val in cat_info if val != '' and val != 'cat']
            
            # select only rows/polygons of interest, from the input shape
            if select_cats != 'all' and len(select_cats) <= len(cats): 
                cats = [val for val in cats if val in select_cats]
            
            # We can include something to calculate only for selected features and not all
            
            # For each selected feature
            for cat in cats:
                
                # Take resolution from raster map
                #rast_info = r.raster_info(self.input_rasters[0])
                #ewres = rast_info['ewres']
                #nsres = rast_info['nsres']
                
                ext_polygon = grass.read_command('v.db.select', map = self.input_shape, where = 'cat = '+cat, flags = 'r')
                ext_polygon_list = ext_polygon.split('\n')
                n = [i for i in ext_polygon_list if 'n=' in i][0].replace('n=', '')
                s = [i for i in ext_polygon_list if 's=' in i][0].replace('s=', '')
                e = [i for i in ext_polygon_list if 'e=' in i][0].replace('e=', '')
                w = [i for i in ext_polygon_list if 'w=' in i][0].replace('w=', '')                
                
                # Create a raster for the feature
                #grass.run_command('g.region', vector = self.input_shape, ewres = ewres, nsres = nsres, align = self.input_rasters[0])
                grass.run_command('g.region', n = n, s = s, e = e, w = w, align = self.input_rasters[0])
                grass.run_command('v.to.rast', input = self.input_shape, output = 'temp_rast', cats = cat, use='val', overwrite = True)
                
                # Set region to the feature
                grass.run_command('g.region', raster = 'temp_rast', zoom = 'temp_rast') 
                
                # Run r.mask for the feature
                grass.run_command('r.mask', vector = self.input_shape, cats = cat, overwrite = True)
                
                # For each raster/column
                for i in range(len(self.input_rasters)):
                    
                    # Column and raster
                    col = self.column_names[i]
                    rast = self.input_rasters[i]
                    
                    # Run function
                    val = function(rast, *args, **kwargs)
                    
                    # Update value in the input shape attribute table
                    grass.run_command('v.db.update', map = self.input_shape, column = col, value = str(val), where='cat = '+cat)
                    
                # Remove mask
                grass.run_command('r.mask', flags = 'r')
                
                # Remove temp raster
                grass.run_command('g.remove', flags = 'f', type = 'raster', pattern = 'temp_rast', verbose = False)
                
                # Message - cat ok
                grass.message("Feature "+str(cat)+' processed with success!')
        
        # If any of the previous moments were not successful, stop.
        else:
            if not self.load_ok:
                raise Exception('Maps were not loaded successfully. Please retry.')
            
            if not self.set_cols:
                raise Exception('Columns were not set successfully. Please retry.')
            
            
    # Function run_zonal_stats v2 - run the the function passed as argument for each zone/feature
    def run_zonal_stats_v2(self, function, select_cats = 'all', *args, **kwargs):
        '''
        Function run_zonal_stats
        
        This function ...
        
        Parameters
        ----------
        function: Python function
            Name of the function to be used to calculate over masks/vetor features.
        cats: list with integers in character form ('1' and not 1)
            List with values of lines of the input vector/shape (cats), representing the polygons/features of
            this vector to be processed. The default is the string 'all', in case all polygons will be processed.
        *args: several
            Argument of the function, not named (only value, e.g. 30, 'int')
        **kwargs: several
            Optional arguments of the function, named (option = value, e.g. threshold = 50)
        '''
        
        # If the previous steps (load/select maps, create/set columns) were done with success, go on
        if self.set_cols and self.load_ok:
            # Get row names (cat)
            cat_info = grass.read_command('db.select', sql = 'SELECT cat FROM '+self.input_shape).split('\n')
            # exclude last line and header
            cats = [val for val in cat_info if val != '' and val != 'cat']
            
            # select only rows/polygons of interest, from the input shape
            if select_cats != 'all' and len(select_cats) <= len(cats): 
                cats = [val for val in cats if val in select_cats]
            
            # We can include something to calculate only for selected features and not all
            
            # For each selected feature
            for cat in cats:
                
                # Take resolution from raster map
                #rast_info = r.raster_info(self.input_rasters[0])
                #ewres = rast_info['ewres']
                #nsres = rast_info['nsres']
                
                ext_polygon = grass.read_command('v.db.select', map = self.input_shape, where = 'cat = '+cat, flags = 'r')
                ext_polygon_list = ext_polygon.split('\n')
                n = [i for i in ext_polygon_list if 'n=' in i][0].replace('n=', '')
                s = [i for i in ext_polygon_list if 's=' in i][0].replace('s=', '')
                e = [i for i in ext_polygon_list if 'e=' in i][0].replace('e=', '')
                w = [i for i in ext_polygon_list if 'w=' in i][0].replace('w=', '')                
                
                # Create a raster for the feature
                #grass.run_command('g.region', vector = self.input_shape, ewres = ewres, nsres = nsres, align = self.input_rasters[0])
                grass.run_command('g.region', n = n, s = s, e = e, w = w, align = self.input_rasters[0])
                grass.run_command('v.to.rast', input = self.input_shape, output = 'temp_rast', cats = cat, use='val', overwrite = True)
                
                # Set region to the feature
                grass.run_command('g.region', raster = 'temp_rast', zoom = 'temp_rast') 
                
                # For each raster/column
                for i in range(len(self.input_rasters)):
                    
                    # Column and raster
                    col = self.column_names[i]
                    rast = self.input_rasters[i]
                    
                    # Run the equivalent to 'r.mask' for the feature, but with r.mapcalc
                    rast_cut = 'temp_rast_input'
                    expression = rast_cut+' = if(temp_rast == 1, '+rast+', null())'
                    grass.mapcalc(expression, overwrite = True)
                    
                    # Run function
                    val = function(rast_cut, *args, **kwargs)
                    
                    # Update value in the input shape attribute table
                    grass.run_command('v.db.update', map = self.input_shape, column = col, value = str(val), where='cat = '+cat)
                    
                    # Remove raster of interest for the polygon
                    grass.run_command('g.remove', flags = 'f', type = 'raster', pattern = rast_cut, verbose = False)
                
                # Remove temp raster
                grass.run_command('g.remove', flags = 'f', type = 'raster', pattern = 'temp_rast', verbose = False)
                
                # Message - cat ok
                grass.message("Feature "+str(cat)+' processed with success!')
        
        # If any of the previous moments were not successful, stop.
        else:
            if not self.load_ok:
                raise Exception('Maps were not loaded successfully. Please retry.')
            
            if not self.set_cols:
                raise Exception('Columns were not set successfully. Please retry.')        
    


        
