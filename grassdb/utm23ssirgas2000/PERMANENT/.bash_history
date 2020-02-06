g.gui
g.gui
clear
g.gui
# iniciar o python
python
# importar bibliotecas
import os
import grass.script as gs
python
pyenv
pyenv version
g.remove -f type=vector name=SP_3543907_USO2@PERMANENT
python
g.gui
clar
clear
# iniciar o python
python
g.remove -f type=raster name=null_uso_HABMAT@PERMANENT
g.remove -f type=raster name=null_uso_HABMAT@PERMANENT,together_0001_uso_HABMAT_pid@PERMANENT
g.remove -f type=raster name=null_uso_HABMAT@PERMANENT,together_0001_uso_HABMAT_pid@PERMANENT,together_0001_uso_HABMAT@PERMANENT
g.remove -f type=raster name=null_uso_HABMAT@PERMANENT,together_0001_uso_HABMAT_pid@PERMANENT,together_0001_uso_HABMAT@PERMANENT,together_0001_uso_HABMAT_patch_AreaHA@PERMANENT
g.remove -f type=raster name=null_uso_HABMAT@PERMANENT,together_0001_uso_HABMAT_pid@PERMANENT,together_0001_uso_HABMAT@PERMANENT,together_0001_uso_HABMAT_patch_AreaHA@PERMANENT,02_patch_size_uso_patch_AreaHA@PERMANENT
g.remove -f type=raster name=null_uso_HABMAT@PERMANENT,together_0001_uso_HABMAT_pid@PERMANENT,together_0001_uso_HABMAT@PERMANENT,together_0001_uso_HABMAT_patch_AreaHA@PERMANENT,02_patch_size_uso_patch_AreaHA@PERMANENT,02_patch_size_uso_pid@PERMANENT
g.remove -f type=raster name=null_uso_HABMAT@PERMANENT,together_0001_uso_HABMAT_pid@PERMANENT,together_0001_uso_HABMAT@PERMANENT,together_0001_uso_HABMAT_patch_AreaHA@PERMANENT,02_patch_size_uso_patch_AreaHA@PERMANENT,02_patch_size_uso_pid@PERMANENT,zero_uso_HABMAT@PERMANENT
g.remove -f type=raster name=02_patch_size_null_uso_HABMAT_patch_AreaHA@PERMANENT
g.remove -f type=raster name=02_patch_size_null_uso_HABMAT_patch_AreaHA@PERMANENT,02_patch_size_null_uso_HABMAT_pid@PERMANENT
g.remove -f type=raster name=02_patch_size_null_uso_HABMAT_patch_AreaHA@PERMANENT,02_patch_size_null_uso_HABMAT_pid@PERMANENT,02_patch_size_zero_uso_HABMAT_patch_AreaHA@PERMANENT
g.remove -f type=raster name=02_patch_size_null_uso_HABMAT_patch_AreaHA@PERMANENT,02_patch_size_null_uso_HABMAT_pid@PERMANENT,02_patch_size_zero_uso_HABMAT_patch_AreaHA@PERMANENT,02_patch_size_zero_uso_HABMAT_pid@PERMANENT
g.remove -f type=raster name=02_patch_size_null_uso_HABMAT_patch_AreaHA@PERMANENT,02_patch_size_null_uso_HABMAT_pid@PERMANENT,02_patch_size_zero_uso_HABMAT_patch_AreaHA@PERMANENT,02_patch_size_zero_uso_HABMAT_pid@PERMANENT,null_uso_HABMAT@PERMANENT
g.remove -f type=raster name=02_patch_size_null_uso_HABMAT_patch_AreaHA@PERMANENT,02_patch_size_null_uso_HABMAT_pid@PERMANENT,02_patch_size_zero_uso_HABMAT_patch_AreaHA@PERMANENT,02_patch_size_zero_uso_HABMAT_pid@PERMANENT,null_uso_HABMAT@PERMANENT,zero_uso_HABMAT@PERMANENT
g.remove -f type=raster name=02_patch_size_zero_uso_HABMAT_patch_AreaHA@PERMANENT
g.remove -f type=raster name=02_patch_size_zero_uso_HABMAT_patch_AreaHA@PERMANENT,02_patch_size_zero_uso_HABMAT_pid@PERMANENT
g.remove -f type=raster name=02_patch_size_zero_uso_HABMAT_patch_AreaHA@PERMANENT,02_patch_size_zero_uso_HABMAT_pid@PERMANENT,02_patch_size_zero_uso_HABMAT_pid@PERMANENT
g.remove -f type=raster name=02_patch_size_zero_uso_HABMAT_patch_AreaHA@PERMANENT,02_patch_size_zero_uso_HABMAT_pid@PERMANENT,02_patch_size_zero_uso_HABMAT_pid@PERMANENT,zero_uso_HABMAT@PERMANENT
g.remove -f type=raster name=02_patch_size_zero_uso_HABMAT_patch_AreaHA@PERMANENT,02_patch_size_zero_uso_HABMAT_pid@PERMANENT,02_patch_size_zero_uso_HABMAT_pid@PERMANENT,zero_uso_HABMAT@PERMANENT,02_patch_size_zero_uso_HABMAT_pid@PERMANENT
g.remove -f type=raster name=02_patch_size_zero_uso_HABMAT_pid@PERMANENT
g.remove -f type=raster name=02_patch_size_zero_uso_HABMAT_pid@PERMANENT,02_patch_size_zero_uso_HABMAT_pid@PERMANENT
g.gui
python
g.remove -f type=raster pattern=_
g.remove -f type=raster exclude=_
g.remove -f type=raster exclude="_"
g.remove -f type=raster pattern="_"
g.remove -f type=raster pattern="_" exclude="_"
g.remove -f type=raster exclude=_
g.remove -f type=raster pattern=_
g.remove -f type=raster pattern=*_*
g.gui
# iniciar o python
python
g.gui
# iniciar o python
python
v.rast.stats map=hex_1000_uso@PERMANENT raster=02_patch_size_bin_zero_uso_HABMAT_pid@PERMANENT column_prefix=stats_
v.rast.stats map=hex_1000_uso@PERMANENT raster=02_patch_size_bin_zero_uso_HABMAT_pid@PERMANENT column_prefix=stats method=number
# iniciar o python
python
g.gui
# iniciar o python
python
# iniciar o python
python
# iniciar o python
python
g.remove -f type=raster name=01_patch_number_bin_zero_uso_HABMAT_patch_AreaHA@PERMANENT
g.remove -f type=raster name=01_patch_number_bin_zero_uso_HABMAT_patch_AreaHA@PERMANENT,01_patch_number_bin_zero_uso_HABMAT_pid@PERMANENT
# iniciar o python
python
