r.colors map=mapbiomas@PERMANENT color=viridis
g.remove type=raster name=mapbiomas@PERMANENT
r.colors map=mapbiomas_af@PERMANENT color=viridis
# python
python3
g.remove type=raster pattern=*fragment_Area*
g.remove -f type=raster pattern=*fragment_Area*
g.remove -f type=raster pattern=*fid*
g.remove type=raster
g.remove type=raster pattern=*02_fragment_area_*
g.remove type=raster pattern=02_fragment_area_*
g.remove -f type=raster pattern=02_fragment_area_*
g.remove -f type=raster pattern=02_fragment_size*
g.remove -f type=raster name=bin_zero_forest_mapbiomas_30m_HABMAT@PERMANENT
g.remove -f type=raster pattern=00_*
g.remove -f type=raster pattern=01_*
g.remove -f type=raster pattern=02_*
g.remove -f type=raster pattern=02_frag*
g.remove -f type=raster pattern=03_frag*
g.remove -f type=raster pattern=03_struc*
g.remove -f type=raster name=03_structural_connectivity_0001_00_binary_mapbiomas_30m_HABMAT_0030m_fid@PERMANENT
g.remove -f type=raster name=03_structural_connectivity_0001_00_binary_mapbiomas_30m_HABMAT_0030m_fid@PERMANENT,mapbiomas@PERMANENT
g.remove -f type=raster pattern=0001*
g.remove -f type=raster name=0001_mapbiomas_30m_HABMAT_0030m_fid@PERMANENT pattern=0001*
g.remove -f type=raster name=0001_mapbiomas_30m_HABMAT_0030m_fid@PERMANENT pattern=0001*
g.remove -f type=raster name=0001_mapbiomas_30m_HABMAT_0030m_fid@PERMANENT
g.remove -f type=raster name=0001_mapbiomas_30m_HABMAT_0030m_fid@PERMANENT,hex@PERMANENT
g.remove -f type=raster pattern=*pct*
g.remove -f type=raster pattern=*HABMAT*
g.remove -f type=raster name=mapbiomas_30m_HABMAT_0030m_fid@PERMANENT
g.remove -f type=raster name=mapbiomas_30m_HABMAT_0030m_fid@PERMANENT,raster_id_cat@PERMANENT
g.list rast pat=*patch*
g.list rast pat=*Area*
g.list rast pat=*patch_AreaHA*
# python
python3
cd /home/mude/data/github/lsmetrics/lsmetrics
ls
python3 LSMetrics_v0_9_1.py
# python
python3
# python
python3
# python
python3
# python
python3
# python
python3
# python
python3
V
# python
python3
# python
python3
# python
python3
g.extension -l
g.extension extension=r.pi
sudo g.extension extension=r.pi
sudo g.extension extension=r.pi
g.extension extension=r.pi
g.extension extension=r.stream.distance
g.extension extension=r.li
g.gui.rlisetup
r.li.patchnum input=mapbiomas_30m_HABMAT_pid conf=r.li out=mapbiomas_30m_HABMAT_number_patch
r.li.patchnum input=mapbiomas_30m_HABMAT_pid conf=r.li out=mapbiomas_30m_HABMAT_number_patch --o
# python
python3
exit
g.remove -f type=vector pattern=vector_cat*
# python
python3
g.extension ext=r.pi
# python
python3
exit
# python
python3
exit
g.gui
sudo g.gio
sudo g.gui
g.gui
g.gui
# python
python3
g.extension.all -f
exit
r.pi.index -a input=mapbiomas_30m output=mapbiomas_30m_index method=area keyval=3
r.pi
g.extension extension=r.pi
g.extension -s extension=r.pi
su g.extension -s extension=r.pi
sudo g.extension -s extension=r.pi
g.extension -d extension=r.pi
make MODULE_TOPDIR=/usr/lib/grass78 ARCH_DISTDIR=/tmp/grass7-mude-170439/tmprnseyssw/r.pi INST_DIR=/home/mude/.grass7/addons install
make MODULE_TOPDIR=/usr/lib/grass78 ARCH_DISTDIR=/tmp/grass7-mude-170439/tmprnseyssw/r.pi INST_DIR=/home/mude/.grass7/addons
g.extension -i extension=r.pi
g.extension extension=r.pi
g.extension extension=r.pi operation=add
g.extension -s extension=r.pi operation=add
exit
g.remove -f type=raster pattern=mapbiomas_30m_HABMAT_null_hex*
r.pi
r.pi.enn
r.pi.index -a input=mapbiomas_30m output=mapbiomas_30m_index method=area keyval=3
# python
python3
g.gui
patch_size(input_maps = map_list_bin, 
           zero = False, 
           diagonal = False,
           prepare_biodim = False, 
           calc_statistics = True, 
           remove_trash = True,
           prefix = "", 
           add_counter_name = False, 
           export = True, 
           export_pid = True, 
           dirout = output_dir)
# python
python3
exit
