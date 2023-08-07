r.diversity input=r prefix=div method=shannon 
r.diversity input=r prefix=div method=shannon size=5
r.diversity input=r prefix=div method=shannon size=5
g.region -p
r.diversity input=r prefix=div method=shannon size=3
r.diversity input=r prefix=div method=shannon size=3
r.diversity input=r prefix=div method=shannon size=3
r.diversity input=r prefix=div method=shannon size=3 --o
r.mask -r
r.diversity input=r prefix=div method=shannon size=3 --o
r.diversity input=r prefix=div method=shannon size=5 --o
r.out.gdal in=div_shannon_size_5 out=div_shannon_size_5@PERMANEN
pwd
cd ~/Downloads/
r.out.gdal in=div_shannon_size_5 out=div_shannon_size_5.tif
r.out.gdal in=r out=r.tif
r.li.setup
r.li.setup
r.li
python
r.info r
r.info r -g
r.info -g r
r.li.shannon --overwrite input=r@PERMANENT config=/home/mude/.grass8/r.li/conf_diversity_100 output=div
exit
