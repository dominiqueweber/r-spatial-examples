# -------------------------------------------------------------------------
# Rayshader 3D LiDAR forest plot
# 
# Author: dominique.weber@wsl.ch, based on script of Moritz Bruggisser
# -------------------------------------------------------------------------

# libs
library(lidR)
library(sf)
library(terra)
library(rayshader)
library(paletteer)

# download data directly from swisstopo (https://www.swisstopo.admin.ch/de/hoehenmodell-swisssurface3d)
las_path <- "https://data.geo.admin.ch/ch.swisstopo.swisssurface3d/swisssurface3d_2021_2641-1131/swisssurface3d_2021_2641-1131_2056_5728.las.zip"
download.file(las_path, "D:/swisssurface3d_2021_2641-1131_2056_5728.las.zip")
unzip("D:/swisssurface3d_2021_2641-1131_2056_5728.las.zip", exdir="D:/")

# read point cloud
las <- readLAS("D:/2641_1131.las")

# inspect the point cloud
plot(las)

# clip to region of interest (ROI)
roi <- st_bbox(c(xmin = 2641000, xmax = 2641100, ymax = 1131400, ymin = 1131300), crs=2056)
las <- clip_roi(las, roi)

# normalize (to get tree heights)
nlas = normalize_height(las, knnidw())

# remove ground points
las_ng = filter_poi(nlas, Classification != 2, Z >0.1)

# add normalized height information
las_ng = add_attribute(las_ng, las_ng$Z, "nZ")
las_ng = unnormalize_height(las_ng)

# create DTM
dtm = rasterize_terrain(las, 0.25, tin(), pkg = "raster")

# init 3D plot with DTM
zscale_val = 0.4
elmat = raster_to_matrix(dtm)
elmat %>%
  sphere_shade(zscale = zscale_val, 
               texture=create_texture("cornsilk","cornsilk4","cornsilk","cornsilk","cornsilk")
  ) %>%
  add_shadow(ray_shade(elmat), 0.6) %>%
  plot_3d(elmat, zscale = zscale_val, fov = 0.5, theta = 310, zoom = 0.8, phi = 35, windowsize = c(1400, 700), solid=F) 

# add vegetation points
render_points(extent = attr(dtm,"extent"),
              lat = las_ng@data$Y,
              long = las_ng@data$X,
              altitude = las_ng@data$Z,
              zscale=zscale_val,
              offset = 0,
              # color=viridis::mako(length(las_ng@data$nZ))[rank(las_ng@data$nZ)],
              color=paletteer_c("ggthemes::Classic Green", length(las_ng@data$nZ))[rank(las_ng@data$nZ)],
              size = 2)

render_snapshot("D:/las_ray.png")
