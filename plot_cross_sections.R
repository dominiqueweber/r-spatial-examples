############################################################################
# Plot lidar cross sections
# 
# Author: Dominique Weber
############################################################################

library(dplyr)
library(lidR)
library(terra)
library(sf)

# config
las_t1 <- "data/las_2014.las"
las_t2 <- "data/las_2018.las"
out_pdf <- "profiles.pdf"
epsg <- "epsg:2056"
cs_width <- 3

# load and normalize point cloud
las <- readLAS(las_t1)
st_crs(las) <- epsg
lasn <- normalize_height(las, knnidw())

# calc chm (mainly for visualization)
chm <- rasterize_canopy(lasn, 0.5, pitfree(c(0,2,5,10,15), c(0, 1.5)))

# cross section sample line
aoi_line <- st_linestring(matrix(c(2689867, 1273846, 2689954, 1273901), ncol = 2, byrow = TRUE)) %>%
  st_sfc(crs=epsg)

# draw your own cross sections (click twice on plot)
# if (.Platform$OS.type == "windows") windows()
# plot(chm)
# aoi_line <- draw(x = "lines") %>%
#   st_as_sf(crs=epsg)

# line length
line_length <- st_length(aoi_line)
print(paste("line length:", round(as.numeric(line_length), 2), "m"))

# extract cross section of given buffer width
lasn_cs <- clip_roi(lasn, st_buffer(aoi_line, cs_width))
lasn_cs$Z[lasn_cs$Z < 0] <- 0

# extract coordinates
coords <- st_coordinates(aoi_line)

p0 <- coords[1,]
p1 <- coords[2,]

rho <- sqrt((lasn_cs$X - p0[1])^2 + (lasn_cs$Y - p0[2])^2)

# plot
plot(rho, lasn_cs$Z,
     xlab="rho [m]",
     ylab="Vegetation height [m]",
     main="Cross section profile",
     asp=1,
     pch=20,
     ylim=c(0,50))

################################
# plot 2014 and 2018 points

# process las 2018 data
las2 <- readLAS(las_t2)
st_crs(las2) <- epsg
lasn2 <- normalize_height(las2, knnidw())

lasn_cs2 <- clip_roi(lasn2, st_buffer(aoi_line, cs_width))
lasn_cs2$Z[lasn_cs2$Z < 0] = 0

rho2 <- sqrt((lasn_cs2$X - p0[1])^2 + (lasn_cs2$Y - p0[2])^2)

# plot and store as pdf
pdf(out_pdf, width=10, height=5)

plot(rho2, lasn_cs2$Z,
     xlab = "rho [m]",
     ylab = "Vegetation height [m]",
     main = "Cross section profile",
     asp = 1,
     pch = 16,
     col = adjustcolor("black", alpha.f = 0.3),
     ylim = c(0, 40))

points(rho, lasn_cs$Z,
       pch = 16,
       col = adjustcolor("purple", alpha.f = 0.3))

legend("topleft",
       legend=c("LiDAR 2014", "LiDAR 2018"),
       col=c("purple", "black"),
       pch=c(16,16),
       cex=0.8)

dev.off()