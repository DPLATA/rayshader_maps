library(rayshader)
library(MetBrewer)
library(colorspace)
library(sf)
library(raster)
library(ggplot2)
library(stars)
library(tidyverse)

mex_population <- st_read("data/kontur_population_MX_20220630.gpkg")
mxRDS <- readRDS("data/gadm36_MEX_1_sp.rds")
state <- mxRDS %>% 
  st_as_sf() %>%
  filter(NAME_1 == "Nuevo León") |> 
  st_transform(crs= st_crs(mex_population))

state |>
  ggplot() +
  geom_sf()

population_state_intersection <- st_intersection(mex_population, state)

bb <- st_bbox(population_state_intersection)


bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) |> 
  st_sfc(crs = st_crs(mex_population))

bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) |> 
  st_sfc(crs = st_crs(mex_population))

state |> 
  ggplot() +
  geom_sf() +
  geom_sf(data = bottom_left) +
  geom_sf(data = bottom_right, color = "red")

width <- st_distance(bottom_left, bottom_right)

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) |> 
  st_sfc(crs = st_crs(mex_population))

height <- st_distance(bottom_left, top_left)

if (width > height) {
  w_ratio <- 1
  h_ratio <- height / width
} else {
  h_ratio <- 1
  w_ratio <- width / height
}

size <- 1000

nx <- floor(size * w_ratio)
ny <- floor(size * h_ratio)

# fix hardcoding
state_rasterize <- st_rasterize(population_state_intersection, 
                                nx = 546,
                                ny = 1000)

population_matrix <- matrix(state_rasterize$population, 
                            nrow = floor(size * w_ratio),
                            ncol = floor(size * h_ratio))


c1 <- met.brewer("VanGogh3")
swatchplot(c1)

texture <- grDevices::colorRampPalette(c1, bias = 1)(256)
swatchplot(texture)

population_matrix |> 
  height_shade(texture = texture) |> 
  plot_3d(heightmap = population_matrix,
          zscale = 100,
          solid = FALSE,
          shadowdepth = 0)

png_outfile <- 'imgs/nuevo_leon.png'

render_camera(theta = -20, phi = 45, zoom = .8)

render_highquality(
  filename = png_outfile,
  interactive = FALSE,
  lightdirection = 280,
  lightaltitude = c(20, 80),
  lightcolor = c(c1[2], "white"),
  lightintensity = c(600, 100),
)


# higher quality render

size <- 2000

nx <- floor(size * w_ratio)
ny <- floor(size * h_ratio)

# fix hardcoding
state_rasterize <- st_rasterize(population_state_intersection, 
                                nx = 1093,
                                ny = 2000)

population_matrix <- matrix(state_rasterize$population, 
                            nrow = floor(size * w_ratio),
                            ncol = floor(size * h_ratio))


c1 <- met.brewer("VanGogh3")
swatchplot(c1)

texture <- grDevices::colorRampPalette(c1, bias = 2)(256)
swatchplot(texture)

population_matrix |> 
  height_shade(texture = texture) |> 
  plot_3d(heightmap = population_matrix,
          zscale = 100 / 2,
          solid = FALSE,
          shadowdepth = 0)

{
  png_outfile <- 'hd_imgs/nuevo_leon.png'
  render_camera(theta = -20, phi = 45, zoom = .8)
  
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")
  if (!file.exists(png_outfile)) {
    png::writePNG(matrix(1), target = png_outfile)
  }
  render_highquality(
    filename = png_outfile,
    interactive = FALSE,
    lightdirection = 280,
    lightaltitude = c(20, 80),
    lightcolor = c(c1[2], "white"),
    lightintensity = c(600, 100),
    samples = 300,
    width = 2000,
    height = 2000
  )
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
  }

