library(rayshader)
library(MetBrewer)
library(colorspace)
library(sf)
library(raster)
library(ggplot2)
library(stars)
library(tidyverse)
library(sp)

mex_population <- st_read("data/kontur_population_MX_20220630.gpkg")
mxRDS <- readRDS("data/gadm36_MEX_1_sp.rds")
df_state <- mxRDS %>% 
  st_as_sf() %>%
  filter(NAME_1 == "Distrito Federal") |> 
  st_transform(crs= st_crs(mex_population))

df_state |>
  ggplot() +
  geom_sf()

df_population_state_intersection <- st_intersection(mex_population, df_state)

df_bb <- st_bbox(df_population_state_intersection)


df_bottom_left <- st_point(c(df_bb[["xmin"]], df_bb[["ymin"]])) |> 
  st_sfc(crs = st_crs(mex_population))

df_bottom_right <- st_point(c(df_bb[["xmax"]], df_bb[["ymin"]])) |> 
  st_sfc(crs = st_crs(mex_population))

df_state |> 
  ggplot() +
  geom_sf() +
  geom_sf(data = df_bottom_left) +
  geom_sf(data = df_bottom_right, color = "red")

df_width <- st_distance(df_bottom_left, df_bottom_right)

df_top_left <- st_point(c(df_bb[["xmin"]], df_bb[["ymax"]])) |> 
  st_sfc(crs = st_crs(mex_population))

df_height <- st_distance(df_bottom_left, df_top_left)

if (df_width > df_height) {
  df_w_ratio <- 1
  df_h_ratio <- df_height / df_width
} else {
  df_h_ratio <- 1
  df_w_ratio <- df_width / df_height
}

df_size <- 1000

nx <- floor(df_size * df_w_ratio)
ny <- floor(df_size * df_h_ratio)

# fix hardcoding
df_state_rasterize <- st_rasterize(df_population_state_intersection, 
                                nx = 902,
                                ny = 1000)

df_population_matrix <- matrix(df_state_rasterize$population, 
                            nrow = floor(df_size * df_w_ratio),
                            ncol = floor(df_size * df_h_ratio))


c1 <- met.brewer("VanGogh3")
swatchplot(c1)

texture <- grDevices::colorRampPalette(c1, bias = 1)(256)
swatchplot(texture)

df_population_matrix |> 
  height_shade(texture = texture) |> 
  plot_3d(heightmap = df_population_matrix,
          zscale = 100,
          solid = FALSE,
          shadowdepth = 0)

png_outfile <- 'imgs/cdmx.png'

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

nx <- floor(size * df_w_ratio)
ny <- floor(size * df_h_ratio)

# fix hardcoding
state_rasterize <- st_rasterize(df_population_state_intersection, 
                                nx = 1805,
                                ny = 2000)

population_matrix <- matrix(state_rasterize$population, 
                            nrow = floor(size * df_w_ratio),
                            ncol = floor(size * df_h_ratio))


c1 <- met.brewer("OKeeffe2")
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
  png_outfile <- 'hd_imgs/cdmx.png'
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


