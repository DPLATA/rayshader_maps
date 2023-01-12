data <- st_read("data/kontur_population_MX_20220630.gpkg")
ags <- getData("GADM", country = "Mexico", level = 1) %>% 
  st_as_sf() %>%
  filter(NAME_1 == "Aguascalientes") |> 
  st_transform(crs= st_crs(data))

ags |>
  ggplot() +
  geom_sf()

st_ags <- st_intersection(data, ags)

bb <- st_bbox(st_ags)


bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) |> 
  st_sfc(crs = st_crs(data))

bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) |> 
  st_sfc(crs = st_crs(data))

ags |> 
  ggplot() +
  geom_sf() +
  geom_sf(data = bottom_left) +
  geom_sf(data = bottom_right, color = "red")

width <- st_distance(bottom_left, bottom_right)

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) |> 
  st_sfc(crs = st_crs(data))

height <- st_distance(bottom_left, top_left)

if (width > height) {
  w_ratio <- 1
  h_ratio <- height / width
} else {
  h_ration <- 1
  w_ratio <- width / height
}

size <- 1000

ags_rast <- st_rasterize(st_ags, 
                         nx = floor(size * w_ratio),
                         ny = floor(size * h_ratio))

mat <- matrix(ags_rast$population, 
              nrow = floor(size * w_ratio),
              ncol = floor(size * h_ratio))


c1 <- met.brewer("OKeeffe2")
swatchplot(c1)

texture <- grDevices::colorRampPalette(c1, bias = 2)(256)
swatchplot(texture)

mat |> 
  height_shade(texture = texture) |> 
  plot_3d(heightmap = mat,
          zscale = 100,
          solid = FALSE,
          shadowdepth = 0)

outfile <- 'obj_models/ags_model.obj'

save_obj(outfile)

