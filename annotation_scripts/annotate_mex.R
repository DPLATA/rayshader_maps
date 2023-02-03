library(magick)
library(MetBrewer)
library(colorspace)
library(ggplot2)
library(glue)
library(stringr)

img <- image_read("hd_imgs/mex.png")

colors<- met.brewer("VanGogh3")
swatchplot(colors)

text_color <- darken(colors[7], .25)
swatchplot(text_color)

annot <- glue("Población estimada seccionada en hexágonos de 400 m") |>
  str_wrap(60)

img |> 
  image_crop(gravity = "center",
             geometry = "2000x2000+0-150") |> 
  image_annotate("Densidad poblacional de México",
                 gravity = "northwest",
                 location = "+200+50",
                 color = text_color,
                 size = 100,
                 weight = 700,
                 font = "Futura") |> 
  image_annotate(annot,
                 gravity = "northwest",
                 location = "+275+220",
                 color = text_color,
                 size = 50,
                 font = "Futura") |> 
  image_annotate(glue("Gráfica por Diego Plata (@foreverpelon) | ",
                      "Data: Kontur Population 30 de junio de 2022"),
                 gravity = "south",
                 location = "+0+50",
                 font = "Futura",
                 color = alpha(text_color, .5),
                 size = 35) |> 
  image_write("annotated_hd_imgs/annotated_hd_mex.png")
