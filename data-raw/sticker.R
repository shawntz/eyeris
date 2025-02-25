## code to prepare `sticker` dataset goes here

sysfonts::font_add_google("Nunito")

showtext::showtext_auto()

imgurl <- file.path("data-raw", "noun-eye-7147281.png")
img <- png::readPNG(imgurl)
img_grob <- grid::rasterGrob(img, interpolate = TRUE)

combined_plot <- cowplot::ggdraw() +
  cowplot::draw_plot(sine_wave, 0, 0.75, 1, 1, scale = 4) +
  cowplot::draw_grob(img_grob, 0, -0.1, 1, 1, 2.75)

sticker <- hexSticker::sticker(
  combined_plot,
  package = "EYERIS",
  h_fill = "#820000", h_color = "#820000",
  s_x = 1, s_y = 1.28, s_width = 0.35,
  p_x = 1, p_y = 0.78, p_size = 22, p_family = "Nunito",
  filename = "inst/figures/sticker.png"
)

sticker

usethis::use_data(sticker, overwrite = TRUE)
