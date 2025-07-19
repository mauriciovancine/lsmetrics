
# remotes::install_github("GuangchuangYu/hexSticker")

library(hexSticker)
library(png)

imgurl <- magick::image_read('hexagon/img.png')

sticker(imgurl,
        package = "lsmetrics",
        p_size = 20,
        p_color = "#4f7476",
        s_x = 1, s_y = .8, s_width = 1.2, s_height = 1.2,
        h_fill = "#ffd445",
        #h_fill = "white",
        h_color = "#006821",
        filename = "hexagon/logo.png")

