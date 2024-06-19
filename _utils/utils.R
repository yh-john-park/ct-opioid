sysfonts::font_add_google(name = "Barlow", 
                         "barlow")
sysfonts::font_add_google(name = "Barlow Semi Condensed", 
                          "barcond")

showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)


# colors
pastel <- c("gray70", rcartocolor::carto_pal(12, "Pastel"))
div <- rcartocolor::carto_pal(5, "Geyser")
div7 <- rcartocolor::carto_pal(7, "Geyser")
greens <- c('#f0eeaa', '#c0d889', '#98c082', '#72a680', '#4d8b7e', '#277278', '#015969')
purp_or <- rcartocolor::carto_pal(7, "PurpOr")
purp_yl <- c('#f0eeaa','#ddd3b3','#c7b9b9','#aea0bc','#938abb','#7475b3','#4c669b')
purp <- "#7F6FD1"


# ggplot stuff
ggplot2::update_geom_defaults("text", list(family = "barcond", fontface = "bold", size = 3))

theme_din2 <- function(base_family = "barcond", base_size = 12, ...) {
  camiller::theme_din(base_family = base_family, base_size = base_size, ...) + 
    ggplot2::theme(legend.text = ggplot2::element_text(size = ggplot2::rel(0.75)), 
                   legend.key.size = ggplot2::unit(1.1, "lines"), 
                   legend.title = ggplot2::element_text(size = ggplot2::rel(0.9)),
                   plot.title = ggplot2::element_text(size = ggplot2::rel(1.1), family = "barlow"),
                   plot.subtitle = ggplot2::element_text(size = ggplot2::rel(0.9), family = "barlow"),
                   strip.text = element_text(face = "bold"),
                   strip.background = ggplot2::element_blank())
}

theme_title <- function(text, ...) {
  cowplot::ggdraw() +
    themed_label(text,
                element = "plot.title", x = 0.02, hjust = 0, vjust = 1, theme = ggplot2::theme_get(), ...) +
    theme(plot.title = element_text(hjust = 0, margin = margin(0, 0, 4, 0, "pt")),
         plot.margin = margin(0, 0, 0, 0, "pt"))
}

theme_subtitle <- function(text, ...) {
  cowplot::ggdraw() +
    themed_label(text,
                 element = "plot.subtitle", x = 0.02, hjust = 0, vjust = 0, theme = ggplot2::theme_get(), ...) +
    theme(plot.subtitle = element_text(hjust = 0, margin = margin(0, 0, 1.5, 0, "lines")),
          plot.margin = margin(0, 0, 0, 0, "lines"))
}

scale_y_barcontinuous <- function(...) {
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.05)), ...)
}


