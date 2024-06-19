# from Kelly's code - OD2A

sysfonts::font_add_google(name = "Barlow", 
                          "barlow")
sysfonts::font_add_google(name = "Barlow Semi Condensed", 
                          "barcond")

showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

theme_src <- function(base_family = "Barlow", base_size = 11, ...) {
  camiller::theme_din(base_family = base_family, base_size = base_size, ...) +
    theme(axis.text = element_text(color = "black"),
          strip.text = element_text(color = "black", size = 9))
}

seq_pal <- RColorBrewer::brewer.pal(5, "YlGnBu")
div_pal <- as.character(khroma::color("PRGn")(7))
base_col <- "#047e87"
qual_pal <- as.character(khroma::color("vibrant")(7))

update_geom_defaults("text", list(family = "Barlow", fontface = "bold", size = 3))

update_geom_defaults("col", list(fill = qual_pal[2]))
theme_set(theme_src())
