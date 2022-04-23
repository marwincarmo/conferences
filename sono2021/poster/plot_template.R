library(dplyr)
library(ggplot2)
library(scales)

windowsFonts(`Roboto Condensed` = windowsFont("Roboto Condensed"))
# tema para ggplot ----
my_theme <- theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(panel.grid.minor = element_blank(),
        # Bold, bigger title
        plot.title = element_text(face = "bold", size = rel(1.7)),
        # Plain, slightly bigger subtitle that is grey
        plot.subtitle = element_text(face = "plain", size = rel(1.3), color = "grey70"),
        # Italic, smaller, grey caption that is left-aligned
        plot.caption = element_text(face = "italic", size = rel(0.7), 
                                    color = "grey70", hjust = 0),
        legend.position = c("top"),
        legend.text = element_text(size=12),
        # Bold legend titles
        legend.title = element_text(face = "bold"),
        # Bold, slightly larger facet titles that are left-aligned for the sake of repetition
        strip.text = element_text(face = "bold", size = rel(1.1), hjust = 0),
        # Bold axis titles
        axis.title = element_text(face = "bold"),
        # Add some space above the x-axis title and make it left-aligned
        axis.title.x = element_text(margin = margin(t = 10), hjust = 0),
        axis.text.x = element_text(color = "grey70"),
        # Add some space to the right of the y-axis title and make it top-aligned
        axis.title.y = element_text(margin = margin(r = 10), hjust = 1),
        axis.text.y = element_text(color = "grey70"),
        # Add a light grey background to the facet titles, with no borders
        strip.background = element_rect(fill = "grey90", color = NA))