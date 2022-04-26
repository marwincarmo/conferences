library(qgraph)
library(reshape2)

my_pretty_theme <- theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(panel.grid.minor = element_blank(),
        # Bold, bigger title
        plot.title = element_text(face = "bold", size = rel(1.7)),
        # Plain, slightly bigger subtitle that is grey
        plot.subtitle = element_text(face = "plain", size = rel(1.3), color = "grey70"),
        # Italic, smaller, grey caption that is left-aligned
        plot.caption = element_text(face = "italic", size = rel(0.7), 
                                    color = "grey70", hjust = 0),
        legend.position = c("top"),
        legend.text = element_text(size=12, color = "grey40"),
        # Bold legend titles
        legend.title = element_text(face = "bold"),
        # Bold, slightly larger facet titles that are left-aligned for the sake of repetition
        strip.text = element_text(face = "bold", size = rel(1.1), hjust = 0),
        # Bold axis titles
        axis.title = element_text(face = "bold"),
        # Add some space above the x-axis title and make it left-aligned
        axis.title.x = element_text(margin = margin(t = 10), hjust = 0),
        axis.text.x = element_text(color = "grey40"),
        # Add some space to the right of the y-axis title and make it top-aligned
        axis.title.y = element_text(margin = margin(r = 10), hjust = 1),
        axis.text.y = element_text(color = "grey70", size = 14),
        # Add a light grey background to the facet titles, with no borders
        strip.background = element_rect(fill = "grey90", color = NA))

# Items centrality --------------------------------------------------------

# compute centrality estimates
str1 <- centrality_auto(Network_factors)$node.centrality[,"Strength"] %>%
  scale() |> 
  as.vector()
names(str1) <- colnames(Network_factors)

cls1 <- centrality_auto(Network_factors)$node.centrality[,"Closeness"] %>%
  scale() |>
  as.vector()
names(cls1) <- colnames(Network_factors)

bet1 <- centrality_auto(Network_factors)$node.centrality[,"Betweenness"] %>%
  scale() |>
  as.vector()
names(bet1) <- colnames(Network_factors)

# merge centrality estimates in a dataframe
cnt1 <- merge(str1[1:5], cls1[1:5], by = 0, all = TRUE) %>%
  rename(node = Row.names, Strength = x, Closeness = y) %>%
  merge(bet1[1:5], by.x = "node", by.y = 0, all = TRUE) %>%
  rename(Betweenness = y)

cnt1$node <- factor(cnt1$node, levels = 1:5,
                    labels = c("ISI", "Consequences", "Worry", "Expectations", "Medication"))

cnt_mlt1 <- melt(cnt1, id.vars = "node",
                 variable.name = "level",
                 value.name = "centrality")
cnt_mlt1 <- arrange(cnt_mlt1, level, node)

# generate the plot using package ggplot2
# cntPlot1 <- ggplot(cnt_mlt1,
#                    aes(x = centrality,
#                        y = node,
#                        group = level,
#                        shape = level,
#                        color = level)) +
#   geom_point(aes(size = 1.5)) + 
#   geom_path(aes(size = 1)) +
#   guides(size = "none", color = "none") +
#   ylab("node") +
#   ggtitle("Measures of centrality") +
#   guides(shape = guide_legend(override.aes = list(size = 5, colour = c("#F8766D", "#00BA38", "#619CFF")))) +
#   my_pretty_theme +
#   theme(legend.position = c(.85, .9),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 14),
#         plot.title = element_text(hjust = 0, size = 18)
#   ) +
#   coord_flip()

cntPlot1 <- ggplot(cnt_mlt1,
                   aes(x = centrality,
                       y = node,
                       group = level,
                       color = level)) +
  geom_point(size = 2.5) + 
  geom_path(size = 1) +
  ylab("node") +
  ggtitle("Measures of centrality") +
  my_pretty_theme +
  theme(legend.position = c(.85, .9),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        plot.title = element_text(hjust = 0, size = 18)
  ) +
  coord_flip()


# Factors centrality ------------------------------------------------------

# compute centrality estimates
str2 <- centrality_auto(Network_items)$node.centrality[,"Strength"] %>%
  scale() |> 
  as.vector()
names(str2) <- colnames(Network_items)

cls2 <- centrality_auto(Network_items)$node.centrality[,"Closeness"] %>%
  scale() |>
  as.vector()
names(cls2) <- colnames(Network_items)

bet2 <- centrality_auto(Network_items)$node.centrality[,"Betweenness"] %>%
  scale() |>
  as.vector()
names(bet2) <- colnames(Network_items)

# merge centrality estimates in a dataframe
cnt2 <- merge(str2[1:17], cls2[1:17], by = 0, all = TRUE) %>%
  rename(node = Row.names, Strength = x, Closeness = y) %>%
  merge(bet2[1:17], by.x = "node", by.y = 0, all = TRUE) %>%
  rename(Betweenness = y)

cnt2$node <- factor(cnt2$node, levels = 1:17,
                    labels = c("ISI", paste0("Q", 1:16)))

cnt_mlt2 <- melt(cnt2, id.vars = "node",
                 variable.name = "level",
                 value.name = "centrality")
cnt_mlt2 <- arrange(cnt_mlt2, level, node)

# generate the plot using package ggplot2
# cntPlot2 <- ggplot(cnt_mlt2,
#                    aes(x = centrality,
#                        y = node,
#                        group = level,
#                        shape = level,
#                        color = level)) +
#   geom_point(aes(size = 1.5)) + 
#   geom_path(aes(size = 1)) +
#   guides(size = "none", color = "none") +
#   ylab("node") +
#   ggtitle("Measures of centrality") +
#   guides(shape = guide_legend(override.aes = list(size = 5, colour = c("#F8766D", "#00BA38", "#619CFF")))) +
#   my_pretty_theme +
#   theme(legend.position = c(.85, .9),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 14),
#         plot.title = element_text(hjust = 0, size = 18)
#   ) +
#   coord_flip()


cntPlot2 <- ggplot(cnt_mlt2,
       aes(x = centrality,
           y = node,
           group = level,
           color = level)) +
  geom_point(size = 2.5) + 
  geom_path(size = 1) +
  ylab("node") +
  ggtitle("Measures of centrality") +
  my_pretty_theme +
  theme(legend.position = c(.85, .9),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        plot.title = element_text(hjust = 0, size = 18)
  ) +
  coord_flip()
