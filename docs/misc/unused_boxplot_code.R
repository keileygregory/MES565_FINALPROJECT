{r, echo=FALSE}
# Explicitly define order of boxes in each plot (detections on left, background on right)
hbk_long$boxorder <- factor("Detection", levels = c("Detection", "Background"))
background_long$boxorder <- factor("Background", levels = c("Detection", "Background"))

# Facted box plot
boxplot_COMBINED <- ggplot() +
  # DETECTION box (colors by variable, keep outliers)
  geom_boxplot(data = hbk_long, aes(x = boxorder, y = Value, fill = Oceanographic_Variable, color = Oceanographic_Variable),
               width = 0.9,
               alpha = 0.45,
               outlier.alpha = 0.7,
               outlier.size = 1.5,
               position = position_nudge(x = -0.01)  # shift box slightly left
  ) +
  # BACKGROUND box (all grey, remove outliers)
  geom_boxplot(data = background_long, aes(x = boxorder, y = Value),
               fill = "grey80",
               color = "grey40",
               width = 0.9,
               alpha = 0.45,
               outlier.alpha = 0.7,
               outlier.size = 1.5,
               position = position_nudge(x = 0.01)  # shift box slightly right
  ) + 
  facet_wrap(
    ~ Oceanographic_Variable,
    scales = "free_y",
    ncol = 3,
    labeller = labeller(Oceanographic_Variable = function(x)
      stringr::str_wrap(clean_var_names[x], width = 18))
  ) + 
  scale_fill_manual(values = custom_colors, guide = "none") +
  scale_color_manual(values = custom_colors, guide = "none") +
  labs(
    x = NULL,
    y = "Value"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    text = element_text(color = "black"),
    strip.text = element_text(color = "black", face = "bold", size = 11),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    axis.title.y = element_text(face = "bold", size = 10, margin = margin(r = 5)),  # bold y-axis title; r = 5 means y axis label 5 pts away from axis value labels
    axis.text.x = element_text(face = "plain"),  # unbold x-axis labels
    axis.ticks.x = element_blank(),  # remove the tick marks on the x-axis
  )
boxplot_COMBINED