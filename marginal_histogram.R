#----------display scatter plot + marginal histograms for subset of data provided by data.table----------
#----Expected is a data frame (data.table) with columns V1, V2, Group, and a string titulek


marginal_histogram <- function(subset_data, titulek, Group) {
  
  theme_m_empty <- theme(
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    legend.position = "none"
  )

  theme_m_top <- theme(
    axis.ticks.y = element_blank(),
    #axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    #axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    legend.position = "none"
  )

  theme_m_right <- theme(
    axis.ticks.x  = element_blank(),
    axis.text.x = element_blank(),
    #axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    #axis.title.y = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    legend.position = "none"
  )

  theme_m_main <- theme(
    axis.ticks = element_blank(),
    # axis.ticks.length= unit(  -5, "pt"),
    # axis.text.x =        element_text(margin = margin(t = 0.8 *  2), vjust = 1),
    # axis.text.x.top =    element_text(margin = margin(b = 0.8 *  2), vjust = 0),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    legend.position = "none"
  )

  hist_top <- ggplot(subset_data) +
    geom_density(aes(V1, fill = Group, colour = Group), alpha = 0.2) +
    scale_x_continuous(position = "top") + #, limits = c(0,13))+
    theme_m_top


  hist_right <- ggplot(subset_data) +
    geom_density(aes(V2, fill = Group, colour = Group), alpha = 0.2) +
    scale_x_continuous(position = "top")+ #, limits = c(0,150))+
    coord_flip() +
    theme_m_right

  scatter <- ggplot(subset_data) +
    geom_point(aes(V1, V2, colour = Group), alpha = 0.2) +
    # scale_x_continuous(position = "top") +
    # scale_y_continuous(position = "right") +
   # xlim(c(0, 13)) +
   # ylim(c(0, 150)) +
    theme_m_main

  empty <- ggplot(subset_data) +
    # geom_point(aes(x = 0.1, y = 4 ),shape = 22, colour = "blue", fill = "lightblue", size = 5, alpha = 0.5)+
    # geom_point(aes(x = 0.1, y = 3),shape = 22, colour = "red",fill = "indianred1", size = 5, alpha = 0.5)+
    # geom_point(aes(x = 0.1, y = 2), shape = 0, colour = "black", size = 5, alpha = 0.5)+
    #  annotate("text", x = 0.4, y = 4, label = "NON", hjust = 0)+
    # 
    #  annotate("text", x = 0.4, y = 3, label = "IND72h",  hjust = 0)+
    #  annotate("text", x = 0.4, y = 2, label = "All NON together", hjust = 0)+
    # labs(title = titulek) +
    # ylim(c(0,5))+
    # xlim(c(0,5))+
    theme_m_empty

  list_of_marginals <- list(hist_top, empty, scatter, hist_right)

  g1 <- grid.arrange(grobs = list_of_marginals, ncol = 2, nrow = 2, widths = c(4, 1.5), heights = c(1.5, 4)) # need to use 'grobs =' to have a list as an argument
}



