#----------display scatter plot + marginal histograms for subset of data provided by data.table----------
#----Expected is a data frame (data.table) with columns V1, V2, Group, and a string titulek


marginal_histogram <- function(data, unbiased_control, info) {
  
  theme_m_empty <- theme(
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #plot.margin = margin(0, 0, 0, 0),
    legend.position = "none"
  )

  theme_m_top <- theme(
    axis.ticks.y = element_blank(),
    #axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    #axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    legend.position = "none",
    panel.background =   element_rect(fill = NA, colour = NA),
    panel.border =       element_blank(),
    panel.grid =         element_line(colour = "grey70", size = 0.01), #!grey
    panel.grid.minor =    element_blank()
  )

  theme_m_right <- theme(
   axis.ticks.x  = element_blank(),
   axis.text.x = element_blank(),
    #axis.text.y = element_blank(),
   axis.title.x = element_blank(),
    #axis.title.y = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    legend.position = "none",
   panel.background =   element_rect(fill = NA, colour = NA),
   panel.border =       element_blank(),
   panel.grid =         element_line(colour = "grey70", size = 0.01),
   panel.grid.minor =    element_blank()
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
    legend.position = "none",
    panel.background =   element_rect(fill = NA, colour = NA),
    panel.border =       element_blank(),
    panel.grid =         element_line(colour = "grey99", size = 0.01), #grey!
    panel.grid.minor =    element_blank(),
  )

  hist_top <- ggplot(data) +
    geom_line(stat="density", aes_string(colnames(data)[1], 
                            #fill = colnames(data)[3],
                            colour = colnames(data)[3]), 
                 alpha = 1, 
                 size = 1.5,) +
    geom_line(stat="density",data = unbiased_control, aes(Shape), size = 1.5, colour = "grey50") +
    scale_x_continuous(position = "top", limits = c(0,1)) + #, limits = c(0,13))+
    scale_y_continuous(limits = c(0,5))+
    scale_colour_brewer(palette = "Set1")+
    theme_m_top


  hist_right <- ggplot(data) +
    geom_line(stat="density", aes_string(colnames(data)[2],
                            #fill = colnames(data)[3],
                            colour = colnames(data)[3]),
                 alpha = 1, 
                 size = 1.5) +
    geom_line(stat="density",data = unbiased_control, aes(Speed), size = 1.5, colour = "grey50") +
    scale_x_continuous(position = "top", limits = c(0,data.table::first(info$Max_Speed)))+
    scale_y_continuous(limits = c(0,0.5))+
    coord_flip() +
    scale_colour_brewer(palette = "Set1")+
    theme_m_right

  
  scatter <- ggplot(data, aes_string(colnames(data)[1], 
                          colnames(data)[2], 
                          fill = colnames(data)[3],
                          colour = colnames(data)[3])) +
    geom_point(alpha = 0.6, size = 0.9, shape = 20) +
    #stat_density_2d()+
    # scale_x_continuous(position = "top") +
    # scale_y_continuous(position = "right") +
    xlim(c(0, 1)) +
    ylim(c(0, data.table::first(info$Max_Speed) )) +
    scale_colour_brewer(palette = "Set1")+
    geom_vline(xintercept = quantile(tracks$Shape,probs = seq(0, 1, 1/10),type=7))+
    geom_hline(yintercept = quantile(tracks$Mean_Speed,probs = seq(0, 1, 1/6),type=7))+
    theme_m_main

  empty <- ggplot(info) +
     geom_point(aes(x = 0, 
                    y = info$Y,
                    colour = info$Group,
                    fill = info$Group),
                shape = 22,
                size = 5,
                alpha = 1) +
    annotate("text", x = 0.5, y =info$Y, label = paste0(info$Group," n = ", info$n), hjust = 0)+
    geom_point(aes(x = 5, y = max(info$Y)+1),shape = 22, colour = "white",fill = "white", size = 5, alpha = 0)+
    #annotate("text", x = 0.5, y = min(info$Y)-1, label = paste("n/Group = ",info$n),  hjust = 0)+
    geom_point(aes(x = 5, y = min(info$Y)-2),shape = 22, colour = "white",fill = "white", size = 5, alpha = 0)+
    
    
    # 
    #  annotate("text", x = 0.4, y = 3, label = "IND72h",  hjust = 0)+
    #  annotate("text", x = 0.4, y = 2, label = "All NON together", hjust = 0)+
     labs(title = info$titulek) +
    # ylim(c(0,5))+
    # xlim(c(0,5))+
    scale_colour_brewer(palette = "Set1")+
    scale_fill_brewer(palette = "Set1")+
    theme_m_empty

  list_of_marginals <- list(hist_top, empty, scatter, hist_right)

  g1 <- grid.arrange(grobs = list_of_marginals,
                     ncol = 2, 
                     nrow = 2, 
                     widths = c(4, 1.5),
                     heights = c(1.5, 4)) # need to use 'grobs =' to have a list as an argument
}



