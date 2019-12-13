#----tracks in bins plot -----------
print_tracks_by_quantile <- function(tracks, quant = 0.1){


list_of_plots <- tracks[n>60,
               .(x=x,
                 y=y,
                 #DP_simplify=DP_simplify,
                 Track = .I,
                 Shape = sapply(DP_simplify, function(x){sum(x)/25}),
                 Speed = sapply(Speed, function(x){mean(x)}),
                 Condition = Condition,
                 Line = Line,
                 bin_shape = bin_shape,
                 bin_speed = bin_speed
                
               )
               ][,
                    .(p = list(ggplot(.SD[,
                                          .(x = unlist(x),
                                            y = unlist(y),
                                            Condition = Condition,
                                            bin_speed = bin_speed
                                            
                                          ),
                                          by = .(Track)],
                                      aes(x ,y ,colour = Condition, group = Track) )+
                                 geom_path()+
                                 labs(title = Line)+
                                 coord_fixed()+
                                 facet_wrap(~bin_speed)
                    )
                               
                    # ,
                    # DP = list(ggplot(.SD[,
                    #                      .(x = rep(1:25),
                    #                        y = unlist(DP_simplify)
                    #                      ),
                    #                      by = .(Track)],
                    #                  aes(x ,y ,group = Track) ) +
                    #             geom_path(alpha = 0.4) +
                    #             ylim(c(0,1))
                    #           
                    # )
                    ),
                    keyby = .(Line,bin_shape)
                    ]
  
  ggsave(paste0(plot_folder,"_DP_scan1.pdf"),
         marrangeGrob(grobs = list_of_plots$p, nrow = 1, ncol = 1),
         width = 60, height = 40, units = "cm"
  )

  # ggsave(paste0(plot_folder,"DP_scan_DP1.pdf"),
  #        marrangeGrob(grobs = list_of_plots$DP, nrow = 1, ncol = 1),
  #        width = 20, height = 20, units = "cm"
  # )
}








#---------plot marginal histograms by Line and Induced/NON-----------
plot_marginals <- function(plot_data, output_file = "marginals.pdf"){
  

list_of_plots <- plot_data[,
                           .(plot = list( marginal_histogram(data = .SD[, .(Shape, Speed, Condition)],
                                                             
                                                             unbiased_control = plot_data[!Line %in% .BY[[1]] & Control == TRUE,
                                                                                       .(Shape, Speed)],
                                                             
                                                             info = .SD[, .(Y = .GRP,
                                                                     titulek = Line, 
                                                                     n = first(n),
                                                                     Max_Speed = first(Max_Speed)
                                                                     ),
                                                                 by = .(Group = Condition)] 
                                                             )
                                          )
                             ),
                           by = Line
                           ]


#---------save plots to one file----------
ggsave(paste0(plot_folder,output_file),
       marrangeGrob(grobs = list_of_plots$plot, nrow = 1, ncol = 1),
       width = 20, height = 20, units = "cm"
      )
}

#-------plot track + point  + resampled points


plot_track <- function(data, track){
  data[track,
        .(plot = list(ggplot(data = .SD[, .(x = unlist(x), y = unlist(y), by = Track)],
                             aes(x,y, group = Track))+
                        geom_path()+
                        geom_point()+
                        geom_point(data = .SD[, .(x = unlist(x_res), y = unlist(y_res), by = Track)],
                                   aes(x,y, group = Track), colour = "red")
        )
        )
       ]
  
}

#------------giant_map-----------
#----tracks in bins plot -----------
print_big_map <- function(tracks, quant = 0.1){
  
  
  list_of_plots <- tracks[n>60,
                          .(
                            x=mapply(function(a,b) (a-a[1])+b*50000, x, Shape),
                            y=mapply(function(a,b) (a-a[1])+b*50000, y, Mean_Speed/max(Mean_Speed)),
                            Condition = Condition,
                            Track = .I,
                            Line = Line
                           
                          )
                          ][,
                            .(p = list(ggplot(.SD[,
                                                  .(x = unlist(x),
                                                    y = unlist(y),
                                                    Condition = Condition  
                                                  ),
                                                  by = .(Track)],
                                              aes(x ,y ,colour = Condition, group = Track) )+
                                         geom_path()+
                                         labs(title = Line)+
                                         coord_fixed()+
                                         theme_void()
                            )
                            
                           
                            ),
                            keyby = Line
                            ]
  
  ggsave(paste0(plot_folder,"big.pdf"),
         marrangeGrob(grobs = list_of_plots$p, nrow = 1, ncol = 1),
         width = 500, height = 500, units = "cm", limitsize = FALSE
  )
  
  # ggsave(paste0(plot_folder,"DP_scan_DP1.pdf"),
  #        marrangeGrob(grobs = list_of_plots$DP, nrow = 1, ncol = 1),
  #        width = 20, height = 20, units = "cm"
  # )
}
