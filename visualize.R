#----tracks in bins plot -----------
print_tracks_by_quantile <- function(tracks, quant = 0.01){


list_of_plots <- tracks[n>50,
               .(x=x,
                 y=y,
                 Shape = dps[Track == .BY, sum(DP_simplify)]
               ),
               by = Track
               ][,
                 `:=`(bin_shape=findInterval(Shape, 
                                             quantile(Shape, probs = seq(0, 1, quant),
                                                      type=7),
                                             rightmost.closed=TRUE)
                 )
                 ][,
                    .(p = list(ggplot(.SD,
                                      aes(x ,y ,group = Track)) +
                                 geom_path() +
                                 coord_fixed()
                               
                    ),
                    DP = list(ggplot(.SD[,
                                         .(x = rep(1:25),
                                           y = dps[Track == .BY, DP_simplify]
                                         ),
                                         by = .(Track)],
                                     aes(x ,y ,group = Track) )+
                                geom_path(alpha = 0.4)
                              
                    )
                    ),
                    keyby = bin_shape
                    ]
  
  ggsave(paste0(plot_folder,"_DP_scan1.pdf"),
         marrangeGrob(grobs = list_of_plots$p, nrow = 1, ncol = 1),
         width = 20, height = 20, units = "cm"
          )

  ggsave(paste0(plot_folder,"DP_scan_DP1.pdf"),
         marrangeGrob(grobs = list_of_plots$DP, nrow = 1, ncol = 1),
         width = 20, height = 20, units = "cm"
         )
}

#---------plot marginal histograms by starved/fresh-----------
plot_marginals <- function(tracks){
  
list_of_plots <- tracks[,
                         .(
                           Shape = dps[Track == .BY, sum(DP_simplify)],
                           Speed = spt[Track == .BY, mean(Speed)],
                           Induced = Induced,
                           Line = Line
                           ),
                         by = Track 
                         ][,
                           `:=`(
                           Shape = Shape/max(Shape)
                           )
                         ][,
                           .(plot = list( marginal_histogram(.SD[, .(Shape, Speed, Induced)],
                                                             .SD[, .(Y = .GRP, titulek = Line, n="x"), by = .(Group = Induced)] 
                                                             )
                                          )
                             ),
                           by = Line
                           ]


#---------save plots to one file----------
ggsave(paste0(plot_folder,"marginals1.pdf"),
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

