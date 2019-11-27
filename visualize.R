#----tracks in bins plot -----------
print_tracks_by_quantile <- function(tracks, quant = 0.01){


list_of_plots <- tracks[n>50,
               .(x=x,
                 y=y,
                 DP_simplify=DP_simplify,
                 Track = .I,
                 Shape = lapply(DP_simplify, function(x){sum(x)})
                
               )
               ][,
                 `:=`(bin_shape=findInterval(unlist(Shape), 
                                             quantile(unlist(Shape),probs = seq(0, 1, quant),
                                                      type=7),
                                             rightmost.closed=TRUE)
                 )
                 ][,
                    .(p = list(ggplot(.SD[,
                                          .(x = unlist(x),
                                            y = unlist(y)
                                          ),
                                          by = .(Track)],
                                      aes(x ,y ,group = Track) )+
                                 geom_path()+
                                 coord_fixed()
                               
                    ),
                    DP = list(ggplot(.SD[,
                                         .(x = rep(1:24),
                                           y = unlist(DP_simplify)
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
plot_marginals <- function(){

list_of_plots <- tracks_old[ sapply(t,`[[`,1)<5,
                         .(sapply(DP_simplify, sum),
                           mean(unlist(Speed))
                           ),
                         
                         by = .(Track, File = File)
                         ][,
                           .(plot = list(
                             marginal_histogram(.SD, titulek = "e", Group = File) )
                             )
                           ]


#---------save plots to one file----------
ggsave(paste0(plot_folder,"marginals1.pdf"),
       marrangeGrob(grobs = list_of_plots$plot, nrow = 1, ncol = 1),
       width = 20, height = 20, units = "cm"
)
}

