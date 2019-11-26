#----tracks in bins plot -----------
DP_scan <- function(tracks, i_max){

for (i in seq(100,i_max,5)){
  tt <- tracks[n>50,
               .(x=x,
                 y=y,
                 DP_simplify=DP_simplify,
                 Track = .I,
                 Shape = lapply(DP_simplify,function(x){sum(x[1:i])})
               )
               ][,
                 `:=`(bin_shape=findInterval(unlist(Shape), 
                                             quantile(unlist(Shape),probs = seq(0, 1, 0.0025),
                                                      type=7),
                                             rightmost.closed=TRUE)
                 )][,
                    .(p = list(ggplot(.SD[,
                                          .(x = unlist(x),
                                            y = unlist(y)
                                          ),
                                          by = .(Track)],
                                      aes(x ,y ,group = Track) )+
                                 geom_path()+
                                 coord_fixed()
                               
                    ) ),
                    keyby = bin_shape
                    ]
  
  ggsave(paste0(plot_folder,"DP_scan",i,".pdf"),
         marrangeGrob(grobs = tt$p, nrow = 1, ncol = 1),
         width = 20, height = 20, units = "cm"
  )
}
}