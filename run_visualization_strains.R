globalspeed <- tracks[Condition == "NON", mean(unlist(Speed))]

plot_data_marginals <- tracks[ n > 60,
                    .(Shape = sapply(DP_simplify, sum),
                      Speed = sapply(Speed, function(x) (mean(log(x)))),
                      Control = Condition %in% "NON",
                      
                      n = .N
                    ),
                    by = .(Line, Condition)
                    ][,
                      nMin := min(n),
                      by = Line
                      
                    ][,
                      .SD[sample(.N,nMin)],
                      
                      by = .(Line, Condition)
                    ][,
                      `:=`(n = .N,
                        Speed_density = max(density(Speed)$y),
                        Shape_density = max(density(Shape/25)$y)
                      )
                      ,
                      by = .(Line, Condition)
                    ][,
                      `:=`(
                        Shape = Shape/25,
                        Max_Speed = log(20),
                        rnd = sample(.N, .N)
                      )
                    ][,
                        `:=`(
                          rank = .SD[Condition == "IND", mean(unlist(Speed))]- globalspeed
                        ),
                        by = Line
                    ][,
                      `:=`(Speed_density = max(Speed_density),
                           Shape_density = max(Shape_density)
                           )
                      ]

setorder(plot_data_marginals,rank,-rnd) #rank - have the pages in the plot ordered by amount of difference

plot_marginals(plot_data_marginals, "marg_mutants3.pdf")

print_tracks_by_quantile(tracks[ n > 200 & Experiment %in% c("2019_12_02")])





tracks[,
       `:=`( Shape = sapply(DP_simplify, function(x){sum(x)/25}),
             Mean_Speed = sapply(Speed, function(x){mean(x)})
             )
       ]
tracks[,
  `:=`(bin_shape=findInterval(Shape, 
                              quantile(Shape,probs = seq(0, 1, 1/10),type=7),
                              rightmost.closed=TRUE)
  )
  ][,
    `:=`(bin_speed=findInterval(Mean_Speed, 
                                quantile(Mean_Speed,probs = seq(0, 1, 1/6),
                                         type=7),
                                rightmost.closed=TRUE)
    ),
    by = bin_shape
    ]











names(tracks)
tracks$bin_shape
q_s <- quantile(tracks$Shape,probs = seq(0, 1, 1/10),type=7)
tracks[,quantile(Mean_Speed,probs = seq(0, 1, 1/6),type=7),by  = bin_shape]



tracks[,.(q_s = quantile(Shape,probs = seq(0, 1, 1/10),type=7))][]

