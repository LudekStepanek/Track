globalspeed <- tracks[Experiment%in%c("2019_12_02","2019_12_05") & Condition == "NON", mean(unlist(Speed))]

plot_data_marginals <- tracks[ n > 60 & Experiment %in% c("2019_12_02","2019_12_05"),
                    .(Shape = sapply(DP_simplify, sum),
                      Speed = sapply(Speed, function(x) (mean(x))),
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
                      n := .N
                      ,
                      by = .(Line, Condition)
                    ][,
                      `:=`(
                        Shape = Shape/25,
                        Max_Speed = 20,
                        rnd = sample(.N, .N)
                      )
                      ][,
                        `:=`(
                          rank = .SD[Condition == "IND",mean(unlist(Speed))]- globalspeed
                        ),
                        by = Line
                        ]

setorder(plot_data_marginals,rank,-rnd) #rank - have the pages in the plot ordered by amount of difference

plot_marginals(plot_data_marginals, "marg_new_strains.pdf")

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

