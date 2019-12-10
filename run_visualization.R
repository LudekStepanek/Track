plot_data <- tracks[ n > 60 & Experiment %in% c("2019_12_02"),
                    .(Shape = sapply(DP_simplify, sum),
                      Speed = sapply(Speed, mean),
                      Control = Condition %in% "NON",
                      
                      n = .N
                    ),
                    by = .(Line, Condition)
                    ][,
                      `:=`(
                        Shape = Shape/25,
                        Max_Speed = 20
                      )
                      ]

plot_marginals(plot_data, "marg1_new.pdf")
