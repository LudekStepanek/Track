plot_data <- tracks[ n > 60 & Experiment %in% c("2019_12_02", "2019_12_05"),
                    .(Shape = sapply(DP_simplify, sum),
                      Speed = sapply(Speed, mean),
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
                      ]

setorder(plot_data,-rnd)

plot_marginals(plot_data, "marg_strains.pdf")


plot_data[,
          .SD[, .(Shape, Speed, Condition)],
          by = Line
          ]

