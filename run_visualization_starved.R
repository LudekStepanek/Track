plot_data <- tracks[ n > 60 & Line == "SMOX",
                    .(Shape = sapply(DP_simplify, sum),
                      Speed = sapply(Speed, mean),
                      Control = FALSE, # Condition %in% "NON",
                      
                      n = .N
                    ),
                    by = .(Line, Condition = fifelse(Condition == "starved"," starved", "fresh"))
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

plot_marginals(plot_data, "marg_starved.pdf")


plot_data[,
          .SD[, .(Shape, Speed, Condition)],
          by = Line
          ]

