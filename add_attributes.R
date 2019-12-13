
add_speed <- function(DT, time_step){
  started.at <- proc.time()
  DT[,
     `:=`(
       x_res = mapply( function(x,t){
         stats::approx(t, x, seq(t[1], t[length(t)],time_step))$y},
         x,t ),
       y_res = mapply( function(x,t){
         stats::approx(t, x, seq(t[1], t[length(t)],time_step))$y},
         y,t ),
       t_res = lapply(t, function(t) seq(t[1], t[length(t)],time_step))
     )
     ][,
     `:=`(Dists = mapply(distances, x_res, y_res),
          Tdiff = lapply(t_res, diff))
     ][,
       `:=`(Speed = mapply(`/`, Dists, Tdiff))
       ]
  saveRDS(DT, paste0(data_folder, "data_speed.sav"))
  cat("Data 1 processed and saved", timetaken(started.at), "\n")
  return(DT)
}

add_DP_simplify <- function(DT, epsilon_max = 100){
  started.at <- proc.time()
  DT[,
     `:=`(DP_simplify = mapply(douglas_peucker_simplification, x, y, epsilon_max, SIMPLIFY = FALSE))
     ]
  saveRDS(DT, paste0(data_folder, "data_DP.sav"))
  cat("Data 2 processed and saved", timetaken(started.at), "\n")
  return(DT)
}

