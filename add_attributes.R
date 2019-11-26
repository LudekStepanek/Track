add_1 <- function(DT){
  started.at <- proc.time()
  DT[,
     `:=`(Dists = mapply(distances, x, y),
          Tdiff = lapply(t, diff))
     ][,
       `:=`( Speed = mapply(`/`, Dists, Tdiff),
            Angles = mapply(angles, x, y))
       ]
  saveRDS(DT, paste0(data_folder, "data_add1.sav"))
  cat("Data 1 processed and saved", timetaken(started.at), "\n")
  return(DT)
}

add_DP_simplify <- function(DT, epsilon_max = 100){
  started.at <- proc.time()
  DT[,
     `:=`(DP_simplify = mapply(douglas_peucker_simplification, x, y, epsilon_max, SIMPLIFY = FALSE))
     ]
  saveRDS(DT, paste0(data_folder, "data_add2.sav"))
  cat("Data 2 processed and saved", timetaken(started.at), "\n")
  return(DT)
}
