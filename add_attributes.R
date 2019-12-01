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

# Interpolate x and y separately. This works by treating x (or y) as a
# function of time (so x axis is time, y axis is trajectory x or y), then
# interpolating to required times. approx interpolates y for given values of x

resample_time <- function(data, time_step){
  data[,
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
                   `:=`(Dists_res = mapply(distances, x_res, y_res),
                     Tdiff_res = lapply(t_res, diff))
                   ][,
                     Speed_res:= mapply(`/`, Dists_res, Tdiff_res)
                  
                 ]
  
}

resample_time_ <- function(data, time_step){
 data[,
                 `:=`(
                   x = mapply( function(x,t){
                     stats::approx(t, x, seq(t[1], t[length(t)],time_step))$y},
                     x,t ),
                   y = mapply( function(x,t){
                     stats::approx(t, x, seq(t[1], t[length(t)],time_step))$y},
                     y,t ),
                   t = lapply(t, function(t) seq(t[1], t[length(t)],time_step))
                 )
                 ]
  
}