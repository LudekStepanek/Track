add_speed <- function(tracks, time_step){
  started.at <- proc.time()
  DT_speed <- tracks[,
                 .(
                   x_res = stats::approx(t, x, seq(t[1], t[length(t)],time_step))$y,
                   
                   y_res = stats::approx(t, x, seq(t[1], t[length(t)],time_step))$y,
                   
                   t_res = seq(t[1], t[length(t)],time_step)
                 ),
                 by = Track
                 ][,
     {
     Dists = distances(x_res, y_res);
     Tdiff = diff(t_res);
     list(Speed = Dists/Tdiff)
     },
       by = Track
       ]
  saveRDS(tracks, paste0(data_folder, "data_speed.sav"))
  cat("Data 1 processed and saved", timetaken(started.at), "\n")
  return(DT_speed)
}



add_DP_simplify <- function(DT, epsilon_max = 100){
  started.at <- proc.time()
  DT_dps <- DT[,
     .(DP_simplify = douglas_peucker_simplification( x, y, epsilon_max)),
     by = Track
     ]
  saveRDS(DT, paste0(data_folder, "data_dps.sav"))
  cat("Data 2 processed and saved", timetaken(started.at), "\n")
  return(DT_dps)
}

# Interpolate x and y separately. This works by treating x (or y) as a
# function of time (so x axis is time, y axis is trajectory x or y), then
# interpolating to required times. approx interpolates y for given values of x

resample_time <- function(tracks, time_step){
  resampled_tracks <- tracks[,
                 .(
                   x_res = stats::approx(t, x, seq(t[1], t[length(t)],time_step))$y,
                  
                   y_res = stats::approx(t, x, seq(t[1], t[length(t)],time_step))$y,
                   
                   t_res = seq(t[1], t[length(t)],time_step)
                 ),
                 by = .(Track, File)
                 ]
  
}
