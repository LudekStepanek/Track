speedvecs <- numeric(10)

speedveco <- numeric(10)
for (i in 1:10){
  print(i)
 resample_time(tracks_starved, (i/5) ) 
  
speedvecs[i] <- mean(  

  tracks_starved[n>60 & File%ilike%"loaded",
     sapply(Speed_res,mean)
       ]
  )



resample_time(tracks_old, (i/5) ) 

speedveco[i] <- mean(  
  
  tracks_old[n>60& File%ilike%"strain1_",
                 sapply(Speed_res,mean)
                 ]
)
}

plot(speedvecs, type = "b", ylim = c(0,10))
lines(speedveco)

plot_track(tracks_starved, 13)[[1]]


resample_time(tracks_starved, 2 ) 
resample_time(tracks_old, 2 ) 

ts1 <- copy(tracks_starved)
to1 <- copy(tracks_old)

resample_time_(ts1, 2 ) 
resample_time_(to1, 2 ) 

add_1(ts1)
add_1(to1)
atg <- rbind(tracks_old,tracks_starved)
plot_marginals(atg)
