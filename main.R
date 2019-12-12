# tracking main

plot_folder <- ("C:\\Users\\Ludek\\Documents\\R\\plots\\tracked\\")

data_folder <- "C:\\Users\\Ludek\\Documents\\R\\data\\tracking_starved\\"
data_folder <- "C:\\Users\\Ludek\\Documents\\R\\data\\ts_sample\\"
data_folder <- "C:\\Users\\Ludek\\Documents\\R\\data\\tracking_old\\"
data_folder <- "C:\\Users\\Ludek\\Documents\\R\\data\\sample04\\"


data_folder <- "K:\\2019_11_07_smox\\"
data_folder <- "K:\\TrackingExperimentProcessed\\"
data_folder <- "K:\\project_starved\\"
data_folder <- "K:\\tracked\\"

setwd("C:\\Users\\Ludek\\Documents\\R\\Track\\")

source("libraries_sources.R")

tracks01 <- import_tracks(data_folder, Load = T)
data_folder12 <- "K:\\tracked12\\"
tracks12 <- import_tracks(data_folder12, Load = T)
tracks <- rbind(tracks01, tracks12)
split_name(tracks)
head(tracks)
add_speed(tracks,2)

add_DP_simplify(tracks, 25)



tracks_old <- readRDS(paste0(data_folder,"data_add2.sav"))
setDT(tracks_old)

tracks_starved <- tracks

tracks_starved <- readRDS(paste0(data_folder,"data_add2.sav"))
setDT(tracks_starved)

add_DP_simplify(tracks, 25)
print_tracks_by_quantile(tracks_starved)

tracks <- rbind(tracks_old, tracks_starved)

tracks[,
       `:=`(
         Experiment = substr(File, start = 1, stop = 10)
         )
       ]

#----------read processed data ------------
tracks <- readRDS("C:\\Users\\Ludek\\Documents\\R\\data\\data_DP.sav")
setDT(tracks)


#-------read processed bind to one frame----------
data_folder <- "C:\\Users\\Ludek\\Documents\\R\\data\\tracking_old\\"
tracks_old <- readRDS(paste0(data_folder,"data_add2.sav"))
setDT(tracks_old)

data_folder <- "C:\\Users\\Ludek\\Documents\\R\\data\\tracking_starved\\"
tracks_starved <- readRDS(paste0(data_folder,"data_add2.sav"))
setDT(tracks_starved)


tracks <- rbind(tracks_old, tracks_starved)
