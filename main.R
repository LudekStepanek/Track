# tracking main

plot_folder <- ("C:\\Users\\Ludek\\Documents\\R\\plots\\tracked\\")

data_folder <- "C:\\Users\\Ludek\\Documents\\R\\data\\tracking_starved\\"
data_folder <- "C:\\Users\\Ludek\\Documents\\R\\data\\ts_sample\\"
data_folder <- "C:\\Users\\Ludek\\Documents\\R\\data\\tracking_old\\"
data_folder <- "C:\\Users\\Ludek\\Documents\\R\\data\\sample04\\"
data_folder <- "C:\\Users\\Ludek\\Documents\\R\\data\\"


data_folder <- "K:\\2019_11_07_smox\\"
data_folder <- "K:\\TrackingExperimentProcessed\\"
data_folder <- "K:\\project_starved\\"
data_folder <- "K:\\tracked\\"

setwd("C:\\Users\\Ludek\\Documents\\R\\Track\\")

source("libraries_sources.R")

#----importing files all at once filled memory, therefore the split abd rbind
tracks01 <- import_tracks(data_folder, Load = T)
data_folder12 <- "K:\\tracked12\\"
tracks12 <- import_tracks(data_folder12, Load = T)
tracks <- rbind(tracks01, tracks12)



split_name(tracks)
head(tracks)
add_speed(tracks,2)

add_DP_simplify(tracks, 25)



tracks <- rbind(tracks_old, tracks_starved)

tracks[,
       `:=`(
         Experiment = substr(File, start = 1, stop = 10)
         )
       ]

#----------read processed data ------------
tracks <- readRDS(paste0(data_folder,"data_DP.sav"))
setDT(tracks)



