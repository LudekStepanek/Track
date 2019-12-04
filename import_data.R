## Reads TrackMate .XML files with trajectories and MicroManager .TXT metadata files with timepoints



#----------read the timepoints from one .TXT file --------------
read_timepoints_file <- function(File = "C:\\Users\\Ludek\\Documents\\R\\Traject\\data\\TrackMateXML_output\\strain1_01.txt ") {

  # read the file ........................filter rows with time.......keep only numeric part of that row.........
  fread(File, header = FALSE, sep = "\n")[V1 %like% "ElapsedTime-ms" & !(V1 %like% "Zyla"), .(as.numeric(gsub(".*?([0-9]+).*", "\\1", V1))) ]
}


#----------combine the timepoints from all .TXT files into one data.table --------------
read_timepoints_folder <- function(Folder = "C:\\Users\\Ludek\\Documents\\R\\Traject\\data\\TrackMate_output\\") {
  Sys.glob(paste(Folder, sep = "", "*.txt")) %>% # generate vector of .TXT files in specified directory
    sapply(read_timepoints_file, simplify = FALSE, USE.NAMES = TRUE) %>% # process the sequence of files, returns timepoint vectors as named list
    {
      rbindlist(., use.names = TRUE, idcol = "File")[, File := basename(tools::file_path_sans_ext(File))]
    } # bind into data.table with id column containing the filenames # trim extensions
}


#----------read the x, y, t from one .XML file --------------

 readXML3 <- function (XML_file = "C:\\Users\\Ludek\\Documents\\R\\data\\tracking_old\\strain1_01.xml") {
   
   xList <- xmlToList(xmlParse(XML_file), addAttributes = FALSE)
   
   track_list <- lapply(xList, function(el_xList) {
     coords <- do.call(rbind, el_xList)[,1:3]
     class(coords) <- "numeric"
     coords[,1] <- coords[,1]+1
     as.data.table(coords)
   } )
   
  rbindlist(track_list, idcol = "Track")
   
 }  

#----------combine the x,y,t data.tables from all .XML files into one data.table --------------
read_TrackMate_folder <- function(Folder = "C:\\Users\\Ludek\\Documents\\R\\Traject\\data\\TrackMate_output\\") {
  Sys.glob(paste(Folder, sep = "", "*.xml")) %>% # generate vector of .XML files in specified directory
    sapply(readXML3, simplify = FALSE, USE.NAMES = TRUE) %>% # process the sequence of files, returns timepoint vectors as named list
    {
      rbindlist(., use.names = TRUE, idcol = "File")[, File := basename(tools::file_path_sans_ext(File))]
    } # trim extensions
}


#----------combine timepoints from MicroManager metadata with the Trackmate coords --------------
read_input_data <- function(path) {
  data_raw <- read_TrackMate_folder(path)
  timepoints <- read_timepoints_folder(path)
  data_raw[, t := timepoints[File == .BY, V1][t], by = "File"]      #assign MicroManager elapsed time to corresponding timepoints
  data_raw[, Track := .GRP, by = .(File, Track)]
  na.omit(data_raw)
}
 
#--------- main import function; optionally loads/saves the data and formats them into the nested data.table----------          
import_tracks <- function(path, Load = FALSE) {
  
  if (Load) {         
    started.at <- proc.time()
    data_imported <- readRDS(paste0(path, "data_imported.sav"))
    cat("Data loaded", timetaken(started.at), "\n")
  }
  
  else {
    started.at <- proc.time()
    data_imported <- read_input_data(path)
    cat("Data imported", timetaken(started.at), "\n")
    
    started.at <- proc.time()
    saveRDS(data_imported, paste0(path, "data_imported.sav"))
    cat("Data saved", timetaken(started.at), "\n")
  }
  
  started.at <- proc.time()
  
  tracks <-  data_imported[, .(t = t / 1000,
                               x = x,
                               y = y, 
                               n = .N, 
                               File = data.table::first(File)
                               
                               ) ,
                             by = Track
                           ]
  
  cat("Data processed", timetaken(started.at), "\n")
  
  return(tracks)
} 
