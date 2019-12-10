
rename_proteins <- function(tracks, data_folder){

protein_IDs <- fread(paste0(data_folder,"Proteins_strains_table.csv") )
protein_IDs[,V3:=paste0("strain",V3)]
tracks[,Strain:=gsub("_.*","",File) ]
tracks[, `:=`(Protein = protein_IDs[V3 == .BY, V1],
              Induced = protein_IDs[V3 == .BY, V2]) , by = Strain] 

}


split_name <- function(tracks) {
  tracks[,
         `:=`(
           Experiment = substr(File, start = 1, stop = 10),
           Line = unlist(strsplit(File, "_", fixed = TRUE))[4],
           Condition = unlist(strsplit(File, "_", fixed = TRUE))[5],
           Field = unlist(strsplit(File, "_", fixed = TRUE))[6]
         ),
         by = File
         ]
  
}

