
rename_proteins <- function(tracks, data_folder){

protein_IDs <- fread(paste0(data_folder,"Proteins_strains_table.csv") )
protein_IDs[,V3:=paste0("strain",V3)]
tracks[,Strain:=gsub("_.*","",File) ]
tracks[, `:=`(Protein = protein_IDs[V3 == .BY, V1],
              Induced = protein_IDs[V3 == .BY, V2]) , by = Strain] 

}


