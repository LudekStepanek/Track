

protein_IDs <- fread("C:\\Users\\ludek\\Documents\\R\\Data\\tracking_old\\Proteins_strains_table.csv")

protein_IDs[,V3:=paste0("strain",V3)]

tracks_old[,Strain:=gsub("_.*","",File) ]

tracks_old[, `:=`(Protein = protein_IDs[V3 == .BY, V1],
                  Induced = protein_IDs[V3 == .BY, V2]) , by = Strain] 


