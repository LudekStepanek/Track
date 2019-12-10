

# setwd is needed for file.rename to work
setwd("C:\\Users\\Ludek\\Documents\\R\\data\\2019_01_07_mutants1") 


lF=(list.files(pattern="*.*", recursive = T))

lFsplit <- strsplit(lF, "_", fixed = TRUE)

#lFrename <- sapply(lFsplit, function(x){paste(c(x[1],x[3],x[2],x[4:6]), collapse = "_")}) #swap day and month

file.rename(lF, lFrename)

#lF=(list.files(pattern="*.csv"))
#----deal with old experiment-----------
protein_IDs <- fread("C:\\Users\\Ludek\\Documents\\R\\data\\Proteins_strains_table.csv")

protein_IDs[,V1:=sapply(strsplit(V1, " "),`[`, 1)][,V1:=sapply(strsplit(V1, "\\."),`[`, 3 )][V2 == "IND72h", V2:="IND"]
protein_IDs[,V3:=paste0("strain",V3)]

al <- lapply(lF,strsplit,"_")

lFd <- setDT(list(lF))
lFd[,`:=`(base = sapply(strsplit(V1,"_"),`[`,1),ext = sapply(strsplit(V1,"_"),`[`,2))]
newlist <- lFd[, basenew:=protein_IDs[V3==.BY,file_base],by = base][,paste0(basenew,"_",ext)]
file.rename(lF, newlist)
#file.rename(lF, paste0("kd_", 1:length(lF),".csv"))
protein_IDs[,file_base:=paste0("2019_01_07_",V1,"_",V2)]
split_name <- function(tracks) {
  tracks[,
         `:=`(
           Line = unlist(strsplit(File, "_", fixed = TRUE))[4],
           Condition = unlist(strsplit(File, "_", fixed = TRUE))[5],
           Field = unlist(strsplit(File, "_", fixed = TRUE))[6]
         ),
         by = File
         ]
  
}
paste(lFsplit[[1]], collapse = "_")
