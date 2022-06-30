library(Consultasocio)
library(tidyverse)

df <- dplyr::bind_rows(CS_read_files_backup("FIESC", "21-24"),
               CS_read_files_backup("FIEC", "19-24"),
               CS_read_files_backup("FIESP", "22-25"),
               CS_read_files_backup("FIEG", "19-22")
               )

FIEG <- CS_read_files_backup("FIEG", "19-22")
FIEC <- CS_read_files_backup("FIEC", "19-24")
FIESP <- CS_read_files_backup("FIESP", "22-25")

# df <- CS_read_board_csv(csv_path = paste0("data/", ent_name, "/", period, "/diretores.csv"))

# df <- CS_read_files_backup("FIESC", "21-24")

ent_name <- "FIEC"
period <- "19-24"
write.csv(x = CS_read_files_backup(ent_name, period), file = paste0("data/", ent_name, "/", period, "/summary.csv"),
          fileEncoding = "utf8")

####

a <- read.csv(paste0(getwd(), "/data/", ent_name, "/", period, "/diretores.csv"), colClasses = rep("character", 6))

links <- unlist(x = strsplit(x = a$Link, split = ","))
links <- links[!is.na(links)]

diretores <- unique(gsub(pattern = "\\?page=(.*)$", replacement = "", x = gsub(pattern = "^http(.+?)/sa/", replacement = "", x = links)))
diretores <- as.data.frame(cbind(sort(diretores), dplyr::arrange(a, Consulta)$CPF))

diretores_lista <- list()
for (i in 1:nrow(diretores)){
  diretor_files <- paste0(backup_dir, "/", backup_files[grep(pattern = diretores$V1[i], backup_files)])
  
  if (length(diretor_files) > 1){
    diretores_lista[[i]] <- do.call(what = bind_rows, args = lapply(X = diretor_files, FUN = CS_get_bus_df, cpf = diretores$V2[i]))
  } else {
    diretores_lista[[i]] <- CS_get_bus_df(path = diretor_files, cpf = diretores$V2[i])
  }
}

###

kogos <- CS_get_bus_df("https://web.archive.org/web/20220513214344/http://www.consultasocio.com/q/sa/waldemar-kogos")
