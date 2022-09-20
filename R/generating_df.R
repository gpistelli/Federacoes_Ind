library(Consultasocio)
library(tidyverse)

df <- dplyr::bind_rows(CS_read_files_backup("FIESC", "21-24"),
               CS_read_files_backup("FIEC", "19-24"),
               CS_read_files_backup("FIESP", "22-25"),
               CS_read_files_backup("FIEG", "19-22"),
               CS_read_files_backup("FIEAM", "19-23"),
               CS_read_files_backup("CNI", "18-22")
               )