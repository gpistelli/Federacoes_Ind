library(Consultasocio)
library(tidyverse)

df <- dplyr::bind_rows(CS_read_files_backup("FIESC", "21-24"),
               CS_read_files_backup("FIEC", "19-24"),
               CS_read_files_backup("FIESP", "22-25")
               )

# df <- CS_read_board_csv(csv_path = paste0("data/", ent_name, "/", period, "/diretores.csv"))

# df <- CS_read_files_backup("FIESC", "21-24")

ent_name <- "FIESC"
period <- "21-24"
write.csv(x = df, file = paste0("data/", ent_name, "/", period, "/summary.csv"))

###

kogos <- CS_get_bus_df("https://web.archive.org/web/20220513214344/http://www.consultasocio.com/q/sa/waldemar-kogos")
