CS_backup_links <- function(ent_name, period){
a <- read.csv(paste0("data/", ent_name, "/", period, "/diretores.csv"))

a <- unlist(x = strsplit(x = a$Link, split = ","))
a <- a[!is.na(a)]

for (i in 1:length(a)){
download.file(url = a[i],
              destfile = paste0("data/",
                                ent_name,
                                "/",
                                period,
                                "/backup/",
                                 gsub(pattern = "\\?page=",
                                      replacement = "",
                                      x = gsub(pattern = "^http(.+?)/sa/", replacement = "", x = a[i])),
                                "_",
                                 Sys.Date(),
                                 ".html")
              )

print(paste0(i, " of ", length(a), " (", (i/length(a))*100, "%) completed!"))  
Sys.sleep(2)
}
}

CS_backup_links("FIEC", "19-24")
CS_backup_links("FIESC", "21-24")
CS_backup_links("FIESP", "22-25")
CS_backup_links("FIEG", "19-22")
CS_backup_links("FIEAM", "19-23")
CS_backup_links("CNI", "18-22")