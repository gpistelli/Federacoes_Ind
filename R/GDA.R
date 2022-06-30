# GDA

library(FactoMineR)
library(factoextra)
library(cluster)

row.names(df) <- paste0(df$Entidade, 1:nrow(df))

df$Maior_n_Func <- gsub(pattern = "10-49 Func", replacement = "20-99 Func", df$Maior_n_Func) %>% 
  gsub(pattern = "50-199 Func", replacement = "100-199 Func", x = .) %>% gsub(pattern = "Até 9 Func", replacement = "Até 19 Func", x = .)

df$Maior_Fat <- gsub("De 30 a 50M Fat|De 50 a 100M Fat", "Mais de 30M Fat", df$Maior_Fat)

df[c("Maior_n_Func", "Maior_Fat", "Entidade")] <- lapply(df[c("Maior_n_Func", "Maior_Fat", "Entidade")], as.factor)     

dir_MCA_nvK <- MCA(X = df[c(1:4, 17)], quali.sup = 5, graph = F)
dir_MCA_set <- MCA(X = df[c("Transf", "Infra", "Agr", "Comerc", "Serv", "Extr", "Constr", "Entidade")], quali.sup = 8, graph = F)
#dir_MCA_all <- MCA(X = df[c("Entidade", "Maior_n_Func", "Maior_Fat", "N_Emp", "KSoc",
#                            "Transf", "Infra", "Agr", "Comerc", "Serv", "Extr")], quali.sup = 1, graph = F)
dir_MCA_reg <- MCA(X = df[c("Entidade", "Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul")], quali.sup = 1, graph = F)
#dir_MCA_all <- MCA(X = df, quali.sup = 17, graph = F)

fviz_mca_var(X = dir_MCA_nvK, axes = c(1, 2), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = T)
fviz_mca_var(X = dir_MCA_set, axes = c(1, 2), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = T)
fviz_mca_var(X = dir_MCA_all, axes = c(1, 2), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = T)
fviz_mca_var(X = dir_MCA_reg, axes = c(1, 2), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = T)

fviz_contrib(X = dir_MCA_nvK, choice = "var", axes = 1, top = 10)
fviz_contrib(X = dir_MCA_nvK, choice = "var", axes = 2, top = 10)
fviz_contrib(X = dir_MCA_nvK, choice = "var", axes = 3, top = 10)

fviz_mca_var(X = dir_MCA_nvK, axes = c(1, 2), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = T)
fviz_mca_var(X = dir_MCA_set, axes = c(1, 2), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = T)
fviz_mca_var(X = dir_MCA_all, axes = c(1, 2), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = T)
fviz_mca_var(X = dir_MCA_reg, axes = c(1, 2), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = T)

fviz_mca_ind(X = dir_MCA_nvK, repel = T, habillage = "Entidade", addEllipses = T)
fviz_mca_ind(X = dir_MCA_set, repel = T, habillage = "Entidade", addEllipses = T)
fviz_mca_ind(X = dir_MCA_all, repel = T, habillage = "Entidade", addEllipses = T)

fviz_contrib(X = dir_MCA_all, choice = "var", axes = 1, top = 10)
fviz_contrib(X = dir_MCA_all, choice = "var", axes = 2, top = 10)
fviz_contrib(X = dir_MCA_all, choice = "var", axes = 3, top = 10)
fviz_contrib(X = dir_MCA_all, choice = "var", axes = 4, top = 10)

dir_cluster_nvk <- HCPC(res = dir_MCA_nvK, nb.clust = -1, graph = F, description = T)
dir_cluster_set <- HCPC(res = dir_MCA_set, nb.clust = -1, graph = F, description = T)
dir_cluster_reg <- HCPC(res = dir_MCA_reg, nb.clust = -1, graph = F, description = T)

FactoMineR::plot.HCPC(x = dir_cluster_nvk, choice = "map", draw.tree = F)
FactoMineR::plot.HCPC(x = dir_cluster_set, choice = "map", draw.tree = F)
FactoMineR::plot.HCPC(x = dir_cluster_reg, choice = "map", draw.tree = F)

fviz_cluster(object = dir_cluster_reg,
             data = dir_cluster_reg$data.clust,
             repel = T)

dir_cluster_nvk$desc.var
dir_cluster_set$desc.var
dir_cluster_reg$desc.var

# nvk_coord <- cut(x = as.data.frame(dir_MCA_nvK$ind$coord)$`Dim 1`, 
#                 breaks = c(-4444, cut_nvk, 55555), 
#                 labels = c("A", "B", "C", "D"))

# df_nvk <- cbind(df[c(1:4, 17)], nvk_coord)

# ggplot(data = df_nvk) %>% 

dir_cluster_nvk$data.clust |> 
  group_by(Entidade) |> 
  count(clust) |> 
  mutate(freq = n / sum(n)) |> 
  as.data.frame()

dir_cluster_set$desc.var

dir_cluster_set$data.clust |> 
  group_by(Entidade) |> 
  count(clust) |> 
  mutate(freq = n / sum(n)) |> 
  as.data.frame()

dir_cluster_reg$data.clust |> 
  group_by(Entidade) |> 
  count(clust) |> 
  mutate(freq = n / sum(n)) |> 
  as.data.frame()

chisq.test(table(dir_cluster_nvk$data.clust$clust, dir_cluster_nvk$data.clust$Entidade))
chisq.test(table(dir_cluster_set$data.clust$clust, dir_cluster_set$data.clust$Entidade))
chisq.test(table(dir_cluster_reg$data.clust$clust, dir_cluster_reg$data.clust$Entidade))

# Fazendo com o factoextra

fviz_cluster(object = dir_cluster_nvk, axes = c(1, 2), data = dir_cluster_nvk$data.clust,
             repel = T, 
             ggtheme = theme_minimal()
)

dir_cluster_set$desc.var
