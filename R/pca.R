library(Gifi)
library(FactoMineR)
library(factoextra)

df_pca <- df[c(1:4, 17)]
df_pca[1:4] <- lapply(df_pca[1:4], as.numeric)
row.names(df_pca) <- paste0(df_pca$Entidade, 1:nrow(df_pca))

dir_PCA_nivk <- PCA(X = df_pca, quali.sup = 5, graph = F)

summary(dir_PCA_nivk)

fviz_pca_biplot(dir_PCA_nivk, repel = T, habillage = "Entidade")
fviz_pca_biplot(dir_PCA_nivk, repel = T, habillage = "Entidade", addEllipses = T)

dir_clust_nvk_pca <- HCPC(dir_PCA_nivk, nb.clust = -1)

dir_clust_nvk_pca$data.clust |> 
  group_by(Entidade) |> 
  count(clust) |> 
  mutate(freq = n / sum(n)) |> 
  as.data.frame()
