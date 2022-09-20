# GDA

library(FactoMineR)
library(factoextra)
library(cluster)

# Ajuste das categorias
df$Maior_n_Func <- gsub(pattern = "10-49 Func", replacement = "20-99 Func", df$Maior_n_Func) %>% 
  gsub(pattern = "50-199 Func", replacement = "100-199 Func", x = .) %>% gsub(pattern = "Até 9 Func", replacement = "Até 19 Func", x = .)

df$Maior_Fat <- gsub("De 30 a 50M Fat|De 50 a 100M Fat|Mais de 100M Fat", "Mais de 30M Fat", df$Maior_Fat)

df[c("Maior_n_Func", "Entidade")] <- lapply(df[c("Maior_n_Func", "Entidade")], as.factor)     

df$Maior_n_Func <- factor(x = df$Maior_n_Func, levels = c("Até 19 Func", "20-99 Func", "100-199 Func", "Mais de 200 Func"))
df$Maior_Fat <- factor(x = df$Maior_Fat, levels = c("Até 240k Fat", "Até 2,4M Fat", "De 2,4 a 5M Fat", "De 5 a 10M Fat",
                                                    "De 10 a 30M Fat", "Mais de 30M Fat"))

# Construindo a tabela de distribuição das categorias
summary_list <- lapply(df[c("N_Emp", "Maior_n_Func", "Maior_Fat", "KSoc", "Transf", "Serv", "Comerc", "Constr", "Agr", "Extr", "Infra")], summary, use.na = T)
names(summary_list) <- c("N_Emp", "Maior_n_Func", "Maior_Fat", "KSoc", "Transf", "Serv", "Comerc", "Constr", "Agr", "Extr", "Infra")

summary_kable_list <- list()
for (i in 1:length(summary_list)){
  summary_kable_list[[i]]  <- kable(summary_list[[i]], col.names = names(summary_list)[i])
}

# TABELA 1
kables(summary_kable_list) %>% kable_styling(bootstrap_options = "striped") %>% save_kable("img/Tabela1.html")


# Construindo as ACMs
dir_MCA_nvK <- MCA(X = df[c(1:4, 17)], quali.sup = 5, graph = F)
dir_MCA_set <- MCA(X = df[c("Transf", "Infra", "Agr", "Comerc", "Serv", "Extr", "Constr", "Entidade")], quali.sup = 8, graph = F)

# ACM 1
fviz_mca_var(X = dir_MCA_nvK, axes = c(1, 2), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = T, 
             title = "ACM 1: Nível de capital dos membros das federações da indústria selecionadas")

# ACM 2
fviz_mca_var(X = dir_MCA_set, axes = c(1, 2), col.var = "contrib", repel = T,  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "ACM 2: Atuação setorial dos membros das federações da indústria selecionadas")


# Construindo os clusteres
dir_cluster_nvk <- HCPC(res = dir_MCA_nvK, nb.clust = -1, graph = F, description = T, iter.max = 10000)

# Salvando tabelas de descrição dos clusteres
for (i in 1:5){
dir_cluster_nvk$desc.var$category[[as.character(i)]] %>% 
  knitr::kable(x = ., digits = 2, caption = paste0("Principais categorias do cluster ", i), fileencoding = "utf8") %>%
  kable_styling(bootstrap_options = "striped") %>% 
  save_kable(file = paste0("img/clust", i, ".html"))
}

# Análise da distribuição de clusteres por entidades

table_freq_clust_nvk <- dir_cluster_nvk$data.clust |> 
  group_by(Entidade) |> 
  count(clust) |> 
  mutate(freq = n / sum(n)) |> 
  as.data.frame()

# GRÁFICO 1
ggplot(data = table_freq_clust_nvk) +
  aes(fill = clust, y = freq, x = Entidade) + 
  geom_bar(position="stack", stat="identity")

# Construindo o gráfico da distribuição de atuação setorial por cluster econômico

df$clust_nvk <- dir_cluster_nvk$data.clust$clust

Clust_Transf <- ggplot(data = df) +
  geom_bar(mapping = aes(x = clust_nvk, fill = Transf), position = "fill")

Clust_Agr <- ggplot(data = df) +
  geom_bar(mapping = aes(x = clust_nvk, fill = Agr), position = "fill")

Clust_Serv <- ggplot(data = df) +
  geom_bar(mapping = aes(x = clust_nvk, fill = Serv), position = "fill")

Clust_Comerc <- ggplot(data = df) +
  geom_bar(mapping = aes(x = clust_nvk, fill = Comerc), position = "fill")

# GRÁFICO 2
grid.arrange(Clust_Agr, Clust_Comerc, Clust_Serv, Clust_Transf, nrow = 2)