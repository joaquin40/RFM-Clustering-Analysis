## ----------------------------------------------------
knitr::purl("RFM_Clustering.rmd")


## ----setup, include=FALSE----------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----------------------------------------------------
pacman::p_load(tidyverse, lubridate, data.table, skimr, cluster, cowplot)


## ----------------------------------------------------
df <- fread("./data/Online Retail.csv")


## ----------------------------------------------------
df
skim(df)


## ----------------------------------------------------
DataExplorer::plot_missing(df)
dev.copy(png, "./images/missing.png")
dev.off()


## ----------------------------------------------------
df1 <- drop_na(df)


## ----------------------------------------------------
skim(df1)


## ----------------------------------------------------
df2 <- df1 %>% 
  mutate(InvoiceDate = dmy_hm(InvoiceDate)) 

max_date <- max(df2$InvoiceDate) + days(1)

# derive recency 
dates_recency <- df2 %>% 
  group_by(CustomerID) %>% 
  summarise(
    date_diff = difftime(max_date, InvoiceDate , units = "days"),
    recency = min(date_diff) %>% round(5)) %>% 
  arrange(date_diff) %>% 
  slice(1)

# derive freq, monetary and merge with recency
df3 <- df2 %>% 
  group_by(CustomerID) %>% 
  summarise(frequency = n(),
            monetary = sum(UnitPrice * Quantity)) %>% 
  left_join(dates_recency, by = "CustomerID") %>% 
  select(-date_diff) %>%
  mutate(recency_num = as.numeric(recency)) %>% 
  ungroup()
 
df3 %>% head()


## ----------------------------------------------------
skim(df3)


## ----------------------------------------------------
frq <- ggplot(df3, aes(frequency)) + 
  geom_boxplot() +
  labs(x = "Frequency") +
  scale_y_continuous(labels = NULL)+ 
  theme_classic()

mon <- ggplot(df3, aes(monetary)) + 
  geom_boxplot() +
  labs(x = "Monetary") +
  scale_y_continuous(labels = NULL) +
  theme_classic()

rec <- ggplot(df3, aes(recency_num)) + 
  geom_boxplot() +
  labs(x = "Recency") + 
  scale_y_continuous(labels = NULL) + 
  theme_classic()



ggsave("./images/rfm_boxplot.png",cowplot::plot_grid(frq, mon, rec,ncol = 1))


## ----------------------------------------------------
summary(df3)


## ----------------------------------------------------
df3_positive <- df3 %>% 
  select(frequency, monetary, recency_num, CustomerID) %>% 
  filter(monetary > 0)


df3_positive_log <- apply(df3_positive[,1:3], 2, log)
df3_positive_log_scale <- data.frame(scale(df3_positive[,1:3])) %>% 
  cbind(df3_positive$CustomerID) %>% 
  rename(CustomerID = "df3_positive$CustomerID")


## ----------------------------------------------------
rfm_log <- df3_positive_log_scale


## ----------------------------------------------------
pacman::p_load(NbClust, clustertend, factoextra,hopkins)

res <- get_clust_tendency(rfm_log[,1:3], n = nrow(rfm_log[,1:3])-1, graph = T, gradient =  list(low = "steelblue", high = "white")
)


## ----------------------------------------------------
res$hopkins_stat


## ----------------------------------------------------
fviz_nbclust(rfm_log[,1:3], kmeans, method = "wss")+ theme_classic()
dev.copy(png, "./images/wss_log.png")
dev.off()

fviz_nbclust(rfm_log[,1:3], kmeans, method = "silhouette")+ theme_classic()
dev.copy(png, "./images/silhouette_log.png")
dev.off()


## ----------------------------------------------------
clusternum_log <- NbClust((rfm_log[,1:3]), distance="euclidean", method="kmeans")


## ----------------------------------------------------
set.seed(1)
km_log <- kmeans(rfm_log[,1:3], 7, nstart = 100, iter.max = 100)
#km


## ----------------------------------------------------
max_cluster <- max(unique(km_log$cluster))

fviz_cluster(km_log, data = rfm_log[,1:3], palette = c("#FC4E07", "#00AFBB", "#E7B800", "#008A00", "#288BA8", "#E83845", "#846AB0"), ellipse.type = "euclid", 
star.plot = T, 
repel = T, 
ggtheme = theme_minimal() )

dev.copy(png, paste0("./images/log_pca_kmeans_",max_cluster ,".png") )
dev.off()


## ----------------------------------------------------
sil <-  silhouette(km_log$cluster, dist(rfm_log[,1:3]))
fviz_silhouette(sil ,palette =c("#FC4E07", "#00AFBB", "#E7B800", "#008A00","#288BA8", "#E83845", "#846AB0"), ggtheme = theme_classic())

dev.copy(png, paste0("./images/log_val_pca_kmeans_", max_cluster, ".png"))
dev.off()


## ----------------------------------------------------
df3_positive$cluster<- km_log$cluster


## ----------------------------------------------------
df3_positive %>% 
  group_by(cluster) %>% 
  count()


## ----------------------------------------------------
rfm_stat <-df3_positive[,c("frequency", "monetary", "recency_num", "cluster")]


## ----------------------------------------------------
rfm_stat_cluster <- rfm_stat %>% 
  group_by(cluster) %>% 
  summarise(
    avgRecency = mean(recency_num),
    avgFrequency = mean(frequency),
    avgMonetary = mean(monetary),
    n = n()
  ) %>% 
  arrange(avgRecency, desc(avgMonetary) )

rfm_stat_cluster


## ----------------------------------------------------
stat_name_rfm <- colnames(rfm_stat_cluster[-1])

gg <- function(y){
  
  max_cluster <-  max(rfm_stat_cluster[1] )
    
p = ggplot(data = rfm_stat_cluster, aes(x= factor(cluster), y = .data[[y]], fill = factor(cluster))) + 
  geom_bar(stat = "identity") +
  labs(x = "Cluster", y = paste("Average ", str_sub(y,start = 4) ) ) +
  theme(legend.position = "none") + 
  scale_fill_manual(values = c("#FC4E07", "#00AFBB", "#E7B800", "#008A00","#288BA8", "#E83845", "#846AB0")) 

 #ggsave(paste0("./images/", "rfm_log_kmeans", max_cluster, y, ".png"), p)
 
}

# bar plot
map(stat_name_rfm[1:3], gg)



## ----------------------------------------------------
iqr <- apply(df3[,c(2,3,5)], 2, IQR)

lower <-  apply(df3[,c(2,3,5)], 2, quantile, probs = c(0.25)) - 3 * iqr
upper <- apply(df3[,c(2,3,5)], 2, quantile, probs = c(0.75)) + 3 * iqr


## ----------------------------------------------------
remove_outliers <- df3 %>% 
  filter(between(frequency, lower[1],upper[1])) %>% 
  filter(between(monetary, lower[2],upper[2]))  %>%
  filter(between(recency_num, lower[3],upper[3])) 


## ----------------------------------------------------
rfm <- data.frame(scale(remove_outliers[,c(2:3, 5)] )) %>% 
  cbind(remove_outliers$CustomerID) %>%
  rename(CustomerID = "remove_outliers$CustomerID")


# unscaled data
unscaled <- function(x){
  return(rfm[[x]] * sd(remove_outliers[[x]]) + mean(remove_outliers[[x]]))
}

col_name <- colnames(rfm[,1:3])
res <- map(col_name, unscaled)

rfm_unscaled = rfm
for(i in seq_along(col_name)){
  rfm_unscaled[col_name[i]] = res[[i]]
}



## ---- eval=FALSE, echo=FALSE-------------------------
## # Scale to remove outliers
## 
## rfm <- data.frame(scale(df3[,c(2:3, 5)] )) %>%
##   cbind(df3$CustomerID) %>%
##   filter(between(frequency, -3,3)) %>%
##   filter(between(monetary, -3,3)) %>%
##   filter(between(recency_num, -3,3)) %>%
##   rename(CustomerID = "df3$CustomerID")
## 
## 
## 
## # unscaled data
## unscaled <- function(x){
##   return(rfm[[x]] * sd(df3[[x]]) + mean(df3[[x]]))
## }
## 
## col_name <- colnames(rfm[,1:3])
## res <- map(col_name, unscaled)
## 
## rfm_unscaled = rfm
## for(i in seq_along(col_name)){
##   rfm_unscaled[col_name[i]] = res[[i]]
## }
## 
## #rfm <- df3[,c(2,3,5)]
## 


## ----------------------------------------------------
set.seed(1)
wcss <- c()
k <-  15

for(i in 1:k){
  km = kmeans(rfm[,1:3], i+1,  nstart = 100, iter.max = 200)
  wcss[i] = km$tot.withinss
}

ggplot(data.frame(x = 2:(k+1), wcss = wcss), aes(x = x, y = wcss)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = (1:k)) + 
  labs(x = "Number of Cluster (k)", y = "Within-cluster sum of squares", title = "") 


## ----------------------------------------------------
set.seed(1)
k_max <- 15

sil_fun <- function(k) {
  km <- kmeans(rfm[,1:3], centers = k, nstart = 200, iter.max = 500)
  ss <- silhouette(km$cluster, dist(rfm[,1:3]))
  return(mean(ss[,3]))
}

sil_width_mean_list <- map(2:k_max, sil_fun)




## ----------------------------------------------------
sil_width_mean <- unlist(sil_width_mean_list)


## ----------------------------------------------------
# Plot the results
plot(2:k_max, sil_width_mean, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters (k)", ylab = "Silhouette Width")


## ----------------------------------------------------
pacman::p_load(NbClust, clustertend, factoextra,hopkins)

res <- get_clust_tendency(rfm[,1:3], n = nrow(rfm[,1:3])-1, graph = T, gradient =  list(low = "steelblue", high = "white")
)


## ----------------------------------------------------
res$hopkins_stat


## ----------------------------------------------------
res$plot
dev.copy(png, "./images/hopkins_plot.png")
dev.copy()


## ----------------------------------------------------
fviz_nbclust(rfm[,1:3], kmeans, method = "wss")+ theme_classic()
dev.copy(png, "./images/wss.png")
dev.off()

fviz_nbclust(rfm[,1:3], kmeans, method = "silhouette")+ theme_classic()
dev.copy(png, "./images/silhouette.png")
dev.off()


## ----------------------------------------------------
clusternum <- NbClust((rfm[,1:3]), distance="euclidean", method="kmeans")


## ----------------------------------------------------
set.seed(1)
km <- kmeans(rfm[,1:3], 3, nstart = 100, iter.max = 100)
#km


## ----------------------------------------------------
max_cluster <- max(unique(km$cluster))

fviz_cluster(km, data = rfm[,1:3], palette = c("#FC4E07", "#00AFBB", "#E7B800", "#008A00", "#288BA8", "#E83845", "#846AB0"), ellipse.type = "euclid", 
star.plot = T, 
repel = T, 
ggtheme = theme_minimal() )

dev.copy(png, paste0("./images/pca_kmeans_",max_cluster ,".png") )
dev.off()


## ----------------------------------------------------
sil <-  silhouette(km$cluster, dist(rfm[,1:3]))
fviz_silhouette(sil ,palette =c("#FC4E07", "#00AFBB", "#E7B800", "#008A00","#288BA8", "#E83845", "#846AB0"), ggtheme = theme_classic())

dev.copy(png, paste0("./images/val_pca_kmeans_", max_cluster, ".png"))
dev.off()


## ----------------------------------------------------
km


## ----------------------------------------------------
#df3$cluster  <- km$cluster 
rfm_unscaled$cluster  <- km$cluster 



## ----------------------------------------------------
rfm_unscaled %>% 
  group_by(cluster) %>% 
  count()


## ----------------------------------------------------
rfm_stat <- rfm_unscaled[,c("frequency", "monetary", "recency_num", "cluster")]


## ----------------------------------------------------
rfm_stat_cluster <- rfm_stat %>% 
  group_by(cluster) %>% 
  summarise(
    avgRecency = mean(recency_num),
    avgFrequency = mean(frequency),
    avgMonetary = mean(monetary),
    n = n()
  ) %>% 
  arrange(avgRecency, desc(avgMonetary) )

rfm_stat_cluster


## ----------------------------------------------------
stat_name_rfm <- colnames(rfm_stat_cluster[-1])

gg <- function(y){
  
  max_cluster <-  max(rfm_stat_cluster[1] )
    
p = ggplot(data = rfm_stat_cluster, aes(x= factor(cluster), y = .data[[y]], fill = factor(cluster))) + 
  geom_bar(stat = "identity") +
  labs(x = "Cluster", y = paste("Average ", str_sub(y,start = 4) ) ) +
  theme(legend.position = "none") + 
  scale_fill_manual(values = c("#FC4E07", "#00AFBB", "#E7B800", "#008A00","#288BA8", "#E83845", "#846AB0")) 

 ggsave(paste0("./images/", "rfm_kmeans", max_cluster, y, ".png"), p)

}

y ="avgRecency"
# bar plot
map(stat_name_rfm[1:3], gg)



