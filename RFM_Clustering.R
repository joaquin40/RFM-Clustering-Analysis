## ------------------------------------------------------------------------------------------------
knitr::purl("RFM_Clustering.rmd")


## ----setup, include=FALSE------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------------------------------
pacman::p_load(tidyverse, lubridate, data.table, skimr)


## ------------------------------------------------------------------------------------------------
df <- fread("./data/Online Retail.csv")


## ------------------------------------------------------------------------------------------------
df
skim(df)


## ------------------------------------------------------------------------------------------------
DataExplorer::plot_missing(df)


## ------------------------------------------------------------------------------------------------
df1 <- drop_na(df)


## ------------------------------------------------------------------------------------------------
skim(df1)


## ------------------------------------------------------------------------------------------------
max_date <- max(df2$InvoiceDate) + days(1)

df2 <- df1 %>% 
  mutate(InvoiceDate = dmy_hm(InvoiceDate)) 

dates_recency <- df2 %>% 
  group_by(CustomerID) %>% 
  summarise(
    date_diff = difftime(max_date, InvoiceDate , units = "days"),
    recency = min(date_diff) %>% round(5)) %>% 
  arrange(date_diff) %>% 
  slice(1)


df3 <- df2 %>% 
  group_by(CustomerID) %>% 
  summarise(frequency = n(),
            monetary = sum(UnitPrice * Quantity)) %>% 
  left_join(dates_recency, by = "CustomerID") %>% 
  select(-date_diff) %>%
  mutate(recency_num = as.numeric(recency)) %>% 
  ungroup()
 
df3

# normalized rfm
rfm <- data.frame(scale(df3[,c(2:3,5)]))
class(rfm)


## ------------------------------------------------------------------------------------------------
set.seed(1)
wcss <- c()
k <-  10

for(i in 1:k){
  km = kmeans(rfm, i,  nstart = 100, iter.max = 200)
  wcss[i] = km$tot.withinss
}

ggplot(data.frame(x = 1:k, wcss = wcss), aes(x = x, y = wcss)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = (1:k)) + 
  labs(x = "Number of Cluster (k)", y = "Within-cluster sum of squares", title = "") 


## ------------------------------------------------------------------------------------------------
set.seed(1)
km <- kmeans(rfm, 7, nstart = 100, iter.max = 200)


## ------------------------------------------------------------------------------------------------
km

## ------------------------------------------------------------------------------------------------
df3$cluster <- km$cluster 

df3 %>% select(CustomerID, cluster)


## ------------------------------------------------------------------------------------------------
#rfm$clsuter <- km$cluster
rfm_stat <- df3[,c(2:3,5:6)]



## ------------------------------------------------------------------------------------------------
rfm_stat

rfm_stat %>% 
  group_by(cluster) %>% 
  summarise(
    avgRecency = mean(recency_num),
    avgFrequency = mean(frequency),
    avgMonetary = mean(monetary)
  ) %>% 
  arrange(avgRecency, desc(avgMonetary) )



## ------------------------------------------------------------------------------------------------


a <- df2 %>% 
  group_by(CustomerID) %>% 
  summarise(
    days = difftime(max(InvoiceDate),InvoiceDate , units = "days"))

hours <- (as.numeric(a$days) %% 1) *24
minutes <- (hours %% 1) * 60

paste0(floor(a$days), " days ", floor(hours), "  ", floor(minutes), " minutes")



date1 <- as.POSIXct("2022-05-20 12:00:00")
date2 <- as.POSIXct("2022-05-24 10:30:00")

# Calculate difference in days
diff_days <- as.numeric(difftime(date2, date1, units = "days"))

# Calculate remaining time in seconds
diff_seconds <- as.numeric(difftime(date2, date1, units = "secs"))

# Convert remaining seconds into hours and minutes format
diff_hm <- format(as.POSIXct(diff_seconds, origin = "1970-01-01", tz = "GMT"), format = "%H:%M")

# Combine the results
diff_time <- paste0(diff_days, " days ", diff_hm)
diff_time

