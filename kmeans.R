
# RFM SEGMENTATION WITH K-MEANS CLUSTERING

library(tidyverse)
library(readxl)
library(lubridate)
library(glue)
library(zoo)
library(magrittr)
library(factoextra)

df <- read_excel("Online Retail.xlsx")

# MONETARY

# Calculate Spend quartiles (q=4)

monetary <- df %>% 
  group_by(CustomerID) %>%
  summarise(Spend = sum(Quantity * UnitPrice)) %>%
  mutate(M_Quantile = ntile(Spend, 4)) # quartile

# check mean and min for each quartile
monetary %>% group_by(M_Quantile) %>% summarise(mean = mean(Spend))

monetary %>% group_by(M_Quantile) %>% summarise(min = min(Spend))

# RECENCY

last_date <- max(as_date(df$InvoiceDate))

recency <- df %>%
  group_by(CustomerID) %>%
  summarise(RecencyDays = as.numeric(last_date - max(as_date(InvoiceDate)))) %>%
  mutate(R_Quantile = ntile(desc(RecencyDays), 4)) # quartile

# FREQUENCY

frequency <- df %>%
  group_by(CustomerID) %>%
  summarise(Frequency = n()) %>%
  mutate(F_Quantile = ntile(Frequency, 4)) # quartile

tenure <- df %>%
  group_by(CustomerID) %>%
  summarise(Tenure = as.numeric(last_date - min(as_date(InvoiceDate)))) %>%
  mutate(T_Quantile = ntile(Tenure, 4)) # quartile

# JOIN

rfm <- list(recency,frequency,monetary,tenure) %>% 
  reduce(left_join) %>%
  filter(!is.na(CustomerID)) %>%
  filter(Spend > 0)

# add rfm segment and score 

rfm %<>%
  mutate(RFM_Segment = paste0(R_Quantile,F_Quantile,M_Quantile,T_Quantile),
         Score   = R_Quantile+F_Quantile+M_Quantile+T_Quantile)

# assign customer segment based on rfm score

rfm %<>%
  mutate(Customer_Segment = case_when(Score > 12 ~ "Gold",
                                      Score > 8 ~ "Silver",
                                      Score > 0 ~ "Bronze")) %>%
  mutate(Customer_Segment = fct_relevel(Customer_Segment,"Gold","Silver","Bronze"))

# preprocess data: log, center, scale 

rfm_norm <- rfm %>%
  select(RecencyDays,Frequency,Spend,Tenure) %>% 
  apply(2,function(x) log(x+1)) %>%
  apply(2, function(x) round(x-mean(x,na.rm=T),1)) %>%
  scale() %>%
  as_tibble %>%
  mutate(CustomerID = rfm$CustomerID,
         Customer_Segment = rfm$Customer_Segment)

# K-means clustering

fviz_nbclust(rfm_norm[,1:4], kmeans, method = "wss")

rfm_clust <- kmeans(rfm_norm[,1:4], centers=4, nstart = 25)

table(rfm$Customer_Segment,rfm_clust$cluster)

fviz_cluster(rfm_clust, data = rfm_norm[,1:4], geom=c("point")) +
  theme_light()

# snake plot with cluster means

rfm_clust$centers %>% 
  as_tibble() %>%
  mutate(Customer_Segment = factor(1:4)) %>% # last val = n clusters
  gather(metric, value, -Customer_Segment) %>%
  group_by(Customer_Segment,metric) %>%
  ungroup() %>%
  mutate(metric = fct_relevel(metric, "RecencyDays","Frequency","Spend")) %>%
  ggplot(aes(x=factor(metric),y=value,group=Customer_Segment,colour = Customer_Segment)) +
  geom_line(size=1.5) +
  geom_point(size=2) +
  theme_light()

