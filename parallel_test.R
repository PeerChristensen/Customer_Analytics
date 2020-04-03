library(readxl)
library(tidyverse)
library(h2o)
library(foreach)
library(doParallel)

df <- read_excel("Online Retail.xlsx")

df <- df %>% 
  group_by(CustomerID,InvoiceNo) %>%
  summarise(quantity = sum(Quantity)) %>%
  ungroup() %>%
  select(-InvoiceNo) %>%
  drop_na()

#setup parallel backend
cl <- makeCluster(3)
registerDoParallel(cl)

# create data partitions
IDs <- df %>%
  distinct(CustomerID)
  
fold_indices <- caret::createFolds(IDs$CustomerID,5)

IDs_list <- list()
df_list <- list()

for (i in 1:length(fold_indices)) {
  
  IDs_list[i] <- IDs[fold_indices[[i]],]
  
  df_list[[i]] <- df %>%
    filter(CustomerID %in% IDs_list[[i]])
  
}

ids <- df %>% dplyr::select(CustomerID) %>% distinct() %>% pull(CustomerID)

library(tictoc)
tic()
output <- foreach(i=1:3, .combine=rbind) %dopar% 
  {
    library(h2o)
    library(tidyverse)
    # Initializing H2O on each instance 
    h2o.init(nthreads = 1)
    
  #  for(id in IDs_list[[i]]) {
  #    d <- df_list[[i]]
    for(id in ids) {
      d <- df %>%
        filter(CustomerID == id) 
       
      if (nrow(d) > 5) {
        d <- d %>%
         as.h2o()
      
      # if (nrow(d) > 1) {
      #   d <- d %>%
      #   filter(CustomerID == id) %>%
      #   as.h2o()
      
      
        km = h2o.kmeans(d,k = 1, x = c("quantity"),estimate_k = T)
        
        centers <- km@model$centers %>% as_tibble()

        centers$wss <- km@model$training_metrics@metrics$centroid_stats$within_cluster_sum_of_squares
      }
        #all_centers <- rbind(all_centers,cbind(Customer_Key = key,centers))
      }}
  
toc()
    

