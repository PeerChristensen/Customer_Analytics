# RFM segmentation

library(tidyverse)
library(readxl)
library(lubridate)
library(glue)
library(zoo)
library(reshape2)
library(magrittr)

df <- read_excel("Online Retail.xlsx")

# MONETARY

# Calculate Spend quartiles (q=4)

monetary <- df %>% 
  group_by(CustomerID) %>%
  summarise(Spend = sum(Quantity * UnitPrice)) %>%
  mutate(M_Quantile = ntile(Spend, 4)) # quartile

# check mean and min for each quartile
monetary %>% group_by(Quantile) %>% summarise(mean = mean(Spend))

monetary %>% group_by(Quantile) %>% summarise(min = min(Spend))

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

# JOIN

rfm <- list(recency,frequency,monetary) %>% 
  reduce(left_join)

# add rfm segment and score 

rfm %<>%
  mutate(RFM_Segment = paste0(R_Quantile,F_Quantile,M_Quantile),
         Score   = R_Quantile+F_Quantile+M_Quantile)
  
# size of each segment

rfm %>% 
  group_by(RFM_Segment)  %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

# assign customer segment based on rfm score

rfm %<>%
  mutate(Customer_Segment = case_when(Score > 8 ~ "Gold",
                                      Score > 4 ~ "Silver",
                                      Score > 0 ~ "Bronze")) %>%
  mutate(Customer_Segment = fct_relevel(Customer_Segment,"Gold","Silver","Bronze"))


# rfm summary of customer segments

rfm %>%
  group_by(Customer_Segment) %>%
  summarise(R_mean = mean(RecencyDays),
            F_mean = mean(Frequency),
            M_mean = mean(Spend))


# define colours (gold, silver, bronze)

cols <- c("#D6AF36","#A7A7AD","#824A02")

rfm %>%
  ggplot(aes(x = log(RecencyDays), fill = Customer_Segment)) +
  geom_density(alpha = .7) +
  scale_fill_manual(values = cols) +
  theme_light()

rfm %>%
  ggplot(aes(x = log(Frequency+1), fill = Customer_Segment)) +
  geom_density(alpha = .7)  +
  scale_fill_manual(values = cols) +
  theme_light()

rfm %>%
  ggplot(aes(x = log(Spend), fill = Customer_Segment)) +
  geom_density(alpha = .7)  +
  scale_fill_manual(values = cols) +
  theme_light()

# plot faceted distribution with long format

rfm %>% 
  mutate(RecencyDays = log(RecencyDays),Frequency = log(Frequency), Spend = log(Spend+1)) %>%
  select(CustomerID,Customer_Segment,RecencyDays,Frequency,Spend) %>% 
  group_by(Customer_Segment,CustomerID) %>% 
  gather(metric, value, -CustomerID, - Customer_Segment) %>%
  ggplot(aes(x=value, fill = Customer_Segment)) +
  geom_density(alpha=.7) +
  facet_wrap(~metric,scales = "free") +
  scale_fill_manual(values = cols) +
  theme_light()

# preprocess data: log, center, scale

rfm_norm <- rfm %>%
  select(RecencyDays,Frequency,Spend) %>% 
  apply(2,function(x) log(x+1)) %>%
  apply(2, function(x) round(x-mean(x,na.rm=T),1)) %>%
  scale() %>%
  as_tibble %>%
  mutate(CustomerID = rfm$CustomerID,
         Customer_Segment = rfm$Customer_Segment)

# snake plot
rfm_norm %>%
  group_by(Customer_Segment,CustomerID) %>%
  gather(metric, value, -CustomerID, - Customer_Segment) %>%
  group_by(Customer_Segment,metric) %>%
  summarise(value = mean(value, na.rm = T)) %>%
  ungroup() %>%
  mutate(metric = fct_relevel(metric, "RecencyDays","Frequency","Spend")) %>%
  ggplot(aes(x=factor(metric),y=value,group=Customer_Segment,colour = Customer_Segment)) +
  geom_line(size=1.5) +
  geom_point(size=2) +
  scale_colour_manual(values = cols) +
  theme_light()

# relative variable importance

group_means <- rfm %>%
  group_by(Customer_Segment) %>%
  summarise(Recency = mean(RecencyDays),
            Frequency = mean(Frequency),
            Monetary = mean(Spend,na.rm=T)) %>%
  select(-Customer_Segment)

pop_means <- rfm %>%
  summarise(Recency = mean(RecencyDays),
            Frequency = mean(Frequency),
            Monetary = mean(Spend,na.rm=T)) %>% 
  as_vector()

relative_imp <- group_means %>% 
  apply(2,function(x) x / pop_means - 1) %>%
  as_tibble() %>%
  mutate(Customer_Segment = levels(rfm$Customer_Segment)) %>%
  select(Customer_Segment, everything())

# relative variable importance heatmap

relative_imp %>%
  gather(metric, value, - Customer_Segment) %>%
  mutate(Customer_Segment = fct_relevel(Customer_Segment, "Bronze","Silver","Gold"),
         metric = fct_relevel(metric, "Recency", "Frequency", "Monetary")) %>%
  ggplot(aes(x = metric, y = Customer_Segment)) +
    geom_raster(aes(fill= value)) +
    geom_text(aes(label = glue::glue("{round(value,2)}")), size = 10, color = "snow") +
  theme_light() +
  theme(axis.text = element_text(size = 16))

  


