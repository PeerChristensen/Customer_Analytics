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
  ggplot(aes(x = RecencyDays, fill = Customer_Segment)) +
  geom_density(alpha = .7) +
  scale_fill_manual(values = cols)

rfm %>%
  ggplot(aes(x = log(Frequency), fill = Customer_Segment)) +
  geom_density(alpha = .7)  +
  scale_fill_manual(values = cols)

rfm %>%
  ggplot(aes(x = log(Spend), fill = Customer_Segment)) +
  geom_density(alpha = .7)  +
  scale_fill_manual(values = cols)
