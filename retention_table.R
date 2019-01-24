# Retention rate visualization

library(tidyverse)
library(readxl)
library(lubridate)
library(glue)
library(zoo)
library(reshape2)

df <- read_excel("Online Retail.xlsx")

# acquisition month cohorts

df <- df                                  %>%
  select(CustomerID,InvoiceDate)          %>%
  mutate(invoiceMonth = floor_date(InvoiceDate,unit = "month")) %>%
  group_by(CustomerID)                    %>%
  mutate(cohortMonth = min(invoiceMonth)) %>%
  mutate(cohortIndex = round(as.numeric((as.yearmon(invoiceMonth) - as.yearmon(cohortMonth)) * 12) + 1),0) %>%
  ungroup()
                               
# Count monthly active customers from each cohort

retention_table <- df                       %>% 
  group_by(cohortMonth,cohortIndex)         %>%
  summarise(n = length(unique(CustomerID))) %>%
  ungroup()                                 %>%
  spread(key = cohortIndex,value = n)

cohort_sizes <- retention_table$`1`

#for (col in 1:ncol(retention_rate)) {
#    retention_rate[,col] = (retention_rate[,col] / cohort_sizes) * 100
#  }

# calculate percentages for each cohort

retention_rate <- retention_table  %>%
  select(-cohortMonth)             %>%
  apply(2,function(x) round((x / cohort_sizes) * 100,0)) %>%
  as_tibble()                      %>%
  mutate(cohortMonth = retention_table$cohortMonth) %>%
  select(cohortMonth, everything())

# melt data frame

retention_rate_melt <- retention_rate   %>%
  melt(id.vars = 'cohortMonth',na.rm=T) %>%
  mutate(date_col    = glue("{months(cohortMonth,abbreviate = T)} {year(cohortMonth)}"),
         cohortMonth = as.numeric(cohortMonth))

# plot retention heatmap

retention_rate_melt     %>%
  filter(variable != 1) %>% # remove 1st month
  ggplot(aes(x = variable, y = reorder(cohortMonth,desc(cohortMonth)))) +
  geom_raster(aes(fill = log(value))) +
  coord_equal(ratio = 1) +
  geom_text(aes(label = glue("{round(value,0)}%")), size = 5, color = "snow") +
  scale_y_discrete(breaks = retention_rate_melt$cohortMonth,
                   labels = retention_rate_melt$date_col) +
  theme_light() +
  theme(axis.text    = element_text(size = 14),
        axis.title   = element_text(size = 16),
        panel.grid   = element_blank(),
        panel.border = element_blank()) +
  labs(y= "cohort start month",
       x = "month #") +
  guides(fill=FALSE)

# retention lines

retention_rate_melt %>% 
  rename(Day = variable) %>%
  ggplot(aes(x=Day,y=value, colour = factor(date_col), group=date_col)) +
  geom_line(size=1.5) +
  geom_point(size=1.5) +
  theme_light() +
  theme(axis.text  = element_text(size = 14),
        axis.title = element_text(size = 16))


