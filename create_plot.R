## in output file, type in \h+ and delete spaces to replace with , to get .txt file
## have (1) utility_output.txt for stacked area graph
## and (2) utility_wealths.txt for histogram plot
## call remove_duplicates_from_output.py [filename] on utility_output.txt
## input here to get visuals

library(ggplot2)
library(dplyr)

rm(list = ls())
setwd("/Users/michellezhang/Desktop/endowment_fund/output")

df = read.table("p3_allocations.txt", sep=",", header = FALSE)
df2 = read.table("p3_wealth.txt", header = FALSE)

names(df) = c("assets", "wealth", "fraction")
names(df2) = c("wealth")
df = df[c("wealth", "assets", "fraction")]
df = arrange(df, wealth)

# stacked area graph
ggplot(df, aes(x=wealth,y=fraction,group=assets,fill=assets)) + geom_area(position="fill") +
  labs(title="Stage 3 asset allocation - Power function")

# histogram
ggplot(df2, aes(x=wealth)) + 
  geom_histogram(binwidth=0.05, color="black", fill="blue") +
  labs(title="Stage 3 wealth - Power function")

