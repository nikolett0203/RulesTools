library(arules)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(moments)
library(mice)

source("./plotting_funs.R")
source("./assoc_funs.R")
source("./ggvenn_custom.R")

#################### PREPROCESSING ####################

raw <- read.csv(file=file.choose())

# create new dataframe containing only variables of interest

sl <- raw %>%
  select(Station_ID, Detect, Temp, PH, Conductivity, 
         TDS, Distance_to_mouth..km., Width..m., Depth..m., Flow..ft3.s.)

sl_mean_fill <- sl %>%
  group_by(Station_ID) %>%
  mutate(across(.cols = c(Temp, PH, Conductivity, TDS, Distance_to_mouth..km., Width..m., Depth..m., Flow..ft3.s.),
                .fns = ~ifelse(is.na(.), mean(., na.rm=TRUE), .)))

sl_mean_fill <- ungroup(sl_mean_fill)

#################### PLOTTING ####################

# station <- bar(sl, Station_ID, "Station ID")
detection <- bar(sl_mean_fill, Detect, "Detection")
# potentially change efish scale
temp <- his(sl_mean_fill, Temp, "Water Temperature (°C)")
ph <- his(sl_mean_fill, PH, "pH")
tds <- his(sl_mean_fill, TDS, "Total Dissolved Solids")
cond <- his(sl_mean_fill, Conductivity, "Conductivity (µS)")
distance <- sl_mean_fill %>% ggplot(aes(x=Distance_to_mouth..km.)) +
  geom_histogram(bins=12, fill="cornflowerblue", colour = "black") + 
  labs(x = "Distance to Mouth (km)", y = "Frequency")
bar(sl_mean_fill, Distance_to_mouth..km., "Distance to Mouth (km)")
width <- his(sl_mean_fill, Width..m., "Width (m)")
depth <- his(sl_mean_fill, Depth..m., "Depth (m)")
flow <- his(sl_mean_fill, Flow..ft3.s., expression("Flow (ft"^2*"/s)"))

grid.arrange(detection, temp, ph, tds, cond, distance, width, depth, flow, ncol=2)

#################### STAT CALCS ####################

uni <- sl_mean_fill %>% 
  summarise(across(everything(), ~ n_distinct(.)))

print(uni)

cont_vars <- sl_mean_fill %>% 
  select(-Station_ID)

means <- cont_vars %>%
  summarise(across(everything(), mean)) 

meds <- cont_vars %>%
  summarise(across(everything(), median))

skew <- cont_vars %>%
  summarise(across(everything(), skewness)) 

summary <- bind_rows(means, meds, skew, uni)
print(summary)

#################### DISCRETISATION PREPROCESSING ####################

# idk what to do with this yet
discrete <- sl_mean_fill %>%
  select(-Station_ID)

#################### BIOLOGICALLY MOTIVATED DISCRETISATION ####################

# 19 for optimal temp of sea lamprey
# 6.5 - 9 pH, 7.75 avg
# TDS ranges from 50-250 in streams, avg 150

# create dataframe of cutoff points
bio <- data.frame(
  Temp = 19,
  PH = 7.75, 
  Conductivity = 206.6,
  TDS = 150,
  Distance_to_mouth..km. = 9.504983, 
  Width..m. = 15, 
  Depth..m. = 0.7, 
  Flow..ft3.s. = 20.52378)

# create dataframe to store 
bio_df <- data.frame(matrix(nrow = nrow(sl_mean_fill), ncol = 0))
bio_df$Detect <- sl_mean_fill$Detect  
bio_df$Detect <- as.factor(bio_df$Detect)


bio_df <- dtize(discrete, bio, bio_df)

tbio <- as(bio_df, "transactions")  
  
itemFrequencyPlot(tbio, support = 1/162, cex.names=0.8, main = "Item Frequency Plot (Biological Discretisation)")

bio_rules <- apriori(tbio, parameter = list(support=1/162, confidence = 1/162),
                     appearance = list(rhs="Detect=1"))

bio_rules <- bio_rules %>%
  subset(!is.redundant(., measure = "confidence")) %>%
  subset(is.significant(., alpha = 0.05)) %>%
  sort(by = c("confidence", "lift", "support"))

write(bio_rules, file = "sealamp_rules.csv", sep = ",", col.names = NA)

#################### IMPUTING VIA MICE ####################

md.pattern(sl)
imp <- mice(sl, seed=123)
cimp <- complete(imp)

#################### PLOTTING ####################

# station <- bar(sl, Station_ID, "Station ID")
detection <- bar(cimp, Detect, "Detection")
# potentially change efish scale
temp <- his(cimp, Temp, "Water Temperature (°C)")
ph <- his(cimp, PH, "pH")
tds <- his(cimp, TDS, "Total Dissolved Solids")
cond <- his(cimp, Conductivity, "Conductivity (µS)")
distance <- cimp %>% ggplot(aes(x=Distance_to_mouth..km.)) +
  geom_histogram(bins=12, fill="cornflowerblue", colour = "black") + 
  labs(x = "Distance to Mouth (km)", y = "Frequency")
bar(cimp, Distance_to_mouth..km., "Distance to Mouth (km)")
width <- his(cimp, Width..m., "Width (m)")
depth <- his(cimp, Depth..m., "Depth (m)")
flow <- his(cimp, Flow..ft3.s., expression("Flow (ft"^2*"/s)"))

grid.arrange(detection, temp, ph, tds, cond, distance, width, depth, flow, ncol=2)


uni <- cimp %>% 
  summarise(across(everything(), ~ n_distinct(.)))

print(uni)

cont_vars <- cimp %>% 
  select(-Station_ID)

means <- cont_vars %>%
  summarise(across(everything(), mean)) 

meds <- cont_vars %>%
  summarise(across(everything(), median))

skew <- cont_vars %>%
  summarise(across(everything(), skewness)) 

summary <- bind_rows(means, meds, skew, uni)
print(summary)



discrete <- cimp %>%
  select(-Station_ID)

#################### BIOLOGICALLY MOTIVATED DISCRETISATION ####################

# 19 for optimal temp of sea lamprey
# 6.5 - 9 pH, 7.75 avg
# TDS ranges from 50-250 in streams, avg 150

# create dataframe of cutoff points
bio <- data.frame(
  Temp = 19,
  PH = 7.75, 
  Conductivity = 206.6,
  TDS = 150,
  Distance_to_mouth..km. = 9.504983, 
  Width..m. = 15, 
  Depth..m. = 0.7, 
  Flow..ft3.s. = 20.52378)

# create dataframe to store 
bio_df2 <- data.frame(matrix(nrow = nrow(cimp), ncol = 0))
bio_df2$Detect <- cimp$Detect  
bio_df2$Detect <- as.factor(bio_df2$Detect)


bio_df2 <- dtize(discrete, bio, bio_df2)

tbio2 <- as(bio_df2, "transactions")  

itemFrequencyPlot(tbio2, support = 1/162, cex.names=0.8, main = "Item Frequency Plot (Biological Discretisation)")

bio_rules2 <- apriori(tbio2, parameter = list(support=1/162, confidence = 1/162),
                     appearance = list(rhs="Detect=1"))

bio_rules2 <- bio_rules2 %>%
  subset(!is.redundant(., measure = "confidence")) %>%
  subset(is.significant(., alpha = 0.05)) %>%
  sort(by = c("confidence", "lift", "support"))

write(bio_rules2, file = "sealamp_rules2.csv", sep = ",", col.names = NA)

rule_by_rule(med=bio_rules, pmm=bio_rules2)

input <- extract_labels(med=bio_rules, pmm=bio_rules2)
length(input)
ggvenn_custom(input)
