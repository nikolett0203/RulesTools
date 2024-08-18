library(arules)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(moments)

source("./plotting_funs.R")
source("./assoc_funs.R")
source("./ggvenn_custom.R")

#################### PREPROCESSING ####################

raw <- read.csv(file=file.choose())

# filter looks in Site vector, checks if any of those terms are in it
# mutate creates new columns
raw <- raw %>% 
  filter(qPCRMethod=="MIC",
         !Site %in% c("NTC", "PC", "IPC")) %>%
  mutate(logConc = (IPCGreen - 40.44) / -3.733,
         eDNAConc = ifelse(IPCGreen == 0, 0, 10^logConc))

# isolate Hanlon September — other data has n/a values but we want complete data
# 126 samples in total
hsept <- raw %>%
  filter(Creek == "Hanlon", Month == "September")

# create new dataframe containing only variables of interest
hsept <- hsept %>%
  select(BackpackUsed, Site, eFish, AirTemp, WaterTemp, 
         pH, DO_mgL, conductivity_mS, volume_filtered, eDNAConc)

#################### PLOTTING ####################

bpused <- bar(hsept, BackpackUsed, "Backpack Used")
sites <- bar(hsept, Site, "Site")
# potentially change efish scale
efish <- bar(hsept, eFish, "Electrofish Catch")
atemp <- his(hsept, AirTemp, "Air Temperature (°C)")
wtemp <- his(hsept, WaterTemp, "Water Temperature (°C)")
ph <- his(hsept, pH, "pH")
do <- his(hsept, DO_mgL, "Dissolved Oxygen (mg/L)")
cond <- his(hsept, conductivity_mS, "Conductivity (µS)")
edna <- his(hsept, eDNAConc, "eDNA Concentration (Copies/µL)")
vol <- his(hsept, volume_filtered, "Volume Filtered (mL)")

grid.arrange(bpused, sites, efish, atemp, wtemp, ph, do, cond, edna, vol, ncol=2)

#################### STAT CALCS ####################

uni <- hsept %>% 
  summarise(across(everything(), n_distinct))

cont_vars <- hsept %>% 
  select(-BackpackUsed, -Site)

means <- cont_vars %>%
  summarise(across(everything(), mean)) 

meds <- cont_vars %>%
  summarise(across(everything(), median))

skew <- cont_vars %>%
  summarise(across(everything(), skewness)) 

summary <- bind_rows(means, meds, skew, uni)
print(summary)

#################### MAKING CORRELATION PLOTS ####################

labels = c("Air Temperature (°C)", "Water Temperature (°C)", "pH", "Dissolved Oxygen (mg/L)", "Conductivity (mS/cm)", "Volume Filtered (L)")
variables <- c("AirTemp", "WaterTemp", "pH", "DO_mgL", "conductivity_mS", "volume_filtered")

# seq_along is a function that generates a sequence of integers from 1 to the length of its argument
# used for forloops
plot_list <- list()

for (i in seq_along(variables)) {
  var <- variables[i]
  lab <- labels[i]
  p <- scatter(hsept, var, "eDNAConc", lab, "eDNA Concentration (copies/µL)")
  plot_list[[i]] <- p
}

plotss <- grid.arrange(grobs = plot_list, ncol = 2, nrow = 3)
print(plotss)
ggsave(filename = "corr.png", plot = plotss, dpi = 300)


#################### DISCRETISATION PREPROCESSING ####################

# site and backpack used must be factorised to work
hsept$Site <- as.factor(hsept$Site)
hsept$BackpackUsed <- as.factor(hsept$BackpackUsed) 

#################### DISCRETISATION BY MEDIAN ####################

# create new df with enough rows to hold all hsept data
med_df <- data.frame(matrix(nrow = nrow(hsept), ncol = 0))

# factorise site and bp
med_df$Site <- hsept$Site  
med_df$BackpackUsed <- hsept$BackpackUsed 

# apply median discretisation to remaining values
med_df <- dtize(hsept, meds, med_df)

# transform to transactional dataset
tmed <- as(med_df, "transactions")

# visualise
itemFrequencyPlot(tmed, support = 1/126, cex.names=0.8, main = "Item Frequency Plot (Median Discretisation)")

# let's mine
med_rules <- apriori(tmed, parameter = list(support=1/126, confidence = 1/126),
                     appearance = list(rhs="eDNAConc=high"))

# remove redundancies, statistically insignif, and then sort
med_rules <-med_rules %>%
  subset(!is.redundant(., measure = "confidence")) %>%
  subset(is.significant(., alpha = 0.05)) %>%
  sort(by = c("confidence", "lift", "support"))

write(med_rules, file = "med_rules.csv", sep = ",", col.names = NA)

#################### DISCRETISATION BY MEAN ####################

mean_df <- data.frame(matrix(nrow = nrow(hsept), ncol = 0))

mean_df$Site <- hsept$Site  
mean_df$BackpackUsed <- hsept$BackpackUsed 

mean_df <- dtize(hsept, means, mean_df)

tmean <- as(mean_df, "transactions")

itemFrequencyPlot(tmean, support = 1/126, cex.names=0.8, main = "Item Frequency Plot (Mean Discretisation)")

mean_rules <- apriori(tmean, parameter = list(support=1/126, confidence = 1/126),
                      appearance = list(rhs="eDNAConc=high"))

mean_rules <- mean_rules %>%
  subset(!is.redundant(., measure = "confidence")) %>%
  subset(is.significant(., alpha = 0.05)) %>%
  sort(by = c("confidence", "lift", "support"))

write(mean_rules, file = "mean_rules.csv", sep = ",", col.names = NA)

#################### OG DISCRETISATION ####################

og_df <- data.frame(matrix(nrow = nrow(hsept), ncol = 0))

og_df$Site <- hsept$Site  
og_df$BackpackUsed <- hsept$BackpackUsed

# gotta restore the original cutoff points
og <- meds
og$eDNAConc <- 13.3

# discretise efish separately
og$eFish <- NULL
og_df$eFish <- discretize(hsept$eFish, method="fixed", breaks=c(-Inf, 0, Inf), right=TRUE, labels = c("absent", "present"))

og_df <- dtize(hsept, og, og_df)

# mining
tog <- as(og_df, "transactions")

itemFrequencyPlot(tog, support = 1/126, cex.names=0.8, main = "Item Frequency Plot (Original Discretisation)")

og_rules <- apriori(tog, parameter = list(support=1/126, confidence = 1/126),
                    appearance = list(rhs="eDNAConc=high"))

og_rules <- og_rules %>%
  subset(!is.redundant(., measure = "confidence")) %>%
  subset(is.significant(., alpha = 0.05)) %>%
  sort(by = c("confidence", "lift", "support"))

write(og_rules, file = "og_rules.csv", sep = ",", col.names = NA)

#################### BIOLOGICALLY MOTIVATED DISCRETISATION ####################

# 15 for optimal temp
# 6.5-9.5 mg/L for DO, 8 is mean
# 6.5 - 9 pH, 7.75 avg
# conductivity
# air temperature between 10-22C, https://www.theweathernetwork.com/ca/historical/ontario/guelph

# create dataframe of cutoff points
bio <- data.frame(
  AirTemp = 16,
  WaterTemp = 25,
  pH = 7.75,
  DO_mgL = 8,
  conductivity_mS = 1047,
  volume_filtered = 1.04,
  eDNAConc = 13.3
)

# create dataframe to store 
bio_df <- data.frame(matrix(nrow = nrow(hsept), ncol = 0))
bio_df$Site <- hsept$Site  
bio_df$BackpackUsed <- hsept$BackpackUsed

# discretise efish separately
bio_df$eFish <- discretize(hsept$eFish, method="fixed", breaks=c(-Inf, 0, Inf), right=TRUE, labels = c("absent", "present"))

# then handle rest of the variables
bio_df <- dtize(hsept, bio, bio_df)

# mining
tbio <- as(bio_df, "transactions")

itemFrequencyPlot(tbio, support = 1/126, cex.names=0.8, main = "Item Frequency Plot (Biological Discretisation)")

bio_rules <- apriori(tbio, parameter = list(support=1/126, confidence = 1/126),
                     appearance = list(rhs="eDNAConc=high"))

bio_rules <- bio_rules %>%
  subset(!is.redundant(., measure = "confidence")) %>%
  subset(is.significant(., alpha = 0.05)) %>%
  sort(by = c("confidence", "lift", "support"))

write(bio_rules, file = "bio_rules.csv", sep = ",", col.names = NA)


#################### COMPARISONS ####################

rule_by_rule(Original=og_rules, Bio=bio_rules)

#################### VENN DIAGRAMS ####################

input <- extract_labels(bio=bio_rules, og=og_rules, mean=mean_rules, med=med_rules)
length(input)
ggvenn_custom(input)


##### compare discretisation of first mined, redundant, and insignif rules
##### profiling, microbenchmark
##### arulesviz
##### hadley wickham, R for datascience, R visualisation
