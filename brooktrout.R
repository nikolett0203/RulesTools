library(arules)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(moments)

#################### PREPROCESSING ####################

raw <- read.csv(file=file.choose())

# filter looks in Site vector, checks if any of those terms are in it
# mutate creates new columns
raw <- raw %>% 
  filter(qPCRMethod=="MIC",
         !Site %in% c("NTC", "PC", "IPC")) %>%
  mutate(logConc = (IPCGreen - 40.44) / -3.733,
         eDNAConc = ifelse(IPCGreen == 0, 0, 10^logConc))

# isolate Hanlon September—other datasets have n/a values but we want complete data
# 126 samples in total
hsept <- raw %>%
  filter(Creek == "Hanlon", Month == "September")

# create new dataframe containing only variables of interest
hsept <- hsept %>%
  select(BackpackUsed, Site, eFish, AirTemp, WaterTemp, 
         pH, DO_mgL, conductivity_mS, volume_filtered, eDNAConc)

#################### PLOTTING ####################

bar <- function (data, xvar, xlab){
  plot <- data %>% ggplot(aes(x={{xvar}})) +
    geom_bar(fill = "cornflowerblue", colour = "black") +
    labs(x = xlab,
         y = "Count")
  return(plot)
}

his <- function(data, xvar, xlab){
  plot <- data %>% ggplot(aes(x={{xvar}})) +
    geom_histogram(fill = "cornflowerblue", colour = "black") +
    labs(x = xlab,
         y = "Frequency")
  return(plot)
}


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

means <- hsept %>% 
  select(-BackpackUsed, -Site) %>%
  summarise(across(everything(), mean)) 

meds <- hsept %>% 
  select(-BackpackUsed, -Site) %>%
  summarise(across(everything(), median))

skew <- hsept %>%
  select(-BackpackUsed, -Site) %>%
  summarise(across(everything(), skewness)) 

uni <- hsept %>% 
  summarise(across(everything(), n_distinct))

summary <- bind_rows(means, meds, skew, uni)
print(summary)

#################### DISCRETISATION PREPROCESSING ####################

# helper function
dtize <- function (data, split, new_df) {
  
  for(col in colnames(split)){
    cut <- c(-Inf, split[[col]], Inf)
    newcol <- discretize(discrete[[col]], method="fixed", breaks=cut, right=TRUE, labels = c("low", "high"))
    new_df[[col]] <- newcol
  }

  return(new_df)
}

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

#################### VENN DIAGRAMS ####################

ogitems <- as(tog, "list") %>% 
  unlist() %>% 
  unique()

meditems <- as(tmed, "list") %>%
  unlist() %>%
  unique()

meanitems <- as(tmean, "list") %>%
  unlist() %>%
  unique()

venn.plot <- venn.diagram(
  x = list("Original Discretisation" = ogitems,
           "Median Discretisation" = meditems,
           "Mean Discretisation" = meanitems),
  category.names = c("Original Discretisation", "Median Discretisation", "Mean Discretisation"),
  filename = NULL,
  output = TRUE
)

grid.draw(venn.plot)
dev.off()


##### compare discretisation of first mined, redundant, and insignif rules
##### profiling, microbenchmark
##### arulesviz
##### hadley wickham, R for datascience, R visualisation
