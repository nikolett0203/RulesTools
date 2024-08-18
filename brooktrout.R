library(arules)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(moments)
library(gplots)
library(grid)

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

# don't really need bar plots
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

# might be able to make this more efficient
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

#################### MAKING CORRELATION PLOTS ####################

labels = c("Air Temperature (°C)", "Water Temperature (°C)", "pH", "Dissolved Oxygen (mg/L)", "Conductivity (mS/cm)", "Volume Filtered (L)")
variables <- c("AirTemp", "WaterTemp", "pH", "DO_mgL", "conductivity_mS", "volume_filtered")

create_plot <- function(data, xV, yV, lab){
  ggplot(data, aes_string(x = xV, y= yV)) +
    geom_point(color = "#ebc349") +  # Set the points to white
    labs(x = lab,
         y = "eDNA Concentration (copies/µL)") +
    theme_minimal(base_size = 12) +
    theme(
      plot.background = element_rect(fill = "#4cbca6", color = NA),  # Background color
      panel.background = element_rect(fill = "#4cbca6", color = NA), # Panel background
      axis.title = element_text(color = "white"),  # Axis title color
      axis.text = element_text(color = "white"),   # Axis text color
      axis.line = element_line(color = "#14303f"),   # Axis line color
      axis.ticks = element_line(color = "white"),  # Axis ticks color
      plot.margin = unit(c(1, 1, 1, 1), "cm")
    )
}

# seq_along is a function that generates a sequence of integers from 1 to the length of its argument
# used for forloops
plot_list <- list()
for (i in seq_along(variables)) {
  var <- variables[i]
  lab <- labels[i]
  p <- create_plot(hsept, var, "eDNAConc", lab)
  plot_list[[i]] <- p
}

plotss <- grid.arrange(grobs = plot_list, ncol = 2, nrow = 3)
ggsave(filename = "corr.png", plot = plotss, dpi = 300)


#################### LAZY CHAT CODE ####################

create_plot <- function(data, xV, yV, lab){
  # Calculate correlation and p-value
  corr_test <- cor.test(data[[xV]], data[[yV]], method = "pearson")
  corr_coeff <- round(corr_test$estimate, 2)
  p_value <- round(corr_test$p.value, 3)
  
  # Create plot
  ggplot(data, aes_string(x = xV, y = yV)) +
    geom_point(color = "#000000") +  # Set the points to #ebc349
    labs(x = lab,
         y = "eDNA Concentration (copies/µL)") +
    theme_minimal(base_size = 12) +
    theme(
      plot.background = element_rect(fill = "#ebc349", color = NA),  # Background color
      panel.background = element_rect(fill = "#ebc349", color = NA), # Panel background
      axis.title = element_text(color = "#000000"),  # Axis title color
      axis.text = element_text(color = "#000000"),   # Axis text color
      axis.line = element_line(color = "#000000"),   # Axis line color
      axis.ticks = element_line(color = "000000"),  # Axis ticks color
      plot.margin = unit(c(1, 1, 1, 1), "cm")
    ) +
    annotate("text", x = Inf, y = Inf, label = paste0("r = ", corr_coeff, "\np = ", p_value),
             hjust = 1.1, vjust = 1.1, color = "#000000", size = 4)
}

# Generate the plots and combine them
plot_list <- list()
for (i in seq_along(variables)) {
  var <- variables[i]
  lab <- labels[i]
  p <- create_plot(hsept, var, "eDNAConc", lab)
  plot_list[[i]] <- p
}

# Combine the plots and save with high DPI
plotss <- grid.arrange(grobs = plot_list, ncol = 3, nrow = 2)
ggsave(filename = "corr.png", plot = plotss, dpi = 300)


#################### DISCRETISATION PREPROCESSING ####################

# helper function
dtize <- function (data, split, new_df) {
  
  for(col in colnames(split)){
    cut <- c(-Inf, split[[col]], Inf)
    newcol <- discretize(data[[col]], method="fixed", breaks=cut, right=TRUE, labels = c("low", "high"))
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


#################### COMPARISON FUNCTIONS ####################

# compare two rules
rule_by_rule <- function (rules1, rules2) {
  
  # add error checking, i.e. what if no rules are in common, valid rule objects
  
  # isolate the rules
  labels1 <- labels(rules1)
  labels2 <- labels(rules2)
  
  # find common rules
  common <- intersect(labels1, labels2)
  
  # collect interestingness measures for each rule from each dataset
  crules1 <- rules1[labels1 %in% common]
  crules2 <- rules2[labels2 %in% common]
  quality1 <- quality(crules1)
  quality2 <- quality(crules2) 
  
  # store in dataframe
  df <- data.frame(
    Rules = common,
    Support_1 = quality1$support,
    Support_2 = quality2$support,
    Confidence_1 = quality1$confidence,
    Confidence_2 = quality2$confidence,
    Lift_1 = quality1$lift,
    Lift_2 = quality2$lift
  )  
  
  print(sprintf("Number of rules in common: %d", length(common)))
  print(df)
}

rule_by_rule(med_rules, og_rules)


# compare indefinite number of rules
rule_compare <- function (...) {
  
  # collect arguments
  rules <- list(...)
  
  # check if all arguments are rules
  if(!all(sapply(rules, inherits, "rules"))) {
    stop("Arguments must be objects of class 'rules'")
  }
  
  # make sure user gives names
  if (is.null(names(rules)) || any(names(rules) == "")) {
    stop("Please provide names for all arguments.")
  }
  
  # isolate rules without interestingness data
  labels <- sapply(rules, labels)
  names <- names(rules)
  
  # find common rules by repeatedly intersecting rule lists together
  common <- Reduce(intersect, labels)
  
  # inform user if no common rules found
  # TEST THIS
  if (length(common) == 0) {
    print("No common rules found.")
    return(NULL)
  }
  
  # initialise dataframe
  df <- data.frame(Rules = common)

  # iterate over all rules and collect interestingness measures in df 
  for (i in seq_along(rules)) {
    
    # find common rules in original rule objects
    crules <- rules[[i]][labels[[i]] %in% common]
    # get the quality dataframe from a particular set
    quality <- quality(crules)
    
    # add support/conf/lift to df using user-inputted names for origin set
    df[paste0("Support_", names[i])] <- quality$support
    df[paste0("Confidence_", names[i])] <- quality$confidence
    df[paste0("Lift_", names[i])] <- quality$lift
  }
  
  print(sprintf("Number of rules in common: %d", length(common)))
  print(df)
  
}

rule_compare(Original=og_rules, Bio=bio_rules)

#################### VENN DIAGRAMS ####################



# REWRITE in combo with new comparison function
#vennrules <- function(rules, names){
  
  # check if sets = names
  # sets should be a list of sets
  
  # initialise empty list  
  # sets <- list()
  
  #for (i in seq_along(rules)) {
   # items <- labels(rules[[i]])
  #  sets[[names[i]]] <- items
 # }
#  diagram <- venn(sets)
  
#  print(diagram)
#}

#rules <- list(med_rules, bio_rules, og_rules)
#names <- c("Median", "Bio", "OG")
#vennrules(rules, names)



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
plot(bio_rules, interactive=TRUE)
plot(bio_rules, method="grouped")
plot(bio_rules, method="graph")
plot(bio_rules, method="paracoord")


##### hadley wickham, R for datascience, R visualisation
