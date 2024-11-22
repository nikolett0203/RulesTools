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