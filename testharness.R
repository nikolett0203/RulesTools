####################### TEST HARNESS ##########################

library(arules)
library(tidyverse)
library(testthat)

source("./assoc_funs.R")

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

################################################################

nonnumeric <- c("Zurich", "Budapest", "Vienna", "Frankfurt", "Prague")
navalues <- c(1, 2, 3, 4, 5, NA, 7, 8, 9, NA)
empty <- numeric(0)

test_that("dtize_col handles invalid column inputs correctly", {
  
  # test if function throws an error when column is not a vector
  expect_error(dtize_col(hsept), 
               regexp = "`column` must be a vector. Please provide a non-empty numeric vector.")
  
  # Test if the function throws an error when the column is not numeric
  expect_error(dtize_col(nonnumeric), 
               regexp = "`column` must be numeric. Please provide a non-empty numeric vector.")
  
  # Test if the function throws an error when the column is empty
  expect_error(dtize_col(empty), 
               regexp = "`column` is empty. Please provide a non-empty numeric vector.")
})

test_that("dtize_col handles invalid split types correctly", {
  
  #test if function throws an error when column is not a vector
  expect_error(dtize_col(hsept$pH, splits = "mode"), 
               regexp = "`splits` must be either `median`, `mean`, or a non-empty numeric vector.")
  
  expect_error(dtize_col(hsept$AirTemp, splits = nonnumeric), 
               regexp = "`splits` must be either `median`, `mean`, or a non-empty numeric vector.")

  expect_error(dtize_col(hsept$WaterTemp, splits = empty), 
               regexp = "`splits` must be either `median`, `mean`, or a non-empty numeric vector.")
  
  expect_error(dtize_col(hsept$pH, splits = hsept),          # not a vector
               regexp = "`splits` must be either `median`, `mean`, or a non-empty numeric vector.")  
  
  expect_error(dtize_col(hsept$pH, splits = NA),
               regexp = "`splits` must be either `median`, `mean`, or a non-empty numeric vector.")    

  expect_error(dtize_col(hsept$pH, splits = navalues),
               regexp = "`splits` cannot contain NA values.")  
  
})

valid_col = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

test_that("dtize_col handles boundaries correctly", {
  
  #right-closed finite upper boundary exceeded
  expect_error(dtize_col(valid_col, splits = c(1, 5, 9), right=TRUE, infinity=FALSE), 
               regexp = ("Values exceed the maximum split. Please ensure all values are within the defined range."))

  #left-closed finite upper boundary exceeded
  expect_error(dtize_col(valid_col, splits = c(1, 5, 10), right=FALSE, infinity=FALSE), 
               regexp = ("Values exceed the maximum split. Please ensure all values are within the defined range."))

  #right-closed finite lower boundary exceeded
  expect_error(dtize_col(valid_col, splits = c(1, 5, 11), right=TRUE, infinity=FALSE, lowest = FALSE), 
               regexp = ("Values fall below the minimum split. Please ensure all values are within the defined range."))
  
  #left-closed finite lower boundary exceeded
  expect_error(dtize_col(valid_col, splits = c(2, 5, 11), right=FALSE, infinity=FALSE, lowest = FALSE), 
               regexp = ("Values fall below the minimum split. Please ensure all values are within the defined range."))
  
  #right-closed + lowest included lower boundary exceeded
  expect_error(dtize_col(valid_col, splits = c(2, 5, 11), right=TRUE, infinity=FALSE, lowest = TRUE), 
               regexp = ("Values fall below the minimum split. Please ensure all values are within the defined range."))

  #left-closed + lowest included lower boundary exceeded
  expect_error(dtize_col(valid_col, splits = c(2, 5, 11), right=FALSE, infinity=FALSE, lowest = TRUE), 
               regexp = ("Values fall below the minimum split. Please ensure all values are within the defined range."))
  
})

