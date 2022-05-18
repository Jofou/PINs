# CREATE BOARD DAIRY ----

# 1.0 LIBRARY ----
library(pins)
library(tidyverse)

# 2.0 BOARD IMPORT ----
## * Create Board ----
board_import <- board_folder("Dairy/1 Import/", versioned = TRUE)
board_import

# Export Prepared Data
board_prepared <- board_folder("Dairy/2 Prepared/", versioned = TRUE)
board_prepared

## * Write Pins ----

board_prepared %>% pin_write(dairy_data, "dairy_data")


## * Read Pins Validation ----
data<-board_prepared %>%
  pin_read("dairy_data")

