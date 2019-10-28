Exploring World Bank Data
================
Kejing Li

## Load necessary libraries

``` r
library(tidyverse)
```

    ## -- Attaching packages ---------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.2.1     v purrr   0.3.2
    ## v tibble  2.1.3     v dplyr   0.8.3
    ## v tidyr   1.0.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## -- Conflicts ------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

## Write a function to import the data files

``` r
read_and_tidy <- function(path) {
  data <- read_csv(path, skip = 4) %>%
    filter("Indicator Code" %in% c( "IC.FRM.FEMO.ZS",
                                    "IC.FRM.FEMM.ZS",
                                    "IC.REG.PROC.FE",
                                    "TX.VAL.TECH.MF.ZS") ) %>%
    pivot_longer(-c("Country Name",
                    "Country Code", 
                    "Indicator Name", 
                    "Indicator Code"),
                 names_to = "Year",
                 values_to = "Value") %>%
    pivot_wider(names_from = "Indicator Code",
                values_from = "Value")
  
  return(data)     
}
```

## Import the data

``` r
directory <- dir(path = "data_world_bank", full.names = TRUE)
data <- vector("list", length(directory))
for (i in seq_along(directory)){
  data[[i]] <-  read_and_tidy(directory[[1]])
}
```

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )

    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: Missing column names filled in: 'X64' [64]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   X64 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

``` r
bind_rows(data)
```

    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>

## Explore the data

``` r
data
```

    ## [[1]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[2]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[3]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[4]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[5]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[6]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[7]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[8]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[9]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[10]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[11]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[12]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[13]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[14]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[15]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[16]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[17]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[18]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[19]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[20]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[21]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[22]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[23]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[24]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[25]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[26]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[27]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[28]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[29]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[30]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[31]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[32]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[33]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[34]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[35]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[36]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[37]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[38]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[39]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[40]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[41]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[42]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[43]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[44]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[45]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[46]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[47]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[48]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[49]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[50]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[51]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[52]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[53]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[54]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[55]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[56]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[57]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[58]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[59]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[60]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[61]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[62]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[63]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[64]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[65]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[66]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[67]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[68]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[69]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[70]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[71]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[72]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[73]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[74]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[75]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[76]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[77]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[78]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[79]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[80]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[81]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[82]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[83]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[84]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[85]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[86]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[87]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[88]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[89]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[90]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[91]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[92]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[93]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[94]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[95]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[96]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[97]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[98]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[99]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[100]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[101]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[102]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[103]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[104]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[105]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[106]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[107]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[108]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[109]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[110]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[111]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[112]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[113]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[114]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[115]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[116]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[117]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[118]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[119]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[120]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[121]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[122]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[123]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[124]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[125]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[126]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[127]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[128]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[129]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[130]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[131]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[132]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[133]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[134]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[135]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[136]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[137]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[138]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[139]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[140]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[141]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[142]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[143]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[144]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[145]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[146]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[147]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[148]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[149]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[150]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[151]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[152]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[153]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[154]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[155]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[156]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[157]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[158]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[159]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[160]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[161]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[162]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[163]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[164]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[165]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[166]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[167]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[168]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[169]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[170]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[171]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[172]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[173]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[174]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[175]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[176]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[177]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[178]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[179]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[180]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[181]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[182]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[183]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[184]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[185]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[186]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[187]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[188]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[189]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[190]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[191]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[192]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[193]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[194]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[195]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[196]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[197]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[198]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[199]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[200]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[201]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[202]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[203]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[204]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[205]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[206]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[207]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[208]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[209]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[210]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[211]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[212]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[213]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[214]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[215]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[216]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[217]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[218]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[219]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[220]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[221]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[222]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[223]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[224]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[225]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[226]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[227]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[228]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[229]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[230]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[231]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[232]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[233]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[234]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[235]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[236]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[237]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[238]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[239]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[240]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[241]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[242]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[243]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[244]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[245]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[246]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[247]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[248]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[249]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[250]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[251]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[252]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[253]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[254]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[255]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[256]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[257]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[258]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[259]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[260]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[261]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[262]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[263]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>
    ## 
    ## [[264]]
    ## # A tibble: 0 x 5
    ## # ... with 5 variables: `Country Name` <chr>, `Country Code` <chr>,
    ## #   `Indicator Name` <chr>, Year <chr>, Value <dbl>

## Session info

``` r
devtools::session_info()
```

    ## - Session info ----------------------------------------------------------
    ##  setting  value                       
    ##  version  R version 3.6.1 (2019-07-05)
    ##  os       Windows 10 x64              
    ##  system   x86_64, mingw32             
    ##  ui       RTerm                       
    ##  language (EN)                        
    ##  collate  English_United States.1252  
    ##  ctype    English_United States.1252  
    ##  tz       America/Chicago             
    ##  date     2019-10-27                  
    ## 
    ## - Packages --------------------------------------------------------------
    ##  package     * version date       lib source        
    ##  assertthat    0.2.1   2019-03-21 [1] CRAN (R 3.6.1)
    ##  backports     1.1.5   2019-10-02 [1] CRAN (R 3.6.1)
    ##  broom         0.5.2   2019-04-07 [1] CRAN (R 3.6.1)
    ##  callr         3.3.2   2019-09-22 [1] CRAN (R 3.6.1)
    ##  cellranger    1.1.0   2016-07-27 [1] CRAN (R 3.6.1)
    ##  cli           1.1.0   2019-03-19 [1] CRAN (R 3.6.1)
    ##  colorspace    1.4-1   2019-03-18 [1] CRAN (R 3.6.1)
    ##  crayon        1.3.4   2017-09-16 [1] CRAN (R 3.6.1)
    ##  desc          1.2.0   2018-05-01 [1] CRAN (R 3.6.1)
    ##  devtools      2.2.1   2019-09-24 [1] CRAN (R 3.6.1)
    ##  digest        0.6.21  2019-09-20 [1] CRAN (R 3.6.1)
    ##  dplyr       * 0.8.3   2019-07-04 [1] CRAN (R 3.6.1)
    ##  ellipsis      0.3.0   2019-09-20 [1] CRAN (R 3.6.1)
    ##  evaluate      0.14    2019-05-28 [1] CRAN (R 3.6.1)
    ##  fansi         0.4.0   2018-10-05 [1] CRAN (R 3.6.1)
    ##  forcats     * 0.4.0   2019-02-17 [1] CRAN (R 3.6.1)
    ##  fs            1.3.1   2019-05-06 [1] CRAN (R 3.6.1)
    ##  generics      0.0.2   2018-11-29 [1] CRAN (R 3.6.1)
    ##  ggplot2     * 3.2.1   2019-08-10 [1] CRAN (R 3.6.1)
    ##  glue          1.3.1   2019-03-12 [1] CRAN (R 3.6.1)
    ##  gtable        0.3.0   2019-03-25 [1] CRAN (R 3.6.1)
    ##  haven         2.1.1   2019-07-04 [1] CRAN (R 3.6.1)
    ##  hms           0.5.1   2019-08-23 [1] CRAN (R 3.6.1)
    ##  htmltools     0.3.6   2017-04-28 [1] CRAN (R 3.6.1)
    ##  httr          1.4.1   2019-08-05 [1] CRAN (R 3.6.1)
    ##  jsonlite      1.6     2018-12-07 [1] CRAN (R 3.6.1)
    ##  knitr         1.25    2019-09-18 [1] CRAN (R 3.6.1)
    ##  lattice       0.20-38 2018-11-04 [1] CRAN (R 3.6.1)
    ##  lazyeval      0.2.2   2019-03-15 [1] CRAN (R 3.6.1)
    ##  lifecycle     0.1.0   2019-08-01 [1] CRAN (R 3.6.1)
    ##  lubridate     1.7.4   2018-04-11 [1] CRAN (R 3.6.1)
    ##  magrittr      1.5     2014-11-22 [1] CRAN (R 3.6.1)
    ##  memoise       1.1.0   2017-04-21 [1] CRAN (R 3.6.1)
    ##  modelr        0.1.5   2019-08-08 [1] CRAN (R 3.6.1)
    ##  munsell       0.5.0   2018-06-12 [1] CRAN (R 3.6.1)
    ##  nlme          3.1-140 2019-05-12 [1] CRAN (R 3.6.1)
    ##  pillar        1.4.2   2019-06-29 [1] CRAN (R 3.6.1)
    ##  pkgbuild      1.0.5   2019-08-26 [1] CRAN (R 3.6.1)
    ##  pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 3.6.1)
    ##  pkgload       1.0.2   2018-10-29 [1] CRAN (R 3.6.1)
    ##  prettyunits   1.0.2   2015-07-13 [1] CRAN (R 3.6.1)
    ##  processx      3.4.1   2019-07-18 [1] CRAN (R 3.6.1)
    ##  ps            1.3.0   2018-12-21 [1] CRAN (R 3.6.1)
    ##  purrr       * 0.3.2   2019-03-15 [1] CRAN (R 3.6.1)
    ##  R6            2.4.0   2019-02-14 [1] CRAN (R 3.6.1)
    ##  Rcpp          1.0.2   2019-07-25 [1] CRAN (R 3.6.1)
    ##  readr       * 1.3.1   2018-12-21 [1] CRAN (R 3.6.1)
    ##  readxl        1.3.1   2019-03-13 [1] CRAN (R 3.6.1)
    ##  remotes       2.1.0   2019-06-24 [1] CRAN (R 3.6.1)
    ##  rlang         0.4.0   2019-06-25 [1] CRAN (R 3.6.1)
    ##  rmarkdown     1.16    2019-10-01 [1] CRAN (R 3.6.1)
    ##  rprojroot     1.3-2   2018-01-03 [1] CRAN (R 3.6.1)
    ##  rstudioapi    0.10    2019-03-19 [1] CRAN (R 3.6.1)
    ##  rvest         0.3.4   2019-05-15 [1] CRAN (R 3.6.1)
    ##  scales        1.0.0   2018-08-09 [1] CRAN (R 3.6.1)
    ##  sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 3.6.1)
    ##  stringi       1.4.3   2019-03-12 [1] CRAN (R 3.6.0)
    ##  stringr     * 1.4.0   2019-02-10 [1] CRAN (R 3.6.1)
    ##  testthat      2.2.1   2019-07-25 [1] CRAN (R 3.6.1)
    ##  tibble      * 2.1.3   2019-06-06 [1] CRAN (R 3.6.1)
    ##  tidyr       * 1.0.0   2019-09-11 [1] CRAN (R 3.6.1)
    ##  tidyselect    0.2.5   2018-10-11 [1] CRAN (R 3.6.1)
    ##  tidyverse   * 1.2.1   2017-11-14 [1] CRAN (R 3.6.1)
    ##  usethis       1.5.1   2019-07-04 [1] CRAN (R 3.6.1)
    ##  utf8          1.1.4   2018-05-24 [1] CRAN (R 3.6.1)
    ##  vctrs         0.2.0   2019-07-05 [1] CRAN (R 3.6.1)
    ##  withr         2.1.2   2018-03-15 [1] CRAN (R 3.6.1)
    ##  xfun          0.10    2019-10-01 [1] CRAN (R 3.6.1)
    ##  xml2          1.2.2   2019-08-09 [1] CRAN (R 3.6.1)
    ##  yaml          2.2.0   2018-07-25 [1] CRAN (R 3.6.0)
    ##  zeallot       0.1.0   2018-01-28 [1] CRAN (R 3.6.1)
    ## 
    ## [1] D:/Tools/R-3.6.1/library
