rm(list = ls())
library(magrittr)
library(tidyr)
library(stringr)

source('R/base_functions.R')
source('R/collection_functions.R')
source('R/execution_functions.R')

io_data <- draw_io_data(
        data = 'data/testData.xlsx',
        full_range = 'E5:HR30',
        Z_range = c(1:26),
        C_range = 27,
        I_range = 29,
        G_range = c(30:32),
        LAB_range = 'E31:AD31',
        GS_range = 'E32:AD34',
        X_range = c(33:222),
        NRH_range = 28,
        f_range = c(27:32),
        industries_range = 'D5:D30'
)

degradation_vector <- c(rep(0, 20), 0.5, rep(0, 5))

dat <- iim(
        totaloutput = io_data$TotalOutput,
        finaldemand = io_data$FinalDemand,
        interindustry = io_data$InterIndustry,
        industrylist = io_data$Industries,
        degradation = degradation_vector
)

dat


