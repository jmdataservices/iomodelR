base_collect_structures <- function(
        file,
        range
) {
        raw_ovr <- readxl::read_excel(
                path = file,
                range = range,
                col_names = F,
                na = "0"
        )
        
        return(raw_ovr)
}

base_define_totaloutput <- function(
        data
) {
        totaloutput <- data %>% 
                apply(
                        MARGIN = 1,
                        FUN = sum,
                        na.rm = T
                ) %>% 
                tibble::as_tibble()
        
        return(totaloutput)
}

dat <- base_collect_structures(file = "data/testData.xlsx", range = "E5:HR30")

dat_totaloutput <- base_define_totaloutput(data = dat)







