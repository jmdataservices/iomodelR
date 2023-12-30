#' This function ultimately will collect data from an IO structure
#' 
#' @param file The root IO file
#' @param range The Excel range over which the data are held
#' @param numeric This determines whether the data are number, automatically set to TRUE/1
#' @returns A dataframe of data collected from the Excel file

collect_structures <- function(
        file,
        range,
        numeric = 1
) {
        raw_ovr <- readxl::read_excel(
                path = file,
                range = range,
                col_names = F,
                na = "0"
        )
        
        if (numeric == 1) {
                raw_ovr <- raw_ovr %>% 
                        dplyr::mutate_all(
                                ~ as.numeric(tidyr::replace_na(.x, 0))
                        )
        } else if (numeric == 0) {
                raw_ovr <- raw_ovr %>% 
                        dplyr::mutate_all(
                                ~ tidyr::replace_na(.x, "")
                        )
        }
        
        return(raw_ovr)
}

#' This function will combine previously collected data structures to ensure only a single column
#' 
#' @param data A dataframe that should be summed together to create a singular vector
#' @param column A boolean determining whether the data should be aggregated into a single column (or alternatively, row)
#' @returns A singular column or row of totals, based on the inputs

combine_structures <- function(
        data,
        column = T
) {
        if(column == T) {
                singlecolumn <- data %>% 
                        apply(
                                MARGIN = 1,
                                FUN = sum,
                                na.rm = T
                        ) %>% 
                        tibble::as_tibble()
                
                return(singlecolumn)
        } else if(column == F) {
                singlerow <- data %>% 
                        apply(
                                MARGIN = 2,
                                FUN = sum,
                                na.rm = T
                        ) %>% 
                        tibble::as_tibble()
                
                return(singlerow)
        }
}
