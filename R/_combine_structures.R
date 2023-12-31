#' This function will combine previously collected data structures to ensure only a single column
#' 
#' @name combine_structures
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