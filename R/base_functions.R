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
