#' This function ultimately will collect data from an IO structure
#' 
#' @name collect_structures
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