#' This function will allow the creation of a dataframe that will be used as an input for the iim/diim functions (or similar). Essentially, it will show a list of industries and the applicable shocks to final demand.
#' 
#' @param io_data An io_data list object, generated as the output of the **draw_io_data** function
#' @param degraded_industry_single_name The name of an industry to be shocked as part of the IO analysis
#' @param degraded_industry_single_shock A proportion of final demand to be shocked as part of the IO analysis
#' @param degraded_industry_dataframe Where more than one industry should experience a shock (for example, when analysing a natural disaster), a dataframe of shocks can be passed here
#' @returns A dataframe of degraded final demand where the Degradation_Proportion or Degradation_Absolute columns can be used in the respective IO analyses

degrade_industries <- function(
                io_data,
                degraded_industry_single_name,
                degraded_industry_single_shock = 0.1,
                degraded_industry_dataframe = NULL
) {
        if (!hasArg(degraded_industry_single_name) & (!hasArg(degraded_industry_dataframe) | is.null(degraded_industry_dataframe))) {
                stop("No argument given for degraded industry name(s)")
        }
        
        if (!hasArg(degraded_industry_dataframe) | !is.data.frame(degraded_industry_dataframe)) {
                if(is.na(match(degraded_industry_single_name, io_data$Industries))) {
                        stop("Industry provided is not part of IO data")
                }
                degradation_vector <- data.frame(
                        Industry = io_data$Industries,
                        Degradation_Proportion = rep(0, length(io_data$Industries))
                ) %>% 
                        dplyr::mutate(
                                Degradation_Proportion = ifelse(Industry == degraded_industry_single_name, degraded_industry_single_shock, Degradation_Proportion),
                                Degradation_Absolute = Degradation_Proportion * io_data$FinalDemand[,1]
                        )
                return(degradation_vector)
        } else {
                if(!is.data.frame(degraded_industry_dataframe)) {
                        stop("Argument for of degraded_industry_dataframe is not a data frame object")
                }
                tmp_df <- degraded_industry_dataframe
                colnames(tmp_df) <- c("Industry", "Degradation_Proportion")
                degradation_vector <- data.frame(
                        Industry = io_data$Industries
                ) %>% 
                        dplyr::left_join(
                                by = "Industry",
                                tmp_df
                        ) %>% 
                        dplyr::mutate(
                                Degradation_Proportion = ifelse(is.na(Degradation_Proportion), 0, Degradation_Proportion),
                                Degradation_Absolute = Degradation_Proportion * io_data$FinalDemand[,1]
                        )
                return(degradation_vector)
        }
}