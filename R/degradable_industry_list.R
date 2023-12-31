#' This function will provide an output list of industries that can be used in the coming analysis. This should be used to help determine where different shocks should/can be applied.
#' 
#' @param io_data An io_data list object, generated as the output of the **draw_io_data** function
#' @returns A list of the industries involved in the analysis

degradable_industry_list <- function(
                io_data
) {
        print(io_data$Industries)
}