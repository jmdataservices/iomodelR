#' This function will take the degradation table created in the **degrade_industries** function and add random noise to the degradation values
#' 
#' @param degradation A degradation table output of the **degrade_industries** function
#' @param stochastic_modifier A stochastic model to be used to add random noise to the degradation: *currently only supports the Normal distribution (rnorm)*
#' @param stochastic_modifier_arg1 A parameter for the stochastic modifier
#' @param stochastic_modifier_arg2 A parameter for the stochastic modifier
#' @param stochastic_modifier_arg3 A parameter for the stochastic modifier
#' @returns An updated dataframe of the inital shocks to final demand, with the random noise added

stochastic_degradation <- function(
                degradation,
                stochastic_modifier = NULL,
                stochastic_modifier_arg1 = NULL,
                stochastic_modifier_arg2 = NULL,
                stochastic_modifier_arg3 = NULL
) {
        if (hasArg(stochastic_modifier) & !is.null(stochastic_modifier)) {
                if (stochastic_modifier == "rnorm" & !is.null(stochastic_modifier_arg1)) {
                        
                        degradation$Stochastic_Proportion <- stats::rnorm(
                                n = length(degradation$Degradation_Proportion),
                                mean = degradation$Degradation_Proportion,
                                sd = stochastic_modifier_arg1
                        ) * ceiling(degradation$Degradation_Proportion)
                        
                        degradation$Stochastic_Proportion <- pmax(
                                degradation$Stochastic_Proportion,
                                0.00001
                        ) * ceiling(degradation$Degradation_Proportion)
                        
                        degradation$Stochastic_Proportion <- pmin(
                                degradation$Stochastic_Proportion,
                                0.99999
                        ) * ceiling(degradation$Degradation_Proportion)
                        
                        degradation <- degradation %>% 
                                dplyr::mutate(
                                        Stochastic_Absolute = ifelse(Degradation_Proportion > 0, Degradation_Absolute * (Stochastic_Proportion / Degradation_Proportion), 0.0),
                                        Degradation_Proportion = as.numeric(Stochastic_Proportion),
                                        Degradation_Absolute = as.numeric(Stochastic_Absolute)
                                ) %>% 
                                dplyr::select(
                                        Industry,
                                        Degradation_Proportion,
                                        Degradation_Absolute
                                )
                }
                return(degradation)
        } else {
                stop("No stochastic modifier or associated arguments have been provided")
        }
}