#' This function will perform a dynamic inoperability input-output analysis on an economy
#' 
#' @param totaloutput The total output vector, most easily collected from the **draw_io_data** function (io_data$TotalOutput)
#' @param finaldemand The final demand vector, most easily collected from the **draw_io_data** function (io_data$FinalDemand)
#' @param interindustry The inter-industry matrix, most easily collected from the **draw_io_data** function (io_data$InterIndustry)
#' @param industrylist The industry list, most easily collected from the **draw_io_data** function (io_data$Industries)
#' @param degradation The degradation vector, most easily collected from the **degrade_industries** function (degradation$Degradation_Proportion)
#' @param degradationtype The degradation approach ("Propertion" or "Absolute")
#' @param type The IO analysis type where type 1 is not including households as a sector and type 2 (not yet available) where households are included as a sector
#' @param estimated_recoveryduration The number of periods expected to generate a recovery from the inital to final inoperability measures
#' @param estimated_initialinoperability The initial inoperability assumed as part of the recovery
#' @param estimated_finalinoperability The final inoperability assumed as part of the recovery (typically very, very small - like 0.001)
#' @param analysisperiod The number of periods over which the DIIM should be conducted
#' @returns A dataframe including the inoperability and total economic loss for each sector in each time period after the shock

diim <- function(
                totaloutput,
                finaldemand,
                interindustry,
                industrylist,
                degradation,
                degradationtype = "Proportion",
                type = 1,
                estimated_recoveryduration,
                estimated_initialinoperability,
                estimated_finalinoperability,
                analysisperiod
) {
        size <- dim(io_data$InterIndustry)[1]
        
        # if (stringr::str_sub(outputpath, nchar(outputpath), nchar(outputpath)) != "/" & outputpath != F) {
        #         outputpath <- paste0(outputpath, "/")
        # }
        
        if (type == 1) {
                xhat <- diag(size) * matrix(totaloutput, nrow = size, ncol = size, byrow = T)
                xhatneg1 <- solve(xhat)
                A <- interindustry %*% xhatneg1
                IminusA <- diag(size) - A
                L <- solve(IminusA)
                
                if (degradationtype == "Proportion") {
                        ftilde_degp <- matrix(degradation, nrow = size, ncol = 1, byrow = T)
                        ftilde <- finaldemand * ftilde_degp
                } else if (degradationtype == "Absolute") {
                        ftilde <- matrix(degradation, nrow = size, ncol = 1, byrow = T)
                } else {
                        stop("Inappropriate degradation type provided.")
                }
                
                Astar <- (xhatneg1 %*% A) %*% xhat
                fstar <- xhatneg1 %*% ftilde
                IminusAstar <- diag(size) - Astar
                inoperability <- solve(IminusAstar) %*% fstar
                
                K <- (log(estimated_initialinoperability / estimated_finalinoperability) / estimated_recoveryduration) * diag(1 / (1 - Astar))
                K <- diag(size) * K
                k <- diag(K)
                
                for(looping_i in c(1:analysisperiod)) {
                        
                        if (looping_i == 1) {
                                
                                adj_inoperability <- inoperability
                                
                                dat_recovery <- data.frame(
                                        Industry = industrylist,
                                        Inoperability = 0,
                                        TimePeriod = 0
                                )
                                
                                tmp_recovery <- data.frame(
                                        Industry = industrylist,
                                        Inoperability = inoperability[,1],
                                        TimePeriod = looping_i
                                )
                                
                                dat_recovery <- rbind(
                                        dat_recovery,
                                        tmp_recovery
                                )
                                
                        } else {
                                
                                tmp_inoperability <- adj_inoperability + (K %*% ((Astar %*% adj_inoperability) - adj_inoperability))
                                
                                adj_inoperability <- tmp_inoperability
                                
                                tmp_recovery <- data.frame(
                                        Industry = industrylist,
                                        Inoperability = tmp_inoperability[,1],
                                        TimePeriod = looping_i
                                )
                                
                                dat_recovery <- rbind(
                                        dat_recovery,
                                        tmp_recovery
                                )
                                
                        }
                        
                }
                
                return(dat_recovery)
                
        }
        
}