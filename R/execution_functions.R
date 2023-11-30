iim <- function(
                totaloutput,
                finaldemand,
                interindustry,
                industrylist,
                degradation,
                degradationtype = "Proportion",
                type = 1
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
                        
                }
                
                ftilde <- finaldemand * ftilde_degp
                Astar <- (xhatneg1 %*% A) %*% xhat
                fstar <- xhatneg1 %*% (finaldemand - ftilde)
                IminusAstar <- diag(size) - Astar
                inoperability <- solve(IminusAstar) %*% fstar
                
                return(
                        data.frame(
                                `Industry` = industrylist,
                                `Inoperability` = inoperability[,1],
                                `Total Output` = totaloutput[,1],
                                `Total Loss` = inoperability[,1] * totaloutput[,1]
                        )
                )
                
        }
        
}
