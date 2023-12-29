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
                
                return(
                        data.frame(
                                Industry = industrylist,
                                Inoperability = inoperability[,1],
                                TotalOutput = totaloutput[,1],
                                TotalLoss = inoperability[,1] * totaloutput[,1]
                        )
                )
                
        }
        
}
