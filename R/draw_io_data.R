#' This function will perform a mass collection of IO data from a particular system
#' 
#' @param data The filepath to the IO Excel file
#' @param full_range The cell range over which the full outputs from production for each sector are calculated
#' @param Z_range The range over which the Z matrix is collected - ie, the inter-industry data. Should be a square matrix
#' @param C_range The range over which the C matrix is collected - ie, the consumption data for each industry
#' @param I_range The range over which the I matrix is collected - ie, the investment data for each industry
#' @param G_range The range over which the G matrix is collected - ie, the government spending data for each industry
#' @param LAB_range The range over which the LAB matrix is collected
#' @param GS_range The range over which the GS matrix is collected
#' @param X_range The range over which the X matrix is collected - ie, the export data for each industry 
#' @param NRH_range The range over which the NRH matrix is collected - ie, the non-residential household consumption data for each industry
#' @param f_range The range over which the f matrix is collected - ie, the final demand for each industry
#' @param industries_range The range over which the industry matrix is collected - ie, the list of industries
#' @returns A large list of all the individual matrix elements required for an IO analysis

draw_io_data <- function(
                data,
                full_range,
                Z_range,
                C_range,
                I_range,
                G_range,
                LAB_range,
                GS_range,
                X_range,
                NRH_range,
                f_range,
                industries_range
) {
        raw <- collect_structures(data, full_range)
        raw_totaloutput <- combine_structures(raw)
        raw_interindustry <- raw[,Z_range] #c(1:26)
        raw_consumption_household <- raw[,C_range] #27
        raw_consumption_government <- raw[,G_range] #29
        raw_consumption_investment <- combine_structures(raw[,I_range]) #30+31+32
        raw_valadd_labourpayments <- collect_structures(data, LAB_range) #E31:AD31
        raw_valadd_grosssurplus <- combine_structures(collect_structures(data, GS_range), column = F) #E32:AD34
        raw_valadd_totallabour <- tibble::as_tibble(sum(raw_valadd_labourpayments[,c(1:length(raw_valadd_labourpayments))], na.rm = T))
        raw_exports <- raw[,X_range] #c(33:222)
        raw_consumption_nonresidenthousehold <- raw[,NRH_range] #28
        raw_finaldemand <- combine_structures(raw[,f_range]) #c(27:32)
        raw_industries <- collect_structures(data, industries_range, numeric = 0) #D5:D30
        
        Z <- as.matrix.data.frame(raw_interindustry)
        x <- as.matrix.data.frame(raw_totaloutput)
        C <- as.matrix.data.frame(raw_consumption_household)
        G <- as.matrix.data.frame(raw_consumption_government)
        I <- as.matrix.data.frame(raw_consumption_investment)
        LAB <- as.matrix.data.frame(raw_valadd_labourpayments)
        NRH <- as.matrix.data.frame(raw_consumption_nonresidenthousehold)
        X <- as.matrix.data.frame(raw_exports)
        Lx <- LAB / t(x)
        GVA <- (LAB + raw_valadd_grosssurplus) / t(x)
        sigLAB <- as.matrix.data.frame(raw_valadd_totallabour)
        f <- as.matrix.data.frame(raw_finaldemand)
        IND <- raw_industries$...1
        
        io_list <- list(
                InterIndustry = Z,
                TotalOutput = x,
                Consumption = C,
                Government = G,
                Investment = I,
                Labour = LAB,
                NonResidentHouseholds = NRH,
                Exports = X,
                Lx = Lx,
                GrossValueAdded = GVA,
                TotalLabour = sigLAB,
                FinalDemand = f,
                Industries = IND
        )
        
        return(io_list)
}