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

degradable_industry_list <- function(
        io_data
) {
        print(io_data$Industries)
}

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
