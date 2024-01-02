![image](https://github.com/jmdataservices/iomodelR/blob/main/inst/assets/IOModellR-LogoWhite.png)

<p align="center">This repo is the home of iomodelR, an IO Modelling package for R. This package aims to simplify the process of performing Leontief Input-Output Analyses and provide a platform from which more complex variations on these analyses can be conducted. Usage instructions and guidance are provided below</p>

<br>
<hr>
<br>

# Overview

The **ioModellR** package aims to provide a single-package mechanism, driven by the *[tidyverse]*(https://www.tidyverse.org/), to allow users to complete Leontief Input-Output analyses with little to no additional effort in completing the individual steps. In doing so, the package aims to create a superior workflow for more advanced analyses - reducing the burden of complex looping or algorithms that enable more complex analyses.

<br>
<hr>
<br>

# Contribute

To contribute to this package, users must be familiar with the R language and best-practice with R package building and programming. Please see:

* CRAN [https://cran.r-project.org/index.html](https://cran.r-project.org/index.html)
* RStudio [https://posit.co/download/rstudio-desktop/](https://posit.co/download/rstudio-desktop/)

Additionally, the [GitHub page for the package](https://github.com/jmdataservices/iomodelR) is the single repository of information.

<br>
<hr>
<br>

# Getting Started

To begin with the iomodellR package, please download and install it using the *devtools* package in R:

```
install.packages("devtools")

devtools::install_git("https://github.com/jmdataservices/iomodelR")

library(iomodelR)
```

Now that the package is loaded, we can draw out the IO data from the appropriate source. As an example, the EORA National IO tables from the worldmrio database [https://worldmrio.com/countrywise/](https://worldmrio.com/countrywise/) can be used as an example:

```
io_data <- draw_io_data(
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
)
```

We can quickly and easily check the industries being included in the analysis using:

```
degradable_industry_list(io_data)
```

Once available, we can apply a particular shock to final demand for a given industry (a key step in all IO analyses). In this case, we'll apply a 15% reduction to final demand in the Fishing industry:

```
degradation <- degrade_industries(io_data, "Fishing", 0.15)
```

Now, we have the appropriate degradation vector, we can complete a standard inoperability input-output analysis using the *iim* function:

```
io_outcome <- iim(
        totaloutput = io_data$TotalOutput,
        finaldemand = io_data$FinalDemand,
        interindustry = io_data$InterIndustry,
        industrylist = io_data$Industries,
        degradation = degradation,
        degradationtype = "Proportion",
        type = 1
)
```

Now, the *io_outcome* object has the inoperability of each industry along with the total economic loss.

<br>
<hr>
<br>

# Guidance

The IO data can be consumed and prepared via the **draw_io_data** function.

There are a series of functionality options for different types of IO analysis and approaches:

* **Degradation of Final Demand**

  * **Degradation Vector**
  
  The degradation vector is created via the *degrade_industries* function.
  
  * **Stochastic Degradation Vector**
  
  The degradation vector created by the *degrade_industries* function can be amended to be a more stochastic measure using the *stochastic_degradation* function.
  
* **Input-Output Analyses**

  * **Inoperability Input-Output Analysis**
  
  The IIM analysis is available via the *iim* function.
  
  * **Dynamic Inoperability Input-Output Analysis**
  
  The DIIM analysis is available via the *diim* function.

<br>
<hr>
<br>

# Credits

This package has been built and maintained by Jamie McLaughlin (jamie@jmdataservices.co.uk). For any questions, please email the attached address.
