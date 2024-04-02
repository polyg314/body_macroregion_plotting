# body_macroregion_plotting
Rmarkdown script and updated CHOIRBM R package for Body Macroregion Plotting

## Getting Started 

### Installing CHOIRBM

1. Navigate to the CHOIRBM directory in R terminal. To do this, once you have opened body_macroregion_plotting.rmd, you can use: 

    ```r
    setwd('CHOIRBM')
    ```

2. Install CHOIRBM using the `devtools` package. If you haven't installed `devtools` yet, you can do so by running `install.packages("devtools")`. Then, run the following command to install CHOIRBM:

    ```r
    devtools::install()
    ```

3. In the body_macroregion_plotting.rmd file, the updated CHOIRBM packaged that will be loaded via:

    ```r
    library(CHOIRBM)
    ```

4. To verify that you have successfully installed and loaded the updated version of CHOIRBM, you can check the package version. The following command should print ‘0.0.2.9001’:

    ```r
    packageVersion("CHOIRBM")
    ```
*Note: you should restart R each time before installing the new CHOIRBM package to prevent any caching issues stopping the updated version from loading*

### Installing Other Necessary Packages for body_macroregion_plotting.rmd

#### Other necessary packages to run the body_macroregion_plotting.rmd file include those found in these libraries: 

    ```r
    library(readxl)
    library(dplyr)
    library(tidyr)
    library(stringr)
    library(gt)
    library(ggplot2)
    ```
#### These can be installed via command: 

    ```r
    install.packages(c("readxl", "dplyr", "tidyr", "stringr", "gt", "webshot2"))
    ```



