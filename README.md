# **GreatBay_BoxModel**

## Table of Contents

  - [Background](#background)
  - [Install](#install)
  - [Usage](#usage)
  - [Contributing](#contributing)
  - [License](#license)

## Background

Purpose: A repository of data and scripts necessary to create a box-model for solute loading to Great Bay Estuary, NH/ME. This repository is associated with the M.S. Thesis titled, "Biogeochemical Stressors and Ecological Response in Great Bay Estuary, NH/ME" & the related paper in preparation for submission. Data was sourced from the UNH Water Quality Analysis Lab, the NH Department of Environmental Services, the U.S. Geological Survey, the Piscataqua Region Estuaries Partnership, and the Great Bay National Estuarine Research Reserve. 
 
## Install

This repository assumes that users have registered a GitHub account, installed R and RStudio, and installed Git. The following link provides installation instructions for installing Git and connecting RStudio and GitHub: https://happygitwithr.com/rstudio-git-github

Add this repository to your RStudio project space.

  1. Navigate to the main page of the repository.
  2. Click <> Code.
  3. Copy the HTTPS URL for the repository.
  4. Open the terminal shell or git bash.
  5. Change current working directory to the locationw here you want the cloned directory.
  6. Type "git clone", then paste the URL.
  7. Hit Enter. 

## Usage

This repository is organized into three main folders, with appropriate sub-folders.

### **data**

The data folder contain raw data products used to build the box model & manuscript figures.

#### emd
##### Contains water quality data for the head of tide river stations (05-LMP, 09-EXT, 02-WNC, GRBAP). Includes all site metadata, along with sample date, time, water quality data for each station. Each row is one water quality solute measured on a given day and site (long data). These data files were downloaded via request from the NH Environmental Monitoring Database (DES.EMD@des.nh.gov). The only modifications were to make the column names the first row of the spreadsheet and to delete the database query from the bottom of the spreadsheet. These files currently are up-to-date through the 2021 and/or 2022 calendar year. 

#### discharge
##### The daily mean discharge for the three rivers the drain to Great Bay. The csv files contain the site id, date, and mean daily discharge (units: cfs). The **text_files** subfolder has the .txt version of the .csv files. 

#### precipitation
##### The precipitation chemistry and rainfall volumes are located here.

####

### **src**

The src folder contains all R scripts needed to complete the box model for Great Bay. They are described in the order that they should be run.

#### main_dataformat.R
##### Purpose: Read in, process, and clean up water quality concentration data for tidal tributaries to Great Bay and for estuarine monitoring stations in Great Bay. 
###### Key results from main_dataformat.R saved in /results/main_dataformat 
* "df_conc.csv" - data frame of organized solute concentrations by date and site, corrected for method detection limits (except for DON concentrations)
* "Q_tidal_tribs" - combined discharge dataframe for Lamprey, Squamscott, and Winnicut Rivers

#### main_load_calc.R
##### Purpose: Calculate flow-weighted annual (calendar year) and monthly solute loads for the three tidal tributaries of Great Bay (Lamprey, Squamscott, & Winnicut). This script relies on csv files created by the main_dataformat.R script, including (df_conc.csv) and (Q_tidal_Tribs.csv).
###### Key final products: 
* Calendar year loads for each tributary saved in results/main_load_calc/FW_loads
* Flow weighted concentrations are saved in results/main_load_calc/FWC

#### main_estuarine_load_calc.R
##### Purpose: Calculates high and low tide flux of solutes based on river input of freshwater and known tidal prism. This script uses the saved output from the main_dataformat.R script (df_conc.csv). Products created in this script are saved in results/main_estuarine_load_calc

###### Key final products: 
* "AP_Flux_kgyr.csv" - dataframe of high and low tide estuarine fluxes
* "AP_Monthly_Flux_kgmonth.csv" - dataframe of monthly estuarine fluxes

#### main_precipitation_format.R
##### Purpose: Calculates precipitation-weighted concentrations for precipitation over Great Bay estuary using rain chemistry from UNH Thompson Farm

###### Key final products: 
* "cy_precip_loads.csv" - dataframe of calendar year precipitation fluxes

#### main_runoff.R
##### Purpose: Estimates load from coastal runoff for area surrounding Great Bay that is not encompassed by river watersheds. Estimates use the Lamprey River loads and watershed areas as a model system.

###### Key final products: 
* "runoff_estimate_kgyr.csv" - dataframe of runoff fluxes

#### main_wwtf_format.R
##### Purpose: Combines wastewater treatment facility water flux with concentration data for TN, DIN, and TSS into wastewater treatment loads for facilities downstream of the tidal tributary monitoring stations. C and P fluxes were estimated using C:N:P ratios for the epping wastewater treatment facility.


###### Key final products:
* "wwtf_annual_loads.csv" - wastewater treatment estimated loads

#### main_dilution_calculation.R
##### Purpose: Exercise to see what we would expect the N concentration in Great Bay to be if the rivers were the only N source. How much does the river load dilute once in the estuary?

#### main_compile_inputs.R
##### Purpose: Combine input loads and output loads into one dataframe, calculate delta storage terms. Requires loading of products saved in results files, including:
* LR_Annual_Loads.csv
* SQR_Annual_Loads.csv
* WNC_Annual_Loads.csv
* cy_precip_loads.csv
* wwtf_annual_loads.csv
* AP_Flux_kgyr.csv
* runoff_estimate_kgyr.csv"

###### Key final products:
* Budget_Components.csv - dataframe of each budget component solute flux, categorized as input or output
* inputsaspercentage.csv - dataframe that considers inputs as percentage of the total input for the year

#### main_plot_annual_budgets.R
##### Purpose: Plot annual solute budget inputs, outputs, and delta storage terms using the dataframes built in main_compile_inputs


### **results**

### Sources
* Trowbridge, P. Hydrological Parameters for New Hampshire’s Estuaries. PREP Reports & Publications (2007).

#### Notes
When running these scripts, keep track of where you are saving things. The scripts are organized in such a way that one can work within a project space. Within the project space, created products will be saved down into a "results" folder. 

## Contributing

### Author: Anna Mikulis, University of New Hampshire
#### Date Created: August 6, 2021

For questions or issues please contact Anna Mikulis: anna.mikulis@unh.edu


## License



