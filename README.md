# GreatBay_BoxModel

### Author: Anna Lowien, University of New Hampshire
#### Date Created: August 6, 2021
#### Last Modified: August 23, 2022
##### Purpose: A respository of data and scripts necessary to create a box-model for solute loading to Great Bay Estuary, NH/ME. This respository is associated with the M.S. Thesis Work titled, "Biogeochemical Stressors and Ecological Reponse in Great Bay Estuary, NH/ME". Data was sourced from monitoring efforts of the UNH Water Quality Analyis Lab, the NH Department of Environmental Services, the Piscataqua Region Estuaries Partnership, and the Great Bay National Estuarine Research Reserve. 


### **Data**

#### original_files/Original_EMD
##### Contains water quality data for the head of tide river stations (05-LMP, 09-EXT, 02-WNC, GRBAP). Includes sample date, time, water quality data for each station. 

### **Source Scripts**
#### main_dataformat.R
##### Purpose: Read in, process, and clean up water quality concentration data for tidal tributaries to Great Bay and for estuarine monitoring stations in Great Bay. 
###### Key final products: 
* "df6" - data frame of organized solute concentrations by date and site, corrected for method detection limits (except for DON concentrations)
* "Q_tidal_tribs" - combined discharge dataframe for Lamprey, Squamscott, and Winnicut

#### main_load_calc.R
##### Purpose: Calculate flow-weighted annual (calendar year and water year) and monthly solute loads for the three tidal tributaries of Great Bay (Lamprey, Squamscott, & Winnicut).
###### Key final products: 
* Calendar Year and Water Year loads for each tributary

#### main_estuarine_load_calc.R
##### Purpose: Calculates high and low tide flux of solutes based on river input of freshwater and known tidal prism

#### main_precipitation_format.R
##### Purpose: Calculates precipitation-weighted concentrations for precipitation over Great Bay estuary.

#### main_runoff.R
##### Purpose: Estimates load from coastal runoff based on Lamprey River loads and watershed areas.

#### main_wwtf_format.R
##### Purpose: Combines wastewater treatment facility water flux with concentration data for TN, DIN, and TSS into wastewater treatment loads for facilities downstream of the tidal tributary monitoring stations. C and P fluxes were estimated using C:N:P ratios for the epping wastewater treatment facility.

#### main_dilution_calculation.R
##### Purpose: Exercise to see what we would expect the N concentration in Great Bay to be if the rivers were the only N source. How much does the river load dilute once in the estuary?

#### main_compile_inputs.R
##### Purpose: Combine input loads and output loads into one dataframe, calculate delta storage terms.

#### main_plot_annual_budgets.R
##### Purpose: Plot annual solute budget inputs, outputs, and delta storage terms.

### Sources
* Trowbridge, P. Hydrological Parameters for New Hampshire’s Estuaries. PREP Reports & Publications (2007).
