# GreatBay_BoxModel

### Author: Anna Lowien, University of New Hampshire
#### Date Created: August 6, 2021
#### Last Modified: August 6, 2021
##### Purpose: A respository of data and scripts necessary to create a box-model for solute loading to Great Bay Estuary, NH/ME. This respository is associated with the M.S. Thesis Work titled, "Biogeochemical Stressors and Ecological Reponse in Great Bay Estuary, NH/ME". Data was sourced from monitoring efforts of the UNH Water Quality Analyis Lab, the NH Department of Environmental Services, the Piscataqua Region Estuaries Partnership, and the Great Bay National Estuarine Research Reserve. 


### **Source Scripts**
#### main_dataformat.R
##### Purpose: Read in, process, and clean up water quality concentration data for tidal tributaries to Great Bay and for estuarine monitoring stations in Great Bay. 
###### Key final product: "df6" - data frame of organized solute concentrations by date and site, corrected for method detection limits (except for DON concentrations)

#### main_load_calc.R
##### Purpose: Calculate flow-weighted annual (calendar year and water year) and monthly solute loads for the three tidal tributaries of Great Bay (Lamprey, Squamscott, & Winnicut). 
