# CA-listed-waters
Visualising the last 20 years of Clean Water Act 303d listings &amp; delistings in California USA

![EPA Region 3 listing & delisting trends, from USEPA 2012](https://github.com/cclatterbuck/CA-listed-waters/blob/main/figures/EPAR3_listingtrends.JPG)

The figure above is available in the USEPA 2012 report on [Identifying and Protecting Healthy Watersheds](https://www.epa.gov/sites/production/files/2015-10/documents/hwi-watersheds-complete.pdf) (pdf). I am interested in re-creating the figure for California's water bodies. 


## Contribution instructions
Please submit your own code visualizing this data! I'd be happy to add it to the scripts file in this repo. 

Forewarning: I'm still figuring out how contributions work, so I doubt I can be especially helpful here. If you use R, these [instructions](https://happygitwithr.com/fork-and-clone.html) from Jenny Bryan might be helpful. Also check out Marc Diethelm's contributing [repo](https://github.com/MarcDiethelm/contributing/blob/master/README.md) for practice. Otherwise, Google is your friend. 

## Data source
All data are from the State Water Resources Control Board's [Integrated Report](https://www.waterboards.ca.gov/water_issues/programs/water_quality_assessment/integrated_report_cycles.html) published regularly from 2022-2022. While the 303(d) lists are available as .xls or .xlsx, delisting data are not available as a flat file download for all years.  

## Scripts
01. Cleans & compiles the raw 303d lists into a single file, CWA_303d_waters_2002_2022.csv. Many columns are removed for the purposes of this exercise.
