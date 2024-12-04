# riat-postproc
postprocessing of RIAT+ output

## content

| file name | description |
|-----------|-------------|
|map_popexp.R|maps of population exposure|
|plot_costs.R|barplot of costs|
|plot_emissions.R|barplot of emission reductions|
|plot_measures.R|detailed plot of main measures selected|
|plot_popexp.R|summary Pareto plot (costs vs population exposure)|
|read_riat.R|read output files|
|recode_tech.R|rename some measures|
|sankey_riat.R|Sankey plot of cost allocation in a scenario|
|table_measures.R|summary tables of measures|
|unbias_riat.R|unbias AQI output|
|prepare_scenario_reductions.R|prepare input files *.emrd for aggregated scenario|
|map_aqi.R|plot AQI maps|
|write_aqi.R|write AQI into CSV file|
|aqi_spatial_synthesis.R|average AQI on regions, provinces and main cities|


## additional content

* function to fit Nth maximum value vs annual mean https://gist.github.com/jobonaf/f20f4902942797a4a004a3a13614b261
* function to rasterize population on a grid https://gist.github.com/jobonaf/cb3d2096a4ea2448d74f8a1e97fd21d6