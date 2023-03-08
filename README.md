# WGSAM skill assessment
Gadget models for the [WGSAM](https://www.ices.dk/community/groups/pages/wgsam.aspx) multispecies skill assessment project

Implement single and multispecies Gadget models using internally consistent multispecies data with known observation error characteristics from an end-to-end ecosystem operative model based on the Norwegian and Barents Sea (NOBA) Atlantis model by Hansen et al. (2016, 2019).
Gadget is one of several estimation models used in the WGSAM multispecies skill assessment project which aims to model performance testing, alternative models comparison, and performance testing of model ensembles. The objectives link to the WGSAM [ToR c](https://www.ices.dk/about-ICES/Documents/Resolutions/2022%20Resolutions/HAPISG%20EGs%20Resolutions%202022.pdf#page=52) "Establish and apply methods to assess the skill of multispecies models intended for operational advice".

### How to use this repository
1. build an [mfdb](https://github.com/gadget-framework/mfdb) database and import the OM simulations ```mfdb/import_noba_simulations.R``` available via [ms-keyrun](https://github.com/NOAA-EDAB/ms-keyrun/)
2. start building single species models. Each model is built with ```models/xx.sppName/setup.R``` and fitted to data with ```models/xx.sppName/run.R```
