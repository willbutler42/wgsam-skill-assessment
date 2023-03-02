# WGSAM_skill_assessment
Gadget models for the WGSAM multispecies skill assessment project

ADD A BRIEF DESCRIPTION OF THE PROJECT WITH RELEVANT LINKS

### How to use this repository
1. build an [mfdb](https://github.com/gadget-framework/mfdb) database in and import the OM simulations ```mfdb/import_noba_simulations.R``` available via [ms-keyrun](https://github.com/NOAA-EDAB/ms-keyrun/)
2. start building single species models. Each model is built with ```models/xx.sppName/setup.R``` and fitted to data with ```models/xx.sppName/run.R```
