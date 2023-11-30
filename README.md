# mycotoxins_BSFL
Data and Rcode to the manuscript "Conversion of mycotoxin-contaminated maize by black soldier fly larvae into feed and fertilizer" published in the Journal of Insects as Food and Feed

# Folders 

code: includes all R code
data: includes all data files

# code

myco.R: Includes all R code related to analysis of the raw mycotoxin results (e.g. dry mass correction, consideration of LOQ/LOD, signficant analysis of results, visualization)
code.R: 

# data

Note: Maoze inclusion level inconsitent and partially incorrect between data sets. See manuscript for correct inclusions.

myco_results.xlsx: Mycotoxin results 
- sample_code	maize_inclusion_perc: 
- afla_level: clean (clean maize without mycotoxin contamination), contam (maize contaminated with mycotoxins), CONTROL (clean maize used in substrate), HIGH (high mycotoxin contamination), LOW (low mycotoxin contamination)
- sample_type: Raw maize (maize used in this study for generation of substrates), frass, larvae (harvested larvae), substrate (mixture of raw maize and agri-food byproducts)
- replicate: biological replicate
- unit: ug/kg (microgram per kg, as is)
- remaining columns are the different mycotoxins, see manuscript and SI

substrate_frass_larvae.xlsx: Substrate, frass and larvae compositonal analyses
- afla_level: see above, MEDIUM (medium mycotoxin contamination, not included in manuscript) 
- maize_inclusion_perc: 40, 60, 80 %DM (dry mass, see correct inclusion levels in manuscript). Only 40 and 80 %DM used in manuscript. 
- sample_type: see above, fishmeal (sampled in Rwanda), soybean (sampled in Rwanda)
- parameter: Amino acid compostion and nutrient composition

LOD_LOQ.xlsx: Level of quantification (LOQ) and level of detection (LOD)

crate_temp.xlsx: Temperaure in the substrate/frass measured throughout the exepriment
- afla_level: See above
- maize_inclusion_perc: 50, 65, 80 %M (same as above, see correct inclusion levels in manuscript)
- replicate: biological replicate = different rearing container
- day: Rearing day, calculated from start of feeding with the maize-based diet
- T_crate_9am: temperature reading in °C taken at 9 am
- T_crate_1pm: temperature reading in °C taken at 1 pm
- T_crate_5pm: temperature reading in °C taken at 5 pm

environmental_conditions.xlsx: ambient temperature/relative humidity (RH) measured in the vicinity of the rearing crates throughout the experiment
- Time: Time of the day the temperature/RH was recorded
- Day: rearing day, see above
- Position: different locations within the building where environmental conditions were recorded to get representative value
- temp_C: temperature in °C
- RH_perc: relative humidity in %
- minT_C: minimum temperature measured in °C at this day
- maxT_C: maximum temperature measured in °C at this day

larval_mass.xlsx: 
- afla_level: see above
- maize_inclusion_perc: see above
- replicate: biological replicate from which the sample was taken = rearing container
- day: rearing day, see above
- time: time the measurment was taken:
- number_larvae: number of larvae in the sample
- weight_larvae_g: total mass of larvae of the sample in gram
