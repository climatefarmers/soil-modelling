# Documentation

## Changes to the model

### Soil cover 
The RothCModel takes a moisture factor as one of the inputs. This is calculated using the function fW.RothC and takes as inputs the soil thickness, clay content, weather effects and the evaporation corefficient. 

However this only allows a logical input for the coverage of the soil (covered/not covered). This has an effect on the Maximum soil decomposition rate (Max.TSMD) which is used as a limiting factor in the calculation of the Accumulated soil decomposition rate. 

The moisture factor, b is calculated as follows:

>  if (Acc.TSMD > (0.444*Max.TSMD)) b = 1 

> else (Acc.TSMD <= (0.444\*Max.TSMD)) b = 0.2 + 0.8((Max.TSMD - Acc.TSMD)/(Max.TSMD - (0.444\*Max.TSMD)))

where the 

> Max.TSMD = -(20+1.3pClay - 0.01pClay^2)(S.thickness/23)*1/B

and

> B = 1 if the soil is covered, and 1.8 if not. 


What this means is that if the soil is not covered, 0.55 of the maximum moisture of the soil can be retained. For thicker soils this seems to have a much lesser effect, however for finer soils, this is evidently more relevant. 

The difference in the modified function, as opposed to the original, is that we calculate the MaxTSMD as a monthly value rather than a single value (for covered or not). The Accumulated TSMD is calculated monthly and compared to this monthly value. Because there may be changes to decomposition rate depending on the temperature and rainfall in a given time and place, these values are also provided for each month rather than yearly. 

### Tillage




## Running the model

### Assumptions

#### Carbon inputs

For this version of the soil modelling efforts we have made assumptions regarding the amount of Carbon that goes into a system.  these assumptions are based on existing literature, and the reasoning and references are provided below. 


**Inputs from crop residues** 


**Inputs from grass turnover** 

**Inputs from tree root and leaf turnover** 

In agroforestry systems trees provide additional carbon from root and leaf turnover. These values were calculated per hectare from information provided by the land manager regarding the number and species of trees in the project area. 

We assume a root turnover of 0.1 yr-1 for the entire root system, 0.5 yr-1 for fine-roots in global forests and 1 yr-1 for fine-roots in European forest [Bruner et al (2013)](https://doi.org/10.1007/s11104-012-1313-5).

For foliage turnover we used species specific amounts reported by the literature in areas similar to the project in terms of climate and management were possible. If not available, we chose an average turnover rate for other species belonging to the same genus, or family. 

In the case of cork oak, we assumed a foliage turnover of 120gC/m²/yr in Montado system in Portugal [(Arosa et al. 2016)](https://doi.org/10.1071/SR15347). 


**Inputs from manure**

In the case of free ranging cattle, we calculated how much manure was input into the soil on a monthly basis based on several factors: 

1. Number of animals per hectare.
2. Animal species present in the project area. 
3. Outdoor grazing time.
4. Amount of manure produced by a single animal per day. 
5. Average carbon content in the manure per animal species. 

The first three items are information provided to us by the land manager themselves. Items four and five were obtained from different sources, see [table] for specifics. 

# References

Brunner, I., Bakker, M.R., Björk, R.G. et al. Fine-root turnover rates of European forests revisited: an analysis of data from sequential coring and ingrowth cores. Plant Soil 362, 357–372 (2013). https://doi.org/10.1007/s11104-012-1313-5  
Maria Luísa, Costa Sofia R., Freitas Helena (2016) Leaf decomposition of cork oak under three different land uses within a montado of southern Portugal. Soil Research 55, 215-221. https://doi.org/10.1071/SR15347  
