## Repo description

This repo allows you to use the RothC model to calculate the C content in soils over time. 

The main functionality builds on the [SoilR](https://cran.r-project.org/web/packages/SoilR/SoilR.pdf) package, a dockerized fastapi app to submit model runs as rest api requests is also provided.

The repo provides functions to carry out a sensitivity analysis of the RothC model. 

### File descritions

| File name | Description | 
| ------------ | -------------- |
| average\_weather\_data.R | code to calculate average historical climate variables in Schwerin |
| data | folder containing data tables necessary to run the models |
| documentation | folder containing further information regarding documentation and model assumptions |
| estimate\_carbon\_input.R\* | script containing functions that guide the conversion of crop and manure input to Carbon estimates |
| field\_initialisation.R | *Unclear* |
| initialize.R | *Unclear* |
| install\_packages.R | script to install all packages needed to use the project\_run.rmd file |
| main.py | *Unclear* |
| main.R | *Unclear* |
| model\_functions.R\* | functions required to run the project\_run.rmd file. *I think this file contains functions from the SoilR package as well as functions that modify this function. It also seems to contain functions to calculate C inputs. Confirm difference with estimate carbon input.R file* |
| modified\_functions.R | This script includes modified functions from the soilR package including a simulation of the effect of tillage |
| plotting\_functions.R | Script provides functions that aid in the plotting of the output of the modelling efforts |
| project\_run.rmd | file on which to run the project farm |
| requirements.txt | *Unclear* |
| sensitivity\_analysis\_ranged\_parameters.R | Requires model\_functions.R and modified\_functions.R. Provides a result for a sensitivity analysis including graphs|
| sensititvity\_analysis.R | script to run a sensitivity analysis on the RothC model including the modifications to tillage factor |
| Uncertainty\_Analysis.rmd | Uncertainty analysis for a theoretical farm in Germany. |
| uncertainty\_functions.R\* | Script contraining functions necessary to run an uncertainty analysis |
| weather\_uncertainty\_calculations.R | |
| \* *these scripts are required to run the project\_run.rmd script* | |

