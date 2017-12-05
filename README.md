<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/ATFutures/popdens.svg)](https://travis-ci.org/ATFutures/popdens) [![Project Status: Concept - Minimal or no implementation has been done yet.](http://www.repostatus.org/badges/0.1.0/concept.svg)](http://www.repostatus.org/#concept)

popdens
=======

Global-scale data for population density. Current potential data sources include:

| Source                                                                                                                                                          | Nominal Resolution | Latest Data | Future Projections? |
|-----------------------------------------------------------------------------------------------------------------------------------------------------------------|--------------------|-------------|---------------------|
| [worldpop](http://www.worldpop.org.uk/)                                                                                                                         | 100m               | 2015        | No                  |
| Euro. Commission [Global Human Settlement](http://ghslsys.jrc.ec.europa.eu/ghs_pop.php)                                                                         | 250m               | 2014        | No                  |
| Euro. Commission [Joint Research Centre](http://data.jrc.ec.europa.eu/dataset/jrc-ghsl-ghs_pop_gpw4_globe_r2015a/resource/ece1dd0b-a69a-4804-a69b-0984b15efcdd) | 250m               | 2015        | No                  |
| [NASA Socioecon. Data (SEDAC)](http://sedac.ciesin.columbia.edu/data/collection/gpw-v3)                                                                         | 1km                | 2020        | Yes                 |

Current analyses use `worldpop` only. Potential extension to incorporate NASA data to allow for future projections.
