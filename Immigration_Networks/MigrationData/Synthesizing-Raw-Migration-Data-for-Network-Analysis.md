---
title: "README"
author: "Joey Hebl"
date: "2026-03-28"
output: 
  html_document: 
    keep_md: true
---


# Getting Started: Sourcing the Raw Migration data

There are many sources of migration data available publicly, but for the present
project I sourced my raw data from the United Nations, specifically from the 
Population Division's "International Migrant Stock" [report](https://www.un.org/development/desa/pd/content/international-migrant-stock). 
Data is presented in 5-year intervals starting from 1990 and continuing until 2020 (at the
time of, and that which is considered in the current version of this project). Relevant country
migration data can be selected according to either Destination, Origin, or Both. 

This excel can be filtered for the countries of interest, both from sending and receiving perspectives.
Though an obvious point, it is worth mentioning here that on a small enough timescale, migration is, at it's core and as will be constructed in the present analysis, is a two-party phenomena (i.e. it involves a country of origin 
and country of destination). A more nuanced, qualitative discussion of this and other aspects of this analysis
can be reviewed in the Further Thoughts and Considerations file. 

For the present project, which is focused on migration in the Americas, the following
criteria was used for selecting countries:
  (1) Minimum population of 2 million (this effectively eliminates the Caribbean-Island 
      nations though in a future analysis I hope to include these states as well).
  (2) Within the "American" Region (the continued debate regarding geographic nomenclature--and the inherent
      effects this has on socioeconomic factors, as well as cultural perceptions--,though 
      important, is irrelevant to the present analysis given the complete inclusion of both the North
      and South American regions).
      
From this filtering-protocol, the following nations, and their respective migration data, were selected:
  Argentina
  Bolivia
  Brazil
  Canada
  Chile
  Colombia
  Costa Rica
  Ecuador
  El Salvador
  Guatemala
  Honduras
  Mexico
  Nicaragua
  Panama
  Paraguay
  Peru
  United States
  Uruguay
  Venezuela
  
The following section will review how this raw data of migration between nation-pairs, 
was converted into a matrix.

# Step 1: Constructing the Matrix

