# First, if you don't already have the devtools/httr/jsonlite packages on your machine run the following on the R commands:
install.packages("devtools")
install.packages("httr")
install.packages("jsonlite")

# Next install pilr.api.r using devtools, and load it to your session. We will also load in the two package dependencies.
library(devtools)
library(httr)
library(jsonlite)
install_github("pilrhealth/pilr.api.r")
library(pilr.api.r)

install.packages("uuid")
install.packages("dplyr")
install.packages("base64enc")
install.packages("zoo")
install.packages("ggvis")

install_github("MeiResearchLtd/pilr.utils.r",auth_token = "x")

install.packages("knitr")
install.packages("plyr")

# Install https://miktex.org/

install.packages("opencpu")
install.packages("ggplot2")