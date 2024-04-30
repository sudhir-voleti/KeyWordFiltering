suppressPackageStartupMessages({

if(!require("shiny")) {install.packages("shiny")}
if(!require("tidyverse")) {install.packages("tidyverse")}
if(!require("DT")) {install.packages("DT")}
if(!require("stringr")) {install.packages("stringr")}
if(!require("quanteda")) {install.packages("quanteda")}
if(!require("tidytext")) {install.packages("tidytext")}
if(!require("purrr")) {install.packages("purrr")}  
if(!require("dplyr")) {install.packages("dplyr")}
if(!require("pdftools")) {install.packages("pdftools")}
if(!require("tools")) {install.packages("tools")}
  
library(shiny)
library(tidyverse)
library(DT)
library(stringr)
library(quanteda)
library(tidytext)
library(pdftools)
library(tools)
})
