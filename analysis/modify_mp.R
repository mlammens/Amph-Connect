## ************************************************************************** ##
## modify_mp.R
##
## Author: Matthew Aiello-Lammens
## Date Created: 2018-08-05
##
## Purpose:
##
## ************************************************************************** ##

## Load packages
library("demgsa")
library(dplyr)


## Read in the snapping turtle model
snapper_mp <- mp.read("demographic_models/Demographics-Snapping Turtles/Snapping-Turtle-Metapop.mp")


## Get the dispersal matrix
snapper_dispersal <- snapper_mp$mp.file$DispMatr

## Calculate row sums to rank populations 
snapper_dispersal_total <- apply(snapper_mp$mp.file$DispMatr, MARGIN = 1, FUN = sum)

## Get population information data.frame
snapper_pop_df <- snapper_mp$mp.file$PopData_df

## Add total dispersal to this data frame
snapper_pop_df$total_disp <- snapper_dispersal_total
snapper_pop_df$rank <- 1:nrow(snapper_pop_df)

## Sort the populations
snapper_pop_df <- snapper_pop_df[order(snapper_pop_df$total_disp, decreasing = TRUE), ]
  
snapper_pop_df_highdispersal <- snapper_pop_df
snapper_pop_df_highdispersal$MaxR[1:10] <- 1.15
snapper_pop_df_highdispersal <- snapper_pop_df_highdispersal[order(snapper_pop_df_highdispersal$rank), ]

snapper_pop_df_lowdispersal <- snapper_pop_df
snapper_pop_df_lowdispersal <- snapper_pop_df_lowdispersal[order(snapper_pop_df_lowdispersal$rank), ]

lowdisp_index <- select(snapper_pop_df, total_disp, rank)
lowdisp_index <- lowdisp_index[order(lowdisp_index$total_disp), ]
lowdisp_index <- lowdisp_index[-which(lowdisp_index$total_disp == 0), ]

snapper_pop_df_lowdispersal$MaxR[lowdisp_index$rank[1:10]] <- 1.15


## Write the new MP files
snapper_mp_highdisp <- snapper_mp$mp.file
snapper_mp_highdisp$PopData_df <- select(snapper_pop_df_highdispersal, -rank, -total_disp)
mp.write(snapper_mp_highdisp, version = snapper_mp$version, mp.new.file = "demographic_models/Demographics-Snapping Turtles/Snapping-Turtle-Metapop-highdisp.mp")

snapper_mp_lowdisp <- snapper_mp$mp.file
snapper_mp_lowdisp$PopData_df <- select(snapper_pop_df_lowdispersal, -rank, -total_disp)
mp.write(snapper_mp_lowdisp, version = snapper_mp$version, mp.new.file = "demographic_models/Demographics-Snapping Turtles/Snapping-Turtle-Metapop-lowdisp.mp")
