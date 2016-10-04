#### recurrence-settings-CM.r: Part of `coordination-miscommunication-analyses.Rmd` ####
#
# This script contains all parameters for categorical CRQA analyses. 
# Changing these will change all categorical recurrence analyses.
#
# Written by: A. Paxton (University of California, Berkeley)
# Date last modified: 1 July 2016
#####################################################################################

# set parameters for all categorical analyses
delay = 1; embed = 1; rescale = 1; radius = 1; normalize = 0; mindiagline = 2;
minvertline = 2; tw = 0; whiteline = FALSE; recpt = FALSE; side = "both"
checkl = list(do = FALSE, thrshd = 3, datatype = "categorical", pad = TRUE)

# set window size
win_size = 10