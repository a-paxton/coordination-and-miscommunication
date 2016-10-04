#### libraries-and-functions-CM.r: Part of `coordination-miscommunication-analyses.Rmd` ####
#
# This script sets the working directory, loads libraries, creates a number of 
# additional functions to facilitate data prep and analysis.
#
# Written by: A. Paxton (University of California, Berkeley)
# Date last modified: 2 September 2016
#####################################################################################

#### Set working directory ####
setwd('/users/alexandra/dropbox/bloco-newest/')

#### Load necessary libraries ####
library(crqa)
library(lme4)
library(plyr)
library(ggplot2)
library(reshape2)
library(pander)
library(dplyr)
library(purrr)
library(pander)
library(gridExtra)
library(plotrix)
library(gtable)
library(e1071)
library(MuMIn)
library(flux)
library(fmsb)
library(viridis)

#### Create functions we'll need ####

# trim time series to be of equal lengths
equal.lengths = function(ts1,ts2){
  
  # see whether it's a 1D object
  if (is.null(dim(ts1))){
    
    # if the two time series aren't of equal lengths...
    if (length(ts1) != length(ts2)){
      
      # ... find the shortest length ...
      minlength = min(length(ts1),length(ts2))
      
      # ... and force them both to be that long ...
      ts1 = ts1[1:minlength]
      ts2 = ts2[1:minlength]
    }
    
  } else {
    
    # if the two time series aren't of equal lengths...
    if (dim(ts1)[1] != dim(ts2)[1]){
      
      # ... find the shortest length ...
      minlength = min(dim(ts1)[1],dim(ts2)[1])
      
      # ... and force them both to be that long ...
      ts1 = ts1[1:minlength,]
      ts2 = ts2[1:minlength,]
    }
  }
  
  # spit out the time series if/once they're the same length
  return(list(ts1,ts2))
}

##

# add prefix to all variable names except "dyad" and "lag"
add.var.prefix = function(df,var.prefix){
  
  # load library for function
  library(magrittr)
  
  # specify which we're skipping
  skip.vars = c('dyad','Pair','lag')
  skip.cols = df[names(df) %in% skip.vars]
  
  # grab all of the other columns and add the relevant prefix
  renaming.cols = df[! names(df) %in% skip.vars]
  renaming.cols = renaming.cols %>%
    setNames(paste0(var.prefix,".",names(.)))
  
  # combine the renamed and unchanged frames and return it
  new.df = cbind.data.frame(skip.cols,renaming.cols)
  return(new.df)
}

## 

# "pander_lme": simplify lme4 printouts (available on GitHub: https://github.com/a-paxton/stats-tools)
pander_lme = function(lme_model_name, stats.caption){
  
  # load in pander
  library(pander)
  
  # # disable scientific notation
  # options(scipen = 999)
  # 
  # convert the model summary to a dataframe
  neat_output = data.frame(summary(lme_model_name)$coefficient)
  
  # round p-values to 2 decimal places except if it's quite small
  neat_output$p = 2*(1-pnorm(abs(neat_output$t.value)))
  neat_output$p[neat_output$p >= .005] = round(neat_output$p[neat_output$p >= .005],2)
  neat_output$p[neat_output$p >= .0005] = round(neat_output$p[neat_output$p >= .0005],3)
  neat_output$p[neat_output$p < .0005] = round(neat_output$p[neat_output$p < .0005],4)

  # create significance and trending markers
  neat_output$sig = ' '
  neat_output$sig[neat_output$p < .1] = '.'
  neat_output$sig[neat_output$p < .05] = '*'
  neat_output$sig[neat_output$p < .01] = '**'
  neat_output$sig[neat_output$p < .001] = '***'
  
  # set a caption that includes R-squared values
  if (stats.caption == TRUE){
    
    # use MuMIN to calculate R-squared
    library(MuMIn)
    model_marginal_r_squared = r.squaredGLMM(lme_model_name)[['R2m']]
    model_conditional_r_squared = r.squaredGLMM(lme_model_name)[['R2c']]
    neat_caption = paste('**Marginal *R*-squared: ',
                         round(model_marginal_r_squared,2), 
                         ". Conditional *R*-squared: ",
                         round(model_conditional_r_squared,2),".**",sep="")
    
    # return the table
    return(pander(neat_output, split.table = Inf, caption = neat_caption, style = 'rmarkdown'))
  } else { # or return a table without it
    return(pander(neat_output, split.table = Inf, style = 'rmarkdown'))
  }
}

## 

# "plot_qi": A function to plot both quartiles ('q') and individual data points ('i') in the same plot.
plot_qi = function(df,x_var_name,y_var_name,quartile_var_name,individual_var_name,
                   plot_title,x_label,y_label){
  
  # require appropriate libraries
  require(ggplot2)
  require(viridis)
  
  # access the target variables within the dataframe
  x_var = df[[x_var_name]]
  y_var = df[[y_var_name]]
  individual_var = df[[individual_var_name]]
  quartile_var = df[[quartile_var_name]]
  
  # set the color options for the 4 quartiles
  quartile_dfs = split(df,df[[quartile_var_name]])
  quartile_colors = viridis(length(unique(quartile_dfs)))
  quartile_alpha = .4
  
  # plot it
  complete_plot = ggplot(df) +
    
    # parameters of the 
    theme(legend.position = "none") +
    ggtitle(plot_title) +
    xlab(x_label) +
    ylab(y_label) +
    ylim(c(.2,1)) +
    
    # plot individual lines
    geom_smooth(aes(x=x_var, 
                    y=y_var, 
                    color=individual_var),
                alpha=.1,
                size=.2) +
    scale_color_viridis(discrete=TRUE, alpha=.6) +
    
    # plot quartile 1
    geom_smooth(data=quartile_dfs[[1]],
                aes(x=quartile_dfs[[1]][[x_var_name]],
                    y=quartile_dfs[[1]][[y_var_name]],
                    color=quartile_dfs[[1]][[quartile_var_name]],
                    fill=quartile_dfs[[1]][[quartile_var_name]]),
                alpha=quartile_alpha,
                color=quartile_colors[1],
                fill=quartile_colors[1]) +
    
    # plot quartile 2
    geom_smooth(data=quartile_dfs[[2]],
                aes(x=quartile_dfs[[2]][[x_var_name]],
                    y=quartile_dfs[[2]][[y_var_name]],
                    color=quartile_dfs[[2]][[quartile_var_name]],
                    fill=quartile_dfs[[2]][[quartile_var_name]]),
                alpha=quartile_alpha,
                color=quartile_colors[2],
                fill=quartile_colors[2]) +
    
    # plot quartile 3
    geom_smooth(data=quartile_dfs[[3]],
                aes(x=quartile_dfs[[3]][[x_var_name]],
                    y=quartile_dfs[[3]][[y_var_name]],
                    color=quartile_dfs[[3]][[quartile_var_name]],
                    fill=quartile_dfs[[3]][[quartile_var_name]]),
                alpha=quartile_alpha,
                color=quartile_colors[3],
                fill=quartile_colors[3])+
    
    # plot quartile 4
    geom_smooth(data=quartile_dfs[[4]],
                aes(x=quartile_dfs[[4]][[x_var_name]],
                    y=quartile_dfs[[4]][[y_var_name]],
                    color=quartile_dfs[[4]][[quartile_var_name]],
                    fill=quartile_dfs[[4]][[quartile_var_name]]),
                alpha=quartile_alpha,
                color=quartile_colors[4],
                fill=quartile_colors[4])
  
  return(complete_plot)
}