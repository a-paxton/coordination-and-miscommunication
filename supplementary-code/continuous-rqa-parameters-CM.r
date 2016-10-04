#### continuous-rqa-parameters-CM.r: Part of `coordination-miscommunication-analyses.Rmd` ####
#
# This script explores the parameters for the continuous cross-recurrence analysis
# that we'll run over the informativeness data.
#
# Written by: A. Paxton (University of California, Berkeley)
# Date last modified: 25 August 2016
#####################################################################################

##### 1. Preliminaries #####

# prep workspace and libraries
library(plyr)
library(dplyr)
library(tseriesChaos)
library(nonlinearTseries)
library(crqa)

# read in bloco dataset
bloco = read.csv('./data/bloco-raw_data-coordination_miscommunication_analyses.csv',
                 header=TRUE,row.names=NULL)

# subset data to minimum for recurrence over language
bloco.info = select(bloco,Pair,Trial,Turn,tTurn,Talker,charlength)

##### 2. Determine delay with average mutual information (AMI) #####

# set maximum AMI
ami_lag_max = 20

# spin off each participant's data into a separate dataframe
E_df = bloco.info[bloco.info$Talker=='E',]
NE_df = bloco.info[bloco.info$Talker=='NE',]

# get AMI (lag and value) for eyetracked participants
E_amis = E_df %>% ungroup() %>%
  group_by(Pair) %>%
  mutate(E_ami_val = min(as.numeric(mutual(charlength, lag.max = ami_lag_max, plot = FALSE)),na.rm=TRUE)) %>%
  mutate(E_ami_loc = which.min(as.numeric(mutual(charlength, lag.max = ami_lag_max, plot = FALSE)))-1) %>%
  group_by(Pair,E_ami_val,E_ami_loc) %>%
  distinct()

# get AMI (lag and value) for eyetracked participants
NE_amis = NE_df %>% ungroup() %>%
  group_by(Pair) %>%
  mutate(NE_ami_val = min(as.numeric(mutual(charlength, lag.max = ami_lag_max, plot = FALSE)),na.rm=TRUE)) %>%
  mutate(NE_ami_loc = which.min(as.numeric(mutual(charlength, lag.max = ami_lag_max, plot = FALSE)))-1) %>%
  group_by(Pair,NE_ami_val,NE_ami_loc) %>%
  distinct()

# join the two together
amis = join(data.frame(E_amis),data.frame(NE_amis),by=c('Pair'),type='full') %>%
  group_by(Pair) %>%
  mutate(ami_selected = min(E_ami_loc,NE_ami_loc))

# write AMI information to file
write.table(amis,'./data/ami_calculations-CM.csv', sep=',',row.names=FALSE,col.names=TRUE)

# if we've already run it, load it in
amis = read.table('./data/ami_calculations-CM.csv', sep=',',header=TRUE)

# join the AMI information to our whole dataframe
bloco.info = join(bloco.info,amis,by=c("Pair"))

##### 3. Determine embedding dimension with false nearest neighbors (FNN) #####

# set maximum percentage of false nearest neighbors
fnnpercent = 10

# create empty dataframe
fnns = data.frame(Pair = numeric(),
                  E_head_embed = numeric(),
                  E_tail_embed = numeric(),
                  NE_head_embed = numeric(),
                  NE_tail_embed = numeric())

# spin off each pair's data into a separate dataframe
pair_dfs = split(bloco.info,bloco.info$Pair)

# cycle through each pair
for (pair in names(pair_dfs)){

    # print update
    print(paste("Beginning FNN calculations for Pair ",pair,sep=""))

    # grab the next pair's data
    E_data = pair_dfs[[pair]][pair_dfs[[pair]]$Talker=='E',]
    NE_data = pair_dfs[[pair]][pair_dfs[[pair]]$Talker=="NE",]
  
    # only proceed if we have the pair's data
    if (length(E_data) > 0 & length(NE_data) > 0) {

      # calculate false nearest neighbors for E
      E_fnn = false.nearest(E_data$charlength, m = 10, d = 1, t = 0, rt = 10, 
                            eps = sd(E_data$charlength)/10)
      E_fnn = E_fnn[1,][complete.cases(E_fnn[1,])]
      E_threshold = E_fnn[1]/fnnpercent
      
      # calculate false nearest neighbors for NE
      NE_fnn = false.nearest(NE_data$charlength, m = 10, d = 1, t = 0, rt = 10, 
                             eps = sd(NE_data$charlength)/10)
      NE_fnn = NE_fnn[1,][complete.cases(NE_fnn[1,])]
      NE_threshold = NE_fnn[1]/fnnpercent

      # identify the largest dimension after a large drop for E
      E_embed_dim_index = as.numeric(which(diff(E_fnn) < -E_threshold)) + 1
      E_head_embed = head(E_embed_dim_index,1)
      E_tail_embed = tail(E_embed_dim_index,1)
      if (length(E_embed_dim_index) == 0){
        E_head_embed = 1
        E_tail_embed = 1
      }

      # identify the largest dimension after a large drop for NE
      NE_embed_dim_index = as.numeric(which(diff(NE_fnn) < -NE_threshold)) + 1
      NE_head_embed = head(NE_embed_dim_index,1)
      NE_tail_embed = tail(NE_embed_dim_index,1)
      if (length(NE_embed_dim_index) == 0){
        NE_head_embed = 1
        NE_tail_embed = 1
      }
      
      # bind everything to data frame
      Pair = as.integer(pair)
      fnns = rbind.data.frame(fnns,
                              cbind.data.frame(Pair,E_head_embed,E_tail_embed,
                                               NE_head_embed,NE_tail_embed))

  }}

# choose the largest embedding dimension
fnns = fnns %>% ungroup() %>%
  group_by(Pair) %>%
  mutate(embed_selected = max(c(E_head_embed,NE_head_embed,
                                E_tail_embed,NE_tail_embed)))

# save false nearest neighbor calculations to file
write.table(fnns,'./data/fnn_calculations-CM.csv', sep=',',row.names=FALSE,col.names=TRUE)

# if we've already run it, load it in
fnns = read.table('./data/fnn_calculations-CM.csv', sep=',',header=TRUE)

# merge with entire dataset
bloco.info = join(bloco.info, fnns, by = c("Pair"))

#### 4. Determine optimal radius ####

# rescale by mean distance
bloco_crqa = bloco.info %>% ungroup() %>%
  select(Pair,Talker,charlength,ami_selected,embed_selected) %>%
  group_by(Pair,Talker) %>%
  mutate(charlength = charlength/mean(charlength)) %>%
  mutate(charlength = charlength/mean(charlength))

# create an empty dataframe to hold the parameter information
radius_selection = data.frame(pair = numeric(),
                              chosen_delay = numeric(),
                              chosen_embed = numeric(),
                              chosen_radius = numeric(),
                              rr = numeric())

# identify radius for calculations -- also tried going as high as 5 in .05 increments
radius.list = seq(to=2.3,from=.01,by=.01)

# cycle through all conversations
crqa_data = split(bloco_crqa,list(bloco.info$Pair))
for (chosen_radius in radius.list){
  for (next_pair in crqa_data){

    # make sure we only proceed if we have data for the conversation
    if (dim(next_pair)[1] != 0){

      # print update
      print(paste("Radius ",chosen_radius,
                  ": Beginning CRQA calculations for Pair ",unique(next_pair$Pair),sep=""))

      # identify parameters
      chosen_delay = unique(next_pair$ami_selected)
      chosen_embed = unique(next_pair$embed_selected)

      # run CRQA and grab recurrence rate (RR)
      rec_analysis = crqa(next_pair$charlength[next_pair$Talker=='E'],
                          next_pair$charlength[next_pair$Talker=='NE'],
                          delay = chosen_delay, embed = chosen_embed, r = chosen_radius,
                          normalize = 0, rescale = 0, mindiagline = 2,
                          minvertline = 2, tw = 0, whiteline = FALSE,
                          recpt=FALSE)
      rr = rec_analysis$RR

      # clear it so we don't take up too much memory (optional)
      rm(rec_analysis)

      # append to dataframe
      pair = unique(next_pair$Pair)
      radius_selection = rbind.data.frame(radius_selection,
                                          cbind.data.frame(pair,
                                                           chosen_delay,
                                                           chosen_embed,
                                                           chosen_radius,
                                                           rr))
    }}}

# save the radius explorations to file
write.table(radius_selection,'./data/radius_calculations-mean_scaled-CM.csv', sep=',',row.names=FALSE,col.names=TRUE)

# if we've already run it, load it in
radius_selection = read.table('./data/radius_calculations-mean_scaled-CM.csv', sep=',',header=TRUE)

# identify how far off each is from our target RR
target = 5
radius_selection$from_target = abs(radius_selection$rr - 5)

# for each conversation in each dyad, choose the radius that gets us closest to a 5% RR
radius_stats = radius_selection %>%
  group_by(pair) %>%
  dplyr::filter(from_target==min(from_target)) %>%
  dplyr::arrange(pair) %>%
  plyr::rename(.,replace=c('pair' = "Pair"))

#### 5. Merge into new dataframe and export ####

# rename our rescaled variables here
bloco_crqa = bloco_crqa %>% ungroup() %>%
  plyr::rename(.,replace=c("charlength" = "rescale_charlength")) %>%
  select(Pair,Talker,rescale_charlength)

# join the dataframes
bloco_crqa = plyr::join(x=bloco_crqa,y=radius_stats, by=c("Pair"="Pair"))

# save to file
write.table(bloco_crqa,'./data/crqa_data_and_parameters-CM.csv', sep=',',row.names=FALSE,col.names=TRUE)