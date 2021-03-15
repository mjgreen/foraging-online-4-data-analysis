require(tidyverse)
require(ggpubr)
fls <- list.files(path='../data', pattern='*.csv', full.names=TRUE)
all_files = tibble()
i=0
for (fl in fls) {
  i=i+1
  cat('reading file #', i, fl,'\n')
  fin <- read_csv(fl, col_types = cols())
  this_file <- fin %>% 
    mutate(subject=NA) %>% 
    mutate(age=ifelse("please type your age" %in% names(fin),`please type your age`,as.numeric(NA))) %>%
    mutate(sex=ifelse("please indicate your gender" %in% names(fin),`please indicate your gender`,as.character(NA))) %>%
    rename(pointer='please select your pointing device') %>% 
    mutate(condition=conditionProportion) %>% 
    rename(trial=trialCount) %>% 
    rename(times=current_time_list) %>% 
    rename(frames=frame_list) %>% 
    rename(events=myevent_list) %>% 
    rename(trees=clicked_in_cells) %>% 
    mutate(points=points_list) %>% 
    select(date, participant, subject, condition, phase, trial, times, frames, events, trees, points)
  all_files <- bind_rows(all_files, this_file) 
}
cat('finished reading csv files\n')
# all_files is still a row-per-trial layout. 
# The per-frame samples are in a single row 
# in a cell as a list - you need to extract 
# the lists to get row-per-frame layout

# number off the subjects in order of date
# and make subject and trial be factors
# Also delete practice trials and rows that 
# were treating the ethics bits as trials
cat("re-arranging data in order of participation date\n")
all_data <- all_files %>% 
  arrange(date, participant, trial) %>% 
  mutate(subject = factor(all_files$date, levels=unique(all_files$date), labels=1:length(unique(all_files$date)))) %>% 
  select(condition, subject, trial, phase, times, frames, events, trees, points) %>% 
  filter(phase=='experimental') %>% select(-phase) %>% 
  mutate(trial = as_factor(trial)) 

# extract frame-by-frame data into a list of lists
cat("extracting the frame-by-frame data from the list-cells\n")
all_frames=list()
for(s in levels(all_data$subject)) {
  for(t in levels(all_data$trial)) {
    all_frames[[s]][[t]]$frame = all_data[all_data$subject==s&all_data$trial==t,'frames'] %>% str_replace_all(c('\\['='','\"'='',']'='')) %>% str_split(pattern=',') %>% unlist() %>% as.numeric()
    all_frames[[s]][[t]]$time = all_data[all_data$subject==s&all_data$trial==t,'times'] %>% str_replace_all(c('\\['='','\"'='',']'='')) %>% str_split(pattern=',') %>% unlist() %>% as.numeric()
    all_frames[[s]][[t]]$tree = all_data[all_data$subject==s&all_data$trial==t,'trees'] %>% str_replace_all(c('\\['='','\"'='',']'='')) %>% str_split(pattern=',') %>% unlist() %>% as.numeric()
    all_frames[[s]][[t]]$ptally = all_data[all_data$subject==s&all_data$trial==t,'points'] %>% str_replace_all(c('\\['='','\"'='',']'='')) %>% str_split(pattern=',') %>% unlist() %>% as.numeric()
    all_frames[[s]][[t]]$event = all_data[all_data$subject==s&all_data$trial==t,'events'] %>% str_replace_all(c('\\['='','\"'='',']'='')) %>% str_split(pattern=',') %>% unlist() %>% as.character()
  }
}

# recombine into a frame-by-frame tibble
cat("recombining the frame-by-frame data into a tibble\n")
fbf = tibble()
for(s in levels(all_data$subject)) {
  for(t in levels(all_data$trial)) {
    tmp=tibble(
      all_data[all_data$subject==s&all_data$trial==t,'condition'],
      subject=as.numeric(s),
      trial=as.numeric(t),
      frame=all_frames[[s]][[t]]$frame,
      time=all_frames[[s]][[t]]$time,
      tree=all_frames[[s]][[t]]$tree,
      ptally=all_frames[[s]][[t]]$ptally,
      event=all_frames[[s]][[t]]$event,
    )
    fbf=bind_rows(fbf,tmp)
  }
}
fbf <- fbf %>% 
  mutate(
    condition=as_factor(condition),
    subject=as_factor(subject),
    trial=as_factor(trial)
  )
cat("summary of the frame-by-frame data:\n")
print(summary(fbf))

# create an event-by-event df
# This also computes and annotates with trial-wise rejection rates for apples and bananas
# but deletes the intermediate columns for the computations
ebe <- 
  fbf %>% 
  group_by(subject, trial) %>% 
  mutate(event = ifelse(ptally>lag(ptally) & event == "sustained click in low value", "consume low value", event)) %>%
  mutate(event = ifelse(ptally>lag(ptally) & event == "sustained click in high value", "consume high value", event)) %>%
  filter(!(event==lag(event)&tree==lag(tree)&ptally==lag(ptally)&trial==lag(trial))) %>% 
  filter(!(event %in% c("null", "mouse newly released", "mouse newly down", "attempt to click n drag detected and rejected", "click in empty", "sustained click in previously-gotten fruit tree"))) %>% 
  filter(!(event==lead(event)&tree==lead(tree)&trial==lead(trial))) %>% 
  filter(!(event=="sustained click in low value"&lead(event)=="consume low value"&tree==lead(tree)&trial==lead(trial))) %>% 
  filter(!(event=="sustained click in high value"&lead(event)=="consume high value"&tree==lead(tree)&trial==lead(trial))) %>% 
  mutate(visit_apple=ifelse(event %in% c("sustained click in low value","consume low value"),tree,NA)) %>% 
  mutate(eat_apple=ifelse(event%in%c("consume low value"),tree,NA)) %>% 
  mutate(ign_apple=ifelse(visit_apple %in% setdiff(visit_apple,eat_apple), tree, NA)) %>% 
  mutate(n_ign_apple=length(na.omit(unique(ign_apple)))) %>% 
  mutate(n_vis_apple=length(na.omit(unique(visit_apple)))) %>% 
  mutate(p_ign_apple=n_ign_apple/n_vis_apple) %>% 
  mutate(visit_banana=ifelse(event %in% c("sustained click in high value","consume high value"),tree,NA)) %>% 
  mutate(eat_banana=ifelse(event%in%c("consume high value"),tree,NA)) %>% 
  mutate(ign_banana=ifelse(visit_banana %in% setdiff(visit_banana,eat_banana), tree, NA)) %>% 
  mutate(n_ign_banana=length(na.omit(unique(ign_banana)))) %>% 
  mutate(n_vis_banana=length(na.omit(unique(visit_banana)))) %>% 
  mutate(p_ign_banana=n_ign_banana/n_vis_banana) %>% 
  mutate(inter_click_interval=time-lag(time)) %>% 
  mutate(event=as_factor(event)) %>% 
  select(-c(visit_apple,eat_apple,ign_apple,visit_banana,eat_banana,ign_banana,n_ign_apple,n_ign_banana,n_vis_apple,n_vis_banana))
cat("summary of the event-by-event data - p_ign_banana is NaN if no bananas were encountered\n")
print(summary(ebe))  

# The number of trials goes from 440 in fbf to 427 in ebe: 
# show that all the 13 lost trials are from subject 6 from 
# trial 8 onward (20 minus 7 is 13 missing trials) - 
# these trials are ones in which no fruit-bearing tree was 
# clicked on so they get filtered out while making ebe
cat("elaborate which trials we lose going from fbf to ebe\n")
i=0
for (s in 1:22) {
  for (t in 1:20) {
    if(nrow(subset(ebe,subject==s&trial==t))==0){
      i=i+1
      cat('lost trial #',i,'sub',s, 'trial',t, 'with', nrow(subset(ebe,subject==s&trial==t)), 'events of interest\n')
    }
  }
}

tbt = ebe %>% group_by(condition, subject, trial) %>% 
  summarise(.groups='drop_last', # aggregate over trials
    score=max(ptally, na.rm=T),
    ici=mean(inter_click_interval, na.rm=T)
    )
ggscatter(
  tbt, x='score', y='ici',  add='reg.line', conf.int=T, facet.by='condition',
  add.params = list(color = "condition", fill = "lightgray")
  ) +
  stat_cor(method='pearson')
  
