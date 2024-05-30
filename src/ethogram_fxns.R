# Ethogram functions

# Load the ethogram task list
load_task_list <- function(opt = "default"){
  require(janitor)
  require(tidyr)
  if(opt == "default"){
    task_list <- read.csv("./data/ethogram/task-list.csv")
    task_list <- clean_names(task_list)
    task_list$dog <- factor(task_list$dog)
  }else if(opt == "kinematics-paper"){
    task_list <- read.csv("./data/ethogram/task-list-kinematics-paper.csv")
    task_list <- task_list[,-grep("Notes", colnames(task_list))]
    task_list <- na.omit(task_list) # Removing rows with NA (not completed)
    task_list <- task_list[task_list$Dog != 10,] # Removing Dog 10
    #task_list <- task_list[task_list$Dog != 21,] # Removing dog 21
    task_list$Dog <- factor(task_list$Dog)
  }else{
    stop("Please choose a valid option.")
  }
  return(task_list)
}


# Head function that loads and reformats the data
load_ethograms <- function(task_list, opt, remove_unprompted){
  require(stringr)
  
  if(opt=="tasks"){
    
    # Initializing list to store data
    event_data <- list()
    event_data$info <- list()
    event_data$tasks <- data.frame()
    event_data$time_budget <- data.frame()
    event_data$task_counts <- data.frame()
    
    # Loops over each dog in the set and each trial and calculates: 
    for(dog in levels(task_list$dog)){
      for(trial in 1:max(task_list$trial)){
        #trial, dog, run, trained
        if(!file.exists(paste0("./data/ethogram/task_csv_files/T", 
                               trial, "D", dog, "R", 1, 
                               "_sideview_events_1.csv"))){
          print(paste0("No data for dog: ", dog, ", trial: ", trial))
          next
        }
          print(paste0("Analyzing event dog: ", dog,", trial: ", trial))
          dog_tasks_run1 <- load_ethogram_tasks(trial, dog, 1, remove_unprompted)
          dog_tasks_run2 <- load_ethogram_tasks(trial, dog, 2, remove_unprompted)
          trialcode_run1 <- paste0("T", trial, "D", dog, "R", 1)
          trialcode_run2 <- paste0("T", trial, "D", dog, "R", 2)
          event_data$info[[trialcode_run1]] <- dog_tasks_run1$info
          event_data$info[[trialcode_run2]] <- dog_tasks_run2$info
          event_data$tasks <- rbind(event_data$tasks, 
                                    dog_tasks_run1$tasks, dog_tasks_run2$tasks)
          event_data$time_budget <- rbind(event_data$time_budget, 
                                          dog_tasks_run1$time_budget,
                                          dog_tasks_run2$time_budget)
          event_data$task_counts <- rbind(event_data$task_counts, 
                                     dog_tasks_run1$event_counts, 
                                     dog_tasks_run2$event_counts)
      }
    }
    return(event_data)
    
  } else if(opt == "individual"){
    # Initializing list to store data
    individual_data <- list()
    individual_data$info <- list()
    individual_data$states <- data.frame()
    individual_data$states_counts <- data.frame()
    individual_data$states_time_budget <- data.frame()
    individual_data$points <- data.frame()
    individual_data$points_counts <- data.frame()
  
    for(dog in levels(task_list$dog)){
      for(trial in 1:max(task_list$trial)){
        if(!file.exists(paste0("./data/ethogram/individual_behavior_csv_files/B_T", 
                               trial, "D", dog, "R", 1, 
                               "_events.csv"))){
          print(paste0("No data for dog: ", dog, ", trial: ", trial))
          next
        }
        print(paste0("Analyzing event dog: ", dog,", trial: ", trial))
        dog_indiv_1 <- load_ethogram_individuals(trial, dog, 1, remove_unprompted)
        dog_indiv_2 <- load_ethogram_individuals(trial, dog, 2, remove_unprompted)
        trialcode_run1 <- paste0("T", trial, "D", dog, "R", 1)
        trialcode_run2 <- paste0("T", trial, "D", dog, "R", 2)
        individual_data$info[[trialcode_run1]] <- dog_indiv_1$info
        individual_data$info[[trialcode_run2]] <- dog_indiv_2$info
        individual_data$states <- rbind(individual_data$states, 
                                        dog_indiv_1$states, dog_indiv_2$states)
        individual_data$states_counts <- rbind(individual_data$states_counts, 
                                        dog_indiv_1$states_counts, 
                                        dog_indiv_2$states_counts)
        individual_data$states_time_budget <- rbind(individual_data$states_time_budget, 
                                                    dog_indiv_1$states_time_budget,
                                                    dog_indiv_2$states_time_budget)
        individual_data$points <- rbind(individual_data$points,
                                        dog_indiv_1$points, dog_indiv_2$points)
        individual_data$points_counts <-rbind(individual_data$points_counts, 
                                             dog_indiv_1$points_counts,
                                             dog_indiv_2$points_counts)
        
      }
    }
  return(individual_data)
  }
}
# Loads task ethogram data 
load_ethogram_tasks <- function(trial, dog, run, remove_unprompted = TRUE){
  # Initialize list object to store data
  events <- list()
  # Loads data file
  if(!file.exists(paste0("./data/ethogram/task_csv_files/T", 
            trial, "D", dog, "R", run, 
            "_sideview_events_1.csv"))){
    events <- NULL
  }else{
    test_dat <- read.table(paste0("./data/ethogram/task_csv_files/T", trial, 
                                  "D", dog, "R", run, "_sideview_events_1.csv"), 
                           nrow=1, sep = ",")
    trialcode <- paste0("T", trial, "D", dog, "R", run)
    split_test_code <- strsplit(test_dat$V2, "_")
    if(trialcode != split_test_code[[1]][1]){
      warning(paste(trialcode,"file had something wrong, please check!"))
      events <- NULL
    }else{
      events_dat <- read.csv(paste0("./data/ethogram/task_csv_files/T", 
                                    trial, "D", dog, "R", run, 
                                    "_sideview_events_1.csv"), 
                             skip = 15)
      
      # Overall trial information
      events$info <- list()
      events$info$media_location <- events_dat$Media.file.path[1]
      events$info$video_total_length <- events_dat$Total.length[1]
      events$info$fps <- events_dat$FPS[1]
      events$info$event_code <- paste0("T", trial, "D", dog, "R", run)
      events$info$dog <- dog
      events$info$trial <- trial
      events$info$run <- run
      events$info$target <- find_target(dog, trial, run)
      events$info$trained <- ifelse(trial >= 3, T, F)
      events$info$operational <- ifelse(trial >= 4, T, F)
      if(remove_unprompted) {
        events_dat <- remove_prompted_ethogram(events_dat, dog, trial, run)
        if(is.null(events_dat)){
          events$tasks <- NULL
          events$time_budget <- NULL
          events$event_counts <- NULL
        }else {
          events <- construct_task_event_data(dog, trial, run, events, events_dat)
        }
      }else{
        events <- construct_task_event_data(dog, trial, run, events, events_dat)
      }
    }
  }
  return(events)
}


# Loads ethogram data for individual behaviors (states and points)
load_ethogram_individuals <- function(trial, dog, run, remove_unprompted = TRUE){
  ind_behaviors <- list()
  ind_behaviors$info <- list()
  if(!file.exists(paste0("./data/ethogram/individual_behavior_csv_files/B_T",
                         trial,"D",dog,"R",run,"_events.csv"))){
    ind_behaviors <- NULL
  }else{
    ind_dat <- read.csv(paste0("./data/ethogram/individual_behavior_csv_files/B_T",
                               trial,"D",dog,"R",run,"_events.csv"), 
                        skip = 15)
    ind_behaviors$info$media_location <- ind_dat$Media.file.path[1]
    ind_behaviors$info$video_total_length <- ind_dat$Total.length[1]
    ind_behaviors$info$fps <- ind_dat$FPS[1]
    ind_behaviors$info$event_code <- paste0("T",trial,"D",dog,"R",run)
    ind_behaviors$info$dog <- dog
    ind_behaviors$info$trial <- trial
    ind_behaviors$info$run <- run
    ind_behaviors$info$target <- find_target(dog, trial, run)
    ind_behaviors$info$trained <- ifelse(trial >= 3, T, F)
    ind_behaviors$info$operational <- ifelse(trial >= 4, T, F)
    if(remove_unprompted) {
      ind_dat <- remove_prompted_ethogram(ind_dat, dog, trial, run)
      if(is.null(ind_dat)){
        ind_behaviors$states <- NULL
        ind_behaviors$states_counts <- NULL
        ind_behaviors$points <- NULL
      }else if (nrow(ind_dat[ind_dat$Status == "POINT", ]) == 0){
        ind_behaviors$points <- NULL
        ind_behaviors$points_counts <- NULL
        ind_behaviors <- construct_state_data(dog, trial, run, 
                                                         ind_behaviors, ind_dat)
      }else if (nrow(ind_dat[ind_dat$Status != "POINT", ]) == 0){
        ind_behaviors$states <- NULL
        ind_behaviors$states_counts <- NULL
        ind_behaviors$states_time_budget <- NULL
        ind_behaviors <- construct_point_data(dog, trial, run, ind_behaviors, 
                                              ind_dat)
      }else{
        ind_behaviors <- construct_state_data(dog, trial, run, 
                                                         ind_behaviors, ind_dat)
        ind_behaviors <- construct_point_data(dog, trial, run, ind_behaviors,
                                              ind_dat)
      }
    }else{
      ind_behaviors <- construct_state_data(dog, trial, run, 
                                                       ind_behaviors, ind_dat)
      ind_behaviors <- construct_point_data(dog, trial, run, ind_behaviors,
                                            ind_dat)
    }
  }
  return(ind_behaviors)
}

construct_task_event_data <- function(dog, trial, run, events, events_dat){
  # Construct data frame for task events:
  events$tasks <- data.frame("dog" = rep(dog, nrow(events_dat)), 
                             "trial" = rep(trial, nrow(events_dat)), 
                             "run" = rep(run, nrow(events_dat)), 
                             "target" = rep(events$info$target, nrow(events_dat)),
                             "trained" = rep(events$info$trained, nrow(events_dat)),
                             "operational" = rep(events$info$operational, nrow(events_dat)),
                             "behavior" = events_dat$Behavior, 
                             "event_time" = events_dat$Time, 
                             "status" = events_dat$Status
  )
  events$tasks$behavior <- factor(events$tasks$behavior, 
                                  levels = c("t", "c" , "o", "a"))
  events$tasks$status <- factor(str_to_lower(events$tasks$status), 
                                levels = c("start", "stop"))
  
  events$tasks <- find_elapsed_times(events$tasks)
  time_dat <- calc_time_budget(events$tasks)
  events$time_budget <- data.frame("dog" = rep(dog, nlevels(events$tasks$behavior) + 1), 
                                   "trial" = rep(trial, nlevels(events$tasks$behavior) + 1),
                                   "run" = rep(run, nlevels(events$tasks$behavior) + 1),
                                   "target" = rep(events$info$target, nlevels(events$tasks$behavior) + 1),
                                   "trained" = rep(events$info$trained, nlevels(events$tasks$behavior) + 1),
                                   "operational" = rep(events$info$operational, nlevels(events$tasks$behavior) + 1),
                                   "behavior" = c(NA, levels(events$tasks$behavior)), 
                                   "total_times" = time_dat[, "totals"],
                                   "fractions" = time_dat[, "fractions"])
  events$event_counts <- count_events(events$tasks)
  return(events)
}  

construct_point_data <- function(dog, trial, run, ind_behaviors, ind_dat){
    df_point <- ind_dat[ind_dat$Status == "POINT", ]
    ind_behaviors$points <- data.frame("dog" = rep(dog, nrow(df_point)), 
                                       "trial" = rep(trial, nrow(df_point)),
                                       "run" = rep(run, nrow(df_point)),
                                       "target" = rep(ind_behaviors$info$target, 
                                                      nrow(df_point)),
                                       "trained" = rep(ind_behaviors$info$trained, 
                                                       nrow(df_point)),
                                       "operational" = rep(ind_behaviors$info$operational, 
                                                       nrow(df_point)),
                                       "behavior" = df_point$Behavior,
                                       "event_time" = df_point$Time)
    ind_behaviors$points$behavior[ind_behaviors$points$behavior=="t"] <- "w"
    ind_behaviors$points$behavior[ind_behaviors$points$behavior=="c"] <- "k"
    ind_behaviors$points$behavior <- factor(ind_behaviors$points$behavior, 
                                            levels = c("h", "e", "f", "b", "w", "v",
                                                       "n", "k"))
    ind_behaviors$points_counts <- count_events(ind_behaviors$points)
    return(ind_behaviors)
  }

construct_state_data <- function(dog, trial, run, ind_behaviors, ind_dat){
  df_states <- ind_dat[ind_dat$Status != "POINT", ] # state behaviors
  ind_behaviors$states <- data.frame("dog" = rep(dog, nrow(df_states)), 
                                     "target" = rep(ind_behaviors$info$target, 
                                                    nrow(df_states)),
                                     "trial" = rep(trial, nrow(df_states)),
                                     "run" = rep(run, nrow(df_states)),
                                     "trained" = rep(ind_behaviors$info$trained, 
                                                     nrow(df_states)),
                                     "operational" = rep(ind_behaviors$info$operational, 
                                                     nrow(df_states)),
                                     "behavior" = df_states$Behavior, 
                                     "event_time" = df_states$Time, 
                                     "status" = df_states$Status)
  ind_behaviors$states$behavior <- factor(ind_behaviors$states$behavior, 
                                          levels = c("p", "j", "s", "l"))
  ind_behaviors$states$status <- factor(str_to_lower(ind_behaviors$states$status), 
                                        levels = c("start", "stop"))
  ind_behaviors$states_counts <- count_events(ind_behaviors$states)
  ind_behaviors$states <- find_elapsed_times(ind_behaviors$states)
  time_dat <- calc_time_budget(ind_behaviors$states)
  ind_behaviors$states_time_budget <- data.frame("dog" = rep(dog, nlevels(ind_behaviors$states$behavior) + 1), 
                                                 "trained" = rep(ind_behaviors$info$trained, nlevels(ind_behaviors$states$behavior) + 1),
                                                 "trial" = rep(trial, nlevels(ind_behaviors$states$behavior) + 1),
                                                 "target" = rep(ind_behaviors$info$target, nlevels(ind_behaviors$states$behavior) + 1),
                                                 "behavior" = c(NA, levels(ind_behaviors$states$behavior)), 
                                                 "total_times" = time_dat[, "totals"],
                                                 "fractions" = time_dat[, "fractions"])
  return(ind_behaviors)
}

# Counts number of behavior events in a data set
count_events <- function(dat){
  counts <- matrix(0, nrow=1, ncol=nlevels(dat$behavior))
  for(j in 1:nlevels(dat$behavior)){
    colnames(counts) <- levels(dat$behavior)
    dat_sub <- dat[dat$behavior==levels(dat$behavior)[j],]
    if(nrow(dat_sub) != 0){
      starts <- dat_sub$event_time[dat_sub$status == "start"]
      if(length(starts)>0){
        counts[j] <- length(starts)
      }else{
        counts[j] <- nrow(dat_sub)
      }
      
    }
  }
  df_counts <- tidyr::pivot_longer(data.frame(counts), cols = everything())
  df_counts <- data.frame("dog" = dat$dog[1], 
                          "trial" = dat$trial[1],
                          "run" = dat$run[1],
                          "target" = dat$target[1],
                          "trained" = dat$trained[1],
                          "operational" = dat$operational[1],
                          "behavior" = df_counts$name, 
                          "counts" = df_counts$value)
  return(df_counts)
}

# Link individual behaviors with tasks
# link_tasks_behaviors <- function(points_sub, events_sub){
#   asso_task <- rep(NA, nrow(points_sub))
#   for (j in 1:nrow(points_sub)){
#     test_time <- points_sub$event_time[j] 
#     for(k in 1:nlevels(events_sub$behavior)){
#       events_extra_sub <- events_sub[events_sub$behavior == 
#                                        levels(events_sub$behavior)[k], ]
#       if(nrow(events_extra_sub) == 0){
#         
#       }else{
#         starts <- events_extra_sub$event_time[
#           events_extra_sub$status == "start"] <= test_time
#         stops <- events_extra_sub$event_time[
#           events_extra_sub$status == "stop"] >= test_time
#         event_temp <- ifelse(starts & stops, T, F)
#         if(all(is.na(event_temp))){
#           asso_task[j] <- NA
#         }else if(any(event_temp) & is.na(asso_task[j])){
#           asso_task[j] <- levels(events_sub$behavior)[k]
#         } else if(any(event_temp) & !is.na(asso_task[j])){
#           asso_task[j] <- paste(asso_task[j], 
#                                 levels(events_sub$behavior)[k], 
#                                 sep = ",")
#         }
#       }
#     }
#   }
#   return(asso_task)
# }

# Calculates the elapsed times for state behaviors
find_elapsed_times <- function(dat){
  #dat<- events$tasks
  dat$elapsed_time <- rep(NA, nrow(dat))
  endpts <- dat[is.na(dat$behavior),]
  for(j in 1:nlevels(dat$behavior)){
    dat_sub <- dat[dat$behavior == levels(dat$behavior)[j],]
    dat_sub <- dat_sub[!is.na(dat_sub$event_time),]
    
    if(nrow(dat_sub) != 0){
      print(paste(j, levels(dat$behavior)[j], sep = ","))
      starts <- dat_sub$event_time[dat_sub$status == "start"]
      stops <- dat_sub$event_time[dat_sub$status == "stop"]
      if(length(starts) != length(stops)) {
        if(length(starts) == 0) {
          starts <- endpts$event_time[1]
          dat <- rbind(dat, dat[is.na(dat$behavior) & dat$status == "start",])
          dat$behavior[is.na(dat$behavior) & dat$status == "start"][1] <- levels(dat$behavior)[j]
        } else if(length(stops) == 0) {
          stops <- endpts$event_time[2]
          dat <- rbind(dat, dat[is.na(dat$behavior) & dat$status == "stop",])
          dat$behavior[is.na(dat$behavior) & dat$status == "stop"][1] <- levels(dat$behavior)[j]
        } else  if(length(starts) > length(stops)){
          stops <- c(stops, endpts$event_time[2])
          dat <- rbind(dat, dat[is.na(dat$behavior) & dat$status == "stop",])
          dat$behavior[is.na(dat$behavior) & dat$status == "stop"][1] <- levels(dat$behavior)[j]
        }else{
          starts <- c(endpts$event_time[1], starts)
          dat <- rbind(dat[is.na(dat$behavior) & dat$status == "start",], dat)
          dat$behavior[is.na(dat$behavior) & dat$status == "start"][1] <- levels(dat$behavior)[j]
        }
        if(any((stops - starts) < 0)){
          starts <- dat_sub$event_time[dat_sub$status == "start"]
          stops <- dat_sub$event_time[dat_sub$status == "stop"]
          stops <- c(stops, endpts$event_time[2])
          dat <- rbind(dat, dat[is.na(dat$behavior) & dat$status == "stop",])
          dat$behavior[is.na(dat$behavior) & dat$status == "stop"][1] <- levels(dat$behavior)[j]
          starts <- c(endpts$event_time[1], starts)
          dat <- rbind(dat[is.na(dat$behavior) & dat$status == "start",], dat)
          dat$behavior[is.na(dat$behavior) & dat$status == "start"][1] <- levels(dat$behavior)[j]
          dat_sub <- dat[dat$behavior == levels(dat$behavior)[j],]
          dat_sub <- dat_sub[!is.na(dat_sub$event_time),]
          starts <- dat_sub$event_time[dat_sub$status == "start"]
          stops <- dat_sub$event_time[dat_sub$status == "stop"]
        }
        print(starts)
        print(length(starts))
        print(stops)
        print(length(stops))
        dat$elapsed_time[which(dat$behavior == levels(dat$behavior)[j] & 
                                 dat$status == "stop")] <- 
          stops - starts
        print(stops-starts)
      } else {
        if(any((stops - starts) < 0)){
          starts <- dat_sub$event_time[dat_sub$status == "start"]
          stops <- dat_sub$event_time[dat_sub$status == "stop"]
          stops <- c(stops, endpts$event_time[2])
          dat <- rbind(dat, dat[is.na(dat$behavior) & dat$status == "stop",])
          dat$behavior[is.na(dat$behavior) & dat$status == "stop"][1] <- levels(dat$behavior)[j]
          starts <- c(endpts$event_time[1], starts)
          dat <- rbind(dat[is.na(dat$behavior) & dat$status == "start",], dat)
          dat$behavior[is.na(dat$behavior) & dat$status == "start"][1] <- levels(dat$behavior)[j]
          dat_sub <- dat[dat$behavior == levels(dat$behavior)[j],]
          dat_sub <- dat_sub[!is.na(dat_sub$event_time),]
          starts <- dat_sub$event_time[dat_sub$status == "start"]
          stops <- dat_sub$event_time[dat_sub$status == "stop"]
        }
        print(starts)
        print(length(starts))
        print(stops)
        print(length(stops))
        dat$elapsed_time[which(dat$behavior == levels(dat$behavior)[j] & 
                                 dat$status == "stop")] <- 
          stops - starts
        print(stops-starts)
      }
      
    }
    rm(dat_sub)
  }
  dat <- dat[order(dat$event_time),]
  return(dat)
}

# Calculates the time budget (percent of total time) for state behaviors
calc_time_budget <- function(dat){
  total_time <- diff(range(as.numeric(dat$event_time), na.rm = T))
  time_dat <- matrix(NA, nrow = (nlevels(dat$behavior)+1), ncol = 2)
  row.names(time_dat) <- c("total", levels(dat$behavior))
  #row.names(time_dat) <- c("total", "off_task", "casting", "on_odor", "alert")
  colnames(time_dat) <- c("totals", "fractions")
  time_dat[1,] <- c(total_time, 1)
  for(k in 1:nlevels(dat$behavior)){
    time_dat[k+1,] <- c(sum(dat$elapsed_time[dat$behavior == 
                                               levels(dat$behavior)[k]], 
                            na.rm = T), 
                        sum(dat$elapsed_time[dat$behavior == 
                                               levels(dat$behavior)[k]], 
                            na.rm = T)/total_time)
  }
  return(time_dat)
}

# Calculates the difference in time budgets before and after training
calc_time_differences <- function(time_budget_before, time_budget_after){
  change_time <- time_budget_after$total_times -
    time_budget_before$total_times
  change_frac <- time_budget_after$fractions -
    time_budget_before$fractions
  
  time_changes <- data.frame(
    "dog" = rep(time_budget_after$dog[1], 5),
    "target" = rep(time_budget_after$target[1], 5),
    "behavior" = time_budget_after$behavior[1:5],
    "time" = change_time,
    "frac" = change_frac
  )
  return(time_changes)
}

# Calculates the count of behavior differences before and after training
calc_count_differences <- function(counts_before, counts_after){
  behaviors <- counts_before$behavior
  counts <- matrix(NA, nrow=1, ncol = length(behaviors))
  colnames(counts) <- behaviors
  for(j in 1:length(behaviors)){
    counts[j] <- counts_after$counts[counts_after$behavior == behaviors[j]] - 
      counts_before$counts[counts_before$behavior == behaviors[j]]
  }
  
  counts_df <- tidyr::pivot_longer(data.frame(counts), cols=everything())
  counts_df <- data.frame(counts_before$dog[1], 
                          counts_before$target[1],
                          counts_df)
  colnames(counts_df) <- c("dog", "target", "behavior", "count_change")
  return(counts_df)
}

# Find and summarize behaviors linked with tasks
find_task_counts <- function(dat, behavior){
  temp_vec <- dat$asso_task[dat$behavior == behavior]
  temp_vec <- temp_vec[- which(is.na(temp_vec))] 
  temp_vec2 <- strsplit(temp_vec, split = ",")
  temp_vec2 <- unlist(temp_vec2)
  levels_vec <- c("t", "c", "o", "a")
  count <- rep(NA, length(levels_vec))
  for(i in 1:length(count)){
    count[i] <- sum(temp_vec2 == levels_vec[i])
  }
  return(count)
}

# Compares all individual behaviors to task event times to see if they associate
compare_task_point <- function(task_list, event_data, individual_data){
  # Create a master list of events using trial codes
  code_list <- c(names(event_data$info), names(individual_data$info))
  code_list <- unique(code_list)
  # Preallocate space for linked tasks and linked states
  individual_data$points$task_link <- rep(NA, nrow(individual_data$points))
  individual_data$points$state_link <- rep(NA, nrow(individual_data$points))

  for(event in code_list){
    info <- individual_data$info[[event]]
    if(is.null(info)) next
    dog <- info$dog
    trial <- info$trial
    run <- info$run
    events_sub <- event_data$tasks[event_data$tasks$dog == dog &
                                     event_data$tasks$trial == trial &
                                     event_data$tasks$run == run, ]
    events_sub <- events_sub[!is.na(events_sub$behavior), ]
    states_sub <- individual_data$states[individual_data$states$dog == dog &
                                           individual_data$states$trial == trial &
                                           individual_data$states$run == run, ]
    states_sub <- states_sub[!is.na(states_sub$behavior), ]
    points_sub <- individual_data$points[individual_data$points$dog == dog &
                                           individual_data$points$trial == trial &
                                           individual_data$points$run == run, ]
    if(nrow(points_sub) == 0) next
    if(nrow(events_sub) == 0 & nrow(states_sub) == 0) next
    points_sub$task_link <- rep(NA, nrow(points_sub))
    points_sub$state_link <- rep(NA, nrow(points_sub))
    for (i in 1:nrow(points_sub)){
      linked_tasks <- ifelse(points_sub$event_time[i] > events_sub$event_time[events_sub$status == "start"] &
               points_sub$event_time[1] < events_sub$event_time[events_sub$status == "stop"],
             as.character(events_sub$behavior), NA)
      points_sub$task_link[i] <- paste(unique(linked_tasks[!is.na(linked_tasks)]), collapse = ",")
      linked_states <- ifelse(points_sub$event_time[i] > states_sub$event_time[states_sub$status == "start"] &
                               points_sub$event_time[1] < states_sub$event_time[states_sub$status == "stop"],
                             as.character(states_sub$behavior), NA)
      points_sub$state_link[i] <- paste(unique(linked_states[!is.na(linked_states)]), collapse = ",")
    }
    individual_data$points$task_link[individual_data$points$dog == dog &
                                       individual_data$points$trial == trial &
                                       individual_data$points$run == run] <-
      points_sub$task_link
    individual_data$points$state_link[individual_data$points$dog == dog &
                                        individual_data$points$trial == trial &
                                        individual_data$points$run == run] <-
      points_sub$state_link
  }
  individual_data$points$task_link[individual_data$points$task_link == ""] <- NA
  individual_data$points$state_link[individual_data$points$state_link == ""] <- NA
  return(individual_data)
}

# Summarizing linked behaviors and tasks
summarize_linked_behaviors <- function(event_data, individual_data){
  # Create a master list of events using trial codes
  code_list <- c(names(event_data$info), names(individual_data$info))
  code_list <- unique(code_list)
  event_data$tasks$state_link <- rep(NA, nrow(event_data$tasks))
  for(event in code_list){
    info <- event_data$info[[event]]
    if(is.null(info)) next
    dog <- info$dog
    trial <- info$trial
    run <- info$run
    events_starts <- event_data$tasks[event_data$tasks$dog == dog &
                                     event_data$tasks$trial == trial &
                                     event_data$tasks$run == run &
                                       event_data$tasks$status == "start", ]
    events_starts <- events_starts[!is.na(events_starts$behavior), ]
    events_stops <- event_data$tasks[event_data$tasks$dog == dog &
                                        event_data$tasks$trial == trial &
                                        event_data$tasks$run == run &
                                        event_data$tasks$status == "stop", ]
    events_stops <- events_stops[!is.na(events_stops$behavior), ]
    states_starts <- individual_data$states[individual_data$states$dog == dog &
                                           individual_data$states$trial == trial &
                                           individual_data$states$run == run &
                                           individual_data$states$status == "start", ]
    states_starts <- states_starts[!is.na(states_starts$behavior), ]
    states_stops <- individual_data$states[individual_data$states$dog == dog &
                                              individual_data$states$trial == trial &
                                              individual_data$states$run == run &
                                              individual_data$states$status == "stop", ]
    states_stops <- states_stops[!is.na(states_stops$behavior), ]
    if(nrow(states_starts) == 0) next
    for (j in 1:nlevels(events_starts$behavior)){
      events_starts_sub <- events_starts[events_starts$behavior ==
                                           levels(events_starts$behavior)[j], ]
      events_stops_sub <- events_stops[events_stops$behavior ==
                                           levels(events_starts$behavior)[j], ]
      if(nrow(events_stops_sub) == 0) next
      for(i in 1:nrow(events_stops_sub)){
        linked_states_1 <- ifelse(states_starts$event_time > events_starts_sub$event_time[i] &
                                  states_starts$event_time < events_stops_sub$event_time[i],
                                as.character(states_starts$behavior), NA)
        linked_states_2 <- ifelse(states_stops$event_time > events_starts_sub$event_time[i] &
                                    states_stops$event_time < events_stops_sub$event_time[i],
                                  as.character(states_starts$behavior), NA)
        combo <- c(linked_states_1, linked_states_2)
        linked_states <- paste(unique(combo[!is.na(combo)]), collapse = ",")
        events_stops_sub$state_link[i] <- linked_states
      }
      events_stops$state_link[events_stops$behavior ==
                     levels(events_starts$behavior)[j]] <- events_stops_sub$state_link
    }
    event_data$tasks$state_link[event_data$tasks$dog == dog &
                       event_data$tasks$trial == trial &
                       event_data$tasks$run == run &
                       event_data$tasks$status == "stop" &
                         !is.na(event_data$tasks$behavior)] <- events_stops$state_link
  }
  event_data$tasks$state_link[event_data$tasks$state_link == ""] <- NA
  return(event_data)
}
  
  
# {
#   targets <- c("2E1H", "Amm")
#   trials <- 1:4
#   df <- data.frame()
#   for(target in targets){
#     for(dog in levels(dogs)){
#       for(trained in trials){
#         dat <- individual_data[[paste0(type, target)]]
#         dat <- dat[dat$dog == dog & 
#                      dat$trial == trained,]
#         behavior_levels <- levels(dat$behavior)
#         for(j in 1:length(behavior_levels)){
#           count_temp <- find_task_counts(dat,  
#                                          behavior_levels[j])
#           times_temp <- find_task_times(dat, behavior_levels[j])
#           df_temp <- data.frame("dog" = rep(dog, 4),
#                                 "trial" = rep(trained, 4),
#                                 "target" = rep(target, 4),
#                                 "trained" = ifelse(trained == 3, T, F),
#                                 "behavior" =
#                                   rep(behavior_levels[j], 4), 
#                                 "task" = c("t","c","o","a"), 
#                                 "count"  = count_temp,
#                                 "times" = times_temp)
#           df <- rbind(df, df_temp)
#           rm(df_temp, count_temp)
#         }
#       }
#     }
#   }
#   return(df)
# }

find_task_times <- function(dat, behavior_level){
  task_levels <- c("t","c","o","a")
  dat_temp<- dat[dat$behavior == behavior_level,]
  times <- rep(NA, length(task_levels))
  for(i in 1:length(task_levels)){
    times[i] <- sum(dat_temp$event_time[dat_temp$asso_task == task_levels[i]], na.rm = T)
  }
  return(times)
}

find_target <- function(dog, trial, run, hole = FALSE){
  chem_locations <- read.csv("./data/flowsensor/chem_locations.csv",
                             skip = 1)
  if(trial == 5 & dog >=105) run <- run + 0.5
  chem_locations <- chem_locations[chem_locations$trial == trial &
                                     chem_locations$run == run,]
  if(hole) {
    return(chem_locations)
  } else {
    target <- ifelse(colnames(chem_locations)[is.na(chem_locations)] == "X2E1H", 
                     "Ammonia", "2E1H")
    return(target)
  }
}

remove_prompted_ethogram <- function(dat, dog, trial, run){
    trialcode <- paste0("T", trial, "D", dog, "R", run)
    if(file.exists(paste0("./data/ethogram/unprompted_search_files/",
                          trialcode,"_sideview_unprompted_1.csv"))){
      timeline <- read.csv(paste0("./data/ethogram/unprompted_search_files/",
                                  trialcode,"_sideview_unprompted_1.csv"),
                           skip = 15, header = T)
      dat$Time <- ifelse(dat$Time < timeline$Time[1] | 
                                  dat$Time > timeline$Time[2], 
                                NA, dat$Time)
      dat <- dat[!is.na(dat$Time),]
      if(nrow(dat) == 0) {
        dat <- NULL
      }else{
        dat <- rbind(data.frame("Time" = timeline$Time[1], 
                                "Media.file.path" = dat$Media.file.path[1],
                                "Total.length" = dat$Total.length[1], 
                                "FPS" = dat$FPS[1], 
                                "Subject" = dat$Subject[1],
                                "Behavior" = NA, 
                                "Behavioral.category" = NA,
                                "Comment" = NA, 
                                "Status" = "START"),
                     dat)
        dat <- rbind(dat,
                     data.frame("Time" = timeline$Time[2], 
                                "Media.file.path" = dat$Media.file.path[1],
                                "Total.length" = dat$Total.length[1], 
                                "FPS" = dat$FPS[1], 
                                "Subject" = dat$Subject[1],
                                "Behavior" = NA, 
                                "Behavioral.category" = NA,
                                "Comment" = NA, 
                                "Status" = "STOP"))
      }
    }else {
      warning(paste(trialcode,"does not have unprompted timing file!"))
    }
  return(dat)
}

# Other important things: 
point_behavior_labels <- c("shoulder shrug", "eye convergence", "facial tensing",
                           "head turn",  "lip lick",  "ear prick",
                           "head snap", "tail wag")
state_behavior_labels <- c("jumping", "look at handler", "pawing", 
                           "sniffing")
