## going through Arya's analysis help map 

#two sample - sampled paired 
#-2 one sample t-test and compare } treat set of differences as one sample 
## test R star ate different temperatures / species - estimate amount of error
## family of different curves fit to the same dataset - bootstrapping (mean +- se) - dot and line


#two sample - samples independent 
# - two sample t-test with pooled variance term? (normal distribution, normal variance)
# - welch two sample t-test, no pooled variances (normal distribution, unequal variances)
# - mann-whitney or wilcoxon signed rank test (very non normally distributed, but similar distribution shapes/variances)
# - transformation / reasses / descriptive statistics (one/both very non normally distributed, without similated variances)

#can we do more than 2 samples with the mixed plate? wouldnt make any sense
## one predictor variable (r* or treatment - categorical?) ( or more than?) - categorical or continuous?
## - one variable (categorical): some type of ANOVA if there is equal or unequal variance, Kruskal wallis for non normal distribution and similar variance
## - multiple categorical == two factor anova

##might not be able to get growth rates from this data - keep getting negative growth, everything dying
##try fitting to monod curve


#POSITct code that didn't work - come back and try later
{
  time_difference_1 <- difftime(times_dates$day_1_POSIXct, times_dates$day_0_POSIXct, units = "hours")
  print(time_difference_1)
  
  time_to_numeric <- function(time_string) {
    parts <- strsplit(time_string, ":")[[1]]
    hours <- as.numeric(parts[1])
    minutes <- as.numeric(parts[2])
    seconds <- as.numeric(parts[3])
    total_seconds <- hours * 3600 + minutes * 60 + seconds
    return(total_seconds)
  }
  
  
  # Apply the time_to_numeric function to convert time strings to numeric (total seconds)
  times_dates$day_0_numeric <- sapply(times_dates$day_0_POSIXct, time_to_numeric)
  all_times$day_1_numeric <- sapply(all_times$day_1, time_to_numeric)
  all_times$day_2_numeric <- sapply(all_times$day_2, time_to_numeric)
  all_times$day_3_numeric <- sapply(all_times$day_3, time_to_numeric)
  
  # Calculate elapsed time in hours
  all_times$elapsed_day_0 <- c(0, diff(all_times$day_0_numeric) / 3600)
  
  merged_time$day_1_numeric <- sapply(merged_time$day_1_formatted, time_to_numeric)
  
  view(merged_time)
  
  
  merged_time$day_1 <- format(merged_time$day_1, format = "%H:%M:%S %p")
  merged_time$day_2 <- format(merged_time$day_2, format = "%H:%M:%S %p")
  merged_time$day_3 <- format(merged_time$day_3, format = "%H:%M:%S %p")
  merged_time$day_4 <- format(merged_time$day_4, format = "%H:%M:%S %p")
  
  view(merged_time)
  
  str(merged_time)
  print(merged_time[, c("day_1", "day_2", "day_3", "day_4")])
  
  ##make time increasing - this didnt work 
  merged_hours <- merged_time %>%
    mutate(
      time_sequence = pmap_dbl(select(., starts_with("day")), ~ as.numeric(difftime(..1, ..[1], units = "hours")))
    )
  
  ## only changed day 1, added todays date 
  for (i in 2:5) {
    all_merged[[i]] <- as.POSIXct(all_merged[[i]], format = "%H:%M:%S %p")
    
    print(sapply(all_merged[2:5], class))
  } 
  view(all_merged)
  
  # View the resulting data frame
  print(data)
  
  all_times_merged <- all_merged %>%
    mutate(`Day 1` = as.numeric(`Day 1`))
  
  str(all_times_merged)
  
  
  unite(col = date_time, Date, time, sep = " ") %>%
    mutate(date_time = ymd_hms(date_time)) %>% 
    
    mutate(start_time = min(date_time)) %>% 
    mutate(days = interval(start_time, date_time)/ddays(1))
}
