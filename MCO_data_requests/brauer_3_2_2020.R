library(tidyverse)
library(plyr)
library(dplyr)
library(stringr)

#read in original data
data = read_csv('/home/zhoylman/MCO/MCO_data_requests/temp_data/NE_Race_1970-2010.csv') 

#define the collumn name identifiers
IDs = c("AA", "AB", "AC", "AD", "AE")

#define the column prefix to rename collumns int he same way
col_prefex = "B18"

#extract the metadata
original_meta = data[,1:7]

#define the interpolation method (spline or linear)
interp_method = "spline" 

#loop though ids (nested for loop)
for(i in 1:length(IDs)){
  #select the collumns that match the id
  temp = data %>%
    select(., contains(IDs[i]))
  
  #generate the new matrix (expand to annual collumns)
  new_collumns = matrix(nrow = length(data$GISJOIN), ncol = ((ncol(temp)-1)*10)+1) %>%
    data.frame()
  
  #determine the year range for interpolation
  year_range = stringr::str_extract_all(colnames(temp), "\\d{4}") %>%
    unlist() %>%
    as.numeric() %>%
    range()
  
  #rename new collumns in the same format as original data
  colnames(new_collumns) = paste0(col_prefex, IDs[i], seq(year_range[1], year_range[2]))
  
  #join the original data with the new dataframe
  temp_expand = plyr::join(temp, new_collumns) %>%
    select(order(colnames(.)))
  
  #run the interpolation by row (loop through rows) based on interp_method definition
  for(r in 1:nrow(temp_expand)){
    if(interp_method == 'spline'){
      temp_expand[r,] = spline(year_range[1]: year_range[2],temp_expand[r,], n = ((ncol(temp)-1)*10)+1)$y
    }
    if(interp_method == "linear"){
      temp_expand[r,] = approx(year_range[1]: year_range[2], temp_expand[r,], method="linear", n = ((ncol(temp)-1)*10)+1)$y
    }
  }
  
  #if this is the first of the ids than create the initial data frame for data storage
  if(i == 1){
    out_df = cbind(original_meta, temp_expand)
  }
  #if its not the first time then append the collumns
  else{
    out_df = cbind(out_df, temp_expand)
  }
}

#write out the new data frame (annual)
write.csv(out_df, paste0("/home/zhoylman/MCO/MCO_data_requests/temp_data/NE_Race_1970-2010_annual_", interp_method, ".csv"))

#check interpolation (simple visualization for sanity)
#define the parameters (id and row)
ID = "AB"
row = 75

#pull original data
orig = data[,8:length(data)] %>%
  select(., contains(ID))

#define original time
orig_time = stringr::str_extract_all(colnames(orig), "\\d{4}") %>%
  unlist()%>%
  as.numeric()

#pull new data
new = out_df[,8:length(out_df)] %>%
  select(., contains(ID))

#define new time 
new_time = stringr::str_extract_all(colnames(new), "\\d{4}") %>%
  unlist()%>%
  as.numeric()

#plot (points represent new data, line represents interpolated data)
plot(orig_time, orig[row,], ylim = c(min(new[row,]), max(new[row,])), xlab = "Time", ylab = "Data",
     main = original_meta$COUNTY[row])
lines(new_time, new[row,])
