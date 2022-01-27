library(rnoaa)
library(tidyverse)
library(lubridate)
library(magrittr)
library(lmomco)

source('/home/zhoylman/drought_indicators/spi_app/R/gamma_fit.R')

spi_point_ghcn = function(precip_data, time_scale){
  #define some date based variables
  precip_data$day = yday(precip_data$time)
  precip_data$year = year(precip_data$time)
  precip_data$month = month(precip_data$time)
  
  output.list = list()
  
  #Start SPI calculation
  for(t in 1:length(time_scale)){
    for(i in rev((length(precip_data$time)-364):length(precip_data$time))){
      #calcualte index vectors of interest based on time
      first_date_breaks = which(precip_data$day == precip_data$day[i])
      second_date_breaks = first_date_breaks-(time_scale[t]-1)
      
      #if there are negative indexes remove last year (incomplete data range)
      #change this to remove all indexes from both vectors that are negative
      if(!all(second_date_breaks < 0)){
        pos_index = which(second_date_breaks > 0)
        first_date_breaks = first_date_breaks[c(pos_index)]
        second_date_breaks = second_date_breaks[c(pos_index)]
      }
      
      #create slice vectors and group by vectors
      for(j in 1:length(first_date_breaks)){
        if(j == 1){
          slice_vec = seq(second_date_breaks[j],first_date_breaks[j], by = 1)
          group_by_vec = rep(j,(first_date_breaks[j] - second_date_breaks[j]+1))
        }
        else{
          slice_vec = append(slice_vec, seq(second_date_breaks[j],first_date_breaks[j], by = 1))
          group_by_vec = append(group_by_vec, rep(j,(first_date_breaks[j] - second_date_breaks[j]+1)))
        }
      }
      
      #slice data for appropriate periods
      data_time_filter = precip_data %>%
        slice(slice_vec) %>%
        tibble::add_column(group_by_vec = group_by_vec)%>%
        group_by(group_by_vec)%>%
        dplyr::summarise(sum = sum(data, na.rm = T))
      
      #remove zeros because they cause the gamma dist to blow up to Inf
      data_time_filter$sum[data_time_filter$sum == 0] = 0.01
      
      #compute date time for day/year of interest
      date_time = as.POSIXct(paste(precip_data$day[first_date_breaks], precip_data$year[first_date_breaks], sep = "-"), format = "%j-%Y")
      
      #fit gamma distrobution to data
      fit.gamma = gamma_fit(data_time_filter$sum)
      #calcualte CDF values for the theoretical distrobution
      fit.cdf = pgamma(data_time_filter$sum, shape = fit.gamma$shape, rate = fit.gamma$rate)
      
      #Unbiased Sample Probability-Weighted Moments (following Beguer ́ıa et al 2014)
      #pwm = pwm.ub(data_time_filter$sum)
      #Probability-Weighted Moments to L-moments
      #lmoments_x = pwm2lmom(pwm)
      #fit gamma
      #fit.parglo = pargam(lmoments_x)
      #compute probabilistic cdf 
      #fit.cdf = cdfgam(data_time_filter$sum, fit.parglo)
      
      #equaprobaility transformation for cdf quantiles
      if(i == length(precip_data$time)){
        output.df = data.frame(time = date_time, 
                               spi = qnorm(fit.cdf, mean = 0, sd = 1))
      }
      
      else{
        output.df = rbind(output.df, data.frame(time = date_time, 
                                                spi = qnorm(fit.cdf, mean = 0, sd = 1)))
      } 
    }
    output.df = output.df[order(output.df$time),]
    output.list[[t]] = output.df
  }
  #if there is only one timescale to calculate return a data frame
  if(length(time_scale) == 1){
    return(output.df)
  }
  # otherwise return a list
  else{
    return(output.list)
  }
}

stations = ghcnd_stations()

filtered_stations = stations %>%
  filter(element == 'PRCP',
         first_year == quantile(.$first_year, 0.05),
         last_year == 2020,
         latitude > 0 & latitude < 50,
         longitude < -50) 

data_raw = ghcnd_search(
  filtered_stations$id[4],
  date_min = NULL,
  date_max = NULL,
  var = "PRCP",
)

data_filtered = data_raw %$%
  prcp %>%
  mutate(year = year(date)) %>%
  filter(year > 1901 & year < 2010) %>% # filter for full years
  select(date, prcp)%>%
  rename(time = date, data = prcp)

full_spi = spi_point_ghcn(data_filtered, 30)

years = 1:6

years_out = data.frame(years = years,
                       out = NA)
tictoc::tic()
set.seed(10)
for(X in years){
  random_years = sample(unique(year(data_filtered$time)), X, replace = F)
  test_year = sample(unique(year(data_filtered$time)),1)
  
  temp_filtered = data_filtered %>%
    filter(year(time) %in% c(random_years, test_year)) 
  
  temp_spi = spi_point_ghcn(temp_filtered, 30)
  
  temp_test = temp_spi %>% filter(year(time) == test_year)
  temp_real = full_spi %>% filter(year(time) == test_year)
  
  joined = left_join(temp_real, temp_test, by='time')
  
  error = joined$spi.x - joined$spi.y
  
  error[error == 'NaN'] = NA
  error_abs = abs(error)
  mae = mean(error_abs, na.rm = T)
  
  years_out$out[X] = mae
  print(X)
}
tictoc::toc()
plot(years_out$years, years_out$out)








###################################


error_comp = function(X){
  random_years = sample(unique(year(data_filtered$time)), X, replace = F)
  test_year = sample(unique(year(data_filtered$time)),1)
  
  temp_filtered = data_filtered %>%
    filter(year(time) %in% c(random_years, test_year)) 
  
  temp_spi = spi_point_ghcn(temp_filtered, 30)
  
  temp_test = temp_spi %>% filter(year(time) == test_year)
  temp_real = full_spi %>% filter(year(time) == test_year)
  
  joined = left_join(temp_real, temp_test, by='time')
  
  error = joined$spi.x - joined$spi.y
  
  error[error == 'NaN'] = NA
  error_abs = abs(error)
  mae = mean(error_abs, na.rm = T)
  
  return(mae)
  print(X)
}

library(doParallel)
cl = makeCluster(7)
registerDoParallel(cl)

years = 1:93
n.simulations = 20
set.seed(10)
monte_carlo = list()


monte_carlo = foreach(i = 1:n.simulations) %dopar% {
  library(tidyverse)
  library(lubridate)
  library(magrittr)
  library(lmomco)
  test = sapply(years, FUN = error_comp)
  test
}

stopCluster(cl)

merged = do.call('cbind', monte_carlo) %>%
  as.matrix() %>%
  apply(., 1, function(x){quantile(x, c(0.1,0.5, 0.9), na.rm = T)}) %>%
  t() %>%
  as.data.frame()

library(ggplot2)

plot = ggplot()+
  geom_ribbon(data = merged, aes(x = years, ymax = `90%`, ymin = `10%`), fill = "grey70")+
  geom_line(data = merged, aes(x = years, y = `50%`), size = 2)+
  theme_bw(base_size = 16)+
  geom_hline(yintercept=0.1, linetype = 'dashed')+
  geom_vline(xintercept = 45, linetype = 'dashed')+
  labs(y = 'Mean Absolute Error', x = 'Years in Climatology')
  

ggsave(plot, file = '/home/zhoylman/temp/drought_year_sensativity.png')




test = sapply(years, FUN = error_comp)
tictoc::toc()
mae = function(x){
  x[x == 'NaN'] = NA
  x_abs = abs(x)
  mae = mean(x_abs, na.rm = T)
}

mae_out = years_out %>%
  lapply(., mae)%>%
  unlist()
