temptime <- 1:23
temprange <- c(1, 23)
tempbasis <- create.fourier.basis(temprange, nbasis=3)

dataset_august %>%
  pivot_longer(temp_105:temp_330) %>%
  ggplot(aes(date, value, color = name)) +
  geom_line()

df1 <- NULL
df2 <- NULL
df3 <- NULL

weather <- dataset_august[,c(1,4)]
names(weather) <- c('date', 'Y')
weather %>%
  transmute(rep = ifelse(str_length(as.character(day(date) - 1)) == 1, paste0('day_0', day(date) - 1), paste0('day_', day(date) - 1)),
            #transmute(rep = ifelse(str_length(as.character(day(date) - 4)) == 1, paste0('day_0', day(date) - 4), paste0('day_', day(date) - 4)),
            
            value = Y,
            hour = hour(date)) %>% 
  filter(hour != 0) %>%
  #mutate(hour = ifelse(hour < 5, hour + 23, hour)) %>% 
  #arrange(rep, hour) %>% 
  pivot_wider(names_from = rep, values_from = value) %>%
  dplyr::select(-hour, -day_16) %>%
  as.matrix() -> weather_data

weatherfd <- smooth.basis(temptime, weather_data, tempbasis, method = 'qr')$fd
control <- as.data.frame(t(custom_coefs(eval.fd(1:23,weatherfd), weatherfd)))

control %>%
  mutate(const = const/lag(const),
         sin = sin/lag(sin),
         cos = cos/lag(cos)) %>%
  replace_na(replace = list(const = 1, sin = 1, cos = 1)) %>%
  mutate(ID = col,
         X = 1:15) -> control

for(col in 1:7)
{
  temperature <- dataset_august[,c(1, col+1)]
  names(temperature) <- c('date', 'Y')
  temperature %>%
    transmute(rep = ifelse(str_length(as.character(day(date) - 1)) == 1, paste0('day_0', day(date) - 1), paste0('day_', day(date) - 1)),
              #transmute(rep = ifelse(str_length(as.character(day(date) - 4)) == 1, paste0('day_0', day(date) - 4), paste0('day_', day(date) - 4)),
              
              value = Y,
              hour = hour(date)) %>% 
    filter(hour != 0) %>%
    #mutate(hour = ifelse(hour < 5, hour + 23, hour)) %>% 
    #arrange(rep, hour) %>% 
    pivot_wider(names_from = rep, values_from = value) %>%
    dplyr::select(-hour, -day_16) %>%
    as.matrix() -> temp_data
  
  tempfd <- smooth.basis(temptime, temp_data, tempbasis, method = 'qr')$fd
  pre_data_rows <- as.data.frame(t(custom_coefs(eval.fd(1:23,tempfd), tempfd)))
  
  pre_data_rows %>%
    mutate(const = const/lag(const),
           sin = sin/lag(sin),
           cos = cos/lag(cos)) %>%
    replace_na(replace = list(const = 1, sin = 1, cos = 1)) %>%
    mutate(ID = col,
           X = 1:15) -> data_rows
  
  df <- bind_rows(df, data_rows)
  
  df1 <- c(df1, mean(abs(data_rows$const - control$const)))
  df2 <- c(df2, mean(abs(data_rows$sin - control$sin)))
  df3 <- c(df3, mean(abs(data_rows$cos - control$cos)))
  
}

rownames(df) <- NULL

mean(df1)
mean(df2)
mean(df3)
