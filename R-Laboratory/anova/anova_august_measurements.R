locations <- unique(loc_data_august$location)
locations <- locations[-2]

temptime <- 1:23
temprange <- c(1, 23)
tempbasis <- create.fourier.basis(temprange, nbasis=3)

df <- data.frame()

for(col in 1:8)
{
  temperature <- dplyr::select(filter(loc_data_august, location == locations[col]), -location)
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
  
}

rownames(df) <- NULL

df %>%
  mutate(cos = ifelse(cos > 50, 1, cos),
         sin = ifelse(sin > 10, 1, sin),
         sin = ifelse(sin < -20, 1, sin),
         sin = ifelse(sin < -7, 1, sin)) %>%
  pivot_longer(const:cos) %>%
  mutate(ID = as.factor(ID)) %>%
  ggplot(aes(X, value, color = ID, group = ID)) +
  geom_line() +
  facet_wrap(~name, nrow = 3) +
  theme_bw()

df %>%
  mutate(cos = ifelse(cos > 50, 1, cos),
         sin = ifelse(sin > 10, 1, sin),
         sin = ifelse(sin < -20, 1, sin),
         sin = ifelse(sin < -7, 1, sin)) %>%
  pivot_longer(const:cos) %>%
  group_by(ID, name) %>%
  summarise(mean = mean(value),
            sd = sd(value)) %>% 
  dplyr::select(-sd) %>%
  pivot_wider(names_from = name,
              values_from = mean)

df %>%
  mutate(cos = ifelse(cos > 50, 1, cos),
         sin = ifelse(sin > 10, 1, sin),
         sin = ifelse(sin < -20, 1, sin),
         sin = ifelse(sin < -7, 1, sin)) %>%
  pivot_longer(const:cos) %>%
  group_by(ID, name) %>%
  summarise(mean = mean(value),
            sd = sd(value)) %>% 
  dplyr::select(-mean) %>%
  pivot_wider(names_from = name,
              values_from = sd)

summary(aov(const ~ ID, data = df))
summary(aov(sin ~ ID, data = df))
summary(aov(cos ~ ID, data = df))


