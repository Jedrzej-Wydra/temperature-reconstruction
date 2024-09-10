source("custom_coeficients_functions.R")

temptime <- 1:23
temprange <- c(1, 23)
results <- data.frame()
bk <- 3
distribution <- data.frame()
distribution2 <- data.frame()

for(loc in unique(data$location))
{
  tempbasis <- create.fourier.basis(temprange, nbasis=bk)
  weatherbasis <- create.fourier.basis(temprange, nbasis=bk)
  
  full_data_temperature_Y <- cbind(test_sets[[loc]]$Y, train_sets[[loc]]$Y)
  full_data_temperature_X <- cbind(test_sets[[loc]]$X, train_sets[[loc]]$X)
  
  tempfd <- smooth.basis(temptime, full_data_temperature_Y, tempbasis, method = 'qr')$fd
  weatherfd <- smooth.basis(temptime, full_data_temperature_X, weatherbasis, method = 'qr')$fd
  
  smoothed_full_data_temperature_Y <- eval.fd(1:23,tempfd)
  smoothed_full_data_temperature_X <- eval.fd(1:23,weatherfd)
  
  dists <- data.frame(location = loc,t(custom_coefs(smoothed_full_data_temperature_Y, tempfd)))
  rownames(dists) <- NULL
  
  distribution <- rbind(distribution, dists)
  
  dists2 <- data.frame(location = loc,t(custom_coefs(smoothed_full_data_temperature_X, weatherfd)))
  rownames(dists) <- NULL
  
  distribution2 <- rbind(distribution2, dists2)
  
  
}

set.seed(2024)

distribution %>%
  pivot_longer(const:cos) %>%
  filter(name == "const") %>%
  group_by(location) %>%
  mutate(normal = rnorm(n(), mean(value), sd(value))) %>%
  ggplot(aes(value)) +
  geom_density(fill = "#619CFF",
               color = "#619CFF",
               alpha = 0.2) +
  geom_density(aes(x = normal),
               fill = "#F8766D",
               color = "#F8766D",
               alpha = 0.2) +
  facet_wrap(~location, scales = "free_y") +
  theme_bw() +
  theme(legend.position = 'none') +
  xlim(15, 30) +
  ggtitle("Constant Coefficient")

set.seed(2024)

distribution %>%
  pivot_longer(const:cos) %>%
  filter(name == "sin") %>%
  group_by(location) %>%
  mutate(normal = rnorm(n(), mean(value), sd(value))) %>%
  ggplot(aes(value)) +
  geom_density(fill = "#619CFF",
               color = "#619CFF",
               alpha = 0.2) +
  geom_density(aes(x = normal),
               fill = "#F8766D",
               color = "#F8766D",
               alpha = 0.2) +
  facet_wrap(~location, scales = "free_y") +
  theme_bw() +
  theme(legend.position = 'none') +
  xlim(-15, 5) +
  ggtitle("Sinus Coefficient")

set.seed(2024)

distribution %>%
  pivot_longer(const:cos) %>%
  filter(name == "cos") %>%
  group_by(location) %>%
  mutate(normal = rnorm(n(), mean(value), sd(value))) %>%
  ggplot(aes(value)) +
  geom_density(fill = "#619CFF",
               color = "#619CFF",
               alpha = 0.2) +
  geom_density(aes(x = normal),
               fill = "#F8766D",
               color = "#F8766D",
               alpha = 0.2) +
  facet_wrap(~location, scales = "free_y") +
  theme_bw() +
  theme(legend.position = 'none') +
  xlim(-15, 5) +
  ggtitle("Cosinus Coefficient")

distribution %>%
  pivot_longer(const:cos) %>%
  filter(name == "const") %>%
  mutate(normal = rnorm(n(), mean(value), sd(value))) %>%
  ggplot(aes(value)) +
  geom_density(fill = "#619CFF",
               color = "#619CFF",
               alpha = 0.2) +
  geom_density(aes(x = normal),
               fill = "#F8766D",
               color = "#F8766D",
               alpha = 0.2) +
  theme_bw() +
  theme(legend.position = 'none') +
  xlim(15, 30) +
  ggtitle("Constant Coefficient")

set.seed(2024)

distribution %>%
  pivot_longer(const:cos) %>%
  filter(name == "sin") %>%
  mutate(normal = rnorm(n(), mean(value), sd(value))) %>%
  ggplot(aes(value)) +
  geom_density(fill = "#619CFF",
               color = "#619CFF",
               alpha = 0.2) +
  geom_density(aes(x = normal),
               fill = "#F8766D",
               color = "#F8766D",
               alpha = 0.2) +
  theme_bw() +
  theme(legend.position = 'none') +
  xlim(-15, 5) +
  ggtitle("Sinus Coefficient")

set.seed(2024)

distribution %>%
  pivot_longer(const:cos) %>%
  filter(name == "cos") %>%
  mutate(normal = rnorm(n(), mean(value), sd(value))) %>%
  ggplot(aes(value)) +
  geom_density(fill = "#619CFF",
               color = "#619CFF",
               alpha = 0.2) +
  geom_density(aes(x = normal),
               fill = "#F8766D",
               color = "#F8766D",
               alpha = 0.2) +
  theme_bw() +
  theme(legend.position = 'none') +
  xlim(-15, 5) +
  ggtitle("Cosinus Coefficient")

