library(readr)

loc_data_august_expand <- read_csv("data/loc_data_august_expand.csv")
loc_data_august <- read_csv("data/loc_data_august.csv")[,-1]
loc_data_december_expand <- read_csv("data/loc_data_december_expand.csv")
loc_data_december <- read_csv("data/loc_data_december.csv")
kod_stacja <- read_csv('data/kod_stacja.csv')
dataset_august <- read_csv("data/dataset_august.csv")
dataset_december <- read_csv("data/dataset_december.csv")

