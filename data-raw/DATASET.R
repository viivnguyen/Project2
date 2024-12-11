## code to prepare `DATASET` dataset goes here

# Import Dataset
system("kaggle datasets download -d footprintnetwork/ecological-footprint --path data-raw --unzip")

usethis::use_data(dataset, overwrite = TRUE)
