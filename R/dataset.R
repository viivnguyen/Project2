library(readxl)
mouse_data <- read_excel("data/mousedata.xlsx")
save(mouse_data, file = "mouse_data.RData")
