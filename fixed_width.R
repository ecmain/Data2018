library(tidyverse)

input_file <- read_table("data/NCDC-CDO-USC00356750.txt",
                         comment = "-")


print(input_file)