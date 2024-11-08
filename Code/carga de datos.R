library(haven)
library(dplyr)

ineval_2014_2015_raw <- read_sav("Data/INEVAL/SBAC15_micro_246169_20200130_SAV.sav")

ineval_2015_2016_raw <- read_sav("Data/INEVAL/SBAC16_micro_266442_20200130_SAV.sav")


juntar <- bind_rows(ineval_2014_2015_raw, ineval_2015_2016_raw)