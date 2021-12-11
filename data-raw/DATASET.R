library(tidyverse)
library(rlang)

pkgload::load_all()

xrays(tibble(a = 1, b = 3), value = "a")
