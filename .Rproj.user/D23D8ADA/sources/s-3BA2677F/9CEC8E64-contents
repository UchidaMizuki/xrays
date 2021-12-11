library(tidyverse)
library(rlang)

data <- expand_grid(from = letters[1:3],
                    to = factor(letters[4:5]),
                    mode = letters[6:7]) %>%
  mutate(value = row_number())
data <- xtabs(value ~ from + to + mode, data)

call_data <- attr(data, "call")

call_data$formula %>%
  rlang::f_lhs() %>%
  as_name()

f_lhs(call_data$formula)

call_data$formula %>%
  f_rhs() %>%
  rlang::is_call_stack()

library(labelled)
data <- tibble(a = 1, b = 2)
data %>%
  look_for("a")
