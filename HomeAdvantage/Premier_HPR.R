devtools::install_github("dashee87/footballR")
library(footballR)
library(purrr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(RCurl)


england <- rbind(england, subset(england_current(), !(Date %in% england$Date & home %in% england$home)))

#subset
EPL <- rbind(england, england_current()) %>%
  subset(tier == 1 & Season %in% 1992:2020)

tail(EPL, 5)

