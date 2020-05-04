# read
source("project/initialization.R")
source("project/move.R")
# data process
library(boot)
library(dplyr)
#data = coal
library(readr)
data <- read_csv("project/data.csv")
#data = round(boot::coal) %>% 
  group_by(date) %>% 
  count()
day = floor(data$x) 
L = day[length(day)]
data = c()
for (i in 1:L) {
  data[i] = ifelse(i %in% day, 1, 0)
}
x = c(1:L)

lambda = 3
k = 5#rpois(1, lambda = lambda)
alpha = 5
beta = 10

s = c(0, sort(runif(k, min = 0, max = L)), L) #length: k + 2, s[1] = 0, s[k + 2] = L
h = rgamma(k+1, alpha, beta)# length: k + 1


# jump = function() {
#   u = runif(1)
#   if (u <= b[k+1]) {
#     birth()
#   } else if (b[k+1] < u & u <= b[k+1]+d[k+1]) {
#     death()
#   } else if (b[k+1]+d[k+1] < u & u <= b[k+1]+d[k+1]+eta[k+1]) {
#     height_change()
#   } else {
#     position_change()
#   }
# }

position_list = list()
height_list = list()
k_list = c()
accept = c()
for (i in 1:10000) {
  u = runif(1)
  if (u <= b[k+1]) {
    birth()
  } else if (b[k+1] < u & u <= b[k+1]+d[k+1]) {
    death()
  } else if (b[k+1]+d[k+1] < u & u <= b[k+1]+d[k+1]+eta[k+1]) {
    height_change()
  } else {
    position_change()
  }
  
  accept[i] = a
  position_list[[i]] = s
  height_list[[i]] = h
  k_list[i] = k
}
