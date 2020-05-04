# read
source("project/initialization.R")
source("project/move.R")
# data process
library(boot)
#data = coal

data = c(rpois(100, 3), rpois(200, 2), rpois(200, 0.8))
L = length(data)
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
  position_list[[i]] = s
  height_list[[i]] = h
  k_list[i] = k
}