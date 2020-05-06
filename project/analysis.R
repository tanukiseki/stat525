# analysis
library(ggplot2)
library(tidyr)
result = list(k_list[10001:50000], height_list[10001:50000], position_list[10001:50000])
fig1 = ggplot(data.frame("k" = result[[1]]), aes(x = k)) + geom_histogram(aes(y = stat(count) / sum(count)), binwidth = 0.5, color = "black", fill = "white") + xlim(0, 8)
fig1 + geom_vline(aes(xintercept=mean(k)),
                         color="blue", linetype="dashed", size=1) + scale_y_continuous("Frequency") + ggtitle("Number of change points")

k1 = data.frame(matrix(unlist(position_list[which(k_list == 1)]), nrow = length(which(k_list == 1)), ncol = 3, byrow = T))
mean(k1$X2)
fig2 = ggplot(k1, aes(x = X2)) + geom_density(color = "black", bw = 200)
fig2 + #geom_vline(aes(xintercept=mean(X2)),
#color="blue", linetype="dashed", size=1)
  scale_y_continuous("Density", labels = scales::comma) + scale_x_continuous("Position of change point (k = 1)")

k2 = data.frame(matrix(unlist(position_list[which(k_list == 2)]), nrow = length(which(k_list == 2)), ncol = 4, byrow = T))
mean(k2$X2)
fig3 = ggplot(k2) + geom_density(aes(x = X2), color = "#2EA9DF", bw = 500) + geom_density(aes(x = X3), color = "#261E47", bw = 500)
fig3
fig3+ scale_y_continuous("Density", labels = scales::comma) + scale_x_continuous("Position of change point (k = 2)")

k3 = data.frame(matrix(unlist(position_list[which(k_list == 3)]), nrow = length(which(k_list == 3)), ncol = 5, byrow = T))
fig4 = ggplot(k3) + geom_density(aes(x = X2), color = "#227D51", bw = 500) + geom_density(aes(x = X3), color = "#33A6B8", bw = 500) + geom_density(aes(x = X4), color = "#86473F", bw = 500)
fig4
fig4 + scale_y_continuous("Density", labels = scales::comma) + scale_x_continuous("Position of change point (k = 3)")

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Mode(k2$X2)
Mode(k2$X3)

h2 = data.frame(matrix(unlist(height_list[which(k_list == 2)]), nrow = length(which(k_list == 2)), ncol = 3, byrow = T))
fig5 = ggplot(h2) + geom_density(aes(x = X1), color = "#227D51", bw = 0.001) + geom_density(aes(x = X2), color = "#33A6B8", bw = 0.001) + geom_density(aes(x = X3), color = "#86473F", bw = 0.001)
fig5 + scale_y_continuous("Density", labels = scales::comma) + scale_x_continuous("Height (Rate) density of each period")
apply(h2, MARGIN = 2, mean)
