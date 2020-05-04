# initialization

# four moves
# birth: b
# death: d
# height change: eta
# position change: pi

# prior of k
k_max = 10
lambda = 3

prior = dpois(c(0:k_max), lambda)
K = length(prior)

b = pmin(rep(1, length(prior)), c(prior[2:length(prior)], 0) / prior)
d = pmin(rep(1, length(prior)), c(0, prior[1:k_max]) / prior)
c = 0.9 / max(b + d)
b = c * b
d = c * d

pi_ = 0.5 * (1 - b - d)
pi_[1] = 0
eta = 1 - b - d - pi_