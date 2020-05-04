# move



# height change
height_change = function() {
	index = sample.int(k, size = 1)
	h_old = h[index]
	h_new = exp(runif(1, -0.5, 0.5)) * h_old
	y = data[which(x >= s[index] & (data < s[index + 1]))]
	likelihood_ratio = sum(log(dpois(y, h_new))) / sum(log(dpois(y, h_old)))
	a = min(1, likelihood_ratio * (h_new / h_old)^alpha * exp(-beta * (h_new - h_old)))
	u = runif(1)
	if (u <= a) {
		h[index] = h_new
	}
}

position_change = function() {
	index = sample.int(k, size = 1) + 1
	s_old = s[index]
	s_new = runif(1, s[index - 1], s[index + 1])
	y_old_1 = data[which(x >= s[index - 1] & (x < s_old))]
	y_old_2 = data[which((x >= s_old) & (x < s[index + 1]))]
	y_new_1 = data[which(x >= s[index - 1] & (x < s_new))]
	y_new_2 = data[which(x >= s_new & x < s[index + 1])]
	likelihood_ratio = (sum(log(dpois(y_new_1, h[index-1]))) + sum(log(dpois(y_new_2, h[index])))) / (sum(log(dpois(y_old_1, h[index-1]))) + sum(log(dpois(y_old_2, h[index]))))
	a = min(1, likelihood_ratio * (s[index+1] - s_new) * (s_new - s[index-1]) / ((s[index+1] - s[index]) * (s[index] - s[index-1])))
	u = runif(1)
	if (u <= a) {
		s[index] = s_new
	}
}

birth = function() {
	s_new = runif(1, 0, L)
	while(1) {
		if (s_new %in% s) {
			s_new = runif(1, 0, L)
		} else {
			break
		}
	}

	for (i in 1:(k+1)) {
		if (s_new >= s[i] && s_new < s[i+1]) {
			index = i
		}
	}
	y_old = data[which(x >= s[index] & x < s[index+1])]
	y_new_1 = data[which(x >= s[index] & x < s_new)]
	y_new_2 = data[which(x >= s_new & x < s[index + 1])]
	u = runif(1)
	h_new_1 = exp(((s[index+1] - s[index]) / (s_new - s[index]) * log(h[index]) - (s[index+1] - s_new) / (s_new - s[index]) * log((1-u)/u)) / (s[index+1] - s[index]))
	h_new_2 = (1 - u) / u * h_new_1
	likelihood_ratio = (sum(log(dpois(y_new_1, h_new_1))) + sum(log(dpois(y_new_2, h_new_2)))) / sum(log(dpois(y_old, h[index])))
	k_prior = prior[k+2] / prior[k+1]
	s_prior = 2 * (k + 1) * (2 * k + 3) / L^2 * (s_new - s[index]) * (s[index+1] - s_new) / (s[index+1] - s[index])
	h_prior = beta^alpha / gamma(alpha) * (h_new_1 * h_new_2 / h[index])^(alpha-1) * exp(-beta * (h_new_1 + h_new_2 - h[index]))
	proposal_ratio = d[k+2] * L / (b[k+1] * (k + 1))
	Jacobian = (h_new_1 + h_new_2)^2 / h[index]
	a = min(1, likelihood_ratio * k_prior * s_prior * h_prior * proposal_ratio * Jacobian)
	u = runif(1)
	if (u <= a) {
		k = k + 1
		s = c(s[1:index], s_new, s[index+1:k+1])
		if (index == 1) {
			h = c(h_new_1, h_new_2, h[-1])
		} else if (index == length(h)) {
			h = c(h[-length(h)], h_new_1, h_new_2)
		} else {
			h = c(h[1:(index-1)], h_new_1, h_new_2, h[(index+1):length(h)])
		}
	}
}

death = function() {
	index = sample.int(k, size = 1) + 1
	y_old_1 = data[which(x >= s[index - 1] & (x < s[index]))]
	y_old_2 = data[which(x >= s[index] & (data < s[index + 1]))]
	y_new = data[which(x >= s[index-1] & (data < s[index+1]))]

	h_new = h[index-1]^((s[index] - s[index-1]) / (s[index+1] - s[index-1])) * h[index]^((s[index+1] - s[index]) / (s[index+1] - s[index-1]))
	likelihood_ratio = sum(log(dpois(y_new, h_new))) / (sum(log(dpois(y_old_1, h[index-1]))) + sum(log(dpois(y_old_2, h[index]))))
	k_prior = prior[k] / prior[k+1]
	s_prior = L^2 / (2 * k * (2 * k + 1)) * (s[index+1] - s[index-1]) / (s[index+1] - s[index]) / (s[index] - s[index-1])
	h_prior = beta^alpha / gamma(alpha) * (h_new/ h[index-1] / h[index])^(alpha-1) * exp(-beta * (h_new - h[index-1] - h[index]))
	proposal_ratio = b[k] * k / (d[k+1] * L)
	Jacobian = h_new / (h[index-1] + h[index])^2
	a = min(1, likelihood_ratio * k_prior * s_prior * h_prior * proposal_ratio * Jacobian)
	u = runif(1)
	if (u <= a) {
		k = k - 1
		s = s[-index]
		if (index == 2) {
			h = c(h_new, h[(index+1):length(h)])
		} else if (index == length(h)) {
			h = c(h[1:(index-2)], h_new)
		} else {
			h = c(h[1:(index-2)], h_new, h[index+1:length(h)])
		}
	}
}