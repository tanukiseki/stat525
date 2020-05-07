# move



# height change
height_change = function() {
	index = sample.int(1:length(h), size = 1)
	h_old = h[index]
	h_new = exp(runif(1, -0.5, 0.5)) * h_old
	y = data[which(x >= s[index] & (x < s[index + 1]))]
	likelihood_ratio = sum(log(dpois(y, h_new))) - sum(log(dpois(y, h_old)))
	a <<- min(1, exp(likelihood_ratio) * dgamma(h_new, shape = alpha, rate = beta) / dgamma(h_old, shape = alpha, rate = beta))
	u = runif(1)
	if (u <= a) {
		h[index] <<- h_new
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
	likelihood_ratio = (sum(log(dpois(y_new_1, h[index-1]))) + sum(log(dpois(y_new_2, h[index])))) - (sum(log(dpois(y_old_1, h[index-1]))) + sum(log(dpois(y_old_2, h[index]))))
	a <<- min(1, exp(likelihood_ratio) * (s[index+1] - s_new) * (s_new - s[index-1]) / ((s[index+1] - s[index]) * (s[index] - s[index-1])))
	u = runif(1)
	if (u <= a) {
		s[index] <<- s_new
	}
}

birth = function() {
	s_new <<- runif(1, 0, L)
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
	temp1<-((1-u)/u)^((s[index+1]-s_new)/(s[index+1]-s[index]))
	temp2<-(u/(1-u))^((s_new-s[index])/(s[index+1]-s[index]))
	h_new_1<-h[index]/temp1
	h_new_2<-h[index]/temp2
	likelihood_ratio = (sum(log(dpois(y_new_1, h_new_1))) + sum(log(dpois(y_new_2, h_new_2)))) - sum(log(dpois(y_old, h[index])))
	k_prior = prior[k+2] / prior[k+1]
	s_prior = 2 * (k + 1) * (2 * k + 3) / L^2 * (s_new - s[index]) * (s[index+1] - s_new) / (s[index+1] - s[index])
	h_prior = dgamma(h_new_1, shape = alpha, rate = beta) * dgamma(h_new_2, shape = alpha, rate = beta) / dgamma(h[index], shape = alpha, rate = beta)
	proposal_ratio = d[k+2] * L / (b[k+1] * (k + 1))
	Jacobian = (h_new_1 + h_new_2)^2 / h[index]
	a <<- min(1, exp(likelihood_ratio) * k_prior * s_prior * h_prior * proposal_ratio * Jacobian)
	if (runif(1) <= a) {
		k <<- k + 1
		s <<- c(s[1:index], s_new, s[(index+1):(k+1)])
		if (index == 1) {
			h <<- c(h_new_1, h_new_2, h[-1])
		} else if (index == length(h)) {
			h <<- c(h[-length(h)], h_new_1, h_new_2)
		} else {
			h <<- c(h[1:(index-1)], h_new_1, h_new_2, h[(index+1):length(h)])
		}
	}
}

death = function() {
	index = sample.int(k, size = 1) + 1
	y_old_1 = data[which(x >= s[index - 1] & (x < s[index]))]
	y_old_2 = data[which(x >= s[index] & (x < s[index + 1]))]
	y_new = data[which(x >= s[index-1] & (x < s[index+1]))]

	h_new = h[index-1]^((s[index] - s[index-1]) / (s[index+1] - s[index-1])) * h[index]^((s[index+1] - s[index]) / (s[index+1] - s[index-1]))
	likelihood_ratio = sum(log(dpois(y_new, h_new))) - (sum(log(dpois(y_old_1, h[index-1]))) + sum(log(dpois(y_old_2, h[index]))))
	k_prior = prior[k] / prior[k+1]
	s_prior = L^2 / (2 * k * (2 * k + 1)) * (s[index+1] - s[index-1]) / (s[index+1] - s[index]) / (s[index] - s[index-1])
	h_prior = dgamma(h_new, shape = alpha, rate = beta) / dgamma(h[index-1], shape = alpha, rate = beta) / dgamma(h[index], shape = alpha, rate = beta)
	proposal_ratio = b[k] * k / (d[k+1] * L)
	Jacobian = h_new / (h[index-1] + h[index])^2
	a <<- min(1, exp(likelihood_ratio) * k_prior * s_prior * h_prior * proposal_ratio * Jacobian)
	u = runif(1)
	if (u <= a) {
		k <<- k - 1
		s <<- s[-index]
		if (index == 2) {
		  if (length(h) == 2) {
		    h <<- h_new
		  } else {
		    h <<- c(h_new, h[(index+1):length(h)])
		  }
		} else if (index == length(h)) {
			h <<- c(h[1:(index-2)], h_new)
		} else {
			h <<- c(h[1:(index-2)], h_new, h[(index+1):(length(h))])
		}
	}
}
