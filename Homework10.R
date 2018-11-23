# Q1
cdfbinom <- function(q, n, p){
  answer <- c()
  for (k in 0 : q){
    a <- choose(n, k) * p^k * (1 - p)^(n - k)
    answer[k+1] = a
    sum(answer)
  }
    sum(answer)
}


cdfbinom(5, 10, 0.5)
pbinom(5, 10, 0.5)



# Q2
powerfunc <- function(n, delta, sd, alpha){
  pvalues <- c()
  for (i in 1 : 10000){
    x <- rnorm(n, delta, sd)
    t_stat <- t.test(x, mu = 0, conf.level = (1 - alpha))
    pvalues[i] <- t_stat$p.value
    power <- sum(pvalues < alpha) / 10000
  }
  power
}

powerfunc(30, 0.5, 1, 0.05)
power.t.test(n = 30, delta = 0.5, sd = 1, sig.level = 0.05, type = 'one.sample')
