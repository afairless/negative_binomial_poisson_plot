plot_negative_binomial_convergence_to_poisson <- function(lambda, step = 0.01) {
  # this function plots how the negative binomial distribution converges to a
  #   Poisson distribution as the parameter 'p' in 'nbinom' approaches 1 from
  #   zero

  # set up vector of 'p'/probability values, then calculate corresponding 'n's
  #   given 'lambda', which is the mean of Poisson distribution and its
  #   equivalent negative binomial distribution
  ps <- seq(0.05, 0.95, step)
  ps <- append(ps, 0.99)
  ns <- vector("numeric", length(ps))
  for (i in seq_along(ps)) {
    p <- ps[i]
    n <- (lambda * p) / (1 - p)
    ns[i] <- n
  }

  # plot Poisson distribution
  x <- 1:(lambda * 2)
  y <- dpois(x, lambda)
  plot(y ~ x, pch = 16)

  # plot the converging negative binomial distributions
  for (i in 5:length(ps)) {
    p <- ps[i]
    n <- ns[i]
    d <- dnbinom(x, n, p)

    # NOTE: negative binomial generates discrete data, not continuous data, but
    #   data are plotted continuously here to make curves visually clearer
    points(d ~ x, col = 3, pch = 16, type = "l")
  }

  title_text <- paste(
    "Poisson distribution (black dots), lambda = ", lambda,
    "\nNegative Binomial distributions (green curves)"
  )
  title(main = title_text)
}


lambda <- 3
filename <- paste(
  "negative_binomial_converge_to_poisson_lambda_", lambda, ".png",
  sep = ""
)
png(filename, width = 640, height = 400)
plot_negative_binomial_convergence_to_poisson(lambda, 0.01)
dev.off()


lambda <- 8
filename <- paste(
  "negative_binomial_converge_to_poisson_lambda_", lambda, ".png",
  sep = ""
)
png(filename, width = 640, height = 400)
plot_negative_binomial_convergence_to_poisson(lambda, 0.01)
dev.off()


lambda <- 20
filename <- paste(
  "negative_binomial_converge_to_poisson_lambda_", lambda, ".png",
  sep = ""
)
png(filename, width = 640, height = 400)
plot_negative_binomial_convergence_to_poisson(lambda, 0.01)
dev.off()
