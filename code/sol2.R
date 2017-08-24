## Signal Detection Theory tutorial in R
## Exercise 2 solution

### random variable distributed normal
x <- rnorm(1000)

# plotting an histogram
h <- hist(x)

# cumulative sum
p <- cumsum(h$counts)/sum(h$counts)

q <- h$mids
plot(q, p)

# it's inverse
plot(p,q)


### functions pnorm() and qnorm() do something similar

q <- seq(-3, 3, .01)
p <- pnorm(q)
plot(q, p, xlab='quantile / z', ylab='p')
title('cumulative normal fun (cdf)')

q <- qnorm(p)
plot(p, q, xlab='p', ylab='quantile/z')
title('quantile fun')

c <- dnorm(q)
plot(q,c, xlab='quantile / z', ylab='c')
title('normal fun (pdf)')


# EOF
