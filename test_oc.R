# Calculate overlap between two samples

library(sfsmisc)

mu1=1
mu2=0
p1=0.5
p2=0.5

oc=numeric(100)
md=numeric(100)

for (j in 1:100)
{
  
x1=numeric(1000)
x2=numeric(1000)

x1[1]=rnorm(1,mean=mu1,sd=1)
x2[1]=rnorm(1,mean=mu2,sd=1)

for (i in 2:1000)
{
  x1[i]=rnorm(1,mean=mu1+p1*x1[i-1],sd=1)
  x2[i]=rnorm(1,mean=mu2+p2*x2[i-1],sd=1)
  
}

# define limits of a common grid, adding a buffer so that tails aren't cut off
lower <- min(c(x1,x2)) - 10 
upper <- max(c(x1,x2)) + 10

# generate kernel densities
da <- density(x1, from=lower, to=upper)
db <- density(x2, from=lower, to=upper)
d <- data.frame(x=da$x, a=da$y, b=db$y)

# calculate intersection densities
d$w <- pmin(d$a, d$b)

# integrate areas under curves
total <- integrate.xy(d$x, d$a) + integrate.xy(d$x, d$b)
intersection <- integrate.xy(d$x, d$w)

# compute overlap coefficient
overlap <- 2 * intersection / total
oc[j]=overlap
md[j]=sqrt((mean(x1)-mean(x2))^2)
}

boxplot(md)
mean(md)

boxplot(oc)
mean(oc)


plot(da)
lines(db)
lines(d$x,d$w,col='red')
