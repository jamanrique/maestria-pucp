# << Ingreso de datos >>
rm(list=ls())
x=c(10.00000,11.42857,12.85714,14.28571,15.71429,17.14286,18.57143, 20.00000,21.42857,22.85714,24.28571,25.71429,27.14286,28.57143, 30.00000,10.00000)
y=c(17.45508,29.99034,28.61538,37.67610,38.23564,40.96085,31.56744, 41.43031,51.52707,55.30979,51.75880,57.15709,61.50122,64.73264,58.27243,60.00000)

# << Declaración de variables >>

n = length(x)
b0 = numeric()
b1 = numeric()
sg = numeric()

N = 10000
set.seed(68493)

b0[1] = lm(y~x)$coefficients[1]
b1[1] = lm(y~x)$coefficients[2]
sg[1] = 1
w = rbeta(n,0.5,1)

# << Gibbs >>

for(h in 1:N){
  b0[h+1] = rnorm(1,mean = sum((y-b1[h]*x)*w)/sum(w),sd = sqrt(sg[h]/sum(w)))
  b1[h+1] = rnorm(n = 1,mean = (sum(w*x*(y-b0[h+1])))/sum(w*x^2),sd = sqrt(sg[h]/sum(w*x^2)))
  sg[h+1] = 1/rgamma(n = 1,n/2,(1/2)*sum(w*(y-b0[h+1]-b1[h+1]*x)^2))
  w[h+1] = rexp(n = 1,((0.5*sg[h+1]*(y-b0[h+1]-b1[h+1]*x)^2)))
}
ts.plot(b0)
ts.plot(b1)
ts.plot(sg)

b0 = b0[500:10000]
b1 = b1[500:10000]
sg = sg[500:10000]

plot(x,y)
abline(lm(y~x),lwd=2,col=2)
abline(a=mean(b0),b=mean(b1),col=44)