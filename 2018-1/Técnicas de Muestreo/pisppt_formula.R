library(combinat)
pisppt <- function(X,n)
{
	N = length(X)
	XT = sum(X)
	m = combn(X,n)
	m = apply(m,2,permn)
	m = matrix(unlist(m),ncol=n,byrow=TRUE)
	nm = dim(m)[1]
	p=0
	for (j in 1:nm)
	{
		p[j] = prod(m[j,])/(XT*prod(XT-cumsum(m[j,1:n-1])))
	}
	pi1=0
	pi2=matrix(0,N,N)
	for (i in 1:(N-1))
	{
		aux1 = (m==X[i])
		index = which(apply(1*aux1,1,sum)==1)
		pi1[i] = sum(p[index])
		for (j in (i+1):N)
		{
			aux2 = (m==X[j])
			aux2 = 1*aux2[index,]
			pi2[i,j] = sum(p[index[which(apply(aux2,1,sum)==1)]])
		}
	}
	pi1[N] = n-sum(pi1)
	pi2 = pi2+t(pi2)
	list(pi1,pi2)
}
