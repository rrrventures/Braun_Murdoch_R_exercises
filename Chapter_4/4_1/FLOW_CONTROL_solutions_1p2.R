###FLOW CONTROL. IF STATEMENT SECTION
###4.1.2

# 1)
## It fails with strings, and with vectors it runs but the if condition is not proper because condition length > 1.
## An error message would be useful

# 2)
# You would really just need : n = p1⋯pk + 1

# 3)

primes # the primes in the seq(2,1000)

j <- 1
counter <- 0
for (prime in primes) {
	if (prime == primes[j+1]-2) {
		counter <- counter + 1
	}
	j <- j + 1
	if (j == length(primes)) break  
}

counter

# 4)

GIC <- function(P, t){
      if (t <= 3) i <- 0.04 else i <- 0.05
      return(P*((1+i)^t -1))
    }

# 5)

mort <-  function(n,P,open){
	if open==TRUE i = 0.005 else i = 0.004
	R = P*i/(1 - (1 + i)^(-n))
}
