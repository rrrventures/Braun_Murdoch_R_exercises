###FLOW CONTROL, WHILE LOOP
###4.1.3

# 1) and 2)

#Decided to make the function different
fib <- 1

while (tail(fib,1) < 300) {
	fib <- c(fib,sum(tail(fib,2)))
}

# 3)

fib <- 1

while (max(fib) < 1000000) {
	fib <- c(fib,sum(tail(fib,2)))
}

nums <- length(fib)-1 # 30 different fib numbers are less than 1000000


# 4)

i <- 0
i0 <- 0.006
while ( abs(i - i0) >= 0.000001){
	i <- i0
	i0 <- (1 - (1 + i)^(-20))/19
}
i # 0.004955

#5)

i <- 0
i0 <- 0.006
counter <- 0
while ( abs(i - i0) >= 0.000001){
	i <- i0
	i0 <- (1 - (1 + i)^(-20))/19
	counter <- counter + 1
}