### FLOW CONTROL, NEWTONS METHOD
###4.1.4

# 1)

x <- 0
f <- x^7 + 10000*x^6 + 1.06 * x^5 + 10600*x^4 + 0.0605 * x^3 + 605 * x^2 + 0.0005 *x +5
tolerance <- 0.000001
count <- 0


while (abs(f) > tolerance) {
	f_prime <- 7*x^6 + 6*10000*x^5 + 5*1.06*x^4 + 4*10600*x^3 + 3*0.0605*x^2 + 2*605 *x + 0.0005
	x <- x - f/f_prime
	f <- x^7 + 10000*x^6 + 1.06 * x^5 + 10600*x^4 + 0.0605 * x^3 + 605 * x^2 + 0.0005 *x +5
	count <- count + 1
}
count # 1 iteration

# 2) & 3) are just replacing values of the above code

# 4) Since the two elements are positive and larger than x, the minimun will be the nearest zero number of the function
# we can use the same newton method to attempt to find the root
# or take the derivative and find the zero with newton´s method


# 5) Has only one root x = 3/5. A) B) and C) approach 1, and last one does not converge

# 6) Has root 3. Behaviour is just replacing in newtons method

# 7)

i <- 0.006
f <- (1 - (1 + i)^(-20))/19 - i
tolerance <- 0.000001
count <- 0

while (abs(f) > tolerance) {
	f_prime <- (20/19)*((1+i)^(-21)) - 1
	i <- i - f/f_prime
	f <- (1 - (1 + i)^(-20))/19 - i
	count <- count + 1
}
count # 2 iterations
i # 0.004939979

