###FLOW CONTROL, REPEAT LOOP
##4.1.5

# 1) 
# a)
	
tolerance <- 0.000001
x1 = 0
x2 = 2

repeat {

	x = (x1 + x2)/2

	f1 <- x1^3 + 2*x1^2 - 7
	f2 <- x2^3 + 2*x2^2 - 7

	if (abs(f1-f2) < tolerance) break

	f  <- x^3 + 2*x^2 - 7

	if (sign(f) != sign(f1)) {
		x2 <- x 
	} else {
		x1 <- x

	}

}

# b)
# Weierstrass Aproximation theorem guarantees a polynomial approximation for any continuous function on
# a closed interval. We can run the previous algorithm with a counter.





