### 4.2 MANAGING COMPLEXITY THROUGH FUNCTIONS

###4.2.1

# 1)
# a simple str(function) does the job

# 2)
# a)

compound.interest <- function(P,i.r,n) {
	P*(1 + i.r)^n
}

#b)
#[1] 1347.849


# 3)



bisect <- function(x1, x2, fun, tolerance = 0.000001) {

	repeat {

		x = (x1 + x2)/2

		f1 <- fun(x1)
		f2 <- fun(x2)

		if (abs(f1-f2) < tolerance) break

		f  <- fun(x)

		if (sign(f) != sign(f1)) {
			x2 <- x 
		} else {
			x1 <- x

		}

	}
	return(x)
}