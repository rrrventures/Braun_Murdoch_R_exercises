##End of chapter 4 excersises of Braun and Murdoch Book

##Vectorized version
directpoly <- function(x,c) {

	if (length(x) == 1 && is.numeric(x) && is.numeric(c)) {
		expo <- 0:(length(c)-1)
		result <- sum((x^expo)*c)
		return(result)
	} else {

		warning("check if x is a numeric length 1 and c a numeric vector")

	}

}


##Non vectorized

directpoly <- function(x,c) {
	
	exp<-0:(length(c)-1)
	vec<-rep(0,length(c))
	lista<-vector("list", length(x))
	o<-0

	for (j in x){
	    o<-o+1

		for (i in exp){
		vec[i+1]<-j^i
		}

		lista[[o]]<-sum(c*vec)

		}

		return(lista)
}


hornerpoly <- function(x,c) {

	xlist <- vector("numeric",length(x))
	len <- length(c)
	a<-c(rep(0,(length(c)-1)),tail(c,1))

	for (j in 1:length(xlist)){


		for (i in (len-1):1) {
			a[i]<-a[i+1]*x[j] + c[i]
		}

		xlist[j]<-a[1]
	}

	return(xlist)
}


#########################
## 4.4.1 - Newtons method
#########################

x <- 2.9
f <- (x-3)*exp(-x)
tolerance <- 0.000001
count <- 0

system.time(

while (abs(f) > tolerance) {
	f_prime <- exp(-x)*(4-x)
	x <- x - f/f_prime
	f <- (x-3)*exp(-x)
	}

)

##########################
## 4.4.2 - Newtons method
#########################


x <- 2.9
f <- (x^2 - 6*x + 9)*exp(-x)
tolerance <- 0.000001

system.time(

while (abs(f) > tolerance) {
	f_prime <- -exp(-x)*(x^2 - 8*x + 15)
	x <- x - f/f_prime
	f <- (x^2 - 6*x + 9)*exp(-x)
	}

)

###########################
##4.5.1 Bisection algorithm
###########################


tolerance <- 0.000001
x1 = 2.1
x2 = 3.1

repeat {

	x = (x1 + x2)/2

	f1 <- (x1-3)*exp(-x1)
	f2 <- (x2-3)*exp(-x2)

	if (abs(f1-f2) < tolerance) break

	f <- (x-3)*exp(-x)

	if (sign(f) != sign(f1)) {
		x2 <- x 
	} else {
		x1 <- x

	}



}

############################
##4.5.2 Bisection algorithm
###########################

	
tolerance <- 0.000001
x1 = 2.1
x2 = 3.1

repeat {

	x = (x1 + x2)/2

	f1 <- (x1^2 - 6*x1 + 9)*exp(-x1)
	f2 <- (x2^2 - 6*x2+ 9)*exp(-x2)

	
	f <- (x^2 - 6*x + 9)*exp(-x)

	if (abs(f1-f2) < tolerance) break

	if (sign(f) != sign(f1)) {
		x2 <- x 
	} else {
		x1 <- x

	}



}


###########################
##4.6 Bubble and Merge sort
###########################


bubblesort <- function(x) { 

	if (length(x) < 2) return (x) ##need to check for numeric too..

	for(last in length(x):2) { 
		for(first in 1:(last - 1)) { 
			if(x[first] > x[first + 1]) {  
				save <- x[first] 
				x[first] <- x[first + 1] 
				x[first + 1] <- save 
			}
		}
	}

	return(x)
}

##This is the actual mergesoft from the book. It is buggy for certain vectors..
mergesort <- function(x, decreasing=FALSE){
    if(decreasing==FALSE){
         len <- length(x)
         if (len<2) result <- x
         else{
            y <- x[1:(len/2)]
            z <- x[((len/2)+1):len]
            y <- mergesort(y)
            z <- mergesort(z)
            result <- c()
            while(min(length(y), length(z))>0){
                 if(y[1] < z[1]){
                      result <- c(result, y[1])
                      y <- y[-1]
                 }else{
                      result <- c(result, z[1])
                      z <- z[-1]
                 }
           }
           if(length(y)>0)
                 result <- c(result, y)
           else
                 result <- c(result, z)
        }
        return(result)
    }else{
         len <- length(x)
         if (len<2) result <- x
         else{
            y <- x[1:(len/2)]
            z <- x[((len/2)+1):len]
            y <- mergesort(y, decreasing=TRUE)
            z <- mergesort(z, decreasing=TRUE)
            result <- c()
            while(min(length(y), length(z))>0){
                 if(y[1]>z[1]){
                      result <- c(result, y[1])
                      y <- y[-1]
                 }else{
                      result <- c(result, z[1])
                      z <- z[-1]
                 }
           }
           if(length(y)>0)
                 result <- c(result, y)
           else
                 result <- c(result, z)
        }
        return(result)
     }
}

###Actual answer of the question

vector <- rnorm(100000)
system.time(bubblesort(vector))
system.time(mergesort(vector))


