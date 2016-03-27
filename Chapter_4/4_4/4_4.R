###4.4 Programming Guidelines

#4.1


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
                 if(y[1]<z[1]){
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


#4.2


## Should have named the variable just x and y but works
## Also, another way to understand the problem was to write a function
## that took functions as arguments. I ignored this case because taking
## derivatives is not trivial in R as far as I'm aware, though I ran into
## one package that seemed to work decently

newton <- function(xn_1 = 1,yn_1 = 1, tolerance = 0.00001) {
	
	count <- 0

	repeat {
	
	fn_1 <- xn_1 + yn_1
	gn_1 <- xn_1^2 + 2*yn_1^2 - 2

	if (abs(fn_1) < tolerance && abs(gn_1) < tolerance) break

	fxn_1 <- 1
	fyn_1 <- 1 

	gxn_1 <- 2*xn_1 
	gyn_1 <- 4*yn_1

	dn_1 <- fxn_1*gyn_1 - fyn_1*gxn_1
	

	xn_1 <- xn_1 - (gyn_1*fn_1 - fyn_1*gn_1)/dn_1
	yn_1 <- yn_1 - (fxn_1*gn_1 - gxn_1*fn_1)/dn_1

	count <- count + 1
	print(count)

	}

	lols <- c(xn_1,yn_1)
	return (lols)

}


## [1] -0.8164966  0.8164966
