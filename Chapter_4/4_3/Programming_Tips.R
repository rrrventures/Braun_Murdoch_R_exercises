###4.3.2

# 1)
# just use factorial(x)

# 3)
 fix(compound.interest)
   #switch to
    function(P, j, m, n){
      i.r <- j/m
        return( P*(1+i.r)^(n*m))
    }

# 4) just substitute values on function...

# 5)
# a)

mortgage.payment <- function(P, i.r, n) {
      R <- P*i.r/(1 - (1 + i.r)^(-n))
      return(R)
}

# c)
## evaluate annuity() fom lifecontigencies package with given values..
