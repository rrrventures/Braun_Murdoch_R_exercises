## 4.1 FLOW CONTROL
### 4.1.1 Solutions, FOR LOOP

#### 1 
#a)

fibonacci <- numeric(12)
fibonacci[1] <- fibonacci[2] <- 2
for (i in 3:12) fibonacci[i] <- fibonacci[i - 2] + fibonacci[i - 1]

#b)

fibonacci <- numeric(12)
fibonacci[1] <- 3
fibonacci[2] <- 2
for (i in 3:12) fibonacci[i] <- fibonacci[i - 2] + fibonacci[i - 1]

#c)

fibonacci <- numeric(12)
fibonacci[1] <- 1
fibonacci[2] <- 1
for (i in 3:12) fibonacci[i] <- -fibonacci[i - 2] + fibonacci[i - 1]

#d)

fibonacci <- numeric(12)
fibonacci[1] <- 1
fibonacci[2] <- 1
fibonacci[3] <- 1
for (i in 4:12) fibonacci[i] <- fibonacci[i - 3] + fibonacci[i - 2] + fibonacci[i - 1]



### 2

#a)
fibonacci <- numeric(30)
fibonacci[1] <- fibonacci[2] <- 1
for (i in 3:30) fibonacci[i] <- fibonacci[i - 2] + fibonacci[i - 1]

ratio <- numeric(29)
for (i in 2:30) ratio[i-1] <- fibonacci[i]/fibonacci[i - 1]

#b)
## This just depends on the level of tolerance for it to converge

golden <- (1 + sqrt(5))/2
ratio <- 0
i <- 2
tolerance <- 0.000001

while (abs(ratio - golden) > tolerance) {

	 ratio <- fibonacci[i]/fibonacci[i - 1]
	 i <- i + 1
}



### 3

#Just had to run some given code, results below

# a) 15
# b) numeric(0)
# c) [1] 0 1 2 3 4 5
# d) 123
# e) [1] 3 21 23 6 11 15 12 22 30 24 13 29 17 26 27 3



### 4

i <- 0.006
for (j in 1:20) {
	i <- (1 - (1 + i)^(-20)) / 19
}
#[1] 0.005264348


i <- 0.005
for (j in 1:20) {
	i <- (1 - (1 + i)^(-20)) / 19
}
#[1] 0.00495812


i <- 0.004
for (j in 1:20) {
	i <- (1 - (1 + i)^(-20)) / 19
}
#[1] 0.004558684

# Answer would be 0.00493, after running the program with i = 0.005 and j 1:1000



### 5

x <- 0.5
i <- 0

while (abs(x - cos(x)) > 0.01){
	x <- cos(x)
	i <- i + 1
} 

i
# [1] 10. Took 10 iterations

x <- 0.5
i <- 0

while (abs(x - cos(x)) > 0.001){
	x <- cos(x)
	i <- i + 1
} 

i
# [1] 15. Took 15 iterations

x <- 0.5
i <- 0

while (abs(x - cos(x)) > 0.0001){
	x <- cos(x)
	i <- i + 1
} 

i
# [1] 21. Took 21 iterations


## Other questions are just replacing number in the formula




### 6
#a)

x <- 0.5
i <- 0

while (abs(x - 1.5*cos(x)) > 0.0001){
	x <- 1.5*cos(x)
	i <- i + 1
} 

i
#Takes too long. Doesnt converge like this

x <- 0.5
i <- 0

while (abs(x - 1.5*cos(x)) > 0.0001){
	x <- cos(x)/30 + 44*x/45
	i <- i + 1
} 

i
#Converges at i = 185

# b)

# Just multply equation by 45 and re arrange to obtain the other previous equation

# c)


abs(-1.5*sin(x))        #[1] 1.188671
abs(-sin(x)/30 + 44/45) #[1] 0.9513629

#Theorem does explain the previous behaviour

