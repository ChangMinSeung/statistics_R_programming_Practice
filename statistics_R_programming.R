
#####uniform_distribution(확률이 똑같다)_simulate#####

runif(100) #0부터 1까지 100개

runif(100)*6 #0부터 6까지 100개

ceiling(runif(100)*6) #올림 

#####


#####sum_of_two_dice#####

roll <- 1000
#주사위 3개#각 각 랜덤한 값이 나와야 함#3을 곱한 것과 다름
dice <- ceiling(runif(roll)*6) + ceiling(runif(roll)*6) + ceiling(runif(roll)*6) 
table(dice)
a <- table(dice)
barplot(a)

#####


#####normal_distribution#####

  #mean = 80,  sd = 10
x <- seq(40, 120, length=300); x #40부터 120까지 300개로 나눔
y <- dnorm(x, mean = 80, sd = 10)
plot(x, y, type = "l", col = "red")

  #probability between 65~75
x2 <- seq(65, 75, length = 200)
y2 <- dnorm(x2, mean = 80, sd = 10)
polygon(c(65, x2, 75), c(0, y2, 0), col = "gray")
#dnorm(y값) / pnorm(0.5%) / qnorm(x값) / rnorm(난수) 
pnorm(75, mean = 80, sd = 10) - pnorm(65, mean = 80, sd = 10)

  #probability of over 92
1 - pnorm(92, mean = 80, sd = 10)

  #probability of less than 68
pnorm(68, mean = 80, sd = 10)

  #cutoff that separates the bottom 30% (하위 30%)
qnorm(0.3 , mean = 80, sd = 10)

  #cutoff that contain the middle 60%
80 - qnorm(0.8, mean = 80, sd = 10)
80 - qnorm(0.2, mean = 80, sd = 10) 


#####


#####Chi-square_test#####
  #t-test(평균 차이)(모집단 2개까지) / chi-square-test(분산 차이)(모집단 1개)
data <- matrix(c(42,30,50,87), nrow = 2, byrow = FALSE); data

  #H0 #함수 만들기
chisq <- function(Obs){
              Expected <- outer(rowSums(Obs), colSums(Obs)) / sum(Obs)
              sum((Obs - Expected)^2/Expected)
                      }

tmp <- chisq(data)

  #자유도
tmp2 <- (2-1)*(2-1)

  #p-value #대립가설 채택 #유의미한 차이
1-pchisq(tmp, tmp2)

  #함수
chisq.test(data, correct = FALSE)


#####


#####tapply_aggregate_function#####

data(mtcars)
str(mtcars)

tmp_mpg_4 <- mtcars[which(mtcars$cyl == 4),]
head(tmp_mpg_4)

tapply(mtcars$mpg, mtcars$cyl, mean)

aggregate(mtcars$mpg, by=list(mtcars$hp, mtcars$wt), mean)

#####


#####ggvis_package#####

library(ggvis)
attach(mtcars)

mtcars %>% ggvis( ~mpg, ~wt, fill:="red") %>% layer_points() %>% layer_smooths()

mtcars %>% ggvis( ~mpg, ~wt, fill= ~cyl) %>% layer_points() %>% layer_smooths()

mtcars %>% ggvis( ~mpg, ~wt, fill= ~gear) %>% layer_points() %>%
                                                  layer_smooths() %>%
                                        add_axis("x", title = "MPG", values = c(10:35)) %>%
                                        add_axis("y", title = "WT", subdivide = 4)

#####


#####binomial/hypergeometric#####

dbinom(3, 10, 0.4)
dhyper(3, 8, 12, 10) #20명이 있다. 8명이 남자이고 12명이 여자다. 10명을 뽑는데 3명이 남자

#####


#####eigen_value_eigen_vector / transpose#####

A <- matrix(c(3,1,0,2,1,0,-2,-2,1), nrow = 3); A

ev <- eigen(A)$values
evec <- eigen(A)$vectors

evec%*%diag(ev)%*%solve(evec)

trans <- function(A){
          B <- matrix(nrow = nrow(A), ncol = ncol(A))
          for(i in 1:nrow(A)){
            FOR(j in 1:ncol(A)){
              B[j,i] <- A[i,j]
            }
          }
          return(B)
}

trans(trans(A)) == A

#####


#####linear_model#####

year <- c(2000, 2001, 2002, 2003, 2004)
value <- c(2.3, 3.2, 5.6, 5.4, 5.8)
plot(jitter(year), value, ylim = c(0,20))

fit <- lm(value ~ year)
abline(fit, col="red")

fit$coefficients[[2]]

  #value - 0.92, year = 1837.38

fit$residuals
summary(fit)
par(mfrow=c(2,2))
plot(fit)


##### 


#####QQ_plot#####

getwd()
light <- read.table("/cloud/project/r_data/light.txt", header = T)
head(light)

qqnorm(light$speed)
qqline(light$speed, col = "red")
shapiro.test(light$speed)

  #H0 : normally distributed

set.seed(0)
normal_dist <- rnorm(10000, 0, 10)
hist(normal_dist)


install.packages("nortest")
library(nortest)

ad.test(normal_dist)

set.seed(0)
normal_dist_2 <- rnorm(1000, 0, 10)
hist(normal_dist_2)

shapiro.test(normal_dist_2)

#####


####bootsstrap#####

getwd()
light <- read.table("/cloud/project/r_data/light.txt", header = T)
hist(light$speed, col = "green")
qqnorm(light$speed)
qqline(light$speed, col = "blue")
length(light$speed)

a <- numeric(10000)
for(i in 1:10000) a[i] <- mean(sample(light$speed, size = 20, replace = T))

hist(a, col = "blue")

#####
