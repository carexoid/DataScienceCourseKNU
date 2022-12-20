1. Функція add2(x, y), яка повертає суму двох чисел.
```r
add2<- function(x,y) {
    x + y
}
> add2(2,2)
[1] 4
```
2. Функція above(x, n), яка приймає вектор та число n, та повертає всі елементі вектору, які більше n. По замовчуванню n = 10.
```r
above<- function(x,n=10){
    x[x > n]
}
> above(c(1,2,3,4,99,9999,99999,999999))
[1]     99   9999  99999 999999
> above(c(1,2,3,4,99,9999,99999,999999), 999)
[1]   9999  99999 999999
```
3. Функція my_ifelse(x, exp, n), яка приймає вектор x, порівнює всі його елементи за допомогою exp з n, та повертає елементи вектору, які відповідають умові expression. Наприклад, my_ifelse(x, “>”, 0) повертає всі елементи x, які більші 0. Exp може дорівнювати “<”, “>”, “<=”, “>=”, “==”. Якщо exp не співпадає ні з одним з цих виразів, повертається вектор x.
```r

my_ifelse <- function(x, exp, n) {
    if (exp == "<") {
        return(x[which(x < n)])
    } else if (exp == ">") {
        return(x[which(x > n)])
    } else if (exp == "<=") {
        return(x[which(x <= n)])
    } else if (exp == ">=") {
        return(x[which(x >= n)])
    } else if (exp == "==") {
      return(x[which(x == n)])
    } else {
        return(x)
    }
}

my_ifelse(c(1,2,3),'>',0)
[1] 1 2 3
my_ifelse(c(1,2,3),'>',1)
[1] 2 3
my_ifelse(c(1,2,3),'>',2)
[1] 3
``` 
4. Функція columnmean(x, removeNA), яка розраховує середнє значення (mean) по кожному стовпцю матриці, або data frame. Логічний параметр removeNA вказує, чи видаляти NA значення. По замовчуванню він дорівнює TRUE.
```r
columnmean<- function(x, removeNA=TRUE){
  res<-c()
  for (el in 1:ncol(x)){
    res<-c(res, mean(x[ ,el], na.rm=removeNA))
  }
  return(res)
}

# Example on matrix
columnmean(matrix(1:12, 3, 4))
[1]  2  5  8 11
matrix(1:12, 3, 4)
     [,1] [,2] [,3] [,4]
[1,]    1    4    7   10
[2,]    2    5    8   11
[3,]    3    6    9   12

# Example on dataframe
columnmean(data.frame(sum = c(1,2,3,4,5),item = c('a','b','c','d','e')))
[1]  3 NA

data.frame(sum = c(1,2,3,4,5),item = c('a','b','c','d','e'))
  sum item
1   1    a
2   2    b
3   3    c
4   4    d
5   5    e

columnmean(data.frame(sum = c(1,2,3,4,NA),item = c('a','b','c','d','e')), removeNA = FALSE)
[1] NA NA
```