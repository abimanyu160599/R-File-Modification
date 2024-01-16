power <- function(base, exponent) {
  result <- 1
  for (i in 1:exponent) {
    result <- result * base
  }
  return(result)
}
isArmstrong <- function(num) {
  num_digits <- nchar(num)
  temp <- num
  sum <- 0
  
  while (temp > 0) {
    digit <- temp %% 10
    sum <- sum + power(digit, num_digits)
    temp <- temp %/% 10
  }
  
  return(sum == num)
}

findArmstrongNumbers <- function(start, end) {
  armstrong_numbers <- c()
  
  for (num in start:end) {
    if (isArmstrong(num)) {
      armstrong_numbers <- c(armstrong_numbers, num)
    }
  }
  
  return(armstrong_numbers)
}


start_range <- 1
end_range <- 1000

armstrong_numbers <- findArmstrongNumbers(start_range, end_range)

cat("Armstrong numbers between", start_range, "and", end_range, "are:", armstrong_numbers, "\n")