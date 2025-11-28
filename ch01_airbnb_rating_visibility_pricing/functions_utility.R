#Extract Number-----------------------------------------------------------------
extract_number <- function(s){
  match <- regmatches(s, regexpr("-?\\d+(\\.\\d+)?", s))
  if(length(match) > 0){
    as.numeric(match)
  } else {
    NA
  }
}
#Df for Welsh's t-test----------------------------------------------------------
computeV <- function(s1, s2, n1, n2){
  
  numerator <- ( ((s1^2)/(n1)) + ((s2^2)/(n2)) )^2
  denomator <-( (s1^4)/((n1^2)*(n1-1)) + (s2^4)/((n2^2)*(n2-1)) )
  return(numerator/denomator)
  
}

#White Test---------------------------------------------------------------------
performWhiteTest <- function(m){
  
  xfit <- fitted(m)
  xfit2 <- fitted(m)^2
  res2 <- residuals(m)^2
  rm(m)
  r2 <- summary(lm(res2 ~ xfit + xfit2))$r.squared
  n <- length(xfit)
  ws <- n*r2
  rm(xfit, xfit2, res2)
  p <- 1 - pchisq(ws, df = 2)
  toOut <- list(white_stat = ws, p_value = p)
  return(toOut)
  
}


#cbind fill---------------------------------------------------------------------
cbind.fill <- function(...) {                                                                                                                                                       
  transposed <- lapply(list(...),t)                                                                                                                                                 
  transposed_dataframe <- lapply(transposed, as.data.frame)                                                                                                                         
  return (data.frame(t(plyr::rbind.fill(transposed_dataframe))))                                                                                                                          
} 
#Table Order--------------------------------------------------------------------
mapOrder <- function(Feature){
  
  temp <- as.data.frame(Feature)
  on <- (temp %>%
    mutate(
      n = case_when(
      temp == "treated" ~ 1,
      temp == "rating" ~ 1.1,
      temp == "treated*rating" ~ 1.2,
      temp == "booked_nights" ~ 2,
      temp == "price1" | temp == "real_price1" ~ 3,
      temp == "price2" | temp == "real_price2" ~ 4,
      temp == "price3" | temp == "real_price3" ~ 5,
      temp == "bathrooms" ~ 6,
      temp == "bedrooms" ~ 7,
      temp == "capacity" ~ 8,
      temp == "covid" ~ 9,
      temp == "number_of_reviews" ~ 10,
      temp == "AC7" ~ 11,
      temp == "AC51" ~ 12,
      temp == "AC72" ~ 13,
      temp == "R Squared" ~ 14,
      temp == "Adj. R Squared" ~ 15,
      temp == "Proj. R Squared" ~ 16,
      temp == "Proj. Adj. R Squared" ~ 17,
      temp == "F Stat" ~ 18,
      temp == "Proj. F Stat" ~ 19,
      temp == "Endog. F Stat" ~ 20,
      temp == "treated Cond. F Stat" ~ 21,
      temp == "booked_nights Cond. F Stat" ~ 22,
      temp == "n" ~ 23
      )
    ))$n
  return(on)
  
}

  
#Estimate SE--------------------------------------------------------------------
estSE <- function(se, n){
  k <- length(se)
  lapply(1:k, function(i){
    x <- rnorm(n, sd = se[i])
  }) %>%
    Reduce('+', .) %>%
    sd() %>%
    return()
}
