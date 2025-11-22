origBlpPerf <- function(){
  own_pre <- dummies_cars
  colnames(own_pre) <- paste0("company", 1:26)
  productData_cars <- cbind(productData_cars, own_pre)
  
  # construct instruments
  nobs <- nrow(productData_cars)
  X <- data.frame(
    productData_cars$const, productData_cars$hpwt,
    productData_cars$air, productData_cars$mpg, productData_cars$space
  )
  
  sum_other <- matrix(NA, nobs, ncol(X))
  sum_rival <- matrix(NA, nobs, ncol(X))
  sum_total <- matrix(NA, nobs, ncol(X))
  
  for (i in 1:nobs) {
    other_ind <- productData_cars$firmid == productData_cars$firmid[i] &
      productData_cars$cdid == productData_cars$cdid[i] &
      productData_cars$id != productData_cars$id[i]
    rival_ind <- productData_cars$firmid != productData_cars$firmid[i] &
      productData_cars$cdid == productData_cars$cdid[i]
    total_ind <- productData_cars$cdid == productData_cars$cdid[i]
    
    sum_other[i, ] <- colSums(X[other_ind == 1, ])
    sum_rival[i, ] <- colSums(X[rival_ind == 1, ])
    sum_total[i, ] <- colSums(X[total_ind == 1, ])
  }
  
  colnames(sum_other) <- paste0("IV", 1:5)
  colnames(sum_rival) <- paste0("IV", 6:10)
  productData_cars <- cbind(productData_cars, sum_other, sum_rival)
  head(productData_cars)
  
  # To show similarities between implementations of other authors,
  # the variable "const" is used, although constants are considered by default.
  blps_model <- as.formula("share ~  0 + const + price + hpwt + air + mpg + space |
                          0 + const + hpwt + air + mpg + space |
                          0 + price + const + hpwt + air + mpg |
                          0 + IV1 + IV2 + IV3 + IV4 + IV5 + IV6 + IV7 + IV8 + IV9 + IV10")
  
  car_data <- BLP_data(
    model = blps_model,
    market_identifier = "cdid",
    product_identifier = "id",
    additional_variables = paste0("company", 1:26), # check reordering works
    productData = productData_cars,
    blp_inner_tol = 1e-9,
    blp_inner_maxit = 5000,
    integration_method = "MLHS",
    integration_accuracy = 50, integration_seed = 48
  )
  
  set.seed(121)
  theta_guesses <- matrix(rnorm(5))
  rownames(theta_guesses) <- c("price", "const", "hpwt", "air", "mpg")
  colnames(theta_guesses) <- "unobs_sd"
  
  
  car_est <- estimateBLP(
    blp_data = car_data,
    par_theta2 = theta_guesses,
    solver_method = "BFGS", solver_maxit = 1000, solver_reltol = 1e-6,
    extremumCheck = FALSE, printLevel = 0
  )
  
  ## Pre-Merger data
  own_pre <- as.matrix(car_data$data$additional_data[, paste0("company", 1:26)])
  delta_pre <- car_est$delta
  theta1_price <- car_est$theta_lin["price", ]
  theta2_price <- car_est$theta_rc["unobs_sd*price"]
  theta2_all <- matrix(car_est$theta_rc)
  rownames(theta2_all) <- c("price", "const", "hpwt", "air", "mpg")
  colnames(theta2_all) <- "unobs_sd"
  
  ## update mean utility in data ( always use update_BLP_data() to update data object to maintain consistent data )
  delta_data <- data.frame(
    "id" = car_data$parameters$product_id,
    "cdid" = car_data$parameters$market_id,
    "delta" = delta_pre
  )
  car_data_updated <- update_BLP_data(
    data_update = delta_data,
    blp_data = car_data
  )
  
  shareObj <- getShareInfo(
    blp_data = car_data_updated,
    par_theta2 = theta2_all,
    printLevel = 0
  )
  
  final <- productData_cars %>%
    select(firmid, cdid, id, share) %>%
    mutate(share_hat = shareObj$shares)
  
  return(final)

}
