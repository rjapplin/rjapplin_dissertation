foc_bertrand_mkt <- function(par, own_prod, blp_data, mkt, marg_cost, theta_lin, theta_rc) {
  # argument par: candidate for post merger prices
  # arguments own_prod, blp_data, mkt, marg_cost, theta_lin, theta_rc: see previous code blocks
  
  # post merger updates: update the BLP_data object for market i
  tmp <- data.frame(
    "brand" = blp_data$parameters$product_id,
    "cdid" = as.numeric(blp_data$parameters$market_id_char_in),
    "delta" = blp_data$data$delta,
    "price" = blp_data$data$X_rand[, "price"]
  )

  delta_old <- blp_data$data$delta
  prices_pre <- blp_data$data$X_rand[, "price"]
  tmp$price[ market_ind ] <- par
  tmp$delta[ market_ind ] <- delta_old[market_ind] - (prices_pre[market_ind] * theta_lin) + (par* theta_lin)
  
  
  new_blp_data <- update_BLP_data(
    blp_data = blp_data,
    data_update = tmp
  )
  
  ShareObj <- getShareInfo(
    blp_data = new_blp_data,
    par_theta2 = theta_rc,
    printLevel = 0
  )
  
  implied_shares <- as.matrix(ShareObj$shares[market_ind])
  
  elasticities_post_mkt <- get_elasticities(
    blp_data = new_blp_data,
    share_info = ShareObj,
    theta_lin = theta_lin,
    variable = "price",
    market = mkt,
    printLevel = 0
  )
  
  scalar_mkt <- matrix(1 / implied_shares) %*% matrix(par, nrow = 1)
  derivatives_mkt <- elasticities_post_mkt / scalar_mkt
  
  markups_post <- c(-solve(t(derivatives_mkt) * own_prod) %*% implied_shares)
  differences <- par - marg_cost- markups_post
  rm(tmp, new_blp_data, ShareObj, implied_shares,
     elasticities_post_mkt, scalar_mkt, derivatives_mkt, markups_post)
  gc()
  
  return(differences)
}
