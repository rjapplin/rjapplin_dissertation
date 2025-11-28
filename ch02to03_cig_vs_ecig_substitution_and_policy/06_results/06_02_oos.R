outSample <- df2 %>%
  filter(is.na(delta))
intercept <- rep(1, dim(outSample)[1])
brand_mat <- model.matrix(~outSample$brand)
brand_mat <- brand_mat[,-1]
year_mat <- model.matrix(~as.factor(outSample$year))
year_mat <- year_mat[,-1]
#Subbagging Performance---------------------------------------------------------

contAlpha <- theta1_bar %>% select(coef) %>% t() %>% as.vector()
names(contAlpha) <- theta1_bar$variable

outSample$delta <- intercept*contAlpha["(Intercept)"] + outSample$price*contAlpha["price"] +
  outSample$men*contAlpha["men"] + outSample$lo*contAlpha["lo"] +
  outSample$border_county*contAlpha["border_countyTRUE"] +
  brand_mat%*%contAlpha[2:36] + year_mat%*%contAlpha[37:43] +
  (year_mat*outSample$border_county)%*%contAlpha[45:51]
  
out_blp <- BLP_data(
  model = "inShare ~ price + men + lo + border_county + as.factor(year) +
  border_county:as.factor(year) + as.factor(brand)  | 
  men + lo + border_county + as.factor(year) +
  border_county:as.factor(year) + as.factor(brand)  | 
  price  |
  0 + iv2 + iv3 + iv8",
  market_identifier = "cdid",
  product_identifier = "brand",
  productData = outSample,
  blp_inner_tol = 1e-6, blp_inner_maxit = 5000,
  integration_method = "MLHS",
  integration_accuracy = 1000,
  integration_seed = 1,
  par_delta = "delta"
)

outSample$inShare_hat_approx <- getShareInfo(out_blp, theta2_bar_alt)$shares
  
outSample %>%
  select(cdid, market, brand, inShare, inShare_hat_approx) %>%
  mutate(iter = "avg") %>%
  dbWriteTable(results, "data.oos_avg_delta", ., overwrite = T)

rm(out_blp)
gc()
#Individual Iteration Performance-----------------------------------------------
for(i in 1:max(dbReadTable(results, "theta1")$iter)){
  
  tmp_res <- dbReadTable(results, "theta1") %>%
    filter(iter == i)
  contAlpha <- tmp_res %>% select(coef) %>% t() %>% as.vector()
  names(contAlpha) <- tmp_res$variable
  
  outSample$delta <- intercept*contAlpha["(Intercept)"] + outSample$price*contAlpha["price"] +
    outSample$men*contAlpha["men"] + outSample$lo*contAlpha["lo"] +
    outSample$border_county*contAlpha["border_countyTRUE"] +
    brand_mat%*%contAlpha[13:47] + year_mat%*%contAlpha[6:12] +
    (year_mat*outSample$border_county)%*%contAlpha[48:54]
  
  out_blp <- BLP_data(
    model = "inShare ~ price + men + lo + border_county + as.factor(year) +
  border_county:as.factor(year) + as.factor(brand)  | 
  men + lo + border_county + as.factor(year) +
  border_county:as.factor(year) + as.factor(brand)  | 
  price  |
  0 + iv2 + iv3 + iv8",
    market_identifier = "cdid",
    product_identifier = "brand",
    productData = outSample,
    blp_inner_tol = 1e-6, blp_inner_maxit = 5000,
    integration_method = "MLHS",
    integration_accuracy = 1000,
    integration_seed = 1,
    par_delta = "delta"
  )
  
  outSample$inShare_hat_approx <- getShareInfo(out_blp, theta2_bar_alt)$shares
  
  outSample %>%
    select(cdid, market, brand, inShare, inShare_hat_approx) %>%
    mutate(iter = i) %>%
    dbWriteTable(results, "data.oos_avg_delta", ., append = T)
  
  rm(out_blp)
  gc()
  
  print(i)
}

rm(tmp_res, year_mat, brand_mat, outSample, market, model, intercept,
   indices, i)  

#Cars Benchmark Data Setup------------------------------------------------------
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
rm(sum_rival, sum_other, sum_total, other_ind, rival_ind, total_ind, X, nobs,
   own_pre)
test_mkts <- sample(productData_cars$cdid, 0.25*20, F)
cars_training <- productData_cars %>%
  filter(!(cdid %in% test_mkts))

#Crs Subsample Performance------------------------------------------------------

for(i in 1:101){
  
  if(i == 97){
    
  } else {
  
  set.seed(i)
  car_mkts <- sample(unique(cars_training$cdid), 10, TRUE)
  car_df <- cars_training %>%
    filter(cdid %in% car_mkts)
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
    productData = car_df,
    blp_inner_tol = 1e-9,
    blp_inner_maxit = 5000,
    integration_method = "MLHS",
    integration_accuracy = 50, integration_seed = 48
  )
  
  car_est <- estimateBLP(
    blp_data = car_data,
    solver_method = "BFGS", solver_maxit = 1000, solver_reltol = 1e-6,
    extremumCheck = FALSE, printLevel = 0
  )
  
  oos_cars <- productData_cars %>%
    filter(cdid %in% test_mkts)
  
  oos_cars$delta <- as.matrix(
    oos_cars %>% 
      mutate(int = 1) %>%
      select(int, price, hpwt, air, mpg, space)
  ) %*% car_est$theta_lin
  
  oos_blp <- BLP_data(
    model = blps_model,
    market_identifier = "cdid",
    product_identifier = "id",
    additional_variables = paste0("company", 1:26), # check reordering works
    productData = oos_cars,
    blp_inner_tol = 1e-9,
    blp_inner_maxit = 5000,
    integration_method = "MLHS",
    integration_accuracy = 50, integration_seed = 48,
    par_delta = "delta"
  )
  
  theta2_car <- as.matrix(car_est$theta_rc)
  rownames(theta2_car) <- gsub("unobs\\_sd\\*", "", rownames(theta2_car))
  colnames(theta2_car) <- "unobs_sd"
  
  oos_cars$share_hat <- getShareInfo(oos_blp, theta2_car)$shares
  
  if(i == 1){
    oos_cars %>% 
      select(cdid, firmid, id, share, share_hat)  %>%
      mutate(
        pe = (share_hat - share)/share
      ) %>%
      mutate(
        rspe = sqrt((pe)^2)
      ) %>%
      group_by(firmid) %>%
      dplyr::summarize(
        mpe = mean(pe)*100,
        rmspe = mean(rspe)*100
      ) %>%
      mutate(iter = i) %>%
      dbWriteTable(results, "cars_oos_perf", ., overwrite = T)
  
    car_est$theta_lin %>%
      as.data.frame() %>%
      rownames_to_column("variable") %>%
      dplyr::rename(coef = V1) %>%
      mutate(iter = i) %>%
      dbWriteTable(results, "cars_75p_linear", ., overwrite = T)
    
    car_est$theta_rc %>%
      as.data.frame() %>%
      rownames_to_column("variable") %>%
      dplyr::rename(coef = ".") %>%
      mutate(iter = i) %>%
      dbWriteTable(results, "cars_75p_rc", ., overwrite = T)
  } else {
    oos_cars %>% 
      select(cdid, firmid, id, share, share_hat)  %>%
      mutate(
        pe = (share_hat - share)/share
      ) %>%
      mutate(
        rspe = sqrt((pe)^2)
      ) %>%
      group_by(firmid) %>%
      dplyr::summarize(
        mpe = mean(pe)*100,
        rmspe = mean(rspe)*100
      ) %>%
      mutate(iter = i) %>%
      dbWriteTable(results, "cars_oos_perf", ., append = T)
    
    car_est$theta_lin %>%
      as.data.frame() %>%
      rownames_to_column("variable") %>%
      dplyr::rename(coef = V1) %>%
      mutate(iter = i) %>%
      dbWriteTable(results, "cars_75p_linear", ., append = T)
    
    car_est$theta_rc %>%
      as.data.frame() %>%
      rownames_to_column("variable") %>%
      dplyr::rename(coef = ".") %>%
      mutate(iter = i) %>%
      dbWriteTable(results, "cars_75p_rc", ., append = T)
  }
  
  print(i)  
  
  }
  
}

#Cars Subbaged Performance------------------------------------------------------
oos_cars <- productData_cars %>%
  filter(cdid %in% test_mkts)

oos_cars$delta <- as.matrix(
  oos_cars %>% 
    mutate(int = 1) %>%
    select(int, price, hpwt, air, mpg, space)
) %*% (dbGetQuery(results, "SELECT variable, 
                  AVG(coef) coef FROM cars_75p_linear GROUP BY variable") %>%
         mutate(order = case_when(
           variable == "const" ~ 1,
           variable == "price" ~ 2,
           variable == "hpwt" ~ 3,
           variable == "air" ~ 4,
           variable == "mpg" ~ 5,
           variable == "space" ~ 6
         )) %>%
         arrange(order) %>%
         select(coef) %>%
         as.matrix()
)

oos_blp <- BLP_data(
  model = blps_model,
  market_identifier = "cdid",
  product_identifier = "id",
  additional_variables = paste0("company", 1:26), # check reordering works
  productData = oos_cars,
  blp_inner_tol = 1e-9,
  blp_inner_maxit = 5000,
  integration_method = "MLHS",
  integration_accuracy = 50, integration_seed = 48,
  par_delta = "delta"
)

theta2_car <- as.matrix(dbGetQuery(results,
                                   "SELECT variable, AVG(coef) coef FROM
                                    cars_75p_rc GROUP BY variable") %>%
                          mutate(
                            variable = gsub("unobs\\_sd\\*", "", variable)
                          ) %>%
                          mutate(
                            order = case_when(
                              variable == "price" ~ 1,
                              variable == "const" ~ 2,
                              variable == "hpwt" ~ 3,
                              variable == "air" ~ 4,
                              variable == "mpg" ~ 5
                            )
                          ) %>%
                          arrange(order) %>%
                          column_to_rownames("variable") %>%
                          select(coef) %>%
                          dplyr::rename(unobs_sd = coef)
)

oos_cars$share_hat <- getShareInfo(oos_blp, theta2_car)$shares

oos_cars %>%
  select(cdid, firmid, id, share, share_hat)  %>%
  mutate(
    pe = (share_hat - share)/share
  ) %>%
  mutate(
    rspe = sqrt((pe)^2)
  ) %>%
  group_by(firmid) %>%
  dplyr::summarize(
    mpe = mean(pe)*100,
    rmspe = mean(rspe)*100
  ) %>%
  mutate(iter = "avg") %>%
  dbWriteTable(results, "cars_oos_perf", ., append = T)

#Cars Full Training Performance-------------------------------------------------
oos_cars <- productData_cars %>%
  filter(cdid %in% test_mkts)

car_data <- BLP_data(
  model = blps_model,
  market_identifier = "cdid",
  product_identifier = "id",
  additional_variables = paste0("company", 1:26), # check reordering works
  productData = cars_training,
  blp_inner_tol = 1e-9,
  blp_inner_maxit = 5000,
  integration_method = "MLHS",
  integration_accuracy = 50, integration_seed = 48
)

car_est <- estimateBLP(
  blp_data = car_data,
  solver_method = "BFGS", solver_maxit = 1000, solver_reltol = 1e-6,
  extremumCheck = FALSE, printLevel = 0
)

oos_cars$delta <- as.matrix(
  oos_cars %>% 
    mutate(int = 1) %>%
    select(int, price, hpwt, air, mpg, space)
) %*% car_est$theta_lin

car_data <- BLP_data(
  model = blps_model,
  market_identifier = "cdid",
  product_identifier = "id",
  additional_variables = paste0("company", 1:26), # check reordering works
  productData = oos_cars,
  blp_inner_tol = 1e-9,
  blp_inner_maxit = 5000,
  integration_method = "MLHS",
  integration_accuracy = 50, integration_seed = 48,
  par_delta = "delta"
)

theta2_car <- as.matrix(car_est$theta_rc)
rownames(theta2_car) <- gsub("unobs\\_sd\\*", "", rownames(theta2_car))
colnames(theta2_car) <- "unobs_sd"

oos_cars$share_hat <- getShareInfo(car_data, theta2_car)$shares


oos_cars %>%
  select(cdid, firmid, id, share, share_hat)  %>%
  mutate(
    pe = (share_hat - share)/share
  ) %>%
  mutate(
    rspe = sqrt((pe)^2)
  ) %>%
  group_by(firmid) %>%
  dplyr::summarize(
    mpe = mean(pe)*100,
    rmspe = mean(rspe)*100
  ) %>%
  mutate(iter = "full_train") %>%
  dbWriteTable(results, "cars_oos_perf", ., append = T)

