iv_cands <- paste0("iv", 1:10)
reslist <- list()
for(i in 101:1000){
  
  n <- sample(1:length(iv_cands), 1)
  set.seed(i^2)
  nmkts <- length(unique(df2$cdid))
  sub_mkts <- sample(1:nmkts, 10000, TRUE)
  df2_sub <- df2 %>% filter(cdid %in% sub_mkts) %>% na.omit()
  
  ivs <- sample(iv_cands, n, F)
  formula <- paste0("delta ~ price + men + lo + border_county + as.factor(year) + border_county:as.factor(year) + as.factor(brand) | men + lo + border_county + as.factor(year) + border_county:as.factor(year) + as.factor(brand) +",
                    paste0(ivs, collapse = " + ")
  )
  reg <- ivreg(formula, data = df2_sub)
  
  res <- data.frame(
    price = reg$coefficients[2],
    iv1 = grepl("iv1", paste0(ivs, collapse = ",")),
    iv2 = grepl("iv2", paste0(ivs, collapse = ",")),
    iv3 = grepl("iv3", paste0(ivs, collapse = ",")),
    iv4 = grepl("iv4", paste0(ivs, collapse = ",")),
    iv5 = grepl("iv5", paste0(ivs, collapse = ",")),
    iv6 = grepl("iv6", paste0(ivs, collapse = ",")),
    iv7 = grepl("iv7", paste0(ivs, collapse = ",")),
    iv8 = grepl("iv8", paste0(ivs, collapse = ",")),
    iv9 = grepl("iv9", paste0(ivs, collapse = ",")),
    iv10 = grepl('iv10', paste0(ivs, collapse = ","))
  )
  
  reslist[[i]] <- res
  print(i)
}

fres <- Reduce(rbind, reslist)
fres %>%
  dbWriteTable(results2, "iv_robustness", .)
