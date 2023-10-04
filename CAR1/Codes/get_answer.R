


sample_size <- 400; delta <- 0.5;
nobs_per_subj <- 8; 
missing_percent <- 0.4; npc <- 2;
grid <- "quadrature"; center <- "FALSE"; 

theo_path <- paste0("Results/Power_Analysis/", "sample_size=", sample_size, 
                   "/nobs_per_subj=", nobs_per_subj,
                   "/delta=", delta, "/missing_percent=", missing_percent, "/",
                   "Theoretical", "/npc=", npc)

load(file.path(theo_path, "power_analysis.Rdata"))
summary(ret_obj$power)

emp_path <- paste0("Results/Power_Analysis/", "sample_size=", sample_size, 
                    "/nobs_per_subj=", nobs_per_subj,
                    "/delta=", delta, "/missing_percent=", missing_percent, "/",
                   "Empirical", "/grid=", grid, "/center=", center)

load(file.path(emp_path, "shrinkage_scores.Rdata"))

reject <- rep(NA, 1000)
for (i in 1:1000){
  to_pick   <- min(ncol(ret_obj[[i]]$scores_1), as.numeric(npc), na.rm=TRUE)
  HT.test   <- Hotelling::hotelling.test(x = ret_obj[[i]]$scores_1[, 1:to_pick, drop = F], 
                                         y = ret_obj[[i]]$scores_2[, 1:to_pick, drop = F],
                                         shrinkage = FALSE, var.equal = TRUE,
                                         perm = FALSE, B = 10000) 
  pVal_HT   <- HT.test$pval
  reject[i] <- as.numeric(pVal_HT < 0.05)
}
mean(reject)