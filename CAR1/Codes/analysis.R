
fun.name <- "PASS_Proj_Test_ufDA"
fun.name <- "Extract_Eigencomp_fDA"
design_num             <- 10
sim_rep                <- 5

npc_for_HT             <- list(NA,2,3)

message("function name = ", fun.name,
        ", design_num = ", design_num,
        ", sim_rep = ", sim_rep, "\n")

load("Files/sim_design.Rdata")
source("Codes/internals.R")
require(dplyr)


fn.args                  <- rlang::fn_fmls_names(getFromNamespace(fun.name, ns = "fPASS"))

var_names <- c("sample_size", "nobs_per_subj", "delta", "missing_percent")
if (fun.name == "PASS_Proj_Test_ufDA"){
  var_names <- c("type", var_names, "npc", "power")
} else{
  var_names <- c("type", var_names, "grid", "center", "npc", "power")
}

power_dat <- tibble()


for (design_num in 1:400){
  folder_params <- c("sample_size", "nobs_per_subj", "delta", "missing_percent")
  if (fun.name == "PASS_Proj_Test_ufDA"){
    sim.design             <- sim.design.all %>% dplyr::select(all_of(fn.args)) %>% 
      dplyr::distinct() %>% purrr::transpose() %>% purrr::pluck(design_num)
    if (purrr::pluck(sim.design, "fpca_optns", "center", .default = FALSE)){
      message("center == TRUE is not applicable for power calculations: setting sim.design as null")
      sim.design <- NULL
    }
    
    if (!is.null(sim.design)){
      fun_params    <- list(ifelse(fun.name == "PASS_Proj_Test_ufDA", "sample_size", "eval_SS"),
                            "nobs_per_subj", list("mean_diff_add_args", "delta"), "missing_percent")
      folder_path   <- paste0(sapply(seq_along(folder_params), function(i) 
        paste0(folder_params[[i]], "=",
               purrr::pluck(sim.design, !!!fun_params[[i]], .default = "NULL"))),
        collapse = .Platform$file.sep)
      
      data_var   <- lapply(seq_along(folder_params), function(i) 
        purrr::pluck(sim.design, !!!fun_params[[i]], .default = NA) )
      
      data_var      <- c("Theoretical", data_var, purrr::pluck(sim.design, "npc_to_use", .default = NA))
      add_str       <- paste0("npc=", purrr::pluck(sim.design, "npc_to_use", .default = "NULL"))
      result_path   <- file.path("Results", "Power_Analysis", folder_path, "Theoretical", add_str)
      if (file.exists(file.path(result_path, "power_analysis.Rdata"))) {
        load(file.path(result_path, "power_analysis.Rdata"))
        runDat <- tibble(!!!setNames(c(data_var, 
                             list(as.vector(summary(ret_obj$power))) ), var_names) )
        runDat$stat <- c("min", "Q1", "Median", "Mean", "Q3", "Max")
        power_dat <- bind_rows(power_dat, runDat)
      } else{
        power_dat <- bind_rows(power_dat, 
                               data.frame(setNames(c(data_var, NA), var_names) ) )
      }
    }
    
  } else if (fun.name == "Extract_Eigencomp_fDA"){
    sim.design             <- sim.design.all %>% dplyr::select(-eval_SS) %>%
      dplyr::rename(eval_SS = sample_size) %>%
      dplyr::select(all_of(fn.args)) %>% 
      dplyr::distinct() %>% purrr::transpose() %>% purrr::pluck(design_num)
    if (!is.null(sim.design)) stopifnot(purrr::pluck(sim.design, "data.driven.scores"))
    
    if (!is.null(sim.design)){
      fun_params    <- list(ifelse(fun.name == "PASS_Proj_Test_ufDA", "sample_size", "eval_SS"),
                            "nobs_per_subj", list("mean_diff_add_args", "delta"), "missing_percent")
      folder_path   <- paste0(sapply(seq_along(folder_params), function(i) 
        paste0(folder_params[[i]], "=",
               purrr::pluck(sim.design, !!!fun_params[[i]], .default = "NULL"))),
        collapse = .Platform$file.sep)
      
      data_var   <- lapply(seq_along(folder_params), function(i) 
        purrr::pluck(sim.design, !!!fun_params[[i]], .default = "NULL") )
      data_var      <- c("Empirical", data_var, 
                         ifelse(stringr::str_detect(deparse(purrr::pluck(sim.design, "work.grid")), "seq"),
                                "uniform", "quadrature"),
                         purrr::pluck(sim.design, "fpca_optns", "center", .default = FALSE) )
      add_str       <- ifelse(stringr::str_detect(deparse(purrr::pluck(sim.design, "work.grid")), "seq"),
                              "grid=uniform", "grid=quadrature")
      add_str1      <- paste0("center=", purrr::pluck(sim.design, "fpca_optns", "center", .default = FALSE))
      result_path   <- file.path("Results", "Power_Analysis", folder_path, "Empirical", add_str, add_str1)
      if (file.exists(file.path(result_path, "shrinkage_scores.Rdata"))) {
        load(file.path(result_path, "shrinkage_scores.Rdata"))
        for (i in seq_along(npc_for_HT)){
          reject <- sapply(ret_obj[1:1000], function(sc) {
            to_pick   <- min(length(sc$est_eigenval), npc_for_HT[[i]], na.rm = TRUE)
            HT.test   <- Hotelling::hotelling.test(x = sc$scores_1[, 1:to_pick, drop = F], 
                                                   y = sc$scores_2[, 1:to_pick, drop = F],
                                                   shrinkage = FALSE, var.equal = TRUE,
                                                   perm = FALSE, B = 10000, progBar = (perm && TRUE)) 
            pVal_HT   <- HT.test$pval
            as.numeric(pVal_HT < 0.05)
          })
          power_dat <- bind_rows(power_dat, data.frame(setNames(c(data_var, 
                                 list(npc_for_HT[[i]]), mean(reject)), var_names) ) )
        }
      } else{
        for (i in seq_along(npc_for_HT)){
          power_dat <- bind_rows(power_dat, data.frame(setNames(c(data_var, 
                                 npc_for_HT[[i]], NA), var_names) ) )
        }
      } 
    }
  }
}

