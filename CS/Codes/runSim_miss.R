

rm(list=ls())

args         <- commandArgs(TRUE)
fun.name     <- as.character(args[1])
design_num   <- as.integer(args[2])
sim_rep      <- as.integer(args[3])
parallel     <- as.logical(args[4])

# fun.name               <- "PASS_Proj_Test_ufDA"
# fun.name               <- "Extract_Eigencomp_fDA"
# design_num             <- 10
# sim_rep                <- 5
# parallel               <- TRUE

message("function name = ", fun.name,
        ", design_num = ", design_num,
        ", sim_rep = ", sim_rep,
        ", parallel = ", parallel, "\n")

load("Files/sim_design_miss.Rdata")
source("Codes/internals.R")
require(dplyr)

fn.args                  <- rlang::fn_fmls_names(getFromNamespace(fun.name, ns = "fPASS"))

if (fun.name == "PASS_Proj_Test_ufDA"){
  sim.design             <- sim.design.all %>% dplyr::select(all_of(fn.args)) %>% 
                            dplyr::distinct() %>% purrr::transpose() %>% purrr::pluck(design_num)
  if (purrr::pluck(sim.design, "fpca_optns", "center", .default = FALSE)){
    message("center == TRUE is not applicable for power calculations: setting sim.design as null")
    sim.design <- NULL
  }
} else if  (fun.name == "Extract_Eigencomp_fDA"){
  sim.design             <- sim.design.all %>% dplyr::select(-eval_SS) %>%
                            dplyr::rename(eval_SS = sample_size) %>%
                            dplyr::select(all_of(fn.args)) %>% 
                            dplyr::distinct() %>% purrr::transpose() %>% purrr::pluck(design_num)
  if (!is.null(sim.design)) stopifnot(purrr::pluck(sim.design, "data.driven.scores"))
}

rm("sim.design.all")

# Result saving part : pre-computation

if (!is.null(sim.design)){
  folder_params <- c("sample_size", "nobs_per_subj", "delta", "missing_percent")
  fun_params    <- list(ifelse(fun.name == "PASS_Proj_Test_ufDA", "sample_size", "eval_SS"),
                        "nobs_per_subj", list("mean_diff_add_args", "delta"), "missing_percent")
  folder_path   <- paste0(sapply(seq_along(folder_params), function(i) 
    paste0(folder_params[[i]], "=",
           purrr::pluck(sim.design, !!!fun_params[[i]], .default = "NULL"))),
    collapse = .Platform$file.sep)
  
  if (fun.name == "PASS_Proj_Test_ufDA"){
    add_str       <- paste0("npc=", purrr::pluck(sim.design, "npc_to_use", .default = "NULL"))
    result_path   <- file.path("Results", "Power_Analysis", folder_path, "Theoretical", add_str)
    if (file.exists(file.path(result_path, "power_analysis.Rdata"))) {
      message("Result already computed! No need to recreate!")
      sim.design <- NULL
    } 
  } else if (fun.name == "Extract_Eigencomp_fDA"){
    add_str       <- ifelse(stringr::str_detect(deparse(purrr::pluck(sim.design, "work.grid")), "seq"),
                            "grid=uniform", "grid=quadrature")
    add_str1      <- paste0("center=", purrr::pluck(sim.design, "fpca_optns", "center", .default = FALSE))
    result_path   <- file.path("Results", "Power_Analysis", folder_path, "Empirical", add_str, add_str1)
    if (file.exists(file.path(result_path, "shrinkage_scores.Rdata"))) {
      message("Result already computed! No need to recreate!")
      sim.design <- NULL
    } 
  }
}



# Computation starts
if (!is.null(sim.design)){
  require(doParallel)
  mean.diff              <- function(t, delta) {delta * (t^3)};
  fPASS_call             <- rlang::call2(fun.name, !!!sim.design, .ns = "fPASS")
  print(fPASS_call)
  if (fun.name == "PASS_Proj_Test_ufDA"){
    if (parallel){
      registerDoParallel(cl <- makeCluster(detectCores()-1))
      ret_obj               <- foreach(itid=1:sim_rep, .export = c("mean.diff")) %dopar% {
                                    fPASS_out <- eval(fPASS_call)
                                    list(power = purrr::pluck(fPASS_out, "power_value"),
                                         eigenval = purrr::pluck(fPASS_out, "est_eigencomp", "est_eigenval"))
                                }
      stopCluster(cl)
    } else{
      ret_obj               <- foreach(itid=1:sim_rep) %do% {
                                    fPASS_out  <- eval(fPASS_call)
                                    list(power = purrr::pluck(fPASS_out, "power_value"), 
                                         eigenval = purrr::pluck(fPASS_out, "est_eigencomp", "est_eigenval"))
                               }
    }
    ret_obj      <- c(purrr::list_transpose(ret_obj), list(call=fPASS_call))
    # result saving : post-computation
    mkdirs(result_path)
    save(ret_obj, file = file.path(result_path, "power_analysis.Rdata"))
    
  } else if (fun.name == "Extract_Eigencomp_fDA"){
    if (parallel){
      registerDoParallel(cl <- makeCluster(detectCores()-1))
      ret_obj               <- foreach(itid=1:sim_rep, .export = c("mean.diff")) %dopar% {
                                        fPASS_out <- eval(fPASS_call)
                                        fPASS_out[c("scores_1", "scores_2")]
                                }
      stopCluster(cl)
    }  else{
    ret_obj                <-  foreach(itid=1:sim_rep) %do% {
                                        fPASS_out  <- eval(fPASS_call)
                                        fPASS_out[c("scores_1", "scores_2")]
                                }
  }
  ret_obj      <- c(ret_obj, list(call=fPASS_call))
  # result saving : post-computation
  mkdirs(result_path)
  save(ret_obj, file = file.path(result_path, "shrinkage_scores.Rdata"))
  }
} else{
  message("sim.design is NULL, nothing is returned!")
}

