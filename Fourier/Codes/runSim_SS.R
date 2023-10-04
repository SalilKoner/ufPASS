

rm(list=ls())

args         <- commandArgs(TRUE)
fun.name     <- as.character(args[1])
design_num   <- as.integer(args[2])
sim_rep      <- as.integer(args[3])
parallel     <- as.logical(args[4])

# fun.name               <- "PASS_Proj_Test_ufDA"
# fun.name               <- "Extract_Eigencomp_fDA"
# design_num             <- 12
# sim_rep                <- 5
# parallel               <- TRUE

message("function name = ", fun.name,
        ", design_num = ", design_num,
        ", sim_rep = ", sim_rep,
        ", parallel = ", parallel, "\n")

load("Files/sim_design_SS.Rdata")
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
}

rm("sim.design.all")

# Result saving part : pre-computation

if (!is.null(sim.design)){
  folder_params <- c("power", "nobs_per_subj", "delta", "missing_percent")
  fun_params    <- list(ifelse(fun.name == "PASS_Proj_Test_ufDA", "target.power", "eval_SS"),
                        "nobs_per_subj", list("mean_diff_add_args", "delta"), "missing_percent")
  folder_path   <- paste0(sapply(seq_along(folder_params), function(i) 
                     paste0(folder_params[[i]], "=",
                            as.character(enquote(purrr::pluck(sim.design, !!!fun_params[[i]], .default = "NULL"))[2]) )),
                          collapse = .Platform$file.sep)
  
  if (fun.name == "PASS_Proj_Test_ufDA"){
    add_str       <- paste0("npc=", purrr::pluck(sim.design, "npc_to_use", .default = "NULL"))
    result_path   <- file.path("Results", "Sample_Size_Analysis", folder_path, "Theoretical", add_str)
    if (file.exists(file.path(result_path, "sample_size_analysis.Rdata"))) {
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
                                    list(power = purrr::pluck(fPASS_out, "required_SS"),
                                         eigenval = purrr::pluck(fPASS_out, "est_eigencomp", "est_eigenval"))
                                }
      stopCluster(cl)
    } else{
      ret_obj               <- foreach(itid=1:sim_rep) %do% {
                                    fPASS_out  <- eval(fPASS_call)
                                    list(power = purrr::pluck(fPASS_out, "required_SS"), 
                                         eigenval = purrr::pluck(fPASS_out, "est_eigencomp", "est_eigenval"))
                               }
    }
    ret_obj      <- c(purrr::list_transpose(ret_obj), list(call=fPASS_call))
    # result saving : post-computation
    mkdirs(result_path)
    save(ret_obj, file = file.path(result_path, "sample_size_analysis.Rdata"))
    
  } 
} else{
  message("sim.design is NULL, nothing is returned!")
}

