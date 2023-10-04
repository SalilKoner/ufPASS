rm(list=ls())

sample_size.all        <- list(NULL)
target.power.all       <- list(0.7, 0.8, 0.9)
cov.type.all           <- list("ST")
cov.par.all            <- list(quote(list("var" = 1, "cor" = nlme::corCompSymm(0.5, form = ~ time | Subject))))
alloc.ratio.all        <- list(c(1,1))
mean_diff_fnm.all      <- list("mean.diff")
sigma2.e.all           <- list(0.001)
sig.level.all          <- list(0.05)
missing_type.all       <- list("nomiss")
missing_percent.all    <- list(0)
nobs_per_subj.all      <- list(5, 8)
fpca_method.all        <- list("fpca.sc")
fpca_optns.all         <- list(list("pve" = 0.95, "nbasis" = 12), list("pve" = 0.95, "nbasis" = 12, "center" = TRUE))
nWgrid.all             <- list(101)
eval_SS.all            <- list(1e4)
npc_to_use.all         <- list(NULL, 1)
return.eigencomp.all   <- list(TRUE)
nsim.all               <- list(1e4)
mean_diff_add_args.all <- list(list(delta = 0.5), list(delta=0.75), list(delta=1))
obs.design.all         <- list(list(design = "functional", fun.domain = c(0,1)))
work.grid.all          <- list(quote(seq(0,1,length.out=101)), quote(gss::gauss.quad(101, c(0,1))$pt)) # For Extract_Eigencomp_fDA() 
data.driven.scores.all <- list(TRUE) # For Extract_Eigencomp_fDA() 

arguments_list         <- ls()[stringr::str_detect(ls(), ".all")]
sim.settings           <- lapply(arguments_list, get)
names(sim.settings)    <- stringr::str_remove_all(arguments_list, ".all")
sim.design.all         <- tidyr::expand_grid(!!!sim.settings) 


save(sim.design.all, file = "Files/sim_design_SS.Rdata")



