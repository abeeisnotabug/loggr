load("../../_shared/sim_prep/simu_conditions.RData")
load("../../_shared/sim_prep/fun_analyze.RData")

this_nnoise <- nnoise[1]

loggr_object <- loggr::initialize_progress(
  this_nnoise = this_nnoise,
  this_Rsq = Rsq[1],
  this_collin = collin[4],
  this_n_sample = n_sample[1],
  r = 68:70
)

library(doParallel)

cl <- makeCluster(2, outfile = loggr_object$outfile)
registerDoParallel(cl)

res <- foreach(this_Rsq = Rsq[1], .packages = c("lavaan", "regsem")) %:% # Folder to take the dataset from
  foreach(this_collin = collin[4]) %:%
  foreach(this_n_sample = n_sample[1]) %:%
  foreach(r = 68:70) %dopar% {
      loggr::log_progress(
        this_nnoise, this_Rsq, this_collin, this_n_sample, r,
        loggr_object = loggr_object,
        expr = {
          temp_data <- readRDS(
            file = sprintf(
              "../../_shared/sim_prep/Daten/nnoise%s/Rsq%s/collin%s/n_sample%s/%s",
              this_nnoise, this_Rsq, this_collin, this_n_sample,
              sprintf("data_%i_%s_%s_%i_r%i.rds",
                      as.integer(this_nnoise),
                      as.character(this_Rsq),
                      as.character(this_collin),
                      this_n_sample,
                      r)
            )
          )

          # loaded file will have the name as specified when it was generated (currently "d")
          temp_model <- fun_analyze(n_allpred = 9 + this_nnoise)

          temp_fit <- sem(model = temp_model,
                          data = temp_data)

          reg_ob <- cv_regsem(model = temp_fit,
                              data = temp_data,
                              type = "ridge",
                              pars_pen = "regressions",
                              lambda.start = 0,
                              jump = 0.01,
                              n.lambda = 50,
                              verbose = FALSE)
        }
      )
    }

stopCluster(cl)
