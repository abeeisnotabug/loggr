loggr_object <- loggr::initialize_progress(prior = TRUE, type = c("u", "v"), i = 1:3, j = 1:10)

library(doParallel)

cl <- makeCluster(2, outfile = loggr_object$outfile)
registerDoParallel(cl)

foreach(type = c("u", "v")) %:%
  foreach(i = 1:3) %:%
  foreach(j = 1:10) %dopar% {
    loggr::log_progress(
      prior = TRUE, type, i, j,
      loggr_object = loggr_object,
      expr = {
        Sys.sleep(runif(1))
        log(rnorm(1))
      }
    )
  }

stopCluster(cl)
