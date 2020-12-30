loggr_object <- loggr::initialize_progress(prior = TRUE, type = c("y", "z"), i = 1:3, j = 1:4)

library(doParallel)

cl <- makeCluster(2, outfile = loggr_object$outfile)
registerDoParallel(cl)

foreach(type = c("y", "z")) %:%
  foreach(i = 1:3) %:%
  foreach(j = 1:4) %dopar% {
    loggr::log_progress(
      prior = TRUE, type, i, j,
      loggr_object = loggr_object,
      expr = {
        Sys.sleep(runif(1, 2, 5))
        if (i == 2) {
          loggr::dontcount()
        }
        log(rnorm(1))
      }
    )
  }

stopCluster(cl)
