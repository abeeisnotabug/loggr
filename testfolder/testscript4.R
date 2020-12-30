loggr_object <- loggr::initialize_progress(gugu = c("ah", "oh"), k = 1:3, rep = 1:5)

library(doParallel)

cl <- makeCluster(2, outfile = loggr_object$outfile)
registerDoParallel(cl)

foreach(gugu = c("ah", "oh")) %:%
  foreach(k = 1:3) %:%
  foreach(rep = 1:5) %dopar% {
    loggr::log_progress(
      gugu, k, rep,
      loggr_object = loggr_object,
      expr = {
        Sys.sleep(runif(1, 2, 5))
        log(rnorm(1))
      }
    )
  }

stopCluster(cl)
