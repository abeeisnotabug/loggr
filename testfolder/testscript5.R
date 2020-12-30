loggr_object <- loggr::initialize_progress(gugu = c("eh", "ih"), k = 1:3, rep = 1:5)

library(doParallel)

cl <- makeCluster(2, outfile = loggr_object$outfile)
registerDoParallel(cl)

foreach(gugu = c("eh", "ih")) %:%
  foreach(k = 1:3) %:%
  foreach(rep = 1:5) %dopar% {
    loggr::log_progress(
      gugu, k, rep,
      loggr_object = loggr_object,
      expr = {
        Sys.sleep(runif(1, 3, 6))
        log(rnorm(1))
      }
    )
  }

stopCluster(cl)
