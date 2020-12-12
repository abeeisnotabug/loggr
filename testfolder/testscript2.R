loggr_object <- loggr::initialize_progress(prior = TRUE, type = c("w", "x"), i = 1:3, j = 1:20)

library(doParallel)

cl <- makeCluster(2, outfile = loggr_object$outfile)
registerDoParallel(cl)

foreach(type = c("w", "x")) %:%
  foreach(i = 1:3) %:%
    foreach(j = 1:20) %dopar% {
      loggr::log_progress(
        prior = TRUE, type, i, j,
        loggr_object = loggr_object,
        expr = {
          Sys.sleep(10)
        }
      )
    }

stopCluster(cl)
