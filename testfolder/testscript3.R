loggr_object <- loggr::initialize_progress(j = 1:10)

library(doParallel)

cl <- makeCluster(2, outfile = "")
registerDoParallel(cl)

res <- foreach(j = 1:10, .errorhandling = "stop") %dopar% {
  res <- list(ls(all.names = T), search())
  loggr::log_progress(
    j,
    loggr_object = loggr_object,
    expr = {
      Sys.sleep(2)
      res <- append(res, list(ls(all.names = T), search()))
    }
  )
  res
}

stopCluster(cl)
