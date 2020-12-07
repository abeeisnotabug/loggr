loggr::initialize_progress()

library(doParallel)

cl <- makeCluster(2, outfile = "testlogfile.cl.log")

foreach(type = c("u", "v")) %:%
  foreach(i = 1:3) %:%
    foreach(j = 1:20) %dopar% {
      loggr::log_progress(type, i, j)
    
      Sys.sleep(10)
    }

stopCluster(cl)
