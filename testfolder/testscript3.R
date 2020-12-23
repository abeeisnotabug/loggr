library(doParallel)

cl <- makeCluster(2, outfile = "")
registerDoParallel(cl)

res <- foreach(j = 1:10, .errorhandling = "stop") %dopar% {
  Sys.sleep(2)
  if (j == 2) {
    log("a")
  } else {
    print(j)
    warning(j)
    log(2)
  }
}

stopCluster(cl)
