top <- function() {
  top_out <- system("top -n 1 -b -c -w 512", intern = TRUE)
  
  top_head <- top_out[1:5]
  top_procs <- top_out[7:length(top_out)]
}