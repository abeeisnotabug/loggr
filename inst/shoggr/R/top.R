library(stringr)

top <- function() {
  top_out <- system("top -n 1 -b -c -w 512", intern = TRUE)

  top_head <- top_out[1:5]
  top_procs <- top_out[7:length(top_out)]

  procs_mat <- str_split_fixed(str_trim(top_procs[2:length(top_procs)]), "[ ]+", 12)
  colnames(procs_mat) <- str_split_fixed(str_trim(top_procs[1]), "[ ]+", 12)
  procs_df <- as.data.frame(procs_mat)
  procs_df[1:11] <- lapply(procs_df[1:11], stringr::str_replace_all, ",", ".")
  procs_df[1:11] <- lapply(procs_df[1:11], utils::type.convert, as.is = TRUE)

  procs_df[, c("VIRT", "RES", "SHR")] <- procs_df[, c("VIRT", "RES", "SHR")] / 1024
  names(procs_df)[5:7] <- paste(names(procs_df)[5:7], "(MiB)")

  mem_mat <- str_split_fixed(top_head[4:5], "[ (?!:)]+", 10)
  mem_mat[, c(3, 5, 7, 9)] <- str_replace_all(mem_mat[, c(3, 5, 7, 9)], ",", ".")
  mem_mat[, c(4, 6, 8)] <- str_replace_all(mem_mat[, c(4, 6, 8)], "[,.]", "")
  ram_df <- data.frame(type = "ram", mib = mem_mat[1, c(3, 5, 7, 9)], measure = mem_mat[1, c(4, 6, 8, 10)])
  swap_df <- data.frame(type = "swap", mib = mem_mat[2, c(3, 5, 7)], measure = mem_mat[2, c(4, 6, 8)])

  mem_df <- rbind(ram_df, swap_df)
  mem_df$mib <- type.convert(mem_df$mib)

  mem_df$mib[1] - mem_df$mib[3] - mem_df$mib[4]

  list(procs_df = procs_df, mem_df = mem_df)
}
