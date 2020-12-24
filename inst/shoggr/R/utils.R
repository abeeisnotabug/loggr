makeMemoryBox <- function(type, total, used) {
  shinydashboard::valueBox(
    sprintf("%.1f / %.1f GiB", (total - used) / 1024, total / 1024),
    paste0(type, " available"),
    icon = icon("memory"),
    color = ifelse(
      used / total < 0.75,
      "green",
      ifelse(
        used / total < 0.9,
        "yellow",
        "red"
      )
    )
  )
}

makeCPUBox <- function(total, used) {
  shinydashboard::valueBox(
    sprintf("%s / %s", used, total),
    "cores in use",
    icon = icon("microchip"),
    color = ifelse(
      used < total,
      "green",
      ifelse(
        used < total * 2,
        "yellow",
        "red"
      )
    )
  )
}