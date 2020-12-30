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

makeRefreshButton <- function(inputId, label, icon) {
  fluidRow(
    align = "center",
    shinyWidgets::actionBttn(
      inputId = inputId,
      label = label,
      style = "bordered",
      icon = icon(icon),
      block = FALSE,
      size = "sm"
    )
  )
}


makeSelfNamedVector <- function(input) {
  `names<-`(input, input)
}

eval_parse_text <- function(text) {
  eval(parse(text = text))
}


countFinishedIters <- function(currents, totals) {
  if (length(currents) > 1) {
    finishedItersInLowerConditions <- (currents[1] - 1) * prod(totals[-1])
    finishedItersInThisCondition <- countFinishedIters(currents[-1], totals[-1])
    
    sum(finishedItersInLowerConditions, finishedItersInThisCondition)
  } else {
    currents
  }
}
