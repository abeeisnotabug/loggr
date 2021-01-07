makeMemoryBox <- function(type, total, used) {
  makeResizedValueBox(
    value = sprintf("%.1f / %.1f GiB", (total - used) / 1024, total / 1024),
    subtitle = paste0(type, " available"),
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
  makeResizedValueBox(
    value = sprintf("%s / %s", used, total),
    subtitle = "cores in use",
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

makeResizedValueBox <- function(value, subtitle, icon, color, href = NULL) {
  valueBox(
    value = tags$p(
      style = "font-size: 80%;",
      value
    ),
    subtitle = subtitle,
    icon = tags$i(class = icon$attribs$class, style = "font-size: 50px"),
    color = color,
    href = href
  )
}

makeButton <- function(inputId, label, icon) {
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

getDigitsToRound <- function(secondsToRound) {
  ifelse(secondsToRound < 1, 3, ifelse(secondsToRound < 10, 2, ifelse(secondsToRound < 100, 1, 0)))
}

roundSecondsToPeriod <- function(secondsToRound, addUnit = NULL) {
  toPaste <- seconds_to_period(round(secondsToRound, getDigitsToRound(secondsToRound)))
  paste0(toPaste, if (is.na(toPaste)) NULL else addUnit)
}
