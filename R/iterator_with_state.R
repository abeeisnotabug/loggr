iterator_with_state <- function() {
  i <- 0L

  nextEl <- function() (i <<- i + 1L)

  currentEl <- function() i

  list(
    nextElem = nextEl,
    currentElem = currentEl
  )
}
