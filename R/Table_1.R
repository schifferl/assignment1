#' Title
#'
#' @return
#' @export
#'
#' @examples
Table_1 <- function() {
  AgeGroup <- AgeGroup()
  MedAssist <- MedAssist()
  LocationType <- LocationType()
  PracticeType <- PracticeType()
  Race <- Race()
  bind_rows(AgeGroup, MedAssist, LocationType, PracticeType, Race)
}
