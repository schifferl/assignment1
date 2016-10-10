#' Title
#'
#' @return
#' @export
#'
#' @examples
Table_2 <- function() {
  glm_AgeGroup <- glm_AgeGroup()
  glm_MedAssist <- glm_MedAssist()
  glm_InsuranceType <- glm_InsuranceType()
  glm_LocationType <- glm_LocationType()
  glm_PracticeType <- glm_PracticeType()
  glm_Minority <- glm_Minority()
  glm_Race <- glm_Race()
  glm_table <- bind_rows(glm_AgeGroup, glm_MedAssist, glm_InsuranceType, glm_LocationType, glm_PracticeType, glm_Minority, glm_Race)
  glm_table$P <- gsub("0$", "< 0.001", glm_table$P)
  glm_table
}
