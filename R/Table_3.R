#' Title
#'
#' @return
#' @export
#'
#' @examples
Table_3 <- function() {
  alt_data <- mutate(gardasil, Minority = ifelse(Race == "White", 0, 1)) %>% mutate(., Minority = factor(Minority, 0:1, c("White", "African American or Hispanic")))
  the_glm <- glm(Completed ~ AgeGroup + MedAssist + LocationType + PracticeType + Minority, family = binomial, data = alt_data) %>% summary() %>% coef() %>% as.data.frame()
  the_glm$Estimate <- exp(the_glm$Estimate)
  the_glm$`95% CI Lower` <- the_glm$Estimate - qnorm(0.975) * the_glm$`Std. Error`
  the_glm$`95% CI Upper` <- the_glm$Estimate + qnorm(0.975) * the_glm$`Std. Error`
  the_glm$`OR (95% CI)` <- paste0(round(the_glm$Estimate, 2), " (", round(the_glm$`95% CI Lower`, 2), "-", round(the_glm$`95% CI Upper`, 2), ")")
  the_glm$P <- round(the_glm$`Pr(>|z|)`, 3)

  filtered_data <- filter(gardasil, Race != "Other/Unknown") %>% mutate(., Minority = ifelse(Race == "White", 0, 1)) %>% mutate(., Minority = factor(Minority, 0:1, c("White", "Minority")))
  alt_glm <- glm(Completed ~ AgeGroup + MedAssist + LocationType + PracticeType + Minority, family = binomial, data = filtered_data) %>% summary() %>% coef() %>% as.data.frame()
  alt_glm$Estimate <- exp(alt_glm$Estimate)
  alt_glm$`95% CI Lower` <- alt_glm$Estimate - qnorm(0.975) * alt_glm$`Std. Error`
  alt_glm$`95% CI Upper` <- alt_glm$Estimate + qnorm(0.975) * alt_glm$`Std. Error`
  alt_glm$`OR (95% CI)` <- paste0(round(alt_glm$Estimate, 2), " (", round(alt_glm$`95% CI Lower`, 2), "-", round(alt_glm$`95% CI Upper`, 2), ")")
  alt_glm$P <- round(alt_glm$`Pr(>|z|)`, 3)

  column_one <- c(levels(alt_data$AgeGroup), levels(alt_data$MedAssist), levels(alt_data$LocationType), levels(alt_data$PracticeType), levels(alt_data$Minority))
  column_two <- c("1.00", the_glm$`OR (95% CI)`[2], "1.00", the_glm$`OR (95% CI)`[3], "1.00", the_glm$`OR (95% CI)`[4], "1.00", the_glm$`OR (95% CI)`[5:6], "1.00", alt_glm$`OR (95% CI)`[7])
  column_three <- c("", the_glm$P[2], "", the_glm$P[3], "", the_glm$P[4], "", the_glm$P[5:6], "", alt_glm$P[7])
  glm_table <- cbind(column_one, column_two, column_three) %>% as.data.frame()
  colnames(glm_table) <- c("Characteristic", "OR (95% CI)", "P")
  glm_table$P <- gsub("0$", "< 0.001", glm_table$P)
  glm_table
}
