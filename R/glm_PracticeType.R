#' Title
#'
#' @return
#' @export
#'
#' @examples
glm_PracticeType <- function() {
  the_glm <- glm(Completed ~ PracticeType, family = binomial, data = gardasil) %>% summary() %>% coef() %>% as.data.frame()

  or_values <- list("1.0")
  for (i in seq_along(the_glm$Estimate)[-1]) {
    or_values <- c(or_values, paste0(round(exp(the_glm$Estimate[i]), 2), " (", round(exp(the_glm$Estimate[i]) - the_glm$`Std. Error`[i] * qnorm(0.975), 2), "-", round(exp(the_glm$Estimate[i]) + the_glm$`Std. Error`[i] * qnorm(0.975), 2), ")"))
  }

  p_values <- list("")
  for (i in seq_along(the_glm$`Pr(>|z|)`)[-1]) {
    p_values <- c(p_values, round(the_glm$`Pr(>|z|)`[i], 2))
  }

  column_one <- group_by(gardasil, PracticeType) %>% summarise("n" = length(PracticeType))
  column_two <- filter(gardasil, Completed == "Completer") %>% group_by(PracticeType) %>% summarise("Completed 3 Vaccinations in 12 Mo (%)" = length(PracticeType))
  column_three <- group_by(gardasil, PracticeType) %>% summarise("OR (95% CI)" = "")
  column_four <- group_by(gardasil, PracticeType) %>% summarise("P" = "")
  all_columns <- list(column_one, column_two, column_three, column_four)
  glm_table <- Reduce(full_join, all_columns) %>% rename(., Group = PracticeType) %>% mutate("Completed 3 Vaccinations in 12 Mo (%)" = paste0(`Completed 3 Vaccinations in 12 Mo (%)`, " (", round(`Completed 3 Vaccinations in 12 Mo (%)` / n * 100, 1), ")"))
  glm_table[, 4] <- unlist(or_values)
  glm_table[, 5] <- unlist(p_values)
  glm_table
}
