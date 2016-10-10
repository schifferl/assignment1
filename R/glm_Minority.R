#' Title
#'
#' @return
#' @export
#'
#' @examples
glm_Minority <- function() {
  alt_data <- filter(gardasil, Race != "Other/Unknown") %>% mutate(., Minority = ifelse(Race == "White", 0, 1)) %>% mutate(., Minority = factor(Minority, 0:1, c("White", "Minority")))
  the_glm <- glm(Completed ~ Minority, family = binomial, data = alt_data) %>% summary() %>% coef() %>% as.data.frame()

  or_values <- list("1.0")
  for (i in seq_along(the_glm$Estimate)[-1]) {
    or_values <- c(or_values, paste0(round(exp(the_glm$Estimate[i]), 2), " (", round(exp(the_glm$Estimate[i]) - the_glm$`Std. Error`[i] * qnorm(0.975), 2), "-", round(exp(the_glm$Estimate[i]) + the_glm$`Std. Error`[i] * qnorm(0.975), 2), ")"))
  }

  p_values <- list("")
  for (i in seq_along(the_glm$`Pr(>|z|)`)[-1]) {
    p_values <- c(p_values, round(the_glm$`Pr(>|z|)`[i], 2))
  }

  column_one <- group_by(alt_data, Minority) %>% summarise("n" = length(Minority))
  column_two <- filter(alt_data, Completed == "Completer") %>% group_by(Minority) %>% summarise("Completed 3 Vaccinations in 12 Mo (%)" = length(Minority))
  column_three <- group_by(alt_data, Minority) %>% summarise("OR (95% CI)" = "")
  column_four <- group_by(alt_data, Minority) %>% summarise("P" = "")
  all_columns <- list(column_one, column_two, column_three, column_four)
  glm_table <- Reduce(full_join, all_columns) %>% rename(., Group = Minority) %>% mutate("Completed 3 Vaccinations in 12 Mo (%)" = paste0(`Completed 3 Vaccinations in 12 Mo (%)`, " (", round(`Completed 3 Vaccinations in 12 Mo (%)` / n * 100, 1), ")"))
  glm_table[, 4] <- unlist(or_values)
  glm_table[, 5] <- unlist(p_values)
  glm_table
}
