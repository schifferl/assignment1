#' Title
#'
#' @return
#' @export
#'
#' @examples
AgeGroup <- function() {
  column_one <- gardasil %>% group_by(AgeGroup) %>% summarise("n (%)" = length(AgeGroup)) %>% mutate("n (%)" = paste0(`n (%)`, " (", round(`n (%)` / sum(`n (%)`) * 100, 1), ")"))
  column_two <- filter(gardasil, Shots == "Received 1 Shot") %>% group_by(AgeGroup) %>% summarise("Received 1 Shot" = length(AgeGroup)) %>% mutate("Received 1 Shot" = paste0(`Received 1 Shot`, " (", round(`Received 1 Shot` / sum(`Received 1 Shot`) * 100, 1), "% ± ", round(sqrt(1 / `Received 1 Shot` * `Received 1 Shot` / sum(`Received 1 Shot`) * (1 - `Received 1 Shot` / sum(`Received 1 Shot`))) * 100, 1), "%)"))
  column_three <- filter(gardasil, Shots == "Received 2 Shots" | Shots == "Received 3 Shots" & Completed == "Noncompleter") %>% group_by(AgeGroup) %>% summarise("Received 2 Shots" = length(AgeGroup)) %>% mutate("Received 2 Shots" = paste0(`Received 2 Shots`, " (", round(`Received 2 Shots` / sum(`Received 2 Shots`) * 100, 1), "% ± ", round(sqrt(1 / `Received 2 Shots` * `Received 2 Shots` / sum(`Received 2 Shots`) * (1 - `Received 2 Shots` / sum(`Received 2 Shots`))) * 100, 1), "%)"))
  column_four <- filter(gardasil, Completed == "Completer") %>% group_by(AgeGroup) %>% summarise("Completer" = length(AgeGroup)) %>% mutate("Completer" = paste0(`Completer`, " (", round(`Completer` / sum(`Completer`) * 100, 1), "% ± ", round(sqrt(1 / `Completer` * `Completer` / sum(`Completer`) * (1 - `Completer` / sum(`Completer`))) * 100, 1), "%)"))
  column_five <- filter(gardasil, Completed == "Noncompleter") %>% group_by(AgeGroup) %>% summarise("Noncompleter" = length(AgeGroup)) %>% mutate("Noncompleter" = paste0(`Noncompleter`, " (", round(`Noncompleter` / sum(`Noncompleter`) * 100, 1), "% ± ", round(sqrt(1 / `Noncompleter` * `Noncompleter` / sum(`Noncompleter`) * (1 - `Noncompleter` / sum(`Noncompleter`))) * 100, 1), "%)"))
  all_columns <- list(column_one, column_two, column_three, column_four, column_five)
  Reduce(full_join, all_columns) %>%
    rename(., Group = AgeGroup)
}
