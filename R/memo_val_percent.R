#' Calculate a value for inclusion in a memo, formatted as a percentage
#'
#' @param question A survey question name
#' @param values Response option value(s) to report. These are usually numeric and should match the questionnaire.
#' @param dataset A dataset of survey responses
#' @param wt A numeric variable of case weights
#' @param ... Additional arguments to pass to underlying freqs call
#' @param group_var Variable that specifies a subgroup
#' @param group_values Subgroup value(s)
#'
#' @return The weighted percentage of survey respondents that selected the specified values of the target question
#' @export
#'
#' @examples
#'
#' responses <- data.frame(
#' s_vote_choice = c(1, 2, 2, 3, 4, 2, NA),
#' s_vote_likelihood = c(1, 2, 2, 3, 4, 1, NA),
#' weights_final_trimmed = c(0.9, 0.9, 1.1, 1.1, 1, 1, 1)
#' )
#'
#'memo_val_percent(question = s_vote_choice, values = 1)
#'memo_val_percent(question = s_vote_choice, values = c(1:2))
#'
#'# Unweighted frequencies are possible but generally not recommended for memos
#'memo_val_percent(question = s_vote_choice, values = 1, wt = NULL)
#'
memo_val_percent <- function(
    question,
    values,
    dataset = responses,  # Keep this as the third argument to facilitate using the function in f(question, value) format
    wt = weights_final_trimmed,
    group_var = NULL,
    group_values = NULL,
    ...) {

  memo_val_decimal({{question}},
                   values,
                   dataset = dataset,  # Keep this as the third argument to facilitate using the function in f(question, value) format
                   wt = {{wt}},
                   group_var = {{group_var}},
                   group_values = group_values,
                   ...) %>%
    scales::percent()

}
