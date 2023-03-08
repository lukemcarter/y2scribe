#' Calculate a value for inclusion in a memo, formatted as a decimal
#'
#' @param question A survey question name
#' @param values Response option value(s) to report. These are usually numeric and should match the questionnaire.
#' @param dataset A dataset of survey responses
#' @param wt A numeric variable of case weights
#' @param ... Additional arguments to pass to underlying freqs call
#'
#' @return The weighted decimal value of survey respondents that selected the specified values of the target question
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
#'memo_val_decimal(question = s_vote_choice, values = 1)
#'memo_val_decimal(question = s_vote_choice, values = c(1:2))
#'
#'# Unweighted frequencies are possible but generally not recommended for memos
#'memo_val_decimal(question = s_vote_choice, values = 1, wt = NULL)
#'

memo_val_decimal <- function(
    question,
    values,
    dataset = responses,
    wt = weights_final_trimmed,
    ...) {

  dataset %>%
    y2clerk::freqs({{question}},
                   wt = {{wt}},
                   ...) %>%
    dplyr::filter(.data$value %in% c(values)) %>%
    dplyr::select(.data$value, .data$result) %>%
    dplyr::summarize(result = sum(.data$result)) %>%
    dplyr::pull(.data$result)

}
