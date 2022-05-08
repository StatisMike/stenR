#' Sample data of results for HEXACO-60 questionnaire
#'
#' @description
#' Dataset containing raw scores of HEXACO-60 questionnaire. They were obtained
#' during 2020 study on Polish incidental sample.
#'
#' @details
#' All HEXACO scales consists of 10 items with responses as numeric values 1-5
#' (so the absolute min and max are 10-50)
#'
#' @format A data frame with 204 rows and 9 variables
#' \describe{
#'  \item{user_id}{identity anonimized with 'ids::adjective_animal'}
#'  \item{sex}{sex of the participant ('M'ale, 'F'emale or 'O'ther)}
#'  \item{age}{age of the participant (15--62)}
#'  \item{HEX_H}{Honesty-Humility raw score (14--50)}
#'  \item{HEX_E}{Emotionality raw score (10--47)}
#'  \item{HEX_X}{eXtraversion raw score (11--46)}
#'  \item{HEX_A}{Agreeableness raw score (12--45)}
#'  \item{HEX_C}{Consciousness raw score (17--50)}
#'  \item{HEX_O}{Openness to Experience raw score (18--50)}
#' }
#'
"HEXACO_60"