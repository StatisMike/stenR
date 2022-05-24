#' Sample data of HEXACO-60 questionnaire results (summed scales)
#'
#' @description
#' Dataset containing summed scale scores of HEXACO-60 questionnaire. They 
#' were obtained during 2020 study on Polish incidental sample.
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

#' Sample data of SLCS questionnaire results (item scores)
#' @description 
#' Dataset containing individual items answers of SLCS questionnaire. They
#' were obtained during 2020 study on Polish incidental sample.
#' 
#' @details 
#' All SLCS item responses can take integer values 1-5. The measure consists
#' of two sub-scales: Self-Liking and Self-Competence, and the General Score
#' can also be calculated. Below are the item numbers that are used for each
#' sub-scale (`R` near the number means that the item need to be reversed.)
#' 
#' - Self-Liking: 1R, 3, 5, 6R, 7R, 9, 11, 15R
#' - Self-Competence: 2, 4, 8R, 10R, 12, 13R, 14, 16
#' - General Score: All of the above items (they need to be reversed as in sub-scales)
#' 
#' @format A data frame with 103 rows and 19 variables
#' \describe{
#'  \item{user_id}{identity anonimized with 'ids::adjective_animal'}
#'  \item{sex}{sex of the participant ('M'ale, 'F'emale or 'O'ther)}
#'  \item{age}{age of the participant (15--68)}
#'  \item{SLCS_1 to SLCS_16}{Score for each of measure items. (1--5)}
#' }
#' 
"SLCS"