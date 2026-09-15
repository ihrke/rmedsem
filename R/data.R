#' Mate-Choice Survey Data from Trondheim
#'
#' Data from a survey on mate-choice in Trondheim, collected in 2021 using a
#' convenience sampling procedure. Participants rated how well physical
#' features (`smv_attr_face`, `smv_attr_body`, `smv_sexy`) and personality
#' features (`smv_kind` to `smv_sociable`) described them as romantic
#' partners, answered items on self-esteem (`ses_*`) and reported how often
#' they had experienced feelings related to mental well-being in the past two
#' weeks (`mwb_*`). All items were measured on a scale from 1 to 5.
#'
#' @format ## `mchoice`
#' A data frame with 1067 rows and 15 columns:
#' \describe{
#'   \item{smv_attr_face}{How well does this describe you as a partner? (attractive face), \[1\] very bad - \[5\] very well}
#'   \item{smv_attr_body}{How well does this describe you as a partner? (attractive body), \[1\] very bad - \[5\] very well}
#'   \item{smv_sexy}{How well does this describe you as a partner? (sexy), \[1\] very bad - \[5\] very well}
#'   \item{ses_satis}{On the whole, I am satisfied with myself, \[1\] totally disagree - \[5\] totally agree}
#'   \item{ses_qualities}{I feel that I have a number of good qualities, \[1\] totally disagree - \[5\] totally agree}
#'   \item{ses_able_todo}{I am able to do things as well as most other people, \[1\] totally disagree - \[5\] totally agree}
#'   \item{mwb_optimistic}{I have been feeling optimistic about the future, \[1\] never - \[5\] always}
#'   \item{mwb_useful}{I have been feeling useful, \[1\] never - \[5\] always}
#'   \item{mwb_energy}{I have had energy to spare, \[1\] never - \[5\] always}
#'   \item{smv_kind}{How well does this describe you as a partner? (kind), \[1\] very bad - \[5\] very well}
#'   \item{smv_caring}{How well does this describe you as a partner? (caring), \[1\] very bad - \[5\] very well}
#'   \item{smv_understanding}{How well does this describe you as a partner? (understanding), \[1\] very bad - \[5\] very well}
#'   \item{smv_make_laughh}{How well does this describe you as a partner? (make people laugh), \[1\] very bad - \[5\] very well}
#'   \item{smv_funny}{How well does this describe you as a partner? (funny), \[1\] very bad - \[5\] very well}
#'   \item{smv_sociable}{How well does this describe you as a partner? (sociable), \[1\] very bad - \[5\] very well}
#' }
#'
#' @examples
#' str(mchoice)
#'
#' mod.txt <- "
#'   OwnLook  =~ smv_attr_face + smv_attr_body + smv_sexy
#'   SelfEst  =~ ses_satis + ses_qualities + ses_able_todo
#'   MentWell =~ mwb_optimistic + mwb_useful + mwb_energy
#'   SelfEst  ~ OwnLook
#'   MentWell ~ OwnLook + SelfEst
#' "
#' mod <- lavaan::sem(mod.txt, data=mchoice)
#' rmedsem(mod, indep="OwnLook", med="SelfEst", dep="MentWell")
"mchoice"


#' Fitness Center Survey Data from Trondheim
#'
#' Data from a survey in a fitness center in Trondheim.
#'
#' @format ## `workout`
#' A data frame with 246 rows and 12 columns:
#' \describe{
#'    \item{age}{Age in years}
#'    \item{lweight}{How important is following to workout- to loose weight}
#'    \item{calories}{How important is following to workout- to burn calories}
#'    \item{cweight}{How important is following to workout- to control my weight}
#'    \item{body}{How important is following to workout- to have a good body}
#'    \item{appear}{How important is following to workout- to improve my appearance}
#'    \item{attract}{How important is following to workout- to look more attractive}
#'    \item{muscle}{How important is following to workout- to develop my muscles}
#'    \item{strength}{How important is following to workout- to get stronger}
#'    \item{endur}{How important is following to workout- to increase my endurance}
#'    \item{face}{How well does the following describe you as a person -  attractive face}
#'    \item{sexy}{How well does the following describe you as a person - sexy}
#' }
#'
#' @examples
#' str(workout)
#'
#' mod.txt <- "
#'   Attractive =~ face + sexy
#'   Appearance =~ body + appear + attract
#'   Muscle     =~ muscle + strength + endur
#'   Appearance ~ Attractive + age
#'   Muscle     ~ Appearance + Attractive + age
#' "
#' mod <- lavaan::sem(mod.txt, data=workout)
#' rmedsem(mod, indep="Attractive", med="Appearance", dep="Muscle")
"workout"


#' High School and Beyond Demo Dataset
#'
#' Demographic information and standardized test scores of 200 students from
#' the High School and Beyond survey, as distributed by the UCLA Statistical
#' Methods and Data Analytics group.
#'
#' @format ## `hsbdemo`
#' A data frame with 200 rows and 13 columns:
#' \describe{
#'  \item{id}{Student ID}
#'  \item{female}{Gender, `"female"` or `"male"`}
#'  \item{ses}{Socio-economic status, `"low"`, `"middle"` or `"high"`}
#'  \item{schtyp}{School type, `"public"` or `"private"`}
#'  \item{prog}{Type of program, `"general"`, `"academic"` or `"vocation"`}
#'  \item{read}{Reading score}
#'  \item{write}{Writing score}
#'  \item{math}{Math score}
#'  \item{science}{Science score}
#'  \item{socst}{Social studies score}
#'  \item{honors}{Enrollment in honors program, `"enrolled"` or `"not enrolled"`}
#'  \item{awards}{Number of awards}
#'  \item{cid}{Class ID}
#' }
#'
#' @examples
#' str(hsbdemo)
#'
#' mod <- lavaan::sem("read ~ math\nscience ~ read + math", data=hsbdemo)
#' rmedsem(mod, indep="math", med="read", dep="science")
"hsbdemo"
