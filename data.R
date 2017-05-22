
#' F1 and F2 from NSP hVd words
#'
#' A dataset containing hand-measured F1×F2 values from the "hVd" words in the
#' Nationwide Speech project corpus.  48 American English talkers produced the
#' 11 monophongal vowels of American English /i ɪ e ɛ æ ʌ u ʊ o ɔ ɑ/, 5 times
#' each (with few exceptions).  Talkers were students at Indiana University and
#' are coded with demographic details (sex, age, and region of origin) and were
#' selected to represent the six major dialect regions of American English
#' (based on where they grew up, before attending IU).
#' 
#' @format A 2,659-row data frame with columns:
#' \describe{
#'   \item{Talker}{Anonymized talker ID}
#'   \item{Dialect}{Talker's native dialect region}
#'   \item{Sex}{Talker's sex ("f" for female, "m" for male)}
#'   \item{Age}{Talker's age at the time of recording}
#'   \item{Word}{Lexical item for eliciting vowel}
#'   \item{Vowel}{Two-letter ASCII (ARPAbet-like) code for vowel identity}
#'   \item{Vowel_ipa}{Unicode IPA symbol for vowel}
#'   \item{Token}{Repetition number for each vowel token}
#'   \item{F1}{First formant frequency (Hz).}
#'   \item{F2}{Second formant frequency (Hz).}
#' }
#'
#' @source Clopper, C. G., Pisoni, D. B., & de Jong, K. J. (2005). Acoustic
#'   characteristics of the vowel systems of six regional varieties of American
#'   English. The Journal of the Acoustical Society of America, 118(3),
#'   1661. \url{http://dx.doi.org/10.1121/1.2000774}
"nsp_vows"

#' Vowels that are likely to be confused in each dialect
#'
#' Based on shifts described qualitatively in Clopper, Pisoni, and de Jong
#' (2005).
#'
#' @format A 18-row data frame with columns:
#' \describe{
#'   \item{Dialect}{Dialect region}
#'   \item{Vowel}{Vowel code}
#'   \item{Competitor}{Vowel code of a likely confusion for Vowel}
#'   \item{Description}{Reason for potential confusion}
#' }
#'
#' @seealso \code{\link{nsp_vows}}
"clopper_competitors"
