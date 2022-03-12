#' Rotation adaptation data for use with the two-rate model
#'
#' A dataset with a perturbation schedule and the reach direction errors of a group
#' of participants doing a typical adaptation schedule that evokes a rebound.
#' The data is stored in a somewhat wide format, atypical for R.
#'
#' @format A data frame with 164 rows and 20 columns:
#' \describe{
#'   \item{block}{blocks 1 through 4: aligned, rotated, reversed, error-clamped}
#'   \item{trial}{trial within the block}
#'   \item{schedule}{the rotation on the trial, with NA for error-clamp trials}
#'   \item{p003}{reach deviations of participant 3 at 1/3 target distance}
#'   \item{p005}{reach deviations of participant 3 at 1/3 target distance}
#'   \item{p006}{reach deviations of participant 3 at 1/3 target distance}
#'   \item{p009}{reach deviations of participant 3 at 1/3 target distance}
#'   \item{p011}{reach deviations of participant 3 at 1/3 target distance}
#'   \item{p012}{reach deviations of participant 3 at 1/3 target distance}
#'   \item{p015}{reach deviations of participant 3 at 1/3 target distance}
#'   \item{p017}{reach deviations of participant 3 at 1/3 target distance}
#'   \item{p018}{reach deviations of participant 3 at 1/3 target distance}
#'   \item{p021}{reach deviations of participant 3 at 1/3 target distance}
#'   \item{p023}{reach deviations of participant 3 at 1/3 target distance}
#'   \item{p024}{reach deviations of participant 3 at 1/3 target distance}
#'   \item{p027}{reach deviations of participant 3 at 1/3 target distance}
#'   \item{p029}{reach deviations of participant 3 at 1/3 target distance}
#'   \item{p030}{reach deviations of participant 3 at 1/3 target distance}
#'   \item{p033}{reach deviations of participant 3 at 1/3 target distance}
#'   \item{p035}{reach deviations of participant 3 at 1/3 target distance}
#' }
"tworatedata"

#' Localization data points to illustrate circle-fitting. Twenty-five
#' localization points each, from one well-aligned and one not so well-aligned
#' participant.
#' 
#' load with: data(localization)
#' 
#' @format Data frame with 50 rows and 6 columns:
#' \describe{
#'   \item{targetangle_deg}{angle of actual hand position relative to home}
#'   \item{handx_cm}{hand position x-coordinate in centimeters}
#'   \item{handy_cm}{hand position y-coordinate in centimeters}
#'   \item{tapx_cm}{localization x-coordinate in centimeters}
#'   \item{tapy_cm}{localization y-coordinate in centimeters}
#'   \item{participant}{participant ID, either 'a', or 'b'}
#' }
"localization"

