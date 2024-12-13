#' Mouse Trial Datasets
#'
#' Datasets containing experimental results from a mouse trial
#'
#' @format Three data frames:
#' \describe{
#'   \item{mouse_birth}{Birth data with 32 rows and 4 columns:
#'     \itemize{
#'       \item ID: Mouse identifier (e.g., "C57BL6J_M_1")
#'       \item Sex: Sex of the mouse (M/F)
#'       \item Num: Numeric identifier
#'       \item Treatment: Treatment group (Plac/Treatment)
#'     }
#'   }
#'   \item{mouse_weight}{Body weight measurements with 32 rows and 7 columns:
#'     \itemize{
#'       \item ID: Mouse identifier
#'       \item Body Weight 1: First weight measurement
#'       \item Date_Weight_1: Date of first measurement
#'       \item Body Weight 2: Second weight measurement
#'       \item Date_Weight_2: Date of second measurement
#'       \item Body Weight 3: Third weight measurement
#'       \item Date_Weight_3: Date of third measurement
#'     }
#'   }
#'   \item{mouse_outcome}{Outcome data with 32 rows and 7 columns:
#'     \itemize{
#'       \item ID: Mouse identifier
#'       \item Outcome 1: First outcome measurement
#'       \item Date_Outcome_1: Date of first outcome
#'       \item Outcome 2: Second outcome measurement
#'       \item Date_Outcome_2: Date of second outcome
#'       \item Outcome 3: Third outcome measurement
#'       \item Date_Outcome_3: Date of third outcome
#'     }
#'   }
#' }
#'
#' @source {Trial data from STAT 108 mouse vaccine study}
"mouse_birth"

#' @rdname mouse_birth
"mouse_weight"

#' @rdname mouse_birth
"mouse_outcome"
