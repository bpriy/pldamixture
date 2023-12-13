#' LIFE-M Data
#'
#' The `lifem` data set contains a subset of data from the Life-M project (\url{https://life-m.org/}) on 3,238 individuals born between 1883
#' to 1906. These records were obtained from linking birth certificates and death certificates either of two
#' ways. A fraction of the records (2,159 records) were randomly sampled to be “hand-linked at some level” (HL).
#' These records are high quality and were manually linked at some point by trained research assistants.
#' The remaining records were “purely machine-linked” (ML) based on probabilistic record linkage without clerical
#' review. The Life-M team expects the mismatch rate among these records to be around 5% (Bailey et al.
#' 2022). Of interest is the relationship between age at death and year of birth. The `lifem` demo data set consists of 2,159 hand-linked records
#' and 1,079 records (for ~2:1 HL-ML ratio) that were randomly sampled from the purely machine-linked records.
#'
#' \itemize{
#'   \item yob: year of birth (value from 1883 and 1906)
#'   \item unit_yob: yob re-scaled to the unit interval for analysis (between 0 and 1). If X is the yob, we use the following:
#'   (X – min(X)) / (max(X) – min(X)) = a * X + b, a = 1/(max(X) – min(X)), b = -min(X)*a
#'   \item age_at_death: age at death (in years)
#'   \item hndlnk: whether record was purely machine-linked or hand-linked at some level.
#'   \item commf: commonness score of first name (between 0 and 1). it is based on the 1940 census.
#'   It is a ratio of the log count of the individual’s first name over the log count of the most
#'   commonly occurring first name in the census.
#'   \item comml: commonness score of last name (between 0 and 1). it is based on the 1940 census.
#'   It is a ratio of the log count of the individual’s last name over the log count of the most
#'   commonly occurring last name in the census.
#' }
#'
#' @docType data
#' @name lifem
#' @usage data(lifem)
#' @format A data frame with 156,453 rows and 6 variables
#'
#' @references Bailey, Martha J., Lin, Peter Z., Mohammed, A.R. Shariq, Mohnen, Paul, Murray, Jared,
#' Zhang, Mengying, and Prettyman, Alexa. LIFE-M: The Longitudinal, Intergenerational
#' Family Electronic Micro-Database. Ann Arbor, MI: Inter-university Consortium for
#' Political and Social Research (distributor), 2022-12-21. \url{https://doi.org/10.3886/E155186V5}\cr
#' \dontshow{\dontrun{
#' library(tidyverse)
#' library(readxl)
#' # read data (V1)
#' master <- readRDS("lifem_master.rds")
#' death <- readRDS("lifem_death.rds")
#'
#' # link master and death data sets within life-m database
#' # using unique "lifemid" identifier
#' df <- left_join(master, death, by = "lifemid")
#'
#' # focus on life-m individuals w/ yob in [1883, 1906]
#' # without missing year of death (only focus on complete cases)
#' df <- df %>% filter((yob >= 1883 & yob <= 1906) & !is.na(dob) & !is.na(yod))
#'
#' # add age at death variable
#' df$surv_age <- df$yod  - df$yob
#'
#' # remove individuals with age > 95 (such individuals should be
#' # top-coded per latest life-m version)
#' df <- df %>% filter(surv_age <= 95)
#'
#' # "hand-linked" variable (used as the latent match indicator in this application)
#' df$hndlnk[(df$lnkd %in% c(0,2)) & (df$lnkm %in% c(0,2)) &
#'             (df$lnkc40 %in% c(0,2)) & (df$lnkc20 %in% c(0,2)) &
#'                         (df$lnkc10 %in% c(0,2)) & (df$lnkc00 %in% c(0,2)) &
#'                                     (df$lnkc80 %in% c(0,2))] <- 0
#' df$hndlnk <- as.factor(df$hndlnk)
#' levels(df$hndlnk) <- c("Purely Machine-Linked", "Hand-Linked At Some Level")
#' # state-link variable - subset individuals linked to Ohio
#' df$stlnk <- as.factor(df$stlnk) # LIFE-M state
#' levels(df$stlnk) <- c("North Carolina", "Ohio")
#' df <- df %>% filter(stlnk == "Ohio") # 156,924 life-m individuals
#'
#' # commonness score of first and last name (commf and comml, respectively)
#' # focus on individuals with both scores available
#' lifem <- df %>% select(lifemid, yob, yod, surv_age, hndlnk, commf, comml)
#' lifem <- lifem %>% filter(comml <= 1)
#' lifem$uyob <- (lifem$yob - min(lifem$yob)) / (max(lifem$yob) - min(lifem$yob))
#'
#' ### Demo Data (n = 3,238) ###
#' ### 2:1 (HL-ML Ratio)
#' hl <- lifem %>% filter(hndlnk == "Hand-Linked At Some Level") #2159 life-m individuals
#' ml <- lifem %>% filter(hndlnk == "Purely Machine-Linked") #154294 life-m individuals
#'
#' n = nrow(hl)*0.5
#'
#' set.seed(123)
#' ml <- sample_n(ml, n)
#' lifem <- rbind(hl, ml)}}
"lifem"
