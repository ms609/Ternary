release_questions <- function() {
  c(
    "Are any new features described in the vignettes?"
  )
}


# Additional tests:
#
# codemeta::write_codemeta()
# check_win_devel(); rhub::check_for_cran()
# build_vignettes()
#
# tools::resaveRdaFiles('data', compress = 'auto') - is default of bzip2 the optimal?
# tools::checkRdaFiles('data') - set optimal compression in `data-raw`
