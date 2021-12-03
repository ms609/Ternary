release_questions <- function() {
  c(
    "Are any new features described in the vignette?"
  )
}


# Additional tests:
#
# spell_check()
# codemeta::write_codemeta()
# pkgdown::build_reference_index()
# check_win_devel(); rhub::check_for_cran()
# revdepcheck::revdep_check()
# build_vignettes()
# tools::resaveRdaFiles('data', compress='auto') - is default of bzip2 the optimal?
# tools::checkRdaFiles('data') - set optimal compression in `data-raw`
