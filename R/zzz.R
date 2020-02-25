release_questions <- function() {
  c(
    "Are new features described in vignettes?",
    "Have you updated the version number in .zenodo.json, NEWS & DESCRIPTION?"
  )
}


# Additional tests:
#
# spell_check()
# codemetar::write_codemeta()
# pkgdown::build_reference_index()
# check_win_devel(); rhub::check_for_cran()
# revdepcheck::revdep_check()
# build_vignettes()
# tools::resaveRdaFiles('data', compress='auto') - is default of bzip2 the optimal?
# tools::checkRdaFiles('data') - set optimal compression in `data-raw`
