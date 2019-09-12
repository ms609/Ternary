release_questions <- function() {
  c(
    "Are new features described in vignettes?",
    "Have you updated .zenodo.json?"
  )
}


# Additional tests:
# 
# check_win_devel(); check_rhub()
# revdepcheck::revdep_check()
# build_vignettes()
# tools::resaveRdaFiles('data', compress='auto') - is default bzip2 the optimal?
# tools::checkRdaFiles('data') - set optimal compression in `data-raw`
