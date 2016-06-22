context("Package synergies")

test_that("add_flags plays well with rgbif", {
    if (requireNamespace("rgbif", quietly = TRUE)) {
        d <- rgbif::occ_data(scientificName="Apis mellifera", limit=50, minimal=FALSE)
        d <- format_gq(d$data, source="rgbif")
        dd <- add_flags(d)
        expect_equal(nrow(dd), 50)
        expect_true("flags" %in% names(dd))
    }
})

test_that("add_flags plays well with rvertnet", {
    if (requireNamespace("rvertnet", quietly = TRUE)) {
        d <- rvertnet::searchbyterm(class="Aves", limit=50)
        d <- format_gq(d$data, source="rvertnet")
        dd <- add_flags(d)
        expect_equal(nrow(dd), 50)
        expect_true("flags" %in% names(dd))
    }
})

test_that("add_flags plays well with rinat", {
    if (requireNamespace("rinat", quietly = TRUE)) {
        tryCatch({
            d <- rinat::get_inat_obs(query="Monarch Butterfly", maxresults = 20)
        }, error = function(e){
            warning("Could not get data from rinat.")
        })
        if (exists("d")) {
            d <- format_gq(d, source="rinat")
            dd <- add_flags(d, quiet=TRUE)
            expect_equal(nrow(dd), 20)
            expect_true("flags" %in% names(dd))
        }
    }
})
