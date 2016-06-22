context("Data frame formating")

test_that("format_gq only works with data.frames", {
    err_msg <- "Please provide a data.frame object as input"
    expect_error(format_gq(), "argument \"indf\" is missing, with no default")
    expect_error(format_gq("a"), err_msg)
    expect_error(format_gq(1), err_msg)
    expect_error(format_gq(c("a", "b", "c")), err_msg)
    expect_error(format_gq(list()), err_msg)
})

test_that("format_gq needs populated data.frames", {
    expect_error(format_gq(data.frame()), "Input data frame missing or empty")
})

test_that("Formating via 'source' works", {
    if (requireNamespace("rgbif", quietly = TRUE)) {
        dg <- rgbif::occ_data(scientificName="Apis mellifera", limit=10, minimal=FALSE)
        expect_message(format_gq(dg$data, source="rgbif"), "Mapping according to rgbif format")
    }
    if (requireNamespace("rvertnet", quietly = TRUE)) {
        dv <- rvertnet::searchbyterm(class = "Aves", limit=10)
        expect_message(format_gq(dv$data, source="rvertnet"), "Mapping according to rvertnet format")
    }
    if (requireNamespace("rinat", quietly = TRUE)) {
        tryCatch({
            di <- rinat::get_inat_obs(query="Monarch Butterfly", maxresults = 20)
        }, error = function(e){
            warning("Could not get data from rinat.")
        })
        if (exists("di")) {
            expect_message(format_gq(di, source="rinat"), "Mapping according to rinat format")
        }
    }
})

test_that("Formating via 'config' works", {
    if (requireNamespace("rgbif", quietly = TRUE)) {
        d <- rgbif::occ_data(scientificName="Apis mellifera", limit=10, minimal=FALSE)
        conf <- list(decimalLatitude="decimalLatitude",
                     decimalLongitude="decimalLongitude",
                     countryCode="countryCode",
                     scientificName="name")
        expect_message(format_gq(d$data, config=conf), "Mapping according to config object")
    }
})

test_that("Formating via 'config' needs all parameters", {
    if (requireNamespace("rgbif", quietly = TRUE)) {
        d <- rgbif::occ_data(scientificName="Apis mellifera", limit=10, minimal=FALSE)
        conf <- list(decimalLatitude="decimalLatitude",
                     countryCode="countryCode",
                     scientificName="name")
        expect_error(format_gq(d$data, config=conf), "\"decimalLongitude\" missing from configuration object")
    }
})

test_that("Formating via named parameters works", {
    if (requireNamespace("rgbif", quietly = TRUE)) {
        d <- rgbif::occ_data(scientificName="Apis mellifera", limit=10, minimal=FALSE)
        expect_message(format_gq(d$data,
                                 decimalLatitude="decimalLatitude",
                                 decimalLongitude="decimalLongitude",
                                 countryCode="countryCode",
                                 scientificName="name"
                                 ),
                       "Mapping via individual parameters")
    }
})
