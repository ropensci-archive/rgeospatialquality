context("Data frame formating")

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
        di <- rinat::get_inat_obs(query="Monarch Butterfly", maxresults = 20)
        expect_message(format_gq(di, source="rinat"), "Mapping according to rinat format")
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
