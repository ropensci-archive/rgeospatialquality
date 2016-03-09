context("Single record")

test_that("flags works with a list object, called 'record'", {
    rec <- list(
        "decimalLatitude"=-42.1833,
        "decimalLongitude"=-1.8332,
        "countryCode"="ES",
        "scientificName"="Puma concolor"
    )
    resp <- parse_record(rec)

    expect_true(resp$negatedLatitude)
    expect_false(resp$coordinatesInsideCountry)
    expect_equal(resp$distanceToRangeMapInKm, 5114.433)
})

test_that("flags works properly", {
    lat <- -42.1833
    lng <- -1.8332
    ccd <- "ES"
    scn <- "Puma concolor"
    resp <- parse_record(decimalLatitude = lat, decimalLongitude = lng, countryCode = ccd, scientificName = scn)

    # Check that some critical flags are right
    expect_true(resp$negatedLatitude)
    expect_false(resp$coordinatesInsideCountry)
    expect_equal(resp$distanceToRangeMapInKm, 5114.433)

})

