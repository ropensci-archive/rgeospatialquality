library()
context("Single record")

test_that("get_single works with a vector object, called 'record'", {

})

test_that("get_single works properly", {
    lat <- -42.1833
    lng <- -1.8332
    ccd <- "ES"
    scn <- "Puma concolor"
    resp <- get_single(decimalLatitude = lat, decimalLongitude = lng, countryCode = ccd, scientificName = scn)

    # Check that input returns in output
    expect_equal(resp$decimalLatitude, "-42.1833")
    expect_equal(resp$decimalLongitude, "-1.8332")
    expect_equal(resp$countryCode, "ES")
    expect_equal(resp$scientificName, "Puma concolor")

    # Check that some critical flags are right
    expect_true(resp$flags$negatedLatitude)
    expect_false(resp$flags$coordinatesInsideCountry)
    expect_equal(resp$flags$distanceToRangeMapInKm, 5114.433)

})

