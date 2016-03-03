context("Multiple records")

test_that("add_flags halts with wrong input", {
    expect_error(add_flags())
    expect_error(add_flags(data.frame()))
    expect_error(add_flags("a"))
    expect_error(add_flags(1))
    expect_error(add_flags(c("a", "b", "c")))
})

test_that("add_flags shows warning messages with incomplete inputs", {
    df <- data.frame("", "", "")
    names(df) <- c("decimalLatitude", "decimalLongitude", "countryCode")
    expect_warning(add_flags(df), "'scientificName' element missing")
    names(df) <- c("decimalLatitude", "decimalLongitude", "scientificName")
    expect_warning(add_flags(df), "'countryCode' element missing")
    names(df) <- c("decimalLatitude", "scientificName", "countryCode")
    expect_warning(add_flags(df), "'decimalLongitude' element missing")
    names(df) <- c("scientificName", "decimalLongitude", "countryCode")
    expect_warning(add_flags(df), "'decimalLatitude' element missing")
})

test_that("add_flags works properly with a 1-row data.frame", {
    df <- data.frame(42.3881, 3.833, "ES", "Puma concolor")
    names(df) <- c("decimalLatitude", "decimalLongitude", "countryCode", "scientificName")
    resp <- add_flags(df)
    expect_true(resp$flags$hasScientificName)
    expect_true(resp$flags$validCountry)
    expect_true(resp$flags$negatedLongitude)
    expect_true(resp$flags$validCoordinates)
    expect_true(resp$flags$hasCountry)
    expect_true(resp$flags$hasCoordinates)
    expect_true(resp$flags$highPrecisionCoordinates)
    expect_true(resp$flags$nonZeroCoordinates)
    expect_false(resp$flags$negatedLatitude)
    expect_false(resp$flags$coordinatesInsideCountry)
    expect_false(resp$flags$transposedCoordinates)
    expect_false(resp$flags$coordinatesInsideRangeMap)
    expect_equal(resp$flags$distanceToRangeMapInKm, 7261.647)
})

test_that("add_flags plays well with rgbif", {
    d <- rgbif::occ_data(scientificName="Apis mellifera", limit=50, minimal=FALSE)
    d <- d$data
    dd <- add_flags(d)
    expect_equal(nrow(dd), 50)
    expect_true("flags" %in% names(dd))
})

test_that("flags works properly", {
    lat <- -42.1833
    lng <- -1.8332
    ccd <- "ES"
    scn <- "Puma concolor"
    resp <- flags(decimalLatitude = lat, decimalLongitude = lng, countryCode = ccd, scientificName = scn)

    # Check that some critical flags are right
    expect_true(resp$negatedLatitude)
    expect_false(resp$coordinatesInsideCountry)
    expect_equal(resp$distanceToRangeMapInKm, 5114.433)

})

