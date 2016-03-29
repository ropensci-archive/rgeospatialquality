context("Single record")

test_that("parse_record works with a list object, called 'record'", {
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

test_that("parse_record does not work if both 'record' and other named parameters are present", {
    rec <- list(
        decimalLatitude=42.33,
        decimalLongitude=-1.833,
        countryCode="ES"
    )
    expect_error(parse_record(record=rec, decimalLatitude = 4), "Both \"record\" and other named parameters are provided")
})

test_that("parse_record works properly", {
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

test_that("parse_record shows warning if any field is missing from record", {
    rec <- list(
        decimalLatitude=42.33,
        decimalLongitude=-1.833,
        countryCode="ES"
    )
    expect_warning(parse_record(record=rec), "element missing")
})

test_that("parse_record shows warning if any field is missing from the named parameters", {
    expect_warning(
        parse_record(decimalLatitude = 42.33, decimalLongitude = -1.833),
        "element missing"
    )
})
