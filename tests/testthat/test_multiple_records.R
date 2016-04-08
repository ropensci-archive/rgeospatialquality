context("Multiple records")

test_that("add_flags only works with data.frames", {
    err_msg <- "Please provide a data.frame object as input"
    expect_error(add_flags(), "argument \"indf\" is missing, with no default")
    expect_error(add_flags("a"), err_msg)
    expect_error(add_flags(1), err_msg)
    expect_error(add_flags(c("a", "b", "c")), err_msg)
    expect_error(add_flags(list()), err_msg)
})

test_that("add_flags needs populated data.frames", {
    expect_error(add_flags(data.frame()), "Input data frame missing or empty")
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

test_that("add_flags works properly guessing names for rvertnet dataset", {
    if (requireNamespace("rvertnet", quietly = TRUE)) {
        dv <- rvertnet::searchbyterm(class="Aves", limit=20)
        dv <- dv$data
        dv <- add_flags(dv, guess_fields=TRUE, quiet=TRUE)
        # Name is not changed
        expect_false("decimalLatitude" %in% names(dv))
        # Name is kept
        expect_true("decimallatitude" %in% names(dv))
        # 'flags' element is present
        expect_true("flags" %in% names(dv))
    }
})

test_that("add_flags stops when a field is missing and guess_fields is TRUE", {
    test.data.frame <- data.frame(cbind(rep("x", times=10), rep("y", times=10), rep("z", 10)))
    expect_error(add_flags(test.data.frame, guess_fields=TRUE), "Could not find a match for decimalLatitude")
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

test_that("add_flags fails when more than 1000 records are passed", {
    test.data.frame <- data.frame(cbind(rep("x", times=1001), rep("y", times=1001), rep("z", 1001)))
    expect_error(add_flags(test.data.frame, quiet=TRUE), "Too many records.")
})
