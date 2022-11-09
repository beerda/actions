test_that("which_invalid", {
    f1 <- tempfile()
    f2 <- tempfile()
    f3 <- tempfile()
    f4 <- tempfile()

    a1 <- action(f1)
    a2 <- action(f2, f1)
    a3 <- action(f3, f2)
    a4 <- action(f4, f3)

    j <- job(a1, a2, a3, a4)

    expect_equal(which_invalid(j), c(T, T, T, T))

    file.create(f1)
    Sys.setFileTime(f1, "1990-01-01")
    expect_equal(which_invalid(j), c(F, T, T, T))

    file.create(f2)
    Sys.setFileTime(f2, "1990-01-02")
    expect_equal(which_invalid(j), c(F, F, T, T))

    file.create(f4)
    Sys.setFileTime(f4, "1990-01-04")
    expect_equal(which_invalid(j), c(F, F, T, T))

    file.create(f3)
    Sys.setFileTime(f3, "1990-01-03")
    expect_equal(which_invalid(j), c(F, F, F, F))
})
