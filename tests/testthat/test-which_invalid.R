test_that("which_invalid", {
    f1 <- tempfile()
    f2 <- tempfile()
    f3 <- tempfile()
    f4 <- tempfile()

    a1 <- action(f1)
    a2 <- action(f2, f1)
    a3 <- action(f3, f2)
    a4 <- action(f4, f3)

    job <- list(a1, a2, a3, a4)

    expect_equal(which_invalid(job), c(T, T, T, T))

    file.create(f1)
    expect_equal(which_invalid(job), c(F, T, T, T))

    file.create(f2)
    expect_equal(which_invalid(job), c(F, F, T, T))

    file.create(f4)
    expect_equal(which_invalid(job), c(F, F, T, T))

    file.create(f3)
    expect_equal(which_invalid(job), c(F, F, F, F))
})
