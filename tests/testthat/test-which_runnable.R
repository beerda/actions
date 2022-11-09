test_that("which_runnable", {
    f1 <- tempfile()
    f2 <- tempfile()
    f3 <- tempfile()
    f4 <- tempfile()

    a1 <- action(f1)
    a2 <- action(f2, f1)
    a3 <- action(f3, f2)
    a4 <- action(f4, f3)

    j <- job(a1, a2, a3, a4)

    expect_equal(which_runnable(j), c(T, F, F, F))
    expect_equal(which_runnable(j, c(T, F, F, F)), c(F, F, F, F))

    file.create(f1)
    Sys.setFileTime(f1, "1990-01-01")
    expect_equal(which_runnable(j), c(F, T, F, F))
    expect_equal(which_runnable(j, c(T, F, F, F)), c(F, F, F, F))
    expect_equal(which_runnable(j, c(F, T, F, F)), c(F, F, F, F))
    expect_equal(which_runnable(j, c(T, T, F, F)), c(F, F, F, F))

    file.create(f2)
    Sys.setFileTime(f2, "1990-01-02")
    expect_equal(which_runnable(j), c(F, F, T, F))
    expect_equal(which_runnable(j, c(T, F, F, F)), c(F, F, F, F))
    expect_equal(which_runnable(j, c(F, T, F, F)), c(F, F, F, F))
    expect_equal(which_runnable(j, c(F, F, T, F)), c(F, F, F, F))
    expect_equal(which_runnable(j, c(T, T, T, F)), c(F, F, F, F))

    file.create(f4)
    Sys.setFileTime(f4, "1990-01-04")
    expect_equal(which_runnable(j), c(F, F, T, F))
    expect_equal(which_runnable(j, c(T, F, F, F)), c(F, F, F, F))
    expect_equal(which_runnable(j, c(F, T, F, F)), c(F, F, F, F))
    expect_equal(which_runnable(j, c(F, F, T, F)), c(F, F, F, F))

    file.create(f3)
    Sys.setFileTime(f3, "1990-01-03")
    expect_equal(which_runnable(j), c(F, F, F, F))

    Sys.setFileTime(f3, "1990-01-05")
    expect_equal(which_runnable(j), c(F, F, F, T))
    expect_equal(which_runnable(j, c(T, F, F, F)), c(F, F, F, F))
    expect_equal(which_runnable(j, c(F, T, F, F)), c(F, F, F, F))
    expect_equal(which_runnable(j, c(F, F, T, F)), c(F, F, F, F))
    expect_equal(which_runnable(j, c(F, F, F, T)), c(F, F, F, F))
})


test_that("which_runnable 2", {
    f1 <- tempfile()
    f2 <- tempfile()
    f3 <- tempfile()
    f4 <- tempfile()

    a1 <- action(f1)
    a2 <- action(f2)
    a3 <- action(f3, f1)
    a4 <- action(f4, f2)

    j <- job(a1, a2, a3, a4)

    expect_equal(which_runnable(j), c(T, T, F, F))
    expect_equal(which_runnable(j, c(T, F, F, F)), c(F, T, F, F))
    expect_equal(which_runnable(j, c(T, T, F, F)), c(F, F, F, F))

    file.create(f1)
    Sys.setFileTime(f1, "1990-01-01")
    expect_equal(which_runnable(j), c(F, T, T, F))
    expect_equal(which_runnable(j, c(T, T, F, F)), c(F, F, F, F))
    expect_equal(which_runnable(j, c(F, T, F, F)), c(F, F, T, F))
    expect_equal(which_runnable(j, c(F, F, T, F)), c(F, T, F, F))
    expect_equal(which_runnable(j, c(F, T, T, F)), c(F, F, F, F))

    file.create(f2)
    Sys.setFileTime(f2, "1990-01-02")
    expect_equal(which_runnable(j), c(F, F, T, T))

    file.create(f4)
    Sys.setFileTime(f4, "1990-01-04")
    expect_equal(which_runnable(j), c(F, F, T, F))

    file.create(f3)
    Sys.setFileTime(f3, "1990-01-03")
    expect_equal(which_runnable(j), c(F, F, F, F))
})
