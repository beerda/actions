test_that("prerequisites", {
    a1 <- action(targets = c('a', 'b'))
    a2 <- action(targets = c('c', 'd'))
    a3 <- action(targets = c('e', 'f'), depends = c('a'))
    a4 <- action(targets = c('g', 'h'), depends = c('a', 'd', 'e'))
    j <- job(a1, a2, a3, a4)

    res <- prerequisites(j)
    expect_true(is.matrix(res))
    expect_true(is.numeric(res))

    expect_true(res[1, 1] == 0)
    expect_true(res[1, 2] == 0)
    expect_true(res[1, 3] > 0)
    expect_true(res[1, 4] > 0)
    expect_true(res[2, 3] == 0)
    expect_true(res[2, 4] > 0)
    expect_true(res[3, 4] > 0)

    expect_equal(res, matrix(c(rep(0, 8), 1, 0, 0, 0, 1, 1, 1, 0), nrow = 4))
})
