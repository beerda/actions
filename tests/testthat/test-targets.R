test_that("targets", {
    a1 <- action(targets = c('a', 'b'))
    a2 <- action(targets = c('c', 'd'))
    a3 <- action(targets = c('e', 'f'), depends = c('a'))
    a4 <- action(targets = c('g', 'h'), depends = c('a', 'd', 'e'))
    job <- list(a1, a2, a3, a4)

    expect_equal(targets(list()), character(0))
    expect_equal(targets(job), letters[1:8])
})
