test_that("failures_of_missing_actions", {
    a1 <- action(targets = c('a', 'b'))
    a2 <- action(targets = c('c', 'd'), depends = c('x'))
    a3 <- action(targets = c('e', 'f'), depends = c('a'))
    a4 <- action(targets = c('g', 'h'), depends = c('a', 'y', 'z'))
    job <- list(a1, a2, a3, a4)

    expect_equal(failures_of_missing_actions(job),
                 list(NULL,
                      'Missing action for: x',
                      NULL,
                      'Missing action for: y, z'))

    expect_equal(failures_of_missing_actions(list()), list())
})
