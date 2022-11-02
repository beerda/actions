test_that("failures_of_failed_prerequisites", {
    a1 <- action(targets = c('a', 'b'))
    a2 <- action(targets = c('c', 'd'), depends = c('a'))
    a3 <- action(targets = c('e', 'f'), depends = c('b'))
    a4 <- action(targets = c('g', 'h'), depends = c('c', 'y', 'z'))
    job <- list(a1, a2, a3, a4)

    expect_equal(failures_of_failed_prerequisites(job, c(F, F, F, F)),
                 list(NULL, NULL, NULL, NULL))

    expect_equal(failures_of_failed_prerequisites(job, c(F, F, F, T)),
                 list(NULL, NULL, NULL, NULL))

    expect_equal(failures_of_failed_prerequisites(job, c(T, F, F, F)),
                 list(NULL,
                      'Failed prerequisite action',
                      'Failed prerequisite action',
                      'Failed prerequisite action'))

    expect_equal(failures_of_failed_prerequisites(job, c(T, T, F, F)),
                 list(NULL,
                      NULL,
                      'Failed prerequisite action',
                      'Failed prerequisite action'))
})
