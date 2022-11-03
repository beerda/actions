test_that("mark_missing_deps", {
    a1 <- action(targets = c('a', 'b'))
    a2 <- action(targets = c('c', 'd'), depends = c('x'))
    a3 <- action(targets = c('e', 'f'), depends = c('a'))
    a4 <- action(targets = c('g', 'h'), depends = c('a', 'y', 'z'))
    job <- list(a1, a2, a3, a4)

    expect_equal(props(job, 'status'),
                 rep(list(NULL), length(job)))

    job <- init_build(job)
    job <- mark_missing_deps(job)

    expect_equal(props(job, 'status'),
                 list('Not executed',
                      'No action to build: x',
                      'Not executed',
                      'No action to build: y, z'))

    expect_equal(props(job, 'failed'),
                 list(F, T, F, T))

    expect_equal(props(job, 'finished'),
                 list(F, T, F, T))
})
