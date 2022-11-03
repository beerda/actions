test_that("mark_failed_deps", {
    a1 <- action(targets = c('a', 'b'))
    a2 <- action(targets = c('c', 'd'), depends = c('a'))
    a3 <- action(targets = c('e', 'f'), depends = c('b'))
    a4 <- action(targets = c('g', 'h'), depends = c('c', 'y', 'z'))
    job <- list(a1, a2, a3, a4)

    job <- init_build(job)
    job2 <- mark_failed_deps(job)

    expect_equal(props(job2, 'status'),
                 list('Not executed',
                      'Not executed',
                      'Not executed',
                      'Not executed'))
    expect_equal(props(job2, 'failed'),
                 rep_len(list(FALSE), length(job2)))
    expect_equal(props(job2, 'finished'),
                 rep_len(list(FALSE), length(job2)))

    props(job, 'failed') <- c(F, F, F, T)
    props(job, 'finished') <- c(F, F, F, T)
    job2 <- mark_failed_deps(job)

    expect_equal(props(job2, 'status'),
                 list('Not executed',
                      'Not executed',
                      'Not executed',
                      'Not executed'))
    expect_equal(props(job2, 'failed'),
                 list(F, F, F, T))
    expect_equal(props(job2, 'finished'),
                 list(F, F, F, T))

    props(job, 'failed') <- c(T, F, F, F)
    props(job, 'finished') <- c(T, F, F, F)
    job2 <- mark_failed_deps(job)

    expect_equal(props(job2, 'status'),
                 list('Not executed',
                      'Failed prerequisite action',
                      'Failed prerequisite action',
                      'Failed prerequisite action'))
    expect_equal(props(job2, 'failed'),
                 list(T, T, T, T))
    expect_equal(props(job2, 'finished'),
                 list(T, T, T, T))

    props(job, 'failed') <- c(T, T, F, F)
    props(job, 'finished') <- c(T, T, F, F)
    job2 <- mark_failed_deps(job)

    expect_equal(props(job2, 'status'),
                 list('Not executed',
                      'Not executed',
                      'Failed prerequisite action',
                      'Failed prerequisite action'))
    expect_equal(props(job2, 'failed'),
                 list(T, T, T, T))
    expect_equal(props(job2, 'finished'),
                 list(T, T, T, T))
})
