test_that("mark_failed_deps", {
    a1 <- action(targets = c('a', 'b'))
    a2 <- action(targets = c('c', 'd'), depends = c('a'))
    a3 <- action(targets = c('e', 'f'), depends = c('b'))
    a4 <- action(targets = c('g', 'h'), depends = c('c', 'y', 'z'))
    j <- job(a1, a2, a3, a4)

    j <- init_build(j)
    j2 <- mark_failed_deps(j)

    expect_equal(props(j2, 'status'),
                 list('Not executed',
                      'Not executed',
                      'Not executed',
                      'Not executed'))
    expect_equal(props(j2, 'failed'),
                 rep_len(list(FALSE), length(j2)))
    expect_equal(props(j2, 'finished'),
                 rep_len(list(FALSE), length(j2)))

    props(j, 'failed') <- c(F, F, F, T)
    props(j, 'finished') <- c(F, F, F, T)
    j2 <- mark_failed_deps(j)

    expect_equal(props(j2, 'status'),
                 list('Not executed',
                      'Not executed',
                      'Not executed',
                      'Not executed'))
    expect_equal(props(j2, 'failed'),
                 list(F, F, F, T))
    expect_equal(props(j2, 'finished'),
                 list(F, F, F, T))

    props(j, 'failed') <- c(T, F, F, F)
    props(j, 'finished') <- c(T, F, F, F)
    j2 <- mark_failed_deps(j)

    expect_equal(props(j2, 'status'),
                 list('Not executed',
                      'Failed prerequisite action',
                      'Failed prerequisite action',
                      'Failed prerequisite action'))
    expect_equal(props(j2, 'failed'),
                 list(T, T, T, T))
    expect_equal(props(j2, 'finished'),
                 list(T, T, T, T))

    props(j, 'failed') <- c(T, T, F, F)
    props(j, 'finished') <- c(T, T, F, F)
    j2 <- mark_failed_deps(j)

    expect_equal(props(j2, 'status'),
                 list('Not executed',
                      'Not executed',
                      'Failed prerequisite action',
                      'Failed prerequisite action'))
    expect_equal(props(j2, 'failed'),
                 list(T, T, T, T))
    expect_equal(props(j2, 'finished'),
                 list(T, T, T, T))
})
