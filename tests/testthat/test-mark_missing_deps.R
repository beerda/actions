test_that("mark_missing_deps", {
    a1 <- action(targets = c('a', 'b'))
    a2 <- action(targets = c('c', 'd'), depends = c('x'))
    a3 <- action(targets = c('e', 'f'), depends = c('a'))
    a4 <- action(targets = c('g', 'h'), depends = c('a', 'y', 'z'))
    j <- job(a1, a2, a3, a4)

    expect_equal(props(j, 'status'),
                 rep(list(NULL), length(j)))

    j <- init_build(j)
    j <- mark_missing_deps(j)

    expect_equal(props(j, 'status'),
                 list('Not executed',
                      'No action to build: x',
                      'Not executed',
                      'No action to build: y, z'))

    expect_equal(props(j, 'failed'),
                 list(F, T, F, T))

    expect_equal(props(j, 'finished'),
                 list(F, T, F, T))
})
