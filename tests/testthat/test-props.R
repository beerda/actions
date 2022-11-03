test_that("props", {
    a1 <- action('a', 'b')
    a2 <- action('b', 'c')
    job <- list(a1, a2)

    expect_equal(props(job, 'targets'),
                 list('a', 'b'))

    props(job, 'targets') <- list(c('a', 'x'), 'y')

    expect_equal(props(job, 'targets'),
                 list(c('a', 'x'), 'y'))
})
