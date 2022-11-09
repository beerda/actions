test_that("props", {
    a1 <- action('a', 'b')
    a2 <- action('b', 'c')
    j <- job(a1, a2)

    expect_equal(props(j, 'targets'),
                 list('a', 'b'))

    props(j, 'targets') <- list(c('a', 'x'), 'y')

    expect_equal(props(j, 'targets'),
                 list(c('a', 'x'), 'y'))
})
