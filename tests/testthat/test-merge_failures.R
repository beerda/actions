test_that("merge_failures", {
    expect_equal(merge_failures(list(NULL, 'a', 'b')),
                 list(NULL, 'a', 'b'))

    expect_equal(merge_failures(list(NULL, 'a', 'b'),
                                list(NULL, 'x', NULL),
                                list(NULL, 'z', NULL)),
                 list(NULL, 'z', 'b'))
})
