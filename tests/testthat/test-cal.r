
test_that("apply_slope_offset creates proper names", {
    expect_match(names(apply_slope_offset(), "sloped_OutflowCO2"))
    expect_match(names(apply_slope_offset(), "sloped_OutflowO2"))
    expect_match(names(apply_slope_offset(), "sloped_InflowO2"))
    expect_match(names(apply_slope_offset(), "sloped_InflowCO2"))
})
