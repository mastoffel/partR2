# partR2 0.9.2
* fix docs for CRAN

# partR2 0.9.1

* data argument is now optional.
* part R^2 calculation has been modified. We now estimate the difference in explained variance in the linear predictor and divide this by the estimated total variance from the full model. The results should be very similar, but more stable.
* added option "none" to possible expct arguments to allow calculating R2 for GLMMs without distribution-specific variation in the denominator.
