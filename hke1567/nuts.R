iters_mc <- function (object, iters)
    {
        it <- length(iters)
        object@catch.n <- catch.n(object)[, , , , , iters]
        dimnames(object@catch.n)$iter <- 1:it
        object@stock.n <- stock.n(object)[, , , , , iters]
        dimnames(object@stock.n)$iter <- 1:it
        object@harvest <- harvest(object)[, , , , , iters]
        dimnames(object@harvest)$iter <- 1:it
        object@index <- lapply(index(object), function(x) {
            x <- x[, , , , , iters]
            dimnames(x)$iter <- 1:it
            x
        })
        object@pars@stkmodel@coefficients <- coefficients(stkmodel(pars(object)))[, iters]
        dimnames(object@pars@stkmodel@coefficients)$iter <- 1:it
        object@pars@qmodel@.Data <- lapply(qmodel(pars(object)),
            function(x) {
                x@coefficients <- x@coefficients[, iters]
                dimnames(x@coefficients)$iter <- 1:it
                x
            })
        object@pars@vmodel@.Data <- lapply(vmodel(pars(object)),
            function(x) {
                x@coefficients <- x@coefficients[, iters]
                dimnames(x@coefficients)$iter <- 1:it
                x
            })
        object
    }

prelim_fit <- sample_nuts(model="a4a", chains=1, iter=1000, path = "./omc")

# 2. Generate 4 new initial lists based on the preliminary fit
new_inits <- sample_inits(fit = prelim_fit, chains = 4)


fit <- sample_nuts(
  model = "a4a",
  path = "./omc",
  iter = 3000,
  warmup=1000,
  thin=10,
  chains = 4,
  cores = 4,
  init = new_inits,
  duration = 720,
  control=list(adapt_delta=.95)
)


