# nocov start
.onAttach <- function(...) {
  BeeGUTS_lib <- dirname(system.file(package = "BeeGUTS"))
  pkgdesc <- suppressWarnings(utils::packageDescription("BeeGUTS", lib.loc = BeeGUTS_lib))

  if (length(pkgdesc) > 1) {
    builddate <- gsub(';.*$', '', pkgdesc$Packaged)
    packageStartupMessage(paste("BeeGUTS (Version ", pkgdesc$Version, ", packaged on the: ", builddate, ")", sep = ""))
  }
  packageStartupMessage("- For execution on a local, multicore CPU with excess RAM we recommend calling")
  packageStartupMessage("      options(mc.cores = parallel::detectCores()-1)")
  packageStartupMessage("- In addition to the functions provided by 'BeeGUTS', we recommend using the packages:")
  packageStartupMessage("   - 'bayesplot' for posterior analysis, model checking, and MCMC diagnostics.")
  packageStartupMessage("   - 'loo' for leave-one-out cross-validation (LOO) using Pareto smoothed")
  packageStartupMessage("       importance sampling (PSIS), comparison of predictive errors between models, and")
  packageStartupMessage("       widely applicable information criterion (WAIC).")
}

# nocov end
