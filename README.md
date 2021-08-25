# DomDF

A personal R package containing functions for Bayesian data analysis and plotting. These include:
 - **gen_cov_exp_quad( )** A function for generating an n-dimensional exponenitated quadratic covariance matrix (in various formats)
 - **tidy_mcmc_draws( )** A function for extracting draws from a CmdStanR object (fitted Stan model) in long, tidy format
 - **VoPI( )** A function for a value of perfect information analysis, for a generic decision making under uncertainty problem
 - ...as well as some webscraping and plotting functions.

This can be installed using the following code:

```
require(devtools)
devtools::install_github(repo = 'DomDF/DomDF')
```
