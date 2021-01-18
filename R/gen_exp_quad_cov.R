#' gen_exp_quad_cov
#'
#' Generate an n-dimensional covariance matrix, based on the exponential-quadratic covariance function.
#' Returns either a matrix (tidy = FALSE), a wide tibble (tidy = TRUE, longer = FALSE) or a long tibble (tidy = TRUE, longer = TRUE).
#'
#'
#' @param n_dims The number of dimensions of the covariance matrix - REQUIRED.
#' @param alpha_sq The maximum co-variance - defaults to 1.
#' @param rho_sq The length scale - defaults to 3.
#' @param sigma_sq The (within-dimensional) variance - defaults to 1.

#' @param tidy Logical: If 'TRUE', returns a tibble, if 'FALSE'  returns a matrix - defaults to FALSE.
#' @param longer Logical: If 'TRUE', returns a long-format tibble, if 'FALSE'  returns a wide-format tibble (requires tidy = TRUE) - defaults to FALSE.
#' @keywords
#' @export
#' @examples
#' ten_dim_cov_mat <- cov_mat <- gen_exp_quad_cov(n_dims = 10, tidy = FALSE)
#' ten_dim_cov_tibble_long <- gen_exp_quad_cov(n_dims = 10, tidy = TRUE, longer = TRUE)

gen_exp_quad_cov <- function(n_dims, alpha_sq = 1, rho_sq = 3, sigma_sq = 1,
                             tidy = FALSE,
                             longer = FALSE) {

  n_dims <- as.integer(n_dims); if(n_dims < 1) stop('Please provide a positive integer number of dimensions: n_dim')
  if(sigma_sq < 0) stop('Please provide a non-negtive variance: sigma_sq')
  if(rho_sq < 0) stop('Please provide a non-negtive length scale: rho_sq')


  cov <- matrix(nrow = n_dims, ncol = n_dims)

  for(i in seq(from = 1, to = n_dims, by = 1)){

    for(j in seq(from = 1, to = n_dims, by = 1)){

      cov[i, j] = alpha_sq * exp(-1/(2 * rho_sq) * (i-j)^2)

    }
  }

  if (tidy == FALSE) {

    cov <- cov

  } else if (tidy == TRUE){

    cov <- cov %>%
      as_tibble() %>%
      rowid_to_column()

    colnames(cov) <- c('id_x', seq(1:n_dims))

    cov <- cov %>%
      mutate(id_x = as.factor(x = id_x)) %>%
      arrange(id_x)

    if (longer == FALSE) {

      cov <- cov

    } else if (longer == TRUE) {

      cov <- cov %>%
        pivot_longer(cols = -c(id_x),
                     names_to = 'id_y') %>%
        mutate(id_y = as.factor(x = as.numeric(x = id_y))) %>%
        arrange(id_x, id_y)

    }

  }

  return(cov)

}

