// generated with brms 2.16.3
functions {
 /* Return the log probability of a proper conditional autoregressive (CAR) 
  * prior with a sparse representation for the adjacency matrix
  * Full credit to Max Joseph (https://github.com/mbjoseph/CARstan)
  * Args:
  *   phi: Vector containing the CAR parameters for each location
  *   car: Dependence (usually spatial) parameter for the CAR prior
  *   sdcar: Standard deviation parameter for the CAR prior
  *   Nloc: Number of locations
  *   Nedges: Number of edges (adjacency pairs)
  *   Nneigh: Number of neighbors for each location
  *   eigenW: Eigenvalues of D^(-1/2) * W * D^(-1/2)
  *   edges1, edges2: Sparse representation of adjacency matrix
  * Details:
  *   D = Diag(Nneigh)
  * Returns:
  *   Log probability density of CAR prior up to additive constant
  */
  real sparse_car_lpdf(vector phi, real car, real sdcar, int Nloc,
                       int Nedges, data vector Nneigh, data vector eigenW,
                       int[] edges1, int[] edges2) {
    real tau;  // precision parameter
    row_vector[Nloc] phit_D;  // phi' * D
    row_vector[Nloc] phit_W;  // phi' * W
    vector[Nloc] ldet;
    tau = inv_square(sdcar);
    phit_D = (phi .* Nneigh)';
    phit_W = rep_row_vector(0, Nloc);
    for (i in 1:Nedges) {
      phit_W[edges1[i]] = phit_W[edges1[i]] + phi[edges2[i]];
      phit_W[edges2[i]] = phit_W[edges2[i]] + phi[edges1[i]];
    }
    for (i in 1:Nloc) {
      ldet[i] = log1m(car * eigenW[i]);
    }
    return 0.5 * (Nloc * log(tau) + sum(ldet) - 
           tau * (phit_D * phi - car * (phit_W * phi)));
  }
 /* Return the log probability of an intrinsic conditional autoregressive 
  * (ICAR) prior with a sparse representation for the adjacency matrix
  * Full credit to Max Joseph (https://github.com/mbjoseph/CARstan)
  * Args:
  *   phi: Vector containing the CAR parameters for each location
  *   sdcar: Standard deviation parameter for the CAR prior
  *   Nloc: Number of locations
  *   Nedges: Number of edges (adjacency pairs)
  *   Nneigh: Number of neighbors for each location
  *   eigenW: Eigenvalues of D^(-1/2) * W * D^(-1/2)
  *   edges1, edges2: Sparse representation of adjacency matrix
  * Details:
  *   D = Diag(Nneigh)
  * Returns:
  *   Log probability density of CAR prior up to additive constant
  */
  real sparse_icar_lpdf(vector phi, real sdcar, int Nloc, 
                        int Nedges, data vector Nneigh, data vector eigenW, 
                        int[] edges1, int[] edges2) {
    real tau;  // precision parameter
    row_vector[Nloc] phit_D;  // phi' * D
    row_vector[Nloc] phit_W;  // phi' * W
    tau = inv_square(sdcar);
    phit_D = (phi .* Nneigh)';
    phit_W = rep_row_vector(0, Nloc);
    for (i in 1:Nedges) {
      phit_W[edges1[i]] = phit_W[edges1[i]] + phi[edges2[i]];
      phit_W[edges2[i]] = phit_W[edges2[i]] + phi[edges1[i]];
    }
    return 0.5 * ((Nloc - 1) * log(tau) - 
           tau * (phit_D * phi - (phit_W * phi)));
  }
}
data {
  int<lower=1> N;  // total number of observations
  int Y[N];  // response variable
  int trials[N];  // number of trials
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  // data for the CAR structure
  int<lower=1> Nloc;
  int<lower=1> Jloc[N];
  int<lower=0> Nedges;
  int<lower=1> edges1[Nedges];
  int<lower=1> edges2[Nedges];
  // scaling factor of the spatial CAR component
  real<lower=0> car_scale;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  int Kc = K - 1;
  matrix[N, Kc] Xc;  // centered version of X without an intercept
  vector[Kc] means_X;  // column means of X before centering
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
}
parameters {
  vector[Kc] b;  // population-level effects
  real Intercept;  // temporary intercept for centered predictors
  real<lower=0> sdcar;  // SD of the CAR structure
  // parameters for the BYM2 structure
  vector[Nloc] zcar;  // spatial part
  vector[Nloc] nszcar;  // non-spatial part
  // proportion of variance in the spatial part
  real<lower=0,upper=1> rhocar;
}
transformed parameters {
  // scaled parameters for the BYM2 structure
  vector[Nloc] rcar;
  // join the spatial and the non-spatial CAR component
  rcar = (sqrt(1 - rhocar) * nszcar + sqrt(rhocar * inv(car_scale)) * zcar) * sdcar;
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = Intercept + Xc * b;
    for (n in 1:N) {
      // add more terms to the linear predictor
      mu[n] += rcar[Jloc[n]];
    }
    target += binomial_logit_lpmf(Y | trials, mu);
  }
  // priors including constants
  target += normal_lpdf(b[1] | 0, 1);
  target += normal_lpdf(b[2] | 0, 1);
  target += normal_lpdf(b[3] | 0, 1);
  target += normal_lpdf(b[5] | 0, 1);
  target += normal_lpdf(b[6] | 0, 1);
  target += normal_lpdf(b[7] | 0, 1);
  target += student_t_lpdf(Intercept | 3, 0, 2.5);
  target += student_t_lpdf(sdcar | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  target += normal_lpdf(rhocar | 0, 1)
    - 1 * log_diff_exp(normal_lcdf(1 | 0, 1), normal_lcdf(0 | 0, 1));
  // improper prior on the spatial BYM2 component
  target += -0.5 * dot_self(zcar[edges1] - zcar[edges2]);
  // soft sum-to-zero constraint
  target += normal_lpdf(sum(zcar) | 0, 0.001 * Nloc);
  // proper prior on the non-spatial BYM2 component
  target += std_normal_lpdf(nszcar);
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
}

