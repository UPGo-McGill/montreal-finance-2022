// generated with brms 2.16.3
functions {
}
data {
  int<lower=1> N;  // total number of observations
  int Y[N];  // response variable
  int trials[N];  // number of trials
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
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
}
transformed parameters {
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = Intercept + Xc * b;
    target += binomial_logit_lpmf(Y | trials, mu);
  }
  // priors including constants
  target += normal_lpdf(b[1] | 0, 2);
  target += normal_lpdf(b[2] | 0, 2);
  target += normal_lpdf(b[3] | 0, 2);
  target += normal_lpdf(b[5] | 0, 2);
  target += normal_lpdf(b[6] | 0, 2);
  target += normal_lpdf(b[7] | 0, 2);
  target += student_t_lpdf(Intercept | 3, 0, 2.5);
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
}

