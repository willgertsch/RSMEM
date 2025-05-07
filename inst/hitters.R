# test on hitters data
library(ggplot2)
library(dplyr)
library(flexmix)
# load data
# omit missing data
dat = na.omit(ISLR2::Hitters)[c(-173, -241),]

# plot
dat %>%
  ggplot(aes(x = RBI, y = Salary)) +
  geom_point(alpha=0.5) +
  geom_smooth(method = 'lm') +
  theme_bw()

# fit model using EM
set.seed(727)
em_mod = flexmix(
  Salary ~ RBI,
  data = dat,
  k = 2,
  concomitant = FLXPmultinom(~AtBat + Hits + HmRun + Runs + Walks + Years +
                               CAtBat + CHits + CHmRun + CRuns + CRBI + CWalks +
                               League + Division + PutOuts + Assists + Errors +
                               NewLeague)
)
summary(em_mod)
beta1 = parameters(em_mod, component = 1)[1:2]
beta2 = parameters(em_mod, component = 2)[1:2]

# plot model predictions
data.frame(
  clusters = as.factor(clusters(em_mod)),
  Salary = dat$Salary,
  RBI = dat$RBI
) %>%
  mutate(
    yhat = ifelse(clusters == 1,
              beta1[1] + beta1[2]*RBI,
              beta2[1] + beta2[2]*RBI)
  ) %>%
  ggplot(aes(x = RBI, y = Salary, color = clusters, group = clusters)) +
  geom_point() +
  #geom_smooth(method = 'lm', se = F) +
  theme_bw() +
  geom_line(aes(y=yhat),linewidth=1) +
  labs(
    color = 'subgroup'
  )

# fit model using metaheuristics
y = dat$Salary
z = model.matrix(Salary ~ RBI, dat)
x = model.matrix(Salary~ ., dat)[,c(-1,-6)]

bounds = list(
  beta1_lb = 0,
  beta1_ub = max(Hitters$Salary),
  beta2_lb = 0,
  beta2_ub = max(Hitters$Salary),
  gamma_lb = -5,
  gamma_ub = 5,
  sigma_lb = 1,
  sigma_ub = sd(Hitters$Salary)
)

res_hs = rsmem(y,z,x, iter = 5000, swarm = 64, 'HS', bounds)
res_de = rsmem(y,z,x, iter = 5000, swarm = 64, 'DE', bounds)
res_pso = rsmem(y,z,x, iter = 5000, swarm = 64, 'PSO', bounds)

# compare log likelihood with EM model
logLik(em_mod)
-res_hs$ll
-res_de$ll
-res_pso$ll

# was able to attain a larger log-likelihood

plot_class(res_hs, y, z, x, 'slr') +
ggplot2::labs(
  y = "Salary",
  x = 'RBI'
)

# fit regularized model for grid of lambda
# just fit one at first
res_lam = rsmem(y,z,x, iter = 5000, swarm = 64, 'HS', bounds, lam=0.5, seed = 1234)
res_lam$ll

grid = 10^seq(10, -10, length = 30)
grid_res = grid_rsmem(y,z,x, iter = 5000, swarm = 64, 'HS', bounds,
                      lam_grid = grid, seed = 204)

grid_res

# BIC
grid_res %>%
  ggplot(aes(x = log(lambda), y = BIC)) +
  geom_point() + geom_line() +
  theme_bw()

best = grid_res[which.min(grid_res$BIC),]
beta1_best = as.numeric(best[6:7])
beta2_best = as.numeric(best[8:9])
gamma_best = as.numeric(best[10:27])
sigma_best = as.numeric(best[28:29])

sum(abs(res_hs$gamma))
sum(abs(gamma_best))

# plot
plot_class(list(beta1=beta1_best, beta2 = beta2_best, gamma = gamma_best, sigma=sigma_best), y, z, x, 'slr') +
  ggplot2::labs(
    y = "Salary",
    x = 'RBI'
  )

best_class = predict_class(list(beta1=beta1_best, beta2 = beta2_best, gamma = gamma_best, sigma=sigma_best), x)
hs_class = ifelse(predict_class(res_hs, x)=='A', 'B', 'A') # classes are swaped

table(best_class, hs_class)

# l1 norm
grid_res %>%
  filter(l1<1) %>%
  ggplot(aes(x = log(lambda), y = l1)) +
  geom_point() + geom_line()

