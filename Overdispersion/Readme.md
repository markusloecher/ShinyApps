# Overdispersion: Beta-Binomial & Negative Binomial

## The Core Problem

Binomial and Poisson assume all trials have identical probability `p` or rate `λ`. Real data has heterogeneity: 

- Students passing a test: `p` varies by prep, sleep, luck → Beta-Binomial
- Website hits per hour: `λ` varies by time-of-day, marketing → Negative Binomial  
- Insurance claims: risk varies per customer → overdispersed counts

**Overdispersion is the rule, not the exception.** Binomial/Poisson are limit cases when heterogeneity → 0.

## Analytic vs Simulation

Earlier versions used simulation, but that adds noise. For teaching, we want exact PMFs:

**Beta-Binomial PMF:**
$$P(X=k) = {n \choose k} \frac{B(k+\alpha, n-k+\beta)}{B(\alpha, \beta)}$$

Mean = `np` where `p = α/(α+β)`  
Variance = `np(1-p)[1 + (n-1)/(α+β+1)]`  

The factor `1 + (n-1)/(α+β+1)` is the overdispersion. As `α+β → ∞`, Beta-Binomial → Binomial.

**Negative Binomial PMF:**
$$P(X=k) = {k+r-1 \choose k} p^r (1-p)^k$$

Mean = `μ = r(1-p)/p`  
Variance = `μ + μ²/r > μ`  

Always overdispersed vs Poisson unless `r → ∞`.

## The Practical Problem: Nobody Knows the Prior

Beta-Binomial should be the default conceptually, but in practice you hit 3 issues:

### 1. The prior is arbitrary when you have no data

If you've never run the ad before, what are α and β?

| Choice | Implies | Problem |
| --- | --- | --- |
| `Beta(1,1)` = Uniform | p could be anything 0-1 | Way too diffuse. Says CTR could be 80% |
| `Beta(0.5,0.5)` = Jeffreys | Believes in extremes | Even worse for ads |
| `Beta(1,99)` | Mean p=0.01, but why 99? | You just made up the variance |
| `Beta(0.1,9.9)` | Same mean, 10x more variance | Also made up |

That choice completely drives your overdispersion factor. With α+β=10 you get huge overdispersion. With α+β=1000 you're back to Binomial. Two analysts can get wildly different CIs for the same campaign just by picking different priors.

### 2. Estimating from empirical data is hard

To estimate α, β you need replicates. Not `n` impressions in one campaign — that tells you nothing about how p varies. You need k different campaigns/users/segments, each with their own true pᵢ.

**Method of moments:**

Mean: $\text{mean}(p) = \frac{\alpha}{\alpha + \beta} =$ observed mean click rate  
Variance: $\text{var}(p) = \frac{\alpha\beta}{(\alpha + \beta)^2(\alpha + \beta + 1)} =$ excess variance beyond Binomial

But to estimate `var(p)` reliably you need lots of independent groups. If you have 1 ad, you have 1 p. Can't estimate a variance from n=1.

**ML estimation:** Same problem. You need `y_ij ~ Binom(n_ij, p_i)` where `p_i ~ Beta(α,β)`. If you only have j=1 group, α and β aren't identified. The likelihood is flat.

### 3. What people actually do

1. **Hierarchical Bayes across campaigns**: Pool data from 100s of past ads. Shrink each new ad toward the population. `p_i ~ Beta(α,β)`, estimate α,β from the corpus. Works but assumes past ads are exchangeable with new ads.
2. **Quasi-likelihood**: Don't specify α,β. Just fit `E[Y]=np`, `Var[Y]=φnp(1-p)` and estimate overdispersion φ from residuals. Get valid SEs without pretending you know the Beta shape.
3. **Give up and use Binomial anyway**: If n is huge, `np` is still unbiased. Your CI will be too narrow, but if n=10M impressions, who cares if the SE is off by 2x.

## The Shiny App: Analytic Comparison

This app plots exact PMFs with no simulation. Toggle between overlaid bars and side-by-side.

