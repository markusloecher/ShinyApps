# Tail Amplification: Why Small Mean Shifts Cause Huge Changes in Extremes

An interactive Shiny app demonstrating how tiny changes in the mean of a bell-shaped distribution can dramatically increase the frequency of "extreme" events. 

**Motivation**: Climate change has raised global mean temperatures by ~1.5°C since 1950, but headlines often report that "extreme hot days have doubled or tripled". This app shows why that nonlinear amplification is mathematically expected, and how sensitive it is to the choice of threshold.

**Live demo**: [https://datateachr.shinyapps.io/TailAmplification/]

---

## Key Insight

For a bell-shaped distribution like daily temperatures, the probability of exceeding a threshold $T$ depends exponentially on how far $T$ is in the tail. A small shift in the mean $\mu$ has a much larger relative effect on rare events than on typical events.

**Example: Berlin Summer**  
DWD 1991-2020 climatology for July-August daily max temperature: $\mu = 24.5°C$, $\sigma = 3.2°C$  
With 1.5°C warming:

| Threshold | DWD Definition | Baseline days/summer | After +1.5°C | Amplification |
| --- | --- | --- | --- | --- |
| 25°C | Sommertag | 42.8 | 57.2 | **1.3x** |
| 30°C | Heißer Tag | 4.6 | 9.2 | **2.0x** |
| 35°C | Extrem heiß | 0.26 | 1.25 | **4.8x** |
| 38°C | Rekordbereich | 0.02 | 0.24 | **12x** |

Same warming, 1.3x to 12x increase depending on threshold. This is not "more volatile weather" — it's pure tail math.

---

## The Mathematics

For a random variable $X$ with mean $\mu$ and standard deviation $\sigma$, the probability of exceeding threshold $T$ is:

$$P(X > T | \mu, \sigma) = 1 - F\left(\frac{T - \mu}{\sigma}\right)$$

Where $F$ is the CDF. For a normal distribution, $F = \Phi$, the standard normal CDF.

### Amplification Ratio

If the mean shifts from $\mu$ to $\mu + \Delta$, the ratio of exceedance probabilities is:

$$R(T, \Delta) = \frac{1 - \Phi\left(\frac{T - \mu - \Delta}{\sigma}\right)}{1 - \Phi\left(\frac{T - \mu}{\sigma}\right)}$$

### Why it Explodes in the Tails

For large $z$, the normal tail has the approximation $1 - \Phi(z) \approx \frac{\phi(z)}{z}$, where $\phi$ is the PDF. This gives:

$$R(T, \Delta) \approx \exp\left(\frac{\Delta(T - \mu - \Delta/2)}{\sigma^2}\right) \quad \text{for } T \gg \mu$$

**Key takeaways:**
1. Amplification is exponential in the shift $\Delta$
2. Amplification is exponential in how far out $T$ is, via the term $(T-\mu)/\sigma^2$
3. No change in variance $\sigma$ is needed — mean shift alone creates this effect

---

## Using the App

### Install & Run
```r
# install.packages(c("shiny", "ggplot2", "dplyr", "tidyr", "shinythemes", "latex2exp"))
shiny::runApp()