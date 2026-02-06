# Energy-I-Score: Implementation Details

## Introduction

This vignette presents the implementation details of the
*energy-I-Score*, a metric designed to evaluate the quality of
imputation methods in incomplete datasets.

The score is based on the concept of energy distance between observed
and imputed distributions. It allows comparing the uncertainty induced
by the imputation model with the variability present in the observed
data. The procedure is model-agnostic: it can be used with any
imputation method $\mathcal{I}$ and with multiple imputation draws.

The score is distribution-free and can be applied to:

- continuous variables,
- mixed-type data (here the score is calculated on dummy variables),
- multiple imputation methods.

------------------------------------------------------------------------

### Notation

Let $X \in {\mathbb{R}}^{n \times p}$ be an original dataset with
missing values, $\widetilde{X} \in {\mathbb{R}}^{n \times p}$ be an
imputed dataset, $\mathcal{I}$ imputation function, and $N$ the number
of imputations drawn from $\mathcal{I}$.

Then, for each variable with missing values $j \in \{ 1,\ldots,p\}$ we
define $L_{j}$ as a set of indices $i$ for which $X_{i,j}$ is observed,
$L_{j}^{c}$ being a set of indices $i$ for which $X_{i,j}$ is missing
and $O_{j}$ a set of fully observed predictor variables for rows with
$X_{i,j}$ observed.

Finally, we define the set of variables with missing values as
$\mathcal{S} = \{ j:L_{j}^{c} \neq \varnothing\}$.

------------------------------------------------------------------------

### Algorithm Overview

The *energy-I-Score* is computed iteratively for each variable with
missing data. The following steps are performed for each
$j \in \mathcal{S}$.

#### Step 1: Selection of Predictor Set

We determine the set of predictor variables:
$$O_{j} = \bigcap\limits_{m \in L_{j}}\{ l:m_{l} = 0\}.$$

If $O_{j}$ is empty, the algorithm automatically selects a fallback
variable $k^{*}$ defined as:
$$k^{*} = \text{argmax}_{k \neq j}|\{ i:m_{i, \cdot} \in L_{j} \cap L_{k}\}|.$$
which is a variable with the largest number of observed values for the
observed part of column $j$. This ensures that the imputation model has
at least one predictor.

#### Step 2: Data Partitioning

The data are split into training and test sets as follows:

$$\text{Train} = \begin{bmatrix}
{NA} & {\widetilde{X}}_{L_{j},O_{j}} \\
{\widetilde{X}}_{L_{j}^{c},j} & {\widetilde{X}}_{L_{j}^{c},O_{j}}
\end{bmatrix},\quad\text{Test} = \begin{bmatrix}
{\widetilde{X}}_{L_{j},j}
\end{bmatrix}.$$

- The **training set** contains the observed predictor values and
  missing target values to be imputed.
- The **test set** contains the observed target values to evaluate
  imputation quality.

#### Step 3: Multiple Imputations

The missing part of the training set is imputed $N$ times using
$\mathcal{I}$:
$${\widetilde{X}}_{i,j}^{(1)},\ldots,{\widetilde{X}}_{i,j}^{(N)} \sim H_{X_{j}|X_{O_{j}},M_{j} = 1}.$$

Each imputation represents a draw from the conditional distribution of
the missing variable given the observed predictors.

#### Step 4: Energy Distance Calculation

For each $i \in L_{j}$, the *energy-I-Score* component is computed as:
$${\widehat{S}}_{NA}^{j}(H,P) = \frac{1}{\left| L_{j} \right|}\sum\limits_{i \in L_{j}}\left\lbrack \frac{1}{2N^{2}}\sum\limits_{l = 1}^{N}\sum\limits_{\ell = 1}^{N}\left| {\widetilde{X}}_{i,j}^{(l)} - {\widetilde{X}}_{i,j}^{(\ell)} \right| - \frac{1}{N}\sum\limits_{l = 1}^{N}\left| {\widetilde{X}}_{i,j}^{(l)} - x_{i,j} \right| \right\rbrack.$$

The first term is internal dispersion of the imputed values and the
second term is distance between the imputed and the actual observations.
The larger the score, the greater the uncertainty of the imputation
relative to the true data.

#### Step 5: Weighting

Each variable’s contribution to the final score is weighted by:
$$w_{j} = \frac{1}{n^{2}}\left| L_{j} \right| \cdot \left| L_{j}^{c} \right|.$$

This accounts for the relative amount of missing and observed data per
variable.

#### Step 6: Final Score

The final *energy-I-Score* is a weighted average over all variables with
missing values:
$${\widehat{S}}_{NA}(H,P) = \frac{1}{|\mathcal{S}|}\sum\limits_{j \in \mathcal{S}}w_{j}{\widehat{S}}_{NA}^{j}(H,P).$$

This scalar measure summarizes the imputation uncertainty across the
dataset.

------------------------------------------------------------------------

### Practical Interpretation

- High values of the score suggest large variability or poor alignment
  between imputed and observed distributions.

- Low values indicate imputations that are close to the observed data
  distribution (better performance).

- Variables with few missing values have lower weight, while those with
  many missing values contribute more.

- Methods that do not rely on multiple imputation or have a weak/random
  draw mechanism tend to perform worse, because they underestimate the
  uncertainty of the missing values.

- The *energy-I-Score* should primarily be used to rank different
  imputation methods, rather than to interpret its absolute numeric
  value directly.

## References

This approach follows the methodology proposed by Näf, Grzesiak, and
Scornet (2025) in “How to rank imputation methods?” (arXiv:2507.11297).
