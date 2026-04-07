# public-recast-code-examples

A public facing repository that houses code examples we use at Recast to help illustrate complex statistical concepts

Here at Recast, we believe in making our work as transparent as possible. To that effect, we created this repository so that you could take the code that you see us running in our articles and practitioners guides and play around with it yourself! As we add new guides, we'll drop little descriptions in here so you can find what you're looking for:

### `counterfactuals_practitioners_guide`

This files illustrate two of the standard approaches to generating counterfactuals, namely forecasting and propensity score matching. This code is meant to serve as a support to this article, which details common counterfactual generation approaches as well as their tradeoffs.

-   `counterfactual_forecasting.R` - this file provides a script for using a forecast to generate a counterfactual for a marketing intervention. We leverage basic ARIMA modeling approaches for time series to show this process from scratch. You may also consider using the `CausalImpact` package to complete similar work.

-   `counterfactual_psm.R` - this file provides a script for generating propensity scores to identify matched markets to serve as counterfactuals for a marketing intervention. We leverage a k-nearest neighbors approach to consider multiple factors and consolidate the performance of the top 3 matches to serve as the counterfactual.

### `multicollinearity_practitioners_guide`

These files are meant to illustrate some of the challenges that marketing analysts and practitioners face when encountering multicollinearity in their data. Here is a [link to the article](https://getrecast.com/understand-and-manage-multicollinearity/) in which walkthroughs of this code can be found.

-   `multicollinearity_beta_to_0.R` - this file illustrates the response of a typical frequentist model to collinear channels and demonstrates the "beta to 0" problem that we encounter when using typical regressions.

-   `multicollinearity_intervention_fix.R` - this file demonstrates how flighting marketing spend, even over short periods of time, can have incredibly powerful impacts on reducing multicollinearity in your models, even when using traditional frequentist regressions.

-   `multicollinearity_freq_v_bayes.R` - this file provides an overview of the statistical benefits associated with utilizing Bayesian methods when collinearity is present in channels and demonstrates the idea that uncertainty in estimates is often a feature rather than a bug.
