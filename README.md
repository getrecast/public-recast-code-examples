# public-recast-code-examples

A public facing repository that houses code examples we use at Recast to help illustrate complex statistical concepts

Here at Recast, we believe in making our work as transparent as possible. To that effect, we created this repository so that you could take the code that you see us running in our articles and practitioners guide and play around with it yourself! As we add new guides, we'll drop little descriptions in here so you can find what you're looking for:

### `multicollinearity_practitioners_guide`

These files are meant to illustrate some of the challenges that marketing analysts and practitioners face when encountering multicollinearity in their data. Here is a link to the article in which walkthroughs of this code can be found.

-   `multicollinearity_beta_to_0.R` - this file illustrates the response of a typical frequentist model to collinear channels and demonstrates the "beta to 0" problem that we encounter when using typical regressions.

-   `multicollinearity_intervention_fix.R` - this file demonstrates how flighting marketing spend, even over short period of time, can have incredibly powerful impacts on reducing multicollinearity in your models, even when using traditional regressions.

-   `multicollinearity_freq_v_bayes.R` - this file provides an overview of the statistical benefits associated with utilizing bayesian methods when collinearity is present in channels and demonstrates the idea that uncertainty in estimates is often a feature rather than a bug.
