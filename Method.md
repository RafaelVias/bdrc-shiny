---
output: 
    html_document
---



## Software <img src="www/logo.png" align="right" alt="" width="140" />

This Shiny application can be used to fit a discharge rating curve from paired observations of water elevation (stage) and discharge. The app uses the R-package `bdrc` (*Bayesian Discharge Rating Curves*) to fit rating curves based on the power law, and the novel generalized power law, using a Bayesian hierarchical model as described in Hrafnkelsson et al. (2022). The R package is freely available on the Comprehensive R Archive Network (CRAN). Four models are implemented in the package:

```plm0() ``` - Power-law model, in which the variance of the error terms is assumed to be constant over all water elevation values. This is a Bayesian hierarchical implementation of the most commonly used discharge rating curve model in hydrological practice. 

```plm() ``` - Power-law model, in which the variance of the error terms is modeled as a function of water elevation

```gplm0() ``` - Generalized power-law model, in which the variance of the error terms is assumed to be constant over all water elevation values. The generalized power law is introduced in Hrafnkelsson et al. (2022).

```gplm() ``` - Generalized power-law model, in which the variance of the error terms is modeled as a function of water elevation. The generalized power law is introduced in Hrafnkelsson et al. (2022).


## About

Most methods for directly measuring the discharge of a water stream are time-consuming and expensive. Therefore, indirect methods for measuring the discharge are usually applied. A popular method for inferring discharge is to fit a model that describes the relationship between discharge and water elevation. Such a model is called a discharge rating curve, and one widely used in hydrology, is the power-law rating curve, given by $Q(h)=a(h-c)^b$, where $Q(h)$ is the discharge, $h$ is the water elevation, and $a$, $b$ and $c$ are constants. However, the power-law rating curve is not always able to adequately describe the relationship between discharge and water elevation. Hrafnkelsson et al. (2022) proposed a novel extension to the power-law rating curve referred to as the generalized power-law rating curve. Its form is $Q(h)=a(h-c)^{f(h)}$, where the power-law exponent, $f(h)$, is a function of water elevation. This generalization allows the water stream's cross-sectional geometry to affect the power-law exponent, and thus lends more flexibility to the rating curve, enabling it to adequately describe the relationship between discharge and water elevation.

## Bayesian Generalized Power-Law Rating Curves

The power-law model is commonly assumed in hydraulic practice to fit rating curves, and as mention above, its form is

$$\hspace{5cm}Q(h) = a(h-c)^{b},\hspace{4.7cm}(1)$$ 

where $Q$, $h$, $a$, $b$ and $c$ are as before, see e.g., Venetis (1970), Clarke (1999) and Clarke et al. (2000).
An extension of the power-law rating curve in (1) was proposed by Hrafnkelsson et al. (2022), referred to as the generalized power-law rating curve. Its construction is based on the hydraulics of open channel flow given in the formulas of Chézy and Manning which are of the form

$$\hspace{5cm}Q = KR^{x}AS^{1/2},\hspace{5.1cm}(2)$$ 

where $K$ and $x$ are constants; $A$ is the area of the cross section; $S$ is the slope of the channel; and $R$ is the hydraulic radius, given by $R = A/P$, where $P$ is the wetted perimeter. According to Chézy $x = 1/2$, while Manning claimed that $x = 2/3$ (Chow, 1959). The form of the generalized power-law rating curve is

$$\hspace{5cm}Q = a(h − c)^{f(h)},\hspace{4.8cm}(3)$$ 

where $a$ and $c$ are constants and $f(h)$ is a function of $h$ and referred to as the generalized power-law exponent. By equating (2) and (3), the form of $f(h)$ can be derived, namely,

$$\hspace{1cm}f(h) = \frac{(x+1)\{ \log A(h) - \log A(1) \}  - x \{ \log P(h) - \log P(1)  \}}{\log h}.\hspace{1.2cm}(4)$$ 

Thus, $f(h)$ is a function of the constant $x$, the cross section $A$ and the wetted perimeter $P$.
A Bayesian approach is proposed for estimating the parameters of the power-law model and the generalized power-law model. Bayesian inference requires specification of prior densities for unknown parameters and unknown quantities, along with a full probabilistic specification of the observed data. The goal of Bayesian inference is to obtain the posterior density of the model parameters, and then interrogate the posterior by calculating summary statistics, such as the posterior mean and the 95% posterior intervals. Analytical formulas for these summary statistics are intractable in most cases and thus they are computed by generating samples from the posterior density using Markov chain Monte Carlo simulation. The Bayesian power-law model is presented on a logarithmic scale as,

$$\hspace{1cm} \log(Q_i) = \log(a) + b \log(h_i -c) + \epsilon_i, \quad i = 1,...,n,\hspace{2.7cm}(5)$$ 


where $\epsilon_i$ follows a normal distribution with mean zero and variance $\sigma_{\epsilon}^2$, $n$ is the number of paired observations and $a$, $b$ and $c$ are as before. The Bayesian inference scheme implemented for the power-law model is standard, however, for efficient posterior simulation, first, samples are obtained from the joint marginal posterior density of $(c, \sigma_{\epsilon}^2)$, then samples are obtained from the conditional posterior density of $(\log(a), b)$ conditional on $(c, \sigma_{\epsilon}^2)$.
The Bayesian generalized power-law model is presented as a Bayesian hierarchical model. The function $f(h)$ is modeled at the latent level as $b$ plus a mean zero continuous stochastic process $\beta(h)$, which is assumed to be twice mean-square differentiable. The model is presented on a logarithmic scale as,

$$\hspace{1cm} \log(Q_{i}) = \log(a) + (b+\beta(h_{i}))\log(h_{i} -c) + \epsilon_{i}, \quad i = 1,...,n,\hspace{1.25cm}(6)$$

where $\epsilon_i$ follows a normal distribution with mean zero and variance $\sigma_{\epsilon}^2(h_i)$ that can vary with water elevation. Here the parameters $a$, $b$ and $c$ play a similar role as in the Bayesian power-law model. The stochastic process $\beta(h)$ is assumed a priori to be a Gaussian process governed by a Matérn covariance function with smoothness parameter $\nu = 2.5$, see Matérn (1960). This model is constrained by setting $b=1.835$. An efficient posterior simulation is achieved by sampling from the joint posterior density of the hyperparameters of the model, and then sampling from the conditional density of the latent parameters given the hyperparameters.


<br />
<br />

## References

Chow, V. (1959). *Open-Channel Hydraulics*. McGraw-Hill. New York.

Clarke, R. (1999). *Uncertainty in the estimation of mean annual flood due to rating-curve indefinition.* Journal of Hydrology 222(1-4). 185–190.

Clarke, R., Mendiondo, E., Brusa L. (2000). *Uncertainties in mean discharges from two large South American rivers due to rating curve variability.* Hydrological Sciences 45(2). 221–236.

Hrafnkelsson, B., Sigurdarson, H., and Gardarsson, S. M. (2022). *Generalization of the power-law rating curve using hydrodynamic theory and Bayesian hierarchical modeling*, Environmetrics, 33(2):e2711.

Matérn, B. (1960). *Spatial variation. Stochastic models and their application to some problems in forest surveys and other sampling investigations.* Meddelanden från statens Skogsforskningsinstitut. 49(5).

Venetis, C. (1970). *A note on the estimation of the parameters in logarithmic stage-discharge relationships with estimates of their error.* International Association of Scientific Hydrology. Bulletin XV 2(6). 105–111. 



