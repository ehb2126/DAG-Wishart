# A Tractable Directed Hyper-Markov Prior for Poisson Log-Linear DAGs

**Research proposal and execution plan — version 0.1**  
**Prepared:** August 23, 2026  
**Target MVP manuscript:** December 20, 2026  
**Target full submission:** May 9, 2027

## Executive summary

This project asks whether the central advantages of the Gaussian DAG-Wishart construction—local prior modularity, directed hyper-Markov structure, conjugate updating, and scalable graph scoring—can be reproduced for directed graphical models whose vertices are counts and whose conditional distributions are Poisson log-linear regressions.

For a DAG \(\mathcal D=(V,E)\) in a parent ordering, the starting model is

\[
X_i\mid X_{\operatorname{pa}(i)}=u
\sim \operatorname{Poisson}\!\left[
\exp\{\phi_i(u)^\top\beta_i\}\right],
\qquad i\in V,
\]

with \(\phi_i(u)=(1,u^\top)^\top\) in the first paper. The proposed prior must be a law on the random local conditional kernels—not merely a convenient collection of coefficient priors—and should make the vertexwise posterior updates independent and computationally useful.

The main obstacle is structural. For observations \(r=1,\ldots,n\), the local likelihood is

\[
L_i(\beta_i)\propto
\exp\left\{
t_i^\top\beta_i-
\sum_{r=1}^{n}\exp(z_{ir}^\top\beta_i)
\right\},
\quad
t_i=\sum_{r=1}^{n}x_i^{(r)}z_{ir},
\]

where \(z_{ir}=\phi_i(x_{\operatorname{pa}(i)}^{(r)})\). Unlike the Gaussian case, the cumulant term changes when a new parent configuration appears. Gamma conjugacy is exact for one Poisson rate and for saturated finite parent strata, but not for a reduced log-linear regression on unbounded count-valued parents. A generic Diaconis–Ylvisaker construction can be written down, but may be design-dependent and may lack a closed posterior normalizer. Consequently, “tractable” will be treated as a theorem-level requirement, not a descriptive label.

The first six weeks are therefore a formal feasibility phase. Three constructions will be compared:

1. a measure-valued Diaconis–Ylvisaker prior with exact local posterior updating;
2. a multivariate-log-gamma/conditional-MLG construction with tuning-free vertexwise posterior simulation; and
3. an exact restricted construction, either finite parent strata or an augmented additive-intensity Poisson DAG, as a fallback.

The preferred new candidate uses a fixed pseudo-design measure on the entire parent-count space. A product-Poisson pseudo-design gives an analytically normalized prior before data are observed and may supply the missing bridge between graph modularity and Poisson regression. Whether its posterior operations are sufficiently tractable is the central feasibility question.

The minimum viable paper will cover a supplied vertex ordering, one prior family, rigorous propriety and posterior-update results, a strong directed hyper-Markov theorem, one genuinely tractable inferential operation, and exact numerical validation. The full paper will add modular ordered-DAG scoring/search, broader simulations, hyperparameter guidance, robustness analysis, one defensible application, and a reproducible software release.

## 1. Research question and scope

### Primary question

Can one construct a proper, data-independent prior for Poisson log-linear DAGs that:

- is modular over vertices and parent sets;
- is closed under vertexwise posterior updating;
- induces a strong directed hyper-Markov law on the random joint distribution;
- supports exact or controlled-accuracy local inference; and
- remains useful for arbitrary sparse DAGs compatible with a supplied ordering?

### Secondary questions

1. What is the sharp propriety domain of the prior?
2. Can its prior and posterior normalizing constants be evaluated exactly, recursively, or with a uniform error guarantee?
3. Can local marginal likelihoods be updated when a single parent is added or removed without refitting the full graph?
4. Which hyperparameters have an interpretable effective-sample-size or prior-predictive meaning?
5. How does the method behave under overdispersion, zero inflation, rare large counts, and ordering misspecification?

### Deliberate exclusions from the MVP

The first paper will not promise unrestricted causal discovery, recovery of a Markov equivalence class, score equivalence, high-dimensional selection consistency, or a negative-binomial extension. The theory will distinguish:

- **distribution theory:** every fixed DAG admits a parent ordering;
- **model selection:** the search is only over DAGs compatible with a supplied ordering; and
- **causal interpretation:** directionality requires assumptions beyond the existence of a Poisson factorization.

These exclusions prevent the central prior-construction problem from being buried under a second, much larger structure-learning project.

## 2. Statistical model

Let \(V=\{1,\ldots,p\}\), and label the vertices so that \(j\to i\) implies \(j>i\). For each vertex, let

\[
K_i(x_i\mid u;\beta_i)
=\frac{\exp\{x_i\phi_i(u)^\top\beta_i
-\exp(\phi_i(u)^\top\beta_i)\}}{x_i!},
\qquad x_i\in\mathbb N_0.
\]

The recursive product

\[
p_{\beta,\mathcal D}(x)
=\prod_{i=1}^{p}K_i(x_i\mid x_{\operatorname{pa}(i)};\beta_i)
\]

defines a proper joint mass function because summation can be performed in reverse topological order. This elementary fact should be stated and proved as the first proposition; it separates validity of the finite-DAG joint distribution from questions about moments, numerical explosiveness, and prior predictive stability.

The principal basis is

\[
\phi_i(u)=(1,u_1,\ldots,u_{pa_i})^\top,
\]

so that \(d_i=pa_i+1\). Interaction, spline, offset, and exposure terms can be allowed later when they preserve the required local geometry. Parent-set changes alter \(d_i\), so coherent hyperparameter transport across models must be specified rather than improvised.

### Stability and prior-predictive calibration

A finite acyclic recursion is proper for all finite coefficient values, but positive coefficients can generate extremely large descendant counts and unstable likelihood calculations. The project will therefore distinguish:

- existence of the joint mass function;
- existence of specified prior-predictive moments; and
- a computationally stable regime for simulation and application.

The issue is not only numerical. In the positive-effect chain

\[
X_3\sim\operatorname{Poisson}(\lambda),\quad
X_2\mid X_3\sim\operatorname{Poisson}(e^{a_2+b_{23}X_3}),\quad
X_1\mid X_2\sim\operatorname{Poisson}(e^{a_1+b_{12}X_2}),
\]

with \(b_{23},b_{12}>0\), the joint mass function is proper but \(\mathbb E(X_1)=\infty\). An unrestricted prior therefore assigns positive mass to valid joint laws with pathological downstream moments. The main theory will study bounded features such as \(h(u)=\mathbf 1\{u>0\}\) and \(h(u)=\min(u,K)\) alongside the standard linear feature \(h(u)=u\). Bounded features allow positive and negative effects while controlling this pathology; restricting every edge coefficient to be nonpositive would be mathematically safer but often scientifically unacceptable.

Intercepts and effect-size hyperparameters will be calibrated by prior-predictive simulation. No theorem about graph recovery will rely on silently discarding explosive datasets; any rejection rule used in experiments will be fixed and reported.

## 3. Closest prior art and the novelty boundary

The broad idea of conjugate priors for graphical or log-linear models is not new. The proposal must be positioned against five established lines of work.

1. **General exponential-family conjugacy.** Diaconis and Ylvisaker developed conjugate priors for natural exponential families. Any proposed family that is only this construction in DAG notation is insufficiently novel. See [Diaconis and Ylvisaker (1979)](https://projecteuclid.org/journals/annals-of-statistics/volume-7/issue-2/Conjugate-Priors-for-Exponential-Families/10.1214/aos/1176344611.full).
2. **Conjugate priors for GLMs.** Chen and Ibrahim gave conjugate priors for generalized linear models, including Poisson regression, with propriety and moment results. Their construction makes the relationship to the design matrix especially important for the present project. See [Chen and Ibrahim (2003)](https://www3.stat.sinica.edu.tw/statistica/j13n2/j13n212/j13n212.html).
3. **Multivariate log-gamma conjugacy.** Bradley, Holan, and Wikle developed multivariate log-gamma distributions and conditional conjugacy for Poisson models, followed by a broader multivariate conjugate framework for natural exponential families. See [Bradley, Holan, and Wikle (2015)](https://arxiv.org/abs/1512.07273) and [Bradley, Holan, and Wikle (2019)](https://arxiv.org/abs/1701.07506).
4. **Hyper-Markov priors for discrete/log-linear graphs.** The hyper-Dirichlet, generalized hyper-Dirichlet, \(P\)-Dirichlet, and related priors already provide strong graphical factorization for discrete or decomposable log-linear models. Relevant entry points include [Massam, Liu, and Dobra (2009)](https://arxiv.org/abs/0711.1609) and [Massam and Wesołowski (2016)](https://arxiv.org/abs/1412.0972).
5. **Poisson DAG learning.** Poisson DAGs have an active identifiability and structure-learning literature. Overdispersion scoring supplies identifiable directions and polynomial-time procedures; later work extends the idea to quadratic-variance families and to learning with a known ordering. See [Park and Raskutti (2015)](https://proceedings.neurips.cc/paper_files/paper/2015/hash/fccb60fb512d13df5083790d64c4d5dd-Abstract.html), [Park and Raskutti (2017)](https://arxiv.org/abs/1704.08783), and [Nguyen et al. (2024)](https://journals.sagepub.com/doi/abs/10.1177/1471082X241266738).
6. **Bayesian count networks and local GLM scores.** Fully Bayesian zero-inflated Poisson networks and additive Bayesian networks with Poisson local GLMs already exist; the latter include Bayesian local scores and graph search, generally using conventional coefficient priors and numerical approximations. See [Choi, Chapkin, and Ni (2020)](https://papers.nips.cc/paper/2020/hash/4175a4b46a45813fccf4bd34c779d817-Abstract.html) and [Kratzer et al. (2023)](https://doi.org/10.18637/jss.v105.i08).
7. **High-dimensional Poisson structural equations.** The exact log-linear conditional-mean model proposed here already has frequentist graph-recovery theory. See [Park and Park (2019)](https://jmlr.org/papers/v20/18-819.html). Bayesian consistency, if pursued, must be compared with that benchmark rather than presented in isolation.

The defensible novelty claim is therefore narrower and more demanding:

> Construct and characterize a graph-modular prior for count-valued Poisson DAG kernels that resolves the changing, unbounded parent-configuration problem, proves a directed hyper-Markov property for the induced random law, and provides a genuinely tractable local posterior or graph score.

The multivariate-log-gamma distribution itself, ordinary Poisson-regression conjugacy, and independent local coefficient priors cannot be claimed as new. Novelty must lie in the DAG-level compatibility, the fixed pre-data pseudo-design construction, new normalizer/propriety theory, or a local scoring algorithm enabled by them.

To avoid confusion with hierarchical contingency-table log-linear models, the eventual paper should use **Poisson-regression DAGs** or **Poisson DAGs with log-linear conditional means** in its title and abstract.

## 4. Candidate prior constructions

### 4.1 Measure-valued directed Diaconis–Ylvisaker family

Let \(\nu_i\) be a finite positive measure on the parent-count space \(\mathbb N_0^{pa_i}\), selected before observing the responses. Consider

\[
\pi_i(\beta_i\mid s_i,\nu_i)
\propto
\exp\left\{
s_i^\top\beta_i-
\int \exp\{\phi_i(u)^\top\beta_i\}\,\nu_i(du)
\right\}.
\tag{P1}
\]

For data \((x_i^{(r)},u_r)\), the update is exact:

\[
s_i^+=s_i+\sum_{r=1}^{n}x_i^{(r)}\phi_i(u_r),
\qquad
\nu_i^+=\nu_i+\sum_{r=1}^{n}\delta_{u_r}.
\tag{U1}
\]

Thus posterior closure is obtained in an explicitly expanding, measure-indexed family. This is mathematically cleaner than pretending that the observed design is fixed across candidate graphs. The questions are whether the normalizer is finite, whether the hyperparameters can be transported modularly across parent sets, and whether the resulting posterior operations are sufficiently tractable.

For finite support \(u_1,\ldots,u_m\), a sharp candidate propriety condition is that \(s_i\) lies in the interior of the positive cone generated by \(\{\phi_i(u_\ell)\}_{\ell=1}^{m}\), together with a full-span condition. This must be proved, not assumed, and then generalized to infinite-support \(\nu_i\).

### 4.2 Factorized-reference DY prior: the preferred feasibility candidate

Take a product reference law over the parent counts,

\[
Q_i=\bigotimes_{j\in\operatorname{pa}(i)}Q_{ij},
\qquad \nu_i=\tau_iQ_i,
\]

and write the local predictor as

\[
\beta_{i0}+\sum_{j\in\operatorname{pa}(i)}
\beta_{ij}h_{ij}(u_j).
\]

If

\[
M_{ij}(b)=\mathbb E_{Q_{ij}}\!\left[e^{b h_{ij}(U)}\right],
\]

then the reference cumulant separates:

\[
\Psi_{\nu_i}(\beta_i)
=\tau_i e^{\beta_{i0}}
\prod_{j\in\operatorname{pa}(i)}M_{ij}(\beta_{ij}).
\]

Call the resulting family the **factorized-reference Diaconis–Ylvisaker (FR-DY) prior**. Its normalizer reduces from a \((pa_i+1)\)-dimensional integral to one-dimensional constants:

\[
Z_i(a_i,\tau_i,Q_i)
=\Gamma(a_{i0})\tau_i^{-a_{i0}}
\prod_j C_{ij}(a_{i0},a_{ij}),
\]

where

\[
C_{ij}(a_0,a_j)=
\int_{\mathbb R}e^{a_jb}M_{ij}(b)^{-a_0}\,db.
\tag{N-FR}
\]

Under standard steepness and minimality conditions, the candidate sharp propriety condition is that \(a_{ij}/a_{i0}\) lies in the interior of the convex support of \(h_{ij}(U)\) under \(Q_{ij}\). For a bounded scalar feature \(h\in[\ell,r]\), this becomes \(a_0\ell<a_j<a_0r\).

The prior also has an exact factorized simulator. If

\[
\Phi_i=\tau_i e^{\beta_{i0}}
\prod_jM_{ij}(\beta_{ij}),
\]

then \(\Phi_i\sim\operatorname{Gamma}(a_{i0},1)\), independently of the slopes, and each slope has density proportional to \(e^{a_{ij}b}M_{ij}(b)^{-a_{i0}}\). Choosing \(a_{ij}=a_{i0}\mathbb E_Q[h_{ij}(U)]\) centers the slope mode at zero. Moreover, because \(M_{ij}(0)=1\), conditioning on \(\beta_{ij}=0\) when deleting the edge \(j\to i\) leaves the smaller-model prior on \(\Phi_i\) and the remaining slopes. This is a concrete candidate for a compatible-prior theorem across parent sets.

#### Standard linear parent effects

For \(h_{ij}(u)=u\) and \(Q_{ij}=\operatorname{Poisson}(c_{ij})\),

\[
M_{ij}(b)=\exp\{c_{ij}(e^b-1)\}.
\]

The prior becomes

\[
\pi_i(\beta_i)\propto
\exp\left\{
a_{i0}\beta_{i0}+\sum_ja_{ij}\beta_{ij}
-\tau_i e^{\beta_{i0}+\sum_jc_{ij}(e^{\beta_{ij}}-1)}
\right\},
\tag{P2}
\]

with explicit normalizer

\[
Z_i=
\Gamma(a_{i0})\tau_i^{-a_{i0}}
\prod_j
\left\{
e^{a_{i0}c_{ij}}
\frac{\Gamma(a_{ij})}{(a_{i0}c_{ij})^{a_{ij}}}
\right\},
\tag{N2}
\]

for positive \(a_{i0},a_{ij},c_{ij},\tau_i\). The variables \(R_{ij}=e^{\beta_{ij}}\) are independent \(\operatorname{Gamma}(a_{ij},a_{i0}c_{ij})\), and \(\beta_{i0}\) is recovered from the independent \(\Phi_i\). Equation (N2) will be verified symbolically and numerically in weeks 1–2 before it is used as a theorem.

#### Moment-safe threshold and capped effects

For \(h(u)=\mathbf 1\{u>0\}\) and \(Q=\operatorname{Poisson}(c)\), let \(q=1-e^{-c}\). Then

\[
M(b)=(1-q)+qe^b,
\]

and \(C(a_0,a)\) has a beta-function closed form for \(0<a<a_0\). For \(h(u)=\min(u,K)\), \(M(b)\) is an explicit finite polynomial plus a tail cell, propriety requires \(0<a<a_0K\), and each \(C\) remains a stable one-dimensional integral. These subclasses retain positive and negative parent effects while avoiding the infinite-moment behavior of unrestricted positive linear chains. They are the recommended primary statistical models if the application permits their interpretation.

#### Posterior normalizer after observing arbitrary configurations

After updating by (U1), define

\[
b_{i0}=a_{i0}+\sum_rx_i^{(r)},\qquad
b_{ij}=a_{ij}+\sum_rx_i^{(r)}h_{ij}(u_{rj}),
\]

and

\[
H_i(\beta_{i,-0})=
\tau_i\prod_jM_{ij}(\beta_{ij})+
\sum_{r=1}^{n}\exp\left\{\sum_j\beta_{ij}h_{ij}(u_{rj})\right\}.
\]

Integrating the intercept analytically reduces the posterior normalizer to

\[
Z_i^+=\Gamma(b_{i0})
\int_{\mathbb R^{pa_i}}
\exp\left\{\sum_jb_{ij}\beta_{ij}\right\}
H_i(\beta_{i,-0})^{-b_{i0}}d\beta_{i,-0}.
\tag{PN}
\]

The integral is only \(pa_i\)-dimensional and its log-integrand is concave; it is strictly concave under a suitable local span condition. Thus \(pa_i=0\) is Gamma-exact, \(pa_i=1\) admits reliable one-dimensional quadrature, small \(pa_i\) admits adaptive cubature, and bounded larger in-degree can use Laplace/importance/SMC methods in parallel by vertex. A fixed-in-degree relative-error result for Laplace approximation is a theorem target, not a current claim.

The central weakness remains that arbitrary empirical point masses destroy the prior factorization in the posterior normalizer. The feasibility phase must determine whether (PN), an augmentation, or a recurrence reaches Level A or Level B tractability.

### 4.3 A finite-dimensional no-go boundary

For the unrestricted linear feature on count-valued parents, exact closure of a finite-dimensional conjugate family under every possible parent design is unlikely and can be formalized. The functions

\[
\exp\{\beta_0+u^\top\beta\},\qquad u\in\mathbb N_0^q,
\]

are linearly independent as monomials in \(e^{\beta_1},\ldots,e^{\beta_q}\). A finite-dimensional log-density basis therefore cannot absorb arbitrary newly observed parent configurations. The project will turn this into a theorem showing that one must accept at least one of:

1. the measure-valued hyperparameter \(\nu_i+m_i\);
2. finitely many parent regimes through binning, thresholding, or capping;
3. a design-dependent prior, which is problematic for ordinary graph evidence; or
4. numerical posterior normalizers.

A sharp impossibility theorem would be a substantive result: it explains exactly why a literal finite-dimensional DAG-Wishart analogue cannot exist for the unrestricted model and motivates FR-DY rather than weakening the claim silently.

### 4.4 Multivariate-log-gamma/conditional-MLG route

For a local design matrix \(Z_i\), the Poisson likelihood is of conditional multivariate-log-gamma form. An MLG prior can be written schematically as

\[
\pi_i(\beta_i)\propto
\exp\{a_i^\top H_i\beta_i-
\kappa_i^\top\exp(H_i\beta_i)\},
\]

and posterior updating stacks the prior and observed design contributions. This may yield exact, tuning-free vertexwise simulation even when a closed marginal likelihood is unavailable.

This route is a serious comparator and possible primary method, but its claim must be accurate: the paper would introduce a DAG-hyper-Markov deployment and graph-modular specification of established MLG machinery, not the MLG family. Before adopting it, the project must verify:

- exact sampling conditions and computational cost;
- whether the prior is independent of the realized responses and acceptably independent of the realized design;
- compatibility when parent sets change; and
- whether any local score can be evaluated without a generic high-dimensional integral.

If only conditional conjugacy survives, the title should say **conditionally conjugate** rather than imply a DAG-Wishart-like closed marginal likelihood.

### 4.5 Exact restricted fallbacks

Two exact alternatives will be developed far enough to serve as controls and honest pivots.

**Finite parent strata.** If each parent count is bounded or explicitly coarsened, assign independent Gamma priors to configuration-specific Poisson rates. Posterior updates and marginal likelihoods are exact. This is closest to a count-valued conditional probability table and may be high-dimensional in the number of configurations.

**Additive-intensity Poisson DAG.** Replace the log link by

\[
X_i\mid X_{\operatorname{pa}(i)}
\sim \operatorname{Poisson}\left(
\lambda_{i0}+\sum_{j\in\operatorname{pa}(i)}\lambda_{ij}X_j
\right).
\]

Latent source allocations and Gamma priors can yield conditional conjugacy and a branching interpretation. This is not a log-linear model and must be labeled as a different scientific model, not as a technical approximation to (P1).

## 5. What “tractable” will mean

The proposal will use a graded definition.

### Level A: exact DAG-Wishart analogue

- closed prior and posterior normalizers;
- exact local marginal likelihoods;
- exact independent vertexwise simulation; and
- local graph-score updates when one parent set changes.

### Level B: exact posterior computation

- exact posterior closure;
- exact or tuning-free vertexwise simulation with polynomial cost; and
- no global MCMC over all coefficients.

### Level C: controlled deterministic computation

- vertexwise log-concavity;
- deterministic approximation to the normalizer or moments; and
- an explicit, empirically validated error bound or asymptotic expansion.

Level A supports the present title without qualification. Level B supports “a conditionally conjugate directed hyper-Markov prior.” Level C supports “a computable directed hyper-Markov prior.” Generic Laplace approximation or generic HMC alone will not count as a new tractability result.

## 6. Directed hyper-Markov target

Let \(\widetilde K_i\) denote the random conditional kernel induced by \(\beta_i\). The initial prior is

\[
\Pi_{\mathcal D}(d\beta)=\prod_{i\in V}\Pi_i(d\beta_i),
\]

with parent-set-specific hyperparameters chosen by a common modular rule. The resulting random joint law is

\[
\widetilde P(x)=\prod_i\widetilde K_i(x_i\mid x_{\operatorname{pa}(i)})
\]

satisfies the following module-level strong directed hyper-Markov property. For every ancestral set \(A\subseteq V\),

\[
\widetilde P_A\ \perp\!\!\!\perp\ 
\widetilde P_{V\setminus A\mid A},
\]

because \(\widetilde P_A\) is measurable with respect to \(\{\widetilde K_i:i\in A\}\), while \(\widetilde P_{V\setminus A\mid A}\) is measurable with respect to the complementary kernels. The precise sigma-field formulation and its relationship to established definitions of strong directed hyper-Markov laws will be fixed in week 1.

This implication may be nearly automatic once the local kernels are independent. It is therefore a required structural theorem, but cannot by itself support the novelty claim. The substantive graphical work is the construction of a coherent family across changing parent sets, including ancestral projectivity and the FR-DY edge-deletion compatibility result.

The paper will separately establish:

1. independence of the local random kernels;
2. projective behavior under ancestral restriction;
3. posterior preservation of these properties; and
4. modularity of the hyperparameter rule when a parent is added or removed.

Calling the prior “strong hyper-Markov” will be conditional on proving this theorem for the induced random law.

## 7. Aims, theorem program, and success criteria

### Aim 1 — Define and normalize the prior

- T1: The recursive Poisson conditional specification defines a unique proper joint law.
- T2: Give necessary and sufficient, or sharp sufficient, propriety conditions for (P1).
- T3: Prove the FR-DY separation formula (N-FR), verify (N2), derive exact prior simulation, and characterize moments.

**Success:** a nonempty, interpretable hyperparameter domain and a prior specified independently of observed responses.

### Aim 2 — Establish posterior and graphical structure

- T4: Prove posterior closure and update (U1).
- T5: Prove the strong directed hyper-Markov theorem.
- T6: Prove parent-set modularity, FR-DY edge-deletion compatibility, and coherent restriction/extension rules for hyperparameters.

**Success:** all updates and graphical statements can be expressed vertexwise and survive ancestral restriction.

### Aim 3 — Deliver a tractable computation

- T7: Derive an exact posterior normalizer, recurrence, augmentation, or controlled approximation.
- T8: Construct an exact/tuning-free sampler or give a deterministic algorithm with a stated error criterion.
- T9: Show that a single-edge graph move changes only one local score, and give its computational complexity.
- T10: Prove the finite-dimensional no-go boundary for arbitrary unbounded parent designs.
- T11: Prove moment-stability results for bounded features and give the positive-chain counterexample for the unrestricted model.

**Success:** at least Level B tractability; Level A is the preferred result.

### Aim 4 — Validate statistically and computationally

- T12: Establish identifiability assumptions and distinguish ordered selection from unrestricted discovery.
- T13: Compare predictive, estimation, graph-selection, and computational performance with established methods.
- T14, optional: prove graph-selection consistency or posterior contraction after the main paper is complete.

Every theorem will first be checked in the zero-, one-, and two-parent cases. T2–T11 require an independent proof audit before full simulations begin.

## 8. Technical workflow

The work proceeds through the following dependency chain:

1. **Freeze definitions.** Fix the model, the base measure convention, the formal hyper-Markov target, and the meaning of tractability.
2. **Solve local cases.** Derive normalizers, gradients, Hessians, modes, and simulators for zero, one, and two parents.
3. **Prove general local theory.** Establish propriety, posterior closure, and moment conditions for arbitrary parent sets.
4. **Lift to the DAG.** Prove kernel independence, ancestral projectivity, and the directed hyper-Markov statement.
5. **Choose the computational route.** Exact normalizer, cMLG sampler, augmentation, recurrence, or controlled deterministic approximation.
6. **Implement and unit-test.** Compare analytic quantities with quadrature and long-run HMC in small dimensions.
7. **Develop graph scores/search.** Only after the local normalizer or a defensible scoring approximation is stable.
8. **Freeze simulations.** Register the design, seeds, metrics, exclusions, and comparator implementations before running the full grid.
9. **Complete application and manuscript.** Use one count dataset with a scientifically credible ordering; omit rather than force an application.

### Required computation checks

- log-density, gradient, and Hessian finite-difference tests;
- analytic normalizer versus high-precision quadrature for \(d_i\le3\);
- sampler moments versus numerical integration;
- equality of local and full-joint likelihood calculations;
- invariance to row ordering;
- locality of a one-edge graph score update;
- stress tests with rare parent configurations and large counts; and
- reproducible clean-environment execution.

## 9. Simulation and empirical plan

### 9.1 Exact validation stage

Use \(p\in\{3,5,8\}\), permitting exact ordered-DAG enumeration.

- Compare normalizers and posterior moments with quadrature, exhaustive summation where available, and long HMC runs.
- Check posterior updates for repeated and previously unseen parent configurations.
- Include adversarial rare-large-count designs.
- Compare the proposed prior with saturated Gamma strata and MLG/cMLG implementations.

The validation stage passes only when discrepancies are within a prespecified numerical tolerance and all failures are reproducible.

### 9.2 Main simulation grid

Primary factors:

- \(p\in\{20,50,100,250\}\), with \(p=500\) as a stretch case;
- \(n\in\{50,100,250,500\}\);
- expected in-degree approximately \(1,3,5\);
- weak, moderate, and mixed-sign effects;
- baseline conditional means approximately \(1\)–\(5\); and
- at least 100 independent replications for each primary configuration.

Comparators:

- oracle graph-constrained Poisson MLE;
- penalized Poisson DAG regression;
- BIC/EBIC local scores;
- independent Gaussian coefficient priors with Laplace or HMC inference;
- an established MLG/cMLG method; and
- saturated Gamma/configuration priors in small problems.

Metrics:

- coefficient RMSE and local conditional Kullback–Leibler loss;
- held-out log predictive density and count prediction error;
- interval coverage and calibration;
- structural Hamming distance, sensitivity, false-discovery rate, specificity, and MCC;
- wall-clock time, memory, numerical failures, and sampler efficiency; and
- sensitivity to prior effective sample size, graph density, and count scale.

Robustness scenarios:

- negative-binomial overdispersion;
- zero inflation;
- one-percent contamination by large counts;
- mild ordering misspecification; and
- omitted nonlinear parent effects.

All tables will include Monte Carlo standard errors and all replicate-level outputs will be retained.

### 9.3 Application criteria

Use one application only if it has:

- genuine count responses rather than rounded continuous measurements;
- a defensible external ordering, such as time or a known process sequence;
- adequate exposure/offset information;
- permission and stable provenance for public release or a reproducible access route; and
- held-out observations or a defensible cross-validation design.

Candidate domains include temporal event counts, neuronal spike counts in designed sequences, ecological abundance networks with known stages, and transaction/call arrival counts. The application will include posterior predictive checks for overdispersion and zero inflation.

## 10. Timeline

Assumption: work starts August 24, 2026, with 8–12 lead-author hours per week plus regular collaborator input. A solo effort at 5–8 hours per week is expected to add 8–12 weeks.

| Dates | Phase | Work product and exit condition |
|---|---|---|
| **Aug. 24–Sep. 6, 2026** | Scope and charter | Freeze model, formal hyper-Markov definition, tractability levels, graph class, roles, and notation. Deliver a 4–6 page prospectus, prior-art matrix, and theorem checklist. |
| **Sep. 7–Oct. 4** | Six-week feasibility study | Derive (P1)–(N2), finite-support propriety, one-/two-parent cases, cMLG alternative, and exact fallbacks. Benchmark normalizers and simulators against quadrature/HMC. |
| **Oct. 5** | **Decision Gate 1** | Proceed with the general log-link title only if the prior is proper and pre-data, updating is closed, new parent configurations are handled, and at least Level B tractability is credible. Otherwise execute a named pivot. |
| **Oct. 5–Nov. 1** | Core probability theory | Complete T1–T6: joint-law validity, propriety, updates, directed hyper-Markov theorem, modularity, and moment restrictions. Independent proof review begins. |
| **Nov. 2–Nov. 29** | Computational theory | Complete T7–T9: normalizer/recurrence/augmentation or controlled approximation, sampler, local score, complexity, and numerical stability. |
| **Nov. 30–Dec. 20** | **MVP manuscript** | Theorem-audited paper with construction, proofs, algorithm, exact validation, and small simulations. Freeze software v0.1 and reproducibility archive. |
| **Dec. 21–Jan. 3** | Buffer and audit | Proof audit, code review, clean rebuild, and resolution of all critical issues. |
| **Jan. 4–Jan. 31, 2027** | Graph scoring/search | Exact enumeration for small \(p\); greedy or shotgun search for larger ordered DAGs; verify local score updates and state what search does not guarantee. |
| **Feb. 1–Mar. 7** | Full simulations | Run frozen factorial design, robustness tests, sensitivity analysis, and timing benchmarks. Freeze replicate-level results. |
| **Mar. 8–Mar. 28** | Application | Complete one defensible application and held-out evaluation, or document why it is omitted. |
| **Mar. 29–Apr. 18** | Full manuscript | Integrate all results and conduct an internal hostile review of novelty, proofs, computation, and empirical claims. |
| **Apr. 19–May 9** | **Release and submission** | Clean compile, package checks, coauthor approval, archival release, preprint, and journal submission. |
| **May–Aug. 2027, optional** | Theory extension | High-dimensional selection consistency or posterior contraction. Do not delay the primary submission unless required by the target journal. |

## 11. Decision gates and pivots

### Gate 1 — October 5, 2026: mathematical feasibility

All five conditions are required:

1. the prior is specified before observing responses and is not covert empirical Bayes;
2. its normalizer is finite under verifiable conditions;
3. arbitrary new parent configurations preserve an explicit posterior family;
4. at least one central operation is materially more tractable than generic Poisson regression; and
5. the strong directed hyper-Markov statement is ready as a precise theorem.

**Pivot A:** exact bounded-configuration Gamma prior.  
**Pivot B:** augmented additive-intensity Poisson DAG.  
**Pivot C:** retain the log-link model but rename the contribution “conditionally conjugate” or “computable.”  
**Pivot D:** if an impossibility result can be made sharp, publish a no-go theorem together with the strongest principled restricted construction.

### Gate 2 — November 30, 2026: computational readiness

Proceed to the MVP only if:

- local algorithms pass numerical validation;
- complexity is stated honestly;
- the proof auditor has no unresolved critical objection; and
- the proposed method demonstrably improves at least one of sampling, scoring, or deterministic integration.

### Gate 3 — January 31, 2027: graph-search readiness

Proceed to full simulations only if local scores are stable, one-edge updates are verified, the search budget is fixed, and exact small-graph comparisons quantify heuristic search error.

### Gate 4 — March 7, 2027: evidence freeze

Freeze simulation outputs, seeds, exclusions, and plotting scripts. New analyses after the freeze must be labeled exploratory.

## 12. Deliverables and definitions of completion

### Minimum viable paper — December 20, 2026

- fixed supplied ordering;
- one named prior family;
- propriety, posterior update, and directed hyper-Markov theorems;
- one Level A or Level B computation;
- exact small-dimensional validation;
- simulations up to approximately \(p=50\) or \(100\);
- open, tested reference implementation; and
- no unrestricted causal-discovery or asymptotic-consistency claim.

### Full paper — May 9, 2027

- modular ordered-DAG score and search method;
- simulation study through at least \(p=250\);
- hyperparameter defaults with sensitivity boundaries;
- robustness experiments;
- one credible application, if available;
- fully reproducible software and results archive; and
- a precise comparison with DY, GLM-conjugate, MLG/cMLG, hyper-Dirichlet, and Poisson-DAG-learning literatures.

### Project completion criteria

The project is complete when:

1. every advertised mathematical property has a proof or is explicitly labeled conjectural;
2. every numerical table can be rebuilt from committed scripts and frozen replicate outputs;
3. a clean environment compiles the manuscript without external or missing assets;
4. the novelty statement survives independent literature and proof review;
5. the title matches the actual level of tractability achieved; and
6. a citable archival software/data release accompanies the submission.

## 13. Repository and reproducibility workflow

Create the following structure at project kickoff:

```text
proposal/
paper/
references/
src/
tests/
simulations/config/
simulations/scripts/
data/README.md
data/raw/                 # ignored when redistribution is prohibited
data/processed/
results/frozen/
figures/
reproducibility/
```

Required controls:

- environment lockfile and container definition;
- one command that rebuilds every table, figure, test, and PDF;
- continuous integration for unit tests and a clean LaTeX build;
- deterministic seeds stored in versioned configuration files;
- tests for log densities, gradients, Hessians, updates, normalizers, graph-score locality, and samplers;
- all DAG diagrams embedded as TikZ;
- every non-TikZ figure committed with its generating script;
- no absolute file paths or case-sensitive filename ambiguities;
- `CITATION.cff`, license, data provenance, and archival release metadata; and
- a clean-room arXiv bundle test to prevent missing-figure failures.

## 14. Roles and governance

Assign these roles in the kickoff meeting:

- **Lead author:** model definition, primary construction, manuscript integration.
- **Graphical-model theorist:** directed hyper-Markov theorem, modularity, and identifiability.
- **Computational statistician:** normalizers, augmentation/sampling, approximation theory, and complexity.
- **Software and empirical lead:** implementation, tests, simulations, and reproducibility archive.
- **Application collaborator:** data provenance, scientific ordering, offsets, and interpretation.
- **Independent proof reader:** hostile audit of T2–T11.

Record provisional CRediT roles by September 6, revisit after the MVP, and settle authorship before the main simulation grid is run.

Project management cadence:

- 30-minute weekly technical meeting;
- written decision log after every gate;
- theorem ledger with owner, status, dependencies, and counterexamples;
- issue tracker labels for `theory`, `computation`, `simulation`, `writing`, and `release`; and
- no full simulation before theory and numerical validation pass their gates.

## 15. Risk register

| Risk | Early warning | Response |
|---|---|---|
| No finite-dimensional general conjugate family exists | Normalizer or update depends irreducibly on every observed parent configuration | Prove a no-go result; use measure-valued family or exact restricted pivot. |
| Construction is generic DY theory in new notation | All results follow immediately after substituting a Poisson likelihood | Require a new DAG-level compatibility, normalizer, modularity, or computational theorem. |
| MLG prior is mistaken for a new distribution | Main formulas duplicate established cMLG results | Treat MLG as prior art/comparator; claim only new DAG construction and consequences. |
| Hyper-Markov terminology is too loose | Proof shows only independent \(\beta_i\) | State sigma-field definition first; prove the property for the induced random law and ancestral restrictions. |
| Counts become explosive | Prior predictive upper quantiles or likelihood exponentials overflow | Calibrate effects, use log-sum-exp, report stability regime, and include stress tests. |
| Normalizer approximation is unstable | Small-case comparisons disagree materially with quadrature | Use high precision, derive error diagnostics, or downgrade the tractability claim. |
| Graph scores are not modular | Changing one parent set changes other prior terms | Redesign hyperparameter transport before implementing search. |
| Poisson application is misspecified | Strong overdispersion/zero inflation in predictive checks | Add robust analysis; narrow claims; consider a later negative-binomial extension. |
| Search heuristic is oversold | Exact small-graph optimum differs frequently | Report search success rate and budget; do not call the heuristic an exact optimizer. |
| Scope creep delays completion | Unrestricted ordering or asymptotic theory enters before MVP | Keep these in the optional extension milestone. |

## 16. Publication strategy

Target the outlet only after Gate 1:

- **Biometrika** or **Bayesian Analysis:** genuinely new exact family with substantial probability theory;
- **JRSS B** or potentially **Annals of Statistics:** strong graphical/high-dimensional theory in addition to the prior;
- **JCGS**, **Statistics and Computing**, or **Bayesian Analysis:** conditionally conjugate computational method with rigorous algorithms and strong experiments; or
- **Annals of Applied Statistics:** scientifically strong count-network application with a useful but less comprehensive theoretical contribution.

The working title should remain internal until the feasibility gate. The public title must reflect whether the achieved result is exact, conditionally conjugate, or controlled-approximate.

## 17. Immediate 14-day action list

### Days 1–3

- Freeze notation and the formal definition of strong directed hyper-Markovity.
- Write the zero-parent and one-parent likelihood/prior calculations.
- Independently verify the product-Poisson normalizer (N2) by symbolic calculation and numerical quadrature.
- Create the prior-art comparison matrix with columns for model, graph class, pre-data specification, conjugacy, normalizer, sampler, marginal score, and hyper-Markov result.

### Days 4–7

- Prove the finite-support cone propriety result or identify a counterexample.
- Derive gradients and Hessians for (P1) and (P2).
- Implement local log densities and high-precision reference integrals for dimensions 1–3.
- Reproduce an MLG/cMLG Poisson regression sampler from the published specification.

### Days 8–10

- Formulate the infinite-support propriety theorem for product-Poisson \(Q_i\).
- Write the induced-random-kernel hyper-Markov statement.
- Specify parent-set hyperparameter transport and test ancestral restriction.
- Compare prior-predictive count distributions under product-Poisson, MLG, Gaussian, and saturated Gamma priors.

### Days 11–14

- Run the first normalizer/sampler benchmark.
- Write a two-page feasibility memo: what is exact, what is conditionally conjugate, and what remains numerical.
- Assign theorem owners and proof reviewer.
- Freeze the six-week experiment list and Gate 1 criteria.

## 18. First manuscript outline

1. Introduction and novelty boundary
2. Ordered Poisson log-linear DAG model
3. The directed measure-valued conjugate prior
4. Propriety and posterior updating
5. Directed hyper-Markov and modularity properties
6. Exact or controlled vertexwise computation
7. Ordered-DAG marginal scores and search
8. Numerical validation and simulations
9. Application
10. Limitations, no-go boundaries, and extensions

Appendices should contain full propriety proofs, hyper-Markov sigma-field arguments, normalizer calculations, algorithms, numerical validation details, and reproducibility specifications.

## 19. Initial reference list

- Bradley, J. R., Holan, S. H., and Wikle, C. K. (2015). [Multivariate log-gamma distributions and their applications to Bayesian count data modeling](https://arxiv.org/abs/1512.07273).
- Bradley, J. R., Holan, S. H., and Wikle, C. K. (2019). [Conjugate multivariate distributions for dependent data](https://arxiv.org/abs/1701.07506).
- Chen, M.-H., and Ibrahim, J. G. (2003). [Conjugate priors for generalized linear models](https://www3.stat.sinica.edu.tw/statistica/j13n2/j13n212/j13n212.html).
- Choi, J., Chapkin, R. S., and Ni, Y. (2020). [Bayesian causal structural learning with zero-inflated Poisson Bayesian networks](https://papers.nips.cc/paper/2020/hash/4175a4b46a45813fccf4bd34c779d817-Abstract.html).
- Diaconis, P., and Ylvisaker, D. (1979). [Conjugate priors for exponential families](https://projecteuclid.org/journals/annals-of-statistics/volume-7/issue-2/Conjugate-Priors-for-Exponential-Families/10.1214/aos/1176344611.full).
- Kratzer, G. et al. (2023). [Additive Bayesian network modeling with the `abn` R package](https://doi.org/10.18637/jss.v105.i08).
- Massam, H., Liu, J., and Dobra, A. (2009). [A conjugate prior for discrete hierarchical log-linear models](https://arxiv.org/abs/0711.1609).
- Massam, H., and Wesołowski, J. (2016). [The hyper-Dirichlet and \(P\)-Dirichlet distributions](https://arxiv.org/abs/1412.0972).
- Nguyen, H. D. et al. (2024). [Guided structure learning of DAGs for count data](https://journals.sagepub.com/doi/abs/10.1177/1471082X241266738).
- Park, G., and Park, S. (2019). [High-dimensional Poisson structural equation model learning via \(\ell_1\)-regularized regression](https://jmlr.org/papers/v20/18-819.html).
- Park, G., and Raskutti, G. (2015). [Learning large-scale Poisson DAG models based on overdispersion scoring](https://proceedings.neurips.cc/paper_files/paper/2015/hash/fccb60fb512d13df5083790d64c4d5dd-Abstract.html).
- Park, G., and Raskutti, G. (2017). [Learning quadratic variance function DAG models via overdispersion scoring](https://arxiv.org/abs/1704.08783).

## Bottom line

The project is promising, but the novelty is not “a conjugate prior for Poisson regression.” The publishable contribution must solve the tension among unbounded parent configurations, a pre-data graph-modular prior, a true directed hyper-Markov law, and useful local computation. The six-week gate makes that standard operational. If the FR-DY or cMLG route reaches Level A or B tractability, the project can credibly become the directed count-data analogue sought here. If it does not, a sharp impossibility result plus an exact restricted construction remains a worthwhile and honest outcome.
