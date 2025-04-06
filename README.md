# PhD-data - review and meta-analysis in Thesis
PhD data ReviewMeta.csv

Meta-analysis - McKnight, E., R. Spake, A. Bates, D. A. Smale, and M. Rius. 2021. Non-native species outperform natives in coastal marine ecosystems subjected to warming and freshening events. Global Ecology and Biogeography 30:1698-1712 [Link](https://onlinelibrary.wiley.com/doi/10.1111/geb.13318)

**Aims**

Contemporary climate change and biological invasions are two main drivers of biodiversity redistribution. Interactive effects between these drivers have been reported in a variety of studies, yet results are conflicting. Some studies find that contemporary climate change facilitates the spread and success of non-native species, especially those with broad physiological tolerances. Other studies conclude that non-natives are vulnerable to current and future changes in climatic conditions. Given that most studies have focused on terrestrial species, here we contribute to this debate by analysing responses of marine native and non-native fauna and flora to key climate-related stressors, namely increased temperature (warming) and decreased salinity (freshening).

**Location:** 

Global

**Time-period:** 

2002–2019

**Major taxa studied:** 

Marine benthic macrophytes and invertebrates

**Methods**

I conducted a meta-analysis of experiments investigating the performance (e.g. growth, survival and reproduction) of benthic species in response to warming and freshening.
I used the ISI Web of Science (Thomson Reuters) to search the peer-reviewed published literature for papers using the following search terms: (‘climate change’ OR ‘global warming’ OR ‘ocean warming’) AND (experiment* OR manipul*) AND (temperature* OR thermal OR salin* OR freshen*) AND (marine OR coastal OR sea OR ocean), with no restriction on publication year. Additional literature was identified by ‘snowballing’ (i.e. searching for references within retrieved articles and reviews). I chose papers that measured biological responses of native and/or non-native species to experimentally controlled changes in temperature (warming) and reduced salinity (freshening). I included papers that measured any biological processes that relate to changes in biological rates (i.e. metabolism and respiration), health or performance (i.e. growth, survival, reproduction) at either the individual organism or population levels. I restricted the search to studies of sessile or sedentary marine and brackish organisms that inhabit depths of up to 10 m, and thus included species typical of shallow seas that are subjected to variation in abiotic stressors. Papers were included if they focused on laboratory or field-based manipulative experiments, either in separate experimental units or together in a community mixture, and measured biological processes among replicated control (ambient) and treatment groups subjected to elevated temperature and/or decreases in salinity. Studies were excluded if experiments did not include replicates or did not provide measures of variation. Multi-driver experiments that explored other variables (e.g. hypoxia or nutrients) for which the independent effects of temperature and freshening could not be measured were excluded.

All studies were coded according to the following covariates: manipulated stressor (warming or freshening), measured biological process (see details in Table 1), nativeness (native or non-native, identified according to the authors’ descriptions or through the World Register of Introduced Marine Species (Ahyong et al., 2020)), the experimental duration (number of days), type of organism (i.e. plant or animal), life-history stage (i.e. larva, adult or juvenile), and geographical location (absolute latitude). Finally, we calculated the ‘stress exposure’ of each study, the absolute difference between experimental and a control level exposure (in degrees Celsius or practical salinity units). For studies that measured environmental variables other than warming and freshening (such as hypoxia or nutrient variation), data were taken from only the experiments where these variables were considered at ‘ambient’ or ‘control’ levels, as determined or described by the authors. The control conditions of the focal environmental stressors (warming or freshening) needed to represent current ambient seawater at the time of collection and study or based on the authors’ knowledge and understanding of ambient conditions for the study organism(s) and area.

For each biological response (here distinguished as a ‘process’ – see Table 1) reported from each paper, we extracted values of mean, sample size (number of replicates) and variance (standard deviation, standard error, or confidence interval) for control and treatment groups. Where articles reported separate values for two or more study locations, species and biological process categories, we regarded each as an independent ‘study’. When studies reported several metrics for the same biological process category (e.g. growth expressed as changes in length and biomass) only one process was selected randomly from the most conservative results. For studies that included time-series data, measurements were taken from the final experimental time point.

**Data analysis**

We estimated Hedges’ d (Rosenberg et al., 2013; see Supporting Information Table S1 for equation and model details) and its variance for each study to estimate the magnitude and direction of the biological process to changes in warming and/or freshening. All analyses were carried out using R v3.6.2 (R Core Team, 2018) and we used R package ‘metaphor’ (Viechtbauer, 2010) to compute summary effect sizes.

The studies were highly heterogeneous; therefore, we split data into two core subgroups, according to stressor and nativeness. First, we separated studies according to stressor, warming and freshening, because these stressors exerted different effects on organisms through different physiological pathways. We then further subgrouped studies according to nativeness because we hypothesized natives and non-natives would respond differently to stressors. Next, for each of these four categories, we subgrouped by categorical variables of interest: the biological process measured, species-type and life-history stages (described in more detail in Supporting Information Figure S1).

We set out to compare the magnitude, direction and heterogeneity of effect sizes, representing responses to stressors, across the subgroups. To account for known sources of variability in the study attributes, we constructed global mixed-effect meta-regression models for each subgroup, consisting of the three continuous moderator variables: the level of stress exposure, experimental duration and absolute latitude. Continuous moderators were converted to z-scores to improve the interpretability and comparability of meta-regression coefficients (Schielzeth, 2010). In each analysis, studies were precision-weighted by the inverse of the sum of their within-study variance (Vd) and between-study variance (τ2), assuming that studies with lower within-study variance were more precise (Hedges & Olkin, 1985). In all models, article and species identifiers were included as random effects, as effect sizes from the same research study or species may be more similar than effects from different groups. For the subgroups divided by life-history stage and species-type, the measured biological process was specified as a random effect. From these global models, we generated a full set of nested models, all to be compared with Akaike’s information criterion (AIC) using R (v3.6.2; R Core Team, 2018). We identified a single, minimum adequate model as the one with the lowest AIC value (Burnham & Anderson, 2002). From these models, we interpreted mean effect sizes as the model intercepts, conditioned on mean values of covariates.

Summary effect sizes were interpreted according to Cohen’s benchmark (Cohen, 1962) of small, moderate and large effects from values of d in the regions of .2, .5 and .8, respectively. Statistical significance was attributed if bias-corrected 95% confidence intervals (Hedges & Olkin, 1985) did not overlap zero. Given the heterogeneity typical of ecological studies (Senior et al., 2016), our focus was on the overall direction and magnitude of summary effects, rather than the statistical significance. A total heterogeneity statistic (I2) was calculated, where I2 is the percentage of variance between effect sizes that cannot be attributed to sampling error (Higgins et al., 2003).

**Results**

We found that non-native species tended to respond positively to elevated temperature, whereas the performance of native species declined. Similarly, decreased salinity negatively affected the biological processes of native species, but non-natives showed neutral or negative overall responses to freshening.

**Main conclusions**

We find evidence that non-native species outperform natives under a wide variety of warming and freshening conditions. The growth and reproduction of non-natives are enhanced by warmer temperatures, and thus ocean warming is expected to facilitate future spread and success of non-native species. Increased freshening along future coastal areas, however, will likely have a negative impact in both native and non-native species and thus is expected to be a driver of significant change in coastal marine ecosystems. Our comprehensive analysis highlighted the need to expand our understanding of climate change effects beyond warming and specifically, studies focusing on salinity changes.








