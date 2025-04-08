### PhD-data - review and meta-analysis

## Meta-analysis 

[publication](https://onlinelibrary.wiley.com/doi/10.1111/geb.13318)

Contemporary climate change and biological invasions are two main drivers of biodiversity redistribution. Interactive effects between these drivers have been reported in a variety of studies, yet results are conflicting. Some studies find that contemporary climate change facilitates the spread and success of non-native species, especially those with broad physiological tolerances. Other studies conclude that non-natives are vulnerable to current and future changes in climatic conditions. Given that most studies have focused on terrestrial species, here I contribute to this debate by analysing responses of marine native and non-native fauna and flora to key climate-related stressors, namely increased temperature (warming) and decreased salinity (freshening).

**Location:** 

Global :globe_with_meridians: 🌏

**Time-period:** 

2002–2019 🕐 🕜 🕙 🕥 

**Major taxa studied:** 

Marine benthic macrophytes and invertebrates 🐠 🐟 🐳 🐋 🐬  🍁 🍃 🍂 🌿

**Methods**

Meta-analysis of experiments investigating the performance (growth, survival and reproduction) of benthic species in response to warming and freshening.
I used ISI Web of Science to search the peer-reviewed published literature for papers. The measured any biological processes related to changes in biological rates (i.e. metabolism and respiration), health or performance (i.e. growth, survival, reproduction) at either the individual organism or population levels. Studies were restricted to sessile or sedentary marine and brackish organisms that inhabit depths of up to 10 m, thus included species typical of shallow seas that are subjected to variation in abiotic stressors. Papers were included if they focused on laboratory or field-based manipulative experiments, either in separate experimental units or together in a community mixture, and measured biological processes among replicated control (ambient) and treatment groups subjected to elevated temperature and/or decreases in salinity. Studies were excluded if experiments did not include replicates or did not provide measures of variation. Multi-driver experiments that explored other variables (e.g. hypoxia or nutrients) for which the independent effects of temperature and freshening could not be measured were excluded.
All studies were coded according to the following covariates: manipulated stressor (warming or freshening), measured biological process (see details in Table 1), nativeness (native or non-native, the experimental duration (number of days), type of organism (i.e. plant or animal), life-history stage (i.e. larva, adult or juvenile), and geographical location (absolute latitude). Finally, I calculated the ‘stress exposure’ of each study, the absolute difference between experimental and a control level exposure (in degrees Celsius or practical salinity units).  
For each biological response - distinguished as a ‘process’ =

![alt text](https://github.com/ellamcknight/PhD-data/blob/main/Images/Table%201.png?raw=true)

Reported from each paper, we extracted values of mean, sample size (number of replicates) and variance (standard deviation, standard error, or confidence interval) for control and treatment groups. Where articles reported separate values for two or more study locations, species and biological process categories, we regarded each as an independent ‘study’. When studies reported several metrics for the same biological process category (e.g. growth expressed as changes in length and biomass) only one process was selected randomly from the most conservative results. For studies that included time-series data, measurements were taken from the final experimental time point.

**Data analysis**

We estimated Hedges’ d (Rosenberg et al., 2013; see Supporting Information Table S1 for equation and model details) and its variance for each study to estimate the magnitude and direction of the biological process to changes in warming and/or freshening. All analyses were carried out using R v3.6.2 using R package ‘metaphor’ to compute summary effect sizes.

![alt text](https://github.com/ellamcknight/PhD-data/blob/main/Images/Table%20S1.png?raw=true)

The studies were highly heterogeneous; therefore, we split data into two core subgroups, according to stressor and nativeness. First, I separated studies according to stressor, warming and freshening.
I then further subgrouped according to nativeness because we hypothesized natives and non-natives would respond differently to stressors. Next, for each of these four categories, we subgrouped by categorical variables of interest: the biological process measured, species-type and life-history stages.

I constructed global mixed-effect meta-regression models for each subgroup, consisting of the three continuous moderator variables: the level of stress exposure, experimental duration and absolute latitude. Continuous moderators were converted to z-scores to improve the interpretability and comparability of meta-regression coefficients. In each analysis, studies were precision-weighted by the inverse of the sum of their within-study variance (Vd) and between-study variance (τ2), assuming that studies with lower within-study variance were more precise. In all models, article and species identifiers were included as random effects, as effect sizes from the same research study or species may be more similar than effects from different groups. For the subgroups divided by life-history stage and species-type, the measured biological process was specified as a random effect. From these global models, we generated a full set of nested models, all to be compared with Akaike’s information criterion (AIC) using R. We identified a single, minimum adequate model as the one with the lowest AIC value. From these models, we interpreted mean effect sizes as the model intercepts, conditioned on mean values of covariates.


**Results**

![alt text](https://github.com/ellamcknight/PhD-data/blob/main/Images/Fig%201_1.png?raw=true)

I found that non-native species tended to respond positively to elevated temperature, whereas the performance of native species declined. Similarly, decreased salinity negatively affected the biological processes of native species, but non-natives showed neutral or negative overall responses to freshening.

![alt text](https://github.com/ellamcknight/PhD-data/blob/main/Images/Fig%202_1.png?raw=true)

![alt text](https://github.com/ellamcknight/PhD-data/blob/main/Images/FIG4_1.png?raw=true)

I found evidence that non-native species outperform natives under a wide variety of warming and freshening conditions. The growth and reproduction of non-natives are enhanced by warmer temperatures, and thus ocean warming is expected to facilitate future spread and success of non-native species. Increased freshening along future coastal areas, however, will likely have a negative impact in both native and non-native species and thus is expected to be a driver of significant change in coastal marine ecosystems. Our comprehensive analysis highlighted the need to expand our understanding of climate change effects beyond warming and specifically, studies focusing on salinity changes.

## Review

# Literature analysis

Research papers were grouped into category levels to describe the climate change literature based on their key focus and concepts (the most dominant ideas). Not all papers were divided into primary concepts, as some explored a range of specialities. For example, some papers focused on organismal development, such as growth, and physiological elements like tissue composition. Therefore, papers were counted more than once if concepts overlapped. The process of identifying the most dominant concept of the paper (secondary to the key concept) was repeated to create sub-groupings. The divisions of fundamental concepts allow for visualising research directions and knowledge gaps. 

![alt text](https://github.com/ellamcknight/PhD-data/blob/main/Images/litsearch_1.png?raw=true)

![alt text](https://github.com/ellamcknight/PhD-data/blob/main/Images/litsearch_1.png?raw=true)

![alt text](

Trends in global temperature and precipitation, publications and the geographical distribution of experiments were included. Below = (a) Global warming is indicated by the rapid increase in the Earth's global mean surface temperature. (b) Global mean precipitation anomalies over the past century (obtained from National Oceanic & Atmospheric Administration - NOAA). (c) Publications identified geographically and over time (n = 143). The cumulative number of papers investigating climate-driventemperature (red) and salinity (blue). Inset: geographical distribution of experiments included in the publications.

![alt text](https://github.com/ellamcknight/PhD-data/blob/main/Images/Fig.%201%20mapYrs_1.png?raw=true)





