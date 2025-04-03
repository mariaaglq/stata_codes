
/* ---------------------------SRQM Final Paper ---------------------------------

- GROUP 1: H. Abdoulkader Yacin, M.Aguilar Quispe, C. Naz Barsbay

- DATE:  30 April, 2021

- TOPIC: Determinants of the Brain Drain Phenomenon: A Cross-National Study of 
         Push Factors Across the World

- DATA:  Quality of Government (QoG) Standard Dataset 2016, provided by the 
		 Quality of Government Institute

- SUMMARY OF OUR RESEARCH PROJECT:

	This do-file explores the Quality of Government (QOG) dataset and focuses 
	on the braindrain phenomenon and its determinants in high, middle and 
	low income countries across the world. By definition, brain drain is the 
	emigration of a nation’s most highly skilled individuals.By studying some 
	of the determinants that facilitate high levels of brain drain in certain
	parts of the world, we are hoping to provide a better understanding of the 
	push factors identified in this research project. 

   
- HYPOTHESES: 

	H1: Higher levels of brain drain are observed in countries with lower 
		national income per capita and high levels of income inequality among 
		citizens.
	
	H2: Higher levels of brain drain are observed in countries with highly 
		competitive labor markets.
	
	H3: Higher levels of brain drain are observed in countries that lack 
		political stability, and effective and transparent institutions. 

	
----------------------------------------------------------------------------- */
* DATASET
*======================================
* We wanted our study to have a global scope, instead of a regional one. 
* Therefore, after careful consideration of the different datasets that were 
* made available to us, we decided to use the 2016 Quality of Government dataset 
* due both to its global scope and macro-level variables. We then proceeded to 
* open it and explore which variables we would find interesting. 

use data/qog2016, clear

*=======================================
* I) Identifying our Dependent Variable 
*=======================================
* As our topic of interest was focused mostly on migration, mobility and 
* development, we searched for variables related to those topics. 
* We found 3 interesting variables: "Net Migration" (Eurostat),"Net Migration"
* (World Bank) and "Human Flight and Brain Drain" (Fund for Peace). We took a 
* look at them in more detail.  
codebook eu_demo_cnmigrat

* This variable had too many missing values. So we discarded it. 

codebook wdi_mignet
codebook ffp_hf
* Both of these variables were numerical, and had a high number of observations 
* (163 and 125 respectively). As a result, we concluded that both are continuous 
* variables. Therefore, both could have satisfied the requirement to be our DV. 
* However, after reading the descriptions on the codebook and discussing it 
* further,  we decided that we were more interested in the Human Flight and 
* Brain Drain variable, so we proceeded to inspect it in more detail as our DV.
 
*.==============================================================================
* =  Part I - Dependent Variable: "Human Flight- Brain Drain (index)" (ffp_hf)
* ==============================================================================

* 1. Summary Statistics

* We begin by looking at general descriptive statistics such as minimum and 
* maximum values and number of observations.
su ffp_hf

* To make sure that it is a continuous variable, we explore its values.
fre ffp_hf

* We note that there are 17 missing observations, which is not too many: 
* (8.76% of the total).

* We see that the values of the DV to be continuous,therefore the variable 
* satifies the requirement to be the DV of our project. Now, we take a look at 
* percentiles and other measures (like median, kurtosis, skewness) to see how 
* it is distributed.
su ffp_hf, d

* We notice it appears to be not normally distributed, but the difference is not
* too big. It is slightly negatively skewed; and it has a negative kurtosis, but 
* these values are not too far from the ones of a normal distribution.


* We now calculate the coefficient of variation (CV)=(st dev/mean)*100
ret li
sca sd_braindr= r(sd)
sca mean_braindr= r(mean)
sca li
display (sd_braindr/mean_braindr)*100 
display "The coefficient of variation of Economic freeedom is "(sd_braindr/mean_braindr)*100 "%"
* The coefficient of variation of Brain Drain is 34% which means the amount of 
* variation in our sample is acceptably good.

* 2. Visualisation

* To get a visual representation of the distribution, we plot a histogram that 
* shows both the density of our sample (kdensity line in green) and compares it
* to a normal distribution line (in red).

hist ffp_hf, normal kdens percent kdenopts(lc(green)) bin(30) xti("Human Flight and Brain Drain in 2012") name(brain_drain_hist,replace) note ("From Quality of Government 2016")
* The distribution is not too far from normality.

* 3. Manipulation of the DV

* We  rename the DV for ease of reference.
ren ffp_hf braindrain
lab var braindrain "Brain Drain Index"

* No further manipulation is needed because we would like to keep the variable's 
* original values and missing values will be adressed in the third part.

* =============================================================================
* =                    Part II - The Independent Variables 
* =============================================================================


* H1) -- Economic Welfare
* ----------------------------------------- 

* First, we will inspect the following two IVs jointly because they 
* make up for our first hypothesis about economic determinants of brain drain.

* IV n°1:  GNI per capita, PPP (current internat. dollars)
* IV n°2:  Gini coefficient

* 1. Summary Statistics

* Renaming the IVs for ease of reference:
ren wdi_gnipcpppcur gnipc
ren wdi_gini gini
lab var gini "GINI Index"

* Summarizing the IVs:
sum gnipc gini
sum gnipc gini, d

* GNI is positively skewed and its kurtosis is considerably high; whereas Gini's
* kurtosis is close to normal distribution and has a slightly positive skewness.

fre gnipc gini
* Both IVs are quantitative and continuous. GNI has 12 missing values, while 
* Gini has 78.

* 2. Visualization 

* Now we want to visualise their distribution in a graph.
hist gnipc, normal kdens percent kdenopts(lc(green)) bin(50) xti("GNI per capita, PPP") name(gnipc_hist,replace) note ("From Quality of Government 2016")

hist gini, normal kdens percent kdenopts(lc(green)) bin(33) xti("Gini coefficient") name(gini_hist,replace) note ("From Quality of Government 2016")
* GNI does not seem normally distributed as the majority of observations 
* are concentrated in the lower values. Gini seems not too far from a normal 
* distribution, with no marked skewness nor kurtosis.

* We produce a boxplot to acknowledge any outliers.
gr hbox gnipc, name(gni_boxp,replace)
gr hbox gini, name(gini_boxp,replace)
* Indeed, GNI presents outliers and a very high one, which distorts the 
* distribution. On the other hand, Gini has some mild outliers.


* H2) -- Labor Market Competition
* -------------------------------------------- 
* We grouped these variables together because we believe they caracterize 
* the level of competition in the labour market, which is what our second 
* hypothesis tests:

* IV n°3: Average School Years, Female and Male (15+)
* IV n°4: Population Density (wdi_popden) 
* IV n°5: Unemployment, total (% of total labor force)

* 1. Summary Statistics

* We first rename the variables for ease of reference. 
ren bl_asy15mf schyrs
ren wdi_popden popden
ren wdi_unempilo unempl
lab var unempl "Unemployment rate"
lab var schyrs "Average School Years of  population in working age (+15)"

* We take a look at the main descriptive statistics of these variables:
sum schyrs popden unempl
sum schyrs popden unempl, d

* We observe that schooling years is not very far from a normal distribution, 
* Unemployment rate is considerably far from the normal distribution, and 
* Population density's skewness and kurtosis differ notably from normality. 
* The latter's kurtosis is alarmingly high.

* Now we observe their frequency distribution and acknowledge their missing 
* values.
fre schyrs popden unempl
* For population density, its remarkable divergence from normality can be seen 
* in the big distance between the median value (78.6) and the mean (280.9).
* For the other 2 variables, mean and median are pretty close.
* Average Schooling Years has 52 missing values, Population Density has only 3, 
* and unemployment has 24 missing observations.

* Another way to see that is through the interquartile ranges.
tabstat schyrs popden unempl, s(p25 median p75 iqr)
* We notice that the interquartile range of population density is notably bigger
* than that of the other 2 variables. This means that there is a very big space 
* between the first and the third quartile of our sample in terms of population 
* density, or put differently, the 50% central part of our sample is very spread.

* 2. Visualization 

* These observations can be confirmed by a visual representation
hist schyrs, normal kdens percent kdenopts(lc(green)) bin(30) title("Average Schooling Years of population in working age") name(schoolyears_hist,replace) note ("From Quality of Government 2016")

hist unempl, normal kdens percent kdenopts (lc(green)) bin(50) xti("Unemployment rate(% of total labor force)") name (unemplo_hist, replace) note ("From Quality of Govenrment 2016")

hist popden, normal kdens percent kdenopts (lc(green)) bin(50) xti("Population Density") name (popden_hist, replace) note ("From Quality of Govenrment 2016")
* As stated before, schooling is slightly skewed to the left, not too far from 
* a normal distribution. Unemployment is considerably skewed to the right.

* Population density has such extreme outliers that they distort the graph to 
* the point that it is difficult to estimate the distribution of the majority 
* of values. 

* This can be easily seen in a box plot.
gr hbox popden, name(popden_boxp, replace)

* So we decide to take a closer look at the histogram omiting those outliers.

hist popden if popden<1000, normal kdens percent kdenopts (lc(green)) bin(50) xti("Population Density") name (popden_hist_zoom, replace) note ("From Quality of Govenrment 2016")

* We notice that, when omitting outliers, the rest of the data is very 
* concentrated in small values, it is skewed to the left as its summary 
* statistics suggests. We will consider its logarithmic transformation 
* in the steps ahead.

* H3) -- Political Statibility and Institutional Effectiveness 
* ----------------------------------------------------------
* The variables grouped in this category are:

* IV n°6: Corruption Perceptions Index
* IV n°7: Government Effectiveness - Estimate 
* IV n°8: Political Stability - Estimate

* 1. Summary Statistics

*Renaming the variables for ease of reference
ren ti_cpi transparency
ren wbgi_gee gov_eff
ren wbgi_pse pol_stab
la var gov_eff "Government Effectiveness"
la var pol_stab "Political Stability"

* According to Transparency International who produced the Corruption 
* Perceptions Index (as found in the QoG dataset), its scale is from 0-100, 
* with lower values corresponding to highly corrupt countries and high values 
* to very transparent ones. Therefore, we proceed to rename the variable as 
* "transparency" to reflect its content effectively.
la var transparency "Transparency Perception Index"

sum transparency gov_eff pol_stab 
sum transparency gov_eff pol_stab, d

* Transparency has a slightly positive skewness (0.8) and a kurtosis that 
* is a bit smaller than normal (2.8). We notice that Government Effectiveness 
* includes negative values, and so does Political Stability. These two IVs 
* have skewness and kurtosis that are not far from normal standards, skewness 
* of 0.4 and kurtosis of -2.3 for the first, and for the last one, -0.5 
* skewness and 2.7 of kurtosis.

fre transparency gov_eff pol_stab 
* Transparency has 14 missing values, Government Effectiveness has 2.


* 2. Visualization 

* We will further verify our overview with a visualization of the data 
* distributions.

hist transparency, normal kdens percent kdenopts(lc(green)) bin(30) xti("Transparency Perceptions Index") name(transparency_hist,replace) note ("From Quality of Government 2016")

hist gov_eff, normal kdens percent kdenopts(lc(green)) bin(35) xti("Government Effectiveness") name(gov_eff_hist,replace) note ("From Quality of Government 2016")

hist pol_stab, normal kdens percent kdenopts(lc(green)) bin(45) xti("Political stability") name(pol_stab_hist,replace) note ("From Quality of Government 2016")
* The distribution of data on both Transparency and Government 
* Effectiveness are slightly skewed to the right but not too far
* from normality, whereas the distribution of Political Stability 
* looks very close to normality.

* As the presence of outliers was suggested by the histograms and boxplots, 
* we take a look at extreme ouliers
ssc install extremes

extremes popden schyrs unempl gnipc gini transparency gov_eff pol_stab braindrain, iqr(3) N

* The last box shows that our DV has 7 extreme outliers and most 
* of our IVs have between 7 and 8 extreme outliers, the only exception 
* being gini, which has only 3. This information completes our overview 
* of the distributions and, keeping this in mind, we proceed to manipulate
* the data.

* =============================================================================
* =       Part III - Missing Values Assessment
* =============================================================================

* To finalize our dataset we need to omit missing values.

* We begin with our DV, trying to find out in which countries 
* it has no observations.

fre cname if mi(braindrain)
* Most of them are small insular countries in the Caribbean. They make up a 
* total of 17 countries, which is 8.76% of our entire sample. Discarding them
* would leave our sample with 177 observations, which is not bad.

* Next, we observe the missing values pattern for all of our independent 
* variables.

misstable pat braindrain popden schyrs unempl gnipc gini transparency gov_eff pol_stab
* Droping our observations for which more than 1 variable is missing would mean 
* droping about 27% of our original sample of 194 countries. This is acceptable.
* However, there is 18% of our sample that has all DVs except for Gini, 
count if mi(gini)
fre cname if mi(gini)
* Keeping Gini would mean droping an additional 18% of our whole sample, 
* keeping only 51% of observations. 

* We calculate how many observations we would keep
dis 51*194/100
* And it seems to be about 99 values. We consider this as an acceptable number 
* of observations. We then proceed to drop variables that are not included in 
* our hypothesis, and all countries with missing values.

keep braindrain popden schyrs unempl gnipc gini transparency gov_eff pol_stab cname ccodealp ht_region

* We are also keeping cname the names of countries(cname), country names in 
* only 3 letters (ccodealp) and region (ht_region) for future graphs.

drop if mi(braindrain, popden, schyrs, unempl, gnipc, gini, transparency, gov_eff, pol_stab)

* The final size of our sample is 99 observations.
sum braindrain popden schyrs unempl gnipc gini transparency gov_eff pol_stab 

* ==============================================================================
* =                     Part IV - Further Analysis of the DV
* ==============================================================================

* 1. Normality Test of the Dependent Variable: Brain Drain
*-----------------------------------------------------------

* 1.a) Vizualization

* We produce a histogram to get a visual sense of the DV onto a graph of a 
* normal distribution. 
hist braindrain, normal kdens percent kdenopts(lc(green)) bin(30) title("Brain Drain in 2012") xti("Brain Drain Index") name(braindr_hist2,replace) note ("From Quality of Government 2016")

* After deleting some countries, the distribution of brain drain looks close to 
* normality.

* 1.b) Formal Assesment 

* We start by measuring its skewness and kurtosis.
su braindrain, d 
* In the new sample, the DV now demonstrates a negative skewness of -0.3 which 
* is not significantly far from 0-skewed normal distribution. The DV then 
* approximates to symmetry. Besides, kurtosis of Brain Drain is 1.98, which is 
* almost within the -2.0 and +2.0 acceptability range around a normal 
* distribution's kurtosis of 3. We believe that this is not such a big deviation 
* from normality.


* We want to get a visual representation of this deviation, so we plot the 
* variable against a normal distribution and its quantiles against the ones of 
* a normal distribution
pnorm braindrain, name(prnom_bdrain, replace)
qnorm braindrain, name(qnorm_bdrain, replace)
* The graphs show  a mild divergence from normality both in the central values 
* and in the tails. We therefore conclude that the our DV's deviation from 
* normality is not too important, but we will try to fix it in the next steps.


* 2. Transformation of the Dependent Variable: Brain Drain
*---------------------------------------------------------------

* We explore the possibility of transforming the DV to bring it closer to 
* normality. The kernel density curve is shown in green, and the normal one 
* in red.

gladder braindrain, percent kdensity kdenopts(lcolor(midgreen)) name(gladder_brdrain, replace)

* Since our DV is continuous and for ease of interpretation, we attempt a log 
* transformation.  

gen logbraindrain = ln(braindrain) 
la var logbraindrain "Log of Brain Drain Index"

* We now produce a histogram of logged braindrain and compare it to the 
* histogram of the variable without log, to spot any improvement.
hist logbraindrain, normal kdens percent kdenopts(lc(green)) bin(30) title("Brain Drain (logged values from 2012)") xti("Log Brain Drain") name(logbrdrain_hist,replace) note ("From Quality of Government 2016")

gr combine braindr_hist2 logbrdrain_hist, imargin(small) ysize(2) ycom xcom name(brdr_vs_logbrdr2, replace)
* This graph makes the comparison using the same scale in x and y axis. Just to 
* show how the values have changed.

* But the previous and new distributions can be better appreciated in the 
* following graph
gr combine braindr_hist2 logbrdrain_hist, imargin(small) ysize(2) name(brdr_vs_logbrdr, replace)
* Appart from increasing kurtosis, the log-braindrain histogram does not show 
* siginigicant improvements towards normality.

* Finally, we compare skewness and kurtosis of the original vs the logged 
* variable
tabstat braindrain logbraindrain, s(n sk kurtosis min max) c(s)

* The log form of Brain Drain presents a kurtosis of 2.89 closer to normal 
* standards (3) than the original values. Although this improved, skewness is 
* now -0.9, which is further from 0.

pnorm logbraindrain, name(prnom_lnbdrain, replace)
qnorm logbraindrain, name(qnorm_lnbdrain, replace)
* The tests completed above enable us to confirm that, although the Brain Drain 
* variable is not perfectly normally distributed, its values did not differ too 
* much from normal distribution. The log transformation did not improve the 
* distribution of the DV towards normality. Therefore, we decide to keep the DV 
* untransformed.  
drop logbraindrain

* 3. Transformation of Independent Variable: GNI per capita
*---------------------------------------------------------------

* 3.a) Visual Assessment

* As it is usual in research to use logaritmic transformations of indicators 
* like GNI or GDP, we will evaluate if this takes GNIpc closer to normality.

gladder gnipc, name(gladder_gni)
* Graphically, it does, so we proceed to create log of GNIpc. We add +1 to 
* avoid missing values.
gen log_gnipc=log(gnipc+1)
la var log_gnipc "Log of GNI per capita"

hist log_gnipc, normal kdens percent kdenopts(lc(green)) bin(30) name(loggni_hist,replace) note ("From Quality of Government 2016")

* We want to get a visual representation of the possible deviations from 
* normality:
pnorm gnipc, name(prnom_gni, replace)
qnorm gnipc, name(qnorm_gni, replace)
pnorm log_gnipc, name(prnom_loggni, replace)
qnorm log_gnipc, name(qnorm_loggni, replace)
* The graphs show that the dirvergence from normality found in the original 
* GNI variable is dramatically corrected by the log transformation, both in 
* the central values and in the tails.

* 3.b) Formal Assesment 

* We finally compare skewness and kurtosis of the original vs the logged 
* variable
tabstat gnipc log_gnipc, s(n sk kurtosis min max) c(s)
* In the new sample, the DV now demonstrates a  skewness of +1.13 and a 
* kurtosis of 3.55. However, its logged version has a skewness of -0.51 and 
* a kurtosis +2.43. 
* Overall we conclude that the transformation approximates the variable GNI 
* per capita to a normal distribution, so this will be used for the future 
* regressions.

* 4. Transformation of Independent Variable: Population Density
*------------------------------------------------------------------

* We mentioned previously our concern regarding the high kurtosis and 
* positive skewness of the variable. Here, we try transforming it:
gladder popden, name(gladderpopdens, replace)

* A logarithmic transformation would be recommended.
gen logpopden= log(popden + 1)
la var logpopden "Log of Population Density"


pnorm popden, name(prnom_popd, replace)
qnorm popden, name(qnorm_popd, replace)
pnorm logpopden, name(prnom_logpopd, replace)
qnorm logpopden, name(qnorm_logpopd, replace)

* We see a substantial reduction in the deviation of this variable's 
* distribution from the diagonal line. This means that the conversion to 
* logarithm would be appropriate. We will use the logged version of population 
* density from now on. 


* 5. Exploring the Hypotheses: Display of the DV over IVs
*---------------------------------------------------------------------

* Hypothesis 1:
**************************************

* IV n°1: Brain Drain and GNI per capita, PPP (current internat. dollars)

* As we are interested in comparing levels of braindrain across the world, we 
* create a categorical variable of the level of national income following the 
* World Bank's classification (https://bit.ly/3rzHSyN), and use it to visualize 
* our DV.

gen incgroup:incgroup = irecode(gnipc, 1035, 4045, 12535, .)

la def incgrouplbl 0 "Low income" 1 "Lower middle-income" 2 "Upper middle-income" 3 "High income", replace
la val incgroup incgrouplbl
la var incgroup "Income group"
fre incgroup


* Now we can observe the distribution of Brain Drain by income groups.
gr hbox braindrain, over(incgroup) name(braind_box_byincgroup, replace)

* These first results seem to confirm our hypothesis. The graph shows a lower 
* level of brain drain in higher income countries, suggesting a positive 
* relationship that we will verify in the next steps. 


* And similarly we would like to compare the distribution of brain drain but 
* with a specific focus on each income group
hist braindrain if incgroup ==0, bin(10) title("Brain Drain in Low income countries") name(braind_h0, replace)
hist braindrain if incgroup ==1, bin(10) title("Brain Drain in lower-middle income countries") name(braind_h1, replace)
hist braindrain if incgroup ==2, bin(10) title("Brain Drain in Upper-middle income countries") name(braind_h2, replace)
hist braindrain if incgroup ==3, bin(10) title("Brain Drain in High income countries") name(braind_h3, replace)
gr combine braind_h0 braind_h1 braind_h2 braind_h3, imargin(small) ysize(3) col(2) name(hist_brdr_comparison, replace)


* IV n°2: Brain Drain and Gini Coefficient

* We create a dummy variable for "high inequality", which takes value 1 if the 
* Gini coefficient is higher than 50% and 0 if it is lower.
gen highineq = gini 
replace highineq=0 if highineq<50
replace highineq=1 if highineq>=50
la def highineqlbl 1 "High inequality" 0 "Low inequality"
la val highineq highineqlbl
fre highineq

gr hbox braindrain, over(highineq) name(braind_box_byhighineq, replace)
* At a first glance, we see countries with more inequality have higher levels of 
* brain drain. So we can expect a positive correlation.

* Hypothesis 2
*************************************

* IV n°3: Brain Drain and Average Schooling Years

* Following Our World In Data's classification
* (https://ourworldindata.org/grapher/mean-years-of-schooling-long-run), we 
* generate a categorical variable as follows:
 
gen schctg=schyrs
replace schctg=0 if schctg <2
replace schctg=1 if schctg >=2 & schctg<4
replace schctg=2 if schctg >=4 & schctg<6
replace schctg=3 if schctg >=6 & schctg<8
replace schctg=4 if schctg >=8 & schctg<10
replace schctg=5 if schctg >=10 & schctg<12
replace schctg=6 if schctg >=12

la def schlbl 0 "less than 2 years" 1 "2 to less than 4 years" 2 "4 to less than 6 years" 3 "6 to less than 8 years" 4 "8 to less than 10" 5 "10 to less than 12 years" 6 "12 or more years"
la val schctg schlbl
la var schctg "Categories of average years of schooling"
fre schctg
* We use this categorical variable to plot Brain drain.
graph dot braindrain, over(schctg) title ("Brain drain by Average years of school") subtitle("population older than 15") name(dot_bdrainbysch, replace)
* We preliminary find that countries with lower average years of school seem to 
* have higher levels of brain drain, which would be contrary to our hypothesis.

* IV n°4: Brain Drain and Population Density 

* We use this variable to create a categorical one that we can use in the graph,
* based on the cutoffs made by Columbia and SEDAC (https://bit.ly/3eie9X9)
gen popdenctg=popden
replace popdenctg=0 if popdenctg<2
replace popdenctg=1 if popdenctg>=2 & popdenctg<=10
replace popdenctg=2 if popdenctg>10 & popdenctg<=40
replace popdenctg=3 if popdenctg>40 & popdenctg<=100
replace popdenctg=4 if popdenctg>100 & popdenctg<=500
replace popdenctg=5 if popdenctg>500
la def popdenlbl 0 "<2 pple/km" 1 "2 to 10 pple/km" 2 "11 to 40 pple/km" 3 "41 to 100 pple/km" 4 "101 to 500 pple/km" 5 ">500 pple/km"
la val popdenctg popdenlbl
la var popdenctg "Categories of population density" 
fre popdenctg
* Now we observe the distribution of brain drain over categories of population 
* density.
gr hbox braindrain, over(popdenctg) name(braind_box_bypopden, replace)
gr dot braindrain, over(popdenctg) name(braind_dot_bypopden, replace)
* It is clearer in the dot plot than in the box plot that there is a tendency 
* for countries of more population density to have higher levels of braindrain.
* This would confirm our hypothesis.

* IV n°5: Brain Drain and Unemployment Rate

* To visualize brain drain with respect to unemployment, we first need to 
* create a categorical variable. We use Our World in Data's cutoffs to create 
* the variable.
gen unemplctg=unempl
replace unemplctg=0 if unemplctg<6
replace unemplctg=1 if unemplctg>=6 & unemplctg<12
replace unemplctg=2 if unemplctg>=12 & unemplctg<18
replace unemplctg=3 if unemplctg>=18 & unemplctg<24
replace unemplctg=4 if unemplctg>=24

la def unempllbl 0 "6% or less" 1 "6 to 12%" 2 "12 to 18%" 3 "18 to 24%" 4 "24% or more"
la val unemplctg unempllbl
la var unemplctg "Groupings of unemployment rate" 
fre unemplctg

* Now we can visualize the distribution of Brain drain by Unemployment rates.
gr dot braindrain, over(unemplctg) name(braind_dot_byunempl, replace)
* There is no clear relationship to be distinguished in this graph. The relation 
* between these variables demands other methods to distinguish the direction of 
* the relation.

* Hypothesis 3
*************************************

* IV n°6: Brain Drain and Transparency Perceptions Index

* According to Transparency International who produced the Corruption 
* Perceptions Index, its scale is from 0-100, with lower values corresponding to 
* highly corrupt countries and high values to very clean ones.
* We thus create a categorical variable based on that.

gen htransp = transparency
replace htransp=0 if htransp<25 
replace htransp=1 if htransp>=25 & htransp<50 
replace htransp=2 if htransp>=50 & htransp<75 
replace htransp=3 if htransp>=75

la def htransplbl 0 "very low" 1 "low" 2  "high" 3 "very high"
la val htransp htransplbl
la var htransp "Levels of Transparency perception"
fre htransp
* We now produce the graph with Brain Drain
gr hbox braindrain, over(htransp) name(braind_box_bytransp, replace)
* This graph shows that there is more brain drain from countries with higher 
* transparency, which would verify our hypothesis, as of these preliminary 
* results. 

* IV n°7: Brain Drain and Government Effectiveness 

* According to the World Bank definition of the Estimate, it gives the country's
* score on the aggregate indicator, in units of a standard normal distribution, 
* thats is ranging from approximately -2.5 to 2.5. 
* We thus create a dummy variable for "high government effectiveness", which 
* takes value 1 if the Government Effectiveness is higher than 0, and 0 if it is 
* lower.

gen highgoveff = gov_eff
replace highgoveff=0 if highgoveff <0
replace highgoveff=1 if highgoveff >0 
la def highgovefflbl 1 "High Government Effectiveness" 0 "Low Government Effectiveness"
la val highgoveff highgovefflbl
fre highgoveff

* Now we can produce the graph with Brain Drain
gr hbox braindrain, over(highgoveff) name(braind_box_bygoveff, replace)
* One can observe that, as predicted by our hypothesis, there is more brain 
* drain in countries with low government effectiveness.

* IV n°8: Brain Drain and Political Stability 

* According to the World Bank Worldwide Governance Indicators website, the 
* Estimate gathers persceptions of the likelihood of political instability and/or
* politically-motivated violence. It gives the country's score on the aggregate 
* indicator in units of a standard normal distribution, i.e. ranging from 
* approximately -2.5 to 2.5. 
* Thus, we create a dummy variable for "High Political Stability", which 
* takes the value of 1 if the Government Effectiveness is higher than 0 and 
* 0 if it is lower, meaning "Low Political Stability". 

gen highpol_stab = pol_stab
replace highpol_stab=0 if highpol_stab <0
replace highpol_stab=1 if highpol_stab >0 
la def hpolstablbl 1 "High Political Stability" 0 "Low Political Stability"
la val highpol_stab hpolstablbl
fre highpol_stab

* Now we can produce the graph with Brain Drain
gr hbox braindrain, over(highpol_stab) name(braind_box_bygoveff, replace)
* Then again, it seems like countries with lower political stability experience 
* higher brain drain, which would verify our hypothesis.

***************************************************************
* Additional analysis: by geographical continent:

* Recoding geographical areas to shorter names for ease of reference:
recode ht_region (6/10 = 6), gen(region)

la var region "Geographical region"

la def regionlb 1 "E. Europe and PSU" 2 "Lat. America" 3 "N. Africa and M. East" 4 "Sub-Sah. Africa" 5 "W. Europe and N. America" 6 "Asia, Pacific and Carribean", replace

la val region regionlb

gr hbox braindrain, over(region) name(braind_box_byregion, replace)
* This graph shows a lower value of the median brain drain for Western Europe 
* and North America. Brain drain in Latin America is more widely distributed 
* than the rest of the regions, with values that go from 2.5 to 8.5 out of 10.
 
 hist braindrain, by(region) normal kdens percent kdenopts(lc(green)) bin(30) leg(off) name (braind_hist_byregion, replace)
* We see very different distributions based on the region. Most of the 
* observations from Western Europe and North America are concentrated in low 
* values of the Brain Drain index (less than 5 out of 10 points). Observations 
* from Latin America and Sub-saharan Africa are, on the other hand, are mostly 
* concentrated in values higher than 5 in the index, and this is even higher
* for countries from the Asia, Pacific and Caribbean region in our sample.
 
* ==============================================================================
* =                             Part V - T-Tests
* ==============================================================================


* As one can only run t-tests with dummy variables, and all our variables are 
* continuous, we could only run them for 3 variables that were previously 
* converted into dummies.

* 1) Does the level of brain drain vary significantly when there exists a high 
* inequality in a given country in comparison to when it does not? 
ttest braindrain, by(highineq)

* At a 5% margin of error, we cannot reject the hypothesis that countries with 
* low inequality have the same levels of brain drain as countries with high 
* inequality. However, if we look at the confidence intervals, we see they 
* overlap just a little bit. Also, the p value of the t-test was only slightly 
* higher than the 5% threshold, so we would not rely too much on this result. 
* One possible explanation is that the threshold (0.5) chosen by intuition to 
* separate low from high inequality countries was not accurate. This is 
* supported by the fact that only 14 out of 99 observations were classified as 
* "high inequality", possibly too few to calculate a statistically significant 
* difference. We prefer to confirm this preliminary result with a simple 
* regression.
reg braindrain gini

* Indeed, when we use the gini coefficient as a continous variable, we observe 
* that there is a statistically significant positive relationship between 
* inequality and brain drain (with a 5% margin of error). Higher inequality is 
* associated with higher levels of brain drain in our sample.

* 2) Does the level of brain drain vary significantly when a given country's 
* government is highly effective in comparison to when it is not?
ttest braindrain, by(highgoveff)
* At 5% margin of error, the test indicates that countries with low government 
* effectiveness significantly differ from countries with high government 
* effectiveness in their levels of brain drain. Countries with poorly effective 
* states have in average a higher level of brain drain.

* 3) Does the level of brain drain vary significantly when the country has high 
* political stability in comparison to when it does not?
ttest braindrain, by(highpol_stab)
* At 5% margin of error, we can state that ountries with low political stability
* significantly differ in their levels of brain drain from countries with high 
* political stability. Low political stability is associated in average with 
* higher levels of brain drain.

* ==============================================================================
*                          Part VI - Association Tests
*==============================================================================

* 1) Correlations Matrix
*********************************

* To review all possible correlations, we  build a correlation matrix from
* respective pairwise correlations, with  an asterisk above statistically 
* significant correlations (5% margin of error).
pwcorr braindrain log_gnipc gini schyrs popden unempl transparency gov_eff pol_stab, star(.05)

* We see there is a high correlation between a pair of independent variables: 
* government efficiency and transparency (0.94). No other pair surpasses the 
* 90% threshold, but log of GNI has a high correlation with average school years
* and government efficiency of the country(>80%). We will revise for 
* multicollinearity between these variables in the following steps. 
* Additionally, we see in the first column that almost all independent variables
* have statistically significant correlations with brain drain, with also rather 
* robust correlation coefficients (most above +/-0.5). However, only population 
* density and unemployment have small non-significant correlation coefficients, 
* which makes us think that we may have to discard them later.


* We now export the correlation matrix into a Word Document.
ssc install corr2docx

corr2docx braindrain log_gnipc gini schyrs popden unempl transparency gov_eff pol_stab using correlation_matrix.docx, pearson(pw) ///
star(* 0.05 ** 0.01 *** 0.001) landscape title("Correlation Matrix") replace

* 2) Visually Inspecting Correlations: Scatterplots 
*******************************************************

graph matrix braindrain log_gnipc gini schyrs logpopden unempl transparency gov_eff pol_stab, half name(scatmatrix, replace) 
* A visual inspection of the graph tells us that the variables look well 
* distributed. Some are very spread with no clear tendency, while others 
* present an evident trend, just as the correlation matrix showed.

* ==============================================================================
*                          Part VII - Regression Models
*=============================================================================

* As we have 3 hypotheses, we will create a linear regression model for each.
ssc install asdoc, replace
* Model 1: Factors Related to Economic Welfare
************************************************************

reg braindrain log_gnipc gini
 
asdoc reg braindrain log_gnipc gini, nest replace cnames(Model 1) stat(r2_a) save(models.doc)

* Model 2: Factors Related to Labor Market Competition
*************************************************************

reg braindrain schyrs logpopden unempl
* The model has an Adjusted R-squared of 45.53%, but unemployment and population
* density are not statistically significant.


asdoc reg braindrain schyrs logpopden unempl, nest append cnames(Model 2) stat(r2_a) save(models.doc)

* Model 3: Factors Related to Institutional Effectiveness  
*************************************************************

reg braindrain transparency gov_eff pol_stab
* Political stability is not statistically significant at 5% nor 10% level. 
vif
* No variable presents perfect multicollinearity but transparency and government 
* efficiency are have Variance Inflation Factors very close to 10.

reg braindrain transparency gov_eff
* The model without political stability had a higher Adjusted R-squared, it went
* from 73.14% to 73.41%. 

reg braindrain gov_eff
* This model's Adjusted R-squared went down to 73.18%. We'd then prefer to keep
* the two variables.

asdoc reg braindrain transparency gov_eff pol_stab, nest append stat(r2_a) cnames(Model 3) save(models.doc)

* Until now, the best model from our separate hypothesis is the third one, with 
* an adjusted R-squared of 73.14%.


* Model 4: All Factors Combined 
****************************************************************

* Running a comprehensive regression of the Dependent Variable and all the 
* Independent Variables. 
reg braindrain log_gnipc gini schyrs logpopden unempl transparency gov_eff pol_stab  

* According to the p-values of the variables in this regression, school years, 
* unmployment,and political stability became non-siginigicant, and even 
* transparency, for just a bit.


 


****write how much each regression explains (R squared, coeffs, t tests of variables)
* Conclusions** it seems like for braindrain, the macro-economic factors -GNI and gini- matter a lot. Also population density and transaprency in a country matter.[complete]
****************************************


* ==============================================================================
* =                 Part VIII - Diagnostics of Regression Models
* ==============================================================================

* (1) Testing for Multicollinearity
*----------------------------------------------
vif
* This regression, as suspected before, presents collinearity between government
* efficiency and transparency. This makes sense conceptually, because states
* that are transparent tend to be the ones that are more efficient too. We, then
* proceed to produce models excluding one of these variables at each time.

* Model 5: Comprehensive Model without Government Efficiency
reg braindrain log_gnipc gini schyrs logpopden unempl transparency pol_stab

* Average school years, unemployment and political stability are still 
* non-significant. But transparency became statistically significant. 

* Model 6: Comprehensive Model without Transparency
reg braindrain log_gnipc gini schyrs logpopden unempl gov_eff pol_stab
asdoc reg braindrain log_gnipc gini schyrs logpopden unempl gov_eff pol_stab, nest append stat(r2_a) cnames(Model 6) save(models.doc)
* Similarly, in this model, government efficiency became statistically 
* significant, but political stability, unemployment, and average schooling 
* years are not. The Adjusted R-squared is higher than in model 5, so we prefer
* to keep this model rather than the other.


* (2) Inspecting the Distribution of Residuals
* ------------------------------------------------

* Model 6: Comprehensive Model without Transparency
reg braindrain log_gnipc gini schyrs logpopden unempl gov_eff pol_stab

* Store the predicted values
cap drop yhat
predict yhat

* We store the unstandardized residuals.
cap drop r
predict r, resid

* We now assess the normality of the residuals, beginning by a visual inspection. 
* First, with a kernel density plot
kdensity r, norm legend(off) ti("Distribution of Residuals of Model 6") name(diag_kdens, replace)

* The distribution of the residuals (blue line) of our model is visually very 
* close to that of a normal distribution.

* In a histogram,it becomes a bit clearer how the residuals are distributed and
* its divergence from a normal distribution.
hist r, normal percent bin(15) name(resid_hist, replace)
* However, the majority of the unstandardized residuals are concentrated around
* 0, which is good. 
sum r, det
* Skewness of the distribution is low (close to 0), which means that the 
* distribution is quite symmetric, and this is good news for the goodness of 
* our model.

* We plot the unstandardized residuals versus fitted values
sc r yhat, yline(0) ms(i) mlab(ccodealp) name(resd_sc, replace)
* or we can use another command for the same result
rvfplot, yline(0) ms(i) mlab(ccodealp) name(rvf_plot, replace)
* As predicted, the residuals look evenly distributed around the 0 value. We do 
* not see a clear trend, which is indicative of goodness of fit of our model.

* To check the presence of outliers, we use standardized residuals.
cap drop rsta
predict rsta, rsta

* Now we can highlight the outlier countries beyond 2 standard deviations.
sc rsta yhat, yline(-2 2) || sc rsta yhat if abs(rsta) > 2, ylab(-3(1)3) mlab(ccodealp) legend(lab(2 "Outliers")) name(diag_rsta, replace)

* We identify that Colombia and Iraq have residuals slightly higher than 2 
* standard deviations from 0. But the highest residual left by our model is the 
* one generated for Mongolia (MNG), which is the only one more than 3 standard 
* deviations away from the central value of 0. This means that our model is a 
* lot more imperfect at estimating the level of brain drain in Mongolia than for
* the other countries.

* We now check for that the residuals are homoskedastic conditional to each one
* of our regressors.
sc r log_gnipc, yline(-2 2) || sc rsta yhat if abs(rsta) > 2, ylab(-3(1)3) mlab(ccodealp) legend(lab(2 "Outliers")) name(resd_gni, replace)

sc r gini, yline(-2 2) || sc rsta yhat if abs(rsta) > 2, ylab(-3(1)3) mlab(ccodealp) legend(lab(2 "Outliers")) name(resd_gini, replace)

sc r schyrs, yline(-2 2) || sc rsta yhat if abs(rsta) > 2, ylab(-3(1)3) mlab(ccodealp) legend(lab(2 "Outliers")) name(resd_sch, replace)

sc r logpopden, yline(-2 2) || sc rsta yhat if abs(rsta) > 2, ylab(-3(1)3) mlab(ccodealp) legend(lab(2 "Outliers")) name(resd_popden, replace)

sc r unempl, yline(-2 2) || sc rsta yhat if abs(rsta) > 2, ylab(-3(1)3) mlab(ccodealp) legend(lab(2 "Outliers")) name(resd_unemp, replace)

sc r gov_eff, yline(-2 2) || sc rsta yhat if abs(rsta) > 2, ylab(-3(1)3) yti(Residuals) xti(Government Efficiency) mlab(ccodealp) legend(lab(2 "Outliers")) name(resd_goveff, replace)
	
sc r pol_stab, yline(-2 2) || sc rsta yhat if abs(rsta) > 2, ylab(-3(1)3) mlab(ccodealp) legend(lab(2 "Outliers")) name(resd_polstab, replace)	

* From all these graphs, only government effectiveness and political stability 
* worry us, because they present 4 outliers at high levels of the independent 
* variables.

* Therefore, we plot a LOWESS curve with each of these two variables, to further
* look for a trend.
lowess rsta gov_eff, bw(.5) yline(0) name(lowess_goveff, replace)

lowess rsta pol_stab, bw(.5) yline(0) name(lowess_polstab, replace)

* Both lowess lines are very close to the y-line at 0 and the cloud of residuals
* look quite evenly distributed, so we are not very concerned about the impact 
* of government effectiveness nor political stability in the residuals of our 
* model.

*(3) Interaction Terms
*------------------------------------

* Model 6:
reg braindrain log_gnipc gini schyrs logpopden unempl gov_eff pol_stab

* Is there a different effect of income per capita on brain drain depending on 
* the level of inequality of the country?
reg braindrain c.log_gnipc##c.gini schyrs logpopden unempl gov_eff pol_stab
* Our results suggest that there is not a statistically significant different
* effect.

reg braindrain c.log_gnipc##c.schyrs gini logpopden unempl gov_eff pol_stab
* We find that different levels of average school years of the population do
* not generate a statistically different effect of per capita income in brain 
* drain.

reg braindrain log_gnipc gini c.logpopden##c.schyrs unempl gov_eff pol_stab

reg braindrain log_gnipc gini c.logpopden##c.unempl schyrs gov_eff pol_stab

reg braindrain c.log_gnipc##c.unempl gini logpopden schyrs gov_eff pol_stab
* Similarly, when interacting population density and average school years of 
* the population, population density with the national level of unemployment, 
* and per capita income with the level of unemployment, none of these factors 
* yield statistically significant results.

*** Model 6 with Dummies Based on Income Levels of Countries

reg braindrain log_gnipc gini i.incgroup schyrs logpopden unempl gov_eff pol_stab
* Finally, we find no statistically significant association between the 
* different income categories of countries with the levels of brain drain.

*** Model 6 with Dummies based on Geographical Region

reg braindrain log_gnipc i.region gini schyrs logpopden unempl gov_eff pol_stab
* We find no significant effect of the different geographical regions of the 
* countries with their corresponding levels of brain drain, except for the 
* dummy of 'Europe and North America'. 


*** Applying Model 6 to Regional Sub-Samples
bys region : reg braindrain log_gnipc gini schyrs logpopden unempl gov_eff pol_stab

* We observe that for the subregion of 'North Africa and the Middle East', 
* the variables log of population density and political stability present 
* collinearity. We also notice that this model had only 6 observations, so that 
* probably makes it hard for Stata to calculate the significance of the variables 
* specified.The models in the regional sub-samples all have adjusted R-squared 
* higher than 60%, except for 'Asia, Pacific and Caribbean' and 'North Africa 
* and Middle East'. It is also noteworthy that some variables see their signs 
* changed across these estimated models. Across all regions, less average 
* schooling years is associated with more brain drain, except for Western Europe
* and North America, where more school years is associated with more brain drain.
* Also, we expected a negative relationshipt between political stability and 
* brain drain (as found in Model 6), but for 'Europe and Post Sovietic Union', 
* and 'Western Europe and North America' we found substantially positive 
* coefficients and this was statistically significant for 'Western Europe and 
* North America', suggesting that in these regions, more political stability 
* could be associated with more brain drain.

cap drop asdoc
asdoc reg braindrain log_gnipc gini schyrs logpopden unempl gov_eff pol_stab if region==1, nest replace cnames(Region 1) stat(r2_a) save(models_geo.doc)
asdoc reg braindrain log_gnipc gini schyrs logpopden unempl gov_eff pol_stab if region==2, nest append cnames(Region 2) stat(r2_a) save(models_geo.doc)
asdoc reg braindrain log_gnipc gini schyrs logpopden unempl gov_eff pol_stab if region==3, nest append cnames(Region 3) stat(r2_a) save(models_geo.doc)
asdoc reg braindrain log_gnipc gini schyrs logpopden unempl gov_eff pol_stab if region==4, nest append cnames(Region 4) stat(r2_a) save(models_geo.doc)
asdoc reg braindrain log_gnipc gini schyrs logpopden unempl gov_eff pol_stab if region==5, nest append cnames(Region 5) stat(r2_a) save(models_geo.doc)
asdoc reg braindrain log_gnipc gini schyrs logpopden unempl gov_eff pol_stab if region==6, nest append cnames(Region 6) stat(r2_a) save(models_geo.doc)

* We check that the residuals are also normally distributed for each region.
hist r, normal by(region) bin(10) xline(0) name(resd_region, replace)

* The residuals's distribution looks close to a normal one in Sub Saharian 
* Africa and 'Europe and North America'.


********************************************************************************
*                        ========= THE END ==========
********************************************************************************
