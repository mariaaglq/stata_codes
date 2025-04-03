***cleaning time series QOG 2021********
holi
*5 mayo
*****
* region 5 contains Europe and North America. Dropping Europe
keep if ht_region==2|ht_region==4| cname=="Canada"|cname=="United States"
la def regionlb 2 "Latin America" 4 "Sub-Sah. Africa" 5 "North America", replace
la var ht_region "Geographical Region"
la val ht_region regionlb

****6 mayo*****************
keep aii_acc aii_aio aii_q08 aii_q09 aii_q10 aii_q14 aii_q24 aii_q25 aii_q26 aii_q29 aii_q30 aii_q35 aii_q37 aii_q42 aii_q49 aii_q48 aii_q45 aii_q52 aii_q55 bti_acp bti_aod bti_gi bti_gp bti_ig bti_pdi bti_poa bti_ij bti_sc bti_sop ccp_cc ccp_market ccp_systyear cri_contr cri_cvalue cri_nocall cri_nonopen cri_singleb cspf_sfi ffp_ps ffp_ued ffp_eco gcb_butil gcb_pper gcb_bper gcb_putil gcb_pj gcb_ppa gcb_pparl hf_business hf_efiscore hf_govint hf_invest hf_trade iiag_acc iiag_be iiag_corr iiag_gov iiag_hd iiag_inf iiag_pa iiag_pri iiag_rolf ti_cpi vdem_corr vdem_exthftps vdem_exembez vdem_execorr vdem_exbribe vdem_elvotbuy vdem_excrptps vdem_gcrrpt vdem_jucorrdc vdem_mecorrpt vdem_pubcorr wbgi_cce wbgi_gee wbgi_rqe wbgi_vae wdi_tacpsr wdi_taxrev wef_qoi wef_wi wef_elec wef_gci wel_coc wel_sma diat_ati diat_iti diat_ti kun_polrel kun_polabs h_polcon3 wvs_jabribe nelda_mbbe sgi_qdai sgi_ecbg sgi_enge sgi_goea sgi_qdrlc dr_eg dr_ig wdi_gini wdi_fdiin wdi_gdpcapgr jw_persr wdi_gdpcappppcon2017 wdi_oilrent nrmi_nrpi aii_q30 ictd_revres ccode ccodealp cname ccodewb ht_region year

*exploring variables
tab cname if htregion==2, sum(vdem_corr)

***verificando si todas las var tienen datos para latam
br aii* if ht_region==2
drop aii* cri* iiag*
br cname year sgi* if ht_region==2
* sgi solo tiene data para 2013 a mas, para Mexico y Chile

* nelda_mbbe (media bias before election) tiene solo para como 4 años,y encima es solo "si/no" :| not useful....

*bti tiene numeros para años de 2005 a mas, varios x país
*ccp, sobre constitutiones, tiene para todos los años salvo 2020, pero solo es si/no
*cspf_sfi numeros del 1 al 10 para todos
*diat: tiene para todos los paises hasta 2010
*dr_eg y dr_ig tiene data continua para todos los paises y años
*ffp* entre 2005-2019 sí, para todos los paises.
*gbc* tiene data continue para 3 o 5  años x pais, para casi todos los paises. Not great
*h_polcon3 ta bien
*hf* continuo y para todos!
*kun* continuo, para todos los paises, pero de 2000-2010
* ti_cpi (corruption perception index) hay para varios años de cada país, ca va
*vdem* hay para todos los paises casi casi todos los años, solo vdem_elvotbuy faltan varios paises pero assez bien
*wbgi* continua y para todos los paises y años
*wdi* continuo y super completo, salvo wdi_tacpsr (CPIA) q solo hay para  haiti honduras nicaragua y bolivia, y wdi_oilrent hay para casi todos los paises de latam, salvo paraguay y uruguay en sudamerica, y algunos mas de centroamerica.
*wef* solo 2016,17,18. es la variable de infraestructura
*wvs_jabribe solo hay 2 obs por pais
*wel_coc (control of corruption) data continua 2000-2012 (no 2001)

*evaluando si drop wel_sma
bys year: tab cname if ht_region==2, sum(wel_sma) //en cada año, qué países sí tienen observaciones
* en los años q sí hay obs lo maximo son 3-4 paises (2005,2006,2000,2012), no hay año en q haya obs de todos. No permitiría hacer comparación de todos los países. quizas comparar 3,o solo 1 pais a través del tiempo


**dropped***
*ictd_revres no tiene para peru ningun año y para varios paises falta :/ dropping
*nelda_mbbe es solo si/no, y tiene pocas obs para cada pais
*nrmi_nrpi (sobre proteccion del ambiente) --> está assez bien, pero no tiene nada q ver con el tema, drop
drop ictd_revres jw_persr nelda_mbbe nrmi_nrpi sgi*

************DATA SET TERMINADA **********

*****buscando proxy para predominance of capitalism in the country
bys ht_region: fre ccp_market
* In Latam, only 5 mention capitalism in their constitution, even in North America, both don't.
list cname if ht_region==2 & ccp_market==1
tab cname if ht_region==2 & year==2016, sum(ccp_market)
* only Guatemala, Dominican republ, cuba, colombia and Peru mention it. I think this variable is not a good proxy for strenght of capitalism in a country. May drop it.
list cname hf_trade if ht_region==2 | ht_region==5
list cname bti_sel if ht_region==2 | ht_region==5

* ccp_cc corruption comission present in constitution, no hay en ningun pais latam. drop

** 
xtset ccode year
xtdescribe

bys year: tab cname if ht_region==2, sum(ti_cpi)
xtline ti_cpi if ht_region==2, i(cname) t(year) tline(2016) name(corrpercp_time_2, replace)

bys year: tab cname if ht_region==2, sum(vdem_corr)
xtline vdem_corr if ht_region==2, i(cname) t(year) tline(2016) name(vdemcorr_panel, replace)

xtline acp, i(cname) t(year) tline(2017) name(bti_acp_panel, replace)

xtline wbgi_cce if ht_region==2, i(cname) t(year) tline(2016) name(contrcorr_panel, replace)

xtline bti_poa if ht_region==2, i(cname) t(year) tline(2016) name(bti_poa_panel, replace) //prosecution of office abuse

***creating lavajato bribes variable

gen lvjbrdmy = cond(ht_region==2,1,0)
replace lvjbrdmy = 35 if cname=="Argentina"
replace lvjbrdmy = 349 if cname=="Brazil"
replace lvjbrdmy = 98 if cname=="Venezuela"
replace lvjbrdmy = 92 if ccodealp=="DOM"
replace lvjbrdmy = 59 if cname=="Panama"
replace lvjbrdmy = 50 if cname=="Angola"
replace lvjbrdmy = 33.5 if cname=="Ecuador"
replace lvjbrdmy = 29 if cname=="Peru"
replace lvjbrdmy = 18 if cname=="Guatemala"
replace lvjbrdmy = 11 if cname=="Colombia"
replace lvjbrdmy = 10.5 if cname=="Mexico"
replace lvjbrdmy = 0.9 if cname=="Mozambique"
rename lvjbrdmy lvjbr
la var lvjbr "Bribes paid by Odebrecht (mill USD) to 2016"
replace lvjbr = 0 if lvjbr==1

gen lvjbrdmy=cond(lvjbr~=0,1,0)
la var lvjbrdmy "Country was involved in Odebrecht scandal"
rename lvjbrdmy bribed
rename lvjbr amountbr

rename bti_acp acp


******creating post-pre 2016 var
gen post= cond(year>2016,1,0)
la var post "After 2016"

******estimating?
**first attempts:"Anti-corruption policy"
xtreg acp bribed post post##bribed,fe

xtreg acp amountbr post post##c.amountbr if ht_region==2,fe
outreg2 using "models.xls", replace ctitle(bamt_fe_latam)

****without panel function
reg acp amountbr post post##c.amountbr, r
diff acp, t (bribed) p(post)
reg acp bribed post post##bribed, r
reg acp bribed post post##bribed if year==2005|year==2007|year==2009 |year==2011|year==2013 |year==2015 |year==2017 |year==2019, r
diff acp, t (amountbr) p(post)

reg acp amountbr post post##c.amountbr if year==2005|year==2007|year==2009 |year==2011|year==2013 |year==2015 |year==2017 |year==2019 & ht_region==2, r

***** again with panel
xtreg acp amountbr post post##c.amountbr wdi_fdiin wdi_gdpcappppcon2017 ti_cpi i.year i.ccodewb if ht_region==2, fe

reg acp amountbr post post##c.amountbr wdi_fdiin wdi_gdpcappppcon2017 ti_cpi wbgi_gee i.year i.ccode if ht_region==2, r

****checking if there's visual association
xtline acp if ht_region==2 & bribed!=0|cname=="Angola"|cname=="Mozambique" , i(cname) t(year) tline(2016) name(acp_panel_bribed, replace)
* indeed, there's substantial improvement only in 5 out of 10 ctries -> that's why it's not significant

** trying Prosecution of Office Abuse
xtline bti_poa if ht_region==2 & bribed!=0, i(cname) t(year) tline(2016) name(btipoa_panel_bribed, replace) //not good

xtline wbgi_cce if ht_region==2 & bribed!=0, i(cname) t(year) tline(2016) name(wbgi_cce_panel_bribed, replace)

**with "Control of Corruption"
reg wbgi_cce bribed post post##bribed wdi_fdiin wdi_gdpcappppcon2017 ti_cpi wbgi_gee i.year i.ccode if ht_region==2, r

*****creating new time variable
xtline acp if ht_region==2 & bribed!=0|cname=="Angola"|cname=="Mozambique" , i(cname) t(year) tline(2017) name(acp_pnl_bribed2017, replace)
**looks like the change appeared 1 yr after the scandal: 2017
*actually "post" is 1 for >=2017

******good model 1**********************
***reg1
reg acp bribed post post##bribed wdi_fdiin wdi_gdpcappppcon2017 ti_cpi wbgi_gee i.year i.ccode if ht_region==2|cname=="Angola"|cname=="Mozambique" , r
* inclugin Angola and Mozambique, still non-significant but closer.

reg acp bribed post post##bribed wdi_fdiin wdi_gdpcappppcon2017 ti_cpi wbgi_gee i.year i.ccode , r


****testing parallel trends assumption
twoway (tsline acp if bribed==1, lcolor(green)) (tsline acp if bribed==0, lcolor(red)) if year==2005|year==2007|year==2009 |year==2011|year==2013 |year==2015 |year==2017 |year==2019 & ht_region==2, tline(2017) name(acp_parlltrendsassmpt, replace)

bys year: egen avgacpbr= mean(acp) if bribed==1
bys year: egen avgacpnobr= mean(acp) if bribed==0
la var avgacpbr "Average acp for countries involved in Odebrecht case"
la var avgacpnobr "Average acp for countries not involved in Odebrecht case"

twoway (tsline avgacpbr if bribed==1, lcolor(green)) (tsline avgacpnobr if bribed==0, lcolor(red)) if ht_region==2, tline(2017) leg(row(1) order(1 "Bribed" 2 "Not Bribed")) name(acp_parlltrendsassmpt2, replace)
***parallel trend assumption is pretty much accepted!!
twoway (tsline avgacpbr if bribed==1, lcolor(green)) (tsline avgacpnobr if bribed==0, lcolor(red)), tline(2017) leg(row(1) order(1 "Bribed" 2 "Not Bribed")) yti(Anti-Corruption Policy) ti(Anti-corruption policy of countries involved in the Odebrecht scandal vs. not-involved) name(acp_parlltrendsassmpt1, replace)

********FURTHER STEPS*************

*** creating variable for number of people processed in the country

*** variable for odebrecht's profits per country

*****************************ESTIMATING

misstable pat acp bribed post wdi_fdiin wdi_gdpcappppcon2017 hf_govint wbgi_gee vdem_mecorrpt if ht_region==2

* wdi_gdpcappppcon2017 --> solo falta venz y cuba

*****recortando data
keep hf_govint year ccode ht_region
merge 1:1 ccode year using "C:\Users\maria\OneDrive - sciencespo.fr\Memoire\datasets_all\hf_govint_ccode_year.dta"

**********************
*probl: too many missing val for acp if we include SSA.
bys year: count if mi( acp ) & ht_region==4

****reg 2: todo latam
reg acp bribed post post##bribed wdi_fdiin wdi_gdpcappppcon2017 hf_govint wbgi_gee vdem_mecorrpt i.year i.ccode if ht_region==2
*N:144

**** reg 1 --> hf_govint + mozbq + angola
reg acp bribed post post##bribed wdi_fdiin wdi_gdpcappppcon2017 hf_govint wbgi_gee vdem_mecorrpt i.year i.ccode if ht_region==2|cname=="Angola"|cname=="Mozambique" , r
** N=159, yrs=2007,9,11,13,15,17,19


** reg 3: latam, no venezuela ni cuba
gen nodata= cond(cname=="Venezuela"|cname=="Cuba",1,0)
la var nodata "Venezuela or Cuba"

reg acp bribed post post##bribed wdi_fdiin wdi_gdpcappppcon2017 hf_govint wbgi_gee vdem_mecorrpt i.year i.ccode if ht_region==2 & nodata==0, r
*N=144

misstable pat acp bribed post wdi_fdiin wdi_gdpcappppcon2017 hf_govint wbgi_gee vdem_mecorrpt if ht_region==2|cname=="Angola"|cname=="Mozambique"
*** la razon por la que habia poco N en reg 3 es porque angola y mozbq no les faltan datos para varias variables

***checking missing values by year
misstable pat acp bribed post wdi_fdiin wdi_gdpcappppcon2017 hf_govint wbgi_gee vdem_mecorrpt bti_ij ffp_ps bti_ig bti_pdi if ht_region==2 & nodata==0

br acp bribed post wdi_fdiin wdi_gdpcappppcon2017 hf_govint wbgi_gee vdem_mecorrpt if ht_region==2 & nodata==0
* N:144 de toda mi dataset de latam sin cuba o venezuela. solo usa yrs=2005,7,9,11,13,15,17,19

outreg2 using "models.xls", replace ctitle(Model1)
****************Adding more controls:
** controls:
misstable pat if ht_region==2

* dr_eg --> solo falta cuba every year, y no 2019
* ffp_ps --> good public services
* bti_ig --> good interest groups
*bti_ij  --> good independent judiciary
* bti_pdi --> good performance of democratic institutions

xtline bti_ij if ht_region==2 & bribed==1, i(cname) t(year) tline(2017) name(btiij_panel_br, replace)
** no pattern defined for bribed countries
xtline bti_ij if ht_region==2 & bribed==0, i(cname) t(year) tline(2017) name(btiij_panel_nobr, replace)
*neither in no bribed
**gonna include it anyway too

xtline ffp_ps if ht_region==2 & bribed==1, i(cname) t(year) tline(2017) name(ffpps_panel_br, replace)
* if any, publ serv stayed the same of got worse, only got better in Vnzl and Brazil

xtline bti_ig if ht_region==2 & bribed==1, i(cname) t(year) tline(2017) name(btiig_panel_br, replace)
*gonna include it anyway bc important

xtline bti_pdi if ht_region==2 & bribed==1, i(cname) t(year) tline(2017) name(btipdi_panel_br, replace)
*no trend either, just 2 ctries improved. gonna include it anyway

xtline undp_hdi if ht_region==2 & bribed==1, i(cname) t(year) tline(2017) name(hi_panel_br, replace)

la def ctrylb 32 "Argentina" 68 "Bolivia" 76 "Brazil" 152 "Chile" 170 "Colombia" 188 "Costa Rica" 192 "Cuba" 214 "Dominican Republic" 218 "Ecuador" 222 "El Savlador" 320 "Guatemala" 332 "Haiti" 340 "Honduras" 484 "Mexico" 558 "Nicaragua" 591 "Panama" 600 "Paraguay" 604 "Peru" 858 "Uruguay" 862 "Venezuela" 24 "Angola" 508 "Mozambique"

la val ccode ctrylb

gen loggdp= log(wdi_gdpcappppcon2017 + 1)
*************************************
********* THIS IS IT*****************
*************************************

***checking missing values by year
misstable pat acp bribed post2016 wdi_fdiin loggdp hf_govint wbgi_gee vdem_mecorrpt bti_ij ffp_ps bti_ig bti_pdi if ht_region==2 & nodata==0
**no missing values -> good to go sample and controls

** reg1: controls in latam, no govef
reg acp bribed post did wdi_fdiin loggdp hf_govint vdem_mecorrpt bti_ij ffp_ps bti_ig bti_pdi

outreg2 using "models.xls", replace ctitle(All controls) adjr2

* significant controls: hf_govint wdi_fdiin vdem_mecorrpt bti_ij 

** reg 2: all controls+year dummies
reg acp bribed post post##bribed wdi_fdiin loggdp hf_govint  vdem_mecorrpt bti_ij ffp_ps bti_ig bti_pdi i.year

outreg2 using "models.xls", append ctitle(Year dummies) label adjr2


**reg 3: all controls+country dummies
reg acp bribed post post##bribed wdi_fdiin loggdp hf_govint  vdem_mecorrpt bti_ij ffp_ps bti_ig bti_pdi i.ccode

outreg2 using "models.xls", append ctitle(Country dummies) label adjr2


** reg 4: all controls + year dummies + country dummies
reg acp bribed post post##bribed wdi_fdiin loggdp hf_govint vdem_mecorrpt bti_ij ffp_ps bti_ig bti_pdi i.year i.ccode

outreg2 using "models.xls", append ctitle(Year + Country dummies) label adjr2 


** reg 5: all controls + year random effects
xtreg acp bribed post post##bribed wdi_fdiin loggdp hf_govint vdem_mecorrpt bti_ij ffp_ps bti_ig bti_pdi i.year, fe

estimates store FE

xtreg acp bribed post post##bribed wdi_fdiin loggdp hf_govint vdem_mecorrpt bti_ij ffp_ps bti_ig bti_pdi i.year, re

estimates store RE

outreg2 using "models.xls", append ctitle(Year random effects) label addtext(Year RE, YES)


hausman FE RE
* Under the chi2 calculated, we cannot reject the h0. If cannot reject h0: should opt for RE, even if you would also be allowed to use FE

**the using F statistic wald test, the coefficients are jointly statistically significant

** reg 5.2: latam but not Venezuela or Cuba, year and country dummies
reg acp bribed post post##bribed wdi_fdiin wdi_gdpcappppcon2017 hf_govint vdem_mecorrpt bti_ij ffp_ps bti_ig bti_pdi i.year i.ccode if nodata==0
* not change in observations, Cuba and Venezuela were already omitted


****************************************
***** Robustness checks*****************
*model 4 is the best so far
reg acp bribed post post##bribed wdi_fdiin loggdp hf_govint vdem_mecorrpt bti_ij ffp_ps bti_ig bti_pdi i.year i.ccode
vif

** multicollinearity btwn all except foreign invstm, vdem_mecorrpt

*** fixing vif
** no country dummies: model 2
reg acp bribed post post##bribed wdi_fdiin loggdp hf_govint vdem_mecorrpt bti_ij ffp_ps bti_ig bti_pdi i.year
vif
** getting rid of the country dummies fixes multicollinearity.  However, there's still collineairty in loggdp

** this makes sense, because if variations in my dataset are at country level and I have control variables, using country dummies takes out all the variation between countries, which I don't want. I would use it if my observations were at individual level, or respondents. 

** Model 6: year dummies+selected variables (no loggdp)
reg acp bribed post post##bribed wdi_fdiin hf_govint vdem_mecorrpt bti_ij ffp_ps bti_ig bti_pdi i.year
vif

outreg2 using "models.xls", append ctitle(year dummies + no gdp) label adjr2 

****** RESIDUALS
* Model 5: year random effects 
xtreg acp bribed post did wdi_fdiin loggdp hf_govint vdem_mecorrpt bti_ij ffp_ps bti_ig bti_pdi i.year, re

gen did= post*bribed

*Model 2: year dummies+all controls
reg acp bribed post post##bribed wdi_fdiin loggdp hf_govint  vdem_mecorrpt bti_ij ffp_ps bti_ig bti_pdi i.year

* Store the predicted values
cap drop yhat2
predict yhat2

* We store the unstandardized residuals.
cap drop res2
predict res2, resid

*Model 5: resids 
* First, with a kernel density plot
kdensity r, norm legend(off) ti("Distribution of Residuals of Model 5") name(diag_kdens, replace)
* residuals skewed to the right, positively skewed.

hist r, normal percent bin(15) name(resid_hist_mod5, replace)

* Model 2: predicted residuals:
kdensity res2, norm legend(off) ti("Distribution of Residuals of Model 2") name(diag_kdens_mod2, replace)
**** very normally distributed

** We plot the unstandardized residuals versus fitted values
sc res2 yhat2, yline(0) ms(i) mlab(ccodealp) name(resd_sc, replace)
* or we can use another command for the same result: residuals vs adjusted values
rvfplot, yline(0) ms(i) mlab(ccodealp) name(rvf_plot_mod2, replace)

**** To check the presence of outliers, we use standardized residuals.
cap drop rsta2
predict rsta2, rsta

* Now we can highlight the outlier countries beyond 2 standard deviations.
sc rsta2 yhat2, yline(-2 2) || sc rsta2 yhat2 if abs(rsta2) > 2, ylab(-3(1)3) mlab(ccodealp) legend(lab(2 "Outliers")) name(diag_rsta_mod2, replace)
** Brazil is the only big outlier. Probably because of the large amounts of money stolen there.

*** Model 7
reg acp amountbr post post##c.amountbr wdi_fdiin loggdp hf_govint  vdem_mecorrpt bti_ij ffp_ps bti_ig bti_pdi i.year

outreg2 using "models.xls", append ctitle(Amount bribes) label adjr2  addtext(Year RE, NO) 

*********
*latam + Mozambique and Angola

|cname=="Angola"|cname=="Mozambique" , r

***adjustment for information of peru
gen amountbr2= amountbr
replace amountbr2 =45 if cname =="Peru"


*** Tabla comparativa
bys bribed: tabstat acp wdi_fdiin hf_govint vdem_mecorrpt ffp_ps bti_pdi diat_ti wdi_gdpcappppcon2017 undp_hdi sgi_qd wdi_popden ictd_revres if year==2005|year==2007|year==2009|year==2011|year==2013|year==2015, s(n mean  median sd min max)

ttest hf_govint, by(bribed)
* reject h0, diff is significant
ttest wdi_gdpcappppcon2017, by(bribed) 
* reject h0, diff is significant
ttest vdem_mecorrpt, by(bribed) 
* reject h0, diff is significant

ttest undp_hdi, by(bribed) 
* reject h0, diff is significant

ttest ffp_ps, by(bribed) 
* cannot reject h0, diff is not significant

ttest ictd_revres, by(bribed) 
* cannot reject h0, diff is not significant

***************************************
*** testing for spillover effects
ttest acp if bribed==0, by (post2016)
* difference observed pre and post 2016 is not statistically significant, therefore, no spillovereffects

***********************
** testing for differences at baseline
ttest acp if year==2005, by(bribed) 
* cannot reject h0, diff is not significant

*** measuring the diff pre post in bribed countries
ttest acp if bribed==1, by (post2016)

ttest acp if cname=="Ecuador"|cname=="Argentina"|cname=="Brasil"|cname=="Peru"|cname=="Colombia", by (post2016)
