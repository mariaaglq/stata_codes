** Tema: Análisis de línea de base - Pasco
** Autor: Maria Aguilar
** Org: CHIRAPAQ
** Stata 17.0

		****************************************
		*** Seccion 3: Cultivos ***
		****************************************	
clear all
cap log close		
set more off

global root 		"C:\Users\maria\Documents\Chirapaq\Linea de base-Pasco\stata_LBPasco_final"
global data 		"$root\data"
global dofiles 		"$root\dofiles"
global tables   	"$root\output"

cd "$root"

* Assembling data set
import excel "$root\data\BD_pasco_familias_final.xlsx", sheet("cultivo") cellrange(A1:DG49) firstrow

egen cultests3=rowtotal(crt1_supf crt3_supf crt4_supf hv1_supf hv2_supf ls1_supf ls2_supf)

egen cultperms3=rowtotal(crt2_supf f1_supf f2_supf f3_supf)


merge 1:1 codfam using "$data\lbpasco_sociodem_byfam", keepusing(distrfam comddfam sectfam)
	drop _merge //all 48 merged
	
	*save "$data\lbpasco_cultivos", replace
	
	u "$data\lbpasco_cultivos2", clear
	
merge 1:1 codfam using "$data\lbpasco_cultimp"
drop _merge //all 48 merged

	 /// CLEANING & FIXING STUFF ///
	rename crt1_prodanual crt1_prod
	replace crt1="1" if !mi(crt1)
	destring crt1, replace
	la def cultivoslb 1 "yuca"
	la val crt1 cultivoslb	
	
	*crt4
	replace crt4_prod="" if crt4_prod=="NS"|crt4_prod=="para autoconsumo"
	destring crt4_prod, replace
	
	rename crt4 crt4b
	gen crt4= cond(!mi(crt4b),2,.)
	replace crt4=3 if crt4b=="camote"
	la def cultivoslb 2 "pituca" 3 "camote",add
	la val crt4 cultivoslb
	drop crt4b
	
	* ls2
	replace ls2_prod="350" if ls2_prod=="7 sacos"
	destring ls2_prod, replace

* Crear variable que cuenta n° especies cultivadas por fam
	egen ncultivos = rownonmiss(crt1 crt2 crt3 crt4 hv1 hv2 ls1 ls2 f1 f2 f3),strok
	la var ncultivos "N° de cultivos sembrados por familia"
	
	egen nfrutas = rownonmiss(f1 f2 f3),strok
	la var nfrutas "N° de frutas sembradas por familia"
	
** Porcentaje ocupado el terreno total por c/cultivo
	egen tsemb= rowtotal(crt?_supf hv?_supf ls?_supf f?_supf)
	la var tsemb "Area total sembrada"
	gen crt1_perc= 100*crt1_supf/tsemb
	gen crt2_perc= 100*crt2_supf/tsemb
	gen crt3_perc= 100*crt3_supf/tsemb
	gen crt4_perc= 100*crt4_supf/tsemb
	gen hv1_perc = 100*hv1_supf/tsemb
	gen hv2_perc = 100*hv2_supf/tsemb
	gen hv3_perc = 100*hv3_supf/tsemb
	gen ls1_perc = 100*ls1_supf/tsemb
	gen ls2_perc = 100*ls2_supf/tsemb
	gen f1_perc  = 100*f1_supf/tsemb
	gen f3_perc  = 100*f3_supf/tsemb
	
	format crt?_perc hv?_perc ls?_perc f?_perc %5.2f
	
/// ANALYSIS ///	
* 1 sola categoria
table (), statistic(mean crt1_supf crt1_prod crt2_supf crt2_prod crt3_supf crt3_prod hv1_supf hv1_prod ls1_supf ls1_prod ls2_supf) ///
		statistic(count crt1_supf crt1_prod crt2_supf crt2_prod crt3_supf crt3_prod hv1_supf hv1_prod ls1_supf ls1_prod ls2_supf) ///
		statistic(min crt1_supf crt1_prod crt2_supf crt2_prod crt3_supf crt3_prod hv1_supf hv1_prod ls1_supf ls1_prod ls2_supf) ///
		statistic(max crt1_supf crt1_prod crt2_supf crt2_prod crt3_supf crt3_prod hv1_supf hv1_prod ls1_supf ls1_prod ls2_supf) ///
		statistic(sd crt1_supf crt1_prod crt2_supf crt2_prod crt3_supf crt3_prod hv1_supf hv1_prod ls1_supf ls1_prod ls2_supf) ///
		nformat(%6.2g mean) ///
		nformat(%6.2g sd) ///
		sformat("(%s)" count)
		
	collect label levels result ///
		count "N° familias", modify
	collect preview
	collect export "$root\output\tablespa.xlsx", ///
	sheet(cultivos) cell(A1) modify

	* f1
	table (), stat(mean f1_supf f1_racimo_prod) ///
		stat(count f1_supf f1_racimo_prod) ///
		stat(min f1_supf f1_racimo_prod) ///
		stat(max f1_supf f1_racimo_prod) ///
		stat(median f1_supf f1_racimo_prod) ///
		stat(sd f1_supf f1_racimo_prod) ///
		nformat(%6.2g mean) ///
		nformat(%6.2g sd) ///
		sformat("(%s)" count)
	collect export "$root\output\tablespa.xlsx", ///
	sheet(cultivos) cell(A62) modify
	
*varias categorias
		table (crt4) (), ///
			stat(mean crt4_supf crt4_prod) ///
			stat(frequency) ///
			stat(min crt4_supf crt4_prod) ///
			stat(max crt4_supf crt4_prod) ///
			stat(sd crt4_supf crt4_prod) ///
			nformat(%6.2g mean) ///
			sformat("(%s)" frequency) ///
			nformat(%6.2g sd) ///
			totals(crt4)
		collect label levels result ///
			frequency "N" ///
			min "Min" ///
			max "Max" ///
			sd "Desv.Est." ///
			mean "Promedio", modify
	collect export "$root\output\tablespa.xlsx", ///
	sheet(cultivos) cell(D1) modify
	
	*** next D7
	* crt1 yuca
	* crt2 cacao
	* crt3 cafe
	*Crt4 pituca o camote
	*Hv1 Maíz
	*Hv2 Pepino, tomate, albahaca
	*Ls1 frejol
	*Ls2 mani
	*F1	platano
*F2	Piña, maracuyá, palta, caimito, naranja, limon, platano
*F3	Papaya,piña, naranja, sandia, pacay

f2_plantas f2_prodkg f3_supf f3_prodkg

* supf
	tabstat hv2_supf, s(n mean sd min max p50) ///
	by(hv2) format(%9.1g)
	tabstat f2_plantas, s(n mean sd min max p50) ///
	by(f2) format(%9.1g)
	tabstat f3_supf, s(n mean sd min max p50) ///
	by(f3)	format(%9.1g)
*prod		
	tabstat hv2_prod, s(n mean sd min max) by(hv2) ///
		format(%9.1g)
	tabstat f2_prodkg, s(n mean sd min max) by(f2) ///
		format(%9.1g)
	tabstat f3_prodkg, s(n mean sd min max) by(f3) ///
		format(%9.1g)

* platano: 29
* maiz: 22

	
	* Policultivo
	sum ncultivos,d
br crt1 crt2 crt3 crt4 hv1 hv2 hv3 ls1 ls2 f1 f2 f3 
ncultivos if ncultivos==1

fre ncultivos if !mi(crt3)|!mi(crt2)

fre nfrutas
sum nfrutas,d

	* Rendimiento
	gen crt2_rend = crt2_prod/crt2_supf
	gen crt3_rend = crt3_prod/crt3_supf

** Espacio ocupado por cultivo del total cultivado
	sum *_perc
	tabstat *_perc, stat(max med)
	count if crt1_perc>60 & !mi(crt1_perc)
	count if crt2_perc>60 & !mi(crt2_perc)
	count if crt3_perc>60 & !mi(crt3_perc)
	count if crt4_perc>60 & !mi(crt4_perc)
	count if hv1_perc>60 & !mi(hv1_perc)
	count if hv2_perc>60 & !mi(hv2_perc)
	count if hv3_perc>60 & !mi(hv3_perc)
	count if ls1_perc>60 & !mi(ls1_perc)
	count if ls2_perc>60 & !mi(ls2_perc)
	count if f1_perc>60 & !mi(f1_perc)
	count if f3_perc>60 & !mi(f3_perc)
*
br	crt1 crt2 crt3 crt4 hv1 hv2 hv3 ls1 ls2 f1 f2 f3 cultimp?
	
	
	
		****************************************
		*** Seccion 5: Ingresos ***
		****************************************	
		
* Assembling data set
import excel "$root\data\BD_pasco_familias_final.xlsx", sheet("ingr") cellrange(A1:R49) firstrow		

merge 1:1 codfam using "$data\lbpasco_sociodem_byfam", keepusing(distrfam comddfam sectfam sizefjunta)
	drop _merge //all 48 merged	
	
	*save "$data\lbpasco_ingr", replace
	u "$data\lbpasco_ingr", clear

merge 1:1 codfam using "$data\lbpasco_sociodem_byfam", keepusing(edad estud sexo)
drop _merge
la var edad "Edad del jefe de familia (años)"
la var sexo "Sexo del jefe de familia"
	
	//CLEANING //

drop ingrnoagranual
sumar ingragranual + ingrnoagr1anual + ingrnoagr2
compararlo a ingrfam1_anual

ingrfam1_anual (antiguo total)
ingragranual
ingrnoagr1 (puede ser mes o anual, ver fre)
crear un ingrnoagr1anual
sumarlo al ingrnoagr2 (solo 1 obs,es anual)

gen ingrnoagr1anual= ingrnoagr1 * 12 if ingrnoagr1_fre=="mes"|ingrnoagr1_fre=="mensual"
replace ingrnoagr1anual = ingrnoagr1 if ingrnoagr1_fre=="anual"
replace ingrnoagr1anual = (ingrnoagr1*2) if ingrnoagr1_fre=="6 meses"
replace ingrnoagr1anual = (ingrnoagr1*3) if ingrnoagr1_fre=="ocasional"
replace ingrnoagr1anual=ingrnoagr1anual*3 if ingrnoagr1_encar=="cada miembro" //porque en esa familia habia 3 hijos que viven en la comunidad y probablemente realizan la actvidad que menciona la madre
replace ingrnoagr1anual=0 if ingrnoagr1anual==. 
replace ingragranual=0 if ingragranual==.
replace ingrnoagr2=0 if ingrnoagr2==.
replace ingrfam1_anual=0 if ingrfam1_anual==.

gen ingrsubtot = ingragranual + ingrnoagr1anual + ingrnoagr2
count if ingrfam1_anual>ingrsubtot
gen ingrtot = ingrsubtot if ingrfam1_anual==.|ingrfam1_anual==0
replace ingrtot = ingrfam1_anual if ingrfam1_anual>ingrsubtot
replace ingrtot=. if ingrtot==0

**
gen ingrtmens = ingrtot/12
gen ingrbymemb = ingrtmens/sizefjunta
la var ingrbymemb "Ingreso mensual per capita (soles)"

sort codfam
gen id=_n

//ANALYSIS//
tabstat ingrtot, s(n mean sd min max p50) ///
	by(comddfam) format(%9.1g)
tabstat ingrbymemb, s(n mean sd min max p50) ///
	by(comddfam) format(%9.1g)

* Pobreza y pobreza extrema
count if ingrbymemb<354 & !mi(ingrbymemb) // pobreza
count if ingrbymemb<184 & !mi(ingrbymemb) // pobr extr
	
	histogram ingrbymemb, freq bin(15) scheme(white_tableau) xline(184, lcolor(red)) xline(354, lcolor(blue)) name(histingrpc,replace) note("Nota: Linea de la pobreza monetaria (azul) y pobreza monetaria extrema (roja). INEI 2021")
	

* Pobreza y factores
scatter ingrbymemb id // solo se ven 3 outliers


scatter ingrbymemb edad if ingrbymemb<500 || lfit ingrbymemb edad, scheme(white_tableau) name(scingrpc_edad,replace)

scatter ingrbymemb sizefjunta if ingrbymemb<500, scheme(white_tableau) name(scingrpc_sizef,replace)	

* 
sum ingrtmens,d
sum ingrtmens if id==16|id==19|id==20|id==36 // transporte
fre ingrtmens if ingrnoagr1_det=="jornal"
sum ingrtmens if ingrnoagr1_det=="jornal" & ingrtmens>60
sum ingrtmens if id==10|id==12|id==18

* correlations
pwcorr ingrbymemb edad, star(5)
pwcorr ingrbymemb sizefjunta, star(5)

* ttest
ttest ingrbymemb, by(sexo) //cannot reject h0, no hay diferencia significativa en ingresos de casas lideradas por hombres vs mujeres