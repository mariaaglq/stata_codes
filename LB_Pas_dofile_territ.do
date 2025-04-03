** Tema: Análisis de línea de base - Pasco
** Autor: Maria Aguilar
** Org: CHIRAPAQ
** Stata 17.0

		****************************************
		*** Seccion 2: Territorio ***
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
* import excel "$root\data\BD_pasco_familias_final.xlsx", sheet("territ") cellrange(A1:BJ49) firstrow

	merge 1:1 codfam using "$data\lbpasco_sociodem_byfam", keepusing(distrfam comddfam sectfam sexo sizefjunta)
	drop _merge //all 48 merged
	
	*save "$data\lbpasco_territ", replace
	
	u "$data\lbpasco_territ", clear
	
/// LABELING & PROCESSING ///	
	
** Usos de Superficie
	
	/* verificamos si la variable supftot es realmente el total
	gen supftot2 =bossup_pcl + passup_pcl + cultpermsup_pcl + cultestsup_pcl + otrosup_pcl
	gen igual = cond(supftot==supftot2,1,0)
	drop supftot2 igual
	*/
	
	replace otrosup_pcl=0 if otrosup_pcl==.
	replace bossup_pcl=0 if bossup_pcl==.
	replace passup_pcl=0 if passup_pcl==.
	
** Seccion3 import
	merge 1:1 codfam using "$data\lbpasco_cultivos", keepusing(cultests3 cultperms3)
	drop _merge //48 merged
	
	* Fixing Cult estacionales y permanentes
	count if cultests3==cultestsup_pcl
	
	* fixing missing cultestsup_pcl
	replace cultestsup_pcl=cultests3 if cultestsup_pcl==.
	replace cultpermsup_pcl=cultperms3 if cultpermsup_pcl==.
	
* Nuevo total superficie
	egen supftot4 = rowtotal(bossup_pcl passup_pcl cultpermsup_pcl cultestsup_pcl otrosup_pcl)
	la var supftot4 "Superficie de la UA (has)"
	
	la var bossup_pcl "Bosques" 
	la var passup_pcl "Pasturas"
	la var cultpermsup_pcl "Cult.Permanentes"
	la var cultestsup_pcl "Cult. Estacionales"
	la var otrosup_pcl "Otros"

	gen bosperc = 100*bossup_pcl/supftot4
	
* N° de parcelas	
	replace nparcelas=1 if nparcelas==0 & supftot4>0
	
* Superficie para siembra
	gen supfsiembra= cultpermsup_pcl + cultestsup_pcl
	la var supfsiembra "Superf. para cultivos"
	
	gen siembperc= 100*supfsiembra/supftot4

* Indice de fragmentación de la propiedad
	gen fragm = supftot4/nparcelas
	gen fragmcat = cond(fragm<1,0,2)
	replace fragmcat=1 if fragm==1
	la def fragmla 0 "Menos 1Ha/parcela" 1 "1Ha c/parcela" 2 "Más 1Ha/parcela" 
	la val fragmcat fragmla
	
* Agua de riego: fag*
	fre fag*
	la var faglluvia "Riego: lluvia"
	la var fagmanant "Riego: agua de manantial, puquio o quebrada"
	gen fagreserv= (fagmanant=="pequeños reservorios")
	replace fagman="" if fagreserv==1
	la var fagreserv "Riego: reservorios"
		
	replace faglluvia=0 if mi(riego1)
	replace fagmanant=0 if mi(riego2)
	replace faglluvia="1" if faglluvia=="lluvia"
	replace fagmanant="1" if fagmanant=="Manantial o puquio o quebrada"
	destring fagmanant faglluvia,replace
	replace faglluvia=0 if mi(faglluvia)
	replace fagmanant=0 if mi(fagmanant)
	egen fag_n = rowtotal(faglluvia fagmanant fagreserv)
	
* Problemas al cultivar: prob*
	replace probplagas="1" if !mi(probplagas)
	replace probsuelos="1" if !mi(probsuelos)
	replace probclim="1" if !mi(probclim)
	
	la var probsuelos "Degradación de suelos"
	la var probclim "Eventos climáticos adversos"
	la var probplagas "Plagas y enfermedades"
	
	drop probprod_num
	egen probprod_n = rownonmiss(probsuelos probclim probplagas probotros), strok
	
	destring probplagas probclim probsuelos,replace
	
* Razones de cultivos principales: raz_*
	gen raz_clima= (razimp1=="se adapta mejor al clima"|razimp2=="se adapta mejor al clima"|razimp3=="se adapta mejor al clima")
	gen raz_consumo=(razimp3=="otro: consumo")
	gen raz_venta = (razimp1=="buenos precios de venta")
	gen raz_barato = (razimp1=="cuesta menos dinero producirlo"|razimp2=="cuesta menos dinero producirlo"|razimp3=="cuesta menos dinero producirlo")
	gen raz_agua= (razimp2=="No usa mucha agua"|razimp2=="no usa mucha agua")
	gen raz_rapido=(razimp2=="menor tiempo para cosecha"|razimp1=="menor tiempo para la cosecha"|razimp3=="menor tiempo para la cosecha")
	
	egen razcult_n = rowtotal(raz_*)
	replace razcult_n=6 if codfam=="PAVRPI004"
	la var razcult_n "N° razones para elegir cultivos principales"
	
* MAF Manejo AgroForestal
	replace maf1="tornillo" if maf1=="Tornillo"

* Manejo de pastos MASP
	drop masp1_prod
	
	* verify no more bosque than total land
	br masp?_area supftot4 if ///
	masp1_area > supftot4 & !mi(masp1_area)
	
	/* if fixing needed:
	replace bossup_pcl=11 if masp1_area==11
	drop supftot4
	*recalcular supftot4
	egen supftot4 = rowtotal(bossup_pcl passup_pcl cultpermsup_pcl cultestsup_pcl otrosup_pcl)
	*/
	
* Abono
	drop abono_det
	replace abono_com= "" if abono_com=="ninguno"
	replace abono_cons= "" if abono_cons=="ninguno"
	
	* Asociación de cultivos: ascult_*
		
	gen ascult_cafe = (asocult1=="cafe"|asocult2=="cafe")
	gen ascult_cacao=(asocult2=="cacao"|asocult3=="cacao")
	gen ascult_plat=(asocult1=="platano"|asocult2=="platano"|asocult3=="platano")
	gen ascult_yuca=(asocult1=="yuca"|asocult2=="yuca")
	gen ascult_maiz=(asocult2=="maiz"|asocult3=="maiz"|asocult4=="maiz")
	gen ascult_pitu=(asocult2=="pituca")
	gen ascult_palt=(asocult3=="palta")
	gen ascult_refo=(asocult3=="arboles forestales(Pino, ulcumano)"|asocult4=="reforestacion"|asocult3=="ulcumano")
	
	la var ascult_cacao "Cacao"
	la var ascult_cafe "Cafe"
	la var ascult_maiz "Maiz"
	la var ascult_palt "Palta"
	la var ascult_pitu "Pituca"
	la var ascult_plat "Platano"
	la var ascult_refo "Arboles forestales"
	la var ascult_yuca "Yuca"
	drop asocult?
			
	egen asocult_n = rowtotal(ascult_*)
	la var asocult_n "N° de cultivos asociados"
		
	* Siembra diversificada: div_*
	rename siemdiv3 div_otro
	
	
	* Control de plagas
	replace contpla_com="" if contpla_com=="ninguno"
	
	* Rotacion: rotacult?
	rename rotacult1 rotacult_fre
	gen rotacult1 = (rotacult_fre=="yuca")
	la def rotacult1lb 1 "yuca"
	la val rotacult1 rotacult1lb
	
	* Señas
	fre senas1
	replace senas1="" if senas1=="no"
	
	* Mitigacion clima adverso
	fre mitigacion_clima_adverso 
	
	
		*******************************
		********* ANALYSIS ************
		*******************************	
				
putexcel set "$root\output\tablespa.xlsx", sheet(territ) modify
	
* Superficie
	* Extensión total de la chacra
	sum supftot4,d
	histogram supftot4, freq scheme(white_tableau) name(supftot_hist,replace) bin(12) xlabel(0(10)80, grid) //fig6
	
	tabstat supftot4, by(distrfam) stats(n mean sd min max med) format(%9.3g)
	tabstat supftot4, by(comddfam) stats(n mean sd min max med) format(%9.3g)

	
	table (comddfam) (), statistic(frequency) ///
		statistic(mean supftot4) ///
		statistic(sd supftot4) ///
		statistic(median supftot4) ///
		statistic(min supftot4) ///
		statistic(max supftot4) ///
		nformat(%6.2g mean) ///
		nformat(%6.2g min) ///
		nformat(%6.2g max) ///
		nformat(%6.2g max) ///
		nformat(%6.2g sd) ///
		sformat("(%s)" frequency)
	collect label levels result ///
		frequency "N° familias" ///
		min "Min" ///
		max "Max" ///
		sd "Desv.Est" , modify
		
	collect preview
	collect export "$root\output\tablespa.xlsx", ///
	sheet(territ) cell(A2) modify //tab9
		
	gr hbox supftot4, over(comddfam) yti("Superficie de la UA (Has)") scheme(white_tableau) name(supftot_box,replace)
		
* Extension de Bosques
	gr hbox bossup_pcl, over(comddfam) yti("Superficie ocupada por bosques (Has)") scheme(white_tableau) name(bossupf_box,replace) 
	
	gr combine supftot_box bossupf_box, ycommon ///
	name(combinedsupf, replace) ysize(5) xsize(13)
	
	sum bosperc,d
	bys comddfam: sum bosperc
	
	* Superficie para siembra
	bys comddfam: sum supfsiembra
	
	gr hbox supfsiembra, over(comddfam) yti("Superficie cultivada (Has) en la UA") scheme(white_tableau) name(cultsupf_box,replace) 
	
	table (comddfam) (), statistic(frequency) ///
		statistic(mean supfsiembra) ///
		statistic(sd supfsiembra) ///
		statistic(median supfsiembra) ///
		statistic(min supfsiembra) ///
		statistic(max supfsiembra) ///
		nformat(%6.2g mean) ///
		nformat(%6.2g min) ///
		nformat(%6.2g max) ///
		nformat(%6.2g max) ///
		nformat(%6.2g sd) ///
		sformat("(%s)" frequency)
	collect label levels result ///
		frequency "N° familias" ///
		min "Min" ///
		max "Max" ///
		sd "Desv.Est" , modify
		
	collect preview
	collect export "$root\output\tablespa.xlsx", ///
	sheet(territ) cell(A41) modify //tab10
	
* Usos de superficie
	graph pie bossup_pcl passup_pcl ///
	cultpermsup_pcl cultestsup_pcl otrosup_pcl, ///
	plabel(_all percent, format(%4.1g)) ///
	leg(rows(1)) ///
	by(comddfam) scheme(burd6) ///
	name(pie_usossupf,replace) //fig7
	
	graph dot (mean) bosperc siembperc, ///
	over(comddfam) ///
	marker(1, mcolor(green) msymbol(Oh)) ///
	marker(2, mcolor(orange) msymbol(T)) ///
	name(dotmean_percsupf, replace) 
	
	graph dot (median) bosperc siembperc, ///
	over(comddfam) yti("Superficie (% de la UA)") ///
	marker(1, mcolor(green) msymbol(D)) ///
	marker(2, mcolor(orange) msymbol(T)) ///
	scheme(white_tableau) ///
	leg(r(1) pos(6) label(1 "Mediana de supf. bosque") label(2 "Mediana de supf. sembrada")) ///
	name(dotp50_percsupf, replace) //fig8
		
*N° parcelas
	sum nparcelas, d
	fre nparcelas
	* Por comunidad
		tabstat nparcelas, by(comddfam) ///
		stats(n min max med) format(%9.3g)

		table (comddfam) (), statistic(frequency) ///
		statistic(mean nparcelas) ///
		statistic(min nparcelas) ///
		statistic(max nparcelas) ///
		nformat(%6.2g mean) ///
		nformat(%6.2g min) ///
		nformat(%6.2g max) ///
		sformat("(%s)" frequency)
	collect label levels result ///
		frequency "N° familias", modify
	collect preview
	collect export "$root\output\tablespa.xlsx", ///
	sheet(territ) cell(A12) modify

* Fragmentacion
	tab comddfam fragmcat
	table (comddfam) (fragmcat), ///
		statistic(frequency) ///
		totals(fragmcat)
	collect export "$root\output\tablespa.xlsx", ///
	sheet(territ) cell(A22) modify

* Principales problemas al cultivar 
probplagas probclim probsuelos probotros prob_det
	fre prob*
			
	table (probprod_n) (probplagas), totals(probplagas)
	collect export "$root\output\tablespa.xlsx", ///
	sheet(territ) cell(A33) modify
	table (probprod_n) (probclim), totals(probclim)
	collect export "$root\output\tablespa.xlsx", ///
	sheet(territ) cell(C33) modify
	table (probprod_n) (probsuelos), totals(probsuelos)
	collect export "$root\output\tablespa.xlsx", ///
	sheet(territ) cell(E33) modify
		
	* Control de plagas
	br contpla* if !mi(contpla_com)|!mi(contpla_det)
	fre contpla_com contpla_det
	
	* Abono
	br abono_* if !mi(abono_com)|!mi(abono_cons)
	
	* Mitigacion de clima adverso, solo hay 2 rptas
	fre mitigacion_clima_adverso

	* Rotacion de cultivos
	fre rotacult?
	br codfam rotacult? if rotacult1==1|!mi(rotacult2)|!mi(rotacult3)|!mi(rotacult4)
	
	* Siembra diversificada
	fre div_* 
	br codfam div_* if !mi(div_yuca1)|!mi(div_yuca2)|!mi(div_plat1)|!mi(div_pla2)|!mi(div_pla3)|!mi(div_maiz)|!mi(div_otro)
	
	* Asociacion de cultivos
	fre asocult_n
	br asocult_n ascult* if ascult_cafe==1|ascult_cacao==1| ascult_plat==1|ascult_yuca==1|ascult_maiz==1|ascult_pitu==1|ascult_palt==1|ascult_refo==1
	tab asocult_n ascult_cafe
	tab asocult_n ascult_cacao
	tab asocult_n ascult_plat
	tab asocult_n ascult_yuca
	tab asocult_n ascult_maiz
	tab asocult_n ascult_pitu
	tab asocult_n ascult_palt
	tab asocult_n ascult_refo
	
	* Semilla tecnicas
	fre semillas_tecnicas
	
	* Senas
	br senas1 if !mi(senas1)
	fre senas1
	tab senas1 comddfam

* Fuentes de agua
	fre faglluvia fagmanant fagreserv
	tab fag_n faglluvia
	tab fag_n fagmanant
	tab fag_n fagreserv
	br codfam fag* if !mi(faglluvia)|!mi(fagmanant)|!mi(fagreserv)
	
	* Cultivos importantes
	fre cultimp1 cultimp2 cultimp3 cultimp4
		
	*raz*
	fre raz_consumo raz_venta raz_barato raz_agua raz_rapido
	tab razcult_n raz_venta
	tab razcult_n raz_rapido
	tab razcult_n raz_barato
	tab razcult_n raz_agua
	tab razcult_n raz_consumo
	
	*masp
	fre masp*
	br codfam passup_pcl masp* if passup_pcl>0|!mi(masp1)
	sum passup_pcl if passup_pcl>0, d
		
	*maf
	fre maf*
	
* FIX !!!!!
	* fix supftot4 que tenia q ser 30, pero como estaban vacios quedaron como 0
	br codfam supftot4 totsup_pcl
	* arreglar comdd_pcl --> usable xq es diff que comddfam
	
	
	
	
	
	
	
	
	
	
	
	