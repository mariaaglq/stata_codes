** Tema: Análisis de línea de base - Pasco
** Autor: Maria Aguilar
** Org: CHIRAPAQ
** Stata 17.0

*seccion 6
*replace =. if "no"

		****************************************
		*** Seccion 1: Datos Sociodemográficos ***
		****************************************	
*by indiv

clear all
cap log close		
set more off

global root 		"C:\Users\maria\Documents\Chirapaq\Linea de base-Pasco\stata_LBPasco_final"
global data 		"$root\data"
global dofiles 		"$root\dofiles"
global tables   	"$root\output"

cd "$root"

* Assembling data set
import excel "$root\data\BD_pasco_familias_final.xlsx", sheet("sociodem") cellrange(A1:U259) firstrow
	
	*save "$data\lbpasco_sociodem_byind", replace
	
	*u "$data\lbpasco_sociodem_byind", clear

/// LABELING & PROCESSING ///
	drop muac
	la def sinolab 1 "Si" 0 "No"
	
	** Contar Familias
	sort codfam
	by codfam : gen ordenenfam=_n
	by codfam : gen sizefam2 = _N
	la var sizefam2 "N° miembros de la familia"
	
	bysort codfam: gen dup_hh=cond(_N==1,0,_n)
	* al tabear,solo contamos 1 duplicado "if dup_hh<=1"
	
	
	* Ubicacion
	replace distrfam = "1" if distrfam=="Palcazú"
	replace distrfam = "2" if distrfam=="Villa Rica"
	destring distrfam, replace
	la def distrlb 1 "Palcazú" 2 "Villa Rica"
	la val distrfam distrlb
	la var distrfam "Distrito del hogar principal"
	
	replace comddfam = "1" if comddfam=="Alto Iscozacin"
	replace comddfam = "2" if comddfam=="Azulis"
	replace comddfam = "2" if comddfam=="San Pedro de Pichanaz-Azulis"
	replace comddfam = "3" if comddfam=="Buenos Aires"
	replace comddfam = "4" if comddfam=="Shiringamazu"
	replace comddfam = "5" if comddfam=="Shiringamazu-Pueblo Libre"
	replace comddfam = "6" if comddfam=="Shiringamazu-San Luis"
	replace comddfam = "7" if comddfam=="Ñagazu"
	replace comddfam = "8" if comddfam=="San Pedro de Pichanaz"
	replace comddfam = "9" if comddfam=="Unión de la Selva"
	destring comddfam, replace
	la def sectlb 1 "Alto Iscozacin" ///
	2 "S.P.Pchz-Azulis" ///
	3 "Buenos Aires" 4 "Shiringamazu" ///
	5 "Shir.Pblo Libre" 6 "Shir.San Luis" 7 "Ñagazu" ///
	8 "S.P.Pchz-Pichanaz" 9 "Unión de la Selva"
	la val comddfam sectlb
		
	rename comddfam sectfam
	la var sectfam "Sector del hogar principal"
	gen comddfam = sectfam
	replace comddfam = 2 if sectfam==8
	replace comddfam = 4 if sectfam==5|sectfam==6
	
	la def comddlb 1 "Alto Iscozacin" ///
	2 "San Pedro de Pichanaz" 3 "Buenos Aires" ///
	4 "Shiringamazu" 7 "Ñagazu" ///
	9 "Unión de la Selva"
	la val comddfam comddlb
	la var comddfam "Comunidad del hogar principal"
	
	/*corrigiendo
	replace sectfam=6 if codfam=="PAPZSH005"
	replace comddfam_sect="San Luis" if codfam=="PAPZSH005"
		
	replace comddfam=2 if comddfam==9 //Azulis solo hay 1 y está en Palcazú
	replace distrfam=1 if comddfam==2
	replace codfam="PAVRPI006" if codfam=="PAPCAZ001"
	replace comddfam_sect="Azulis" if comddfam==2
	
	
	*I could adjust codfam but that may be unnecessary
	*replace codfam="PAPZPI005" if codfam="PAVRPI005" 
	*replace codfam="PAPZPI006" if codfam="PAVRPI006"
	probably not needed coz fixed in db excel */
	
	** Encoding relajef
	gen relajef_s = relajef
	replace relajef_s="1" if relajef== "jefe de hogar"
	replace relajef_s="2" if relajef== "jefa de hogar"
	replace relajef_s="3" if relajef=="esposa"
	replace relajef_s="4" if relajef=="hijo"|relajef=="hija"|relajef=="hijastro"|relajef=="hijastra"
	
	destring relajef_s, replace
	la def relajeflb 1 "jefe" 2 "jefa" 3 "esposa" 4 "hijo/hijastro"
	la val relajef_s relajeflb
	la var relajef_s "Relacion con jefe/a de familia"

* id
	sort codfam relajef_s
	gen id=_n
	
	* Estado civil
	replace estciv = "1" if estciv=="Soltero"|estciv=="soltero"
	replace estciv = "2" if estciv=="casado"
	replace estciv = "3" if estciv=="Conviviente"|estciv=="conviviente"
	replace estciv = "4" if estciv=="divorciado/separado"
	replace estciv = "5" if estciv=="viudo"
	replace estciv = "6" if estciv=="otro"
	destring  estciv, replace
	la def estcivlb 1 "Soltero" 2 "Casado" ///
	3 "Conviviente" 4 "Divorc./separado" 5 "Viudo" ///
	6 "Otro"
	la val estciv estcivlb
	la var estciv "Estado civil"
	
** Sexo
* convertimos la var a dummy de mujer
	gen sex =(sexo=="M")
	la def sexlab 0 "Hombre" 1 "Mujer"
	la val sex sexlab
	drop sexo
	rename sex sexo
	la var sexo "Sexo"

** Grupos de edad
	gen gredad=edad
	replace gredad=1 if edad<=5
	replace gredad=2 if edad>=6 & edad<=11
	replace gredad=3 if edad>=12 & edad<=17
	replace gredad=4 if edad>=18 & edad<=29
	replace gredad=5 if edad>=30 & edad<=44
	replace gredad=6 if edad>=45 & edad<=59
	replace gredad=7 if edad>=60 & edad !=.
	replace gredad=1 if mi(edad) //pq los mi(edad) tienen edad_meses
	la var gredad "Grupos de edad"
	la def gredadlb 1 "Primera infancia (0-5)" ///
		2"Infante (6-11)" ///
		3 "Adolescente(12-17)" ///
		4 "Joven (18-29)" ///
		5 "Adulto joven(30-44)" ///
		6 "Adulto (45-59)" ///
		7 "Anciano (60+)", replace
	la val gredad gredadlb
	
** Nivel de Estudios
	replace estud="inicial" if estud=="ninguno" & !mi(estud_det)
	replace estud="primaria completa" if (codfam=="PAVRUS008" & estud=="otro")
	
	replace estud = "0" if estud=="ninguno"
	replace estud = "1" if estud=="inicial"
	replace estud = "2" if estud=="primaria incompleta"
	replace estud = "3" if estud=="primaria completa"
	replace estud = "4" if estud=="secundaria incompleta"
	replace estud = "5" if estud=="secundaria completa"
	replace estud = "6" if estud=="tecnica"
	replace estud = "7" if estud=="superior"
	replace estud = "8" if estud=="otro"
	destring  estud, replace
	la def estudlb 0 "Ninguno" 1 "Inicial" ///
	2 "Prim. incompleta" 3 "Prim. completa" ///
	4 "Secund. incompleta" 5 "Secund. completa" ///
	6 "Técnica" 7 "Superior" 8 "otro"
	la val estud estudlb
	la var estud "Nivel de estudios"

** Lengua Materna
	
	encode lengmat, gen(lenguamat)
	drop lengmat
	rename lenguamat lengmat
	la var lengmat "Lengua materna"
	
	gen castell= (lengmat==2)
	la def castelb 1 "castellano" 0 "leng.indig"
	la val castell castelb
	la var castell "Castellano es lengua materna"

** Jefx de fam
	gen jefx=(relajef_s==1|relajef_s==2)
	la var jefx "Es jefe/a de familia"
	la val jefx sinolab

** Familia matriarcal
	gen jefa= (relajef_s==2)
	egen hayjefa=max(jefa), by(codfam)
	la var hayjefa "Familia matriarcal"
	la val hayjefa sinolab
	drop jefa
	
* Comunidad de residencia actual
	gen lugres2 = lugres
	replace lugres2 = "1" if lugres=="Alto Iscozacin"
	replace lugres2 = "2" if lugres=="Azulis"
	replace lugres2 = "2" if lugres=="San Pedro de Pichanaz-Azulis"
	replace lugres2 = "3" if lugres=="Buenos Aires"
	replace lugres2 = "4" if lugres=="Shiringamazu"
	replace lugres2 = "5" if lugres=="Shiringamazu-Pueblo Libre"
	replace lugres2 = "6" if lugres=="Shiringamazu-San Luis"
	replace lugres2 = "7" if lugres=="Ñagazu"
	replace lugres2 = "8" if lugres=="San Pedro de Pichanaz"
	replace lugres2 = "9" if lugres=="Unión de la Selva"
	replace lugres2 = "10" if lugres=="Iscozacin"|lugres=="La Cantuta"|lugres=="Villa Rica"|lugres=="Yunculmas"|lugres=="Oxapampa"
	replace lugres2 = "11" if lugres=="Chanchamayo"|lugres=="La merced "|lugres=="Sanchirio"
	replace lugres2 = "12" if lugres=="Huaraz"|lugres=="Pucallpa"
	replace lugres2 = "13" if lugres=="Lima"
	replace lugres2 = "14" if lugres=="Argentina"
	
	destring lugres2, replace
	la def lugreslb 1 "Alto Iscozacin" ///
	2 "S.P.Pchz-Azulis" 3 "Buenos Aires" ///
	4 "Shiringamazu" 5 "Shir.Pblo Libre" ///
	6 "Shir.San Luis" 7 "Ñagazu" ///
	8 "S.P.Pchz-Pichanaz" 9 "Unión de la Selva" ///
	10 "En prov. Oxapampa" ///
	11 "En Chanchamayo" 12 "Otros dptos" 13 "Lima" ///
	14 "Fuera del Perú"
	la val lugres2 lugreslb
	la var lugres2 "Lugar de residencia actual"
	
** Extensión de familias
		
	** Proxy : vive en comdd del jefe de hogar
	* 1)generar una var con el lugres del jefx de la familia
	gen lugjefx1= lugres if jefx==1
	* todos los no jefes serán missing
	* 2) asignar ese val del jefx a toda la fam
	bys codfam: egen lugjefx = mode(lugjefx1)
	la var lugjefx "Residencia del jefx de hogar"
	drop lugjefx1

	* 3) gen var que señala si la persona vive en 
	* el mismo lugar que el/la jefx
	gen samecomddjef=(lugjefx==lugres)
	rename samecomddjef miemhog
	la var miemhog "Es miembro del hogar"
	
	* Verificamos
	br codfam comddfam relajef lugres2 sizefam2 if miemhog==0 //all good to go
	
	egen nfamjunta=count(id) if miemhog==1, by(codfam)
	replace nfamjunta=0 if nfamjunta==.
	egen sizefjunta=max(nfamjunta), by(codfam)
	drop nfamjunta
	la var sizefjunta "N° integrantes de la familia que vive junta"
	
	** n° hijos
	gen hijx1 = cond(relajef_s==4,1,0)
	egen nhijx1=count(id) if hijx1==1, by(codfam)
	replace nhijx1=0 if nhijx1==.
	egen nhijx1nomiss=max(nhijx1), by(codfam)
	drop hijx1 nhijx1
	rename nhijx1nomiss nhijxs
	la var nhijxs "N° hijxs en la familia"
	
** n° hijos <18 que son miemhog
	gen hijx2 = cond(relajef_s==4 & edad<18 & miemhog==1,1,0)
	egen nhijx2=count(id) if hijx2==1, by(codfam)
	replace nhijx2=0 if nhijx2==.
	egen nhijx2nm=max(nhijx2), by(codfam)
	drop hijx2 nhijx2
	rename nhijx2nm nhijmen
	la var nhijmen "N° hijxs menores de 18 viviendo en casa"
	
* Population weights
	* frequency weights
	gen pop = 1
	replace pop = 20 if comddfam==1
	replace pop = 56 if comddfam==2
	replace pop = 43 if comddfam==3
	replace pop = 91 if comddfam==4
	replace pop = 99 if comddfam==7
	replace pop = 45 if comddfam==9
	
	* ponderacion
	gen comddwgt = 1
	replace comddwgt=(20/354)/(8/48) if comddfam==1
	replace comddwgt=(56/354)/(6/48) if comddfam==2
	replace comddwgt=(43/354)/(12/48) if comddfam==3
	replace comddwgt=(91/354)/(9/48) if comddfam==4
	replace comddwgt=(99/354)/(3/48) if comddfam==7
	replace comddwgt=(45/354)/(10/48) if comddfam==9

	// DATA ANALISIS //
	
* Declaring survey data
	svyset codfam [pw=comddwgt], strata(comddfam)

	* choosing weights
	tab comddfam if dup_hh<=1 [aweight=comddwgt]	
	tab comddfam if dup_hh<=1 [fweight=pop]
	tab comddfam if dup_hh<=1 [iweight=comddwgt] //uso aweight

** Población encuestada
	by distrfam, sort : tabulate comddfam //n° personas
	by distrfam, sort : tabulate comddfam if dup_hh<=1 //n° familias	
	
** Edad
* ------------
	* Grupos de edad
	tab gredad sexo if miemhog==1 & distrfam==1, sum(id) nomeans nostandard //palcazu
	tab gredad sexo if miemhog==1 & distrfam==2, sum(id) nomeans nostandard //villa rica
	tab gredad sexo if miemhog==1, sum(id) nomeans nostandard //para totales segun sexo
	fre gredad if miemhog==1 //para totales %
	
	histogram edad if miemhog==1, by(sexo) freq ///
	w(5) xlabel(#5) sch(burd6) name(hist_edad_sex2,replace) //fig1
	
	* Edades por comunidad
	tabstat edad if miemhog==1, by(comddfam) ///
	stat(n mean sd min max p50) format(%9.4g)
	
	tabstat edad if miemhog==1 [fw=pop], ///
	by(comddfam) stat(n mean sd min max p50) ///
	format(%9.4g) //inclusion de frequency weights no cambia mucho los promedios
	
	br comddfam edad_meses if !mi(edad_meses)
	
	table comddfam if miemhog==1, ///
		statistic(frequency) ///
		statistic(mean edad) ///
		statistic(sd edad) ///
		statistic(min edad) ///
		statistic(max edad) ///
		nformat(%6.2g mean) ///
		nformat(%6.2g sd)
	collect label levels result ///
			frequency "N° obs." ///
			sd "Desv. Est." ///
			min "Mínimo" ///
			max "Máximo" ///
			mean "Promedio", modify

	collect preview
	collect export "$root\output\tablespa.xlsx", ///
	sheet(familias) cell(A1) modify
		
	gr hbox edad if miemhog==1, over(comddfam, sort(1)des) yti("Edad de encuestados que residen juntos") name(edadbycomdd_box,replace)

** Tamaño de familia 
* -----------------------
	* n° de hijos
	table comddfam if dup_hh<=1, ///
		statistic(frequency) ///
		statistic(mean nhijxs) ///
		statistic(sd nhijxs) ///
		statistic(min nhijxs) ///
		statistic(max nhijxs) ///
		nformat(%6.2g mean) ///
		nformat(%6.2g sd)
		
		collect label levels result ///
			frequency "N° obs." ///
			sd "Desv. Est." ///
			min "Mínimo" ///
			max "Máximo" ///
			mean "Promedio", modify
		collect preview
		
	collect export "$root\output\tablespa.xlsx", ///
	sheet(familias) cell(A21) modify
	
	* N° hijos dependientes
	tabstat nhijmen if miemhog==1, by(comddfam) ///
	stat(n mean sd min max p50) format(%9.4g)
	
	* tamaño de familia
	tabstat sizefam2 if dup_hh<=1, by(comddfam) stat(n mean sd min max p50) format(%9.4g)	
	
	* familia que vive junta
	tabstat sizefjunta if dup_hh<=1, by(comddfam) ///
	stat(n mean sd min max p50) format(%9.4g) 
	
	table comddfam if dup_hh<=1, ///
		statistic(frequency) ///
		statistic(mean sizefjunta) ///
		statistic(sd sizefjunta) ///
		statistic(min sizefjunta) ///
		statistic(max sizefjunta) ///
		nformat(%6.2g mean) ///
		nformat(%6.2g sd)
		
		collect label levels result ///
			frequency "N° obs." ///
			sd "Desv. Est." ///
			min "Mínimo" ///
			max "Máximo" ///
			mean "Promedio", modify
		collect preview
		
	collect export "$root\output\tablespa.xlsx", ///
	sheet(familias) cell(A11) modify
	
	* Graph de la variable
	gr hbox sizefjunta if dup_hh<=1, over(comddfam, sort(1)des) yti("N° personas viviendo en el mismo hogar") scheme(white_tableau) name(fjuntasizebycomdd_box,replace)
	
** Emigración
	fre lugres2 if miemhog==0
	fre gredad if miemhog==0
	fre comddfam if miemhog==0
	fre sexo if miemhog==0
	
** Estado civil de JFs
* ---------------------
	tab estciv sexo if jefx==1
	*codfam=="PAPZSH005"|codfam=="PAVRPI002" son las mujeres que se declaran conviviente pero no lo enlistan en su familia.
	list codfam sizefam edad if relajef=="jefa" & (estciv==2|estciv==3)
	
	bys codfam: count if 
	br codfam relajef_s if relajef_s==1|relajef_s==3
	br if codfam=="PAPZBA011"|codfam=="PAVRUS003" //las convivientes pero sin pareja declarada
	br codfam edad sizefam2 sizefjunta nhijxs nhijmen  estciv if (relajef=="jefa de hogar") & (estciv==1|estciv==4|estciv==5|estciv==3) // ver jefas fam
	
** Nivel de educación
* -----------------------
	bys distrfam:tab estud sexo if jefx==1
	bys comddfam:tab estud sexo if edad>=18 & miemhog==1
	
	bysort sexo estud: gen N = _N
	bysort sexo : gen Na1 = (N/_N)*100
	by sexo : gen N1 = string(Na1,"%5.2f") +"%"
	drop N Na1
	
	spineplot estud sexo if edad>=18 & miemhog==1, ///
	xlabel(, angle(45) axis(2)) perc text(N1) ///
	scheme(white_tableau) name(spine_educ, replace)
	
	catplot estud, over(sexo) stack percent(sexo) /// 
	asyvars yla(0(25)100) blabel(bar, format(%4.1f) ///
	pos(base) color(white)) scheme(white_tableau) ///
	name(cat_estud_stack, replace)
  	
	catplot sexo estud if edad>=18 & miemhog==1, ///
	stack asyvars blabel(bar, format(%4.0f) ///
	pos(base) color(white)) scheme(white_tableau) ///
	name(cat_estud_stack3, replace)	//arrange overlapping labels, frecuencias
	
** Lengua materna
* ----------------------
	
	* Lengua materna de jefxs de familia
	tab comddfam lengmat if jefx==1
	tab sexo lengmat if jefx==1
		
	* Castellano vs leng.indig by gender (residentes)
	tab sexo castell if miemhog==1, cell
	
	graph pie if miemhog==1, over(castell) ///
	plabel(_all percent, format(%4.1f) size(*3)) ///
	leg(size(large)) by(sexo) scheme(white_tableau) ///
	name(lengmat_sex_cake,replace) 
	
	* Lengua materna by age
	tab gredad castell if miemhog==1
	
	catplot castell gredad if miemhog==1, ///
	stack asyvars blabel(bar, format(%4.0f) ///
	pos(base) color(white)) scheme(white_tableau) ///
	name(lengmat_cat, replace)	
	
	* niños de 3 años o menos
	list codfam nomcompl edad edad_meses if edad <=3 | !mi(edad_meses)
	br codfam sectfam nomcompl relajef_s if (relajef_s==1|relajef_s==2|relajef_s==3) & (codfam=="PAVRPI003"|codfam=="PAVRPI006"|codfam=="PAPZBA002"|codfam=="PAPZBA003"|codfam=="PAPZBA010"|codfam=="PAPZBA008"|codfam=="PAPZBA007"|codfam=="PAPZSH004"|codfam=="PAVRUS009"|codfam=="PAVRUS005")

	
// Preparing dataset to be merged with other sections //
	bys codfam: keep if jefx==1 //dejamos solo al jefx
	/*Solo 1 jefe de familia no vive en el hogar 
	con el resto de su familia*/

	*save "$data\lbpasco_sociodem_byfam", replace
	* u "$data\lbpasco_sociodem_byfam", clear

	
	
	
	
	