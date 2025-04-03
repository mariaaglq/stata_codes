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
import excel "$root\data\BD_pasco_familias_final.xlsx", sheet("ganado") cellrange(A1:AP49) firstrow

	*save "$data\lbpasco_ganado", replace
	
	u "$data\lbpasco_ganado", clear

/// CLEANING DATA ///
	gen ame4="chancho" if !mi(ame4_prod)
	gen ame4_prod= ame2_prod if ame2=="chancho"
	replace ame4_prod = ame3_prod if ame3=="chancho"
	replace ame2_prod=. if ame2=="chancho"
	replace ame3_prod=. if ame3=="chancho"
	replace ame2="" if ame2=="chancho"
	replace ame3="" if ame3=="chancho"
	
	egen nespanim = rownonmiss(ama1 ame? pez1), strok
	la var nespanim "N° especies de ganado"
	
/// ANALYSIS ///
	*ama1 vacuno
	*ame1 gallina
	*ame2 cuy
	*ame3 pato
	*ame4 chancho
	*pez1
	*pez2
	
sum *_prod,d
sum ame2_prod, d
sum ame4_prod, d
sum ame3_prod, d

br pez* if !mi(pez1)|!mi(pez2)
