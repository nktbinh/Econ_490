clear
use "C:\Users\Binh Nguyen\Desktop\ECON 490\Main Paper\ICPSR_36361\DS0001\36361-0001-Data.dta" 
cd D:\MainPaper\
set more off
rename IRMARIT maristat // variable for marital status of the participant
rename INCOME inc // variable for family income level of the participant
label var inc "Family Income"
rename DEPNDALC alcdep // binary variable for whether or not the partipant has alchol dependency (1 if yes 0 if no)
label var alcdep "Alcohol Dependence"
label define alcdep 1 "Dependent" 0 "Not Dependent"
label values alcdep alcdep
rename EDUCCAT2 educat // composite variable for education and age
label var educat "Education & Age"
label define educat 1 "Less than high school" 2 "High school graduate" 3 "Some college" 4 "College graduate"
label values educat educat
rename SEXRACE sexrace // composite variable for sex and race
label var sexrace "Sex & Race"
label define sexrace 1 "White Male" 2 "White Female" 3 "Black Male" 4 "Black Female" 5 "Hispanic Male" 6 "Hispanic Female" 7 "Others"
label values sexrace sexrace
rename IREDUC2 educ // variable for education level of the participant
rename NEWRACE2 race // variable for race
label var race "Race"
label define race 1 "White" 2 "Black" 3 "Native American/Alaska" 4 "Native Hawaiian/Pacific Islanders" 5 "Asian" 6 "More than one race" 7 "Hispanic"
label value race race
rename ABUSEALC alcabu // variable for past year alcohol abuse
rename TXNEDALC alctre // variable for whether respondent needed treatment for alcohol use last year
rename ABODALC abudep // variable for past year alcohol abuse OR dependence
rename POVERTY2 pov // variable for poverty level
label var pov "Poverty Level"
rename WHODASC3 who 
label var who "WHODAS Score"
recode IRSEX(2=1)(1=0), gen(gender) // generated variable for gender (1 if female 0 if male)
label var gender "Gender"
label define gender 1 female 0 male
label values gender gender
tab gender, gen(genderid)
gen daysalc=ALCYRTOT
label var daysalc "Days of alcohol consumption past year"
replace daysalc=0 if ALCYRTOT==991
replace daysalc=0 if ALCYRTOT==993
gen male=.
replace male=1 if gender==0
replace male=0 if gender==1
gen female=.
replace female=1 if gender==1
replace female=0 if gender==0
recode CATAGE (2=1)(3=2)(4=3)(1=4), gen(age)
label define age 1 "18 - 25" 2 "26 - 34" 3 ">=35"
label value age age
recode BINGEHVY (1=4)(2=3)(3=2)(4=1), gen(alcbin)
recode EMPSTATY (1=1)(2=1)(3=0)(4=0), gen(employed)
label var employed Employment
label define employed 1 employed 0 unemployed
label values employed employed
recode EMPSTATY (3=0)(1=1)(2=1)(4=2), gen(empstat)
recode EMPSTATY (1=1)(2=0)(3=0)(4=0), gen(fullemp)
label var fullemp "Fulltime Employment"
label define fullemp 1 "Fulltime Employed" 0 "Not Fulltime Fmployed"
label values fullemp fullemp
recode AMDELT (1=1)(2=0), gen(mde)
recode race(7=1)(1=0)(2=0)(3=0)(4=0)(5=0)(6=0), gen(hispanic)
recode maristat (1=1)(4=3)(2=2)(3=2), gen(maristat2)
label var maristat2 "Marital Status"
label define maristat2 1 Married 2 "Divorced or Widowed" 3 "Never Married"
label values maristat2 maristat2
recode maristat (1=1)(4=0)(2=0)(3=0), gen(maristat1)
label var maristat1 "Maried or Not"
label define maristat1 1 Married 0 "Not Married"
label values maristat1 maristat1
tab inc, gen(incid)
tab age, gen(ageid)
tab race, gen(raceid)
tab maristat2, gen(mariid)
tab educ, gen(educid)
tab educat, gen(educatid)
mark condition if maristat!=99 & daysalc!=985 & daysalc<=993 & mde>=0 & EMPSTATY!=4 & who>=0 & pov>=1
mark conditionm if maristat!=99 & daysalc!=985 & daysalc<=993 & mde>=0 & EMPSTATY!=4 & who>=0 & pov>=1 & gender==0
mark conditionf if maristat!=99 & daysalc!=985 & daysalc<=993 & mde>=0 & EMPSTATY!=4 & who>=0 & pov>=1 & gender==1
***************************************

//OVERALL SUMMARY STATISTIC

***************************************


//Alcohol Dependence by Genders of all Controls
eststo clear
eststo Men: estpost tabstat employed incid1 incid2 incid3 incid4 ageid1 ageid2 ageid3 raceid1 raceid2 raceid3 raceid4 raceid5 raceid6 raceid7 mariid1 mariid2 mariid3 educid1 educid2 educid3 educid4 educid5 educid6 educid7 educid8 educid9 educid10 educid11 if conditionm==1, by(alcdep) listwise stat(mean sd) col(stat)
eststo Women: estpost tabstat employed incid1 incid2 incid3 incid4 ageid1 ageid2 ageid3 raceid1 raceid2 raceid3 raceid4 raceid5 raceid6 raceid7 mariid1 mariid2 mariid3 educid1 educid2 educid3 educid4 educid5 educid6 educid7 educid8 educid9 educid10 educid11 if conditionf==1, by(alcdep) listwise stat(mean sd) col(stat)
eststo BothGenders: estpost tabstat employed incid1 incid2 incid3 incid4 ageid1 ageid2 ageid3 raceid1 raceid2 raceid3 raceid4 raceid5 raceid6 raceid7 mariid1 mariid2 mariid3 educid1 educid2 educid3 educid4 educid5 educid6 educid7 educid8 educid9 educid10 educid11 if condition==1, by(alcdep) listwise stat(mean sd) col(stat)
esttab Men Women BothGenders using SummaryStat.rtf, replace main(mean) aux(sd) note(woah) varwidth(20) refcat(....) br label nostar mti ti(SUMMARY STATISTIC)

//All Treatments by Genders
eststo clear
sort gender
by gender: eststo: estpost sum alcdep alcabu abudep daysalc alctre if condition==1, listwise
esttab using Treatments.rtf, replace main(mean) aux(sd) nonumber mtitle("Male" "Female") label br

eststo clear
eststo: estpost sum alcdep alcabu abudep daysalc alctre if condition==1, listwise
esttab using Treatments.rtf, main(mean) aux(sd) nonumber mtitle("Both Genders") label br append

//Non Alcohol Dependence
eststo clear
eststo: estpost sum employed incid1 incid2 incid3 incid4 ageid1 ageid2 ageid3 raceid1 raceid2 raceid3 raceid4 raceid5 raceid6 raceid7 mariid1 mariid2 mariid3 educid1 educid2 educid3 educid4 educid5 educid6 educid7 educid8 educid9 educid10 educid11 if alcdep==0, listwise
esttab using Non-Alcdep-Allages.rtf, replace main(mean) aux(sd) nonumber mtitle label br

//Non Alcohol Dependence by Genders
eststo clear
sort gender
by gender: eststo: estpost sum employed incid1 incid2 incid3 incid4 ageid1 ageid2 ageid3 raceid1 raceid2 raceid3 raceid4 raceid5 raceid6 raceid7 mariid1 mariid2 mariid3 educid1 educid2 educid3 educid4 educid5 educid6 educid7 educid8 educid9 educid10 educid11 if alcdep==0, listwise
esttab using Non-Alcdep.rtf, replace main(mean) aux(sd) nonumber mtitle("Male" "Female") label br

//Non Full-time Employed by Genders
eststo clear
by gender: eststo: estpost sum alcdep alcabu abudep daysalc alctre if fullemp==0 & condition==1, listwise
esttab using Non-Fulltime-Employed.rtf, replace main(mean) aux(sd) nonumber mtitle("Male" "Female") label br

eststo clear
eststo: estpost sum alcdep alcabu abudep daysalc alctre if fullemp==0 & condition==1, listwise
esttab using Non-Fulltime-Employed.rtf, main(mean) aux(sd) nonumber mtitle("Male" "Female") label br append
****************************************

//GRAPHS & CHARTS

****************************************

//Summary Statistic Pie Chart of Genders & Alcohol Dependence
graph pie genderid1 genderid2 if condition==1, plabel(1 percent, size(*1.5) color(white)) plabel(2 percent, size(*1.5) color(white)) plotregion(lstyle(none)) title("Summary of Genders")
graph pie alcdep if condition==1, over(gender) pie(2,explode) plabel(1 percent, size(*1.5) color(white)) plabel(2 percent, size(*1.5) color(white)) title("Alcohol Dependence according to Genders")

graph pie alcdep if condition==1, over(race) pie(1,explode) plabel(1 percent, size(*1.5) color(white)) plabel(2 percent, size(*1.5) color(white)) plabel(3 percent, size(*1.0) color(black)) plabel(4 percent, size(*1.0) color(black)) plabel(5 percent, size(*1.0) color(black)) plabel(6 percent, size(*0.5) color(black)) plabel(7 percent, size(*1.5) color(white)) title("Alcohol Dependence according to Races")


//Graphs for Overall OLS and Probit Regression Results
eststo clear
eststo A1: quietly xi:reg inc alcdep employed educat i.maristat2 i.race if condition==1
eststo B1: quietly xi:reg inc alcdep employed educat i.maristat2 i.race if conditionm==1
eststo C1: quietly xi:reg inc alcdep employed educat i.maristat2 i.race if conditionf==1
eststo A2: quietly xi:reg inc alcabu employed educat i.maristat2 i.race if condition==1
eststo B2: quietly xi:reg inc alcabu employed educat i.maristat2 i.race if conditionm==1
eststo C2: quietly xi:reg inc alcabu employed educat i.maristat2 i.race if conditionf==1
eststo A3: quietly xi:reg inc abudep employed educat i.maristat2 i.race if condition==1
eststo B3: quietly xi:reg inc abudep employed educat i.maristat2 i.race if conditionm==1
eststo C3: quietly xi:reg inc abudep employed educat i.maristat2 i.race if conditionf==1
eststo A4: quietly xi:reg inc daysalc employed educat i.maristat2 i.race if condition==1
eststo B4: quietly xi:reg inc daysalc employed educat i.maristat2 i.race if conditionm==1
eststo C4: quietly xi:reg inc daysalc employed educat i.maristat2 i.race if conditionf==1
eststo A5: quietly xi:reg inc alctre employed educat i.maristat2 i.race if condition==1
eststo B5: quietly xi:reg inc alctre employed educat i.maristat2 i.race if conditionm==1
eststo C5: quietly xi:reg inc alctre employed educat i.maristat2 i.race if conditionf==1
coefplot (A1\A2\A3\A4\A5, label(Both Genders)) (B1\B2\B3\B4\B5, label(Male)) (C1\C2\C3\C4\C5, label(Female)), keep(alcdep alcabu daysalc alctre abudep) xline(0) xtitle(OLS Regression according to gender) grid(between glcolor(black) glpattern(dash)) mlabel format(%7.0g) mlabposition(12) mlabgap(*2)

eststo clear
eststo M1: quietly probit fullemp alcdep inc educat i.maristat2 i.race if condition==1
eststo N1: quietly probit fullemp alcdep inc educat i.maristat2 i.race if conditionm==1
eststo P1: quietly probit fullemp alcdep inc educat i.maristat2 i.race if conditionf==1
eststo M2: quietly probit fullemp alcabu inc educat i.maristat2 i.race if condition==1
eststo N2: quietly probit fullemp alcabu inc educat i.maristat2 i.race if conditionm==1
eststo P2: quietly probit fullemp alcabu inc educat i.maristat2 i.race if conditionf==1
eststo M3: quietly probit fullemp abudep inc educat i.maristat2 i.race if condition==1
eststo N3: quietly probit fullemp abudep inc educat i.maristat2 i.race if conditionm==1
eststo P3: quietly probit fullemp abudep inc educat i.maristat2 i.race if conditionf==1
eststo M4: quietly probit fullemp daysalc inc educat i.maristat2 i.race if condition==1
eststo N4: quietly probit fullemp daysalc inc educat i.maristat2 i.race if conditionm==1
eststo P4: quietly probit fullemp daysalc inc educat i.maristat2 i.race if conditionf==1
eststo M5: quietly probit fullemp alctre inc educat i.maristat2 i.race if condition==1
eststo N5: quietly probit fullemp alctre inc educat i.maristat2 i.race if conditionm==1
eststo P5: quietly probit fullemp alctre inc educat i.maristat2 i.race if conditionf==1
coefplot (M1\M2\M3\M4\M5, label(Both Genders)) (N1\N2\N3\N4\N5, label(Male)) (P1\P2\P3\P4\P5, label(Female)), keep(alcdep alcabu daysalc alctre abudep) xline(0) xtitle(OLS Regression according to gender) grid(between glcolor(black) glpattern(dash)) mlabel format(%7.0g) mlabposition(12) mlabgap(*2)

////Subgraphs by Different Genders
//OLS Both Genders
eststo clear
quietly eststo multivariate: regress inc alcdep alcabu abudep daysalc alctre if condition==1
foreach var in alcdep alcabu abudep daysalc alctre {
quietly eststo `var': regress inc `var' employed educat i.maristat2 i.race if condition==1
}
coefplot (alcdep\alcabu\abudep\daysalc\alctre, label(treatments)), keep(alcdep alcabu daysalc alctre abudep) xline(0) xtitle(OLS Regression for Both Genders) mlabel format(%7.0g) mlabposition(12) mlabgap(*2)

//OLS Men
eststo clear
quietly eststo multivariate: regress inc alcdep alcabu abudep daysalc alctre if conditionm==1
foreach var in alcdep alcabu abudep daysalc alctre {
quietly eststo `var': regress inc `var' employed educat i.maristat2 i.race if conditionm==1
}
coefplot (alcdep\alcabu\abudep\daysalc\alctre, label(treatments)), keep(alcdep alcabu daysalc alctre abudep) xline(0) xtitle(OLS Regression for Men) mlabel format(%7.0g) mlabposition(12) mlabgap(*2)

//OLS Women
eststo clear
quietly eststo multivariate: regress inc alcdep alcabu abudep daysalc alctre if conditionf==1
foreach var in alcdep alcabu abudep daysalc alctre {
quietly eststo `var': regress inc `var' employed educat i.maristat2 i.race if conditionf==1
}
coefplot (alcdep\alcabu\abudep\daysalc\alctre, label(treatments)), keep(alcdep alcabu daysalc alctre abudep) xline(0) xtitle(OLS Regression for Women) mlabel format(%7.0g) mlabposition(12) mlabgap(*2)

//Probit Both Genders
eststo clear
quietly eststo multivariate: probit fullemp alcdep alcabu abudep daysalc alctre if condition==1
foreach var in alcdep alcabu abudep daysalc alctre {
quietly eststo `var': probit fullemp alcdep inc educat i.maristat2 i.race if condition==1
}
coefplot (alcdep\alcabu\abudep\daysalc\alctre, label(treatments)), keep(alcdep alcabu daysalc alctre abudep) xline(0) xtitle(OLS Regression for Both Genders) mlabel format(%7.0g) mlabposition(12) mlabgap(*2)

//Probit Men
eststo clear
quietly eststo multivariate: probit fullemp alcdep alcabu abudep daysalc alctre if conditionm==1
foreach var in alcdep alcabu abudep daysalc alctre {
quietly eststo `var': probit fullemp alcdep inc educat i.maristat2 i.race if conditionm==1
}
coefplot (alcdep\alcabu\abudep\daysalc\alctre, label(treatments)), keep(alcdep alcabu daysalc alctre abudep) xline(0) xtitle(OLS Regression for Both Genders) mlabel format(%7.0g) mlabposition(12) mlabgap(*2)

//Probit Women
eststo clear
quietly eststo multivariate: probit fullemp alcdep alcabu abudep daysalc alctre if conditionf==1
foreach var in alcdep alcabu abudep daysalc alctre {
quietly eststo `var': probit fullemp alcdep inc educat i.maristat2 i.race if conditionf==1
}
coefplot (alcdep\alcabu\abudep\daysalc\alctre, label(treatments)), keep(alcdep alcabu daysalc alctre abudep) xline(0) xtitle(OLS Regression for Both Genders) mlabel format(%7.0g) mlabposition(12) mlabgap(*2)


***************************************

//OLS REGRESSION

***************************************

eststo clear	
	capt prog drop appendmodels
program appendmodels, eclass
    // using first equation of model
    version 8
    syntax namelist
    tempname b V tmp
    foreach name of local namelist {
        qui est restore `name'
        mat `tmp' = e(b)
        local eq1: coleq `tmp'
        gettoken eq1 : eq1
        mat `tmp' = `tmp'[1,"`eq1':"]
        local cons = colnumb(`tmp',"_cons")
        if `cons'<. & `cons'>1 {
            mat `tmp' = `tmp'[1,1..`cons'-1]
        }
        mat `b' = nullmat(`b') , `tmp'
        mat `tmp' = e(V)
        mat `tmp' = `tmp'["`eq1':","`eq1':"]
        if `cons'<. & `cons'>1 {
            mat `tmp' = `tmp'[1..`cons'-1,1..`cons'-1]
        }
        capt confirm matrix `V'
        if _rc {
            mat `V' = `tmp'
        }
        else {
            mat `V' = ///
            ( `V' , J(rowsof(`V'),colsof(`tmp'),0) ) \ ///
            ( J(rowsof(`tmp'),colsof(`V'),0) , `tmp' )
        }
    }
    local names: colfullnames `b'
    mat coln `V' = `names'
    mat rown `V' = `names'
    eret post `b' `V'
    eret local cmd "whatever"
end
eststo clear
eststo a1: quietly xi:reg inc alcdep employed educat i.maristat2 i.race if condition==1
eststo a2: quietly xi:reg inc alcabu employed educat i.maristat2 i.race if condition==1
eststo a3: quietly xi:reg inc abudep employed educat i.maristat2 i.race if condition==1
eststo a4: quietly xi:reg inc daysalc employed educat i.maristat2 i.race if condition==1
eststo a5: quietly xi:reg inc alctre employed educat i.maristat2 i.race if condition==1
eststo bothgender: appendmodels a1 a2 a3 a4 a5
esttab bothgender, keep(alcdep alcabu daysalc alctre abudep) p ar2 obs mti label noomit nonumber 
eststo b1: quietly xi:reg inc alcdep employed educat i.maristat2 i.race if conditionm==1
eststo b2: quietly xi:reg inc alcabu employed educat i.maristat2 i.race if conditionm==1
eststo b3: quietly xi:reg inc abudep employed educat i.maristat2 i.race if conditionm==1
eststo b4: quietly xi:reg inc daysalc employed educat i.maristat2 i.race if conditionm==1
eststo b5: quietly xi:reg inc alctre employed educat i.maristat2 i.race if conditionm==1
eststo Male: appendmodels b1 b2 b3 b4 b5
esttab Male, keep(alcdep alcabu daysalc alctre abudep) p r2 obs mti label noomit nonumber  
eststo c1: quietly xi:reg inc alcdep employed educat i.maristat2 i.race if conditionf==1
eststo c2: quietly xi:reg inc alcabu employed educat i.maristat2 i.race if conditionf==1
eststo c3: quietly xi:reg inc abudep employed educat i.maristat2 i.race if conditionf==1
eststo c4: quietly xi:reg inc daysalc employed educat i.maristat2 i.race if conditionf==1
eststo c5: quietly xi:reg inc alctre employed educat i.maristat2 i.race if conditionf==1
eststo Female: appendmodels c1 c2 c3 c4 c5
esttab Female, keep(alcdep alcabu daysalc alctre abudep) p r2 obs mti label noomit nonumber  
esttab bothgender Male Female using RegressionResult.rtf, replace keep(alcdep alcabu daysalc alctre abudep) mtitles("Overall Household Income" "Men's Household Income" "Women's Household Income") ti(TABLE 1. OLS Regression Result) p label noomit nonumber
eststo clear
	
****************************************************************************************

//PROBIT REGRESSION

****************************************************************************************
eststo clear	
eststo a1: quietly probit fullemp alcdep inc educat i.maristat2 i.race if condition==1
eststo a2: quietly probit fullemp alcabu inc educat i.maristat2 i.race if condition==1
eststo a3: quietly probit fullemp abudep inc educat i.maristat2 i.race if condition==1
eststo a4: quietly probit fullemp daysalc inc educat i.maristat2 i.race if condition==1
eststo a5: quietly probit fullemp alctre inc educat i.maristat2 i.race if condition==1
eststo bothgender: appendmodels a1 a2 a3 a4 a5
esttab bothgender, keep(alcdep alcabu daysalc alctre abudep) p r2 label noomit nonumber
eststo b1: quietly probit fullemp alcdep inc educat i.maristat2 i.race if conditionm==1
eststo b2: quietly probit fullemp alcabu inc educat i.maristat2 i.race if conditionm==1
eststo b3: quietly probit fullemp abudep inc educat i.maristat2 i.race if conditionm==1
eststo b4: quietly probit fullemp daysalc inc educat i.maristat2 i.race if conditionm==1
eststo b5: quietly probit fullemp alctre inc educat i.maristat2 i.race if conditionm==1
eststo Male: appendmodels b1 b2 b3 b4 b5
esttab Male, keep(alcdep alcabu daysalc alctre abudep) p r2 label noomit nonumber 
eststo c1: quietly probit fullemp alcdep inc educat i.maristat2 i.race if conditionf==1
eststo c2: quietly probit fullemp alcabu inc educat i.maristat2 i.race if conditionf==1
eststo c3: quietly probit fullemp abudep inc educat i.maristat2 i.race if conditionf==1
eststo c4: quietly probit fullemp daysalc inc educat i.maristat2 i.race if conditionf==1
eststo c5: quietly probit fullemp alctre inc educat i.maristat2 i.race if conditionf==1
eststo Female: appendmodels c1 c2 c3 c4 c5
esttab Female, keep(alcdep alcabu daysalc alctre abudep) p r2 label noomit nonumber 
esttab bothgender Male Female using RegressionResult.rtf, keep(alcdep alcabu daysalc alctre abudep) mtitles("Overall Fulltime Employment" "Men's Fulltime Employment" "Women's Fulltime Employment") ti(TABLE 2. Probit Regression Result) p label noomit nonumber append
eststo clear



//Probit Regression with employed as the outcome variable and maristat2 no longer included
eststo clear	
eststo a1: quietly probit employed alcdep inc educat i.race if condition==1
eststo a2: quietly probit employed alcabu inc educat i.race if condition==1
eststo a3: quietly probit employed abudep inc educat i.race if condition==1
eststo a4: quietly probit employed daysalc inc educat i.race if condition==1
eststo a5: quietly probit employed alctre inc educat i.race if condition==1
eststo bothgender: appendmodels a1 a2 a3 a4 a5
esttab bothgender, keep(alcdep alcabu daysalc alctre abudep) p r2 label noomit nonumber
eststo b1: quietly probit employed alcdep inc educat i.race if conditionm==1
eststo b2: quietly probit employed alcabu inc educat i.race if conditionm==1
eststo b3: quietly probit employed abudep inc educat i.race if conditionm==1
eststo b4: quietly probit employed daysalc inc educat i.race if conditionm==1
eststo b5: quietly probit employed alctre inc educat i.race if conditionm==1
eststo Male: appendmodels b1 b2 b3 b4 b5
esttab Male, keep(alcdep alcabu daysalc alctre abudep) p r2 label noomit nonumber 
eststo c1: quietly probit employed alcdep inc educat i.race if conditionf==1
eststo c2: quietly probit employed alcabu inc educat i.race if conditionf==1
eststo c3: quietly probit employed abudep inc educat i.race if conditionf==1
eststo c4: quietly probit employed daysalc inc educat i.race if conditionf==1
eststo c5: quietly probit employed alctre inc educat i.race if conditionf==1
eststo Female: appendmodels c1 c2 c3 c4 c5
esttab Female, keep(alcdep alcabu daysalc alctre abudep) p r2 label noomit nonumber 
esttab bothgender Male Female using RegressionResult.rtf, keep(alcdep alcabu daysalc alctre abudep) mtitles("Overall Fulltime Employment" "Men's Fulltime Employment" "Women's Fulltime Employment") ti(TABLE 2. Probit Regression Result) p label noomit nonumber append
eststo clear
	
****************************************************************************************		
	
****************************************************************************************
