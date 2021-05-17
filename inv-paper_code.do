**Sokolova, A., Buskens, V., Raub, W. Cooperation through Investments in
*Repeated Interactions and Contractual Agreements: An Experimental Study*

version 14
clear all
cls
set more off
cd "C:\Users\aasok\YandexDisk\Thesis paper\Data\Paper"
use inv-paper_data.dta, clear

//////////////////////////// Data preparation ///////////////////////////////

//Original data
///cd "C:\Users\aasok\YandexDisk\Thesis paper\Data\Paper"
///use inv-paper_original-data.dta, clear

//delete the 1 participant who left
*drop if session=="200211_0947"&Subject==6

//Generate investment cost variable:
*gen inv_cost = 5
*replace inv_cost = 15 if session=="191211_0939" | session=="191211_1339" ///
*| session=="191212_1126" | session=="200115_0953" | session=="200117_0947" ///
*| session=="200211_0947"
*replace inv_cost = 45 if session=="191210_1019" | session=="191210_1447" ///
*| session=="200205_1002"

//Generate condition variable:
*gen exp_cond = 0
/// RI_OS_lc (start with 'Repeated interactions/One-shot PD', low investment costs) 
*replace exp_cond = 1 if inv_cost==5 & RepeatFirst==1
/// CA_OS_lc (start with 'Contractual agreements/One-shot PD', low investment costs)
*replace exp_cond = 2 if inv_cost==5 & RepeatFirst==0
/// CA_OS_mc
*replace exp_cond = 3 if inv_cost==15 & RepeatFirst==1
/// IC_OS_mc
*replace exp_cond = 4 if inv_cost==15 & RepeatFirst==0
/// RI_OS_hc
*replace exp_cond = 5 if inv_cost==45 & RepeatFirst==1
/// CA_OS_hc
*replace exp_cond = 6 if inv_cost==45 & RepeatFirst==0

//Generate session size variable
*gen session_size = 12
*replace session_size = 20 if session=="191209_0848" | session=="191210_1447"
*replace session_size = 22 if session=="191205_1445"
*replace session_size = 16 if session=="191210_1019" | session=="191211_1339"
*replace session_size = 14 if session=="200205_1002" 
*replace session_size = 4 if session=="200113_0944" 
*replace session_size = 10 if session=="200211_0947" | session=="191212_1126"
*replace session_size = 8 if session=="191211_0939" 

// Generate a unique ID for subjects
*encode session, generate (session_id)
*gen subject_id = (session_id * 100) + Subject

///save "inv-paper_data.dta", replace


///////////////////////// Investments in embeddedness /////////////////////////

/// Table 2:
tab choice Treatment

// Part 2 dummy
*gen part2 = 0 if Treatment < 3
*replace part2 = 1 if Treatment > 2
*replace part2 = . if Treatment > 4

/// Earnings from Part 2
*gen payoff_full = PayoffAverage
*replace payoff_full = PayoffAverage + inv_cost if condition != 1
*replace payoff_full = . if Treatment > 2
*sort subject_id
*by subject_id: egen payoff_p2 = total(payoff_full)
*sort session Treatment subject
//centered (standardized) payoffs from Part 2
*egen mean_payoff_p2 = mean(payoff_p2)
*gen payoff_p2_cntr = payoff_p2 - mean_payoff_p2
*replace payoff_p2_cntr = 0 if Treatment < 3
*replace payoff_p2_cntr = . if Treatment > 4

/// Investments in cooperation mechanisms in Part 2
*gen condition_p2 = 1 if condition > 1
*replace condition_p2 = 0 if condition==1
*replace condition_p2 = .  if Treatment > 2
*sort subject_id
*by subject_id: egen mechexp_p2 = total(condition_p2)
*sort session Treatment subject
//centered (standardized) investments in mechanisms from Part 2
*egen mean_mechexp_p2 = mean(mechexp_p2)
*gen mechexp_p2_cntr = mechexp_p2 - mean_mechexp_p2
*replace mechexp_p2_cntr = 0 if Treatment < 3
*replace mechexp_p2_cntr = . if Treatment > 4

// choice of RI
*gen choice_RI = 1 if choice==1 & RepeatFirst==1 & Treatment < 3
*replace choice_RI = 1 if choice==1 & RepeatFirst==0 & Treatment > 2
*replace choice_RI = 0 if choice==0 & RepeatFirst==1 & Treatment < 3
*replace choice_RI = 0 if choice==0 & RepeatFirst==0 & Treatment > 2
*replace choice_RI = . if Treatment > 4

// choice of CA
*gen choice_CA = 1 if choice==1 & RepeatFirst==0 & Treatment < 3
*replace choice_CA = 1 if choice==1 & RepeatFirst==1 & Treatment > 2
*replace choice_CA = 0 if choice==0 & RepeatFirst==0 & Treatment < 3
*replace choice_CA = 0 if choice==0 & RepeatFirst==1 & Treatment > 2
*replace choice_CA = . if Treatment > 4


//whether the choice is between CA(contractual agreements) and OS(one-shot) (0) 
///or RI(repeated interactions) and OS (1) - 
*gen ri_os = 0
*replace ri_os = 1 if exp_cond==1 & Treatment<3
*replace ri_os = 1 if exp_cond==3 & Treatment<3
*replace ri_os = 1 if exp_cond==5 & Treatment<3
*replace ri_os = 1 if exp_cond==2 & Treatment>2
*replace ri_os = 1 if exp_cond==4 & Treatment>2
*replace ri_os = 1 if exp_cond==6 & Treatment>2
*replace ri_os = . if Treatment > 4


melogit choice i.inv_cost i.ri_os i.inv_cost##i.ri_os || subject_id:
margins, over(inv_cost ri_os)
margins, over(inv_cost ri_os) pwcompare(effects)


/////////////////////////////// RI vs CA //////////////////////////////////////

///// Part 4

// Earnings from RI and CA in Parts 2-3
*gen payoff_ri = PayoffAverage + inv_cost if condition == 2
*replace payoff_ri = 0 if condition != 2
*replace payoff_ri = . if Treatment > 4
*gen payoff_ca = PayoffAverage + inv_cost if condition == 3
*replace payoff_ca = 0 if condition != 3
*replace payoff_ca = . if Treatment > 4

*sort subject_id
*by subject_id: egen payoff_ri_full = total(payoff_ri)
*by subject_id: egen payoff_ca_full = total(payoff_ca)
*sort session Treatment subject
*egen mean_payoff_ri = mean(payoff_ri_full)
*gen payoff_ri_cntr = payoff_ri_full - mean_payoff_ri
*egen mean_payoff_ca = mean(payoff_ca_full)
*gen payoff_ca_cntr = payoff_ca_full - mean_payoff_ca


// Choosing RI and CA in Parts 2-3
*gen ri_p23 = 1 if condition==2
*replace ri_p23 = 0 if condition !=2
*replace ri_p23 = . if Treatment > 4
*gen ca_p23 = 1 if condition==3
*replace ca_p23 = 0 if condition !=3
*replace ca_p23 = . if Treatment > 4

*sort subject_id
*by subject_id: egen ri_p23_full = total(ri_p23)
*by subject_id: egen ca_p23_full = total(ca_p23)
*sort session Treatment subject
*egen mean_ri = mean(ri_p23_full)
*gen ri_p23_cntr = ri_p23_full - mean_ri
*egen mean_ca = mean(ca_p23_full)
*gen ca_p23_cntr = ca_p23_full - mean_ca


keep if Treatment > 4

//empty model: constant (Table 4, Model 1)
melogit choice|| subject_id:
estimates store part4_empty
estat ic
estat icc

//model with investment costs (Table 4, Model 2)
melogit choice i.inv_cost || subject_id:
estimates store part4_cost
estat ic
estat icc
lrtest part4_empty part4_cost

//model with previous earnings
melogit choice i.inv_cost c.payoff_ri_cntr c.ri_p23_cntr ///
c.payoff_ca_cntr c.ca_p23_cntr || subject_id:
estimates store part4_payoffs
estat ic
estat icc
lrtest part4_cost part4_payoffs

//model with previous earnings+interactions
melogit choice i.inv_cost c.payoff_ri_cntr c.payoff_ca_cntr ///
c.payoff_ri_cntr#c.ri_p23_cntr c.payoff_ca_cntr#c.ca_p23_cntr|| subject_id: , var
estimates store part4_int
estat ic
estat icc
lrtest part4_payoffs part4_int

use inv-paper_data.dta, clear


//////////////////////////// Effects of RI and CA ////////////////////////////

//Recode decision
///0 - defection, 1 - cooperation
*gen decision_coop = 0
*replace decision_coop = 1 if decision==0
*replace decision_coop = . if Treatment==7


///// Parts 2-4 

keep if Treatment < 7
***decision_coop = 1: 'cooperation'
// Table 5:
tab condition decision_coop if Treatment == 1
tab condition decision_coop if Treatment == 2
tab condition decision_coop if Treatment == 3
tab condition decision_coop if Treatment == 4
tab condition decision_coop if Treatment == 5
tab condition decision_coop if Treatment == 6

//intercept only
melogit decision_coop || subject_id:
estat ic
estat icc

//model only with mechanism types (Table 6)
melogit decision_coop i.condition || subject_id:
estimates store coop_mech
estat ic
estat icc

//model with mechanism types and costs
melogit decision_coop i.condition i.inv_cost ///
i.inv_cost##i.choice_mech|| subject_id:
margins condition, pwcompar(effects)
estimates store coop_inv
estat ic
estat icc
lrtest coop_mech coop_inv

use inv-paper_data.dta, clear

//model with interactions
*meqrlogit decision_coop i.condition i.inv_cost i.inv_cost#i.RepeatFirst ///
*i.inv_cost#i.condition i.condition#i.RepeatFirst || subject_id: , var
*estimates store coop_int
*estat ic
*estat icc
*lrtest coop_mech coop_int


///// Parts 2-3 (same results)

//choice of mechanisms for cooperation (RI or CA), Parts 2-3
*gen choice_mech = 1 if condition==2 | condition==3
*replace choice_mech = 0 if condition==1
*replace choice_mech = . if Treatment > 6

*keep if Treatment < 5
//intercept only
*melogit decision_coop || subject_id:
//model only with mechanism types
*melogit decision_coop i.condition || subject_id: 
//model with mechanism types and costs
*melogit decision_coop i.condition i.inv_cost i.inv_cost##i.choice_mech|| subject_id:
*use inv-paper_data.dta, clear


///////////////////////////// APPENDIX /////////////////////////////

/// Table D1: investments in Part 2
melogit choice i.ri_os if part2 == 0 || subject_id:

melogit choice i.inv_cost i.ri_os i.inv_cost##i.ri_os if part2 == 0 || subject_id: 

///+Part 2, iteration 1
logit choice i.inv_cost i.ri_os i.inv_cost##i.ri_os if part2 == 0 & iteration == 1

/// Table D2: effects on cooperation in Part 2 
melogit decision_coop i.condition if part2 == 0|| subject_id:

melogit decision_coop i.condition i.inv_cost i.inv_cost##i.choice_mech if part2 == 0|| subject_id:

///+Part 2, iteration 1
logit decision_coop i.condition i.inv_cost i.inv_cost##i.choice_mech if part2 == 0 & iteration == 1


/// Table D3: investements in CA and RI, full table (Parts 2,3)

*********** Mlvl logreg, random intercept on a subject lvl: RI ***********
//intercept only
melogit choice_RI || subject_id:
estat ic
estimates store intercept_ri

//simple model
meqrlogit choice_RI i.inv_cost || subject_id: , var
melogit choice_RI i.inv_cost || subject_id:
margins i.inv_cost, pwcompare (effects)  
estimates store empty_ri
estat ic
estat icc
lrtest empty_ri intercept_ri

//experience
melogit choice_RI i.inv_cost i.part2  c.payoff_p2_cntr c.mechexp_p2_cntr || subject_id:
estimates store exp_ri
estat ic
estat icc
lrtest empty_ri exp_ri

//experience+interaction
melogit choice_RI i.inv_cost i.part2  c.payoff_p2_cntr c.mechexp_p2_cntr ///
c.payoff_p2_cntr#c.mechexp_p2_cntr || subject_id:
margins i.inv_cost, pwcompare (effects)  
estimates store expint_ri
estat ic
estat icc
lrtest exp_ri expint_ri


*********** Mlvl logreg, random intercept on a subject lvl: CA ***********
//intercept only
melogit choice_CA || subject_id:
estat ic
estat icc

//simple model
melogit choice_CA i.inv_cost || subject_id:
meqrlogit choice_CA i.inv_cost || subject_id: , var
estimates store empty_ca
estat ic
estat icc

//model with experience
melogit choice_CA i.inv_cost i.part2 c.payoff_p2_cntr c.mechexp_p2_cntr ///
|| subject_id:
margins i.inv_cost, pwcompare (effects)  
estimates store exp_ca
estat ic
estat icc
lrtest empty_ca exp_ca

//experience+interaction
melogit choice_CA i.inv_cost i.part2  c.payoff_p2_cntr c.mechexp_p2_cntr ///
c.payoff_p2_cntr#c.mechexp_p2_cntr || subject_id:
estimates store expint_ca
estat ic
estat icc
lrtest exp_ca expint_ca


/// Table D4:

//// Session characteristics
// session size - session_size
// order condition - RepeatFirst

//// Social value orientations
*gen trust = (trust1+trust2+trust3)/3
*gen risk = (risk1+risk1_a+risk1_b+risk1_c+risk1_d+risk1_e+risk1_f)/7
*gen reciprocity = (reciprocity1+reciprocity2+reciprocity3+reciprocity4)/4

//// SVO slider measure
//svo_type (Altruistic - 1, Prosocial - 2, Individualistic - 3, Competitive - 4)

*gen individualist = 0 if svo_type==3
*replace individualist = 1 if svo_type!=3

//// Demographic and social characteristics
// age
//nationality
*gen international = 1 
*replace international = 0 if nationality=="Dutch"
*replace international = 0 if nationality=="dutch"
*replace international = 0 if nationality=="NL"
*replace international = 0 if nationality=="nl"

// gender
*gen gender_num = 0 if gender=="Female"
*replace gender_num = 1 if gender=="Male"

// game_theory - knowledge in game theory
*gen gt_knowledge = 0 if game_theory=="No;"
*replace gt_knowledge = 1 if game_theory!="No;"

// experience - experimental participation
*gen lab_experience = 0 if experience=="No"
*replace lab_experience = 1 if experience=="Yes, once or twice"
*replace lab_experience = 2 if experience=="Yes, more than two times but less than five"
*replace lab_experience = 3 if experience=="Yes, five times or more"

// understanding the instructions - exp1 (0-9, not difficult - very diffcult)


/// RC: demographics 
meqrlogit choice_RI i.inv_cost ///
c.age i.gender_num i.international || subject_id: , var

meqrlogit choice_CA i.inv_cost ///
c.age i.gender_num i.international || subject_id: , var

meqrlogit decision_coop i.condition i.inv_cost ///
c.age i.gender_num i.international || subject_id: , var

/// RC: social orientations 
meqrlogit choice_RI i.inv_cost ///
c.trust c.risk c.reciprocity i.individualist || subject_id: , var

meqrlogit choice_CA i.inv_cost ///
c.trust c.risk c.reciprocity i.individualist || subject_id: , var

meqrlogit decision_coop i.condition i.inv_cost ///
c.trust c.risk c.reciprocity i.individualist || subject_id: , var

/// RC: knowledge and experience
meqrlogit choice_RI i.inv_cost ///
i.gt_knowledge i.lab_experience c.exp1 || subject_id: , var

meqrlogit choice_CA i.inv_cost ///
i.gt_knowledge i.lab_experience c.exp1 || subject_id: , var

meqrlogit decision_coop i.condition i.inv_cost ///
i.gt_knowledge i.lab_experience c.exp1 || subject_id: , var

/// RC: session characteristics 
meqrlogit choice_RI i.inv_cost ///
session_size i.RepeatFirst || subject_id: , var

meqrlogit choice_CA i.inv_cost ///
session_size i.RepeatFirst || subject_id: , var

meqrlogit decision_coop i.condition i.inv_cost ///
session_size i.RepeatFirst || subject_id: , var

/// RC: all together
meqrlogit choice_RI i.inv_cost ///
c.age i.gender_num i.international c.trust c.risk c.reciprocity i.individualist ///
i.gt_knowledge i.lab_experience c.exp1 session_size i.RepeatFirst || subject_id: , var

meqrlogit choice_CA i.inv_cost ///
c.age i.gender_num i.international c.trust c.risk c.reciprocity i.individualist ///
i.gt_knowledge i.lab_experience c.exp1 session_size i.RepeatFirst || subject_id: , var

meqrlogit decision_coop i.condition i.inv_cost ///
c.age i.gender_num i.international c.trust c.risk c.reciprocity i.individualist ///
i.gt_knowledge i.lab_experience c.exp1 session_size i.RepeatFirst || subject_id: , var

///////////////
//decisions under forced choices
*gen forced_choice = 0
*replace forced_choice = 1 if Treatment<=4 & choice==1 & condition==1
*replace forced_choice = 2 if Treatment>=5 & choice==1 & condition==2

tab forced_choice

keep if forced_choice<1

melogit choice_RI i.inv_cost  c.payoff_p2_cntr c.embexp_p2_cntr part2 || subject_id:

melogit choice_CA i.inv_cost  c.payoff_p2_cntr c.embexp_p2_cntr part2 || subject_id:

melogit decision_coop i.condition i.inv_cost || subject_id:

use inv-paper_data.dta, clear


