library(jsonlite)
HHdat<-fromJSON('Prueba3_Baseline_Clean_JSON.json')


hhnames<-HHdat$codigo_hogar

#Progress out of Poverty index Guatamala

#Progress out of Poverty index Guatamala

PPI_under13<-rowSums(cbind(HHdat$malesunder7,HHdat$femalesunder7,HHdat$males7to13,HHdat$females7to13),na.rm=TRUE)
PPI_under13[PPI_under13>5]<-5
PPI_females<-HHdat$femalesover20
PPI_work<-HHdat$offfarm_incomes_any
PPI_educationfemale<-HHdat$educacion_jhm
PPI_education_kids<-HHdat$inscripcion_escolar
PPI_floor_material<-HHdat$material_casa
PPI_fridge<-HHdat$refri
PPI_stove_electric<-HHdat$estufa
PPI_electric_iron<-HHdat$plancha
PPI_stonemill<-HHdat$piedra
#PPI_biganimals<-list(c())
#PPI_biganimals[[1]]<-HHdat$livestock_name.1
#PPI_biganimals[[2]]<-HHdat$livestock_name.2
#PPI_biganimals[[3]]<-HHdat$livestock_name.3
#PPI_biganimals[[4]]<-HHdat$livestock_name.4
#PPI_biganimals[[5]]<-HHdat$livestock_name.5
#PPI_biganimals[[6]]<-HHdat$livestock_name.6


#PPI-scores
PPIscore<-list()
PPIscore$under13<-cbind(c(NA,0,1,2,3,4,5),c(33,33,23,17,12,10,0))
PPIscore$education<-cbind(c('NO',NA,'SI'),c(0,2,6))
PPIscore$readwrite<-cbind(c('N','Y','NO HEAD'),c(0,6,9))
PPIscore$job<-cbind(c('Y','N'),c(0,5))
PPIscore$construction<-cbind(c(NA,'material1','material2','material3','material4'),c(0,0,3,9,15))
PPIscore$fridge<-cbind(c(NA,'NO','SI'),c(0,0,9))
PPIscore$electricstove<-cbind(c(NA,'NO','SI'),c(0,0,8))
PPIscore$stonemill<-cbind(c(NA,'SI','NO'),c(0,0,3))
PPIscore$electriciron<-cbind(c(NA,'NO','SI'),c(0,0,8))
PPIscore$biganimals<-cbind(c(NA,'N','Y','not in agriculture'),c(0,0,3,4))


#FANTA food security index
foodshortagetime<-HHdat$foodshortagetime
foodshortagetime_months_amount<-HHdat$foodshortagetime_months_amount
foodshortagetime_months_which<-HHdat$foodshortagetime_months_which
food_securtiy_frequencies_hungryseason<-HHdat$food_securtiy_frequencies_hungryseason
food_securtiy_frequencies_normalseason<-HHdat$food_securtiy_frequencies_normalseason
food_worry<-HHdat$food_worry
food_notpreferredfood<-HHdat$food_notpreferredfood
food_limitvariety<-HHdat$food_limitvariety
food_eatnastyfood<-HHdat$food_eatnastyfood
food_smallermeals<-HHdat$food_smallermeals
food_skipmeals<-HHdat$food_skipmeals
food_nofoodinhouse<-HHdat$food_nofoodinhouse
food_sleephungry<-HHdat$food_sleephungry
food_24hrsnofood<-HHdat$food_24hrsnofood
scoring_HFIAS<-cbind(c(NA,'never','fewpermonth','fewperweek','daily'),c(0,0,1,2,3))

#nutrition diversity score
Nutr_diff_between_seasons<-HHdat$foodshortagetime


bad_season.Grains<-HHdat$Grains
bad_season.Roots_tubers<-HHdat$Roots_tubers
bad_season.Vegetables<-HHdat$Vegetables
bad_season.Fruits<-HHdat$Fruits
bad_season.Legumes<-HHdat$Legumes
bad_season.Meat<-HHdat$Meat
bad_season.Fish<-HHdat$Fish
bad_season.Eggs<-HHdat$Eggs
bad_season.Milk_Dairy<-HHdat$Milk_Dairy
bad_season.Fats<-HHdat$Fats
bad_season.Sweets<-HHdat$Sweets
bad_season.Other<-HHdat$Other

good_season.Grains<-HHdat$Grains_goodseason
good_season.Roots_tubers<-HHdat$Roots_tubers_goodseason
good_season.Vegetables<-HHdat$Vegetables_goodseason
good_season.Fruits<-HHdat$Fruits_goodseason
good_season.Legumes<-HHdat$Legumes_goodseason
good_season.Meat<-HHdat$Meat_goodseason
good_season.Fish<-HHdat$Fish_goodseason
good_season.Eggs<-HHdat$Eggs_goodseason
good_season.Milk_Dairy<-HHdat$Milk_Dairy_goodseason
good_season.Fats<-HHdat$Fats_goodseason
good_season.Sweets<-HHdat$Sweets_goodseason
good_season.Other<-HHdat$Other_goodseason




worry_score<-array(0,length(unique(hhnames)),1)
notpreferredfood_score<-array(0,length(unique(hhnames)),1)
limitvariety_score<-array(0,length(unique(hhnames)),1)
eatnastyfood_score<-array(0,length(unique(hhnames)),1)
smallermeals_score<-array(0,length(unique(hhnames)),1)
skipmeals_score<-array(0,length(unique(hhnames)),1)
nofoodinhouse_score<-array(0,length(unique(hhnames)),1)
sleephungry_score<-array(0,length(unique(hhnames)),1)
hrsnofood_score<-array(0,length(unique(hhnames)),1)


#set score variables to zero
score_PPI<-array(0,length(unique(hhnames)),1)
score_HFIAS<-array(0,length(unique(hhnames)),1)
score_HDDS_good<-array(0,length(unique(hhnames)),1)
score_HDDS_bad<-array(0,length(unique(hhnames)),1)

for (k in 1:length(hhnames)) {
   index<-k
     
#   #under13
   score_PPI[k]<-score_PPI[k]+PPIscore$under13[PPIscore$under13[,1]%in%PPI_under13[index],2]
#   #education
   score_PPI[k]<-score_PPI[k]+as.numeric(PPIscore$education[PPIscore$education[,1]%in%PPI_education_kids[index],2])
   #readwrite
   #if no female spouse
   if (is.na(PPI_educationfemale[index])) {
      readwrite_var<-'NO HEAD'
   } else {
      readwrite_var<-ifelse((PPI_educationfemale[index]=='edu1'),'N','Y')
   }
   score_PPI[k]<-score_PPI[k]+as.numeric(PPIscore$readwrite[PPIscore$readwrite[,1]%in%readwrite_var,2])
   #work
   if (!is.na(PPI_work[index])) {
      PPI_work_sc<-ifelse((PPI_work[index]=='otherfarms_community')|(PPI_work[index]=='otherfarms_outside')|(PPI_work[index]=='domestic'),PPI_work_sc<-'Y',PPI_work_sc<-'N')
   } else {
      PPI_work_sc<-'N'
   }
   score_PPI[k]<-score_PPI[k]+as.numeric(PPIscore$job[PPIscore$job[,1]%in%PPI_work_sc,2])
   #construction
   score_PPI[k]<-score_PPI[k]+as.numeric(PPIscore$construction[PPIscore$construction[,1]%in%PPI_floor_material[index],2])
   #refrigerator
   score_PPI[k]<-score_PPI[k]+as.numeric(PPIscore$fridge[PPIscore$fridge[,1]%in%PPI_fridge[index],2])
   #electric stove
   score_PPI[k]<-score_PPI[k]+as.numeric(PPIscore$electricstove[PPIscore$electricstove[,1]%in%PPI_fridge[index],2])
   #stone mill
   score_PPI[k]<-score_PPI[k]+as.numeric(PPIscore$stonemill[PPIscore$stonemill[,1]%in%PPI_stonemill[index],2])
   #electric iron
   score_PPI[k]<-score_PPI[k]+as.numeric(PPIscore$electriciron[PPIscore$electriciron[,1]%in%PPI_electric_iron[index],2])
   #big animals?
#   biganimals<-c('cows','donkeys','oxen','buffalo','horses','pigs','calves','bulls')
#   have_biganimal<-'N'
#   for (j1 in 1:6) {
#       if (!is.na(PPI_biganimals[[j1]][index])) {
#          for (j2 in 1:length(biganimals)) {
#              ifelse(grepl(biganimals[j2],PPI_biganimals[[j1]][index]),have_biganimal<-'Y',0)
#          }
#       }
#   }
#   score_PPI[k]<-score_PPI[k]+as.numeric(PPIscore$biganimals[PPIscore$biganimals[,1]%in%have_biganimal,2])
#   havebiganimals[index]<-have_biganimal

   #FANTA HFIAS score
   worry_score[k]<-as.numeric(scoring_HFIAS[scoring_HFIAS[,1]%in%food_worry[k],2])
   notpreferredfood_score[k]<-as.numeric(scoring_HFIAS[scoring_HFIAS[,1]%in%food_notpreferredfood[k],2])
   limitvariety_score[k]<-as.numeric(scoring_HFIAS[scoring_HFIAS[,1]%in%food_limitvariety[k],2])
   eatnastyfood_score[k]<-as.numeric(scoring_HFIAS[scoring_HFIAS[,1]%in%food_eatnastyfood[k],2])
   smallermeals_score[k]<-as.numeric(scoring_HFIAS[scoring_HFIAS[,1]%in%food_smallermeals[k],2])
   skipmeals_score[k]<-as.numeric(scoring_HFIAS[scoring_HFIAS[,1]%in%food_skipmeals[k],2])
   nofoodinhouse_score[k]<-as.numeric(scoring_HFIAS[scoring_HFIAS[,1]%in%food_nofoodinhouse[k],2])
   sleephungry_score[k]<-as.numeric(scoring_HFIAS[scoring_HFIAS[,1]%in%food_sleephungry[k],2])
   hrsnofood_score[k]<-as.numeric(scoring_HFIAS[scoring_HFIAS[,1]%in%food_24hrsnofood[k],2])

   score_HFIAS[k]<-worry_score[k]+notpreferredfood_score[k]+limitvariety_score[k]+eatnastyfood_score[k]+smallermeals_score[k]+skipmeals_score[k]+nofoodinhouse_score[k]+sleephungry_score[k]+hrsnofood_score[k]
 
   #nutrition diversity score
   if (!is.na(Nutr_diff_between_seasons[index]) & Nutr_diff_between_seasons[index]=='Y'){
        bad_season.Grains_sc<-ifelse((bad_season.Grains[index]=='daily'|bad_season.Grains[index]=='fewperweek'),1,0)
        bad_season.Roots_tubers_sc<-ifelse((bad_season.Roots_tubers[index]=='daily'|bad_season.Roots_tubers[index]=='fewperweek'),1,0)
        bad_season.Vegetables_sc<-ifelse((bad_season.Vegetables[index]=='daily'|bad_season.Vegetables[index]=='fewperweek'),1,0)
        bad_season.Fruits_sc<-ifelse((bad_season.Fruits[index]=='daily'|bad_season.Fruits[index]=='fewperweek'),1,0)
        bad_season.Legumes_sc<-ifelse((bad_season.Legumes[index]=='daily'|bad_season.Legumes[index]=='fewperweek'),1,0)
        bad_season.Meat_sc<-ifelse((bad_season.Meat[index]=='daily'|bad_season.Meat[index]=='fewperweek'),1,0)
        bad_season.Fish_sc<-ifelse((bad_season.Fish[index]=='daily'|bad_season.Fish[index]=='fewperweek'),1,0)
        bad_season.Eggs_sc<-ifelse((bad_season.Eggs[index]=='daily'|bad_season.Eggs[index]=='fewperweek'),1,0)
        bad_season.Milk_Dairy_sc<-ifelse((bad_season.Milk_Dairy[index]=='daily'|bad_season.Milk_Dairy[index]=='fewperweek'),1,0)
        bad_season.Fats_sc<-ifelse((bad_season.Fats[index]=='daily'|bad_season.Fats[index]=='fewperweek'),1,0)
        bad_season.Sweets_sc<-ifelse((bad_season.Sweets[index]=='daily'|bad_season.Sweets[index]=='fewperweek'),1,0)
        bad_season.Other_sc<-ifelse((bad_season.Other[index]=='daily'|bad_season.Other[index]=='fewperweek'),1,0)

        score_HDDS_bad[k]<-bad_season.Grains_sc+bad_season.Roots_tubers_sc+bad_season.Vegetables_sc+bad_season.Fruits_sc+bad_season.Legumes_sc+bad_season.Meat_sc+bad_season.Fish_sc+bad_season.Eggs_sc+bad_season.Milk_Dairy_sc+bad_season.Fats_sc+bad_season.Sweets_sc+bad_season.Other_sc

    } else {
        bad_season.Grains_sc<-ifelse((good_season.Grains[index]=='daily'|good_season.Grains[index]=='fewperweek'),1,0)
        bad_season.Roots_tubers_sc<-ifelse((good_season.Roots_tubers[index]=='daily'|good_season.Roots_tubers[index]=='fewperweek'),1,0)
        bad_season.Vegetables_sc<-ifelse((good_season.Vegetables[index]=='daily'|good_season.Vegetables[index]=='fewperweek'),1,0)
        bad_season.Fruits_sc<-ifelse((good_season.Fruits[index]=='daily'|good_season.Fruits[index]=='fewperweek'),1,0)
        bad_season.Legumes_sc<-ifelse((good_season.Legumes[index]=='daily'|good_season.Legumes[index]=='fewperweek'),1,0)
        bad_season.Meat_sc<-ifelse((good_season.Meat[index]=='daily'|good_season.Meat[index]=='fewperweek'),1,0)
        bad_season.Fish_sc<-ifelse((good_season.Fish[index]=='daily'|good_season.Fish[index]=='fewperweek'),1,0)
        bad_season.Eggs_sc<-ifelse((good_season.Eggs[index]=='daily'|good_season.Eggs[index]=='fewperweek'),1,0)
        bad_season.Milk_Dairy_sc<-ifelse((good_season.Milk_Dairy[index]=='daily'|good_season.Milk_Dairy[index]=='fewperweek'),1,0)
        bad_season.Fats_sc<-ifelse((good_season.Fats[index]=='daily'|good_season.Fats[index]=='fewperweek'),1,0)
        bad_season.Sweets_sc<-ifelse((good_season.Sweets[index]=='daily'|good_season.Sweets[index]=='fewperweek'),1,0)
        bad_season.Other_sc<-ifelse((good_season.Other[index]=='daily'|good_season.Other[index]=='fewperweek'),1,0)

        score_HDDS_bad[k]<-bad_season.Grains_sc+bad_season.Roots_tubers_sc+bad_season.Vegetables_sc+bad_season.Fruits_sc+bad_season.Legumes_sc+bad_season.Meat_sc+bad_season.Fish_sc+bad_season.Eggs_sc+bad_season.Milk_Dairy_sc+bad_season.Fats_sc+bad_season.Sweets_sc+bad_season.Other_sc
    }
    good_season.Grains_sc<-ifelse((good_season.Grains[index]=='daily'|good_season.Grains[index]=='fewperweek'),1,0)
    good_season.Roots_tubers_sc<-ifelse((good_season.Roots_tubers[index]=='daily'|good_season.Roots_tubers[index]=='fewperweek'),1,0)
    good_season.Vegetables_sc<-ifelse((good_season.Vegetables[index]=='daily'|good_season.Vegetables[index]=='fewperweek'),1,0)
    good_season.Fruits_sc<-ifelse((good_season.Fruits[index]=='daily'|good_season.Fruits[index]=='fewperweek'),1,0)
    good_season.Legumes_sc<-ifelse((good_season.Legumes[index]=='daily'|good_season.Legumes[index]=='fewperweek'),1,0)
    good_season.Meat_sc<-ifelse((good_season.Meat[index]=='daily'|good_season.Meat[index]=='fewperweek'),1,0)
    good_season.Fish_sc<-ifelse((good_season.Fish[index]=='daily'|good_season.Fish[index]=='fewperweek'),1,0)
    good_season.Eggs_sc<-ifelse((good_season.Eggs[index]=='daily'|good_season.Eggs[index]=='fewperweek'),1,0)
    good_season.Milk_Dairy_sc<-ifelse((good_season.Milk_Dairy[index]=='daily'|good_season.Milk_Dairy[index]=='fewperweek'),1,0)
    good_season.Fats_sc<-ifelse((good_season.Fats[index]=='daily'|good_season.Fats[index]=='fewperweek'),1,0)
    good_season.Sweets_sc<-ifelse((good_season.Sweets[index]=='daily'|good_season.Sweets[index]=='fewperweek'),1,0)
    good_season.Other_sc<-ifelse((good_season.Other[index]=='daily'|good_season.Other[index]=='fewperweek'),1,0)

    score_HDDS_good[k]<-good_season.Grains_sc+good_season.Roots_tubers_sc+good_season.Vegetables_sc+good_season.Fruits_sc+good_season.Legumes_sc+good_season.Meat_sc+good_season.Fish_sc+good_season.Eggs_sc+good_season.Milk_Dairy_sc+good_season.Fats_sc+good_season.Sweets_sc+good_season.Other_sc

}

#specific HFIAS test
HFIA_score<-array('0',length(worry_score),1)
k<-(worry_score<3)&(notpreferredfood_score<1)&(limitvariety_score<1)&(eatnastyfood_score<1)&(smallermeals_score<1)&(skipmeals_score<1)&(nofoodinhouse_score<1)&(sleephungry_score<1)&(hrsnofood_score<1)
HFIA_score[k]<-'food secure' 
k<-((worry_score>1)|(notpreferredfood_score>0)|(limitvariety_score==1)|(eatnastyfood_score==1))&(smallermeals_score<1)&(skipmeals_score<1)&(nofoodinhouse_score<1)&(sleephungry_score<1)&(hrsnofood_score<1)
HFIA_score[k]<-'mildly food insecure' 
k<-((limitvariety_score>1)|(eatnastyfood_score>1)|(smallermeals_score==1)|(smallermeals_score==2)|(skipmeals_score==1)|(skipmeals_score==2)|(nofoodinhouse_score==1)|(nofoodinhouse_score==2))&(sleephungry_score<1)&(hrsnofood_score<1)
HFIA_score[k]<-'moderately food insecure' 
k<-((smallermeals_score>2)|(skipmeals_score>2)|(nofoodinhouse_score>0)|(sleephungry_score>0)|(hrsnofood_score>0))
HFIA_score[k]<-'severely food insecure' 


HFIAS_grouping<-HFIA_score
HHdat<-cbind(HHdat,score_HFIAS,HFIAS_grouping,score_PPI,score_HDDS_good,score_HDDS_bad)

#cat(toJSON(HHdat),file="Prueba3_Clean_withindicators.json")

# define what you want to keep
tokeep <- c('HHdat','score_HFIAS','HFIAS_grouping','score_PPI','score_HDDS_good','score_HDDS_bad')
# calculate what you want to remove
toremove <- setdiff(ls(), tokeep)

rm(list = c(toremove, 'toremove'))

