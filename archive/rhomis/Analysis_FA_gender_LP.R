

library(jsonlite)


HHdat <- fromJSON("./input/rhomis/LargaPrueba3Julio23_results_7-9-2015.json")

names(HHdat)

hhnames<-HHdat$codigo_hogar
Lvstdat<-HHdat$livestock_repeat
Cropdat<-HHdat$crop_repeat
Offfarmdat<-HHdat$offfarm_income_repeat
Beandat<-HHdat$frijoles_repeat

#unique_hhnames<-unique(hhnames)
unique_hhnames<-hhnames

#set parameter values and initialise key variables
source('./input/rhomis/FA_initparam.R')

land_area<-HHdat$tierra_produccion
for (i in 1: length(pars$area_units[,1])) {
   index<-HHdat$tierra_unidades==pars$area_units[i,1]&!is.na(HHdat$tierra_unidades)
   land_area[index]<-land_area[index]*as.numeric(pars$area_units[i,2])
}

cropdiv<-c()
crop_fertinput<-c()

for (counter in 1:length(unique_hhnames)) {
  print(counter)
  index<-which(hhnames==unique_hhnames[counter])
  index<-index[1]
     
   #diversification
   cropdiv<-c(cropdiv,as.numeric(HHdat$crop_count[index]))
   crop_fertinput<-c(crop_fertinput,as.numeric(HHdat$fertiliser_amount[index]))

   #HH size in adult equivalents
   
   AdultEquivalent[counter]<-sum(pars$coeffMY0.13*(HHdat$malesunder7[index]+HHdat$males7to13[index]),pars$coeffFY0.13*(HHdat$femalesunder7[index]+HHdat$females7to13[index]),pars$coeffMY13.19*HHdat$males14to19[index],pars$coeffFY13.19*HHdat$females14to19[index],pars$coeffMY19.59*HHdat$malesover20[index],pars$coeffFY19.59*HHdat$femalesover20[index],na.rm=TRUE)

   #land area
   land.ha[counter]<-land_area[index]
   
   #select right crops with right information
   cropset<-Cropdat[[index]]
   crop_y<-Cropdat[[index]]$crop_yield
   crop_c<-crop_y*0
   crop_sw<-crop_y*0
   for (j in 1:length(crop_y)) {
     cropN<-cropset$crop_name[j]
     ifelse(cropN=='other1',cropN<-as.character(HHdat$crops_other1[index]),0)
     ifelse(cropN=='other2',cropN<-as.character(HHdat$crops_other2[index]),0)
     ifelse(cropN=='other3',cropN<-as.character(HHdat$crops_other3[index]),0)

     ifelse(cropN=='Ca?a'|cropN=='Cultivo de caña'|cropN=='Cultivo de ca?a'|cropN=='Caña',cropN<-'sugarcane',0)

     if (!is.na(Cropdat[[index]]$crop_yield_units[j])) { 
         conv_f<-pars$weight_units[Cropdat[[index]]$crop_yield_units[j]==pars$weight_units[,1], 2]
     } else {
         conv_f<-1
     }
     crop_y[j]<-crop_y[j]*as.numeric(conv_f)
     
     if (cropset$crop_use[j]=='use') {
         crop_c[j]<-crop_y[j]
         crop_sw[j]<-0
     } else {
        if (cropset$crop_use[j]=='sell') {
           crop_sw[j]<-crop_y[j]
           crop_c[j]<-0
        } else {
           if (!is.na(cropset$crop_sold_quantity[j])) {
               crop_sw[j]<-cropset$crop_sold_quantity[j]*as.numeric(conv_f)
               crop_c[j]<-pmax(0,crop_y[j]-crop_sw[j])
           } else {
               crop_sw[j]<-0.5*crop_y[j]
               crop_c[j]<-0.5*crop_y[j]
           }
        }
      }
        
      crop_energy_values<-read.csv('./input/rhomis/MIS_Survey_CropEnergy.csv')
      crop_prices<-read.csv('./input/rhomis/Trifinio_CropPrice.csv')
   
      indy<-crop_prices[,1]%in%cropN
      crop_cashvalue<-crop_prices[indy,2]

      indy<-crop_energy_values[,1]%in%cropN
      crop_energyvalue<-crop_energy_values[indy,2]
     
      #gender differentiated control information on crops
      source('./input/rhomis/Gender_eating_crop.R')
      source('./input/rhomis/Gender_sales_crop.R') 

      #key crop variables
      crop_Eval_prod[counter]<-crop_Eval_prod[counter]+crop_energyvalue*crop_y[j]
      crop_Eval_cons[counter]<-crop_Eval_cons[counter]+crop_energyvalue*crop_c[j]

      M_gender_crop_Eval_cons[counter]<-M_gender_crop_Eval_cons[counter]+crop_energyvalue*crop_c[j]*male_score_cons
      F_gender_crop_Eval_cons[counter]<-F_gender_crop_Eval_cons[counter]+crop_energyvalue*crop_c[j]*female_score_cons

      crop_Cashval_sold[counter]<-crop_Cashval_sold[counter]+crop_cashvalue*crop_sw[j]
      crop_Cashval_prod[counter]<-crop_Cashval_prod[counter]+crop_cashvalue*crop_y[j]
      M_gender_crop_Cashval_sold[counter]<-M_gender_crop_Cashval_sold[counter]+crop_cashvalue*crop_sw[j]*male_score_cons
      F_gender_crop_Cashval_sold[counter]<-F_gender_crop_Cashval_sold[counter]+crop_cashvalue*crop_sw[j]*female_score_cons
   }

   #beans are recorded separately
   source('./input/rhomis/run_beans_analysis.R')  
   crop_prices<-read.csv('./input/rhomis/Trifinio_CropPrice.csv')
   
   indy<-crop_prices[,1]%in%'beans'
   beans_cashvalue<-crop_prices[indy,2]
   indy<-crop_energy_values[,1]%in%'beans'
   beans_energyvalue<-crop_energy_values[indy,2]
     
   source('./input/rhomis/Gender_eating_beans.R')   
   source('./input/rhomis/Gender_sales_beans.R')   
   
   #add beans to key crop variables
   crop_Eval_prod[counter]<-crop_Eval_prod[counter]+beans_energyvalue*bean_y
   crop_Eval_cons[counter]<-crop_Eval_cons[counter]+beans_energyvalue*bean_c

   M_gender_crop_Eval_cons[counter]<-M_gender_crop_Eval_cons[counter]+crop_energyvalue*bean_c*male_score_cons
   F_gender_crop_Eval_cons[counter]<-F_gender_crop_Eval_cons[counter]+crop_energyvalue*bean_c*female_score_cons

   crop_Cashval_sold[counter]<-crop_Cashval_sold[counter]+beans_cashvalue*bean_sw
   crop_Cashval_prod[counter]<-crop_Cashval_prod[counter]+beans_cashvalue*bean_y
   M_gender_crop_Cashval_sold[counter]<-M_gender_crop_Cashval_sold[counter]+crop_cashvalue*bean_sw*male_score_cons
   F_gender_crop_Cashval_sold[counter]<-F_gender_crop_Cashval_sold[counter]+crop_cashvalue*bean_sw*female_score_cons


   #livestock
   lvstset<-Lvstdat[[index]]
   if (ncol(lvstset)>0) {

      #livestock species by livestock species
      for (j in 1:length(lvstset[,1])) {
       
         lvstN<-lvstset$livestock_name[j]
         ifelse(lvstN=='other1',lvstN<-as.character(HHdat$livestock_other1[index]),0)
         ifelse(lvstN=='other2',lvstN<-as.character(HHdat$livestock_other2[index]),0)
         ifelse(lvstN=='other3',lvstN<-as.character(HHdat$livestock_other3[index]),0)


         if (!is.na(lvstN)&lvstN!=""&(lvstset$animals_count[j]>0|lvstset$beehive_count[j]>0)) {
            if (!is.na(lvstset$animals_count[j]>0)) { 
               tlu[counter]<-tlu[counter]+as.numeric(pars$tlu_set[pars$tlu_set[,1]%in%lvstN,2])*lvstset$animals_count[j]
            }
            source('./input/rhomis/Lvst_cons_sales.r')
             
            #calculate all key variables
            lvst_Cashval_sold[counter]<-lvst_Cashval_sold[counter]+sales_wholesale[1]+sales_meat[1]+sales_eggs[1]+sales_milk[1]+sales_cheese[1]+sales_butter[1]+sales_cream[1]+sales_honey[1]
            M_gender_lvst_Cashval_sold[counter]<-male_control$sales_wholesale*sales_wholesale[1]+male_control$sales_meat*sales_meat[1]+male_control$sales_eggs*sales_eggs[1]+male_control$sales_milk*sales_milk[1]+male_control$sales_cheese*sales_cheese[1]+male_control$sales_butter*sales_butter[1]+male_control$sales_cream*sales_cream[1]
            F_gender_lvst_Cashval_sold[counter]<-female_control$sales_wholesale*sales_wholesale[1]+female_control$sales_meat*sales_meat[1]+female_control$sales_eggs*sales_eggs[1]+female_control$sales_milk*sales_milk[1]+female_control$sales_cheese*sales_cheese[1]+female_control$sales_butter*sales_butter[1]+female_control$sales_cream*sales_cream[1]
            lvst_Cashval_prod[counter]<-lvst_Cashval_prod[counter]+sales_wholesale[1]+sales_meat[2]+sales_eggs[2]+sales_milk[2]+sales_cheese[2]+sales_butter[2]+sales_cream[2]+sales_honey[2]

            lvst_Eval_cons[counter]<-lvst_Eval_cons[counter]+cons_meat[1]+cons_eggs[1]+cons_milk[1]+cons_cheese[1]+cons_butter[1]+cons_cream[1]+cons_honey[1]
            M_gender_lvst_Eval_cons[counter]<-male_control$cons_meat*cons_meat[1]+male_control$cons_eggs*cons_eggs[1]+male_control$cons_milk*cons_milk[1]+male_control$cons_cheese*cons_cheese[1]+male_control$cons_butter*cons_butter[1]+male_control$cons_cream*cons_cream[1]
            F_gender_lvst_Eval_cons[counter]<-female_control$cons_meat*cons_meat[1]+female_control$cons_eggs*cons_eggs[1]+female_control$cons_milk*cons_milk[1]+female_control$cons_cheese*cons_cheese[1]+female_control$cons_butter*cons_butter[1]+female_control$cons_cream*cons_cream[1]
            lvst_Eval_prod[counter]<-lvst_Eval_prod[counter]+cons_meat[2]+cons_eggs[2]+cons_milk[2]+cons_cheese[2]+cons_butter[2]+cons_cream[2]+cons_honey[2]
        }
    }
  }
}

#off farm income
for (counter in 1:length(lvst_Cashval_sold)) {
  index<-counter
  total_income_farm[counter]<-lvst_Cashval_sold[counter]+crop_Cashval_sold[counter]
  total_value_farmprod[counter]<-lvst_Cashval_prod[counter]+crop_Cashval_prod[counter]
  off_farmprop<-HHdat$offfarm_total_income_proportion[index]
    pars$offfarm_classes<-cbind(c('All','Most','Half','UnderHalf','Little'),c(0.9,0.7,0.5,0.2,0.1))
    if (!is.na(off_farmprop)) {
        offfarm_income[counter]<-(total_value_farmprod[counter]*as.numeric(pars$offfarm_classes[pars$offfarm_classes[,1]%in%off_farmprop,2]))/(1-as.numeric(pars$offfarm_classes[pars$offfarm_classes[,1]%in%off_farmprop,2]))
    } else {
        offfarm_income[counter]<-0
    }
    
    #gender differentiated control
    source('./input/rhomis/Gender_offfarm.R')
    Offarm_cash_malecontrol[counter]<-male_score_labour*offfarm_income[counter]
    Offarm_cash_femalecontrol[counter]<-female_score_labour*offfarm_income[counter]
}

#calculate FA indicator
staple_cropN<-'maize'
indy<-crop_prices[,1]%in%staple_cropN
staplecrop_price<-crop_prices[indy,2]
indy<-crop_energy_values[,1]%in%staple_cropN
staplecrop_energyvalue<-crop_energy_values[indy,2]

total_income<-total_income_farm + offfarm_income

kgs_staple_crop_bought<-total_income/staplecrop_price
kgs_staple_crop_marketorientation<-total_income_farm/staplecrop_price
                
energy_staple_crop_bought<-kgs_staple_crop_bought*staplecrop_energyvalue
energy_staple_crop_marketorientation<-kgs_staple_crop_marketorientation*staplecrop_energyvalue
energy_fromlvst<-lvst_Eval_cons+(lvst_Cashval_sold)/staplecrop_price*staplecrop_energyvalue

total_Eval<-energy_staple_crop_bought+lvst_Eval_cons+crop_Eval_cons
FA_value<-total_Eval/AdultEquivalent/365

energy_bought<-energy_staple_crop_bought
crop_production_Eval<-crop_Eval_cons+crop_Cashval_sold*staplecrop_energyvalue/staplecrop_price
totalfarm_produce<-total_Eval-staplecrop_energyvalue*offfarm_income/staplecrop_price

crop_production_Eval<-crop_Eval_cons+crop_Cashval_sold*staplecrop_energyvalue/staplecrop_price

#gender control score, for now only in energy terms
Female_control_energy<-staplecrop_energyvalue*F_gender_lvst_Cashval_sold/staplecrop_price+F_gender_lvst_Eval_cons+staplecrop_energyvalue*F_gender_crop_Cashval_sold/staplecrop_price+F_gender_crop_Eval_cons
Male_control_energy<-staplecrop_energyvalue*M_gender_lvst_Cashval_sold/staplecrop_price+M_gender_lvst_Eval_cons+staplecrop_energyvalue*M_gender_crop_Cashval_sold/staplecrop_price+M_gender_crop_Eval_cons

rel_Female_control_energy<-Female_control_energy/(Female_control_energy+Male_control_energy)
rel_Male_control_energy<-Male_control_energy/(Female_control_energy+Male_control_energy)

F_cons_control<-F_gender_crop_Eval_cons/(F_gender_crop_Eval_cons+M_gender_crop_Eval_cons)
F_sold_control<-F_gender_crop_Cashval_sold/(F_gender_crop_Cashval_sold+M_gender_crop_Cashval_sold)



#AdultEquivalent<-AdultEquivalent



#AdultEquivalent<-AdultEquivalent

#extra variables
#energy from off farm income
energy_offfarm_income<-offfarm_income/staplecrop_price*staplecrop_energyvalue
energy_cropsales<-crop_Cashval_sold*staplecrop_energyvalue/staplecrop_price
energy_livestocksales<-(lvst_Cashval_sold)*staplecrop_energyvalue/staplecrop_price
foodselfsufficiency<-(crop_Eval_cons+lvst_Eval_cons)/AdultEquivalent/365

#gender control score, for now only in energy terms
Female_control_energy<-staplecrop_energyvalue*F_gender_lvst_Cashval_sold/staplecrop_price+F_gender_lvst_Eval_cons+staplecrop_energyvalue*F_gender_crop_Cashval_sold/staplecrop_price+F_gender_crop_Eval_cons
Male_control_energy<-staplecrop_energyvalue*M_gender_lvst_Cashval_sold/staplecrop_price+M_gender_lvst_Eval_cons+staplecrop_energyvalue*M_gender_crop_Cashval_sold/staplecrop_price+M_gender_crop_Eval_cons

rel_Female_control_energy<-Female_control_energy/(pmax(0.1,(Female_control_energy+Male_control_energy)))
rel_Male_control_energy<-Male_control_energy/(pmax(0,1,(Female_control_energy+Male_control_energy)))

#rel_importance_wildfood<-wildfood_energy/(pmax(0.1,total_Eval))

