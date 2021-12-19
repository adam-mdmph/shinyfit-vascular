# Clean variables for PVI_International_PROC
# Make sure to name dataset "STF"
# Required packages Hmisc

library(Hmisc)

STF <- STF2

## Demographics----

STF <- STF %>%
	mutate(GENDER = ifelse(is.na(GENDER), '3', GENDER))
STF$GENDER <- factor(STF$GENDER, levels = c(1, 2, 3), labels = c("Male", "Female", "Unknown"))

STF <- STF %>%
	mutate(ETHNICITY = ifelse(is.na(ETHNICITY), '2', ETHNICITY))
STF$ETHNICITY <- factor(STF$ETHNICITY, levels = c(0, 1, 2), labels = c("Not Hispanic", "Hispanic", "Unknown"))

STF$RACE <- factor(STF$RACE, levels = c(1, 2, 3, 4, 5, 6, 7), 
                       labels = c("American Indian", "Asian", "Black", "Native Hawaiin/Islander", "White",
                                  "More than 1 race", "Unknown"))

STF <- STF %>%
	mutate(LIVINGSTATUS = ifelse(is.na(LIVINGSTATUS), '4', LIVINGSTATUS))
STF$LIVINGSTATUS <- factor(STF$LIVINGSTATUS, levels = c(1,2,3,4), labels = c("Home", "Nursing Home", "Homeless",
																																						 "Unknown"))


## Comorbidities----
STF <- mutate(STF, BMI = WTKG / (HTCM / 100)^2)
STF$BMIclass <- cut(STF$BMI, breaks = c(0, 18.5, 25.0, 30, 100), labels = c("Underweight", "Normal", "Pre-Obesity", "Obese"))
STF <- STF %>%
	mutate(BMIclass = ifelse(is.na(BMIclass), '5', BMIclass))
STF$BMIclass <- factor(STF$BMIclass, levels = c(1,2,3,4,5), labels = c("Underweight", "Normal", "Pre-Obesity", 
																																		 "Obese", "Unknown"))
STF <- STF %>%
	mutate(PREOP_FUNCSTATUS = ifelse(is.na(PREOP_FUNCSTATUS), '5', PREOP_FUNCSTATUS))
STF$PREOP_FUNCSTATUS <- factor(STF$PREOP_FUNCSTATUS, 
															 levels = c(0,1,2,3,4,5), 
															 labels = c("Full", "Light Work", "Self Care", "Assisted Care", "Bed Bound", "Unknown"))

summary(STF2$PREOP_FUNCSTATUS)
																																												 																																												 

STF$PREOP_AMBUL <- factor(STF$PREOP_AMBUL, levels = c(1,2,3,4,5,6), 
													labels = c("Ambulatory", "Ambulatory w. assistance", "Wheelchair", "Bedridden", "Ambulatory w. prosthesis", "Ambulatory w. Assistance/Prosthesis"))

STF$PRIOR_CVD <- factor(STF$PRIOR_CVD, levels = c(0,1,2,3),
                         labels = c("None", "Hx Stroke, Asymptomatic", "Hx Stroke, Minor Deficit", "Hx Stroke, Major Deficit"))


STF$PRIOR_CAD <- factor(STF$PRIOR_CAD, levels = c(0, 1, 2, 3, 4, 5), 
                         labels = c("None", "History of MI but no Sx", "Stable Angina", "Unstable Angina or MI less than 6 mos", 
                                    "MI less than 6 mos", "Unstable Angina"))


STF$PRIOR_CHF <- factor(STF$PRIOR_CHF, levels = c(0, 1, 2, 3, 4), labels = c("None", "Asymptomatic, Hx CHF", "Mild", "Moderate", "Severe"))

STF$PREOP_DYSRHYTHMIA <- factor(STF$PREOP_DYSRHYTHMIA, levels = c(0,1,2,3,4), labels = c("No", "Yes, AFib/Flutter", "Yes, ventricular", "Yes, Pacemaker", "Yes, ICD"))

STF$COPD <- factor(STF$COPD, levels = c(0, 1, 2, 3), labels = c("No", "Not Treated", "On Meds", "On Home Oxygen"))

STF$PREOP_DIABETES <- factor(STF$PREOP_DIABETES, levels = c(0, 1, 2, 3), labels = c("None", "Diet", "Non-insulin Meds", "Insulin"))

STF$DIALYSIS <- factor(STF$DIALYSIS, levels = c(0, 1, 2), labels = c("No", "Functioning Transplant", "On Dialysis"))

STF$HTN <- factor(STF$HTN, levels = c(0, 1), labels = c("No", "Yes")) 

STF$PREOP_SMOKING <- factor(STF$PREOP_SMOKING, levels = c(0, 1, 2), labels = c("Never", "Prior", "Current"))

STF$STRESS <- factor(STF$STRESS, levels = c(0,1,2,3,4), labels = c("Not Done", "Normal", "Positive, Ischemia", "Positive, MI", "Positive, Both"))

## Pre-operative Medications- Dictionary Rows- 44 to 49----
STF$PREOP_ACE <- factor(STF$PREOP_ACE, levels = c(0, 1, 2, 3), labels = c("No", "Yes", "No, Medical Reason", "Non-compliant"))

STF$PREOP_ASA <-factor(STF$PREOP_ASA, levels = c(0,1,2,3), labels = c("No", "Yes", "No, for medical reason", "Non-compliant"))

STF$PREOP_ANTICOAG <- factor(STF$PREOP_ANTICOAG, levels = c(0, 7, 8, 9, 4, 5, 6), 
                                 labels = c("None", "Vitamin K Antagonist", "Thrombin Inhibitor", "Factor Xa Inhibitor", "Other", "No, Medical Reason", "Non-compliant"))

STF$PREOP_P2Y <- factor(STF$PREOP_P2Y, levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8), 
                                 labels = c("None", "Clopidogrel", "Prasugrel", "Ticlopidine", "Ticagrelor", "Other", "No, Medical Reason", "Non-compliant", "PAR1 Inhibitor"))
                            
STF$PREOP_STATIN <- factor(STF$PREOP_STATIN, levels = c(0,1,2,3), labels = c("No", "Yes", "No, Medical Reason", "Non-Compliant"))

STF$PREOP_CILOSTAZOL <- factor(STF$PREOP_CILOSTAZOL, levels = c(0,1,2,3), labels = c("No", "Yes", "No, Medical Reason", "Non-Compliant"))

## Surgical History- Dictionary Rows- 50 to 76----
STF$PRIOR_CABG <- factor(STF$PRIOR_CABG, levels = c(0,1,2), labels = c("None", "Less than 5yrs", "More than 5yrs"))

STF$PRIOR_PCI <- factor(STF$PRIOR_PCI, levels = c(0,1,2), labels = c("None", "Less than 5yrs", "More than 5yrs"))

STF$PRIOR_CEACAS <- factor(STF$PRIOR_CEACAS, levels = c(0,1,2,3), labels = c("Neither", "CEA", "CAS", "Both"))

STF$PRIOR_ANEURYSM <- factor(STF$PRIOR_ANEURYSM, levels = c(0,1,2,3,6), labels = c("None", "Aortic", "Other", "Both", "Yes"))

STF$PRIOR_BYPENDARPVI <- factor(STF$PRIOR_BYPENDARPVI, levels = c(0,1), labels = c("No", "Yes"))

STF$PRIOR_AMP <- factor(STF$PRIOR_AMP, levels = c(0,1), labels = c("No", "Yes"))

STF$INFLOWTX_R <- factor(STF$INFLOWTX_R, levels = c(0,1,2,3), labels = c("No", "PVI", "Bypass", "Both"))

STF$INFLOWTX_L <- factor(STF$INFLOWTX_L, levels = c(0,1,2,3), labels = c("No", "PVI", "Bypass", "Both"))

STF$LEGTX_R <- factor(STF$LEGTX_R, levels = c(0,1,2,3), labels = c("No", "PVI", "Bypass", "Both"))

STF$LEGTX_L <- factor(STF$LEGTX_L, levels = c(0,1,2,3), labels = c("No", "PVI", "Bypass", "Both"))

STF$PRIOR_AMP_R <- factor(STF$PRIOR_AMP_R, levels = c(0,1,2,3,4,5,6,7), labels = c("No", "Toes", "Transmet/Midfoot", "BK or Thru Knee", "AK or Higher", 
                                                                                           "Minor Amputation", "Major Amputation", "Both Minor and Major Amp"))

STF$PRIOR_AMP_L <- factor(STF$PRIOR_AMP_L, levels = c(0,1,2,3,4,5,6,7), labels = c("No", "Toes", "Transmet/Midfoot", "BK or Thru Knee", "AK or Higher", 
                                                                                           "Minor Amputation", "Major Amputation", "Both Minor and Major Amp"))

STF$FEMENDART <- factor(STF$FEMENDART, levels = c(0,1,2,3), labels = c("No", "Right", "Left", "Both"))

STF$LEGSYMP_R <- factor(STF$LEGSYMP_R, levels = c(0,1,2,3,4,5,6,7,8,9,10,11),
                         labels = c("Asymptomatic", "Claudication", "Moderate Claudication", "Tissue Loss", "Ischemic Rest Pain",
                                    "Ulcer/necrosis", "Non-healing Amputation", "Both Ulcer/Non-healing Amp", "Acute Ischemia", "Mild Claudication", 
                                    "Severe Claudication", "Not Treated"))

STF$LEGSYMP_L <- factor(STF$LEGSYMP_L, levels = c(0,1,2,3,4,5,6,7,8,9,10,11),
                         labels = c("Asymptomatic", "Claudication", "Moderate Claudication", "Tissue Loss", "Ischemic Rest Pain",
                                    "Ulcer/necrosis", "Non-healing Amputation", "Both Ulcer/Non-healing Amp", "Acute Ischemia", "Mild Claudication", 
                                    "Severe Claudication", "Not Treated"))

STF$BYPPAT_1 <- factor(STF$BYPPAT_1, levels = c(0,1), labels = c("No", "Yes"))

STF$BYPPAT_2 <- factor(STF$BYPPAT_2, levels = c(0,1), labels = c("No", "Yes"))

STF$ACUTEISCHEMIA_R <- factor(STF$ACUTEISCHEMIA_R, levels = c(1,2,3,4), labels = c("Viable", "Marginally Threatened", "Immediately Threatened", "Irreversible"))

STF$ACUTEISCHEMIA_L <- factor(STF$ACUTEISCHEMIA_L, levels = c(1,2,3,4), labels = c("Viable", "Marginally Threatened", "Immediately Threatened", "Irreversible"))

STF$TISSUELOSS_SEV_R <- factor(STF$TISSUELOSS_SEV_R, levels = c(1,2,3), labels = c("Grade 1- Shallow", "Grade 2- Deep", "Grade 3- Extensive"))

STF$TISSUELOSS_SEV_L <- factor(STF$TISSUELOSS_SEV_L, levels = c(1,2,3), labels = c("Grade 1- Shallow", "Grade 2- Deep", "Grade 3- Extensive"))

STF$INFECTION_R <- factor(STF$INFECTION_R, levels = c(0,1,2,3), labels = c("Grade 0- None", "Grade 1- Mild", "Grade 2- Moderate", "Grade 3- Severe"))

STF$INFECTION_L <- factor(STF$INFECTION_L, levels = c(0,1,2,3), labels = c("Grade 0- None", "Grade 1- Mild", "Grade 2- Moderate", "Grade 3- Severe"))


## Procedure- Dictionary Rows- 77 to 353 (GENERAL and OCCLUSIVE VARIABLES)----
STF$SURGYEAR <- factor(STF$SURGYEAR)
STF$SETTING <- factor(STF$SETTING, levels = c(1,2,3,4),
											labels = c("Hospital Outpatient", "Hospital Inpatient", "Ambulatory Center", "Office"))

STF$URGENCY <- factor(STF$URGENCY, levels = c(1,2,3,4), labels = c("Elective", "Urgent", "Emergent", "Emergent"))


STF$NUMACCSITES <- factor(STF$NUMACCSITES)

STF$ACCSIDE_1 <- factor(STF$ACCSIDE_1, levels = c(1,2), labels = c("Right", "Left"))

STF$ACCSIDE_2 <- factor(STF$ACCSIDE_2, levels = c(1,2), labels = c("Right", "Left"))

STF$CLOSUREDEVSUCCESS_1 <- factor(STF$CLOSUREDEVSUCCESS_1, levels = c(0,1), labels = c("No", "Yes"))

STF$CLOSUREDEVSUCCESS_2 <- factor(STF$CLOSUREDEVSUCCESS_2, levels = c(0,1), labels = c("No", "Yes"))

STF$CCENDAR_1 <- factor(STF$CCENDAR_1, levels = c(0,1,2), labels = c("No", "Yes, Primary", "Yes, Patch"))

STF$CCENDAR_2 <- factor(STF$CCENDAR_2, levels = c(0,1,2), labels = c("No", "Yes, Primary", "Yes, Patch"))

STF$PROTAMINE <- factor(STF$PROTAMINE, levels = c(0,1), labels = c("No", "Yes"))

STF$NUMARTTREAT <- factor(STF$NUMARTTREAT)

STF$PVI_INDICATION_1 <- factor(STF$PVI_INDICATION_1, levels = c(1,2,3,4),
                                labels = c("Occlusive Disease", "Aneurysm", "Occlusive or Aneurysm", "None"))
STF$PVI_INDICATION_2 <- factor(STF$PVI_INDICATION_2, levels = c(1,2,3,4),
                                labels = c("Occlusive Disease", "Aneurysm", "Occlusive or Aneurysm", "None"))
STF$PVI_INDICATION_3 <- factor(STF$PVI_INDICATION_3, levels = c(1,2,3,4),
                                labels = c("Occlusive Disease", "Aneurysm", "Occlusive or Aneurysm", "None"))
STF$PVI_INDICATION_4 <- factor(STF$PVI_INDICATION_4, levels = c(1,2,3,4),
                                labels = c("Occlusive Disease", "Aneurysm", "Occlusive or Aneurysm", "None"))

STF$ARTERY_1 <- factor(STF$ARTERY_1, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16),
                        labels = c("Aorta", "Common Iliac", "External Iliac", "Common and External", "Common Femoral",
                                   "Profunda", "SFA", "Popliteal", "SFA and Pop", "Anterior Tibial", "TP Trunk", "Posterior Tibial", 
                                   "Peroneal", "Dorsal Pedal", "Plantar", "Internal Iliac"))

STF$ARTERY_2 <- factor(STF$ARTERY_2, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16),
                        labels = c("Aorta", "Common Iliac", "External Iliac", "Common and External", "Common Femoral",
                                   "Profunda", "SFA", "Popliteal", "SFA and Pop", "Anterior Tibial", "TP Trunk", "Posterior Tibial", 
                                   "Peroneal", "Dorsal Pedal", "Plantar", "Internal Iliac"))

STF$ARTERY_3 <- factor(STF$ARTERY_3, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16),
                        labels = c("Aorta", "Common Iliac", "External Iliac", "Common and External", "Common Femoral",
                                   "Profunda", "SFA", "Popliteal", "SFA and Pop", "Anterior Tibial", "TP Trunk", "Posterior Tibial", 
                                   "Peroneal", "Dorsal Pedal", "Plantar", "Internal Iliac"))

STF$ARTERY_4 <- factor(STF$ARTERY_4, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16),
                        labels = c("Aorta", "Common Iliac", "External Iliac", "Common and External", "Common Femoral",
                                   "Profunda", "SFA", "Popliteal", "SFA and Pop", "Anterior Tibial", "TP Trunk", "Posterior Tibial", 
                                   "Peroneal", "Dorsal Pedal", "Plantar", "Internal Iliac"))

STF$PROCSIDE_1 <- factor(STF$PROCSIDE_1, levels = c(1,2,3), 
                          labels = c("Right", "Left", "Aorta"))
STF$PROCSIDE_2 <- factor(STF$PROCSIDE_2, levels = c(1,2,3), 
                          labels = c("Right", "Left", "Aorta"))
STF$PROCSIDE_3 <- factor(STF$PROCSIDE_3, levels = c(1,2,3), 
                          labels = c("Right", "Left", "Aorta"))
STF$PROCSIDE_4 <- factor(STF$PROCSIDE_4, levels = c(1,2,3), 
                          labels = c("Right", "Left", "Aorta"))

STF$PRIOR_TXSITE_1 <- factor(STF$PRIOR_TXSITE_1, levels = c(0,1,2,3,4,5),
                              labels = c("No", "Yes, PTA", "Yes, Atherectomy", "Yes, Stent", "Yes, Other", "Unsure"))
STF$PRIOR_TXSITE_2 <- factor(STF$PRIOR_TXSITE_2, levels = c(0,1,2,3,4,5),
                              labels = c("No", "Yes, PTA", "Yes, Atherectomy", "Yes, Stent", "Yes, Other", "Unsure"))
STF$PRIOR_TXSITE_3 <- factor(STF$PRIOR_TXSITE_3, levels = c(0,1,2,3,4,5),
                              labels = c("No", "Yes, PTA", "Yes, Atherectomy", "Yes, Stent", "Yes, Other", "Unsure"))
STF$PRIOR_TXSITE_4 <- factor(STF$PRIOR_TXSITE_4, levels = c(0,1,2,3,4,5),
                              labels = c("No", "Yes, PTA", "Yes, Atherectomy", "Yes, Stent", "Yes, Other", "Unsure"))

STF$TASCGRADE_1 <- factor(STF$TASCGRADE_1, levels = c(1,2,3,4,5),
                           labels = c("A","B", "C", "D", "Protect Adjacent Vessel"))
STF$TASCGRADE_2 <- factor(STF$TASCGRADE_2, levels = c(1,2,3,4,5),
                           labels = c("A","B", "C", "D", "Protect Adjacent Vessel"))
STF$TASCGRADE_3 <- factor(STF$TASCGRADE_3, levels = c(1,2,3,4,5),
                           labels = c("A","B", "C", "D", "Protect Adjacent Vessel"))
STF$TASCGRADE_4 <- factor(STF$TASCGRADE_4, levels = c(1,2,3,4,5),
                           labels = c("A","B", "C", "D", "Protect Adjacent Vessel"))

STF$CALC_1 <- factor(STF$CALC_1, levels = c(0,1,2,3,4,5),
                      labels = c("None", "Focal", "Mild", "Moderate", "Severe", "Not Evaluated"))
STF$CALC_2 <- factor(STF$CALC_2, levels = c(0,1,2,3,4,5),
                      labels = c("None", "Focal", "Mild", "Moderate", "Severe", "Not Evaluated"))
STF$CALC_3 <- factor(STF$CALC_3, levels = c(0,1,2,3,4,5),
                      labels = c("None", "Focal", "Mild", "Moderate", "Severe", "Not Evaluated"))
STF$CALC_4 <- factor(STF$CALC_4, levels = c(0,1,2,3,4,5),
                      labels = c("None", "Focal", "Mild", "Moderate", "Severe", "Not Evaluated"))

STF$NUMDEV_1 <- factor(STF$NUMDEV_1, levels = c(1,2,3), labels = c("1", "2", "3"))
STF$NUMDEV_2 <- factor(STF$NUMDEV_2, levels = c(1,2,3), labels = c("1", "2", "3"))
STF$NUMDEV_3 <- factor(STF$NUMDEV_3, levels = c(1,2,3), labels = c("1", "2", "3"))
STF$NUMDEV_4 <- factor(STF$NUMDEV_4, levels = c(1,2,3), labels = c("1", "2", "3"))


##Treatment type Artery 1, Device 1-3
STF$TXTYPE_1 <- factor(STF$TXTYPE_1, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), 
                          labels = c("Plain Balloon", "Self Expanding Stent", "Balloon Expandable Stent", "Stent Graft",
                                     "Cryoplasty", "Cutting Balloon", "Laser Atherectomy", "Mechanical Atherectomy", "Mechanical Atherectomy",
                                     "Special Balloon", "Stent", "Atherectomy", "Not Able to Treat", "Bailout Stent", "Bailout Stent Graft"))

STF$TXTYPE_2 <- factor(STF$TXTYPE_2, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), 
                          labels = c("Plain Balloon", "Self Expanding Stent", "Balloon Expandable Stent", "Stent Graft",
                                     "Cryoplasty", "Cutting Balloon", "Laser Atherectomy", "Mechanical Atherectomy", "Mechanical Atherectomy",
                                     "Special Balloon", "Stent", "Atherectomy", "Not Able to Treat", "Bailout Stent", "Bailout Stent Graft"))

STF$TXTYPE_3 <- factor(STF$TXTYPE_3, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), 
                          labels = c("Plain Balloon", "Self Expanding Stent", "Balloon Expandable Stent", "Stent Graft",
                                     "Cryoplasty", "Cutting Balloon", "Laser Atherectomy", "Mechanical Atherectomy", "Mechanical Atherectomy",
                                     "Special Balloon", "Stent", "Atherectomy", "Not Able to Treat", "Bailout Stent", "Bailout Stent Graft"))


##Treatment type Artery 2, Device 1-3
STF$TXTYPE_4 <- factor(STF$TXTYPE_4, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), 
                          labels = c("Plain Balloon", "Self Expanding Stent", "Balloon Expandable Stent", "Stent Graft",
                                     "Cryoplasty", "Cutting Balloon", "Laser Atherectomy", "Mechanical Atherectomy", "Mechanical Atherectomy",
                                     "Special Balloon", "Stent", "Atherectomy", "Not Able to Treat", "Bailout Stent", "Bailout Stent Graft"))

STF$TXTYPE_5 <- factor(STF$TXTYPE_5, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), 
                          labels = c("Plain Balloon", "Self Expanding Stent", "Balloon Expandable Stent", "Stent Graft",
                                     "Cryoplasty", "Cutting Balloon", "Laser Atherectomy", "Mechanical Atherectomy", "Mechanical Atherectomy",
                                     "Special Balloon", "Stent", "Atherectomy", "Not Able to Treat", "Bailout Stent", "Bailout Stent Graft"))

STF$TXTYPE_6 <- factor(STF$TXTYPE_6, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), 
                          labels = c("Plain Balloon", "Self Expanding Stent", "Balloon Expandable Stent", "Stent Graft",
                                     "Cryoplasty", "Cutting Balloon", "Laser Atherectomy", "Mechanical Atherectomy", "Mechanical Atherectomy",
                                     "Special Balloon", "Stent", "Atherectomy", "Not Able to Treat", "Bailout Stent", "Bailout Stent Graft"))

##Treatment type Artery 3, Device 1-3
STF$TXTYPE_7 <- factor(STF$TXTYPE_7, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), 
                          labels = c("Plain Balloon", "Self Expanding Stent", "Balloon Expandable Stent", "Stent Graft",
                                     "Cryoplasty", "Cutting Balloon", "Laser Atherectomy", "Mechanical Atherectomy", "Mechanical Atherectomy",
                                     "Special Balloon", "Stent", "Atherectomy", "Not Able to Treat", "Bailout Stent", "Bailout Stent Graft"))

STF$TXTYPE_8 <- factor(STF$TXTYPE_8, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), 
                          labels = c("Plain Balloon", "Self Expanding Stent", "Balloon Expandable Stent", "Stent Graft",
                                     "Cryoplasty", "Cutting Balloon", "Laser Atherectomy", "Mechanical Atherectomy", "Mechanical Atherectomy",
                                     "Special Balloon", "Stent", "Atherectomy", "Not Able to Treat", "Bailout Stent", "Bailout Stent Graft"))

STF$TXTYPE_9 <- factor(STF$TXTYPE_9, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), 
                          labels = c("Plain Balloon", "Self Expanding Stent", "Balloon Expandable Stent", "Stent Graft",
                                     "Cryoplasty", "Cutting Balloon", "Laser Atherectomy", "Mechanical Atherectomy", "Mechanical Atherectomy",
                                     "Special Balloon", "Stent", "Atherectomy", "Not Able to Treat", "Bailout Stent", "Bailout Stent Graft"))


##Treatment type Artery 4, Device 1-3
STF$TXTYPE_10 <- factor(STF$TXTYPE_10, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), 
                           labels = c("Plain Balloon", "Self Expanding Stent", "Balloon Expandable Stent", "Stent Graft",
                                      "Cryoplasty", "Cutting Balloon", "Laser Atherectomy", "Mechanical Atherectomy", "Mechanical Atherectomy",
                                      "Special Balloon", "Stent", "Atherectomy", "Not Able to Treat", "Bailout Stent", "Bailout Stent Graft"))

STF$TXTYPE_11 <- factor(STF$TXTYPE_11, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), 
                           labels = c("Plain Balloon", "Self Expanding Stent", "Balloon Expandable Stent", "Stent Graft",
                                      "Cryoplasty", "Cutting Balloon", "Laser Atherectomy", "Mechanical Atherectomy", "Mechanical Atherectomy",
                                      "Special Balloon", "Stent", "Atherectomy", "Not Able to Treat", "Bailout Stent", "Bailout Stent Graft"))

STF$TXTYPE_12 <- factor(STF$TXTYPE_12, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), 
                           labels = c("Plain Balloon", "Self Expanding Stent", "Balloon Expandable Stent", "Stent Graft",
                                      "Cryoplasty", "Cutting Balloon", "Laser Atherectomy", "Mechanical Atherectomy", "Mechanical Atherectomy",
                                      "Special Balloon", "Stent", "Atherectomy", "Not Able to Treat", "Bailout Stent", "Bailout Stent Graft"))

STF$THROMBTIMING_ANEUR_1 <- factor(STF$THROMBTIMING_ANEUR_1, levels = c(1,2,3), labels = c("Prior Procedure", "Planned, Current", "Complication, Current"))
STF$THROMBTIMING_ANEUR_2 <- factor(STF$THROMBTIMING_ANEUR_2, levels = c(1,2,3), labels = c("Prior Procedure", "Planned, Current", "Complication, Current"))
STF$THROMBTIMING_ANEUR_3 <- factor(STF$THROMBTIMING_ANEUR_3, levels = c(1,2,3), labels = c("Prior Procedure", "Planned, Current", "Complication, Current"))
STF$THROMBTIMING_ANEUR_4 <- factor(STF$THROMBTIMING_ANEUR_4, levels = c(1,2,3), labels = c("Prior Procedure", "Planned, Current", "Complication, Current"))


#Concomitant Procedures
STF$CONCOMITANT_1_0[is.na(STF$CONCOMITANT_1_0)] <-0
STF$CONCOMITANT_1_1[is.na(STF$CONCOMITANT_1_1)] <-0
STF$CONCOMITANT_1_2[is.na(STF$CONCOMITANT_1_2)] <-0
STF$CONCOMITANT_1_3[is.na(STF$CONCOMITANT_1_3)] <-0
STF$CONCOMITANT_1_4[is.na(STF$CONCOMITANT_1_4)] <-0
STF$CONCOMITANT_1_5[is.na(STF$CONCOMITANT_1_5)] <-0
STF$CONCOMITANT_1_6[is.na(STF$CONCOMITANT_1_6)] <-0
STF$CONCOMITANT_1_7[is.na(STF$CONCOMITANT_1_7)] <-0
STF$CONCOMITANT_1_8[is.na(STF$CONCOMITANT_1_8)] <-0

STF$CONCOMITANT_2_0[is.na(STF$CONCOMITANT_2_0)] <-0
STF$CONCOMITANT_2_1[is.na(STF$CONCOMITANT_2_1)] <-0
STF$CONCOMITANT_2_2[is.na(STF$CONCOMITANT_2_2)] <-0
STF$CONCOMITANT_2_3[is.na(STF$CONCOMITANT_2_3)] <-0
STF$CONCOMITANT_2_4[is.na(STF$CONCOMITANT_2_4)] <-0
STF$CONCOMITANT_2_5[is.na(STF$CONCOMITANT_2_5)] <-0
STF$CONCOMITANT_2_6[is.na(STF$CONCOMITANT_2_6)] <-0
STF$CONCOMITANT_2_7[is.na(STF$CONCOMITANT_2_7)] <-0
STF$CONCOMITANT_2_8[is.na(STF$CONCOMITANT_2_8)] <-0

STF$CONCOMITANT_3_0[is.na(STF$CONCOMITANT_3_0)] <-0
STF$CONCOMITANT_3_1[is.na(STF$CONCOMITANT_3_1)] <-0
STF$CONCOMITANT_3_2[is.na(STF$CONCOMITANT_3_2)] <-0
STF$CONCOMITANT_3_3[is.na(STF$CONCOMITANT_3_3)] <-0
STF$CONCOMITANT_3_4[is.na(STF$CONCOMITANT_3_4)] <-0
STF$CONCOMITANT_3_5[is.na(STF$CONCOMITANT_3_5)] <-0
STF$CONCOMITANT_3_6[is.na(STF$CONCOMITANT_3_6)] <-0
STF$CONCOMITANT_3_7[is.na(STF$CONCOMITANT_3_7)] <-0
STF$CONCOMITANT_3_8[is.na(STF$CONCOMITANT_3_8)] <-0

STF$CONCOMITANT_4_0[is.na(STF$CONCOMITANT_4_0)] <-0
STF$CONCOMITANT_4_1[is.na(STF$CONCOMITANT_4_1)] <-0
STF$CONCOMITANT_4_2[is.na(STF$CONCOMITANT_4_2)] <-0
STF$CONCOMITANT_4_3[is.na(STF$CONCOMITANT_4_3)] <-0
STF$CONCOMITANT_4_4[is.na(STF$CONCOMITANT_4_4)] <-0
STF$CONCOMITANT_4_5[is.na(STF$CONCOMITANT_4_5)] <-0
STF$CONCOMITANT_4_6[is.na(STF$CONCOMITANT_4_6)] <-0
STF$CONCOMITANT_4_7[is.na(STF$CONCOMITANT_4_7)] <-0
STF$CONCOMITANT_4_8[is.na(STF$CONCOMITANT_4_8)] <-0


#Concomitant 1_0 is None
STF$CONCOMITANT_1_0 <- factor(STF$CONCOMITANT_1_0, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_2_0 <- factor(STF$CONCOMITANT_2_0, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_3_0 <- factor(STF$CONCOMITANT_3_0, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_4_0 <- factor(STF$CONCOMITANT_4_0, levels = c(0,1), labels = c("No", "Yes"))

#Concomitant 1_1 is Thrombolysis Pharmcologic
STF$CONCOMITANT_1_1 <- factor(STF$CONCOMITANT_1_1, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_2_1 <- factor(STF$CONCOMITANT_2_1, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_3_1 <- factor(STF$CONCOMITANT_3_1, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_4_1 <- factor(STF$CONCOMITANT_4_1, levels = c(0,1), labels = c("No", "Yes"))

#Concomitant 1_2 is Thrombolysis Mechanical
STF$CONCOMITANT_1_2 <- factor(STF$CONCOMITANT_1_2, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_2_2 <- factor(STF$CONCOMITANT_2_2, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_3_2 <- factor(STF$CONCOMITANT_3_2, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_4_2 <- factor(STF$CONCOMITANT_4_2, levels = c(0,1), labels = c("No", "Yes"))

#Concomitant 1_3 is Embolic Protection Device
STF$CONCOMITANT_1_3 <- factor(STF$CONCOMITANT_1_3, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_2_3 <- factor(STF$CONCOMITANT_2_3, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_3_3 <- factor(STF$CONCOMITANT_3_3, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_4_3 <- factor(STF$CONCOMITANT_4_3, levels = c(0,1), labels = c("No", "Yes"))

#Concomitant 1_4 is IVUS
STF$CONCOMITANT_1_4 <- factor(STF$CONCOMITANT_1_4, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_2_4 <- factor(STF$CONCOMITANT_2_4, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_3_4 <- factor(STF$CONCOMITANT_3_4, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_4_4 <- factor(STF$CONCOMITANT_4_4, levels = c(0,1), labels = c("No", "Yes"))

#Concomitant 1_5 is CTO Device
STF$CONCOMITANT_1_5 <- factor(STF$CONCOMITANT_1_5, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_2_5 <- factor(STF$CONCOMITANT_2_5, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_3_5 <- factor(STF$CONCOMITANT_3_5, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_4_5 <- factor(STF$CONCOMITANT_4_5, levels = c(0,1), labels = c("No", "Yes"))

#Concomitant 1_6 is Suction Thrombectomy
STF$CONCOMITANT_1_6 <- factor(STF$CONCOMITANT_1_6, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_2_6 <- factor(STF$CONCOMITANT_2_6, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_3_6 <- factor(STF$CONCOMITANT_3_6, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_4_6 <- factor(STF$CONCOMITANT_4_6, levels = c(0,1), labels = c("No", "Yes"))

#Concomitant 1_7 is Concomitant Infra Bypass
STF$CONCOMITANT_1_7 <- factor(STF$CONCOMITANT_1_7, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_2_7 <- factor(STF$CONCOMITANT_2_7, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_3_7 <- factor(STF$CONCOMITANT_3_7, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_4_7 <- factor(STF$CONCOMITANT_4_7, levels = c(0,1), labels = c("No", "Yes"))

#Concomitant 1_8 is Concomitant Supra Bypass
STF$CONCOMITANT_1_8 <- factor(STF$CONCOMITANT_1_8, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_2_8 <- factor(STF$CONCOMITANT_2_8, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_3_8 <- factor(STF$CONCOMITANT_3_8, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_4_8 <- factor(STF$CONCOMITANT_4_8, levels = c(0,1), labels = c("No", "Yes"))


STF$THROMBTIMING_OCC_1[is.na(STF$THROMBTIMING_OCC_1)] <- 0 
STF$THROMBTIMING_OCC_2[is.na(STF$THROMBTIMING_OCC_2)] <- 0
STF$THROMBTIMING_OCC_3[is.na(STF$THROMBTIMING_OCC_3)] <- 0
STF$THROMBTIMING_OCC_4[is.na(STF$THROMBTIMING_OCC_4)] <- 0

STF$THROMBTIMING_OCC_1 <- factor(STF$THROMBTIMING_OCC_1, levels = c(0,1,2,3), labels = c("None", "Prior Procedure", "Planned, Current", "Complication, Current"))
STF$THROMBTIMING_OCC_2 <- factor(STF$THROMBTIMING_OCC_2, levels = c(0,1,2,3), labels = c("None", "Prior Procedure", "Planned, Current", "Complication, Current"))
STF$THROMBTIMING_OCC_3 <- factor(STF$THROMBTIMING_OCC_3, levels = c(0,1,2,3), labels = c("None", "Prior Procedure", "Planned, Current", "Complication, Current"))
STF$THROMBTIMING_OCC_4 <- factor(STF$THROMBTIMING_OCC_4, levels = c(0,1,2,3), labels = c("None", "Prior Procedure", "Planned, Current", "Complication, Current"))



##Timing of suction thrombectomy to categorical data
STF$SUCTHROMB_1[is.na(STF$SUCTHROMB_1)] <- 0 
STF$SUCTHROMB_2[is.na(STF$SUCTHROMB_2)] <- 0
STF$SUCTHROMB_3[is.na(STF$SUCTHROMB_3)] <- 0
STF$SUCTHROMB_4[is.na(STF$SUCTHROMB_4)] <- 0

STF$SUCTHROMB_1 <- factor(STF$SUCTHROMB_1, levels = c(0,1,2,3), labels = c("None", "Prior Procedure", "Planned, Current", "Complication, Current"))
STF$SUCTHROMB_2 <- factor(STF$SUCTHROMB_2, levels = c(0,1,2,3), labels = c("None", "Prior Procedure", "Planned, Current", "Complication, Current"))
STF$SUCTHROMB_3 <- factor(STF$SUCTHROMB_3, levels = c(0,1,2,3), labels = c("None", "Prior Procedure", "Planned, Current", "Complication, Current"))
STF$SUCTHROMB_4 <- factor(STF$SUCTHROMB_4, levels = c(0,1,2,3), labels = c("None", "Prior Procedure", "Planned, Current", "Complication, Current"))

## Technical Result Occlusive
STF$TECHRESULT_1 <- factor(STF$TECHRESULT_1, levels = c(1,2,3,4,5), labels = c("Successful- stenosis <= 30%", "Stenosis > 30%", "Target Lesion Occlusion", "Technical failure, unable to cross", "Stenosis > 30%"))
STF$TECHRESULT_2 <- factor(STF$TECHRESULT_2, levels = c(1,2,3,4,5), labels = c("Successful- stenosis <= 30%", "Stenosis > 30%", "Target Lesion Occlusion", "Technical failure, unable to cross", "Stenosis > 30%"))
STF$TECHRESULT_3 <- factor(STF$TECHRESULT_3, levels = c(1,2,3,4,5), labels = c("Successful- stenosis <= 30%", "Stenosis > 30%", "Target Lesion Occlusion", "Technical failure, unable to cross", "Stenosis > 30%"))
STF$TECHRESULT_4 <- factor(STF$TECHRESULT_4, levels = c(1,2,3,4,5), labels = c("Successful- stenosis <= 30%", "Stenosis > 30%", "Target Lesion Occlusion", "Technical failure, unable to cross", "Stenosis > 30%"))

STF$SUBSEQTX_1 <- factor(STF$SUBSEQTX_1, levels = c(0,1,2,3), labels = c("None", "Medical", "Interventional", "Surgical"))
STF$SUBSEQTX_2 <- factor(STF$SUBSEQTX_2, levels = c(0,1,2,3), labels = c("None", "Medical", "Interventional", "Surgical"))
STF$SUBSEQTX_3 <- factor(STF$SUBSEQTX_3, levels = c(0,1,2,3), labels = c("None", "Medical", "Interventional", "Surgical"))
STF$SUBSEQTX_4 <- factor(STF$SUBSEQTX_4, levels = c(0,1,2,3), labels = c("None", "Medical", "Interventional", "Surgical"))


#Number of stents used (Covered)
STF$NUMSTENTS_1 <- factor(STF$NUMSTENTS_1)
STF$NUMSTENTS_2 <- factor(STF$NUMSTENTS_2)
STF$NUMSTENTS_3 <- factor(STF$NUMSTENTS_3)
STF$NUMSTENTS_4 <- factor(STF$NUMSTENTS_4)

## Procedure- Dictionary Rows- 354 to 401 (Aneurysm VARIABLES)----
#Concomitant Procedures
#Artery 1
STF$CONCOMITANT_21_0[is.na(STF$CONCOMITANT_21_0)] <-0
STF$CONCOMITANT_21_1[is.na(STF$CONCOMITANT_21_1)] <-0
STF$CONCOMITANT_21_2[is.na(STF$CONCOMITANT_21_2)] <-0
STF$CONCOMITANT_21_3[is.na(STF$CONCOMITANT_21_3)] <-0
STF$CONCOMITANT_21_4[is.na(STF$CONCOMITANT_21_4)] <-0
STF$CONCOMITANT_21_6[is.na(STF$CONCOMITANT_21_6)] <-0
STF$CONCOMITANT_21_7[is.na(STF$CONCOMITANT_21_7)] <-0
STF$CONCOMITANT_21_8[is.na(STF$CONCOMITANT_21_8)] <-0

#Artery 2
STF$CONCOMITANT_24_0[is.na(STF$CONCOMITANT_24_0)] <-0
STF$CONCOMITANT_24_1[is.na(STF$CONCOMITANT_24_1)] <-0
STF$CONCOMITANT_24_2[is.na(STF$CONCOMITANT_24_2)] <-0
STF$CONCOMITANT_24_3[is.na(STF$CONCOMITANT_24_3)] <-0
STF$CONCOMITANT_24_4[is.na(STF$CONCOMITANT_24_4)] <-0
STF$CONCOMITANT_24_6[is.na(STF$CONCOMITANT_24_6)] <-0
STF$CONCOMITANT_24_7[is.na(STF$CONCOMITANT_24_7)] <-0
STF$CONCOMITANT_24_8[is.na(STF$CONCOMITANT_24_8)] <-0

#Artery 3
STF$CONCOMITANT_27_0[is.na(STF$CONCOMITANT_27_0)] <-0
STF$CONCOMITANT_27_1[is.na(STF$CONCOMITANT_27_1)] <-0
STF$CONCOMITANT_27_2[is.na(STF$CONCOMITANT_27_2)] <-0
STF$CONCOMITANT_27_3[is.na(STF$CONCOMITANT_27_3)] <-0
STF$CONCOMITANT_27_4[is.na(STF$CONCOMITANT_27_4)] <-0
STF$CONCOMITANT_27_6[is.na(STF$CONCOMITANT_27_6)] <-0
STF$CONCOMITANT_27_7[is.na(STF$CONCOMITANT_27_7)] <-0
STF$CONCOMITANT_27_8[is.na(STF$CONCOMITANT_27_8)] <-0

#Artery 4
STF$CONCOMITANT_210_0[is.na(STF$CONCOMITANT_210_0)] <-0
STF$CONCOMITANT_210_1[is.na(STF$CONCOMITANT_210_1)] <-0
STF$CONCOMITANT_210_2[is.na(STF$CONCOMITANT_210_2)] <-0
STF$CONCOMITANT_210_3[is.na(STF$CONCOMITANT_210_3)] <-0
STF$CONCOMITANT_210_4[is.na(STF$CONCOMITANT_210_4)] <-0
STF$CONCOMITANT_210_6[is.na(STF$CONCOMITANT_210_6)] <-0
STF$CONCOMITANT_210_7[is.na(STF$CONCOMITANT_210_7)] <-0
STF$CONCOMITANT_210_8[is.na(STF$CONCOMITANT_210_8)] <-0

#Concomitant 1_0 is None
STF$CONCOMITANT_21_0 <- factor(STF$CONCOMITANT_21_0, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_24_0 <- factor(STF$CONCOMITANT_24_0, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_27_0 <- factor(STF$CONCOMITANT_27_0, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_210_0 <- factor(STF$CONCOMITANT_210_0, levels = c(0,1), labels = c("No", "Yes"))

#Concomitant 1_1 is Thrombolysis Pharmcologic
STF$CONCOMITANT_21_1 <- factor(STF$CONCOMITANT_21_1, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_24_1 <- factor(STF$CONCOMITANT_24_1, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_27_1 <- factor(STF$CONCOMITANT_27_1, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_210_1 <- factor(STF$CONCOMITANT_210_1, levels = c(0,1), labels = c("No", "Yes"))

#Concomitant 1_2 is Thrombolysis Mechanical
STF$CONCOMITANT_21_2 <- factor(STF$CONCOMITANT_21_2, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_24_2 <- factor(STF$CONCOMITANT_24_2, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_27_2 <- factor(STF$CONCOMITANT_27_2, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_210_2 <- factor(STF$CONCOMITANT_210_2, levels = c(0,1), labels = c("No", "Yes"))

#Concomitant 1_3 is Embolic Protection Device
STF$CONCOMITANT_21_3 <- factor(STF$CONCOMITANT_21_3, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_24_3 <- factor(STF$CONCOMITANT_24_3, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_27_3 <- factor(STF$CONCOMITANT_27_3, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_210_3 <- factor(STF$CONCOMITANT_210_3, levels = c(0,1), labels = c("No", "Yes"))

#Concomitant 1_4 is IVUS
STF$CONCOMITANT_21_4 <- factor(STF$CONCOMITANT_21_4, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_24_4 <- factor(STF$CONCOMITANT_24_4, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_27_4 <- factor(STF$CONCOMITANT_27_4, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_210_4 <- factor(STF$CONCOMITANT_210_4, levels = c(0,1), labels = c("No", "Yes"))

#Concomitant 1_6 is Suction Thrombectomy
STF$CONCOMITANT_21_6 <- factor(STF$CONCOMITANT_21_6, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_24_6 <- factor(STF$CONCOMITANT_24_6, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_27_6 <- factor(STF$CONCOMITANT_27_6, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_210_6 <- factor(STF$CONCOMITANT_210_6, levels = c(0,1), labels = c("No", "Yes"))

#Concomitant 1_7 is Concomitant Infra Bypass
STF$CONCOMITANT_21_7 <- factor(STF$CONCOMITANT_21_7, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_24_7 <- factor(STF$CONCOMITANT_24_7, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_27_7 <- factor(STF$CONCOMITANT_27_7, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_210_7 <- factor(STF$CONCOMITANT_210_7, levels = c(0,1), labels = c("No", "Yes"))

#Concomitant 1_8 is Concomitant Supra Bypass
STF$CONCOMITANT_21_8 <- factor(STF$CONCOMITANT_21_8, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_24_8 <- factor(STF$CONCOMITANT_24_8, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_27_8 <- factor(STF$CONCOMITANT_27_8, levels = c(0,1), labels = c("No", "Yes"))
STF$CONCOMITANT_210_8 <- factor(STF$CONCOMITANT_210_8, levels = c(0,1), labels = c("No", "Yes"))




##Timing of suction thrombectomy to categorical data (Aneurysmal)
STF$SUCTHROMB_A1[is.na(STF$SUCTHROMB_A1)] <- 0 
STF$SUCTHROMB_A2[is.na(STF$SUCTHROMB_A2)] <- 0
STF$SUCTHROMB_A3[is.na(STF$SUCTHROMB_A3)] <- 0
STF$SUCTHROMB_A4[is.na(STF$SUCTHROMB_A4)] <- 0

STF$SUCTHROMB_A1 <- factor(STF$SUCTHROMB_A1, levels = c(0,1,2,3), labels = c("None", "Prior Procedure", "Planned, Current", "Complication, Current"))
STF$SUCTHROMB_A2 <- factor(STF$SUCTHROMB_A2, levels = c(0,1,2,3), labels = c("None", "Prior Procedure", "Planned, Current", "Complication, Current"))
STF$SUCTHROMB_A3 <- factor(STF$SUCTHROMB_A3, levels = c(0,1,2,3), labels = c("None", "Prior Procedure", "Planned, Current", "Complication, Current"))
STF$SUCTHROMB_A4 <- factor(STF$SUCTHROMB_A4, levels = c(0,1,2,3), labels = c("None", "Prior Procedure", "Planned, Current", "Complication, Current"))

## Technical Result Aneurysmal
STF$TECHRESULT_1A <- factor(STF$TECHRESULT_1A, levels = c(1,2,3,4,5,6,7,8), labels = c("Successful- stenosis <= 30%", "Stenosis > 30%", "Target Lesion Occlusion", "Technical failure, unable to cross", "Stenosis > 30%", "Successful- stenosis <= 30%", "Endoleak", "Endoleak and Stenosis >30%"))
STF$TECHRESULT_2A <- factor(STF$TECHRESULT_2A, levels = c(1,2,3,4,5,6,7,8), labels = c("Successful- stenosis <= 30%", "Stenosis > 30%", "Target Lesion Occlusion", "Technical failure, unable to cross", "Stenosis > 30%", "Successful- stenosis <= 30%", "Endoleak", "Endoleak and Stenosis >30%"))
STF$TECHRESULT_3A <- factor(STF$TECHRESULT_3A, levels = c(1,2,3,4,5,6,7,8), labels = c("Successful- stenosis <= 30%", "Stenosis > 30%", "Target Lesion Occlusion", "Technical failure, unable to cross", "Stenosis > 30%", "Successful- stenosis <= 30%", "Endoleak", "Endoleak and Stenosis >30%"))
STF$TECHRESULT_4A <- factor(STF$TECHRESULT_4A, levels = c(1,2,3,4,5,6,7,8), labels = c("Successful- stenosis <= 30%", "Stenosis > 30%", "Target Lesion Occlusion", "Technical failure, unable to cross", "Stenosis > 30%", "Successful- stenosis <= 30%", "Endoleak", "Endoleak and Stenosis >30%"))


## Outcomes/Complications- Dictionary Rows- 402 to 439 ----
STF <- STF %>%
	mutate(DEAD = ifelse(is.na(DEAD), '2', DEAD))
STF$DEAD <- factor(STF$DEAD, levels = c(0,1,2), labels = c("No", "Yes", "Unknown"))

#Endoleak Completion for ANeurysmal
STF$ENDOLEAK_1 <- factor(STF$ENDOLEAK_1, levels = c(0,1,2,3), labels = c("None", "Type 1", "Type 2", "Type 3"))
STF$ENDOLEAK_2 <- factor(STF$ENDOLEAK_2, levels = c(0,1,2,3), labels = c("None", "Type 1", "Type 2", "Type 3"))
STF$ENDOLEAK_3 <- factor(STF$ENDOLEAK_3, levels = c(0,1,2,3), labels = c("None", "Type 1", "Type 2", "Type 3"))
STF$ENDOLEAK_4 <- factor(STF$ENDOLEAK_4, levels = c(0,1,2,3), labels = c("None", "Type 1", "Type 2", "Type 3"))

STF$POSTOP_COMP <- factor(STF$POSTOP_COMP, levels = c(0,1,2), labels = c("No", "Yes", "Yes, Requiring Admission"))
STF$CARDIACCOMP <- factor(STF$CARDIACCOMP, levels = c(0,1), labels = c("No", "Yes"))
STF$POSTOP_MI <- factor(STF$POSTOP_MI, levels = c(0,1,2), labels = c("No", "Troponin Only", "EKG/Clinical MI"))
STF$PULMCOMP <- factor(STF$PULMCOMP, levels = c(0,1), labels = c("No", "Yes"))
STF$RENALCOMP <- factor(STF$RENALCOMP, levels = c(0,1), labels = c("No", "Yes"))
STF$ACCSITECOMP <- factor(STF$ACCSITECOMP, levels = c(0,1), labels = c("No", "Yes"))
STF$CONTRASTCOMP <- factor(STF$CONTRASTCOMP, levels = c(0,1), labels = c("No", "Yes"))
STF$OTHERCOMP <- factor(STF$OTHERCOMP, levels = c(0,1), labels = c("No", "Yes"))

STF$THROMBTX <- factor(STF$THROMBTX, levels = c(0,1,2,3), labels = c("None", "Medical", "Interventional", "Surgical"))
STF$EMBOTX <- factor(STF$EMBOTX, levels = c(0,1,2,3), labels = c("None", "Medical", "Interventional", "Surgical"))
STF$TARGLESIONTX <- factor(STF$TARGLESIONTX, levels = c(0,1,2,3), labels = c("None", "Medical", "Interventional", "Surgical"))
STF$REMOTEDISTX <- factor(STF$REMOTEDISTX, levels = c(0,1,2,3), labels = c("None", "Medical", "Interventional", "Surgical"))
STF$PERFTX <- factor(STF$PERFTX, levels = c(0,1,2,3), labels = c("None", "Medical", "Interventional", "Surgical"))

#Complications by Access site 1 or 2
STF$HEMATOMA_1 <- factor(STF$HEMATOMA_1, levels = c(0,1,2,3,4), labels = c("No", "Minor", "Transfusion", "Thrombin Injection", "Require Surgery"))
STF$HEMATOMA_2 <- factor(STF$HEMATOMA_2, levels = c(0,1,2,3,4), labels = c("No", "Minor", "Transfusion", "Thrombin Injection", "Require Surgery"))

STF$STENOSISOCC_1 <- factor(STF$STENOSISOCC_1, levels = c(0,1,2,3,4,5), labels = c("No", "Medical Tx", "Interventional Tx", "Surgical Tx", "Stenosis", "Occlusion"))
STF$STENOSISOCC_2 <- factor(STF$STENOSISOCC_2, levels = c(0,1,2,3,4,5), labels = c("No", "Medical Tx", "Interventional Tx", "Surgical Tx", "Stenosis", "Occlusion"))

STF$INFECTION_1 <- factor(STF$INFECTION_1, levels = c(0,1,2), labels = c("No", "Medical Treatment", "Surgical Treatment"))
STF$INFECTION_2 <- factor(STF$INFECTION_2, levels = c(0,1,2), labels = c("No", "Medical Treatment", "Surgical Treatment"))

STF$PSEUDOANEURYSM_1 <- factor(STF$PSEUDOANEURYSM_1, levels = c(0,1,2,3,4), labels = c("No", "Minor", "Transfusion", "Thrombin Injection", "Require Surgery"))
STF$PSEUDOANEURYSM_2 <- factor(STF$PSEUDOANEURYSM_2, levels = c(0,1,2,3,4), labels = c("No", "Minor", "Transfusion", "Thrombin Injection", "Require Surgery"))

STF$AVFIST_1 <- factor(STF$AVFIST_1, levels = c(0,1,2,3), labels = c("No", "Medical Tx", "Interventional Tx", "Surgical Tx"))
STF$AVFIST_2 <- factor(STF$AVFIST_2, levels = c(0,1,2,3), labels = c("No", "Medical Tx", "Interventional Tx", "Surgical Tx"))


#Amputation Variables
STF$AMPUTATION <- factor(STF$AMPUTATION, levels = c(0,1,2,3,4,5,6,7), labels = c("No", "Toes", "Transmet/midfoot", "BK or Thru Knee", "AK or Higher", "Right", "Left", "Bilateral"))

STF$PLANNEDAMP <- factor(STF$PLANNEDAMP, levels = c(0,1,2,3), labels = c("No", "Right", "Left", "Bilateral"))

STF$AMPLEVEL_R <- factor(STF$AMPLEVEL_R, levels = c(0,1,2,3,4), labels = c("No", "Toes", "Transmet/midfoot", "BK or Thru Knee", "AK or Higher"))
STF$AMPLEVEL_L <- factor(STF$AMPLEVEL_L, levels = c(0,1,2,3,4), labels = c("No", "Toes", "Transmet/midfoot", "BK or Thru Knee", "AK or Higher"))

#Postop Med Variables
STF$MEDCHANGE <- factor(STF$MEDCHANGE, levels = c(0,1), labels = c("No", "Yes"))

STF$DC_ASA <- factor(STF$DC_ASA, levels = c(0, 1, 2, 3), 
                      labels = c("No", "Yes", "No, Medical Reason", "Non-compliant"))

STF$DC_P2Y <- factor(STF$DC_P2Y, levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8), 
                      labels = c("None", "Clopidogrel", "Prasugrel", "Ticlopidine", "Ticagrelor", "Other", "No, Medical Reason", "Non-compliant", "PAR1 Inhibitor"))

STF$DC_STATIN <- factor(STF$DC_STATIN, levels = c(0, 1, 2, 3), 
                        labels = c("No", "Yes", "No, Medical Reason", "Non-compliant"))

STF$DC_ANTICOAG <- factor(STF$DC_ANTICOAG, levels = c(0, 7, 8, 9, 4, 5, 6), 
                           labels = c("None", "Vitamin K Antagonist", "Thrombin Inhibitor", "Factor Xa Inhibitor", 
                                      "Other", "No, Medical Reason", "Non-compliant"))

STF$DC_ACE <- factor(STF$DC_ACE, levels = c(0, 1, 2, 3), 
                        labels = c("No", "Yes", "No, Medical Reason", "Non-compliant"))

STF$DC_CILASTAZOL <- factor(STF$DC_CILASTAZOL, levels = c(0, 1, 2, 3), 
                        labels = c("No", "Yes", "No, Medical Reason", "Non-compliant"))

STF$DC_BETABLOCKER <- factor(STF$DC_BETABLOCKER, levels = c(0, 1, 2, 3), 
                             labels = c("No", "Yes", "No, Medical Reason", "Non-compliant"))

#Other/Retired variables- Dictionary Rows- 440 to 492 ----
STF$TRANSFER <- factor(STF$TRANSFER, levels = c(0,1,2), labels = c("No", "Hospital", "Rehab Unit"))

STF$ASACLASS <- factor(STF$ASACLASS)

STF$PREOP_BETABLOCKER <- factor(STF$PREOP_BETABLOCKER, levels = c(0,1,2,3,4,5), labels = c("No", "Preop 1-30 days", "Chronic, over 30 days", "No, Medical Reason", "Op Day Only", "Non-Compliant"))

STF$N_ACETYLCYSTEINE <- factor(STF$N_ACETYLCYSTEINE, levels = c(0,1), labels = c("No", "Yes"))

STF$TREATMENT_REQ <- factor(STF$TREATMENT_REQ, levels = c(1,2,3), labels = c("Interventional", "Surgical", "Both"))

STF$ARTERIAL_DISSECTION <- factor(STF$ARTERIAL_DISSECTION, levels = c(0,1,2,3), labels = c("No", "Iliac", "Fem-pop", "Tibial"))
STF$ARTERIAL_PERFORATION <- factor(STF$ARTERIAL_PERFORATION, levels = c(0,1,2,3), labels = c("No", "Iliac", "Fem-pop", "Tibial"))


#Adjuncts is retired variable.
STF$ADJUNCTS1[is.na(STF$ADJUNCTS1)] <-0
STF$ADJUNCTS2[is.na(STF$ADJUNCTS2)] <-0
STF$ADJUNCTS3[is.na(STF$ADJUNCTS3)] <-0
STF$ADJUNCTS4[is.na(STF$ADJUNCTS4)] <-0

STF$ADJUNCTS1 <- factor(STF$ADJUNCTS1, levels = c(0,1,2,3,4,5), labels = c("None", "Thrombolysis, Pharmacological", "Thrombolysis, Mechanical", 
                                                                           "Re-entry, Device", "Embolic protection device", "Femoral Endarterectomy"))
STF$ADJUNCTS2 <- factor(STF$ADJUNCTS2, levels = c(0,1,2,3,4,5), labels = c("None", "Thrombolysis, Pharmacological", "Thrombolysis, Mechanical", 
                                                                           "Re-entry, Device", "Embolic protection device", "Femoral Endarterectomy"))
STF$ADJUNCTS3 <- factor(STF$ADJUNCTS3, levels = c(0,1,2,3,4,5), labels = c("None", "Thrombolysis, Pharmacological", "Thrombolysis, Mechanical", 
                                                                          "Re-entry, Device", "Embolic protection device", "Femoral Endarterectomy"))
STF$ADJUNCTS4 <- factor(STF$ADJUNCTS4, levels = c(0,1,2,3,4,5), labels = c("None", "Thrombolysis, Pharmacological", "Thrombolysis, Mechanical", 
                                                                           "Re-entry, Device", "Embolic protection device", "Femoral Endarterectomy"))

STF$DISTAL_EMBOLIZATION <- factor(STF$DISTAL_EMBOLIZATION, levels = c(0,1,2,3), labels = c("No", "Minor", "Perc Intervention", "Operation"))

#Treatment Types for aneursym RETIRED
STF$TREATMENT_TYPE_1_A <- factor(STF$TREATMENT_TYPE_1_A, levels = c(1,2,3,4,5,6,7,8,9), 
                                 labels = c("PTA", "Stent, Self-expand", "Stent, Balloon-expand", "Stent Graft", "Cryoplasty", "Cutting Balloon",
                                 "Laser Atherect", "Mechanical Atherect, Orbital", "Mechanical Atherect, Excisionsal"))
STF$TREATMENT_TYPE_4_A <- factor(STF$TREATMENT_TYPE_4_A, levels = c(1,2,3,4,5,6,7,8,9), 
                                 labels = c("PTA", "Stent, Self-expand", "Stent, Balloon-expand", "Stent Graft", "Cryoplasty", "Cutting Balloon",
                                            "Laser Atherect", "Mechanical Atherect, Orbital", "Mechanical Atherect, Excisionsal"))

STF$TREATMENT_TYPE_7_A <- factor(STF$TREATMENT_TYPE_7_A, levels = c(1,2,3,4,5,6,7,8,9), 
                                 labels = c("PTA", "Stent, Self-expand", "Stent, Balloon-expand", "Stent Graft", "Cryoplasty", "Cutting Balloon",
                                            "Laser Atherect", "Mechanical Atherect, Orbital", "Mechanical Atherect, Excisionsal"))

STF$TREATMENT_TYPE_10_A <- factor(STF$TREATMENT_TYPE_10_A, levels = c(1,2,3,4,5,6,7,8,9), 
                                 labels = c("PTA", "Stent, Self-expand", "Stent, Balloon-expand", "Stent Graft", "Cryoplasty", "Cutting Balloon",
                                            "Laser Atherect", "Mechanical Atherect, Orbital", "Mechanical Atherect, Excisionsal"))

STF$TREATMENT_TYPE_2_A <- factor(STF$TREATMENT_TYPE_2_A, levels = c(1,2,3,4,5,6,7,8,9), 
                                 labels = c("PTA", "Stent, Self-expand", "Stent, Balloon-expand", "Stent Graft", "Cryoplasty", "Cutting Balloon",
                                            "Laser Atherect", "Mechanical Atherect, Orbital", "Mechanical Atherect, Excisionsal"))

STF$TREATMENT_TYPE_5_A <- factor(STF$TREATMENT_TYPE_5_A, levels = c(1,2,3,4,5,6,7,8,9), 
                                 labels = c("PTA", "Stent, Self-expand", "Stent, Balloon-expand", "Stent Graft", "Cryoplasty", "Cutting Balloon",
                                            "Laser Atherect", "Mechanical Atherect, Orbital", "Mechanical Atherect, Excisionsal"))

STF$TREATMENT_TYPE_8_A <- factor(STF$TREATMENT_TYPE_8_A, levels = c(1,2,3,4,5,6,7,8,9), 
                                 labels = c("PTA", "Stent, Self-expand", "Stent, Balloon-expand", "Stent Graft", "Cryoplasty", "Cutting Balloon",
                                            "Laser Atherect", "Mechanical Atherect, Orbital", "Mechanical Atherect, Excisionsal"))
STF$TREATMENT_TYPE_11_A <- factor(STF$TREATMENT_TYPE_11_A, levels = c(1,2,3,4,5,6,7,8,9), 
                                 labels = c("PTA", "Stent, Self-expand", "Stent, Balloon-expand", "Stent Graft", "Cryoplasty", "Cutting Balloon",
                                            "Laser Atherect", "Mechanical Atherect, Orbital", "Mechanical Atherect, Excisionsal"))


STF$MEDCX <- factor(STF$MEDCX, levels = c(0,1), labels = c("No", "Yes"))

STF$PRIOR_CEA <- factor(STF$PRIOR_CEA, levels = c(0,1), labels = c("No", "Yes"))

STF$CIN_PROPHYLAXIS <- factor(STF$CIN_PROPHYLAXIS, levels = c(0,1,2,3), labels = c("None", "Bicarb", "Saline", "Both"))

STF$PROC_ANTICOAGULANT <- factor(STF$PROC_ANTICOAGULANT, levels = c(0,1,2,3), labels = c("No", "Heparin", "Bivalirudin", "Other"))

STF$POST_THROMBOSIS_1 <- factor(STF$POST_THROMBOSIS_1, levels = c(0,1,2,3), labels = c("No", "Medical Tx", "Interventional Tx", "Surgical Tx"))
STF$POST_THROMBOSIS_2 <- factor(STF$POST_THROMBOSIS_2, levels = c(0,1,2,3), labels = c("No", "Medical Tx", "Interventional Tx", "Surgical Tx"))
STF$POST_THROMBOSIS_3 <- factor(STF$POST_THROMBOSIS_3, levels = c(0,1,2,3), labels = c("No", "Medical Tx", "Interventional Tx", "Surgical Tx"))
STF$POST_THROMBOSIS_4 <- factor(STF$POST_THROMBOSIS_4, levels = c(0,1,2,3), labels = c("No", "Medical Tx", "Interventional Tx", "Surgical Tx"))

STF$POST_DISSECTION_1 <- factor(STF$POST_DISSECTION_1, levels = c(0,1,2,3), labels = c("No", "Medical Tx", "Interventional Tx", "Surgical Tx"))
STF$POST_DISSECTION_2 <- factor(STF$POST_DISSECTION_2, levels = c(0,1,2,3), labels = c("No", "Medical Tx", "Interventional Tx", "Surgical Tx"))
STF$POST_DISSECTION_3 <- factor(STF$POST_DISSECTION_3, levels = c(0,1,2,3), labels = c("No", "Medical Tx", "Interventional Tx", "Surgical Tx"))
STF$POST_DISSECTION_4 <- factor(STF$POST_DISSECTION_4, levels = c(0,1,2,3), labels = c("No", "Medical Tx", "Interventional Tx", "Surgical Tx"))

STF$BIVALIRUDIN <- factor(STF$BIVALIRUDIN, levels = c(0,1), labels = c("No", "Yes"))

##NARROW DOWN DATA SET TO INCLUDE ONLY USED VARIABLES----

#STF <- select(STF, PATIENTID, PRIMPROCID, DEAD, PROC_SURVIVALDAYS, PHYSICIANID, REGIONID, CENTERID, SURGYEAR, SURGMONTH, SURGWEEKDAY, PROC_CAUSEDEATH, TOTAL_LOS, POSTOP_LOS, 
#               GENDER, ETHNICITY, AGE, RACE, HTIN, HTCM, WTLB, WTKG, PRIMARYINSURER, DC_STATUS, SETTING, URGENCY, LIVINGSTATUS, PREOP_FUNCSTATUS, PREOP_AMBUL, PRIOR_CVD, 
#               PRIOR_CAD, PRIOR_CHF, PREOP_DYSRHYTHMIA, COPD, PREOP_DIABETES, DIALYSIS, HTN, PREOP_SMOKING, STRESS, PREOP_ACE, PREOP_ASA, PREOP_ANTICOAG, PREOP_P2Y,               PREOP_STATIN,	PREOP_CILOSTAZOL,	PRIOR_CABG,	PRIOR_PCI,	PRIOR_CEACAS,	PRIOR_ANEURYSM,	PRIOR_BYPENDARPVI,	PRIOR_AMP,	INFLOWTX_R,	INFLOWTX_L,	LEGTX_R,	
#               LEGTX_L,	PRIOR_AMP_R,	PRIOR_AMP_L,	FEMENDART,	LEGSYMP_R,	LEGSYMP_L,	BYPPAT_1,	BYPPAT_2,	ACUTEISCHEMIA_R,	ACUTEISCHEMIA_L,	TISSUELOSS_SEV_R,	
#               TISSUELOSS_SEV_L, INFECTION_R,	INFECTION_L, NUMACCSITES,	ACCSITE_1,	ACCSITE_2,	ACCSIDE_1,	ACCSIDE_2,	ACCGUIDE_1,	ACCGUIDE_2, CLOSUREDEVSUCCESS_1, 
#               CLOSUREDEVSUCCESS_2, CCENDAR_1,	CCENDAR_2, PROTAMINE,	NUMARTTREAT,	PVI_INDICATION_1,	PVI_INDICATION_2,	PVI_INDICATION_3,	PVI_INDICATION_4,	ARTERY_1,	
#               ARTERY_2,	ARTERY_3,	ARTERY_4,	PROCSIDE_1,	PROCSIDE_2,	PROCSIDE_3,	PROCSIDE_4,	PROXIMAL_R,	PROXIMAL_L,	DISTAL_R,	DISTAL_L,	PRIOR_TXSITE_1, PRIOR_TXSITE_2,	
#               PRIOR_TXSITE_3,	PRIOR_TXSITE_4,	TASCGRADE_1,	TASCGRADE_2,	TASCGRADE_3,	TASCGRADE_4, TREATLEN_1,	TREATLEN_2,	TREATLEN_3,	TREATLEN_4,	OCCLEN_1,	OCCLEN_2,	
#               OCCLEN_3,	OCCLEN_4,	CALC_1,	CALC_2,	CALC_3,	CALC_4,	NUMDEV_1,	NUMDEV_2,	NUMDEV_3,	NUMDEV_4,	TXTYPE_1,	TXTYPE_4,	TXTYPE_7,	TXTYPE_10, TXTYPE_2,	TXTYPE_5,	TXTYPE_8,	TXTYPE_11, TXTYPE_3,	TXTYPE_6,	TXTYPE_9,	TXTYPE_12,	
#               CONCOMITANT_1_0,	CONCOMITANT_1_1,	CONCOMITANT_1_2,	CONCOMITANT_1_3,	CONCOMITANT_1_4,	CONCOMITANT_1_5,	CONCOMITANT_1_6,	CONCOMITANT_1_7,	CONCOMITANT_1_8,	
#               CONCOMITANT_2_0,	CONCOMITANT_2_1,	CONCOMITANT_2_2,	CONCOMITANT_2_3,	CONCOMITANT_2_4,	CONCOMITANT_2_5,	CONCOMITANT_2_6,	CONCOMITANT_2_7,	CONCOMITANT_2_8,	
#               CONCOMITANT_3_0,	CONCOMITANT_3_1,	CONCOMITANT_3_2,	CONCOMITANT_3_3,	CONCOMITANT_3_4,	CONCOMITANT_3_5,	CONCOMITANT_3_6,	CONCOMITANT_3_7,	CONCOMITANT_3_8,	
#               CONCOMITANT_4_0,	CONCOMITANT_4_1,	CONCOMITANT_4_2,	CONCOMITANT_4_3,	CONCOMITANT_4_4,	CONCOMITANT_4_5,	CONCOMITANT_4_6,	CONCOMITANT_4_7,	CONCOMITANT_4_8,	
#               THROMBTIMING_OCC_1,	THROMBTIMING_OCC_2,	THROMBTIMING_OCC_3,	THROMBTIMING_OCC_4,	SUCTHROMB_1,	SUCTHROMB_2,	SUCTHROMB_3,	SUCTHROMB_4,	TECHRESULT_1,	TECHRESULT_2,	
#               TECHRESULT_3,	TECHRESULT_4,	SUBSEQTX_1,	SUBSEQTX_2,	SUBSEQTX_3,	SUBSEQTX_4, ENDOLEAK_1,	ENDOLEAK_2,	ENDOLEAK_3,	ENDOLEAK_4,	POSTOP_COMP,	CARDIACCOMP,	POSTOP_MI,	
#               PULMCOMP,	RENALCOMP,	ACCSITECOMP,	CONTRASTCOMP,	OTHERCOMP,	THROMBTX,	EMBOTX,	TARGLESIONTX,	REMOTEDISTX,	PERFTX,	HEMATOMA_1,	HEMATOMA_2,	STENOSISOCC_1,	STENOSISOCC_2,
#               INFECTION_1,	INFECTION_2,	PSEUDOANEURYSM_1,	PSEUDOANEURYSM_2,	AVFIST_1,	AVFIST_2,	AMPUTATION,	PLANNEDAMP,	AMPLEVEL_R,	AMPLEVEL_L,	MEDCHANGE,	DC_ASA,	DC_P2Y,	DC_STATIN,
#               DC_ANTICOAG,	DC_ACE,	DC_CILASTAZOL,	TRANSFER,	ASACLASS,	PREOP_BETABLOCKER,	N_ACETYLCYSTEINE,	NUM_LESIONS_TREATED1,	NUM_LESIONS_TREATED2,	NUM_LESIONS_TREATED3,	
#               NUM_LESIONS_TREATED4,	TREATMENT_REQ,	ARTERIAL_DISSECTION,	ARTERIAL_PERFORATION,	ADJUNCTS1,	ADJUNCTS2,	ADJUNCTS3,	ADJUNCTS4,	DISTAL_EMBOLIZATION,	OCC_LGTH_ANEUR_1,	
#               OCC_LGTH_ANEUR_2,	OCC_LGTH_ANEUR_3,	OCC_LGTH_ANEUR_4, MEDCX,	DC_BETABLOCKER,	PROXIMAL_LEFT_OLD,	PROXIMAL_RIGHT_OLD,	DISTAL_LEFT_OLD,	DISTAL_RIGHT_OLD,	PRIOR_CEA,	
#               CIN_PROPHYLAXIS,	PROC_ANTICOAGULANT,	POST_THROMBOSIS_1,	POST_THROMBOSIS_2,	POST_THROMBOSIS_3,	POST_THROMBOSIS_4,	POST_DISSECTION_1,	POST_DISSECTION_2,	
#               POST_DISSECTION_3,	POST_DISSECTION_4,	LTF_CALC,	BIVALIRUDIN, PREOP_ABIR, PREOP_ABIL, PREOP_TOEPRESSURE_R, PREOP_TOEPRESSURE_L)


STF$date <- ifelse((STF$SURGYEAR == '2016' & (STF$SURGMONTH == "oct" | STF$SURGMONTH == "nov" | STF$SURGMONTH == "dec")) | 
									 	STF$SURGYEAR == '2017' | STF$SURGYEAR == '2018' | STF$SURGYEAR == '2019', 1, 0)
STF$date <- factor(STF$date, levels = c(0,1), labels = c("No", "Yes"))

##Variable Clean Up to Groups
levels(STF$DC_STATUS) <- list('Home' = 'Home',
															'Rehab/Other Facility' = c("Rehab", "Nursing Home", "Other Hospital"),
															'Dead' = "Dead")

STF$RACE[is.na(STF$RACE)] <- 'Unknown'
levels(STF$RACE) <- list("White" = "White",
												 "Black" = "Black",
												 "Other" = c("American Indian", "Asian", "Native Hawaiin/Islander", "More than 1 race"),
												 "Unknown" = "Unknown")

levels(STF$PREOP_AMBUL) <- list('Ambulatory' = 'Ambulatory',
																'W. Assist/Prosthesis' = c('Ambulatory w. assistance', 'Ambulatory w. prosthesis', 'Ambulatory w. Assistance/Prosthesis'),
																'Wheelchair/Bedridden' = c('Wheelchair', 'Bedridden'))

levels(STF$PRIOR_CVD) <- list('No' = 'None',
															'Yes' = c('Hx Stroke, Asymptomatic', 'Hx Stroke, Minor Deficit', 'Hx Stroke, Major Deficit'))


levels(STF$PRIOR_CAD) <- list("No" = "None", 
															"Yes" = c("History of MI but no Sx", "Stable Angina", "MI less than 6 mos", "Unstable Angina"))

levels(STF$PREOP_DYSRHYTHMIA) <- list('No' = 'No',
																			'Yes' = c('Yes, AFib/Flutter', 'Yes, ventricular', 'Yes, Pacemaker', 'Yes, ICD'))

levels(STF$PREOP_DIABETES) <- list('No' = 'None',
																	 'Diet/Med Controlled' = c('Diet', 'Non-insulin Meds', 'Insulin'))

levels(STF$PRIOR_CHF) <- list("No" = "None", 
															"Yes" = c("Asymptomatic, Hx CHF", "Mild", "Moderate", "Severe"))                                                                                           

levels(STF$COPD) <- list("No/Not Treated" = c("No", "Not Treated"), 
												 "Meds or Home O2" = c("On Meds", "On Home Oxygen"))

STF$STRESS[is.na(STF$STRESS)] <- 'Not Done'
levels(STF$STRESS) <- list('Not Done/Unknown' = 'Not Done',
													 'Normal' = 'Normal',
													 'Positive' = c('Positive, Ischemia', 'Positive, MI', 'Positive, Both'))

levels(STF$PREOP_SMOKING) <- list('Never' = 'Never',
																	'Prior/Current' = c('Prior', 'Current'))

levels(STF$PRIOR_CABG) <- list('No' = 'None',
															 'Yes' = c('Less than 5yrs', 'More than 5yrs'))

levels(STF$PRIOR_PCI) <- list('No' = 'None',
															'Yes' = c('Less than 5yrs', 'More than 5yrs'))

levels(STF$PRIMARYINSURER) <- list('Medicare' = c('Medicare', 'Medicare Advantage'),
																	 'Medicaid' = 'Medicaid',
																	 'Commercial' = 'Commercial',
																	 'Other' = c('Military/VA', 'Non-US', 'Self Pay'))

levels(STF$PREOP_FUNCSTATUS) <-list('Full/Self Care' = c('Full', 'Light Work', 'Self Care'),
																		'Assisted/Bedbound' = c('Assisted Care', 'Bed Bound'))

STF$PRIOR_CEACAScomb <- ifelse((STF$PRIOR_CEA == "Yes" | STF$PRIOR_CEACAS == "CEA" | STF$PRIOR_CEACAS == "CAS" | STF$PRIOR_CEACAS == "Both"),1,0)

STF$PRIOR_CEACAScomb[is.na(STF$PRIOR_CEACAScomb)] <- 0
STF$PRIOR_CEACAScomb <- factor(STF$PRIOR_CEACAScomb, levels = c(0,1), labels = c("No", "Yes"))

levels(STF$PRIOR_ANEURYSM) <- list('No' = 'None', 
																	 'Yes' = c('Aortic', 'Other', 'Both', 'Yes'))




levels(STF$ASACLASS) <- list("Class 1 or 2" = c("1", "2"),
														 "Class 3" = "3",
														 "Class 4 or 5" = c("4", "5"))

STF$OBL. <- ifelse((STF$SETTING == "Ambulatory Center"  | STF$SETTING == "Office"),1,0)
STF$OBL <- factor(STF$OBL., levels = c(0,1), labels = c("Hospital", "OBL"))


levels(STF$LEGSYMP_R) <- list('Asymptomatic' = 'Asymptomatic',
															'Claudication' = c('Claudication', 'Mild Claudication', 'Moderate Claudication', 'Severe Claudication'),
															'Rest Pain' = 'Ischemic Rest Pain',
															'Tissue Loss' = c('Tissue Loss', 'Ulcer/necrosis', 'Non-healing Amputation', 'Both Ulcer/Non-healing Amp'),
															'Acute Ischemia' = 'Acute Ischemia',
															'Not Treated' = 'Not Treated')

levels(STF$LEGSYMP_L) <- list('Asymptomatic' = 'Asymptomatic',
															'Claudication' = c('Claudication', 'Mild Claudication', 'Moderate Claudication', 'Severe Claudication'),
															'Rest Pain' = 'Ischemic Rest Pain',
															'Tissue Loss' = c('Tissue Loss', 'Ulcer/necrosis', 'Non-healing Amputation', 'Both Ulcer/Non-healing Amp'),
															'Acute Ischemia' = 'Acute Ischemia',
															'Not Treated' = 'Not Treated')


levels(STF$PRIOR_AMP_R) <- list('No' = 'No',
																'Minor' = c('Toes', 'Transmet/Midfoot', 'Minor Amputation'),
																'Major' = c('BK or Thru Knee', 'AK or Higher', 'Major Amputation', 'Both Minor and Major Amp'))



levels(STF$PRIOR_AMP_L) <- list('No' = 'No',
																'Minor' = c('Toes', 'Transmet/Midfoot', 'Minor Amputation'),
																'Major' = c('BK or Thru Knee', 'AK or Higher', 'Major Amputation', 'Both Minor and Major Amp'))

#Preop/Discharge Meds----
levels(STF$PREOP_ASA) <- list("No" = c("No", "No, for medical reason", "Non-compliant"),
															"Yes" = "Yes")

levels(STF$PREOP_ACE) <- list("No" = c("No", "No, Medical Reason", "Non-compliant"),
															"Yes" = "Yes")

levels(STF$PREOP_P2Y) <- list("No" = c("None", "No, Medical Reason", "Non-compliant"),
															"Yes" = c("Clopidogrel", "Prasugrel", "Ticlopidine", "Ticagrelor", "PAR1 Inhibitor", "Other"))

levels(STF$PREOP_STATIN) <- list("No" = c("No", "No, Medical Reason", "Non-compliant"),
																 "Yes" = "Yes")

levels(STF$PREOP_ANTICOAG) <- list("No" = c("None", "No, Medical Reason", "Non-compliant"),
																	 "Yes" = c("Warfarin", "Vitamin K Antagonist", "Thrombin Inhibitor", "Factor Xa Inhibitor"))

levels(STF$PREOP_CILOSTAZOL) <- list("No" = c("No", "No, Medical Reason", "Non-compliant"),
																		 "Yes" = "Yes")

#Discharge
levels(STF$DC_ASA) <- list("No" = c("No", "No, for medical reason", "Non-compliant"),
													 "Yes" = "Yes")

levels(STF$DC_ACE) <- list("No" = c("No", "No, Medical Reason", "Non-compliant"),
													 "Yes" = "Yes")

levels(STF$DC_P2Y) <- list("No" = c("None", "No, Medical Reason", "Non-compliant"),
													 "Yes" = c("Clopidogrel", "Prasugrel", "Ticlopidine", "Ticagrelor", "PAR1 Inhibitor", "Other"))

levels(STF$DC_STATIN) <- list("No" = c("No", "No, Medical Reason", "Non-compliant"),
															"Yes" = "Yes")

levels(STF$DC_ANTICOAG) <- list("No" = c("None", "No, Medical Reason", "Non-compliant"),
																"Yes" = c("Warfarin", "Vitamin K Antagonist", "Thrombin Inhibitor", "Factor Xa Inhibitor"))

levels(STF$DC_CILASTAZOL) <- list("No" = c("No", "No, Medical Reason", "Non-compliant"),
																	"Yes" = "Yes")

# Group all AC/AP values into one variable for further analysis. Already removed all NAs. Used the factor function to maintain the order of variable values.
STF$DC_COMBAC <- ifelse(STF$DC_ASA == "No" & STF$DC_P2Y == "No" & STF$DC_ANTICOAG == "No", 1, 
												ifelse(STF$DC_ASA == "Yes" & STF$DC_P2Y == "No" & STF$DC_ANTICOAG == "No", 2,
															 ifelse(STF$DC_ASA == "No" & STF$DC_P2Y == "Yes" & STF$DC_ANTICOAG == "No", 3,
															 			 ifelse(STF$DC_ASA == "No" & STF$DC_P2Y == "No" & STF$DC_ANTICOAG == "Yes", 4,
															 			 			 ifelse(STF$DC_ASA == "Yes" & STF$DC_P2Y == "Yes" & STF$DC_ANTICOAG == "No", 5,
															 			 			 			 ifelse(STF$DC_ASA == "Yes" & STF$DC_P2Y == "No" & STF$DC_ANTICOAG == "Yes", 6,
															 			 			 			 			 ifelse(STF$DC_ASA == "No" & STF$DC_P2Y == "Yes" & STF$DC_ANTICOAG == "Yes", 7, 8))))))) 

STF$DC_COMBAC <- factor(STF$DC_COMBAC, levels = c(1,2,3,4,5,6,7,8),
												labels = c("None", "ASA Only", "P2Y Only", "AC Only", "ASA/P2Y", "ASA/AC", "P2Y/AC", "ASA/P2Y/AC"))




## Artery 1 Treated
levels(STF$ARTERY_1) <- list('Aortoiliac/CFA/DFA' = c('Aorta', 'Common Iliac', 'External Iliac', 'Common and External', 'Internal Iliac', 'Common Femoral', 'Profunda'),
														 'Femoral Popliteal' = c('SFA', 'Popliteal', 'SFA and Pop'),
														 'Tibials' = c('Anterior Tibial', 'TP Trunk', 'Posterior Tibial', 'Peroneal', 'Dorsal Pedal', 'Plantar'))

## Artery 2 Treated
levels(STF$ARTERY_2) <- list('Aortoiliac/CFA/DFA' = c('Aorta', 'Common Iliac', 'External Iliac', 'Common and External', 'Internal Iliac', 'Common Femoral', 'Profunda'),
														 'Femoral Popliteal' = c('SFA', 'Popliteal', 'SFA and Pop'),
														 'Tibials' = c('Anterior Tibial', 'TP Trunk', 'Posterior Tibial', 'Peroneal', 'Dorsal Pedal', 'Plantar'))

## Artery 3 Treated
levels(STF$ARTERY_3) <- list('Aortoiliac/CFA/DFA' = c('Aorta', 'Common Iliac', 'External Iliac', 'Common and External', 'Internal Iliac', 'Common Femoral', 'Profunda'),
														 'Femoral Popliteal' = c('SFA', 'Popliteal', 'SFA and Pop'),
														 'Tibials' = c('Anterior Tibial', 'TP Trunk', 'Posterior Tibial', 'Peroneal', 'Dorsal Pedal', 'Plantar'))
## Artery 4 Treated
levels(STF$ARTERY_4) <- list('Aortoiliac/CFA/DFA' = c('Aorta', 'Common Iliac', 'External Iliac', 'Common and External', 'Internal Iliac', 'Common Femoral', 'Profunda'),
														 'Femoral Popliteal' = c('SFA', 'Popliteal', 'SFA and Pop'),
														 'Tibials' = c('Anterior Tibial', 'TP Trunk', 'Posterior Tibial', 'Peroneal', 'Dorsal Pedal', 'Plantar'))


##Treatment type Artery 1, device 1
levels(STF$TXTYPE_1) <- list('Angioplasty' = c('Plain Balloon', 'Cutting Balloon', 'Special Balloon'),   
														 'Stent' = c('Self Expanding Stent','Balloon Expandable Stent', 'Stent'),
														 'Stent Graft' = 'Stent Graft',
														 'Cryoplasty' = 'Cryoplasty',
														 'Atherectomy' = c('Laser Atherectomy', 'Mechanical Atherectomy', 'Atherectomy'),
														 'Not Able to Treat' = 'Not Able to Treat',
														 'Bailout Stent/Graft' = c('Bailout Stent', 'Bailout Stent Graft'))

##Treatment type Artery 1, device 2
levels(STF$TXTYPE_2) <- list('Angioplasty' = c('Plain Balloon', 'Cutting Balloon', 'Special Balloon'),   
														 'Stent' = c('Self Expanding Stent','Balloon Expandable Stent', 'Stent'),
														 'Stent Graft' = 'Stent Graft',
														 'Cryoplasty' = 'Cryoplasty',
														 'Atherectomy' = c('Laser Atherectomy', 'Mechanical Atherectomy', 'Atherectomy'),
														 'Not Able to Treat' = 'Not Able to Treat',
														 'Bailout Stent/Graft' = c('Bailout Stent', 'Bailout Stent Graft'))

##Treatment type Artery 1, device 3
levels(STF$TXTYPE_3) <- list('Angioplasty' = c('Plain Balloon', 'Cutting Balloon', 'Special Balloon'),   
														 'Stent' = c('Self Expanding Stent','Balloon Expandable Stent', 'Stent'),
														 'Stent Graft' = 'Stent Graft',
														 'Cryoplasty' = 'Cryoplasty',
														 'Atherectomy' = c('Laser Atherectomy', 'Mechanical Atherectomy', 'Atherectomy'),
														 'Not Able to Treat' = 'Not Able to Treat',
														 'Bailout Stent/Graft' = c('Bailout Stent', 'Bailout Stent Graft'))

##Treatment type Artery 2, device 1
levels(STF$TXTYPE_4) <- list('Angioplasty' = c('Plain Balloon', 'Cutting Balloon', 'Special Balloon'),   
														 'Stent' = c('Self Expanding Stent','Balloon Expandable Stent', 'Stent'),
														 'Stent Graft' = 'Stent Graft',
														 'Cryoplasty' = 'Cryoplasty',
														 'Atherectomy' = c('Laser Atherectomy', 'Mechanical Atherectomy', 'Atherectomy'),
														 'Not Able to Treat' = 'Not Able to Treat',
														 'Bailout Stent/Graft' = c('Bailout Stent', 'Bailout Stent Graft'))

##Treatment type Artery 2, device 2
levels(STF$TXTYPE_5) <- list('Angioplasty' = c('Plain Balloon', 'Cutting Balloon', 'Special Balloon'),   
														 'Stent' = c('Self Expanding Stent','Balloon Expandable Stent', 'Stent'),
														 'Stent Graft' = 'Stent Graft',
														 'Cryoplasty' = 'Cryoplasty',
														 'Atherectomy' = c('Laser Atherectomy', 'Mechanical Atherectomy', 'Atherectomy'),
														 'Not Able to Treat' = 'Not Able to Treat',
														 'Bailout Stent/Graft' = c('Bailout Stent', 'Bailout Stent Graft'))

##Treatment type Artery 2, device 3
levels(STF$TXTYPE_6) <- list('Angioplasty' = c('Plain Balloon', 'Cutting Balloon', 'Special Balloon'),   
														 'Stent' = c('Self Expanding Stent','Balloon Expandable Stent', 'Stent'),
														 'Stent Graft' = 'Stent Graft',
														 'Cryoplasty' = 'Cryoplasty',
														 'Atherectomy' = c('Laser Atherectomy', 'Mechanical Atherectomy', 'Atherectomy'),
														 'Not Able to Treat' = 'Not Able to Treat',
														 'Bailout Stent/Graft' = c('Bailout Stent', 'Bailout Stent Graft'))

##Treatment type Artery 3, device 1
levels(STF$TXTYPE_7) <- list('Angioplasty' = c('Plain Balloon', 'Cutting Balloon', 'Special Balloon'),   
														 'Stent' = c('Self Expanding Stent','Balloon Expandable Stent', 'Stent'),
														 'Stent Graft' = 'Stent Graft',
														 'Cryoplasty' = 'Cryoplasty',
														 'Atherectomy' = c('Laser Atherectomy', 'Mechanical Atherectomy', 'Atherectomy'),
														 'Not Able to Treat' = 'Not Able to Treat',
														 'Bailout Stent/Graft' = c('Bailout Stent', 'Bailout Stent Graft'))

##Treatment type Artery 3, device 2
levels(STF$TXTYPE_8) <- list('Angioplasty' = c('Plain Balloon', 'Cutting Balloon', 'Special Balloon'),   
														 'Stent' = c('Self Expanding Stent','Balloon Expandable Stent', 'Stent'),
														 'Stent Graft' = 'Stent Graft',
														 'Cryoplasty' = 'Cryoplasty',
														 'Atherectomy' = c('Laser Atherectomy', 'Mechanical Atherectomy', 'Atherectomy'),
														 'Not Able to Treat' = 'Not Able to Treat',
														 'Bailout Stent/Graft' = c('Bailout Stent', 'Bailout Stent Graft'))

##Treatment type Artery 3, device 3
levels(STF$TXTYPE_9) <- list('Angioplasty' = c('Plain Balloon', 'Cutting Balloon', 'Special Balloon'),   
														 'Stent' = c('Self Expanding Stent','Balloon Expandable Stent', 'Stent'),
														 'Stent Graft' = 'Stent Graft',
														 'Cryoplasty' = 'Cryoplasty',
														 'Atherectomy' = c('Laser Atherectomy', 'Mechanical Atherectomy', 'Atherectomy'),
														 'Not Able to Treat' = 'Not Able to Treat',
														 'Bailout Stent/Graft' = c('Bailout Stent', 'Bailout Stent Graft'))

##Treatment type Artery 4, device 1
levels(STF$TXTYPE_10) <- list('Angioplasty' = c('Plain Balloon', 'Cutting Balloon', 'Special Balloon'),   
															'Stent' = c('Self Expanding Stent','Balloon Expandable Stent', 'Stent'),
															'Stent Graft' = 'Stent Graft',
															'Cryoplasty' = 'Cryoplasty',
															'Atherectomy' = c('Laser Atherectomy', 'Mechanical Atherectomy', 'Atherectomy'),
															'Not Able to Treat' = 'Not Able to Treat',
															'Bailout Stent/Graft' = c('Bailout Stent', 'Bailout Stent Graft'))

##Treatment type Artery 4, device 2
levels(STF$TXTYPE_11) <- list('Angioplasty' = c('Plain Balloon', 'Cutting Balloon', 'Special Balloon'),   
															'Stent' = c('Self Expanding Stent','Balloon Expandable Stent', 'Stent'),
															'Stent Graft' = 'Stent Graft',
															'Cryoplasty' = 'Cryoplasty',
															'Atherectomy' = c('Laser Atherectomy', 'Mechanical Atherectomy', 'Atherectomy'),
															'Not Able to Treat' = 'Not Able to Treat',
															'Bailout Stent/Graft' = c('Bailout Stent', 'Bailout Stent Graft'))

##Treatment type Artery 4, device 3
levels(STF$TXTYPE_12) <- list('Angioplasty' = c('Plain Balloon', 'Cutting Balloon', 'Special Balloon'),   
															'Stent' = c('Self Expanding Stent','Balloon Expandable Stent', 'Stent'),
															'Stent Graft' = 'Stent Graft',
															'Cryoplasty' = 'Cryoplasty',
															'Atherectomy' = c('Laser Atherectomy', 'Mechanical Atherectomy', 'Atherectomy'),
															'Not Able to Treat' = 'Not Able to Treat',
															'Bailout Stent/Graft' = c('Bailout Stent', 'Bailout Stent Graft'))



## Determine if procedure included specific procedure type into a binary scale.
STF$TXTYPE_PTA <- ifelse((STF$ARTERY_1 == "Femoral Popliteal" & (STF$TXTYPE_1 == "Angioplasty" | STF$TXTYPE_2 == "Angioplasty" | STF$TXTYPE_3 == "Angioplasty")) 
												 | (STF$ARTERY_2 == "Femoral Popliteal" & (STF$TXTYPE_4 == "Angioplasty" | STF$TXTYPE_5 == "Angioplasty" | STF$TXTYPE_6 == "Angioplasty"))
												 | (STF$ARTERY_3 == "Femoral Popliteal" & (STF$TXTYPE_7 == "Angioplasty" | STF$TXTYPE_8 == "Angioplasty" | STF$TXTYPE_9 == "Angioplasty"))
												 | (STF$ARTERY_4 == "Femoral Popliteal" & (STF$TXTYPE_10 == "Angioplasty" | STF$TXTYPE_11 == "Angioplasty" | STF$TXTYPE_12 == "Angioplasty")),1,0)
STF$TXTYPE_PTA[is.na(STF$TXTYPE_PTA)] <- 0
STF$TXTYPE_PTA <- factor(STF$TXTYPE_PTA, levels = c(0,1), labels = c("No", "Yes"))


STF$TXTYPE_STENT <- ifelse((STF$ARTERY_1 == "Femoral Popliteal" & (STF$TXTYPE_1 == "Stent" | STF$TXTYPE_2 == "Stent" | STF$TXTYPE_3 == "Stent")) 
													 | (STF$ARTERY_2 == "Femoral Popliteal" & (STF$TXTYPE_4 == "Stent" | STF$TXTYPE_5 == "Stent" | STF$TXTYPE_6 == "Stent"))
													 | (STF$ARTERY_3 == "Femoral Popliteal" & (STF$TXTYPE_7 == "Stent" | STF$TXTYPE_8 == "Stent" | STF$TXTYPE_9 == "Stent"))
													 | (STF$ARTERY_4 == "Femoral Popliteal" & (STF$TXTYPE_10 == "Stent" | STF$TXTYPE_11 == "Stent" | STF$TXTYPE_12 == "Stent")),1,0)
STF$TXTYPE_STENT[is.na(STF$TXTYPE_STENT)] <- 0
STF$TXTYPE_STENT <- factor(STF$TXTYPE_STENT, levels = c(0,1), labels = c("No", "Yes"))


STF$TXTYPE_STENTGRAFT <- ifelse((STF$ARTERY_1 == "Femoral Popliteal" & (STF$TXTYPE_1 == "Stent Graft" | STF$TXTYPE_2 == "Stent Graft" | STF$TXTYPE_3 == "Stent Graft")) 
																| (STF$ARTERY_2 == "Femoral Popliteal" & (STF$TXTYPE_4 == "Stent Graft" | STF$TXTYPE_5 == "Stent Graft" | STF$TXTYPE_6 == "Stent Graft"))
																| (STF$ARTERY_3 == "Femoral Popliteal" & (STF$TXTYPE_7 == "Stent Graft" | STF$TXTYPE_8 == "Stent Graft" | STF$TXTYPE_9 == "Stent Graft"))
																| (STF$ARTERY_4 == "Femoral Popliteal" & (STF$TXTYPE_10 == "Stent Graft" | STF$TXTYPE_11 == "Stent Graft" | STF$TXTYPE_12 == "Stent Graft")),1,0)
STF$TXTYPE_STENTGRAFT[is.na(STF$TXTYPE_STENTGRAFT)] <- 0
STF$TXTYPE_STENTGRAFT <- factor(STF$TXTYPE_STENTGRAFT, levels = c(0,1), labels = c("No", "Yes"))


STF$TXTYPE_CRYO <- ifelse((STF$ARTERY_1 == "Femoral Popliteal" & (STF$TXTYPE_1 == "Cryoplasty" | STF$TXTYPE_2 == "Cryoplasty" | STF$TXTYPE_3 == "Cryoplasty")) 
													| (STF$ARTERY_2 == "Femoral Popliteal" & (STF$TXTYPE_4 == "Cryoplasty" | STF$TXTYPE_5 == "Cryoplasty" | STF$TXTYPE_6 == "Cryoplasty"))
													| (STF$ARTERY_3 == "Femoral Popliteal" & (STF$TXTYPE_7 == "Cryoplasty" | STF$TXTYPE_8 == "Cryoplasty" | STF$TXTYPE_9 == "Cryoplasty"))
													| (STF$ARTERY_4 == "Femoral Popliteal" & (STF$TXTYPE_10 == "Cryoplasty" | STF$TXTYPE_11 == "Cryoplasty" | STF$TXTYPE_12 == "Cryoplasty")),1,0)
STF$TXTYPE_CRYO[is.na(STF$TXTYPE_CRYO)] <- 0
STF$TXTYPE_CRYO <- factor(STF$TXTYPE_CRYO, levels = c(0,1), labels = c("No", "Yes"))


STF$TXTYPE_NA <- ifelse((STF$ARTERY_1 == "Femoral Popliteal" & (STF$TXTYPE_1 == "Not Able to Treat" | STF$TXTYPE_2 == "Not Able to Treat" | STF$TXTYPE_3 == "Not Able to Treat")) 
												| (STF$ARTERY_2 == "Femoral Popliteal" & (STF$TXTYPE_4 == "Not Able to Treat" | STF$TXTYPE_5 == "Not Able to Treat" | STF$TXTYPE_6 == "Not Able to Treat"))
												| (STF$ARTERY_3 == "Femoral Popliteal" & (STF$TXTYPE_7 == "Not Able to Treat" | STF$TXTYPE_8 == "Not Able to Treat" | STF$TXTYPE_9 == "Not Able to Treat"))
												| (STF$ARTERY_4 == "Femoral Popliteal" & (STF$TXTYPE_10 == "Not Able to Treat" | STF$TXTYPE_11 == "Not Able to Treat" | STF$TXTYPE_12 == "Not Able to Treat")),1,0)
STF$TXTYPE_NA[is.na(STF$TXTYPE_NA)] <- 0
STF$TXTYPE_NA <- factor(STF$TXTYPE_NA, levels = c(0,1), labels = c("No", "Yes"))


STF$TXTYPE_BAIL <- ifelse((STF$ARTERY_1 == "Femoral Popliteal" & (STF$TXTYPE_1 == "Bailout Stent/Graft" | STF$TXTYPE_2 == "Bailout Stent/Graft" | STF$TXTYPE_3 == "Bailout Stent/Graft")) 
													| (STF$ARTERY_2 == "Femoral Popliteal" & (STF$TXTYPE_4 == "Bailout Stent/Graft" | STF$TXTYPE_5 == "Bailout Stent/Graft" | STF$TXTYPE_6 == "Bailout Stent/Graft"))
													| (STF$ARTERY_3 == "Femoral Popliteal" & (STF$TXTYPE_7 == "Bailout Stent/Graft" | STF$TXTYPE_8 == "Bailout Stent/Graft" | STF$TXTYPE_9 == "Bailout Stent/Graft"))
													| (STF$ARTERY_4 == "Femoral Popliteal" & (STF$TXTYPE_10 == "Bailout Stent/Graft" | STF$TXTYPE_11 == "Bailout Stent/Graft" | STF$TXTYPE_12 == "Bailout Stent/Graft")),1,0)
STF$TXTYPE_BAIL[is.na(STF$TXTYPE_BAIL)] <- 0
STF$TXTYPE_BAIL <- factor(STF$TXTYPE_BAIL, levels = c(0,1), labels = c("No", "Yes"))


STF$TXTYPE_ATH <- ifelse((STF$ARTERY_1 == "Femoral Popliteal" & (STF$TXTYPE_1 == "Atherectomy" | STF$TXTYPE_2 == "Atherectomy" | STF$TXTYPE_3 == "Atherectomy")) 
												 | (STF$ARTERY_2 == "Femoral Popliteal" & (STF$TXTYPE_4 == "Atherectomy" | STF$TXTYPE_5 == "Atherectomy" | STF$TXTYPE_6 == "Atherectomy"))
												 | (STF$ARTERY_3 == "Femoral Popliteal" & (STF$TXTYPE_7 == "Atherectomy" | STF$TXTYPE_8 == "Atherectomy" | STF$TXTYPE_9 == "Atherectomy"))
												 | (STF$ARTERY_4 == "Femoral Popliteal" & (STF$TXTYPE_10 == "Atherectomy" | STF$TXTYPE_11 == "Atherectomy" | STF$TXTYPE_12 == "Atherectomy")),1,0)
STF$TXTYPE_ATH[is.na(STF$TXTYPE_ATH)] <- 0
STF$TXTYPE_ATH <- factor(STF$TXTYPE_ATH, levels = c(0,1), labels = c("No", "Yes"))


STF$TXTYPE_PLYSIS <- ifelse((STF$ARTERY_1 == "Femoral Popliteal" & STF$CONCOMITANT_1_1 == "Yes") | (STF$ARTERY_2 == "Femoral Popliteal" & STF$CONCOMITANT_2_1 == "Yes") 
														| (STF$ARTERY_3 == "Femoral Popliteal" & STF$CONCOMITANT_3_1 == "Yes")  | (STF$ARTERY_4 == "Femoral Popliteal" & STF$CONCOMITANT_4_1 == "Yes"),1,0)
STF$TXTYPE_PLYSIS[is.na(STF$TXTYPE_PLYSIS)] <- 0
STF$TXTYPE_PLYSIS <- factor(STF$TXTYPE_PLYSIS, levels = c(0,1), labels = c("No", "Yes"))


STF$TXTYPE_MLYSIS <- ifelse((STF$ARTERY_1 == "Femoral Popliteal" & STF$CONCOMITANT_1_2 == "Yes") | (STF$ARTERY_2 == "Femoral Popliteal" & STF$CONCOMITANT_2_2 == "Yes") 
														| (STF$ARTERY_3 == "Femoral Popliteal" & STF$CONCOMITANT_3_2 == "Yes")  | (STF$ARTERY_4 == "Femoral Popliteal" & STF$CONCOMITANT_4_2 == "Yes"),1,0)
STF$TXTYPE_MLYSIS[is.na(STF$TXTYPE_MLYSIS)] <- 0
STF$TXTYPE_MLYSIS <- factor(STF$TXTYPE_MLYSIS, levels = c(0,1), labels = c("No", "Yes"))


STF$TXTYPE_SUCTION <- ifelse((STF$ARTERY_1 == "Femoral Popliteal" & STF$CONCOMITANT_1_6 == "Yes") | (STF$ARTERY_2 == "Femoral Popliteal" & STF$CONCOMITANT_2_6 == "Yes") 
														 | (STF$ARTERY_3 == "Femoral Popliteal" & STF$CONCOMITANT_3_6 == "Yes")  | (STF$ARTERY_4 == "Femoral Popliteal" & STF$CONCOMITANT_4_6 == "Yes"),1,0)
STF$TXTYPE_SUCTION[is.na(STF$TXTYPE_SUCTION)] <- 0
STF$TXTYPE_SUCTION <- factor(STF$TXTYPE_SUCTION, levels = c(0,1), labels = c("No", "Yes"))


STF$PROCEDURE <- ifelse(STF$TXTYPE_ATH == "Yes", "Atherectomy", ifelse(STF$TXTYPE_SUCTION == "Yes", "Suction Thrombectomy", 
																																			 ifelse(STF$TXTYPE_MLYSIS == "Yes", "Mech Lysis", ifelse(STF$TXTYPE_BAIL == "Yes", "Bailout Stent",
																																			 																												ifelse(STF$TXTYPE_PLYSIS == "Yes", "Pharm Lysis", ifelse(STF$TXTYPE_PTA == "Yes", "PTA",
																																			 																																																								 ifelse(STF$TXTYPE_STENTGRAFT == "Yes", "Stent Graft", ifelse(STF$TXTYPE_STENT == "Yes", "Stent","Other"))))))))

##Complications----
summary(STF$POSTOP_MI)
levels(STF$POSTOP_COMP) <- list('No' = 'No',
																'Yes' = c('Yes', 'Yes, Requiring Admission'))

levels(STF$POSTOP_MI) <- list('No' = 'No',
															'Yes' = c('Troponin Only', 'EKG/Clinical MI'))

levels(STF$THROMBTX) <- list('None' = 'None',
														 'Yes' = c('Medical', 'Interventional', 'Surgical'))

levels(STF$EMBOTX) <- list('None' = 'None',
													 'Yes' = c('Medical', 'Interventional', 'Surgical'))

levels(STF$TARGLESIONTX) <- list('None' = 'None',
																 'Yes' = c('Medical', 'Interventional', 'Surgical'))

levels(STF$REMOTEDISTX) <- list('None' = 'None',
																'Yes' = c('Medical', 'Interventional', 'Surgical'))

levels(STF$PERFTX) <- list('None' = 'None',
													 'Yes' = c('Medical', 'Interventional', 'Surgical'))

levels(STF$STENOSISOCC_1) <- list('None' = 'No',
																	'Treated' = c('Medical Tx', 'Interventional Tx', 'Surgical Tx'))
STF$STENOSISOCC_1 <- droplevels(STF$STENOSISOCC_1)

levels(STF$STENOSISOCC_2) <- list('None' = 'No',
																	'Treated' = c('Medical Tx', 'Interventional Tx', 'Surgical Tx'))
STF$STENOSISOCC_2 <- droplevels(STF$STENOSISOCC_2)

levels(STF$INFECTION_1) <- list('None' = 'No',
																'Treated' = c('Medical Treatment', 'Surgical Treatment'))

levels(STF$INFECTION_2) <- list('None' = 'No',
																'Treated' = c('Medical Treatment', 'Surgical Treatment'))

levels(STF$AVFIST_1) <- list('None' = 'No',
														 'Treated' = c('Medical Tx', 'Interventional Tx', 'Surgical Tx'))

levels(STF$AVFIST_2) <- list('None' = 'No',
														 'Treated' = c('Medical Tx', 'Interventional Tx', 'Surgical Tx'))

summary(STF$AVFIST_1)

##Compile outcome variables----

STF$EPD1 <- ifelse(STF$CONCOMITANT_1_3 == "Yes",1, ifelse(STF$ADJUNCTS1 == "Embolic protection device", 1, 0))
STF$EPD2 <- ifelse(STF$CONCOMITANT_2_3 == "Yes",1, ifelse(STF$ADJUNCTS2 == "Embolic protection device", 1, 0))
STF$EPD3 <- ifelse(STF$CONCOMITANT_3_3 == "Yes",1, ifelse(STF$ADJUNCTS3 == "Embolic protection device", 1, 0))
STF$EPD4 <- ifelse(STF$CONCOMITANT_4_3 == "Yes",1, ifelse(STF$ADJUNCTS4 == "Embolic protection device", 1, 0))

STF$EPDANY <- STF$EPD1 + STF$EPD2 + STF$EPD3 + STF$EPD4
STF$EPDANY <- factor(STF$EPDANY, levels = c(0,1,2,3,4 ), 
										 labels = c("No", "Yes", "Yes", "Yes", "Yes"))

##Convert Embolic Protection Device to Categorical data
STF$EPD1 <- factor(STF$EPD1, levels = c(0, 1), labels = c("No", "Yes"))
STF$EPD2 <- factor(STF$EPD2, levels = c(0, 1), labels = c("No", "Yes"))
STF$EPD3 <- factor(STF$EPD3, levels = c(0, 1), labels = c("No", "Yes"))
STF$EPD4 <- factor(STF$EPD4, levels = c(0, 1), labels = c("No", "Yes"))




#Thrombolysis for COMPLICATION
STF$THROMBTIMING_OCCANY <- ifelse(STF$THROMBTIMING_OCC_1 == "Complication, Current"| STF$THROMBTIMING_OCC_2 == "Complication, Current" | STF$THROMBTIMING_OCC_3 == "Complication, Current" | STF$THROMBTIMING_OCC_4== "Complication, Current", 1,0)
STF$THROMBTIMING_OCCANY <- factor(STF$THROMBTIMING_OCCANY, levels = c(0,1), labels = c("No", "Yes"))

#Thrombolysis for Planned
STF$THROMBTIMING_OCCPLAN <- ifelse(STF$THROMBTIMING_OCC_1 == "Planned, Current" | STF$THROMBTIMING_OCC_2 == "Planned, Current" | STF$THROMBTIMING_OCC_3 == "Planned, Current" | STF$THROMBTIMING_OCC_4== "Planned, Current", 1,0)
STF$THROMBTIMING_OCCPLAN <- factor(STF$THROMBTIMING_OCCPLAN, levels = c(0,1), labels = c("No", "Yes"))

##Convert Concomitant Suction Thrombectomy to labeled variable name
STF$consuctthrom1 <- STF$CONCOMITANT_1_6
STF$consuctthrom2 <- STF$CONCOMITANT_2_6
STF$consuctthrom3 <- STF$CONCOMITANT_3_6
STF$consuctthrom4 <- STF$CONCOMITANT_4_6


#Timing of suction Thrombectomy
STF$SUCTHROMBANY <- ifelse(STF$SUCTHROMB_1 == "Complication, Current" | STF$SUCTHROMB_2 == "Complication, Current" | STF$SUCTHROMB_3 == "Complication, Current" | STF$SUCTHROMB_4== "Complication, Current",1,0)
STF$SUCTHROMBANY <- factor(STF$SUCTHROMBANY, levels = c(0,1), labels = c("No", "Yes"))


STF$SUCTHROMBPLAN <- ifelse(STF$SUCTHROMB_1 == "Planned, Current" | STF$SUCTHROMB_2 == "Planned, Current" | STF$SUCTHROMB_3 == "Planned, Current" | STF$SUCTHROMB_4 == "Planned, Current",1,0)
STF$SUCTHROMBPLAN <- factor(STF$SUCTHROMBPLAN, levels = c(0,1), labels = c("No", "Yes"))

#Unplanned SUction Thrombectomy
STF$UNPLANNED <- ifelse(STF$SUCTHROMBANY == "Yes" | STF$THROMBTIMING_OCCANY == "Yes",1,0)
STF$UNPLANNED <- factor(STF$UNPLANNED, levels = c(0,1), labels = c("No", "Yes"))

STF$EMBOTXBIN <- STF$EMBOTX
levels(STF$EMBOTXBIN) <- list('No' = 'None',
															'Yes' = c("Medical", "Interventional", "Surgical"))


## Hematoma Complication 
levels(STF$HEMATOMA_1) <- list('No' = 'No',
															 'Yes' = c('Minor', 'Transfusion', 'Thrombin Injection', 'Require Surgery'))

levels(STF$HEMATOMA_2) <- list('No' = 'No',
															 'Yes' = c('Minor', 'Transfusion', 'Thrombin Injection', 'Require Surgery'))
levels(STF$PSEUDOANEURYSM_1) <- list('No' = 'No',
																		 'Yes' = c('Minor', 'Transfusion', 'Thrombin Injection', 'Require Surgery'))
levels(STF$PSEUDOANEURYSM_2) <- list('No' = 'No',
																		 'Yes' = c('Minor', 'Transfusion', 'Thrombin Injection', 'Require Surgery'))

STF$BLEEDINGCOMP <- ifelse(STF$HEMATOMA_1 == "Yes" | STF$HEMATOMA_2 == "Yes" | STF$PSEUDOANEURYSM_1 == "Yes" | STF$PSEUDOANEURYSM_2== "Yes",1,0)
STF$BLEEDINGCOMP <- factor(STF$BLEEDINGCOMP, levels = c(0,1), labels = c("No", "Yes"))

##Create Inclusion, exclusion, exposure and outcome variables----

## Create inclusion Variable- Fem/Pop intervention on right or Left
STF$INCLUSION_R <- ifelse((STF$ARTERY_1 == "Femoral Popliteal" & STF$PROCSIDE_1 == "Right") | (STF$ARTERY_2 == "Femoral Popliteal" & STF$PROCSIDE_2 == "Right")
													| (STF$ARTERY_3 == "Femoral Popliteal" & STF$PROCSIDE_3 == "Right") | (STF$ARTERY_4 == "Femoral Popliteal" & STF$PROCSIDE_4 == "Right"),1, 0)
STF$INCLUSION_R[is.na(STF$INCLUSION_R)] <- 0
STF$INCLUSION_R <- factor(STF$INCLUSION_R, levels = c(0,1),
													labels = c("No", "Yes"))

STF$INCLUSION_L <- ifelse((STF$ARTERY_1 == "Femoral Popliteal" & STF$PROCSIDE_1 == "Left") | (STF$ARTERY_2 == "Femoral Popliteal" & STF$PROCSIDE_2 == "Left")
													| (STF$ARTERY_3 == "Femoral Popliteal" & STF$PROCSIDE_3 == "Left") | (STF$ARTERY_4 == "Femoral Popliteal" & STF$PROCSIDE_4 == "Left"),1, 0)
STF$INCLUSION_L[is.na(STF$INCLUSION_L)] <- 0
STF$INCLUSION_L <- factor(STF$INCLUSION_L, levels = c(0,1),
													labels = c("No", "Yes"))

STF$INCLUDE <- ifelse(STF$INCLUSION_R == "Yes" & STF$INCLUSION_L == "No", 1, 
											ifelse(STF$INCLUSION_R == "No" & STF$INCLUSION_L == "Yes", 2, 
														 ifelse(STF$INCLUSION_R == "Yes" & STF$INCLUSION_L == "Yes", 3, "NA")))
STF$INCLUDE <- factor(STF$INCLUDE, levels = c(1,2,3),
											labels = c("Right", "Left", "Bilateral"))


##Create exclusion Variable- Aortoiliac/CFA/DFA lesion on right or left side
STF$EXCLUSION_R <- ifelse((STF$ARTERY_1 == "Aortoiliac/CFA/DFA" & STF$PROCSIDE_1 == "Right") | (STF$ARTERY_2 == "Aortoiliac/CFA/DFA" & STF$PROCSIDE_2 == "Right")
													| (STF$ARTERY_3 == "Aortoiliac/CFA/DFA" & STF$PROCSIDE_3 == "Right") | (STF$ARTERY_4 == "Aortoiliac/CFA/DFA" & STF$PROCSIDE_4 == "Right"),1, 0)
STF$EXCLUSION_R[is.na(STF$EXCLUSION_R)] <- 0
STF$EXCLUSION_R <- factor(STF$EXCLUSION_R, levels = c(0,1),
													labels = c("No", "Yes"))

STF$EXCLUSION_L <- ifelse(STF$ARTERY_1 == "Aortoiliac/CFA/DFA" & STF$PROCSIDE_1 == "Left" | STF$ARTERY_2 == "Aortoiliac/CFA/DFA" & STF$PROCSIDE_2 == "Left"
													| STF$ARTERY_3 == "Aortoiliac/CFA/DFA" & STF$PROCSIDE_3 == "Left" | STF$ARTERY_4 == "Aortoiliac/CFA/DFA" & STF$PROCSIDE_4 == "Left",1, 0)
STF$EXCLUSION_L[is.na(STF$EXCLUSION_L)] <- 0
STF$EXCLUSION_L <- factor(STF$EXCLUSION_L, levels = c(0,1),
													labels = c("No", "Yes"))

STF$EXCLUDE <- ifelse(STF$INCLUSION_R == "Yes" & STF$EXCLUSION_R == "Yes" | STF$INCLUSION_L == "Yes" & STF$EXCLUSION_L == "Yes", 1, 0)
STF$EXCLUDE <- factor(STF$EXCLUDE, levels = c(0,1),
											labels = c("No", "Yes"))

##Create exposure variable- YES IS EPD USED
STF$EXPOSURE_R <- ifelse(STF$ARTERY_1 != "Aortoiliac/CFA/DFA" & STF$PROCSIDE_1 == "Right" & STF$EPD1 == "Yes" | STF$ARTERY_2 != "Aortoiliac/CFA/DFA" & STF$PROCSIDE_2 == "Right"& STF$EPD2 == "Yes"
												 | STF$ARTERY_3 != "Aortoiliac/CFA/DFA" & STF$PROCSIDE_3 == "Right" & STF$EPD3 == "Yes"| STF$ARTERY_4 != "Aortoiliac/CFA/DFA" & STF$PROCSIDE_4 == "Right" & STF$EPD4 == "Yes",1, 0)
STF$EXPOSURE_R[is.na(STF$EXPOSURE_R)] <- 0
STF$EXPOSURE_R <- factor(STF$EXPOSURE_R, levels = c(0,1),
												 labels = c("No", "Yes"))

STF$EXPOSURE_L <- ifelse(STF$ARTERY_1 != "Aortoiliac/CFA/DFA" & STF$PROCSIDE_1 == "Left" & STF$EPD1 == "Yes" | STF$ARTERY_2 != "Aortoiliac/CFA/DFA" & STF$PROCSIDE_2 == "Left" & STF$EPD2 == "Yes"
												 | STF$ARTERY_3 != "Aortoiliac/CFA/DFA" & STF$PROCSIDE_3 == "Left" & STF$EPD3 == "Yes" | STF$ARTERY_4 != "Aortoiliac/CFA/DFA" & STF$PROCSIDE_4 == "Left" & STF$EPD4 == "Yes",1, 0)
STF$EXPOSURE_L[is.na(STF$EXPOSURE_L)] <- 0
STF$EXPOSURE_L <- factor(STF$EXPOSURE_L, levels = c(0,1),
												 labels = c("No", "Yes"))

STF$EXPOSURE <- ifelse(STF$EXPOSURE_R == "Yes" | STF$EXPOSURE_L == "Yes",1,0)
STF$EXPOSURE[is.na(STF$EXPOSURE)] <- 0 
STF$EXPOSURE <- factor(STF$EXPOSURE, levels = c(0,1),
											 labels = c("No EPD", "EPD Used"))

##Create Indication
STF$INDICATION <- ifelse(STF$INCLUDE == "Right", STF$LEGSYMP_R, 
												 ifelse(STF$INCLUDE == "Left", STF$LEGSYMP_L, 
												 			 ifelse(STF$INCLUDE == "Bilateral", 7, "NA")))
STF$INDICATION <- factor(STF$INDICATION, levels = c(1,2,3,4,5,6,7),
												 labels = c("Asymptomatic", "Claudication", "Rest Pain", "Tissue Loss", "Acute Ischemia", "Not Treated", "Bilateral Symptoms"))

##Create outcome variable to place unplanned suction thrombectomy or thrombolysis into each side
STF$OUTCOME_R <- ifelse(((STF$SUCTHROMB_1 == "Complication, Current" | STF$THROMBTIMING_OCC_1 == "Complication, Current") & STF$PROCSIDE_1 == "Right" & (STF$ARTERY_1 == "Femoral Popliteal" | STF$ARTERY_1 == "Tibials")) |
													((STF$SUCTHROMB_2 == "Complication, Current" | STF$THROMBTIMING_OCC_2 == "Complication, Current") & STF$PROCSIDE_2 == "Right" & (STF$ARTERY_2 == "Femoral Popliteal" | STF$ARTERY_2 == "Tibials")) |
													((STF$SUCTHROMB_3 == "Complication, Current" | STF$THROMBTIMING_OCC_3 == "Complication, Current") & STF$PROCSIDE_3 == "Right" & (STF$ARTERY_3 == "Femoral Popliteal" | STF$ARTERY_3 == "Tibials")) |
													((STF$SUCTHROMB_4 == "Complication, Current" | STF$THROMBTIMING_OCC_4 == "Complication, Current") & STF$PROCSIDE_4 == "Right" & (STF$ARTERY_4 == "Femoral Popliteal" | STF$ARTERY_4 == "Tibials")),1,0)
STF$OUTCOME_R[is.na(STF$OUTCOME_R)] <- 0
STF$OUTCOME_R <- factor(STF$OUTCOME_R, levels = c(0,1),
												labels = c("No", "Yes"))


STF$OUTCOME_L <- ifelse(((STF$SUCTHROMB_1 == "Complication, Current" | STF$THROMBTIMING_OCC_1 == "Complication, Current") & STF$PROCSIDE_1 == "Left" & (STF$ARTERY_1 == "Femoral Popliteal" | STF$ARTERY_1 == "Tibials")) |
													((STF$SUCTHROMB_2 == "Complication, Current" | STF$THROMBTIMING_OCC_2 == "Complication, Current") & STF$PROCSIDE_2 == "Left" & (STF$ARTERY_2 == "Femoral Popliteal" | STF$ARTERY_2 == "Tibials")) |
													((STF$SUCTHROMB_3 == "Complication, Current" | STF$THROMBTIMING_OCC_3 == "Complication, Current") & STF$PROCSIDE_3 == "Left" & (STF$ARTERY_3 == "Femoral Popliteal" | STF$ARTERY_3 == "Tibials")) |
													((STF$SUCTHROMB_4 == "Complication, Current" | STF$THROMBTIMING_OCC_4 == "Complication, Current") & STF$PROCSIDE_4 == "Left" & (STF$ARTERY_4 == "Femoral Popliteal" | STF$ARTERY_4 == "Tibials")),1,0)
STF$OUTCOME_L[is.na(STF$OUTCOME_L)] <- 0
STF$OUTCOME_L <- factor(STF$OUTCOME_L, levels = c(0,1),
												labels = c("No", "Yes"))

STF$OUTCOME <- ifelse(STF$OUTCOME_R == "Yes" | STF$OUTCOME_L == "Yes",1,0)
STF$OUTCOME[is.na(STF$OUTCOME)] <- 0 
STF$OUTCOME <- factor(STF$OUTCOME, levels = c(0,1),
											labels = c("No", "Yes"))


STF$OVERALLOUTCOME <- ifelse(STF$EXPOSURE_R == "Yes" & STF$OUTCOME_R == "Yes" | STF$EXPOSURE_L == "Yes" & STF$OUTCOME_L == "Yes", 1, 0) 
STF$OVERALLOUTCOME[is.na(STF$OVERALLOUTCOME)] <- 0
STF$OVERALLOUTCOME <- factor(STF$OVERALLOUTCOME, levels = c(0,1),
														 labels = c("No", "Yes"))

#Lesion Specific Variables----

##TASC
STF$TASC_FP <- ifelse((STF$ARTERY_4 == "Femoral Popliteal"),STF$TASCGRADE_4,NA)
STF$TASC_FP <- ifelse((STF$ARTERY_3 == "Femoral Popliteal"),STF$TASCGRADE_3, STF$TASC_FP)
STF$TASC_FP <- ifelse((STF$ARTERY_2 == "Femoral Popliteal"),STF$TASCGRADE_2, STF$TASC_FP)
STF$TASC_FP <- ifelse((STF$ARTERY_1 == "Femoral Popliteal"),STF$TASCGRADE_1, STF$TASC_FP)
STF$TASC_FP[is.na(STF$TASC_FP)] <- 6
STF$TASC_FP <- factor(STF$TASC_FP,levels = c(1,2,3,4,5,6), labels = c("A", "B", "C", "D", "Protect adjacent artery", "Not Evaluated"))


##CALC
STF$CALC_FP <- ifelse((STF$ARTERY_4 == "Femoral Popliteal"),STF$CALC_4,NA)
STF$CALC_FP <- ifelse((STF$ARTERY_3 == "Femoral Popliteal"),STF$CALC_3, STF$CALC_FP)
STF$CALC_FP <- ifelse((STF$ARTERY_2 == "Femoral Popliteal"),STF$CALC_2, STF$CALC_FP)
STF$CALC_FP <- ifelse((STF$ARTERY_1 == "Femoral Popliteal"),STF$CALC_1, STF$CALC_FP)
STF$CALC_FP[is.na(STF$CALC_FP)] <- 6
STF$CALC_FP <- factor(STF$CALC_FP, levels = c(1,2,3,4,5,6), labels = c("None", "Focal", "Mild", "Moderate", "Severe", "Not Evaluated"))


##LENGTH
STF$TREATLEN_FP <- ifelse((STF$ARTERY_4 == "Femoral Popliteal"),STF$TREATLEN_4,NA)
STF$TREATLEN_FP <- ifelse((STF$ARTERY_3 == "Femoral Popliteal"),STF$TREATLEN_3, STF$TREATLEN_FP)
STF$TREATLEN_FP <- ifelse((STF$ARTERY_2 == "Femoral Popliteal"),STF$TREATLEN_2, STF$TREATLEN_FP)
STF$TREATLEN_FP <- ifelse((STF$ARTERY_1 == "Femoral Popliteal"),STF$TREATLEN_1, STF$TREATLEN_FP)
STF$TREATLEN_FP[STF$TREATLEN_FP > 90] <- NA
STF$TREATLEN_FP[STF$TREATLEN_FP < 0] <- NA
table(STF$TREATLEN_FP)

##OCL
STF$OCCLEN_FP <- ifelse((STF$ARTERY_4 == "Femoral Popliteal"),STF$OCCLEN_4,NA)
STF$OCCLEN_FP <- ifelse((STF$ARTERY_3 == "Femoral Popliteal"),STF$OCCLEN_3, STF$OCCLEN_FP)
STF$OCCLEN_FP <- ifelse((STF$ARTERY_2 == "Femoral Popliteal"),STF$OCCLEN_2, STF$OCCLEN_FP)
STF$OCCLEN_FP <- ifelse((STF$ARTERY_1 == "Femoral Popliteal"),STF$OCCLEN_1, STF$OCCLEN_FP)
STF$OCCLEN_FP[STF$OCCLEN_FP > 90] <- NA
STF$OCCLEN_FP[STF$OCCLEN_FP < 0] <- NA
table(STF$OCCLEN_FP)


##ISR
STF$ISR_FP <- ifelse((STF$ARTERY_1 == "Femoral Popliteal" & STF$PRIOR_TXSITE_1== "Yes, Stent")
										 | (STF$ARTERY_2 == "Femoral Popliteal" & STF$PRIOR_TXSITE_2== "Yes, Stent")
										 | (STF$ARTERY_3 == "Femoral Popliteal" & STF$PRIOR_TXSITE_3== "Yes, Stent")
										 | (STF$ARTERY_4 == "Femoral Popliteal" & STF$PRIOR_TXSITE_4== "Yes, Stent"),1,0)
STF$ISR_FP[is.na(STF$ISR_FP)] <- 0
STF$ISR_FP <- factor(STF$ISR_FP, levels = c(0,1),
										 labels = c("No", "Yes"))

##RVD
##Runoff vessels
STF$RUNOFF_EXP <- ifelse((STF$INCLUSION_R == "Yes"), STF$DISTAL_R, NA)
STF$RUNOFF_EXP <- ifelse((STF$INCLUSION_L == "Yes"), STF$DISTAL_L, STF$RUNOFF_EXP)
STF$RUNOFF_EXP[STF$RUNOFF_EXP==4] <- NA
STF$RUNOFF_EXP <- factor(STF$RUNOFF_EXP)

STF$LEGSYMP_FP <- ifelse((STF$INCLUSION_R == "Yes"), STF$LEGSYMP_R, NA)
STF$LEGSYMP_FP <- ifelse((STF$INCLUSION_L == "Yes"), STF$LEGSYMP_L, STF$LEGSYMP_FP)
STF$LEGSYMP_FP <- factor(STF$LEGSYMP_FP, levels = c(1,2,3,4,5,6),
												 labels = c("Asymptomatic", "Claudication", "Rest Pain", "Tissue Loss", "Acute Ischemia", "Not Treated"))



STF$TISSUELOSS_SEV_FP <- ifelse((STF$INCLUSION_R == "Yes"), STF$TISSUELOSS_SEV_R, NA)
STF$TISSUELOSS_SEV_FP <- ifelse((STF$INCLUSION_L == "Yes"), STF$TISSUELOSS_SEV_L, STF$TISSUELOSS_SEV_FP)
STF$TISSUELOSS_SEV_FP <- factor(STF$TISSUELOSS_SEV_FP, levels = c(1,2,3),
																labels = c("Grade 1, Shallow", "Grade 2, Deep", "Grade 3, Extensive"))

STF$INFECTION_FP <- ifelse((STF$INCLUSION_R == "Yes"), STF$INFECTION_R, NA)
STF$INFECTION_FP <- ifelse((STF$INCLUSION_L == "Yes"), STF$INFECTION_L, STF$INFECTION_FP)
STF$INFECTION_FP <- factor(STF$INFECTION_FP, levels = c(1,2,3,4),
													 labels = c("Grade 0, None", "Grade 1, Mild", "Grade 2, Moderate", "Grade 3, Severe"))

STF$PREOP_ABI_FP <- ifelse((STF$INCLUSION_R == "Yes"), STF$PREOP_ABIR, NA)
STF$PREOP_ABI_FP <- ifelse((STF$INCLUSION_L == "Yes"), STF$PREOP_ABIL, STF$PREOP_ABI_FP)

STF$PREOP_TOEPRESSURE_FP <- ifelse((STF$INCLUSION_R == "Yes"), STF$PREOP_TOEPRESSURE_R, NA)
STF$PREOP_TOEPRESSURE_FP <- ifelse((STF$INCLUSION_L == "Yes"), STF$PREOP_TOEPRESSURE_L, STF$PREOP_TOEPRESSURE_FP)

STF$PRIOR_AMP_FP <- ifelse((STF$INCLUSION_R == "Yes"), STF$PRIOR_AMP_R, NA)
STF$PRIOR_AMP_FP <- ifelse((STF$INCLUSION_L == "Yes"), STF$PRIOR_AMP_L, STF$PRIOR_AMP_FP)
STF$PRIOR_AMP_FP <- factor(STF$PRIOR_AMP_FP, levels = c(1,2,3),
													 labels = c("No", "Minor Amputation", "Major Amputation"))

#STF$ATH_EPD <- ifelse((STF$TXTYPE_ATH == "Yes"), STF$EXPOSURE, 0)


STF$ATH_EPD <- ifelse((STF$TXTYPE_ATH == "Yes" & STF$EXPOSURE == "No EPD"),1,
											ifelse((STF$TXTYPE_ATH == "Yes" & STF$EXPOSURE == "EPD Used"),2,3))
STF$ATH_EPD <- factor(STF$ATH_EPD, levels = c(1,2),
											labels = c("Ath w.o EPD", "Ath w. EPD"))
summary(STF$ATH_EPD)

STF$STENT_EPD <- ifelse((STF$TXTYPE_STENT == "Yes" & STF$EXPOSURE == "No EPD"),1,
												ifelse((STF$TXTYPE_STENT == "Yes" & STF$EXPOSURE == "EPD Used"),2,3))
STF$STENT_EPD <- factor(STF$STENT_EPD, levels = c(1,2),
												labels = c("Stent w.o EPD", "Stent w. EPD"))

STF$PTA_EPD <- ifelse((STF$TXTYPE_PTA == "Yes" & STF$EXPOSURE == "No EPD"),1,
											ifelse((STF$TXTYPE_PTA == "Yes" & STF$EXPOSURE == "EPD Used"),2,3))
STF$PTA_EPD <- factor(STF$PTA_EPD, levels = c(1,2),
											labels = c("Angioplasty w.o EPD", "Angioplasty w. EPD"))

summary(STF$EXPOSURE)

## Label Variables - Done last so they don't get changed - Only categorical variables.  labels for continuous variables breaks summary list
label(STF$DEAD) <- "Death Status"
label(STF$REGIONID) <- "Region ID"
label(STF$SURGYEAR) <- "Surgery Year"
label(STF$PROC_CAUSEDEATH) <- "Cause of Death"
label(STF$TOTAL_LOS) <- "Total Length of Stay"
label(STF$POSTOP_LOS) <- "Post-op Lenght of Stay"
label(STF$GENDER) <- "Sex"
label(STF$ETHNICITY) <- "Ethnicity"
label(STF$RACE) <- "Race"
label(STF$PRIMARYINSURER) <- "Primary Insurer"
label(STF$TRANSFER) <- "Transfer"
label(STF$HTN) <- "Hypertension"
label(STF$PREOP_DIABETES) <- "Diabetes"
label(STF$PRIOR_CHF) <- "Prior CHF"
label(STF$PREOP_DYSRHYTHMIA) <- "Prior Dysrhythmia"
label(STF$COPD) <- "COPD"
label(STF$PRIOR_CAD) <- "CAD Symptoms"
label(STF$SETTING) <- "Procedure Setting"
label(STF$OBL) <- "Hospital/OBL"
label(STF$LIVINGSTATUS) <- "Living Status"
label(STF$PREOP_FUNCSTATUS) <- "Functional Status"
label(STF$PREOP_AMBUL) <- "Ambulatory Status"
label(STF$PRIOR_CVD) <- "Cerebrovascular Disease"
label(STF$DIALYSIS) <- "Dialysis"
label(STF$PREOP_SMOKING) <- "Smoking History"
label(STF$BMIclass) <- "BMI"
label(STF$DC_STATUS) <- "Discharge Status"

label(STF$PREOP_ASA) <- "Pre-operative Aspirin"
label(STF$PREOP_P2Y) <- "Pre-operative P2Y12 Antagonist"
label(STF$PREOP_STATIN) <- "Pre-operative Statin"
label(STF$PREOP_BETABLOCKER) <- "Pre-operative Beta Blocker"
label(STF$PREOP_ACE) <- "Pre-operative ACE Inhibitor" 
label(STF$PREOP_ANTICOAG) <- "Pre-operative Anticoagulation"
label(STF$PREOP_CILOSTAZOL) <- "Pre-operative Cilostazol"

label(STF$TASC_FP) <- "TASC Grade"
label(STF$CALC_FP) <- "Calcification"
label(STF$ISR_FP) <- "In Stent Restenosis"
label(STF$RUNOFF_EXP) <- "Runoff Vessels"
label(STF$TISSUELOSS_SEV_FP) <- "Tissue Loss"
label(STF$INFECTION_FP) <- "Infection"

label(STF$TXTYPE_ATH) <- "Treatment- Atherectomy"
label(STF$TXTYPE_PTA) <- "Treatment- Angioplasty"
label(STF$TXTYPE_STENT) <- "Treatment- Stent"
label(STF$TXTYPE_STENTGRAFT) <- "Treatment- Stent Graft"
label(STF$TXTYPE_BAIL) <- "Treatment- Bail Out Stent"
label(STF$TXTYPE_CRYO) <- "Treatment- Cryoplasty"
label(STF$TXTYPE_NA) <- "Treatment- Unable to Treat"
label(STF$ATH_EPD) <- "Athrectomy and EPD Use"
label(STF$STENT_EPD) <- "Stent and EPD Use"
label(STF$PTA_EPD) <- "Angioplasty and EPD Use"
label(STF$THROMBTIMING_OCCANY) <- "Thrombolysis- For Complication"
label(STF$THROMBTIMING_OCCPLAN) <- "Thrombolysis- Planned"
label(STF$SUCTHROMBPLAN) <- "Suction Thrombectomy- Planned"
label(STF$UNPLANNED) <- "Unplanned Thrombectomy or Thrombolysis"


label(STF$STRESS) <- "Pre-op Stress Test"
label(STF$PRIOR_CABG) <- "Prior CABG"
label(STF$PRIOR_PCI) <- "Prior PCI"
label(STF$PRIOR_CEACAScomb) <- "Prior Carotid Intervention (CEA/CAS)"
label(STF$PRIOR_ANEURYSM) <- "Prior Aneurysm Repair"
label(STF$PRIOR_BYPENDARPVI) <- "Prior PVI"
label(STF$FEMENDART) <- "Prior Femoral Endarterectomy"

label(STF$EMBOTXBIN) <- "Postop Embolization Requiring Treatment"
label(STF$CLOSUREDEVSUCCESS_1) <- "Closure Device 1- Success"
label(STF$CLOSUREDEVSUCCESS_2) <- "Closure Device 2- Success"

label(STF$OVERALLOUTCOME) <- "Complication- Distal Embolization"
label(STF$POSTOP_COMP) <- "Post-operative Complication"
label(STF$CARDIACCOMP) <- "Complication- Cardiac"
label(STF$POSTOP_MI) <- "Complication- MI"
label(STF$PULMCOMP) <- "Complication- Pulmonary"
label(STF$RENALCOMP) <- "Complication- Renal"
label(STF$BLEEDINGCOMP) <- "Complication- Bleeding"
label(STF$ACCSITECOMP) <- "Complication- Access Site"
label(STF$OTHERCOMP) <- "Complication- Other"
