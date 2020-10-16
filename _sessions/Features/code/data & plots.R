

pima = read_csv('_sessions/Features/1_Data/pima_diabetes.csv')
pima

cat(paste0('  = ',names(pima),',\n'),sep='')

diabetes = pima %>% rename(
Diabetes  = diabetes,
Schwangerschaften  = pregnant,
Glucose  = glucose,
Blutdruck  = pressure,
BMI  = mass,
fam_Vorerkrankungen = pedigree,
Alter  = age
  )

write_csv(diabetes,'_sessions/Features/1_Data/diabetes.csv')

cat(paste0('|',names(diabetes),'|  |\n'),sep='')
