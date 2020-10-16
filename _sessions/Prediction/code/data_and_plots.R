

college_train = read_csv('1_Data/college_train.csv')
college_test = read_csv('1_Data/college_test.csv')

graduation_train = college_train %>% 
  mutate(Private = case_when(
    Private == 'No' ~ 'Nein',
    TRUE ~ 'Ja'
  )) %>% 
  rename(
    Privatuniversitaet  =  Private,
    Bewerbungen  =  Apps,
    Angenommen  =  Accept,
    Eingeschrieben  =  Enroll,
    Prozent_Top10  =  Top10perc,
    Prozent_Top25  =  Top25perc,
    Vollzeit  =  F.Undergrad,
    Teilzeit  =  P.Undergrad,
    Kosten_ausserhalb  =  Outstate,
    Kosten_Unterkunft  =  Room.Board,
    Kosten_Buecher=  Books,
    Kosten_persoenlich  =  Personal,
    Prozent_PhD  =  PhD,
    Prozent_Degree  =  Terminal,
    Verhaeltnis_Stud.Doz.  =  S.F.Ratio,
    Prozent_Spenden  =  perc.alumni,
    Kosten_Student  =  Expend,
    Abschlussrate  =  Grad.Rate
    ) 


graduation_test = college_test %>% 
  mutate(Private = case_when(
    Private == 'No' ~ 'Nein',
    TRUE ~ 'Ja'
  )) %>% 
  rename(
    Privatuniversitaet  =  Private,
    Bewerbungen  =  Apps,
    Angenommen  =  Accept,
    Eingeschrieben  =  Enroll,
    Prozent_Top10  =  Top10perc,
    Prozent_Top25  =  Top25perc,
    Vollzeit  =  F.Undergrad,
    Teilzeit  =  P.Undergrad,
    Kosten_ausserhalb  =  Outstate,
    Kosten_Unterkunft  =  Room.Board,
    Kosten_Buecher=  Books,
    Kosten_persoenlich  =  Personal,
    Prozent_PhD  =  PhD,
    Prozent_Degree  =  Terminal,
    Verhaeltnis_Stud.Doz.  =  S.F.Ratio,
    Prozent_Spenden  =  perc.alumni,
    Kosten_Student  =  Expend,
    Abschlussrate  =  Grad.Rate
  )

write_csv(graduation_train, '_sessions/Prediction/1_Data/graduation_train.csv')
write_csv(graduation_test, '_sessions/Prediction/1_Data/graduation_test.csv')

cat(paste0('|',names(graduation_train),'|  |\n'),sep='')



house_train = read_csv('_sessions/Prediction/1_Data/house_train_eng.csv')
house_test = read_csv('_sessions/Prediction/1_Data/house_test_eng.csv')


cat(paste0('  = ',names(house_train),',\n'),sep='')

house_train = house_train %>%  rename(
  Preis  = price,
  Schlafzimmer  = bedrooms,
  Baeder  = bathrooms,
  Gesamt_sqft  = sqft_living,
  Grundstück_sqft  = sqft_lot,
  Stockwerke  = floors,
  Uferlage  = waterfront,
  Besichtigung  = view,
  Zustand  = condition,
  Einstufung  = grade,
  Wohnraum_sqft  = sqft_above,
  Keller_sqft  = sqft_basement,
  Baujahr  = yr_built,
  Renovationsjahr  = yr_renovated,
  Postleitzahl  = zipcode,
  Breitengrad  = lat,
  Laengengrad  = long,
  Gesamt_sqft_2015  = sqft_living15,
  Grundstück_sqft_2015  = sqft_lot15
  )


house_test = house_test %>%  rename(
  Preis  = price,
  Schlafzimmer  = bedrooms,
  Baeder  = bathrooms,
  Gesamt_sqft  = sqft_living,
  Grundstück_sqft  = sqft_lot,
  Stockwerke  = floors,
  Uferlage  = waterfront,
  Besichtigung  = view,
  Zustand  = condition,
  Einstufung  = grade,
  Wohnraum_sqft  = sqft_above,
  Keller_sqft  = sqft_basement,
  Baujahr  = yr_built,
  Renovationsjahr  = yr_renovated,
  Postleitzahl  = zipcode,
  Breitengrad  = lat,
  Laengengrad  = long,
  Gesamt_sqft_2015  = sqft_living15,
  Grundstück_sqft_2015  = sqft_lot15
)

write_csv(house_train, '_sessions/Prediction/1_Data/house_train.csv')
write_csv(house_test, '_sessions/Prediction/1_Data/house_test.csv')


cat(paste0('|',names(house_train),'|  |\n'),sep='')
