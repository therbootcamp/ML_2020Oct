
require(tidyverse)
hitters_test = read_csv('_sessions/Optimization/1_Data/hitters_train.csv')
hitters_train = read_csv('_sessions/Optimization/1_Data/hitters_test.csv')

cat(paste0('  = ',names(hitters_test),',\n'),sep='')

baseball_train = hitters_train %>% rename(
Schlaeger86 = AtBat,
Getroffen86  = Hits,
Home_runs86  = HmRun,
Runs86  = Runs,
Runs_andere86  = RBI,
Walks86  = Walks,
Erfahrung  = Years,
Schlaeger = CAtBat,
Getroffen  = CHits,
Home_runs  = CHmRun,
Runs  = CRuns,
Runs_andere  = CRBI,
Walks  = CWalks,
Liga  = League,
Division  = Division,
Putout86  = PutOuts,
Vorlage86  = Assists,
Fehler86  = Errors,
Gehalt  = Salary,
Liga87  = NewLeague) %>% 
  select(Gehalt, contains("86"), everything())

baseball_test = hitters_test %>% rename(
  Schlaeger86 = AtBat,
  Getroffen86  = Hits,
  Home_runs86  = HmRun,
  Runs86  = Runs,
  Runs_andere86  = RBI,
  Walks86  = Walks,
  Erfahrung  = Years,
  Schlaeger = CAtBat,
  Getroffen  = CHits,
  Home_runs  = CHmRun,
  Runs  = CRuns,
  Runs_andere  = CRBI,
  Walks  = CWalks,
  Liga  = League,
  Division  = Division,
  Putout86  = PutOuts,
  Vorlage86  = Assists,
  Fehler86  = Errors,
  Gehalt  = Salary,
  Liga87  = NewLeague) %>% 
  select(Gehalt, contains("86"), everything())

write_csv(baseball_train, "_sessions/Optimization/1_Data/baseball_train.csv")
write_csv(baseball_test, "_sessions/Optimization/1_Data/baseball_test.csv")


cat(paste0('|',names(baseball_train),'|  |\n'),sep='')
