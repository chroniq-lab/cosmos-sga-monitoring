rm(list=ls());gc();source(".Rprofile")

query = "
SELECT DISTINCT mof_clean.PatientDurableKey as PatientDurableKey, mof_clean.SimpleGenericName as SimpleGenericName, 
dates.Year as Year, dates.YearMonth as YearMonth
FROM (
	SELECT	mof.PatientDurableKey as PatientDurableKey,
			mof.MedicationKey as MedicationKey,
			supreme.diagnosis_datekey as diagnosis_datekey,
			CASE WHEN (mof.StartDateKey = -1) THEN mof.EndDateKey
				ELSE mof.StartDateKey END AS DateOfEvent,
			MEDS.SimpleGenericName as SimpleGenericName
	FROM dbo.MedicationOrderFact mof
	INNER JOIN PROJECTS.ProjectD0C076.[ET4003\\shdw_1208_jvargh1].RFD03 supreme
	ON mof.PatientDurableKey = supreme.PatientDurableKey
	INNER JOIN (
	SELECT * 
    FROM dbo.MedicationDim
    WHERE PharmaceuticalClass LIKE '%ANTIPSYCHOTIC%'
	  AND PharmaceuticalClass NOT LIKE '%ATYP%'
	
	) MEDS
	ON mof.MedicationKey = MEDS.MedicationKey
) mof_clean
	
	INNER JOIN (SELECT * FROM dbo.PatientDim WHERE UseInCosmosAnalytics_X =1) as pd
	ON mof_clean.PatientDurableKey = pd.DurableKey
	INNER JOIN DateDim dates
	ON mof_clean.DateOfEvent = dates.DateKey
WHERE mof_clean.DateOfEvent > mof_clean.diagnosis_datekey 

"

dbGetQuery(con_Cosmos, query) %>% 
  write_dataset(.,path=paste0(path_sga_dm_control_folder,"/working/sdcdat101"),partitioning = "Year")

open_dataset(paste0(path_sga_dm_control_folder,"/working/sdcdat101"),partitioning = "Year") %>% 
  distinct(PatientDurableKey) %>% 
  tally() %>% 
  collect()

open_dataset(paste0(path_sga_dm_control_folder,"/working/sdcdat101"),partitioning = "Year") %>% 
  head()  %>% 
  collect()

