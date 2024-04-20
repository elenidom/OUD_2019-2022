
dput(names(meds_all))

a <- meds_all %>% dplyr::select(c("patid", "therapy", "mydrugsubstance_all", "anydrug")) %>% distinct()

a <- tidyr::spread(a, mydrugsubstance_all, anydrug)

myvars <- c("Agomelatine_Agomelatine", "Alprazolam_Alprazolam", 
            "Amisulpride_Amisulpride", "_Amitriptyline", "Amitriptyline hydrochloride_Amitriptyline", 
              "Amitriptyline hydrochloride/ Perphenazine_Perphenazine_Amitriptyline",
              "Amoxapine_Amoxapine", "Aripiprazole_Aripiprazole", "Atomoxetine hydrochloride_Atomoxetine",
              "Bupropion hydrochloride_Bupropion", "Carbamazepine_Carbamazepine",
              "_Chlordiazepoxide", "Chlordiazepoxide hydrochloride_Chlordiazepoxide",
              "_Chlorpromazine", "Chlorpromazine hydrochloride_Chlorpromazine",
              "Citalopram hydrobromide_Citalopram", "Citalopram hydrochloride_Citalopram",
              "Clobazam_Clobazam", "Clomipramine hydrochloride_Clomipramine",
              "_Clonazepam", "Clonazepam_Clonazepam", "_Clorazepate", "Clozapine_Clozapine",
              "_Diazepam", "Diazepam_Diazepam", "Dosulepin hydrochloride_Dosulepin",
              "Doxepin hydrochloride_Doxepin", "_Droperidol", "Duloxetine hydrochloride_Duloxetine",
              "Escitalopram oxalate_Escitalopram", "Flunitrazepam_Flunitrazepam",
              "Fluoxetine hydrochloride_Fluoxetine", "Flupentixol decanoate_Flupentixol",
              "Flupentixol dihydrochloride_Flupentixol", "Fluphenazine decanoate_Fluphenazine",
              "Fluphenazine hydrochloride_Fluphenazine", "Fluphenazine hydrochloride/ Nortriptyline hydrochloride_Fluphenazine_Nortriptyline",
              "Fluvoxamine maleate_Fluvoxamine", "_Gabapentin", "Gabapentin_Gabapentin",
              "_Haloperidol", "Haloperidol decanoate_Haloperidol", "Haloperidol_Haloperidol",
              "Imipramine hydrochloride_Imipramine", "Isocarboxazid_Isocarboxazid",
              "_Ketamine", "Ketamine hydrochloride_Ketamine", "Lamotrigine_Lamotrigine",
              "Levomepromazine hydrochloride_Levomepromazine", "Levomepromazine maleate_Levomepromazine",
              "Lithium carbonate_Lithium carbonate", "Lithium citrate_Lithium citrate",
              "Lofepramine hydrochloride_Lofepramine", "Loprazolam mesilate_Loprazolam",
              "Lorazepam_Lorazepam", "Lormetazepam_Lormetazepam", "Lurasidone hydrochloride_Lurasidone",
              "Maprotiline hydrochloride_Maprotiline", "Mianserin hydrochloride_Mianserin",
              "_Midazolam", "Midazolam hydrochloride_Midazolam", "Mirtazapine_Mirtazapine",
              "Moclobemide_Moclobemide", "_Nefazodone", "Nefazodone hydrochloride_Nefazodone",
              "_Nitrazepam", "Nitrazepam_Nitrazepam", "Nortriptyline hydrochloride_Nortriptyline",
              "Olanzapine_Olanzapine", "_Oxazepam", "Oxazepam_Oxazepam", "Oxcarbazepine_Oxcarbazepine",
              "Oxitriptan_Oxitriptan", "Paliperidone_Paliperidone", "Paliperidone palmitate_Paliperidone",
              "Paroxetine hydrochloride_Paroxetine", "Pericyazine_Pericyazine",
              "Perphenazine_Perphenazine", "Pipotiazine palmitate_Pipotiazine",
              "Pregabalin_Pregabalin", "_Prochlorperazine", "Prochlorperazine maleate_Prochlorperazine",
              "_Protriptyline", "_Quetiapine", "Quetiapine fumarate_Quetiapine",
              "Reboxetine mesilate_Reboxetine", "Risperidone_Risperidone",
              "Sertraline hydrochloride_Sertraline", "_Sodium valproate", "Sodium valproate_Sodium valproate",
              "Sulpiride_Sulpiride", "_Temazepam", "Temazepam_Temazepam", "Thioridazine hydrochloride_Thioridazine",
              "Thioridazine_Thioridazine", "Topiramate_Topiramate", "Trazodone hydrochloride_Trazodone",
              "Trifluoperazine hydrochloride_Trifluoperazine", "Trimipramine maleate_Trimipramine",
              "Tryptophan_Tryptophan", "Valproate semisodium_Valproate semisodium",
              "Valproic acid_Valproic acid", "Venlafaxine hydrochloride_Venlafaxine",
              "Vortioxetine hydrobromide_Vortioxetine", "Zuclopenthixol acetate_Zuclopenthixol",
              "Zuclopenthixol decanoate_Zuclopenthixol", "Zuclopenthixol dihydrochloride_Zuclopenthixol"
)

cols <- dput(names(a))
a[,cols] <- lapply(a[,cols], factor)

# Assign strata
factorVars <- "therapy"

# produce TableOne
TOoutput <- CreateTableOne(vars = myvars, strata = factorVars, data = a)
TOoutput
capture.output(print(TOoutput, formatOptions = list(big.mark = ",")), 
               file = paste0("./TableOne_Spread_Drugs.txt"))


# ====================================================================================================


dput(names(meds_all))

a <- meds_all %>% dplyr::select(c("patid", "therapy", "myclass_all", "anydrug")) %>% distinct()

a <- tidyr::spread(a, myclass_all, anydrug)

myvars <- c(c("Anticonvulsant", "Benzodiazepine", "FirstGeneration", 
              "FirstGeneration_TCA", "Gabapentinoids", "Lithium", "MAOI", "NRI", 
              "OffLabel", "Other_antidepressant", "SARI", "SecondGeneration", 
              "SMS", "SNRI", "SSRI", "TCA", "TeCA"))

cols <- dput(names(a))
a[,cols] <- lapply(a[,cols], factor)

# Assign strata
factorVars <- "therapy"

# produce TableOne
TOoutput <- CreateTableOne(vars = myvars, strata = factorVars, data = a)
TOoutput
capture.output(print(TOoutput, formatOptions = list(big.mark = ",")), 
               file = paste0("./TableOne_Drug_Classes.txt"))


