library(Strategus)
library(PatientLevelPrediction)
library(dplyr)

# Sebastiaan Remmers
## Erasmus Medical Center, Rotterdam
## s.remmers@erasmusmc.nl
####################################

rm (list = ls())
# big data
cov1 <- FeatureExtraction::createCovariateSettings(
  useDemographicsAge = TRUE,
  useConditionGroupEraLongTerm = TRUE,
  useMeasurementMediumTerm = TRUE,
  useCharlsonIndex = TRUE)
cov2 <- FeatureExtraction::createCovariateSettings(
  useDemographicsAge = TRUE,
  useMeasurementMediumTerm = TRUE,
  useMeasurementLongTerm = TRUE,
  useMeasurementValueLongTerm = TRUE,
  useMeasurementValueMediumTerm = TRUE,
  useConditionGroupEraLongTerm = TRUE,
  useConditionGroupEraMediumTerm = TRUE,
  useConditionOccurrenceLongTerm = TRUE,
  useConditionOccurrenceMediumTerm = TRUE,
  useDrugEraLongTerm = TRUE,
  useDrugEraMediumTerm = TRUE,
  useDrugGroupEraLongTerm = TRUE,
  useDrugGroupEraMediumTerm = TRUE,
  useDrugGroupEraShortTerm = TRUE,
  useProcedureOccurrenceLongTerm = TRUE,
  useProcedureOccurrenceMediumTerm = TRUE,
  useCharlsonIndex = TRUE)

covariateSettings <- list(cov1, cov2)

populationSettings <- PatientLevelPrediction::createStudyPopulationSettings(
  washoutPeriod = 0,
  firstExposureOnly = FALSE,
  removeSubjectsWithPriorOutcome = FALSE,
  priorOutcomeLookback = 1,
  riskWindowStart = 0,
  riskWindowEnd = 365,
  startAnchor =  'cohort start',
  endAnchor =  'cohort start',
  minTimeAtRisk = 30,
  requireTimeAtRisk = FALSE,
  includeAllOutcomes = TRUE
)

splitSettings <-  createDefaultSplitSetting(
  trainFraction = 0.75,
  testFraction = 0.25,
  type = 'stratified',
  nfold = 3, 
  splitSeed = 1234
)

sampleSettings <- createSampleSettings()
featureEngineeringSettings <- createFeatureEngineeringSettings()

preprocessSettings <- createPreprocessSettings(
  minFraction = 0.001, 
  normalize = T, 
  removeRedundancy = T
)
outcomeIds <- c(27, 28, 29, 30)

lrModel <- setLassoLogisticRegression()
TAR <- c(365, 730, 1826, 3650)


modelDesignList <- list()
length(modelDesignList) <- length(TAR) * length(outcomeIds)

### clinical model
count <- 0
for (i in 1:length(TAR)) {
  for (j in 1:length(outcomeIds)) {
count <- count+1
    modelDesignList[[count]] <- PatientLevelPrediction::createModelDesign(
  targetId = 31,
  outcomeId = outcomeIds[j],
  restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(), 
  populationSettings = createStudyPopulationSettings(
    washoutPeriod = 0,
    firstExposureOnly = FALSE,
    removeSubjectsWithPriorOutcome = FALSE,
    priorOutcomeLookback = 1,
    riskWindowStart = 0,
    riskWindowEnd = TAR[i],
    startAnchor =  'cohort start',
    endAnchor =  'cohort start',
    minTimeAtRisk = 30,
    requireTimeAtRisk = FALSE,
    includeAllOutcomes = TRUE
  ),
  covariateSettings = list ( PatientLevelPrediction::createCohortCovariateSettings(
    cohortName = "grade 1",
    settingId = 1, cohortTable = NULL,
    cohortDatabaseSchema = NULL,
    cohortId = 6,
    startDay = 0,
    endDay = 0,
    analysisId = 300+count
  ),
  
  PatientLevelPrediction::createCohortCovariateSettings(
    cohortName = "grade 2",
    settingId = 1, cohortTable = NULL,
    cohortDatabaseSchema = NULL,
    cohortId = 7,
    startDay = 0,
    endDay = 0,
    analysisId = 300+count
  ),
  PatientLevelPrediction::createCohortCovariateSettings(
    cohortName = "grade 3",
    settingId = 1, cohortTable = NULL,
    cohortDatabaseSchema = NULL,
    cohortId = 8,
    startDay = 0,
    endDay = 0,
    analysisId = 300+count
  ),
  PatientLevelPrediction::createCohortCovariateSettings(
    cohortName = "grade 4",
    settingId = 1, cohortTable = NULL,
    cohortDatabaseSchema = NULL,
    cohortId = 9,
    startDay = 0,
    endDay = 0,
    analysisId = 300+count
  ),
  PatientLevelPrediction::createCohortCovariateSettings(
    cohortName = "grade 5",
    settingId = 1, cohortTable = NULL,
    cohortDatabaseSchema = NULL,
    cohortId = 10,
    startDay = 0,
    endDay = 0,
    analysisId = 300+count
  ),
  PatientLevelPrediction::createCohortCovariateSettings(
    cohortName = "psa_10",
    settingId = 1, cohortTable = NULL,
    cohortDatabaseSchema = NULL,
    cohortId = 11,
    startDay = 0,
    endDay = 0,
    analysisId = 300+count
  ),
  PatientLevelPrediction::createCohortCovariateSettings(
    cohortName = "psa_10_20",
    settingId = 1, cohortTable = NULL,
    cohortDatabaseSchema = NULL,
    cohortId = 12,
    startDay = 0,
    endDay = 0,
    analysisId = 300+count
  ),
  PatientLevelPrediction::createCohortCovariateSettings(
    cohortName = "psa_20",
    settingId = 1, cohortTable = NULL,
    cohortDatabaseSchema = NULL,
    cohortId = 13,
    startDay = 0,
    endDay = 0,
    analysisId = 300+count
  ),
  PatientLevelPrediction::createCohortCovariateSettings(
    cohortName = "age55",
    settingId = 1, cohortTable = NULL,
    cohortDatabaseSchema = NULL,
    cohortId = 14,
    startDay = 0,
    endDay = 0,
    analysisId = 300+count
  ),
  PatientLevelPrediction::createCohortCovariateSettings(
    cohortName = "age55_80",
    settingId = 1, cohortTable = NULL,
    cohortDatabaseSchema = NULL,
    cohortId = 15,
    startDay = 0,
    endDay = 0,
    analysisId = 300+count
  ),
  PatientLevelPrediction::createCohortCovariateSettings(
    cohortName = "age_80",
    settingId = 1, cohortTable = NULL,
    cohortDatabaseSchema = NULL,
    cohortId = 16,
    startDay = 0,
    endDay = 0,
    analysisId = 300+count
  ),
  PatientLevelPrediction::createCohortCovariateSettings(
    cohortName = "charlson 0",
    settingId = 1, cohortTable = NULL,
    cohortDatabaseSchema = NULL,
    cohortId = 17,
    startDay = 0,
    endDay = 0,
    analysisId = 300+count
  ),PatientLevelPrediction::createCohortCovariateSettings(
    cohortName = "charlson 1",
    settingId = 1, cohortTable = NULL,
    cohortDatabaseSchema = NULL,
    cohortId = 18,
    startDay = 0,
    endDay = 0,
    analysisId = 300+count
  ),
  PatientLevelPrediction::createCohortCovariateSettings(
    cohortName = "charlson 2 and higher",
    settingId = 1, cohortTable = NULL,
    cohortDatabaseSchema = NULL,
    cohortId = 19,
    startDay = 0,
    endDay = 0,
    analysisId = 300+count
  ),
  PatientLevelPrediction::createCohortCovariateSettings(
    cohortName = "T1",
    settingId = 1, cohortTable = NULL,
    cohortDatabaseSchema = NULL,
    cohortId = 20,
    startDay = 0,
    endDay = 0,
    analysisId = 300+count
  ),
  PatientLevelPrediction::createCohortCovariateSettings(
    cohortName = "T2",
    settingId = 1, cohortTable = NULL,
    cohortDatabaseSchema = NULL,
    cohortId = 21,
    startDay = 0,
    endDay = 0,
    analysisId = 300+count
  ),
  PatientLevelPrediction::createCohortCovariateSettings(
    cohortName = "T34",
    settingId = 1, cohortTable = NULL,
    cohortDatabaseSchema = NULL,
    cohortId = 22,
    startDay = 0,
    endDay = 0,
    analysisId = 300+count
  ),
  PatientLevelPrediction::createCohortCovariateSettings(
    cohortName = "Family history of PCa",
    settingId = 1, cohortTable = NULL,
    cohortDatabaseSchema = NULL,
    cohortId = 23,
    startDay = 0,
    endDay = 0,
    analysisId = 300+count
  ),
  PatientLevelPrediction::createCohortCovariateSettings(
    cohortName = "Stroke",
    settingId = 1, cohortTable = NULL,
    cohortDatabaseSchema = NULL,
    cohortId = 24,
    startDay = 0,
    endDay = 0,
    analysisId = 300+count
  ),
  PatientLevelPrediction::createCohortCovariateSettings(
    cohortName = "Type 2 diabetes",
    settingId = 1, cohortTable = NULL,
    cohortDatabaseSchema = NULL,
    cohortId = 25,
    startDay = 0,
    endDay = 0,
    analysisId = 300+count
  ),
  PatientLevelPrediction::createCohortCovariateSettings(
    cohortName = "M1 disease",
    settingId = 1, cohortTable = NULL,
    cohortDatabaseSchema = NULL,
    cohortId = 26,
    startDay = 0,
    endDay = 0,
    analysisId = 300+count
  )),
  featureEngineeringSettings = featureEngineeringSettings,
  sampleSettings = sampleSettings,
  preprocessSettings = preprocessSettings,
  modelSettings = lrModel,
  runCovariateSummary = T)
  }
}


modelDesignList_bigdata <- list ()
length(modelDesignList_bigdata) <- length(TAR) * length(outcomeIds)

count <- 0
for (i in 1:length(TAR)) {
  for (j in 1:length(outcomeIds)) {
    count <- count + 1
    modelDesignList_bigdata[[count]] <- PatientLevelPrediction::createModelDesign(
  targetId = 31,
  outcomeId = outcomeIds[j],
  restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(), 
  populationSettings = createStudyPopulationSettings(
    washoutPeriod = 0,
    firstExposureOnly = FALSE,
    removeSubjectsWithPriorOutcome = FALSE,
    priorOutcomeLookback = 1,
    riskWindowStart = 0,
    riskWindowEnd = TAR[i],
    startAnchor =  'cohort start',
    endAnchor =  'cohort start',
    minTimeAtRisk = 30,
    requireTimeAtRisk = FALSE,
    includeAllOutcomes = TRUE
  ),
  covariateSettings = 
    FeatureExtraction::createCovariateSettings(
      useDemographicsAge = TRUE,
      useMeasurementMediumTerm = TRUE,
      useMeasurementLongTerm = TRUE,
      useMeasurementValueLongTerm = TRUE,
      useMeasurementValueMediumTerm = TRUE,
      useConditionGroupEraLongTerm = TRUE,
      useConditionGroupEraMediumTerm = TRUE,
      useConditionOccurrenceLongTerm = TRUE,
      useConditionOccurrenceMediumTerm = TRUE,
      useDrugEraLongTerm = TRUE,
      useDrugEraMediumTerm = TRUE,
      useDrugGroupEraLongTerm = TRUE,
      useDrugGroupEraMediumTerm = TRUE,
      useDrugGroupEraShortTerm = TRUE,
      useProcedureOccurrenceLongTerm = TRUE,
      useProcedureOccurrenceMediumTerm = TRUE,
      useCharlsonIndex = TRUE),
  featureEngineeringSettings = featureEngineeringSettings,
  sampleSettings = sampleSettings,
  preprocessSettings = preprocessSettings,
  modelSettings = lrModel,
  runCovariateSummary = T)
}}

createPatientLevelPredictionModuleSpecifications <- function() {
  specifications <- list(
    module = "PatientLevelPredictionModule",
    version = "0.0.7",
    remoteRepo = "github.com",
    remoteUsername = "ohdsi",
    settings = modelDesignList # out design list
  )
  class(specifications) <- c("DeepPatientLevelPredictionModuleSpecifications", "ModuleSpecifications")
  return(specifications)
}


createPatientLevelPredictionModuleSpecifications2 <- function() {
  specifications <- list(
    module = "PatientLevelPredictionModule",
    version = "0.0.7",
    remoteRepo = "github.com",
    remoteUsername = "ohdsi",
    settings = modelDesignList_bigdata # out design list
  )
  class(specifications) <- c("DeepPatientLevelPredictionModuleSpecifications", "ModuleSpecifications")
  return(specifications)
}

createCohortGeneratorModuleSpecifications <- function() {
  specifications <- list(module = "CohortGeneratorModule",
                         version = "0.0.10",
                         remoteRepo = "github.com",
                         remoteUsername = "ohdsi",
                         settings = list(
                           incremental = TRUE,
                           generateStats = TRUE
                         )
  )
  class(specifications) <- c("CohortGeneratorModuleSpecifications", "ModuleSpecifications")
  return(specifications)
}


createCohortSharedResource <- function(cohortDefinitionSet) {
  sharedResource <- list(cohortDefinitions = cohortDefinitionSet)
  class(sharedResource) <- c("CohortDefinitionSharedResources", "SharedResources")
  return(sharedResource)
}

baseUrl = "http://localhost:8080/WebAPI"

cohortDefinitions <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl, 
  cohortIds = c(6:31), 
  generateStats = F # set this to T if you want stats
)


cohortDefinitions <- lapply(1:length(cohortDefinitions$atlasId), function(i){list(
  cohortId = cohortDefinitions$cohortId[i],
  cohortName = cohortDefinitions$cohortName[i],
  cohortDefinition = cohortDefinitions$json[i]
)})

analysisSpecifications <- createEmptyAnalysisSpecificiations() %>%
  addSharedResources(createCohortSharedResource(cohortDefinitions)) %>%
  addModuleSpecifications(createCohortGeneratorModuleSpecifications()) %>%
  addModuleSpecifications(createPatientLevelPredictionModuleSpecifications())


analysisSpecifications_bigdata <- createEmptyAnalysisSpecificiations() %>%
  addSharedResources(createCohortSharedResource(cohortDefinitions)) %>%
  addModuleSpecifications(createCohortGeneratorModuleSpecifications()) %>%
  addModuleSpecifications(createPatientLevelPredictionModuleSpecifications2())

outputfolder <- getwd()
ParallelLogger::saveSettingsToJson(analysisSpecifications, paste0(outputfolder, "/PIONEER_clinician_driven_model.json"))
ParallelLogger::saveSettingsToJson(analysisSpecifications_bigdata, paste0(outputfolder, "/PIONEER_bigdata_model.json"))

