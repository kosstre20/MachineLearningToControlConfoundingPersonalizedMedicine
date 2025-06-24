
#------ To use the simulated dataset

# Set the working directory 
setwd("....")

# Load the simulated dataset from a CSV file
dat = read.csv("Simulation_data_Application.csv")

# Rename variables
dat = dat %>%
  rename(
    Treatment = A,
    Age = cage,
    Smoking_Status = ctabac,
    BMI = cbmi,
    Menopause_Status = meno,
    Grade = grade,
    HR_Status = HR,
    Stage = staging_rvalue,
    Surgery = type_chx,
    Family_History = histfm_1degre,
    Hormone_replacement_therapy = prise_hts,
    Chemotherapy = ind_chi,
    Radiotherapy = ind_rad,
    Herceptin_Therapy = ind_herceptin,
    Year = year_rec
  )

# Add variable labels to each variable
var_label(dat) = list(
  Treatment = "Treatment ",
  Age = "Age ",
  Smoking_Status = "Smoking Status ",
  BMI = "BMI ",
  Menopause_Status = "Menopause status ",
  Grade = "Grade ",
  HR_Status = "Hormone receptor status ",
  Stage = "Stage ",
  Surgery = "Surgery ",
  Family_History = "Family history of breast cancer ",
  Hormone_replacement_therapy = "Hormone replacement therapy usage ",
  Chemotherapy = "Chemotherapy ",
  Radiotherapy = "Radiotherapy ",
  Herceptin_Therapy = "Herceptin therapy ",
  Year = "Year"
)

# Recode variables as factors with descriptive category labels
dat$Treatment=factor(dat$Treatment, levels = c(0, 1), labels = c("No hormonal therapy", "Hormonal therapy"))
dat$Age=factor(dat$Age, levels = 1:5, labels = c("≤39 years", "40–49 years", "50–59 years", "60–69 years", "≥70 years"))
dat$Smoking_Status=factor(dat$Smoking_Status, levels = 1:3, labels = c("Never", "Ever", "Unknown"))
dat$BMI=factor(dat$BMI, levels = c(0, 1), labels = c("≤ 25 kg/m²", "≥ 25 kg/m²" ))
dat$Menopause_Status=factor(dat$Menopause_Status, levels = c(0, 1), labels = c("Premenopausal", "Postmenopausal"))
dat$Grade=factor(dat$Grade, levels = 1:4, labels = c("1", "2", "3", "Unknown"))
dat$HR_Status=factor(dat$HR_Status, levels = 1:3, labels = c("Positive", "Negative", "Unknown"))
dat$Stage=factor(dat$Stage, levels = 1:3, labels = c("I", "II", "III"))
dat$Surgery=factor(dat$Surgery, levels = 1:3, labels = c("Mastectomy", "Breast-conserving", "None"))
dat$Family_History=factor(dat$Family_History, levels = c(0, 1), labels = c("No", "Yes"))
dat$Hormone_replacement_therapy=factor(dat$Hormone_replacement_therapy, levels = c(0, 1), labels = c("No", "Yes"))
dat$Chemotherapy=factor(dat$Chemotherapy, levels = c(0, 1), labels = c("No", "Yes"))
dat$Radiotherapy=factor(dat$Radiotherapy, levels = c(0, 1), labels = c("No", "Yes"))
dat$Herceptin_Therapy=factor(dat$Herceptin_Therapy, levels = c(0, 1), labels = c("No", "Yes"))
dat$Year=factor(dat$Year, levels = 1:5, labels = c("1985–1989", "1990–1994", "1995–1999", "2000–2004", "2005–2009"))


# Descriptive statistics for covariate variables stratified by treatment 'A' (Table 5)
cov_names <- c("Age", "Smoking_Status", "BMI", "Menopause_Status", "Grade", "HR_Status",
                            "Stage", "Surgery", "Family_History", "Hormone_replacement_therapy", "Chemotherapy", 
                              "Radiotherapy", "Herceptin_Therapy", "Year")

print(CreateTableOne(vars = cov_names, strata = "Treatment", data = dat), test = FALSE, smd = TRUE)

