# ============================================================
# Project: add-health-adolescent-risk-models
# Script 01b: Thesis Questionnaire Construct Inventory
# Author: Gelo Picol
#
# Purpose:
#   Convert the doctoral thesis questionnaire into a structured
#   construct inventory for later mapping to Add Health public-use
#   documentation and variables.
#
# Important:
#   This script does not import, process, or export individual-level
#   data from the thesis or from Add Health.
#
#   It only creates a metadata inventory based on the thesis
#   questionnaire structure: construct blocks, item codes, response
#   scales, theoretical models and Add Health search keywords.
# ============================================================


# ============================================================
# 0. Project root
# ============================================================

project_root <- "D:/GitHub/add-health-adolescent-risk-models"


# ============================================================
# 1. Required packages
# ============================================================

required_packages <- c(
  "dplyr",
  "tibble",
  "readr",
  "stringr",
  "openxlsx"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(dplyr)
library(tibble)
library(readr)
library(stringr)
library(openxlsx)


# ============================================================
# 2. Check folder structure
# ============================================================

required_folders <- c(
  "R",
  "data",
  "data/raw",
  "data/processed",
  "data/metadata",
  "outputs",
  "outputs/tables",
  "outputs/figures",
  "outputs/diagnostics",
  "outputs/logs",
  "docs"
)

for (folder in required_folders) {
  dir.create(
    file.path(project_root, folder),
    recursive = TRUE,
    showWarnings = FALSE
  )
}


# ============================================================
# 3. Questionnaire metadata
# ============================================================

questionnaire_metadata <- tibble(
  field = c(
    "source_document",
    "source_type",
    "original_project",
    "target_population",
    "school_grade_range",
    "age_range",
    "geographic_scope",
    "main_topics",
    "purpose_in_current_project",
    "microdata_imported"
  ),
  value = c(
    "Doctoral thesis questionnaire",
    "Research instrument metadata",
    "Doctoral thesis on adolescent pregnancy, HIV infection and sexual risk behaviour",
    "Secondary-school adolescents",
    "10th to 12th grade",
    "15 to 19 years",
    "Matola, Boane and Namaacha, Maputo Province, Mozambique",
    "Adolescent pregnancy, HIV, sexual risk behaviour, family connection, school connection and health beliefs",
    "Create a construct inventory to guide Add Health variable mapping",
    "No"
  )
)


# ============================================================
# 4. Response scale dictionary
# ============================================================

response_scale_dictionary <- tibble(
  scale_id = c(
    "single_choice_location",
    "single_choice_gender",
    "single_choice_age",
    "single_choice_grade",
    "asset_count",
    "yes_no",
    "sexual_history_category",
    "likert_agreement_5",
    "self_efficacy_certainty_5",
    "true_false",
    "open_text"
  ),
  scale_description = c(
    "Single categorical response for district or place of residence",
    "Single categorical response for gender",
    "Single categorical response for age group",
    "Single categorical response for school grade",
    "Categorical household asset ownership or frequency",
    "Binary yes/no response",
    "Categorical response on sexual initiation, partners, condom/contraceptive use and frequency of sexual activity",
    "Five-point Likert agreement scale from strong disagreement to complete agreement",
    "Five-point certainty scale from no certainty to complete certainty",
    "True/false knowledge item",
    "Open-ended qualitative response"
  ),
  original_response_options = c(
    "Matola; Boane; Namaacha",
    "Female; Male; Other",
    "15; 16; 17; 18; 19",
    "10th; 11th; 12th grade",
    "None; one; two; more than two; or equivalent item-specific categories",
    "Yes; No",
    "Item-specific categories",
    "Strongly disagree; disagree a little; neither agree nor disagree; agree somewhat; agree completely",
    "No certainty; some certainty; half certainty; much certainty; total certainty",
    "True; False",
    "Narrative answer"
  ),
  numeric_coding_suggestion = c(
    "Use factor coding",
    "Use factor coding",
    "Use numeric age where available",
    "Use ordered factor or numeric grade",
    "Use ordered factor or asset index",
    "0 = No; 1 = Yes",
    "Use item-specific ordered coding",
    "1 to 5, with reverse coding where theoretically required",
    "1 to 5",
    "0 = Incorrect; 1 = Correct, after defining correct answers",
    "Qualitative thematic coding only"
  )
)


# ============================================================
# 5. Thesis questionnaire construct inventory
# ============================================================
# The table below is a metadata translation of the thesis questionnaire.
# It is not a dataset and contains no individual-level answers.

thesis_questionnaire_inventory <- tribble(
  ~thesis_section, ~thesis_item_code, ~item_short_description, ~theoretical_model, ~construct, ~variable_role, ~response_scale, ~expected_add_health_equivalent, ~priority_for_mapping, ~notes,

  # -----------------------------
  # Demographic and sample variables
  # -----------------------------
  "Demographic and socioeconomic characteristics", "RES", "Place of residence: Matola, Boane or Namaacha", "Socioecological model", "Geographic context", "Control or stratification variable", "single_choice_location", "Urban/rural or region variable, if public-use Add Health permits", "Medium", "Direct district equivalence is not expected in Add Health.",
  "Demographic and socioeconomic characteristics", "GEN", "Gender", "Socioecological model", "Gender", "Control and heterogeneity variable", "single_choice_gender", "Sex or gender variable", "High", "Core control variable.",
  "Demographic and socioeconomic characteristics", "AGE", "Age from 15 to 19 years", "Socioecological model", "Age", "Sample criterion and control variable", "single_choice_age", "Age at Wave I", "High", "Complementary alignment criterion with the thesis.",
  "Demographic and socioeconomic characteristics", "GRADE", "School grade: 10th, 11th or 12th", "Socioecological model", "School grade", "Main sample criterion", "single_choice_grade", "Grade at Wave I", "High", "Primary comparability criterion with the thesis.",

  # -----------------------------
  # Socioeconomic status
  # -----------------------------
  "Household socioeconomic characteristics", "AFL1", "Family owns car, van or truck", "Socioecological model", "Household assets", "SES proxy", "asset_count", "Household assets or parental SES", "High", "Candidate for asset-based socioeconomic index.",
  "Household socioeconomic characteristics", "AFL2", "Own bedroom", "Socioecological model", "Household assets", "SES proxy", "asset_count", "Own room or housing conditions", "Medium", "May have approximate equivalent.",
  "Household socioeconomic characteristics", "AFL3", "Family vacation frequency in the last 12 months", "Socioecological model", "Household assets", "SES proxy", "asset_count", "Family SES or household resources", "Medium", "May not have direct equivalent.",
  "Household socioeconomic characteristics", "AFL4", "Number of computers at home", "Socioecological model", "Household assets", "SES proxy", "asset_count", "Computer ownership or household assets", "Medium", "May have equivalent depending on Add Health wave.",

  # -----------------------------
  # Sexuality information and behavioural outcomes
  # -----------------------------
  "Sexuality information", "CULT_PRESEX", "Cultural permissiveness toward sexual relations before adulthood", "Socioecological model", "Cultural norms", "Contextual explanatory variable", "yes_no", "Attitudes, family norms or perceived norms about adolescent sex", "Medium", "Direct equivalence uncertain.",
  "Sexuality information", "SEX_EVER", "Ever had sexual relations", "Behavioural risk model", "Sexual initiation", "Outcome or sample stratification variable", "yes_no", "Ever had sexual intercourse", "High", "Core behavioural variable.",
  "Sexuality information", "ISX1", "Age at first sexual relation", "Behavioural risk model", "Early sexual initiation", "Outcome or risk indicator", "sexual_history_category", "Age at first intercourse", "High", "Central outcome or timing variable.",
  "Sexuality information", "ISX2", "Number of sexual partners in the last year", "Behavioural risk model", "Multiple partners", "Outcome or risk indicator", "sexual_history_category", "Number of sexual partners", "High", "Sensitive variable; report only aggregate outputs.",
  "Sexuality information", "ISX3", "Frequency of condom or contraceptive use in the last year", "Behavioural risk model", "Condom or contraceptive use", "Main outcome or protective behaviour", "sexual_history_category", "Condom use and contraceptive use", "High", "Central outcome for protection models.",
  "Sexuality information", "ISX4", "Frequency of sexual relations in the last year", "Behavioural risk model", "Sexual activity frequency", "Risk exposure indicator", "sexual_history_category", "Frequency of sexual intercourse", "Medium", "Use with disclosure caution.",

  # -----------------------------
  # Health Belief Model: perceived susceptibility
  # -----------------------------
  "Health questions", "SUP1", "Possibility of contracting AIDS", "Health Belief Model", "Perceived susceptibility", "Explanatory construct", "likert_agreement_5", "Perceived risk of HIV or STI infection", "High", "MCS core construct.",
  "Health questions", "SUP2", "Fear of contracting AIDS", "Health Belief Model", "Perceived susceptibility", "Explanatory construct", "likert_agreement_5", "Worry or fear about HIV/STI", "High", "MCS core construct.",
  "Health questions", "SUP3", "Perceived HIV exposure through heterosexual partner", "Health Belief Model", "Perceived susceptibility", "Explanatory construct", "likert_agreement_5", "Perceived partner-related HIV/STI risk", "Medium", "Direct equivalent uncertain.",
  "Health questions", "SUP4", "Perceived risk even with one sexual partner", "Health Belief Model", "Perceived susceptibility", "Explanatory construct", "likert_agreement_5", "Perceived HIV/STI risk with partner context", "Medium", "Direct equivalent uncertain.",

  # -----------------------------
  # Health Belief Model: perceived severity
  # -----------------------------
  "Health questions", "SEP1", "AIDS causes death", "Health Belief Model", "Perceived severity", "Explanatory construct", "likert_agreement_5", "Perceived seriousness of HIV/AIDS", "High", "MCS core construct.",
  "Health questions", "SEP2", "Preference for another terminal disease instead of AIDS", "Health Belief Model", "Perceived severity", "Explanatory construct", "likert_agreement_5", "Perceived seriousness or fear of AIDS", "Low", "May be too specific for Add Health.",
  "Health questions", "SEP3", "Preference for violent death rather than AIDS death", "Health Belief Model", "Perceived severity", "Explanatory construct", "likert_agreement_5", "Perceived seriousness or fear of AIDS", "Low", "Sensitive wording; unlikely equivalent.",
  "Health questions", "SEP4", "AIDS as probably the worst disease", "Health Belief Model", "Perceived severity", "Explanatory construct", "likert_agreement_5", "Perceived seriousness of HIV/AIDS", "Medium", "MCS severity indicator.",

  # -----------------------------
  # Health Belief Model: perceived benefits
  # -----------------------------
  "Health questions", "BEP1", "Condom use reduces probability of AIDS", "Health Belief Model", "Perceived benefits", "Explanatory construct", "likert_agreement_5", "Belief that condom use prevents HIV/STI", "High", "MCS benefit construct.",
  "Health questions", "BEP2", "Worth carrying condoms", "Health Belief Model", "Perceived benefits", "Explanatory construct", "likert_agreement_5", "Perceived benefit of condom availability", "Medium", "May have behavioural or attitude equivalent.",
  "Health questions", "BEP3", "One sexual partner reduces probability of AIDS", "Health Belief Model", "Perceived benefits", "Explanatory construct", "likert_agreement_5", "Belief in monogamy or partner reduction as prevention", "Medium", "May have approximate equivalent.",
  "Health questions", "BEP4", "Worth abstaining if no condom is available", "Health Belief Model", "Perceived benefits", "Explanatory construct", "likert_agreement_5", "Belief in abstinence or delay when protection unavailable", "High", "Bridge to intention to delay sex.",

  # -----------------------------
  # Health Belief Model: perceived barriers
  # -----------------------------
  "Health questions", "BAP1", "Condom use may be seen by partner as an insult", "Health Belief Model", "Perceived barriers", "Explanatory construct", "likert_agreement_5", "Partner-related barrier to condom use", "High", "MCS barrier construct.",
  "Health questions", "BAP2", "Embarrassment in buying condoms", "Health Belief Model", "Perceived barriers", "Explanatory construct", "likert_agreement_5", "Embarrassment or access barrier to condoms", "High", "MCS barrier construct.",
  "Health questions", "BAP3", "Reduced pleasure with condom use", "Health Belief Model", "Perceived barriers", "Explanatory construct", "likert_agreement_5", "Pleasure-related barrier to condom use", "Medium", "Direct equivalent uncertain.",

  # -----------------------------
  # Self-efficacy for condom use
  # -----------------------------
  "Health questions", "AEF1", "Ability to use a condom correctly", "Health Belief Model / Planned Behaviour", "Condom self-efficacy", "Explanatory construct", "self_efficacy_certainty_5", "Condom-use self-efficacy", "High", "Important bridge between MCS and TCP.",
  "Health questions", "AEF2", "Ability to use a condom every time", "Health Belief Model / Planned Behaviour", "Condom self-efficacy", "Explanatory construct", "self_efficacy_certainty_5", "Consistent condom-use self-efficacy", "High", "Central self-efficacy item.",
  "Health questions", "AEF3", "Ability to use condom after alcohol consumption", "Health Belief Model / Planned Behaviour", "Condom self-efficacy under risk", "Explanatory construct", "self_efficacy_certainty_5", "Condom self-efficacy in risky contexts", "Medium", "May have approximate equivalent.",
  "Health questions", "AEF4", "Ability to insist on condom use if partner refuses", "Health Belief Model / Planned Behaviour", "Negotiation self-efficacy", "Explanatory construct", "self_efficacy_certainty_5", "Condom negotiation self-efficacy", "High", "Very important for TCP control.",
  "Health questions", "AEF5", "Ability to refuse sex without condom", "Health Belief Model / Planned Behaviour", "Refusal self-efficacy", "Explanatory construct", "self_efficacy_certainty_5", "Refusal self-efficacy", "High", "Important protective construct.",
  "Health questions", "AEF6", "Financial ability to buy condoms", "Health Belief Model / Planned Behaviour", "Access self-efficacy", "Explanatory construct", "self_efficacy_certainty_5", "Financial access to condoms", "Medium", "May map to access barriers.",
  "Health questions", "AEF7", "Ability to enter a shop and buy condoms", "Health Belief Model / Planned Behaviour", "Access self-efficacy", "Explanatory construct", "self_efficacy_certainty_5", "Comfort buying condoms", "Medium", "May map to embarrassment or access.",

  # -----------------------------
  # HIV/AIDS knowledge and information
  # -----------------------------
  "HIV/AIDS knowledge", "INA1", "Knows someone infected with HIV/AIDS", "Health Belief Model", "Exposure to HIV/AIDS information", "Contextual or knowledge variable", "yes_no", "Knows someone with HIV/AIDS", "Medium", "Possible perceived risk correlate.",
  "HIV/AIDS knowledge", "INA2", "Discussed HIV/AIDS with friends or colleagues", "Health Belief Model / Socioecological", "Peer discussion", "Information and peer context", "yes_no", "Talked with friends about HIV/STI", "Medium", "Peer communication variable.",
  "HIV/AIDS knowledge", "INA3", "Followed HIV/AIDS information through media", "Health Belief Model", "Media information exposure", "Information variable", "yes_no", "Media exposure to HIV/STI information", "Medium", "May map to health information exposure.",
  "HIV/AIDS knowledge", "INA4", "Teachers discussed HIV/AIDS at school", "Socioecological model", "School-based information", "School context variable", "yes_no", "School HIV/STI education", "High", "Important school-level exposure.",
  "HIV/AIDS knowledge", "CVS1", "AIDS means Acquired Immunodeficiency Syndrome", "Health Belief Model", "HIV/AIDS factual knowledge", "Knowledge item", "true_false", "HIV/AIDS knowledge item", "High", "Can contribute to knowledge index.",
  "HIV/AIDS knowledge", "CVS2", "Most people with AIDS recover", "Health Belief Model", "HIV/AIDS factual knowledge", "Knowledge item", "true_false", "HIV/AIDS misconception item", "High", "Reverse-coded if incorrect.",
  "HIV/AIDS knowledge", "CVS3", "HIV is found in blood", "Health Belief Model", "HIV/AIDS factual knowledge", "Knowledge item", "true_false", "HIV transmission knowledge", "High", "Knowledge index candidate.",
  "HIV/AIDS knowledge", "CVS4", "HIV/AIDS can be contracted from toilet seats", "Health Belief Model", "HIV/AIDS misconception", "Knowledge item", "true_false", "HIV misconception item", "High", "Reverse-coded if incorrect.",
  "HIV/AIDS knowledge", "CVS5", "HIV/AIDS can be contracted from shared swimming pool", "Health Belief Model", "HIV/AIDS misconception", "Knowledge item", "true_false", "HIV misconception item", "High", "Reverse-coded if incorrect.",
  "HIV/AIDS knowledge", "CVS6", "HIV/AIDS can be contracted from deep kissing", "Health Belief Model", "HIV/AIDS misconception", "Knowledge item", "true_false", "HIV misconception item", "Medium", "Depends on Add Health items.",
  "HIV/AIDS knowledge", "CVS7", "Good idea to ask partner about past sexual practices", "Health Belief Model / Planned Behaviour", "Preventive communication", "Knowledge or attitude item", "true_false", "Partner communication about sexual history", "Medium", "May map to communication or attitude.",
  "HIV/AIDS knowledge", "CVS8", "Condom use reduces probability of AIDS", "Health Belief Model", "HIV prevention knowledge", "Knowledge item", "true_false", "Condom effectiveness knowledge", "High", "Strong candidate for knowledge index.",
  "HIV/AIDS knowledge", "CVS9", "Abstinence is safer than sex with condom", "Health Belief Model", "HIV prevention knowledge", "Knowledge item", "true_false", "Abstinence or prevention belief", "Medium", "Requires careful coding.",

  # -----------------------------
  # Planned Behaviour: attitudes
  # -----------------------------
  "Planned Behaviour constructs", "AIS1", "Wrong to have sexual relations while minor", "Theory of Planned Behaviour", "Attitude toward sexual initiation", "Explanatory construct", "likert_agreement_5", "Attitudes toward adolescent sex", "Medium", "May map to attitudes about sex timing.",
  "Planned Behaviour constructs", "AIS2", "Adolescents would be better if they said no to sex", "Theory of Planned Behaviour", "Attitude toward delaying sex", "Explanatory construct", "likert_agreement_5", "Attitude toward abstinence or delay", "High", "Bridge to intention to delay sex.",
  "Planned Behaviour constructs", "AIS3", "Delaying sex until adulthood is important", "Theory of Planned Behaviour", "Attitude toward delaying sex", "Explanatory construct", "likert_agreement_5", "Importance of delaying sex", "High", "Core attitude item.",
  "Planned Behaviour constructs", "APC1", "Condom and contraceptive should always be used by sexually active adolescents", "Theory of Planned Behaviour", "Attitude toward condom/contraceptive use", "Explanatory construct", "likert_agreement_5", "Attitude toward condom/contraceptive use", "High", "Core TCP attitude item.",
  "Planned Behaviour constructs", "APC2", "Condom or contraceptive should always be used even if partners know each other well", "Theory of Planned Behaviour", "Attitude toward consistent protection", "Explanatory construct", "likert_agreement_5", "Attitude toward consistent condom use", "High", "Core TCP attitude item.",
  "Planned Behaviour constructs", "APC3", "Another contraceptive should always be used even if the boy uses a condom", "Theory of Planned Behaviour", "Attitude toward dual protection", "Explanatory construct", "likert_agreement_5", "Attitude toward contraception or dual protection", "Medium", "May map to contraception attitudes.",

  # -----------------------------
  # Planned Behaviour: perceived behavioural control/self-efficacy
  # -----------------------------
  "Planned Behaviour constructs", "EPC1", "Not very difficult to buy condoms or contraceptives", "Theory of Planned Behaviour", "Perceived behavioural control", "Explanatory construct", "likert_agreement_5", "Ease of buying condoms/contraceptives", "High", "TCP control item.",
  "Planned Behaviour constructs", "EPC2", "Not very difficult to have condom or contraceptive available when needed", "Theory of Planned Behaviour", "Perceived behavioural control", "Explanatory construct", "likert_agreement_5", "Ease of having protection available", "High", "TCP control item.",
  "Planned Behaviour constructs", "EPC3", "Could convince partner to use condom or contraceptive", "Theory of Planned Behaviour", "Negotiation self-efficacy", "Explanatory construct", "likert_agreement_5", "Partner negotiation for condom/contraceptive use", "High", "TCP control item.",
  "Planned Behaviour constructs", "EIS1", "Can delay sexual initiation until adulthood", "Theory of Planned Behaviour", "Self-efficacy to delay sex", "Mediator or explanatory construct", "likert_agreement_5", "Self-efficacy to delay sex", "High", "Relevant for mediation model.",
  "Planned Behaviour constructs", "EIS2", "Can say no if liked person wants sex", "Theory of Planned Behaviour", "Refusal self-efficacy", "Mediator or explanatory construct", "likert_agreement_5", "Ability to refuse sex", "High", "Relevant for mediation model.",
  "Planned Behaviour constructs", "EIS3", "Friends will not pressure to have sex", "Theory of Planned Behaviour / Socioecological", "Peer pressure control", "Mediator or peer-norm construct", "likert_agreement_5", "Peer pressure about sex", "High", "Can also map to peer norms.",

  # -----------------------------
  # Planned Behaviour: subjective norms
  # -----------------------------
  "Planned Behaviour constructs", "NPP1", "Most friends believe condoms or contraceptives should always be used", "Theory of Planned Behaviour", "Peer subjective norm", "Explanatory construct", "likert_agreement_5", "Friends' norms about condom/contraceptive use", "High", "Core TCP norm item.",
  "Planned Behaviour constructs", "NPP2", "Most friends believe condom should always be used even if girl uses pills", "Theory of Planned Behaviour", "Peer subjective norm", "Explanatory construct", "likert_agreement_5", "Friends' norms about condom use", "High", "Core TCP norm item.",
  "Planned Behaviour constructs", "NPP3", "Most friends use contraceptive methods when having sex", "Theory of Planned Behaviour", "Descriptive peer norm", "Explanatory construct", "likert_agreement_5", "Perceived friends' contraceptive use", "Medium", "Descriptive norm.",
  "Planned Behaviour constructs", "NP4", "Most friends have not yet had sexual relations", "Theory of Planned Behaviour", "Descriptive peer norm", "Explanatory construct", "likert_agreement_5", "Friends' sexual activity", "High", "Peer environment variable.",
  "Planned Behaviour constructs", "NP5", "Best friends think respondent should delay sexual initiation", "Theory of Planned Behaviour", "Injunctive peer norm", "Explanatory construct", "likert_agreement_5", "Friends' approval of delaying sex", "High", "Relevant for intention to delay.",
  "Planned Behaviour constructs", "NP6", "Friends think people of this age should wait until grown before sex", "Theory of Planned Behaviour", "Injunctive peer norm", "Explanatory construct", "likert_agreement_5", "Peer norms about delaying sex", "High", "Relevant for intention to delay.",

  # -----------------------------
  # Planned Behaviour: intentions
  # -----------------------------
  "Planned Behaviour constructs", "IPS1", "Intends to delay sexual initiation until grown or adulthood", "Theory of Planned Behaviour", "Intention to delay sex", "Dependent variable or mediator", "likert_agreement_5", "Intention to delay sex", "High", "Central TCP outcome.",
  "Planned Behaviour constructs", "IPS2", "Intends to abstain from sex until marriage or union", "Theory of Planned Behaviour", "Intention to abstain", "Dependent variable or mediator", "likert_agreement_5", "Intention to abstain", "Medium", "Cultural equivalence may differ.",
  "Planned Behaviour constructs", "IPC1", "If having sex next year, intends to always use condom", "Theory of Planned Behaviour", "Intention to use condom", "Dependent variable", "likert_agreement_5", "Intention to use condoms", "High", "Central TCP outcome.",
  "Planned Behaviour constructs", "IPC2", "If having sex next year, intends to always use effective contraception", "Theory of Planned Behaviour", "Intention to use contraception", "Dependent variable", "likert_agreement_5", "Intention to use contraception", "High", "Central TCP outcome.",

  # -----------------------------
  # Family connection
  # -----------------------------
  "Family", "LFA1", "Parents generally know where respondent is", "Socioecological model / Planned Behaviour mediation", "Family monitoring", "Contextual predictor", "likert_agreement_5", "Parental monitoring: parents know whereabouts", "High", "Important for family-connection construct.",
  "Family", "LFA2", "Parents, relatives or tutors know who respondent is with", "Socioecological model / Planned Behaviour mediation", "Family monitoring", "Contextual predictor", "likert_agreement_5", "Parental monitoring: parents know friends", "High", "Important for family-connection construct.",
  "Family", "LFA3", "Can count on parents, relatives or tutors when having problems", "Socioecological model / Planned Behaviour mediation", "Family support", "Contextual predictor", "likert_agreement_5", "Parent-family support or connectedness", "High", "Important for mediation and socioecological models.",

  # -----------------------------
  # Qualitative mixed questionnaire: school connection
  # -----------------------------
  "Mixed questionnaire: school connection", "Q01", "Support from adults and staff at school", "Socioecological model", "School adult support", "Qualitative construct", "open_text", "School connectedness and adult support", "High", "Can guide qualitative comparison or variable search.",
  "Mixed questionnaire: school connection", "Q02", "Positive peer group", "Socioecological model", "Positive peer environment", "Qualitative construct", "open_text", "Peer group and school climate", "Medium", "Qualitative construct.",
  "Mixed questionnaire: school connection", "Q03", "Commitment to education and teacher attention", "Socioecological model", "Educational commitment", "Qualitative construct", "open_text", "School engagement or school attachment", "High", "Important school connection construct.",
  "Mixed questionnaire: school connection", "Q04", "Positive or negative school climate", "Socioecological model", "School climate", "Qualitative construct", "open_text", "School climate and school belonging", "High", "Important school connection construct.",
  "Mixed questionnaire: school connection", "Q05", "Family and community involvement in school", "Socioecological model", "Family/community involvement", "Qualitative construct", "open_text", "Parent-school involvement or community support", "Medium", "May have limited Add Health equivalent.",
  "Mixed questionnaire: school connection", "Q06", "Sense of belonging or connection to school", "Socioecological model", "School connectedness", "Qualitative construct", "open_text", "School connectedness", "High", "Key socioecological construct.",
  "Mixed questionnaire: school connection", "Q07", "School and older people as sources of sexual health knowledge", "Socioecological model / Health Belief Model", "School-based sexual health knowledge", "Qualitative construct", "open_text", "Sex education at school or adult communication about sexual health", "High", "Bridge between school context and HIV/pregnancy prevention."
)


# ============================================================
# 6. Construct block summary
# ============================================================

construct_block_summary <- thesis_questionnaire_inventory %>%
  group_by(theoretical_model, construct, variable_role, response_scale, priority_for_mapping) %>%
  summarise(
    number_of_items = n(),
    item_codes = paste(thesis_item_code, collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(theoretical_model, construct, priority_for_mapping)


# ============================================================
# 7. Search-term dictionary for Add Health documentation
# ============================================================

add_health_search_terms <- tibble(
  search_block = c(
    "Sample alignment",
    "Sample alignment",
    "Demographics",
    "Socioeconomic status",
    "Sexual initiation",
    "Sexual behaviour",
    "Condom use",
    "Contraceptive use",
    "Pregnancy",
    "HIV and STI outcomes",
    "HIV knowledge",
    "Perceived susceptibility",
    "Perceived severity",
    "Perceived benefits",
    "Perceived barriers",
    "Condom self-efficacy",
    "Refusal self-efficacy",
    "Attitudes toward sex",
    "Attitudes toward condom or contraception",
    "Peer norms",
    "Family monitoring",
    "Family support",
    "School connectedness",
    "School climate",
    "Sex education"
  ),
  thesis_constructs = c(
    "Grade 10-12",
    "Age 15-19",
    "Gender, age, grade",
    "AFL1-AFL4",
    "SEX_EVER, ISX1",
    "ISX2, ISX4",
    "ISX3, APC, AEF, IPC",
    "ISX3, APC, EPC, IPC",
    "Pregnancy-related thesis outcome",
    "HIV/STI-related thesis outcome",
    "INA, CVS",
    "SUP",
    "SEP",
    "BEP",
    "BAP",
    "AEF, EPC",
    "AEF5, EIS",
    "AIS",
    "APC",
    "NPP, NP",
    "LFA1, LFA2",
    "LFA3",
    "Q01, Q03, Q04, Q06",
    "Q04",
    "INA4, Q07"
  ),
  english_keywords_for_codebook_search = c(
    "grade school grade current grade",
    "age date of birth age at interview",
    "sex gender age grade race ethnicity",
    "household assets parent education income computer car own room",
    "ever had sex sexual intercourse age at first sex",
    "number of partners sexual frequency sexual activity",
    "condom use used condom frequency condom",
    "contraception birth control contraceptive method",
    "pregnancy ever pregnant got pregnant pregnancy outcome",
    "HIV STI sexually transmitted infection test diagnosis",
    "HIV knowledge AIDS knowledge transmission prevention",
    "risk of HIV perceived risk worry AIDS",
    "seriousness AIDS severity HIV serious",
    "benefit condom prevents AIDS prevention belief",
    "barrier condom embarrassment access partner refuses pleasure",
    "condom self efficacy ability use condom correctly",
    "refuse sex say no pressure partner",
    "attitude sex abstinence delay sexual initiation",
    "attitude condom contraception birth control",
    "friends peers norms peer pressure friends had sex",
    "parents know whereabouts parents know friends parental monitoring",
    "parents support family support family connectedness",
    "school connectedness school belonging teachers care school attachment",
    "school climate safe school good school bad school",
    "sex education HIV education school taught AIDS"
  ),
  expected_wave_priority = c(
    "Wave I",
    "Wave I",
    "Wave I",
    "Wave I",
    "Wave I to Wave III",
    "Wave I to Wave III",
    "Wave I to Wave III",
    "Wave I to Wave III",
    "Wave I to Wave IV",
    "Wave III onward, but verify public-use availability",
    "Wave I to Wave III",
    "Wave I to Wave III",
    "Wave I to Wave III",
    "Wave I to Wave III",
    "Wave I to Wave III",
    "Wave I to Wave III",
    "Wave I to Wave III",
    "Wave I to Wave III",
    "Wave I to Wave III",
    "Wave I to Wave III",
    "Wave I",
    "Wave I",
    "Wave I to Wave II",
    "Wave I to Wave II",
    "Wave I to Wave III"
  ),
  search_priority = c(
    "High", "High", "High", "High", "High",
    "High", "High", "High", "High", "High",
    "High", "Medium", "Medium", "Medium", "High",
    "High", "High", "High", "High", "High",
    "High", "High", "High", "Medium", "High"
  )
)


# ============================================================
# 8. Add Health mapping template
# ============================================================
# This empty template will be filled after reviewing Add Health
# public-use codebooks and documentation.

add_health_mapping_template <- thesis_questionnaire_inventory %>%
  transmute(
    thesis_section,
    thesis_item_code,
    item_short_description,
    theoretical_model,
    construct,
    variable_role,
    response_scale,
    expected_add_health_equivalent,
    add_health_wave = NA_character_,
    add_health_file_or_codebook = NA_character_,
    add_health_variable_name = NA_character_,
    add_health_variable_label = NA_character_,
    public_use_status = "to_be_verified",
    mapping_quality = "to_be_verified",
    notes
  )


# ============================================================
# 9. Ethical and methodological notes
# ============================================================

script01b_methodological_notes <- tibble(
  note_id = 1:8,
  note = c(
    "This inventory is based on questionnaire constructs, not on individual responses.",
    "The original thesis database remains confidential and is not required for this script.",
    "The Add Health mapping must be done by construct equivalence, not literal item equivalence.",
    "Grade 10-12 remains the main comparability criterion.",
    "Age 15-19 remains the complementary comparability criterion.",
    "Sensitive outcomes such as sexual behaviour, pregnancy, HIV and STI indicators must be reported only in aggregate form.",
    "Small-cell outputs must be reviewed before GitHub publication.",
    "Variables involving friends, partners, school identifiers or networks may be restricted in Add Health public-use files."
  )
)


# ============================================================
# 10. Execution checklist
# ============================================================

script01b_checklist <- tibble(
  check_id = 1:11,
  check_item = c(
    "Project root exists",
    "Folder structure verified",
    "Questionnaire metadata created",
    "Response scale dictionary created",
    "Thesis questionnaire construct inventory created",
    "Construct block summary created",
    "Add Health search-term dictionary created",
    "Add Health mapping template created",
    "Methodological notes created",
    "CSV outputs exported",
    "Excel workbook exported"
  ),
  status = c(
    ifelse(dir.exists(project_root), "OK", "FAIL"),
    ifelse(all(dir.exists(file.path(project_root, required_folders))), "OK", "FAIL"),
    "OK",
    "OK",
    "OK",
    "OK",
    "OK",
    "OK",
    "OK",
    "PENDING",
    "PENDING"
  )
)


# ============================================================
# 11. Export CSV outputs
# ============================================================

write_csv(
  questionnaire_metadata,
  file.path(project_root, "outputs/tables/questionnaire_metadata_script01b.csv")
)

write_csv(
  response_scale_dictionary,
  file.path(project_root, "outputs/tables/response_scale_dictionary_script01b.csv")
)

write_csv(
  thesis_questionnaire_inventory,
  file.path(project_root, "outputs/tables/thesis_questionnaire_construct_inventory_script01b.csv")
)

write_csv(
  construct_block_summary,
  file.path(project_root, "outputs/tables/construct_block_summary_script01b.csv")
)

write_csv(
  add_health_search_terms,
  file.path(project_root, "outputs/tables/add_health_search_terms_script01b.csv")
)

write_csv(
  add_health_mapping_template,
  file.path(project_root, "outputs/tables/add_health_mapping_template_script01b.csv")
)

write_csv(
  script01b_methodological_notes,
  file.path(project_root, "outputs/tables/script01b_methodological_notes.csv")
)

script01b_checklist$status[
  script01b_checklist$check_item == "CSV outputs exported"
] <- "OK"


# ============================================================
# 12. Export Excel workbook
# ============================================================

xlsx_path <- file.path(
  project_root,
  "outputs/tables/script01b_thesis_questionnaire_construct_inventory.xlsx"
)

wb <- createWorkbook()

addWorksheet(wb, "questionnaire_metadata")
writeData(wb, "questionnaire_metadata", questionnaire_metadata)

addWorksheet(wb, "response_scales")
writeData(wb, "response_scales", response_scale_dictionary)

addWorksheet(wb, "construct_inventory")
writeData(wb, "construct_inventory", thesis_questionnaire_inventory)

addWorksheet(wb, "construct_summary")
writeData(wb, "construct_summary", construct_block_summary)

addWorksheet(wb, "add_health_search_terms")
writeData(wb, "add_health_search_terms", add_health_search_terms)

addWorksheet(wb, "mapping_template")
writeData(wb, "mapping_template", add_health_mapping_template)

addWorksheet(wb, "methodological_notes")
writeData(wb, "methodological_notes", script01b_methodological_notes)

addWorksheet(wb, "script01b_checklist")
writeData(wb, "script01b_checklist", script01b_checklist)

for (sheet in names(wb)) {
  setColWidths(wb, sheet = sheet, cols = 1:30, widths = "auto")
  freezePane(wb, sheet = sheet, firstRow = TRUE)
}

saveWorkbook(wb, xlsx_path, overwrite = TRUE)

script01b_checklist$status[
  script01b_checklist$check_item == "Excel workbook exported"
] <- "OK"


# ============================================================
# 13. Export Markdown documentation
# ============================================================

construct_note <- c(
  "# Thesis Questionnaire Construct Inventory",
  "",
  "This document summarises the construct structure extracted from the doctoral thesis questionnaire.",
  "",
  "The purpose is to guide the mapping between the thesis architecture and Add Health public-use documentation.",
  "",
  "The inventory does not include individual-level thesis data.",
  "",
  "## Main construct blocks",
  "",
  "- Demographic and socioeconomic characteristics;",
  "- Household asset indicators;",
  "- Sexual initiation and sexual behaviour;",
  "- Health Belief Model constructs;",
  "- Theory of Planned Behaviour constructs;",
  "- HIV/AIDS knowledge;",
  "- Family monitoring and support;",
  "- School connectedness and qualitative school-context constructs.",
  "",
  "## Main sample alignment",
  "",
  "The thesis focused on students from 10th to 12th grade, approximately aged 15 to 19.",
  "",
  "In the Add Health project, grades 10 to 12 at Wave I remain the main sample criterion.",
  "",
  "Age 15 to 19 is used as a complementary alignment or sensitivity criterion.",
  "",
  "## Mapping principle",
  "",
  "The Add Health mapping must be based on construct equivalence, not exact wording equivalence."
)

writeLines(
  construct_note,
  con = file.path(project_root, "docs/thesis_questionnaire_construct_inventory.md")
)

search_terms_note <- c(
  "# Add Health Variable Search Keywords",
  "",
  "This document lists the main keyword blocks that will guide the search in Add Health public-use documentation.",
  "",
  "## Priority search blocks",
  "",
  "- grade and age;",
  "- gender, race or ethnicity and socioeconomic status;",
  "- sexual initiation;",
  "- condom use;",
  "- contraceptive use;",
  "- pregnancy;",
  "- HIV/STI indicators;",
  "- HIV/AIDS knowledge;",
  "- perceived risk and perceived barriers;",
  "- condom self-efficacy and refusal self-efficacy;",
  "- attitudes and intentions;",
  "- peer norms;",
  "- family monitoring and family support;",
  "- school connectedness and school climate;",
  "- sex education.",
  "",
  "These search terms will be used in Script 02 to review Add Health documentation and identify candidate variables."
)

writeLines(
  search_terms_note,
  con = file.path(project_root, "docs/add_health_variable_search_keywords.md")
)


# ============================================================
# 14. Save final checklist
# ============================================================

write_csv(
  script01b_checklist,
  file.path(project_root, "outputs/diagnostics/script01b_execution_checklist.csv")
)


# ============================================================
# 15. Console summary
# ============================================================

cat("\n============================================================\n")
cat("Script 01b completed: Thesis Questionnaire Construct Inventory\n")
cat("============================================================\n\n")

cat("Project root:\n")
cat(project_root, "\n\n")

cat("Main outputs created:\n")
cat("- outputs/tables/questionnaire_metadata_script01b.csv\n")
cat("- outputs/tables/response_scale_dictionary_script01b.csv\n")
cat("- outputs/tables/thesis_questionnaire_construct_inventory_script01b.csv\n")
cat("- outputs/tables/construct_block_summary_script01b.csv\n")
cat("- outputs/tables/add_health_search_terms_script01b.csv\n")
cat("- outputs/tables/add_health_mapping_template_script01b.csv\n")
cat("- outputs/tables/script01b_thesis_questionnaire_construct_inventory.xlsx\n")
cat("- outputs/diagnostics/script01b_execution_checklist.csv\n")
cat("- docs/thesis_questionnaire_construct_inventory.md\n")
cat("- docs/add_health_variable_search_keywords.md\n\n")

cat("Construct inventory summary:\n")
cat("- Number of questionnaire items inventoried: ", nrow(thesis_questionnaire_inventory), "\n", sep = "")
cat("- Number of construct summary rows: ", nrow(construct_block_summary), "\n", sep = "")
cat("- Number of Add Health search blocks: ", nrow(add_health_search_terms), "\n\n", sep = "")

cat("Important note:\n")
cat("No individual-level thesis data or Add Health microdata were imported or exported.\n\n")

cat("Execution checklist:\n")
print(script01b_checklist)