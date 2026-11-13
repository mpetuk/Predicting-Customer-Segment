# =============================================================================
# Population Insights - Shiny prototype
# -----------------------------------------------------------------------------
# Combines the Demographic, Condition/Utilization/SDoH and Top Diagnoses tabs
# into one tab driven by a single set of population filters.
#
# ALL DATA IN THIS FILE IS FICTIONAL SAMPLE DATA (account IL009999 does not
# exist). Replace the "Sample data" section with the real queries.
#
# Packages: shiny, bslib. shinyWidgets is optional - when installed, the
# filters use searchable checkbox dropdowns (pickerInput); otherwise they fall
# back to Shiny's built-in multi-select.
# =============================================================================

library(shiny)
library(bslib)

HAS_WIDGETS <- requireNamespace("shinyWidgets", quietly = TRUE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

ACCENT <- "#5B5FC7"
TOTAL  <- 6240   # members in the sample account
MIN_N  <- 30     # below this many eligible, a HEDIS rate is not shown

# =============================================================================
# Sample data (fictional)
# =============================================================================

CO <- data.frame(
  name    = c("Cook", "DuPage", "Will", "Lake", "Kane"),
  members = c(2480, 1310, 980, 760, 710),
  cf      = c(1.12, 0.82, 1.00, 0.90, 1.05)   # county effect on rates
)

IL_COUNTIES <- c(
  "Adams", "Alexander", "Bond", "Boone", "Brown", "Bureau", "Calhoun", "Carroll",
  "Cass", "Champaign", "Christian", "Clark", "Clay", "Clinton", "Coles", "Cook",
  "Crawford", "Cumberland", "DeKalb", "De Witt", "Douglas", "DuPage", "Edgar",
  "Edwards", "Effingham", "Fayette", "Ford", "Franklin", "Fulton", "Gallatin",
  "Greene", "Grundy", "Hamilton", "Hancock", "Hardin", "Henderson", "Henry",
  "Iroquois", "Jackson", "Jasper", "Jefferson", "Jersey", "Jo Daviess", "Johnson",
  "Kane", "Kankakee", "Kendall", "Knox", "Lake", "LaSalle", "Lawrence", "Lee",
  "Livingston", "Logan", "McDonough", "McHenry", "McLean", "Macon", "Macoupin",
  "Madison", "Marion", "Marshall", "Mason", "Massac", "Menard", "Mercer", "Monroe",
  "Montgomery", "Morgan", "Moultrie", "Ogle", "Peoria", "Perry", "Piatt", "Pike",
  "Pope", "Pulaski", "Putnam", "Randolph", "Richland", "Rock Island", "St. Clair",
  "Saline", "Sangamon", "Schuyler", "Scott", "Shelby", "Stark", "Stephenson",
  "Tazewell", "Union", "Vermilion", "Wabash", "Warren", "Washington", "Wayne",
  "White", "Whiteside", "Will", "Williamson", "Winnebago", "Woodford"
)

AGE <- data.frame(
  name    = c("0-17", "18-25", "26-34", "35-44", "45-54", "55-64", "65+"),
  members = c(1210, 640, 980, 1150, 1090, 970, 200),
  f       = c(0.90, 1.18, 1.12, 1.00, 0.94, 0.88, 0.80)
)
GEN <- data.frame(name = c("Female", "Male"), members = c(3270, 2970), f = c(0.95, 1.06))
RACE <- data.frame(
  name = c("White", "Hispanic or Latino", "Black", "Asian", "Other / unknown"),
  pct  = c(48, 22, 14, 11, 5),
  f    = c(0.90, 1.18, 1.22, 0.84, 1.00)
)

EDU_NAMES <- c("Less than high school", "High school diploma", "Some college",
               "Bachelor's degree", "Graduate degree")
EDU <- rbind(            # % by county (rows follow CO)
  c(8, 26, 25, 25, 16),
  c(3, 18, 24, 34, 21),
  c(5, 26, 30, 27, 12),
  c(5, 21, 26, 30, 18),
  c(9, 27, 29, 24, 11)
)
VETERANS  <- c(4.9, 5.6, 7.1, 6.4, 5.8)   # % of households
BLUE_DOOR <- c(92, 74, 48, 35, 22)        # % of members within 10 miles

PREV <- data.frame(
  group = rep(c("Physical Health", "Behavioral Health", "Substance Use"), c(7, 7, 3)),
  name = c("Asthma", "Colorectal Cancer", "Diabetes", "Hypertension",
           "Musculoskeletal Condition", "Obesity", "CCI >= 2 (Charlson Comorbidity Index)",
           "ADHD", "Anxiety Disorders", "Autism Spectrum Disorders (ASD)", "Bipolar Disorders",
           "Depressive Disorders", "Other Mental Health Disorders", "Psychotic Disorders",
           "Alcohol Use Disorder (AUD)", "Opioid Use Disorder", "Other Substance Use Disorders"),
  base = c(8.2, 0.4, 9.6, 21.3, 18.7, 14.9, 7.4,
           5.1, 16.8, 1.3, 2.2, 13.9, 4.0, 0.8,
           2.9, 0.9, 2.4)
)

OE_GROUP <- "Observed vs Expected (O/E)"
hedis_rows <- list(
  c("Cancer Screening (ECDS)", "Breast Cancer Screening", 31, 820),
  c("Cancer Screening (ECDS)", "Cervical Cancer Screening", 36, 1410),
  c("Cancer Screening (ECDS)", "Colorectal Cancer Screening", 44, 1620),
  c("Cancer Screening (ECDS)", "Colorectal Cancer Screening (Ages 51-75)", 41, 1180),
  c("Maternal Health (MIH)", "Child and Adolescent Well Care Visits", 38, 1150),
  c("Maternal Health (MIH)", "Postpartum", 22, 64),
  c("Maternal Health (MIH)", "Timeliness of Prenatal Care", 15, 64),
  c("Maternal Health (MIH)", "Well Visits - First 15 Months", 27, 58),
  c("Diabetes Care (CDC)", "Antipsychotic Medication", 30, 12),
  c("Diabetes Care (CDC)", "Eye Exam", 52, 410),
  c("Diabetes Care (CDC)", "Glycemic Status (<8.0)", 41, 410),
  c("Diabetes Care (CDC)", "Glycemic Status (>9.0)", 24, 410),
  c("Diabetes Care (CDC)", "Kidney Evaluation", 58, 410),
  c("Diabetes Care (CDC)", "Medication Adherence for Diabetes Medications", 21, 290),
  c("Diabetes Care (CDC)", "Statin Therapy", 34, 260),
  c("Diabetes Care (CDC)", "Statin Use", 37, 300),
  c("Diabetes Care (CDC)", "Statin Therapy Adherence", 25, 180),
  c("Behavioral Health (BH)", "Follow-Up After Emergency Visit - 30 Day", 44, 48),
  c("Behavioral Health (BH)", "Follow-Up After Emergency Visit - 7 Day", 62, 48),
  c("Behavioral Health (BH)", "Follow-Up After Hospitalization MI - 30 Day", 39, 31),
  c("Behavioral Health (BH)", "Follow-Up After Hospitalization MI - 7 Day", 57, 31),
  c("Behavioral Health (BH)", "Follow-Up Care for Children Prescribed ADHD Medication", 49, 74),
  c("Behavioral Health (BH)", "Pharmacotherapy for Opioid Use Disorder", 53, 22),
  c("Behavioral Health (BH)", "Concurrent Use of Opioids and Benzodiazepines", 9, 95),
  c("Behavioral Health (BH)", "Depression Screening", 71, 3900),
  c("Behavioral Health (BH)", "Depression Follow-Up", 46, 220),
  c("Cardiovascular", "Controlling Blood Pressure", 37, 960),
  c("Cardiovascular", "Medication Adherence for Cholesterol", 23, 640),
  c("Cardiovascular", "Medication Adherence for Hypertension", 19, 820),
  c("Cardiovascular", "Statin Therapy", 32, 210),
  c("Cardiovascular", "Statin Therapy Adherence", 26, 150),
  c("Immunizations (IMM)", "Adult Immunization Status (Influenza)", 58, 4700),
  c("Immunizations (IMM)", "Adult Immunization Status (Influenza) (Ages 19-64)", 61, 4450),
  c("Immunizations (IMM)", "Adult Immunization Status (Pneumococcal)", 49, 310),
  c("Immunizations (IMM)", "Adult Immunization Status (Tdap)", 54, 4700),
  c("Immunizations (IMM)", "Adult Immunization Status (Zoster)", 72, 1420),
  c("Immunizations (IMM)", "Childhood Immunization Combo 10", 63, 72),
  c("Immunizations (IMM)", "Childhood Immunization Combo 3", 31, 72),
  c("Immunizations (IMM)", "Immunization for Adolescents", 35, 160),
  c("Acute Illness & Transitions (AITC)", "Follow-Up After ED With Multiple High-Risk Chronic Conditions", 41, 38),
  c("Acute Illness & Transitions (AITC)", "Transitions of Care", 52, 25),
  c("Care for Older Adults (COA)", "Medication Review", 34, 140),
  c("Care for Older Adults (COA)", "Functional Status Assessment", 46, 140),
  c("Care for Older Adults (COA)", "Osteoporosis Management in Women with Fracture", 60, 9),
  c("Care for Older Adults (COA)", "Polypharmacy", 12, 140),
  c(OE_GROUP, "Acute Hospital Utilization", 1.08, NA),
  c(OE_GROUP, "All Cause Readmissions", 0.91, NA),
  c(OE_GROUP, "Emergency Department Utilization", 1.17, NA)
)
HEDIS <- data.frame(
  group    = vapply(hedis_rows, `[`, "", 1),
  name     = vapply(hedis_rows, `[`, "", 2),
  rate     = as.numeric(vapply(hedis_rows, `[`, "", 3)),  # O/E rows: the ratio
  eligible = suppressWarnings(as.numeric(vapply(hedis_rows, `[`, "", 4)))
)

# Single-value cards. values = one per county (order of CO).
# adjust = TRUE: age/gender/race filters change the value; FALSE: county-level.
card_item <- function(cat, group, name, unit, digits, adjust, values) {
  list(cat = cat, group = group, name = name, unit = unit, digits = digits,
       adjust = adjust, values = values,
       id = paste0("card_", gsub("[^A-Za-z0-9]+", "_", paste(cat, name))))
}
CARDS <- list(
  card_item("util", "Utilization", "ER visits per 1,000 members", "", 0, TRUE, c(341, 254, 318, 289, 333)),
  card_item("util", "Utilization", "Telehealth visits per 1,000 members", "", 0, TRUE, c(534, 512, 421, 466, 398)),
  card_item("util", "Average EZ Risk Scores", "Average EZ Medical Risk Score", "", 2, TRUE, c(1.12, 0.93, 1.01, 0.97, 1.08)),
  card_item("util", "Average EZ Risk Scores", "Average EZ Medical & Pharmacy Risk Score", "", 2, TRUE, c(1.17, 0.98, 1.06, 1.02, 1.13)),
  card_item("sdoh", "Overlays", "Crime rate (per 1,000)", "", 1, FALSE, c(32.1, 14.2, 17.8, 16.5, 19.3)),
  card_item("sdoh", "Overlays", "Food desert - rural, 10 mi", "%", 1, FALSE, c(0.4, 0.1, 3.8, 1.9, 4.6)),
  card_item("sdoh", "Overlays", "Food desert - urban, 0.5 mi", "%", 1, FALSE, c(18.2, 9.7, 11.3, 10.4, 12.8)),
  card_item("sdoh", "Overlays", "Households without internet", "%", 1, FALSE, c(11.4, 5.2, 6.9, 6.1, 7.7)),
  card_item("sdoh", "Overlays", "Life expectancy (years)", "", 1, FALSE, c(76.9, 82.1, 79.8, 80.6, 80.2)),
  card_item("sdoh", "Overlays", "Maternity care desert", "%", 1, FALSE, c(0, 0, 2.1, 0, 3.4)),
  card_item("sdoh", "Overlays", "Pharmacy desert", "%", 1, FALSE, c(6.2, 2.9, 8.4, 5.1, 9.0)),
  card_item("sdoh", "Overlays", "HCSC primary care desert", "%", 1, FALSE, c(4.1, 2.2, 9.5, 6.3, 11.2)),
  card_item("sdoh", "Overlays", "Primary care desert", "%", 1, FALSE, c(3.5, 1.8, 7.9, 5.4, 10.1)),
  card_item("sdoh", "Overlays", "Public transit options (score 0-10)", "", 1, FALSE, c(8.6, 5.2, 3.1, 3.8, 3.4)),
  card_item("sdoh", "Social Vulnerability Index", "SVI - Overall", "", 2, FALSE, c(0.68, 0.21, 0.39, 0.33, 0.47)),
  card_item("sdoh", "Social Vulnerability Index", "SVI - Socioeconomic", "", 2, FALSE, c(0.66, 0.18, 0.35, 0.29, 0.49)),
  card_item("sdoh", "Social Vulnerability Index", "SVI - Household", "", 2, FALSE, c(0.52, 0.24, 0.43, 0.37, 0.45)),
  card_item("sdoh", "Social Vulnerability Index", "SVI - Racial/Ethnic", "", 2, FALSE, c(0.81, 0.46, 0.52, 0.55, 0.61)),
  card_item("sdoh", "Social Vulnerability Index", "SVI - Housing/Transport", "", 2, FALSE, c(0.71, 0.29, 0.36, 0.34, 0.42))
)

dx <- function(type, level, rows) {
  data.frame(type = type, level = level,
             dx = vapply(rows, `[[`, "", 1),
             claims = vapply(rows, function(r) as.numeric(r[[2]]), 0),
             members = vapply(rows, function(r) as.numeric(r[[3]]), 0))
}
DX <- rbind(
  dx("non", "Inpatient Acute", list(list("Depressive disorders", 46, 18), list("Substance-related disorders", 31, 12), list("Anxiety disorders", 12, 6))),
  dx("non", "Intensive Outpatient", list(list("Depressive disorders", 210, 34), list("Anxiety disorders", 168, 29), list("Substance-related disorders", 122, 17))),
  dx("non", "Partial Hospitalization", list(list("Anxiety disorders", 74, 9), list("Depressive disorders", 61, 8), list("Substance-related disorders", 22, 3))),
  dx("non", "Outpatient", list(list("Anxiety disorders", 1420, 388), list("Depressive disorders", 1180, 305), list("ADHD", 640, 172), list("Trauma- and stressor-related", 410, 118))),
  dx("non", "Professional", list(list("Anxiety disorders", 2310, 602), list("Depressive disorders", 1890, 497), list("ADHD", 980, 266), list("Adjustment disorders", 520, 171))),
  dx("non", "Residential Treatment Center", list(list("Substance-related disorders", 18, 4), list("Depressive disorders", 9, 2))),
  dx("non", "Other BH", list(list("Anxiety disorders", 96, 41), list("Depressive disorders", 72, 33))),
  dx("spmi", "Inpatient Acute", list(list("Bipolar disorders", 38, 11), list("Schizophrenia and psychotic", 29, 7), list("Depressive disorders", 22, 9))),
  dx("spmi", "Intensive Outpatient", list(list("Bipolar disorders", 64, 12), list("Schizophrenia and psychotic", 41, 8))),
  dx("spmi", "Partial Hospitalization", list(list("Bipolar disorders", 27, 5), list("Schizophrenia and psychotic", 19, 4))),
  dx("spmi", "Outpatient", list(list("Bipolar disorders", 380, 74), list("Schizophrenia and psychotic", 210, 39), list("Depressive disorders", 190, 52))),
  dx("spmi", "Professional", list(list("Bipolar disorders", 610, 88), list("Schizophrenia and psychotic", 344, 51), list("Depressive disorders", 290, 70))),
  dx("spmi", "Residential Treatment Center", list(list("Schizophrenia and psychotic", 12, 2))),
  dx("spmi", "Other BH", list(list("Bipolar disorders", 31, 10))),
  dx("med", "Inpatient", list(list("Liveborn", 88, 88), list("Osteoarthritis", 41, 37), list("Septicemia", 29, 24))),
  dx("med", "Emergency Room", list(list("Superficial injury", 220, 190), list("Abdominal pain", 180, 152), list("Upper respiratory infection", 160, 141))),
  dx("med", "Outpatient", list(list("Back problems", 640, 280), list("Essential hypertension", 590, 410), list("Diabetes", 510, 290))),
  dx("med", "Professional", list(list("Medical examination", 3900, 2800), list("Essential hypertension", 1700, 980), list("Back problems", 1500, 620)))
)
BH_LEVELS  <- unique(DX$level[DX$type == "non"])
MED_LEVELS <- unique(DX$level[DX$type == "med"])

# =============================================================================
# Helpers
# =============================================================================

fmt <- function(x) format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
pct <- function(x, d = 1) sprintf(paste0("%.", d, "f%%"), x)
safe_id <- function(...) gsub("[^A-Za-z0-9]+", "_", paste(..., sep = "_"))
wavg <- function(f, w, sel) sum(f[sel] * w[sel]) / sum(w[sel])
by_county <- function(values, idx) sum(values[idx] * CO$members[idx]) / sum(CO$members[idx])

scope_label <- function(idx) {
  if (length(idx) == nrow(CO)) "All counties"
  else if (length(idx) == 1) paste(CO$name[idx], "County")
  else paste(CO$name[idx], collapse = " + ")
}

snap_button <- function(title) {
  # Hook for the Report Builder: wire this to the existing capture pipeline.
  tags$button(type = "button", class = "pi-snap",
              `aria-label` = paste("Add", title, "to report"), icon("camera"))
}

# A card whose body (and optional per-chart County picker) render on the server.
card_ui <- function(id, title, sub = NULL) {
  div(class = "pi-card",
    div(class = "pi-card-head",
      h3(title),
      div(class = "pi-card-tools", uiOutput(paste0(id, "_pv"), inline = TRUE), snap_button(title))
    ),
    if (!is.null(sub)) p(class = "pi-sub", sub),
    uiOutput(paste0(id, "_body"))
  )
}

bar <- function(width, stripe = FALSE, color = ACCENT) {
  inner <- if (stripe) "pi-bar pi-stripe" else "pi-bar"
  span(class = "pi-track",
       span(class = inner, style = if (stripe) NULL else
         sprintf("width:%.1f%%;background:%s", max(2, min(100, width)), color)))
}

bar_rows <- function(labels, values, texts) {
  m <- max(values, 0)
  div(class = "pi-rows", lapply(seq_along(labels), function(i)
    div(class = "pi-row3",
        span(class = "pi-lab", labels[i]),
        bar(if (m > 0) values[i] / m * 100 else 0),
        span(class = "pi-val", texts[i]))))
}

measure_list <- function(df) {
  # df: group, label, note, value, width, stripe
  tagList(lapply(unique(df$group), function(g) {
    d <- df[df$group == g, ]
    tagList(
      h4(class = "pi-group", g),
      lapply(seq_len(nrow(d)), function(i)
        div(class = "pi-mrow",
            div(div(class = "pi-mname", d$label[i]), div(class = "pi-note", d$note[i])),
            bar(d$width[i], d$stripe[i]),
            span(class = "pi-val pi-strong", d$value[i])))
    )
  }))
}

donut_svg <- function(f_n, m_n) {
  tot <- f_n + m_n
  share <- if (tot > 0) f_n / tot else 0
  d <- 439.8 * share
  HTML(sprintf(paste0(
    '<svg role="img" aria-label="Donut chart: Female %s, Male %s" width="190" height="190" viewBox="0 0 200 200">',
    '<circle cx="100" cy="100" r="70" fill="none" stroke="#F1F2F5" stroke-width="30"/>',
    '<g transform="rotate(-90 100 100)">',
    '<circle cx="100" cy="100" r="70" fill="none" stroke="#2F6DB5" stroke-width="30" stroke-dasharray="%.1f 439.8"/>',
    '<circle cx="100" cy="100" r="70" fill="none" stroke="#E8833A" stroke-width="30" stroke-dasharray="%.1f 439.8" stroke-dashoffset="%.1f"/>',
    '</g><text x="100" y="96" text-anchor="middle" font-size="22" font-weight="700" fill="#111827">%s</text>',
    '<text x="100" y="116" text-anchor="middle" font-size="12" fill="#5F6670">members</text></svg>'),
    fmt(f_n), fmt(m_n), d, 439.8 - d, -d, fmt(tot)))
}

# Multi-select filter: searchable checkbox dropdown when shinyWidgets exists.
multi_filter <- function(id, choices, all_text, noun, search = FALSE, disabled = NULL, subtext = NULL) {
  if (HAS_WIDGETS) {
    opts <- list(disabled = disabled, subtext = subtext)
    shinyWidgets::pickerInput(
      id, label = NULL, choices = choices, multiple = TRUE, width = "100%",
      options = shinyWidgets::pickerOptions(
        actionsBox = TRUE, liveSearch = search, noneSelectedText = all_text,
        selectedTextFormat = "count > 2", countSelectedText = paste("{0}", noun, "selected"),
        deselectAllText = "Clear", selectAllText = "Select all", size = 10
      ),
      choicesOpt = Filter(Negate(is.null), opts)
    )
  } else {
    if (!is.null(disabled)) {   # built-in select can't disable options: drop them
      flat <- unlist(choices)
      keep <- flat[!disabled]
      choices <- choices[vapply(choices, function(g) any(unlist(g) %in% keep), TRUE)]
      choices <- lapply(choices, function(g) g[unlist(g) %in% keep])
    }
    selectizeInput(id, label = NULL, choices = choices, multiple = TRUE, width = "100%",
                   options = list(placeholder = all_text, plugins = list("remove_button")))
  }
}

filter_block <- function(num, field, input_el) {
  div(class = "pi-filter",
    span(class = "pi-fnum", num),
    div(class = "pi-fhead", span(class = "pi-field", field), span(class = "pi-op", "Includes")),
    input_el)
}

and_sep <- function() div(class = "pi-and", span(), "AND", span())

chip_buttons <- function(id, choices) {
  radioButtons(id, label = NULL, choices = choices, selected = choices[1], inline = TRUE)
}

# =============================================================================
# UI
# =============================================================================

county_choices <- {
  in_acct <- sort(CO$name)
  other <- setdiff(IL_COUNTIES, CO$name)
  list(
    "In this account" = setNames(as.character(match(in_acct, CO$name)), in_acct),
    "Other Illinois counties" = setNames(paste0("x_", other), other)
  )
}
county_disabled <- c(rep(FALSE, nrow(CO)), rep(TRUE, length(IL_COUNTIES) - nrow(CO)))
county_subtext  <- c(paste(fmt(CO$members[match(sort(CO$name), CO$name)]), "members"),
                     rep("no members", length(IL_COUNTIES) - nrow(CO)))

css <- sprintf("
body { background:#F4F5F7; color:#1F2937; }
.pi-header { background:#fff; border-bottom:1px solid #E2E5EA; padding:12px 28px; display:flex; flex-wrap:wrap; gap:8px 28px; align-items:center; }
.pi-brand b { font-size:18px; display:block; } .pi-brand small { color:#5F6670; }
.pi-nav a { color:#4B5563; text-decoration:none; margin-right:18px; font-size:14px; }
.pi-nav a.active { color:#111827; font-weight:600; border-bottom:2px solid %1$s; padding-bottom:6px; }
.pi-banner { background:#FFF7E6; border:1px solid #F2D49B; color:#7A4B00; border-radius:8px; padding:9px 14px; font-size:13px; margin-bottom:16px; }
.pi-title h1 { font-size:26px; font-weight:600; margin:0; } .pi-title p { color:#5F6670; margin:4px 0 16px; }
.pi-count { background:#F4F5FB; border-radius:8px; padding:10px 12px; margin-bottom:6px; }
.pi-count b { font-size:20px; display:block; } .pi-count span { font-size:12px; color:#5F6670; }
.pi-filter { border:1px solid #E5E7EB; border-radius:8px; padding:10px; background:#fff; }
.pi-fnum { font-size:12px; color:#5F6670; }
.pi-fhead { display:grid; grid-template-columns:1fr 96px; gap:6px; margin:6px 0; }
.pi-field, .pi-op { border:1px solid #D1D5DB; border-radius:6px; height:34px; display:flex; align-items:center; padding:0 10px; font-size:13px; }
.pi-field { background:#F9FAFB; } .pi-op { color:#374151; }
.pi-filter .form-group, .pi-filter .shiny-input-container { margin-bottom:0; }
.pi-and { display:flex; align-items:center; gap:8px; font-size:11px; font-weight:600; color:#5F6670; letter-spacing:.06em; margin:8px 0; }
.pi-and span { flex:1; height:1px; background:#E5E7EB; }
.pi-kpis { display:grid; grid-template-columns:repeat(auto-fit,minmax(170px,1fr)); gap:14px; margin-bottom:16px; }
.pi-kpi { background:#fff; border:1px solid #E2E5EA; border-radius:10px; padding:14px 16px; }
.pi-kpi small { color:#5F6670; font-size:12px; display:block; } .pi-kpi b { font-size:26px; display:block; }
.pi-card { background:#fff; border:1px solid #E2E5EA; border-radius:10px; padding:16px 18px; min-width:0; }
.pi-card-head { display:flex; justify-content:space-between; align-items:center; gap:8px; }
.pi-card-head h3 { font-size:15px; font-weight:600; margin:0; }
.pi-card-tools { display:flex; align-items:center; gap:4px; }
.pi-card-tools .form-group { margin:0; display:flex; align-items:center; gap:6px; }
.pi-card-tools label { font-size:12px; color:#5F6670; margin:0; }
.pi-card-tools select { height:32px; font-size:12px; padding:0 6px; border:1px solid #D1D5DB; border-radius:6px; }
.pi-snap { width:40px; height:40px; border:none; background:none; color:#5F6670; }
.pi-snap:hover { color:%1$s; }
.pi-sub, .pi-note, .pi-scope { font-size:12px; color:#5F6670; margin:0 0 10px; }
.pi-note { margin:0; }
.pi-grid { display:grid; grid-template-columns:repeat(auto-fit,minmax(min(100%%,420px),1fr)); gap:16px; }
.pi-grid-sm { display:grid; grid-template-columns:repeat(auto-fit,minmax(min(100%%,260px),1fr)); gap:14px; }
.pi-rows { display:flex; flex-direction:column; gap:9px; }
.pi-row3 { display:grid; grid-template-columns:150px minmax(0,1fr) 110px; gap:10px; align-items:center; font-size:13px; }
.pi-mrow { display:grid; grid-template-columns:minmax(0,1.3fr) minmax(0,1fr) 56px; gap:14px; align-items:center; padding:7px 0; border-bottom:1px solid #F1F2F5; font-size:13px; }
.pi-track { display:block; height:14px; border-radius:3px; background:#F1F2F5; }
.pi-bar { display:block; height:14px; border-radius:3px; }
.pi-stripe { width:100%%; background:repeating-linear-gradient(45deg,#E5E7EB 0 4px,#F9FAFB 4px 8px); }
.pi-val { text-align:right; font-size:12px; } .pi-strong { font-weight:600; font-size:13px; }
.pi-group { font-size:12px; font-weight:600; color:#5F6670; text-transform:uppercase; letter-spacing:.04em; margin:16px 0 6px; }
.pi-big { font-size:28px; font-weight:700; }
.pi-donut { display:flex; flex-wrap:wrap; align-items:center; justify-content:center; gap:28px; }
.pi-legend span { display:inline-block; width:12px; height:12px; border-radius:2px; margin-right:6px; vertical-align:-1px; }
.pi-dx-row { display:grid; grid-template-columns:150px minmax(0,1fr); gap:10px; align-items:center; font-size:13px; margin-bottom:12px; }
.pi-dx-bar { display:flex; align-items:center; gap:8px; margin:3px 0; font-size:12px; font-weight:600; }
.pi-dx-bar span.b { height:14px; border-radius:3px; min-width:3px; }
/* radio buttons styled as chips */
.pi-chips .shiny-options-group { display:flex; flex-wrap:wrap; gap:8px; }
.pi-chips .radio-inline, .pi-chips .form-check-inline { margin:0; padding:0; }
.pi-chips input { position:absolute; opacity:0; }
.pi-chips label span { display:inline-block; padding:8px 14px; border:1px solid #D1D5DB; border-radius:20px; background:#fff; font-size:13px; cursor:pointer; }
.pi-chips input:checked + span { background:%1$s; color:#fff; border-color:%1$s; }
.pi-chips input:focus-visible + span { outline:2px solid %1$s; outline-offset:2px; }
.nav-pills .nav-link.active { background:%1$s; }
", ACCENT)

ui <- page_fluid(
  theme = bs_theme(version = 5, primary = ACCENT,
                   base_font = "Inter, system-ui, -apple-system, 'Segoe UI', sans-serif"),
  tags$head(tags$style(HTML(css))),
  div(class = "pi-header",
    div(class = "pi-brand", tags$b("PopART"), tags$small("Population Analytics and Reporting Tool")),
    tags$nav(class = "pi-nav", `aria-label` = "Main",
      a(href = "#", "Landing Page"), a(href = "#", "Interactive Map"), a(href = "#", "Zip Explorer"),
      a(href = "#", "Member Explorer"), a(href = "#", class = "active", `aria-current` = "page", "Population Insights"),
      a(href = "#", "Report Builder"), a(href = "#", "Help & Support"))
  ),
  div(style = "max-width:1440px;margin:0 auto;padding:20px 12px;",
    div(class = "pi-banner", role = "note", tags$b("Sample data."),
        " Fictional account and made-up numbers for layout review only, not real members."),
    div(class = "pi-title",
      h1("Population Insights"),
      p("IL009999 - Prairie Valley Manufacturing | Data range 10/01/2025 - 09/30/2026 | everything on the right describes the population chosen on the left")),
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        width = 330, title = "Population", bg = "#FFFFFF",
        uiOutput("count_box"),
        filter_block("1", "Account", tags$input(type = "text", class = "form-control", readonly = NA,
                     value = "IL009999 - Prairie Valley Manufacturing", `aria-label` = "Account")),
        and_sep(),
        filter_block("2", "State", selectInput("f_state", NULL, "IL", width = "100%")),
        and_sep(),
        filter_block("3", "County", multi_filter("f_county", county_choices, "All counties", "counties",
                     search = TRUE, disabled = county_disabled, subtext = county_subtext)),
        and_sep(),
        filter_block("4", "Age group", multi_filter("f_age", AGE$name, "All ages", "age groups")),
        and_sep(),
        filter_block("5", "Gender", multi_filter("f_gender", GEN$name, "All genders", "genders")),
        and_sep(),
        filter_block("6", "Race / ethnicity", multi_filter("f_race", RACE$name, "All races / ethnicities", "groups")),
        actionLink("clear", "Clear filters"),
        p(class = "pi-note", "With 2+ counties selected, each chart gets its own County picker.")
      ),
      uiOutput("kpis"),
      navset_pill(
        id = "section",
        nav_panel("Demographics",
          div(class = "pi-grid", style = "margin-top:16px",
            card_ui("gender", "Membership by gender"),
            card_ui("age", "Membership by age group", "Members | % of population"),
            card_ui("race", "Race / ethnicity", "Members | % of population"),
            card_ui("edu", "Education", "Highest level completed, census-based estimate"),
            card_ui("county", "Membership by county", "Filtered members in each selected county"),
            card_ui("vet", "Veteran households", "% of households (county-level)"),
            card_ui("bluedoor", "Near a Blue Door Neighborhood Center", "% of members within 10 miles")
          )),
        nav_panel("Conditions & SDoH",
          navset_underline(
            id = "cat",
            nav_panel("Prevalence",
              div(class = "pi-chips", style = "margin:14px 0", chip_buttons("sub_prev", c("All", unique(PREV$group)))),
              card_ui("prev", "Prevalence", "% of members with the condition")),
            nav_panel("Utilization & Risk",
              p(class = "pi-sub", style = "margin-top:14px", "For the filtered population"),
              lapply(unique(vapply(Filter(function(x) x$cat == "util", CARDS), `[[`, "", "group")), function(g)
                tagList(h4(class = "pi-group", g),
                        div(class = "pi-grid-sm", lapply(Filter(function(x) x$cat == "util" && x$group == g, CARDS),
                                                         function(x) card_ui(x$id, x$name)))))),
            nav_panel("HEDIS Care Gaps",
              div(class = "pi-chips", style = "margin:14px 0", chip_buttons("sub_hedis", c("All", unique(HEDIS$group)))),
              card_ui("hedis", "HEDIS Care Gaps", "Gap rate (gaps / eligible)")),
            nav_panel("SDoH",
              p(class = "pi-sub", style = "margin-top:14px", "Area values for the county - age, gender and race filters don't apply"),
              lapply(unique(vapply(Filter(function(x) x$cat == "sdoh", CARDS), `[[`, "", "group")), function(g)
                tagList(h4(class = "pi-group", g),
                        div(class = "pi-grid-sm", lapply(Filter(function(x) x$cat == "sdoh" && x$group == g, CARDS),
                                                         function(x) card_ui(x$id, x$name))))))
          )),
        nav_panel("Top Diagnoses",
          div(style = "display:flex;flex-wrap:wrap;justify-content:space-between;align-items:center;gap:12px;margin:16px 0 8px",
            div(h2(style = "font-size:18px;margin:0", "Behavioral Health"),
                p(class = "pi-sub", "Top CCS diagnosis categories for each care level | claims and unique members")),
            div(class = "pi-chips", chip_buttons("spmi", c("Non-SPMI" = "non", "SPMI" = "spmi", "All" = "all")))),
          div(class = "pi-legend pi-sub", tags$span(style = paste0("background:", ACCENT)), "Claims ",
              tags$span(style = "background:#E8833A;margin-left:12px"), "Unique members"),
          div(class = "pi-grid", lapply(BH_LEVELS, function(l) card_ui(safe_id("bh", l), l))),
          h2(style = "font-size:18px;margin:24px 0 8px", "Medical"),
          div(class = "pi-grid", lapply(MED_LEVELS, function(l) card_ui(safe_id("med", l), l))))
      )
    )
  )
)

# =============================================================================
# Server
# =============================================================================

server <- function(input, output, session) {

  observeEvent(input$clear, {
    for (id in c("f_county", "f_age", "f_gender", "f_race")) {
      if (HAS_WIDGETS) shinyWidgets::updatePickerInput(session, id, selected = character(0))
      else updateSelectizeInput(session, id, selected = character(0))
    }
  })

  # ---- Filter state: everything on the page reads from this ----------------
  fs <- reactive({
    sel_c <- suppressWarnings(as.integer(input$f_county))
    sel_c <- sort(sel_c[!is.na(sel_c)])
    if (!length(sel_c)) sel_c <- seq_len(nrow(CO))
    sel_a <- match(input$f_age, AGE$name);    if (!length(sel_a)) sel_a <- seq_len(nrow(AGE))
    sel_g <- match(input$f_gender, GEN$name); if (!length(sel_g)) sel_g <- seq_len(nrow(GEN))
    sel_r <- match(input$f_race, RACE$name);  if (!length(sel_r)) sel_r <- seq_len(nrow(RACE))
    share_a <- sum(AGE$members[sel_a]) / TOTAL
    share_g <- sum(GEN$members[sel_g]) / TOTAL
    share_r <- sum(RACE$pct[sel_r]) / 100
    list(
      sel_c = sel_c, sel_a = sel_a, sel_g = sel_g, sel_r = sel_r,
      share_a = share_a, share_g = share_g, share_r = share_r,
      demo = share_a * share_g * share_r,
      ff = wavg(AGE$f, AGE$members, sel_a) * wavg(GEN$f, GEN$members, sel_g) * wavg(RACE$f, RACE$pct, sel_r),
      fem = if (1 %in% sel_g) GEN$members[1] / sum(GEN$members[sel_g]) else 0
    )
  })
  pop_of <- function(idx, s) sum(CO$members[idx]) * s$demo

  # ---- Per-chart County picker ---------------------------------------------
  # Registers <id>_pv (the picker) and returns a reactive scope for <id>_body.
  pivot <- function(id) {
    pv_id <- paste0("pv_", id)
    output[[paste0(id, "_pv")]] <- renderUI({
      sel <- fs()$sel_c
      if (length(sel) < 2) return(NULL)
      choices <- c(setNames("all", sprintf("All selected (%d)", length(sel))),
                   setNames(as.character(sel), CO$name[sel]))
      cur <- isolate(input[[pv_id]]) %||% "all"
      if (!cur %in% choices) cur <- "all"
      selectInput(pv_id, "County", choices, cur, width = "auto", selectize = FALSE)
    })
    reactive({
      sel <- fs()$sel_c
      v <- input[[pv_id]]
      if (length(sel) < 2 || is.null(v) || v == "all" || !v %in% as.character(sel)) {
        list(idx = sel, label = scope_label(sel), h = 6L)
      } else {
        i <- as.integer(v)
        list(idx = i, label = scope_label(i), h = i)
      }
    })
  }

  # ---- Sidebar count + KPI tiles ---------------------------------------------
  output$count_box <- renderUI({
    s <- fs(); n <- pop_of(s$sel_c, s)
    div(class = "pi-count", tags$b(fmt(n)), span(sprintf("members match | %s of account", pct(n / TOTAL * 100))))
  })
  output$kpis <- renderUI({
    s <- fs(); n <- pop_of(s$sel_c, s)
    risk <- 1.04 * s$ff * by_county(CO$cf, s$sel_c) / by_county(CO$cf, seq_len(nrow(CO)))
    kpi <- function(lab, val, sub) div(class = "pi-kpi", tags$small(lab), tags$b(val), tags$small(sub))
    div(class = "pi-kpis",
      kpi("Filtered members", fmt(n), paste(pct(n / TOTAL * 100), "of 6,240")),
      kpi("Female", pct(s$fem * 100), paste(fmt(n * s$fem), "members")),
      kpi("Male", pct((1 - s$fem) * 100), paste(fmt(n * (1 - s$fem)), "members")),
      kpi("Avg EZ medical risk", sprintf("%.2f", risk), "1.00 = book of business"))
  })

  # ---- Demographics -----------------------------------------------------------
  sc_gender <- pivot("gender")
  output$gender_body <- renderUI({
    s <- fs(); sc <- sc_gender(); n <- pop_of(sc$idx, s)
    f_n <- n * s$fem; m_n <- n - f_n
    tagList(p(class = "pi-scope", sc$label),
      div(class = "pi-donut", donut_svg(f_n, m_n),
        div(class = "pi-legend",
          div(span(style = "background:#2F6DB5"), sprintf("Female | %s (%s)", fmt(f_n), pct(s$fem * 100))),
          div(style = "margin-top:8px", span(style = "background:#E8833A"),
              sprintf("Male | %s (%s)", fmt(m_n), pct((1 - s$fem) * 100))))))
  })

  sc_age <- pivot("age")
  output$age_body <- renderUI({
    s <- fs(); sc <- sc_age()
    v <- sum(CO$members[sc$idx]) * s$share_g * s$share_r * AGE$members[s$sel_a] / TOTAL
    tagList(p(class = "pi-scope", sc$label),
            bar_rows(AGE$name[s$sel_a], v, sprintf("%s | %s", fmt(v), pct(v / sum(v) * 100))))
  })

  sc_race <- pivot("race")
  output$race_body <- renderUI({
    s <- fs(); sc <- sc_race()
    v <- sum(CO$members[sc$idx]) * s$share_a * s$share_g * RACE$pct[s$sel_r] / 100
    tagList(p(class = "pi-scope", sc$label),
            bar_rows(RACE$name[s$sel_r], v, sprintf("%s | %s", fmt(v), pct(v / sum(v) * 100))))
  })

  sc_edu <- pivot("edu")
  output$edu_body <- renderUI({
    sc <- sc_edu()
    v <- vapply(seq_along(EDU_NAMES), function(k) by_county(EDU[, k], sc$idx), 0)
    tagList(p(class = "pi-scope", sc$label), bar_rows(EDU_NAMES, v, pct(v, 0)))
  })

  output$county_body <- renderUI({   # compares counties itself: no picker
    s <- fs(); idx <- s$sel_c
    v <- CO$members[idx] * s$demo
    tagList(p(class = "pi-scope", scope_label(idx)),
            bar_rows(CO$name[idx], v, sprintf("%s | %s", fmt(v), pct(v / sum(v) * 100))))
  })

  big_value <- function(sc, value) tagList(div(class = "pi-big", value), p(class = "pi-scope", sc$label))
  sc_vet <- pivot("vet")
  output$vet_body <- renderUI({ sc <- sc_vet(); big_value(sc, pct(by_county(VETERANS, sc$idx))) })
  sc_bd <- pivot("bluedoor")
  output$bluedoor_body <- renderUI({ sc <- sc_bd(); big_value(sc, pct(by_county(BLUE_DOOR, sc$idx), 0)) })

  # ---- Conditions & SDoH ------------------------------------------------------
  sc_prev <- pivot("prev")
  output$prev_body <- renderUI({
    s <- fs(); sc <- sc_prev()
    pop <- pop_of(sc$idx, s); cf <- by_county(CO$cf, sc$idx)
    j <- seq_len(nrow(PREV)); w <- ((j * 7 + sc$h * 3) %% 9) - 4
    p_ <- pmax(0.1, PREV$base * s$ff * cf + w * 0.1 * pmin(1, PREV$base / 3))
    df <- data.frame(group = PREV$group, label = PREV$name,
                     note = paste(fmt(p_ / 100 * pop), "members"),
                     value = pct(p_), width = p_ / 30 * 100, stripe = FALSE)
    if (!identical(input$sub_prev, "All")) df <- df[df$group == input$sub_prev, ]
    tagList(p(class = "pi-scope", sc$label), measure_list(df))
  })

  sc_hedis <- pivot("hedis")
  output$hedis_body <- renderUI({
    s <- fs(); sc <- sc_hedis()
    pop <- pop_of(sc$idx, s); cf <- by_county(CO$cf, sc$idx)
    j <- seq_len(nrow(HEDIS)); w <- ((j * 7 + sc$h * 3) %% 9) - 4
    oe <- HEDIS$group == OE_GROUP
    rate <- pmin(98, pmax(1, round(HEDIS$rate * s$ff * cf + w)))
    ratio <- pmax(0.5, HEDIS$rate * s$ff * cf + w * 0.02)
    el <- round(HEDIS$eligible * pop / TOTAL)
    gaps <- round(rate / 100 * el)
    small <- !oe & el < MIN_N
    df <- data.frame(
      group = HEDIS$group, label = HEDIS$name,
      note = ifelse(oe, "Observed / expected | 1.00 = as expected",
             ifelse(small, paste(el, "eligible | too few to show a rate"),
                    paste(fmt(gaps), "gaps of", fmt(el), "eligible"))),
      value = ifelse(oe, sprintf("%.2f", ratio), ifelse(small, "-", paste0(rate, "%"))),
      width = ifelse(oe, ratio / 1.5 * 100, rate),
      stripe = small
    )
    if (!identical(input$sub_hedis, "All")) df <- df[df$group == input$sub_hedis, ]
    tagList(
      p(class = "pi-scope", sc$label),
      measure_list(df),
      if (any(df$stripe)) p(class = "pi-note", style = "margin-top:12px",
        span(class = "pi-stripe", style = "display:inline-block;width:18px;height:10px;margin-right:6px"),
        sprintf("Fewer than %d eligible: rate not shown", MIN_N))
    )
  })

  lapply(CARDS, function(x) {
    sc <- pivot(x$id)
    output[[paste0(x$id, "_body")]] <- renderUI({
      s <- fs(); scv <- sc()
      v <- by_county(x$values, scv$idx) * (if (x$adjust) s$ff else 1)
      big_value(scv, paste0(formatC(v, format = "f", digits = x$digits, big.mark = ","), x$unit))
    })
  })

  # ---- Top Diagnoses -----------------------------------------------------------
  dx_card <- function(id, rows_fn) {
    sc <- pivot(id)
    output[[paste0(id, "_body")]] <- renderUI({
      s <- fs(); scv <- sc()
      base <- rows_fn()
      scale <- pop_of(scv$idx, s) / TOTAL
      cf <- by_county(CO$cf, scv$idx)
      claims <- round(base$claims * scale * s$ff * cf)
      members <- round(base$members * scale)
      keep <- claims > 0
      d <- data.frame(dx = base$dx, claims = claims, members = members)[keep, ]
      d <- d[order(-d$claims), ]
      if (!nrow(d)) return(tagList(p(class = "pi-scope", scv$label), p(class = "pi-sub", "No claims for this population.")))
      m <- max(d$claims)
      tagList(p(class = "pi-scope", scv$label),
        lapply(seq_len(nrow(d)), function(i)
          div(class = "pi-dx-row", span(d$dx[i]),
            div(
              div(class = "pi-dx-bar", span(class = "b", style = sprintf("width:%.1f%%;background:%s", d$claims[i] / m * 100, ACCENT)), fmt(d$claims[i])),
              div(class = "pi-dx-bar", span(class = "b", style = sprintf("width:%.1f%%;background:#E8833A", d$members[i] / m * 100)), fmt(d$members[i]))))))
    })
  }
  lapply(BH_LEVELS, function(l) dx_card(safe_id("bh", l), function() {
    sp <- input$spmi %||% "non"
    d <- DX[DX$level == l & (if (sp == "all") DX$type %in% c("non", "spmi") else DX$type == sp), ]
    # "All" adds Non-SPMI + SPMI. In the real query, count unique members once.
    agg <- aggregate(cbind(claims, members) ~ dx, data = d, FUN = sum)
    head(agg[order(-agg$claims), ], 4)
  }))
  lapply(MED_LEVELS, function(l) dx_card(safe_id("med", l), function() DX[DX$type == "med" & DX$level == l, ]))
}

shinyApp(ui, server)
