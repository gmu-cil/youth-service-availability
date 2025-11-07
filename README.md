---
title: Youth Service Availability and School Dropout
author: Myeong Lee, Julia H.P. Hsu, Gie Myung Lee
date: October 2025
---

# Youth Service Availability and School Dropout

* This repository contains data and analysis scripts for our paper publisehd in Research on Social Work Practice (Sage):
[Leveling Socoeconomic Disparities: The Role of Service Availability in School Dropout Rates](https://journals.sagepub.com/doi/full/10.1177/10497315251377009).

## Data Sources

### Census Data
Census variables are downloaded using the R census package from the [U.S. Census Bureau](https://www.census.gov/).

### Geographic Data
Boundary shapefiles from the [U.S. Census Bureau TIGER/Line Shapefiles](https://catalog.data.gov/dataset/tiger-line-shapefile-2016-state-virginia-current-county-subdivision-state-based).
Aggregated by **county** and **school district**.

### Clinic Data
Virginia Youth Clinic Locations and treatment types:
  - Substance Use (SU)
  - Mental Health (MH)
  - Health Care Centers (HC)
  - Buprenorphine Practitioners (BUPREN)
  - Opioid Treatment Programs (OTP)
- Sources:
  - [National Directory of Drug and Alcohol Abuse Treatment Facilities (2022)](https://www.samhsa.gov/data/report/national-directory-of-drug-and-alcohol-abuse-treatment-facilities)
  - [National Directory of Mental Health Treatment Facilities (2022)](https://www.samhsa.gov/data/report/national-directory-of-mental-health-treatment-facilities)
  - [Psychology Today](https://www.psychologytoday.com/us/treatment-rehab/virginia)
  - [Virginia Association of Free and Charitable Clinics](https://www.vafreeclinics.org/clinics-in-virginia)
  - [FindTreatment.gov](https://findtreatment.gov/)

## Files Description

### Data Aggregation
- `src/py/aggregation_average_values.ipynb` - Aggregates census variables using average values
- `src/py/aggregation_median_age.ipynb` - Aggregates census variables using grouped median calculation for age
- `src/py/aggregation_median_income.ipynb` - Aggregates census variables using grouped median calculation for income

### Analysis
- `src/py/dropout_rate_pred.ipynb` - Dropout rate prediction model and analysis

### R Scripts
- `src/r/drop_comm_agg.R` – Aggregates geocoded clinic and community data by county.
- `src/r/regression_Social_Work.R` – Regression analysis examining relationships between service availability and dropout rates.

### Outputs
- `output/aggregation.geojson` – Spatial aggregation of clinic locations.
- `output/dropout_attached.geojson` – Joined dataset combining community and dropout data for mapping.
