# youth-service-availability

## Data Sources

### Census Data
Census variables are downloaded using the R census package from the [U.S. Census Bureau](https://www.census.gov/).

### Geographic Data
Shape files are downloaded from [Data.gov](https://www.data.gov/). Data is aggregated according to boundary types:
- County-level aggregation
- School district-level aggregation

## Files Description

### Data Aggregation
- `aggregation_average_values.ipynb` - Aggregates census variables using average values
- `aggregation_median_age.ipynb` - Aggregates census variables using grouped median calculation for age
- `aggregation_median_income.ipynb` - Aggregates census variables using grouped median calculation for income

### Analysis
- `dropout_rate_pred.ipynb` - Dropout rate prediction model and analysis

