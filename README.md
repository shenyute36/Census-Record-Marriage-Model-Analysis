## Data Privacy Notice 
**Confidentiality & Compliance:**
This project analyzes sensitive **Taiwan Census Data (1990 & 2000)**. Due to data privacy issues, the raw datasets are **confidential and not included** in this repository.

The provided R scripts demonstrate the data cleaning logic, string comparison, and probabilistic matching models used in the research and figure, assuming the data is available.

## Project Overview

This research investigates patterns in Taiwan’s marriage market in 1990 and 2000, focusing on:

- **Age assortative mating**  
- **Net gains from marriage for men and women**  
- **Total systematic gain to marriage (π-value)**  
- **Gender-availability imbalances across age groups**  
- **Historical anomaly among men born ~1930** (likely linked to 1949 KMT retreat)

The analysis follows the structural model in:

> Choo & Siow (2006), *Who Marries Whom and Why*, Journal of Political Economy.

Full details are included in the research PDF.

## Methodology Summary

### **1. Couple Identification**
Because Census data do not directly list spouse IDs, couples are detected using:

- Household head ↔ spouse  
- Parent pairs  
- Child ↔ child’s spouse  
- Sex cross-checking  
- Restriction to households with exactly one man and one woman marked as married

This yields **~75.5% identified couples**, consistent with demographic constraints.

### **2. Constructing Marriage Match Frequencies**
From identified couples:

- Count all matches by `(man_age, woman_age)`
- Construct availability vectors for men and women
- Build contingency tables (age–age grids)

### **3. Visualization**
The scripts generate:

- Age–age surface plots  
- Net gain curves  
- π-value contour maps  
- Distributions  

Mathematical details appear in the PDF report.


## Repository Structure

```text
Census-Record-Marriage-Model-Analysis/
│
├── README.md                        # Project overview and privacy notice
├── Census_Data_Matching_Model.pdf   # Full research report detailing the mathematical framework and results
│
├── 1990.R                           # Data preprocessing and linkage logic for 1990 Census data
└── 2000.R                           # Data preprocessing and linkage logic for 2000 Census data
