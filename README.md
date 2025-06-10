# Master Thesis: Clustering and Forecasting of Time Series  
**Application to the European Electricity Market**  
**Alicia Burgos Carrascón — June 2022**  

Welcome to the repository for my Master’s Thesis, which explores advanced techniques in clustering and forecasting of time series, specifically applied to the European electricity market. This repository contains the thesis document, code, and datasets needed to reproduce the results.

![image](https://github.com/user-attachments/assets/be49d7a5-237f-4022-8578-6385881df2c3)


---

## Table of Contents

- [Overview](#overview)
- [Repository Structure](#repository-structure)
- [Thesis Document](#thesis-document)
- [Data](#data)
- [Code](#code)
- [Dependencies](#dependencies)
- [How to Run](#how-to-run)
- [Results and Outputs](#results-and-outputs)
- [References](#references)
- [Contact](#contact)

---

## Overview

This project analyzes monthly electricity price series from various European countries. The main objectives are:

- **Clustering**: Grouping countries with similar temporal evolution using multiple distance metrics (Euclidean, DTW, COR, CORT) and hierarchical clustering techniques.
- **Representative Selection**: Identifying a representative series for each cluster.
- **Forecasting**: Applying state-of-the-art forecasting models (SARIMA, TBATS, ARNN, k-NN, SVM, ensemble methods) to selected representatives.
- **Model Comparison**: Evaluating predictive accuracy using error measures (RMSE, MAE) and comparing individual vs. combined models.

All steps, from data preprocessing to visualization and model evaluation, are implemented in R and included in this repository.

---

## Repository Structure

```
.
├── TFM_AliciaBurgos.pdf      # Full thesis document (in Spanish)
├── tfm19-06.R                # Annotated R script with the full workflow
├── datostfm (1).xlsx         # Dataset: Monthly electricity prices in Europe
├── ARNN.R                    # ARNN model implementation (if separated)
├── [Outputs]/                # (Generated) Results: plots, tables, PDFs
├── README.md                 # (This file)
```

---

## Thesis Document

The thesis is available as [TFM_AliciaBurgos.pdf](./TFM_AliciaBurgos.pdf) and includes:

- Abstract & Introduction
- Data Description
- Methodology (Clustering, Forecasting)
- Results & Discussion
- Conclusions & Future Work
- References

> **Language:** Spanish

---

## Data

- **File:** `datostfm (1).xlsx`
- **Content:** Monthly average electricity prices (in €/MWh) for 23 European countries, from January 2015 to December 2021.
- **Sheet:** `'Hoja2'`, data range `'B8:Z92'`.

---

## Code

- **Main script:** [`tfm19-06.R`](./tfm19-06.R)
  - Loads and preprocesses data
  - Performs clustering with various distance metrics and linkages
  - Selects cluster representatives
  - Runs predictive models (SARIMA, TBATS, ARNN, kNN, SVM)
  - Combines models using ensemble techniques (ForecastComb)
  - Generates plots and error metrics for comparison

- **Supporting script:** `ARNN.R` (if separated)
  - Provides the ARNN (AutoRegressive Neural Network) model implementation

---

## Dependencies

Ensure you have R installed (recommended: R 4.0+). Required R packages include:

- `readxl`
- `xts`
- `dygraphs`
- `TSclust`
- `cluster`
- `pdc`
- `MASS`
- `factoextra`
- `NbClust`
- `e1071`
- `forecast`
- `tsfknn`
- `rnn`
- `ForecastComb`

Install all required packages with:

```r
install.packages(c("readxl", "xts", "dygraphs", "TSclust", "cluster", "pdc",
                   "MASS", "factoextra", "NbClust", "e1071", "forecast",
                   "tsfknn", "rnn", "ForecastComb"))
```

---

## How to Run

1. **Clone the repository:**
   ```bash
   git clone https://github.com/alicia-enelpais/Master.git
   cd Master
   ```

2. **Open R and set the working directory.**

3. **Run the script:**
   - Edit the path in `tfm19-06.R` if needed, or select the data file when prompted.
   - Source the script in R:
     ```r
     source("tfm19-06.R")
     ```
   - The script will generate results, plots, and PDFs in the working directory.

> **Note:** All instructions and comments in `tfm19-06.R` are in Spanish. Detailed explanations are in the thesis document.

---

## Results and Outputs

- **Plots:** Cluster visualizations, dendrograms, model fits, and forecast comparisons.
- **Tables:** Error metrics for all models.
- **PDFs/PNGs:** Saved in the working directory as specified in the script.

---

## References

All academic and technical references are listed in the thesis document.

---

## Contact

**Author:** Alicia Burgos Carrascón  
**GitHub:** [alicia-enelpais](https://github.com/alicia-enelpais)  
**University:** [URJC University/ Program: Ingeniería de Sistemas de Decisión]

---

### Acknowledgments

Thanks to my supervisors, professors, and colleagues who supported this work <3.

---

**License:** Please see the repository for license details.  
