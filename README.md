# 🏠 STAT 6950 Final Project: Housing Prices and Economic Factors

## 📖 Introduction
This repository contains the final project for **STAT 6950**, conducted by **Chenze Li and Pin-Hsun Mao**.  
The study investigates the **relationship between U.S. housing prices and economic factors** using data from **Harvard Dataverse (1990–2020)**.  

The project aims to:
- Explore the trends in housing prices.  
- Identify key economic factors driving housing price fluctuations.  
- Build predictive models to forecast housing prices.  

---

## ⚙️ Data
- **Source**: Harvard Dataverse housing dataset.  
- **Period**: January 1990 – October 2020.  
- **Variables**: 14 macroeconomic and housing-related factors, including:  
  - Unemployment Rate (UNEMP)  
  - Construction Spending (CONST)  
  - Mortgage Rate (30-Year Fixed)  
  - Housing Starts & Permits  
  - Disposable Income & Consumption  
  - Savings  
  - Homes for Sale / Sold  
  - Home Price Index (HPI, response variable)  

---

## 🔍 Methodology
1. **Exploratory Data Analysis (EDA)**  
   - Correlation analysis and heatmaps.  
   - Scatter plots for nonlinear relationships.  
   - Time plots to assess seasonality.  

2. **Modeling Approach**  
   - Linear regression with backward stepwise selection (BIC).  
   - Residual diagnostics and transformation (Box-Cox, log).  
   - Autoregressive models for error terms (AR(2)).  
   - Segmented modeling for two periods:  
     - **2000–2006 (Early Period)**  
     - **2014–2020 (Later Period)**  

3. **Evaluation**  
   - AIC/BIC for model selection.  
   - Residual diagnostics (normality, homoscedasticity, autocorrelation).  
   - Train-test split for predictive validation.  

---

## 📊 Results
- **Key Predictors**: Construction Spending, Consumption, and Savings consistently appeared in all models.  
- **Early Period (2000–2006)**:  
  - Log(HPI) positively associated with construction spending, consumption, and savings.  
  - Negatively associated with disposable income and mortgage rates.  
- **Later Period (2014–2020)**:  
  - Housing prices increased with construction spending, consumption, and savings.  
  - Negatively associated with disposable income.  
- **Autoregressive Error Models** improved predictions but revealed instability during 2020.  

---

## ✅ Conclusion
- **Economic drivers of housing prices**: Consumption, savings, and construction spending are consistently important.  
- **Period-specific effects** highlight changing dynamics of income and mortgage rates.  
- The final models provide interpretable insights and reasonable predictive ability, though **external shocks (e.g., COVID-19 in 2020)** require further investigation.  

---

## 📚 References
- Harvard Dataverse: [Home Price Dataset](https://dataverse.harvard.edu/dataverse/HomePrice)  
- Shumway, R. & Stoffer, D. (2000). *Time Series Analysis and Its Applications*. Springer.  
- Brockwell, P. & Davis, R. (2002). *Introduction to Time Series and Forecasting*. Springer.  
- Fox, J. (2015). *Applied Regression Analysis and Generalized Linear Models*.  

