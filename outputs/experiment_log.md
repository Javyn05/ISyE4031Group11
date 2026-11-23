# Experiment log (auto-generated)
Run started 2026-11-19 07:32:32

## 1. Data 
- Quality filter: 3567007 values flagged 8/9 set to missing 
- raw bottles: 864,863 
- complete cases (target + 9 preds): 204,134 
- after physical-range check: 204,093 
- split: train: 166,102 
- split: valid: 10,552 
- split: test: 27,439 
- Year range in analysis set: 1977-2016 

## 2. Tuning on validation years 2009-2010 (test years untouched) 
- Broken-stick temperature break chosen on validation: 12.25 C 
- RF tuned on validation: mtry = 3, min.node.size = 5 (M2 used 2 / 5) 

## 3. Main comparison: fit on <= 2010, test on 2011-2016 (n = 27,439) 
- GAM fit time: 0.6 s 
- RF fit time: 16.1 s 
- Mean (reference)     test RMSE 26.807 | MAE 23.104 | R2 -0.0261 | train RMSE 24.985 
- OLS (9 predictors)   test RMSE 4.525 | MAE 3.364 | R2 0.9708 | train RMSE 4.231 
- Broken stick OLS     test RMSE 4.210 | MAE 2.967 | R2 0.9747 | train RMSE 3.748 
- GAM                  test RMSE 3.908 | MAE 2.677 | R2 0.9782 | train RMSE 3.410 
- Random forest        test RMSE 3.379 | MAE 2.323 | R2 0.9837 | train RMSE 1.013 
- GAM effective df per smooth: Depthm=7.8, T_degC=8.8, Salnty=8.8, PO4uM=8.9, SiO3uM=8.6, NO2uM=8.7, NO3uM=7.5, ChlorA=8.9, Phaeop=8.2 

## 4. Test RMSE by depth band (OLS vs RF) 
- 0-50 m     n= 10777  OLS 5.31  GAM 5.16  RF 4.22 
- 50-200 m   n= 15228  OLS 3.95  GAM 2.88  RF 2.74 
- 200-500 m  n=  1434  OLS 3.79  GAM 2.13  RF 2.21 

## 5. Feature-group ablation (fit <= 2010, test 2011+) 
- All 9           OLS 4.53 | GAM 3.91 | RF 3.38 | RF gain 25.3% 
- Drop nutrients  OLS 8.81 | GAM 6.76 | RF 5.67 | RF gain 35.7% 
- Drop biology    OLS 4.69 | GAM 4.04 | RF 3.61 | RF gain 23.1% 
- Drop physical   OLS 4.61 | GAM 4.30 | RF 3.80 | RF gain 17.5% 
- Physical only   OLS 9.52 | GAM 7.11 | RF 6.24 | RF gain 34.4% 
- Nutrients only  OLS 4.86 | GAM 4.65 | RF 4.32 | RF gain 11.1% 

## 6. Uncertainty 
- RF test RMSE across 2 seeds: mean 3.382, sd 0.0021, range 3.380-3.383 
- (Year-to-year spread is in table 06 -- usually much bigger than seed spread) 
- 90% interval coverage on test (target 0.90): 
  - ALL        OLS 0.919 (width 13.9) | QRF 0.845 (width 9.3) 
  - 0-50 m     OLS 0.894 (width 13.9) | QRF 0.809 (width 10.9) 
  - 50-200 m   OLS 0.933 (width 13.9) | QRF 0.872 (width 8.4) 
  - 200-500 m  OLS 0.954 (width 13.9) | QRF 0.826 (width 6.3) 

## 8. Does old bottle-era data help or hurt? (train 1994-2010 only) 
- All years <= 2010  n= 176654  OLS 4.53 | RF 3.38 
- CTD era 1994-2010  n=  90677  OLS 4.32 | RF 3.29 

Done 2026-11-19 07:38:10. Tables in outputs/tables, figures in outputs/figures. 
