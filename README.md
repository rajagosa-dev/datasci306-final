# DATASCI 306 Final Project: Ann Arbor Housing Price Gradient

This Shiny app explores how home characteristics (beds, baths, square footage, and direction from the city center) change the estimated price gradient with distance from downtown Ann Arbor. Users can compare predictions from a simple distance-only model and a multiple regression model that adjusts for home features.

---

## 🔗 Live App  
https://05i3vc-samarth-rajagopal.shinyapps.io/datasci306-final/

---

## Files Included
- `FINAL.R` — Full Shiny application code  
- `a2housing.RData` — Cleaned dataset used in the app  
- `a2_housing.csv` — Raw dataset provided by instructors  

---

## Running the App Locally

1. Download all files in this repository  
2. Place them in the same directory  
3. In RStudio, run:

```r
shiny::runApp("PATH/TO/FOLDER")
