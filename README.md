# SewingMedieval
## An interactive Shiny app for creating medieval sewing patterns from your own measurements.

The aim of the project is to make it easier to create and explore medieval sewing patterns using individual body measurements. Instead of relying on standard clothing sizes, users can enter their own measurements and use them to generate pattern dimensions for several medieval garments and accessories.

### What can you make?

The app currently includes patterns for:

Medieval alms purse (aumônière) — a small medieval pouch
Circle skirt/ medieval cloak — a circular garment that can be adapted to different measurements
Underdress/ undergarment/ undershift (chemise) — a basic medieval undergarment
Surcoat/ sideless surcote (pellote) — a sleeveless outer garment

The terminology and garment names reflect the variety of terms used when discussing historical clothing. The app is intended as a practical starting point for making garments.

### How it works
Enter your measurements in the Shiny app.
Select a garment or pattern you would like to make.
The app uses your measurements to calculate the relevant pattern dimensions.
Use the resulting dimensions to draft and construct your own pattern.

The goal is to make pattern drafting more accessible to people who are interested in historical clothing, reenactment, experimental archaeology, or simply sewing their own medieval-inspired garments.

### Getting started

The application is written in R using Shiny.

To run the app locally, clone this repository and open the project in RStudio, Visual Studio Code, or Positron:

git clone https://github.com/StellaPaulina/SewingMedieval.git
cd SewingMedieval

Then open the Shiny application in R/RStudio/Positron and run it.

### Requirements

You will need:

R
RStudio (recommended)
The R packages used by the application

If required packages are not already installed, install them in R with:

install.packages("shiny")

Additional package requirements can be found in the project files.


### Historical context

Medieval garments were constructed in many different ways depending on period, region, material, etc. This app is therefore not intended to claim that there was one universal "medieval pattern." Instead, it provides computational tools for experimenting with selected garment constructions and adapting them to individual measurements.


### Authors

Fatemeh Rangani, Libuše Janská & Stella Axelsson

Created during the NBIS RaukR course, August 2026.

Repository: SewingMedieval on GitHub
