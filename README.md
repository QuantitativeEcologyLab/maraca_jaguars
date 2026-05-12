# Jaguar Movement Ecology - Maracá-Jipioca Island

This repository contains the data and scripts required to reproduce all analyses and figures in the paper "Spatial ecology of jaguars in a coastal Amazon island ecosystem" by Leandro Silveira, Anah T. A. Jácomo, Tiago J. Silveira, Gediendson R. de Araujo, Renato A. Moreira, Francesca B. L. Palmeira, Marcia de A. M. M. Ferraz, and Michael J. Noonan

## Dependencies

All analyses were conducted in R version 4.3.3. The following packages are required:

```r
install.packages(c(
  "ctmm",             # v1.1.0   - movement modelling
  "mgcv",             # v1.9-1   - statistical models
  "metafor",          # v4.8-0   - meta-analytic methods
  "ggplot2",          # v3.5.2   - figures
  "terra",            # v1.8-60  - raster processing
  "raster",           # v3.6-32  - raster processing (legacy, required by ctmm)
  "sf",               # v1.0-21  - spatial data
  "ggspatial",        # v1.1.9   - scale bars
  "tidyterra",        # v0.7.2   - maps in ggplot
  "scico",            # v1.5.0   - colour palettes
  "gratia",           # v0.10.0  - GAM visualisation
  "ggpubr",           # v0.6.1   - figure layout
  "gridExtra",        # v2.3     - figure layout
  "lubridate",        # v1.9.4   - date/time handling
  "weights",          # v1.1.2   - weighted proportions
  "fitdistrplus",     # v1.2-4   - distribution fitting
  "ggdist",           # v3.3.3   - distribution visualisation
  "rnaturalearth",    #          - country boundary data
  "rnaturalearthdata" #          - country boundary data
))
```

## Repository Structure

```
data/
    jaguar_metadata.csv                                   # Individual-level metadata (sex, age, weight)
    jaguar_speeds.csv                                     # Instantaneous movement speed estimates
    gps/
        jaguar_data_locations_Maraca-Jipioca.csv          # GPS tracking data
    environment/
        maraca.tif                                        # MapBiomas 2023 land classification raster
        maraca/
            maraca.shp                                    # Maracá-Jipioca island boundary shapefile
        spatial_tide_data.nc                              # Hourly sea level data (CMEMS)

results/
    jaguar_fits.Rda                                       # Fitted ctmm movement models
    jaguar_akdes.Rda                                      # AKDE home range estimates
    jaguar_speeds.Rda                                     # Mean speed estimates
    jaguar_overlap.csv                                    # Pairwise home range overlap and proximity ratios
    jaguar_habitat_use.csv                                # Weighted habitat use proportions from HR PDFs
    distance_df.Rda                                       # Pairwise separation distances
    movement_models/                                      # Per-individual fitted movement models (.rda)
    akdes/                                                # Per-individual AKDE home range estimates (.rda)

scripts/
    00_functions.R
    01_data_import.R
    02_movement_modelling.R
    03_hr_analysis.R
    04_directional_persistence_analysis.R
    05_speed_analysis.R
    06_habitat_selection_analyses.R
    07_tide_analysis.R
    08_figures_1_2.R
    09_figure_3.R
    10_figure_4.R
    11_figure_5.R
    12_figure_s5.R

figures/                                                  # Output figures

writing/                                                  # Manuscript files
```

## Data Availability

The GPS tracking data will be deposited on Movebank prior to publication. The Movebank study ID will be provided here upon acceptance.

The following environmental layers are required to reproduce the analyses and are publicly available at the URLs below:

**Land cover classification (Brazil)**
MapBiomas Brazil Collection 9 land use and land cover classification.
- Data: https://brasil.mapbiomas.org/en/
- Legend key: https://brasil.mapbiomas.org/wp-content/uploads/sites/4/2024/10/Legenda-Colecao-9-LEGEND-CODE_v2.pdf

**Sea level data**
Copernicus Marine Environment Monitoring Service (CMEMS) global ocean physics analysis and forecast.
- Data: https://marine.copernicus.eu/

## Workflow

Scripts should be run in the following order. Scripts 03 onward require scripts 00-02 to have been run first but are otherwise independent of each other.

| Script | Description |
|--------|-------------|
| `00_functions.R` | Custom functions used across all scripts |
| `01_data_import.R` | Imports tracking data, environmental rasters, and pre-computed results |
| `02_movement_modelling.R` | Fits ctmm movement models, estimates AKDE home ranges and movement speeds, and calculates pairwise overlap and proximity ratios |
| `03_hr_analysis.R` | Analyses home range sizes including sex differences and correlations with body weight and age |
| `04_directional_persistence_analysis.R` | Analyses directional persistence (tau_v) including sex differences and correlations with body weight and age |
| `05_speed_analysis.R` | Analyses movement speeds, habitat-specific activity, and diurnal activity patterns |
| `06_habitat_selection_analyses.R` | Fits resource selection functions and generates habitat selection figures |
| `07_tide_analysis.R` | Models the relationship between tidal sea level and distance to coast |
| `08_figures_1_2.R` | Generates Figures 1 and 2 (study area map and home range summaries) and Figure S2 |
| `09_figure_3.R` | Generates Figure 3 (home range overlap and encounter rates) and Figure S3 |
| `10_figure_4.R` | Generates Figure 4 (movement speed summaries) and Figure S4 |
| `11_figure_5.R` | Generates Figure 5 (diurnal activity patterns) |
| `12_figure_s5.R` | Generates Figure S5 (weighted habitat use proportions) |

## Citation

...

## License

MIT