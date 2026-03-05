# Smoke Plume Detection Agent
## Satellite-Based Strike Monitoring for the Middle East

### Architecture

```
┌─────────────────────────── TRAIN (run once / periodically) ───────────────────────┐
│                                                                                    │
│  UCDP-GED API ────→ Label Collection ──→ Sentinel-2 Feature Extraction            │
│         +                                         │                                │
│  FIRMS Negatives ──→ Negative Sampling             ▼                               │
│  (flares, ag burns)               Spatial CV → RF + XGBoost + LogReg → Export      │
│                                                                                    │
└────────────────────────────────────────────────────┬───────────────────────────────┘
                                                     │
                                             models/strike_classifier.rds
                                                     │
┌────────────────────────── DETECT (every 6 hours) ──┴──────────────────────────────┐
│                                                                                    │
│  NASA FIRMS API ──→ Thermal Filter ──→ Sentinel-2 Imagery ──→ Model Scoring       │
│                                                                      │             │
│                                                                      ▼             │
│                                                          Shiny Review Dashboard    │
│                                                                      │             │
│                                                                      ▼             │
│                                                     X Post (image + map)           │
└────────────────────────────────────────────────────────────────────────────────────┘
```

### File Structure

| File | Purpose |
|------|---------|
| `00a_build_training_data.R` | Collect labeled events from UCDP-GED + negatives, extract spectral features |
| `00b_train_model.R` | Train RF + XGBoost with spatial CV, threshold optimization, diagnostics |
| `01_detect_hotspots.R` | Pull FIRMS thermal anomalies, filter, cluster, exclude known industrial |
| `02_pull_imagery.R` | Fetch Sentinel-2 true-color, SWIR, and aerosol composites |
| `03_classify_smoke.R` | Score events with trained model (rule-based fallback if no model) |
| `04_generate_post.R` | Compose satellite + map image, generate post text |
| `05_post_to_x.R` | Publish human-approved posts to X via API v2 |
| `app.R` | Shiny review dashboard for human-in-the-loop approval |
| `run_pipeline.R` | Orchestrator — supports `train`, `detect`, `retrain`, `full` modes |

### Quick Start

```bash
# 1. Fill in API credentials
cp .Renviron.template .Renviron
# Edit .Renviron with your keys

# 2. Install R packages
Rscript -e "install.packages(c('httr2','tidyverse','sf','terra','ranger',
  'xgboost','tidymodels','vip','probably','leaflet','shiny','bslib',
  'magick','showtext','glue','DT','patchwork','rnaturalearth',
  'rnaturalearthdata','geosphere','base64enc'))"

# 3. Train the model (one-time, ~2-4 hours depending on sample size)
Rscript run_pipeline.R train

# 4. Run detection
Rscript run_pipeline.R detect

# 5. Review detections
Rscript -e "shiny::runApp('app.R')"

# 6. Publish approved posts
Rscript 05_post_to_x.R
```

### Scheduling

```bash
# Cron: run detection every 6 hours
0 */6 * * * cd /path/to/project && Rscript run_pipeline.R detect >> logs/pipeline.log 2>&1

# Cron: retrain monthly with fresh UCDP data
0 3 1 * * cd /path/to/project && Rscript run_pipeline.R retrain >> logs/retrain.log 2>&1
```

### API Keys Required

| Service | Purpose | Cost | Registration |
|---------|---------|------|-------------|
| NASA FIRMS | Thermal anomaly data | Free | firms.modaps.eosdis.nasa.gov |
| UCDP-GED | Confirmed violence labels | Free (token required) | ucdp.uu.se/apidocs |
| Copernicus CDSE | Sentinel-2 catalog | Free | dataspace.copernicus.eu |
| Sentinel Hub | Image rendering API | Free tier (30k req/mo) | sentinel-hub.com |
| X Developer | Posting | Free (basic) | developer.x.com |

### Model Details

The classifier distinguishes airstrike smoke from common false positives:

**Positive class**: Violent events from UCDP-GED filtered for airstrike/bombardment signatures (keyword matching on source text + state-based violence type + one-sided violence against civilians)

**Negative class**: Gas flares, agricultural burns, industrial fires, background FIRMS detections

**Features** (~70 total):
- Sentinel-2 band statistics (B02–B12 mean, sd, max)
- Spectral indices (NDVI, aerosol index, SWIR smoke index, NBR, red edge)
- Spatial texture (center-edge brightness/SWIR contrast, B12 variance)
- FIRMS-derived (FRP, detection count, nighttime flag)
- Contextual (latitude, seasonality encoding)

**Validation**: Spatial cross-validation (events grouped by geographic cluster) to prevent spatial leakage. Threshold optimized for ≥85% precision.

### Important Caveats

- **Human review is essential.** The model reduces false positives but cannot eliminate them.
- **Cloud cover** limits Sentinel-2 availability. Some events may lack imagery.
- **Timing**: Sentinel-2 revisit is ~5 days. FIRMS detects events within hours, but optical imagery may lag.
- **Attribution**: Detecting smoke from a thermal anomaly is not the same as confirming a military strike. Always caveat posts accordingly.
- **Labeling quality**: UCDP data requires keyword filtering to identify airstrikes (it doesn't have a dedicated sub-event field like ACLED). The three-tier system (confirmed/probable/possible) helps, but ground-truth verification always improves the model.
