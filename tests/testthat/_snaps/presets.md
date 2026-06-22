# preset definitions are stable (snapshot of all presets)

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["minimal", "standard", "extended"]
        }
      },
      "value": [
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["name", "label", "files", "descriptions"]
            }
          },
          "value": [
            {
              "type": "character",
              "attributes": {},
              "value": ["minimal"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Minimal (3 steps)"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["01_read_data", "02_eda", "03_report"]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["01_read_data", "02_eda", "03_report"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["Import raw data from data/raw/"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["Exploratory data analysis: summaries, distributions, correlations"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["Final summary report and export"]
                }
              ]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["name", "label", "files", "descriptions"]
            }
          },
          "value": [
            {
              "type": "character",
              "attributes": {},
              "value": ["standard"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Standard (8 steps)"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["01_read_data", "02_tidy", "03_preprocess", "04_descriptive", "05_inference", "06_modelling", "07_dim_reduction", "08_sub_reports"]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["01_read_data", "02_tidy", "03_preprocess", "04_descriptive", "05_inference", "06_modelling", "07_dim_reduction", "08_sub_reports"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["Import raw data from data/raw/"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["Clean, reshape, and validate data"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["Handle missing data, outliers, transformations"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["Table 1, distributions, correlations"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["Hypothesis testing and group comparisons"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["Predictive modelling with tidymodels"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["PCA, UMAP, t-SNE projections"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["Subgroup analyses and parameterized reports"]
                }
              ]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["name", "label", "files", "descriptions"]
            }
          },
          "value": [
            {
              "type": "character",
              "attributes": {},
              "value": ["extended"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Extended (12 steps)"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["01_read_data", "02_tidy", "03_preprocess", "04_quality_control", "05_descriptive", "06_inference", "07_modelling", "08_model_diagnostics", "09_dim_reduction", "10_sensitivity", "11_sub_reports", "12_final_report"]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["01_read_data", "02_tidy", "03_preprocess", "04_quality_control", "05_descriptive", "06_inference", "07_modelling", "08_model_diagnostics", "09_dim_reduction", "10_sensitivity", "11_sub_reports", "12_final_report"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["Import raw data from data/raw/"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["Clean, reshape, and validate data"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["Handle missing data, outliers, transformations"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["Data quality checks, completeness, plausibility"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["Table 1, distributions, correlations, missingness"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["Hypothesis testing and group comparisons"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["Predictive modelling with tidymodels"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["Residuals, calibration, ROC/AUC, model comparison"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["PCA, UMAP, t-SNE projections"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["Sensitivity analyses: complete case, per-protocol, worst-case"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["Subgroup analyses and parameterized reports"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["Integrated final report with all results"]
                }
              ]
            }
          ]
        }
      ]
    }

