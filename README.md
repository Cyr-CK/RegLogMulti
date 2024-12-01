# README - reg-log-multi-for-R

## A Multinomial Logistic Regression Package for R with a Shiny Application

The **reg-log-multi-for-R** package provides a comprehensive implementation of multinomial logistic regression, equipped with a user-friendly Shiny application. This package simplifies data preprocessing, model training, and evaluation, making it accessible to users without advanced programming skills.

---


## Table of Contents
1. [Description](#description)
2. [Installation](#installation)
3. [Features](#features)
4. [Shiny Application Guide](#shiny-application-guide)
   - [Data Management](#data-management)
   - [Preprocessing](#preprocessing)
   - [Model Training](#model-training)


---

## Description
The **reg-log-multi-for-R** package enables users to:
- Handle datasets with mixed quantitative and qualitative variables.
- Train multinomial logistic regression models using gradient descent optimizers like Adam and SGD.
- Visualize model performance through loss curves, confusion matrices, and accuracy metrics.

The included Shiny application provides an interactive graphical interface for data handling, preprocessing, model configuration, and evaluation.

---
## Installation

To install and set up the package:

1. **Clone the repository**:
   ```bash
   git clone https://github.com/username/reg-log-multi-for-R.git
   cd reg-log-multi-for-R

## Features

### Data Preprocessing
- Manage missing values for both qualitative and quantitative variables.
- Perform One-Hot Encoding for categorical variables.
- Scale numerical features using methods like Standard Scaler, Min-Max Scaler, or Robust Scaler.
- Apply Factorial Analysis of Mixed Data (FAMD) for dimensionality reduction.

### Modeling
- Multinomial logistic regression implementation.
- Parameter optimization using gradient descent algorithms such as Adam and SGD.
- Support for L1 and L2 regularization.

### Evaluation
- Calculate model accuracy.
- Generate confusion matrices.
- Visualize loss curves and learning rate progression.

### Interactive User Interface
- A Shiny application for intuitive use without the need for programming expertise.

---

## Shiny Application Guide

### Data Management
1. Open the **Data** tab to upload your dataset in `.csv` or `.xlsx` format.
2. View the dataset structure and detect missing values.
3. Impute missing values using available options for both qualitative and quantitative features.

### Preprocessing
1. Navigate to the **Preprocessing** tab.
2. Apply transformations such as:
   - One-Hot Encoding for categorical variables.
   - Scaling techniques like Standard Scaler, Min-Max Scaler, or Robust Scaler for numerical features.
3. Use the **FAMD** method to analyze mixed data and reduce dimensionality.

### Model Training
1. Go to the **Training** tab.
2. Configure model parameters, including:
   - Learning rate.
   - Optimizer type (e.g., Adam, SGD).
   - Regularization (L1, L2).
3. Click **Fit Model** to train the multinomial logistic regression model.
4. Visualize:
   - Loss and learning rate curves.
   - Confusion matrix.
   - Model accuracy metrics.

---
