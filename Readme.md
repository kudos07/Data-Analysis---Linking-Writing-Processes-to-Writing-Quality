# Linking Writing Processes to Writing Quality Analysis

This repository hosts the code and analytical processes for the "Linking Writing Processes to Writing Quality" project, which was part of a Kaggle competition. The project aims to uncover the connections between how individuals write and the quality of their writing. By applying statistical analyses and machine learning techniques, we explore the intricate dynamics of writing processes.

## Project Overview

This analysis delves into the correlation between the methodologies of writing and the resultant quality, leveraging a dataset specifically curated for this purpose, as detailed in the [Kaggle competition](https://www.kaggle.com/competitions/linking-writing-processes-to-writing-quality). The project encompasses hypothesis testing, employing a Z-test for initial inquiries, followed by Multiple Linear Regression (MLR) to investigate the influence of various writing processes on writing quality.

### Hypotheses and Analysis Techniques

- **Z-Test for the First Hypothesis:** A statistical approach to explore the initial hypothesis concerning writing methodologies.
- **Multiple Linear Regression (MLR) for the Second Hypothesis:** An in-depth analysis using MLR to examine how different writing processes affect the quality of writing.

### Data Preprocessing

The analysis begins with comprehensive data preprocessing, adhering to the specifications provided in the Kaggle competition details. This phase includes managing missing values, normalizing data, and other preparatory steps crucial for subsequent analysis.

## Analysis Results

Our statistical analysis has revealed several key insights into the writing process variables that significantly impact writing quality. The bar graph below illustrates the p-values obtained for each variable under different conditions:

![P-values for Variables](Output pictures/comparisions.png)

Variables with p-values falling below the red dashed line meet the commonly accepted threshold for statistical significance (p < 0.05), suggesting a potential influence on writing quality that is unlikely to be due to chance. This visual representation aids in quickly identifying which aspects of the writing process may warrant further investigation and possible inclusion in models predicting writing quality.

## Getting Started

To get a local copy up and running, follow these simple steps:

### Prerequisites

Ensure you have Python installed on your system. This project uses Python 3.x.

### Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/YourUsername/Data-Analysis---Linking-Writing-Processes-to-Writing-Quality.git
