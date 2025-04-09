## Calanda Community

General structure of the code

1. Data Cleaning & Community State Creation

The script reads in your raw CSV file, converts date strings to proper Date objects, and aggregates data by day to produce community state strings (a unique combination of species present on that day). This is useful for visualizing community transitions.

2. Converting Daily Presence Data to Presence Intervals

	•	Read daily presence/absence data.
	•	Create a binary indicator (Yes/No) per species per day.
	•	Group consecutive days of presence into intervals by using a trick based on the difference between dates and row numbers.
	•	Output a summary for each species with the first presence, last presence, and total number of days present.

3. Converting Abundance Data to Binary Data

Then converts abundance measurements (e.g., number of flowers) to binary data by setting a threshold (usually greater than 0 indicates presence). This binary format is needed for current temporal network analyses.

4. Subsetting Large Datasets

We extract a manageable subset from a very large dataset (data from only 5 cameras and 50 species) so that you can test the analyses and validate your workflow before scaling up.

5. Fitting Custom Phenophase Curves

Next, we use custom likelihood function to fit a phenophase curve via MLE. The model uses the inverse logit of a quadratic function of day-of-year, parameterized by:
	•	peakp: the day of peak phenophase,
	•	rangep: a parameter controlling the width (should be negative for a concave-down curve),
	•	maxp: an offset on the logit scale.

The script fits the model with optim() and plots the observed data along with the fitted curve.

## Usage 
```bash
git clone https://github.com/ajijohn/CalandaCommunity.git
cd CalandaCommunity
```

## Colloboration 

V.R from Rice University 
