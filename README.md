# Event study

This project estimates the Abnormal Returns ($AR$) over a time window ranging from -5 to +5 days and calculates the Cumulative Abnormal Returns ($CAR$) for three different periods: from -5 to -1 days, from -5 to +5 days, and from 0 to +5 days. The estimation window consists of 200 days, and the event window spans 11 days. The code also performs significance tests on both daily abnormal returns ($AR_t$) and cumulative abnormal returns ("CAR_t$). The code is written entirelly in R. Following there is a brief description of each file.

### Events.xlsx
This file provides the structure required for analyzing corporate events.

### 1.Load_data.R
This script imports the necessary data for the analysis.

### 2.Function.R
This script defines the functions used to calculate $AR$ and $CAR$, as well as to conduct the associated significance tests.

### 3.Output_generation.R
This script applies the functions to the imported data to generate the final outputs.
