# Brown_Phillips_Semi-Automated_Biosensor_Manufacturing_2024

## **Code for Brown and Phillips et. al. “Semi-automated production of cell-free biosensors”**

This repo contains python code and data files for analysis used in Brown and Philips et. al. “Semi-automated production of cell-free biosensors”

1.	`Figure_1_Curve_Fitting.ipynb` contains code for determining the time to half maximum signal, half maximum signal, and maximum signal of the data contained in `Figure_1_Raw_Data_LacZ.xlsx` and `Figure_1_Raw_Data_sfGFP.xlsx` for their corresponding reporters. 
2.	`Figure_2_Analysis.ipynb` contains code for analyzing the time to half max signal, half max signal, and max signal of cell-free reactions for an entire 384 well plate. Then, this notebook will classify the outputs into H, M, L values corresponding to the interleaved signal format used in Supplemental Figure 3. Following this the notebook is used to generate swarm and violin plots, and then calculate the fraction of reactions with values above the maximum value of L and the fraction of reactions with values above 0.5 uM FITC signal, as well as the expected/predicted outcomes of this experimental set. 
3.	`Figure_1_Sensor_Build.json` provides the Opentrons code for replicating  the experiments carried out in Figure 1 in Brown and Philips et. al. “Semi-automated production of cell-free biosensors”.
4.	`Figure_2_Sensor_Build.py` provides the Opentrons code for manufacturing 96 up to1056 biosensor reactions in 96-well plates.

## **Downloading Files**
Files can be downloaded manually or obtained by:
   
    git clone https://github.com/LucksLab/Brown_Phillips_Semi-Automated_Biosensor_Manufacturing_2024.git

## **System Requirements**
The python packages required for a Python 3 Jupyter Notebook are as follows

    import pandas as pd
    import numpy as np
    import matplotlib.pyplot as plt
    import seaborn as sns

For Opentrons protocols, download Opentrons software from https://opentrons.com/ot-app/

## **Code Usage**

### *Figure 1 Data*
For `Figure_1_Curve_Fitting.ipynb`

Excel files `Figure_1_Raw_Data_LacZ.xlsx` and `Figure_1_Raw_Data_sfGFP.xlsx` are used for `Figure_1_Curve_Fitting.ipynb` and should be downloaded. 

Change the path found in the Jupyter notebook to the path of xlsx files

Running this notebook should generate output files `Figure_1_Output_Half_Max_Times_LacZ.xlsx`, `Figure_1_Output_Half_Max_Times_sfGFP.xlsx`, `Figure_1_Output_Max_Signals_LacZ.xlsx`, `Figure_1_Output_Max_Signals_sfGFP.xlsx` in the same path as the input excel files. 

Violin plots are then generated from these excel files and saved into the folder path as svg files. 

### *Figure 2 Data*
For `Figure_2_Analysis.ipynb`

Download the files and change the path of of `Figure_2_FRS_Compiled_Data.xlsx` to the local path on your computer as well as output paths for the consecutive excel worksheets generated during analysis.

`Figure_2_FRS_Compiled_Data.xlsx` is the raw data used in analysis for this section. Running the code should create multiple output excel files from this one including the half maximum signal, time to half maximum signal, and maximum signal values output as `Figure_2_Output_FRS_Compiled_Data.xlsx`

From that output, the notebook will then classify the data as the H (H=High), M (M=Medium), or L(L=Low), conditions from an interleaved signal format structure for a 384 well plate. This will create a new excel spreadsheet with data called `Figure_2_Updated_Output_FRS_Compiled_Data.xlsx`

This data is then used to generate swarm and violin plots, which will save as svg files from the provided code. 

Finally, the data from `Figure_2_Updated_Output_FRS_Compiled_Data.xlsx` is used to determine the fractions of reactions that met expected conditions vs. the actual experimental meeting of this criteria described in the notebook. These values were then manually plotted in the manuscript. 

Note depending on modifications to the experimental conditions, the interleaved signal classification, as well as the performance criteria must be changed in the notebook to meet experimenters conditions and plate layouts. 

### *Opentrons Protocols*

Opentrons protocols `Figure_1_Sensor_Build.json` and `Figure_2_Sensor_Build.py` can be uploaded into the Opentrons app, which will them provide information on deck set up and allow for the protocols to be ran. 

