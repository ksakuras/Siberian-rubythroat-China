Siberian rubythroat pressure analysis

LOGGER TYPE: CAnMove multi-sensor loggers (developed by CAnMove lab from Biology department, Lund University)

DATA TYPE: 

- Barometric data and temperature data (per hour, Aug 15 - Nov 20, 2021, Mar 15 - May 20, 2022)
- Accelerometric data (per hour, Aug 15, 2021 - May 20, 2022)
- Light-level data (per 5 min, Dec 21 - 31, 2021)

SPECIES: Siberian rubythroat (Calliope calliope)

LOCATIONS: 1. Datong county, Qinghai, China; 2. Mirnoye, Russia

PERIOD: 2021 (deployment) to 2022 (re-collection)

DESCRIPTION: 

# before analysis, open the "preparation" script and load all the libraries
# All Qinghai data should be able to run from 99-combined, and simply change the gdl <- "LOGGERNAME"
# Prior to the analysis, a quick Visualizing of final results (pressure, static and wind) can be run; if not finishing the analysis, you won't have anything to load here, just skip 
# If you run independent R files by steps, in 1-Pressure please run from beginning to enable to trainset_read() function, as it requires a pre-defined pam file and a .csv file for the logger you are running the analysis
# PLEASE beaware that 56C dataset only contains autumn data, and it no longer has the winter gap of pressure. Thus, in 5-2_windgraph_create section, please unmask those that labels with "56C only" and not run those 





# For new data:
# 1. copy pressure, temperature, actoscore (sum and independent) into one sheet
# 2. a new csv file, adapting the pressure, temp, actoscore into the trainset format - timestamp use excel format to YYYY-MM-DD"T"HH:MM:SS".000Z
# 3. use trainset website to process
# 4. put a copy of the csv file in 0_PAM folder
# 5. use the code to transform it to be readable, write_trainset()
# 6. manually copy the label (change them from label_1 to "1") to the auto-generated file, rename as "xx-labeled"
# 7. read_trainset and continue
