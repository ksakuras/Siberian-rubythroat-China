
# before analysis, open the "preparation" script and load all the libraries
# I have one R script file for each logger. In each script, I rename the variables with a suffix, e.g.,"pam.5D6"

# For 2022 Qinghai data set: jump directly to the read.trainset to start
# For new data:
# 1. copy pressure, temperature, actoscore (sum and independent) into one sheet
# 2. a new csv file, adapting the pressure, temp, actoscore into the trainset format - timestamp use excel format to YYYY-MM-DD"T"HH:MM:SS".000Z
# 3. use trainset website to process
# 4. put a copy of the csv file in 0_PAM folder
# 5. use the code to transform it to be readable, write_trainset()
# 6. manually copy the label (change them from label_1 to "1") to the auto-generated file, rename as "xx-labeled"
# 7. read_trainset and continue
