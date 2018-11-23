# humanexercise
QBI Human Exercise Analysis Packages


This analysis packages serves as a package to automatise analyse and visualisation of data downloaded from the QBI human exercise trial.
This package contains the following modules:

    init - initalises the project within a specified directory. Reads all the data files within a data directory into a local sqlite database. This MUST be run before any other functions can be used.  

    fetchdata - the main pipeline. Parses SQL statements to gather data from different experiments. Can filter out subjects, intervals and select particular fields of interest. Also reshapes bloods and computes prepost exercise differences, max value, percent_change between pre-exercise and post-exercise etc. 

    delta_change - a function that can be applied to an S4 object of "human_exercise" that is produced by fetchdata which reshapes the data into a delta change format.  


    john_the_fisherman - an "algorithm" (I use that term lightly) to cycle through all possible correlations (e.g. baseline vs. pal change, delta vs. delta change) for ALL fields from an experiment to PAL, SWM or pretty much any other experiment of interest. 


    run_grid: plotting function 



