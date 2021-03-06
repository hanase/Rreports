Rreports
========

Scripts and data for creating internal R reports.


###Generate confidence intervals for  a refined run:

1. In the Opus repository, update inprocess/hana.
2. In file inprocess/hana/uncertainty/run_bm_from_file.py:
    - Set the cache_directory to the folder with the refined run cache. Within cache_directory it is expected to be called run_xxx, e.g. run_215.
    - Set validation_geography to either faz or large_area, depending on what kind of CIs are to be generated.
    - Set pardir to the directory containing a file called bm_parameters, e.g. WORKDIR/quantiles/run213_val2010_faz. (This is supplied).
    - Set output_directory to a place where the output should be written. Leave the name of the last part of the folder (runxxx_quantiles_yyy), since the R report uses it.
3. Run the file.

###Generate R report:

1. In the file fazreportMR.R, cityreportMR.R or tractreport.R:
    - Set run numbers to be included (`runs`).
    - Set which of the runs has confidence intervals (`ci.run`).
    - Set names of the refinement runs (`ref.run`). These runs will be shown as dots.
    - Object `ref.names` contains strings used as names of the refined runs in the tables.
    - Object `refining` makes the connection of the refined runs to the unrefined runs.
    - Set the desired color of the dots (`ref.cols`)
    - Objects `ref.run`, `ref.names`, `refining`, and `ref.cols` have to be of the same length.
    - For large area reports use fazreportMR.R and set `aggregate.to.large.area` to TRUE.
    - If using your own indicators, set directory with the indicators (`sim.dir`). This directory is expected to have sub-directories run_xxx and run_zzz where xxx are the values from `runs` and zzz is the value of `ref.run`. The prefix though can be changed in `sim.prefix`. Each of these directories should have a folder called 'indicators' with indicators computed on the corresponding geography using the Table object (see below). The 'runs' folder contains default indicators.
    - If using your own CIs, set `ci.dir` to the directory with the results from running the run_bm_from_file.py file. The quantiles folder contains default CIs.
    - Object `years` contains time points for which there are observed data, indicators and CIs.
    - Object `years.for.table` contains time points shown in the tables.
    - Object `years.for.refinement` contains time points for each refined run.
    - If `show.all.refined.years` is FALSE, only dots are shown that differ from the corresponding non-refined run more than 10%. 
2. Run the file in R using e.g. `source('cityreportMR.R')`. It creates a pdf and a text file.


The following indicators should be run for years = [2000, 2010, 2020, 2025, 2030, 2035, 2040] and in case of refinement runs for years = [2020, 2030, 2040]:

```
    Table(
         attribute = 'households=faz.aggregate(urbansim_parcel.building.number_of_households, intermediates=[parcel, zone])',
         dataset_name = 'faz',
         output_type = 'tab',
         source_data = source_data,
         ),
    Table(
         attribute = 'jobs=faz.aggregate(urbansim_parcel.building.number_of_jobs, intermediates=[parcel, zone])',
         dataset_name = 'faz',
         output_type = 'tab',
         source_data = source_data,
         ),
    Table(
         attribute = 'population=faz.aggregate(urbansim_parcel.building.population, intermediates=[parcel, zone])',
         dataset_name = 'faz',
         output_type = 'tab',
         source_data = source_data,
         ),
    Table(
        attribute = 'population=large_area.aggregate(urbansim_parcel.building.population, intermediates=[parcel, zone, faz])',
        dataset_name = 'large_area',
        source_data = source_data,
        ),
    Table(
        attribute = 'households=large_area.aggregate(urbansim_parcel.building.number_of_households, intermediates=[parcel, zone, faz])',
        dataset_name = 'large_area',
        source_data = source_data,
        ),
    Table(
        attribute = 'jobs=large_area.aggregate(urbansim_parcel.building.number_of_jobs, intermediates=[parcel, zone, faz])',
        dataset_name = 'large_area',
        source_data = source_data,
        ),
    Table(
        attribute = 'population=city.aggregate(urbansim_parcel.parcel.population)',
        dataset_name = 'city',
        source_data = source_data,
        output_type = 'tab',
        ),
    Table(
        attribute = 'households=city.aggregate(urbansim_parcel.parcel.number_of_households)',
        dataset_name = 'city',
        source_data = source_data,
        output_type = 'tab',
        ),
    Table(
        attribute = 'jobs=city.aggregate(urbansim_parcel.parcel.number_of_jobs)',
        dataset_name = 'city',
        source_data = source_data,
        output_type = 'tab',
        ),
    Table(
        attribute = 'population=tract10.aggregate(urbansim_parcel.parcel.population)',
        dataset_name = 'tract10',
        source_data = source_data,
        output_type = 'tab',
        ),
    Table(
        attribute = 'households=tract10.aggregate(urbansim_parcel.parcel.number_of_households)',
        dataset_name = 'tract10',
        source_data = source_data,
        output_type = 'tab',
        ),
    Table(
        attribute = 'jobs=tract10.aggregate(urbansim_parcel.parcel.number_of_jobs)',
        dataset_name = 'tract10',
        source_data = source_data,
        output_type = 'tab',
        ),
```

