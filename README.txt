Generate confidence intervals for  a refined run:
================================================
1. Update inprocess/hana.
2. In file inprocess/hana/uncertainty/run_bm_from_file.py:
- Set the cache_directory to the folder with the refined run cache. Within cache_directory it is expected to be called run_xxx, e.g. run_215.
- Set validation_geography to either faz or large_area, depending on what kind of CIs are to be generated.
- Set pardir to the directory containing a file called bm_parameters, e.g. WORKDIR/quantiles/run213_val2010_faz. (This is supplied).
- Set output_directory to a place where the output should be written. Leave the name of the last part of the folder (runxxx_quantiles_yyy), since the R report uses it.

3. Run the file.

Generate R report:
=================
1. In the file fazreport.R:
- Set run numbers to include (runs).
- Set which of the runs has confidence intervals (ci.run)
- Set name of the 2040-refinement run (ref.run)
- For reports on faz level, set aggregate.to.large.area to FALSE, otherwise TRUE.
- Set directory with the indicators (sim.dir). This directory is expected to have sub-directories run_xxx and run_zzz where xxx are the values from 'runs' and zzz is the value of 'ref.run'. The prefix though can be changed in sim.prefix. Each of these directories should have an indicators folder with indicators computed on the corresponding geography using the Table object (see below).
- Set ci.dir to the directory with the results from running the run_bm_from_file.py file (or use the supplied ones in the quantiles folder).
- years give time points for which there are observed data, indicators and CIs.

2. Run the file using source('fazrepport.R'). It creates a pdf and a text file.


Indicators for years = [2000, 2010, 2020, 2025, 2030, 2035, 2040] (refinement run only for 2040):
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
