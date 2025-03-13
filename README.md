# Instructions to setup app

### Start by archiving the old data: Move files 

-   from data/current to data/old and 
-   from out/current to out/old/

### Main challenges:

1)  File layouts may? differ year to year (e.g. column names)
2)  Inconsistent naming of the industries between sources.

### Required files: (files with these EXACT names need to be placed in data/current)

1)  historic.xlsx (LFS data prepared for stokes: it has the correct names). Requirements:

-   has a sheet called "British Columbia".
-   the first 3 rows contain meta data (skipped), and all other rows contain valid data. MAKE SURE NO GARBAGE AT END
-   has exactly one column that contain the string "code"
-   has exactly one column that contains the string "industry"
-   has many columns that start with the number "2" i.e. wide format

![](images/historic.png)

2) old.xlsx (employment from 4CastExporter).  Requirements:

-   that first 3 rows contain meta data (skipped), and all other rows contain valid data. MAKE SURE NO GARBAGE AT END   
-   contains a column called "Industry"
-   has many columns that start with the number "2" i.e. wide format

![](images/old.png)

3) driver.xlsx (the driver data).  Requirements:

-   first row contains column names, and all other rows contain valid data. MAKE SURE NO GARBAGE AT END   
-   contains a column called "Ind Des"
-   has many columns that start with the number "2" i.e. wide format

![](images/driver.png)


4) notes.xlsx (the notes from previous years).  Requirements:

-  first row contains column names, and all other rows contain valid data. MAKE SURE NO GARBAGE AT END
-  there is exactly one column that contains the string "name" (the industry names)
-  there are several columns that start with the number "2" i.e. wide format

![](images/notes.png)

5) constraint.xlsx (the finance forecast).  Requirements:

-  first row contains column names, and all other rows contain valid data. MAKE SURE NO GARBAGE AT END
-  contains two columns, year and employment i.e. long format

![](images/constraint.png)

### Run the code:

1)  Either sourcing the file 00_source_me.R, or run through it line by line (control+enter)
-   this creates the proportion forecasts 

2)  In the file 01_run_document.Rmd increment the historic start and end by 1.  
-   Do a preliminary run through the first code chunk of file 01_run_document.Rmd (control+enter) looking for errors.

3)  Run document 01_run_document.Rmd 

4)  Do your forecasting.

5)  After the forecasting is complete, in the file 02_source_me.R increment the historic start and end by 1.  
-   Either source the file 02_source_me.R, or run through it line by line (control+enter)

6)  Forecast can be found in folder out/current

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/new_industry_forecast/issues/).

### How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

### License

```         
Copyright 2023 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
```

------------------------------------------------------------------------

*This project was created using the [bcgovr](https://github.com/bcgov/bcgovr) package.*
