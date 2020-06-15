# Time series analysis for the PM10 pollution particle

This project was made as a task during the study of Time Series Analysis, which is a class of the statistics program in the Statistics Bachelor Course of Federal University of Uberlândia (UFU - MG/Brazil).

The teacher provided each student a particular database and told us to adjust a possible ARIMA/SARIMA model to suit the data. The model had to follow the proper statistics concepts learn during the course such as residual properties that these kinds (ARIMA/SARIMA) of model demand.

This is my solution to the project. Hope you guys like it.

### About the data

The [data](https://github.com/nascimento1195/time-series-analysis-PM10/blob/master/basepoluicao.xls) that was given to me is about the concentration of a pollution particle named 'PM10' in the air of a city (not mentioned by the teacher) in the full year of 1997 (each day had a line in the database).

### Prerequisites

To run the [code](https://github.com/nascimento1195/time-series-analysis-PM10/blob/master/timeseriesanalysis.R) you will need a default R environment with the following libraries installed:

* library(readxl)
* library(ggplot2)
* library(tseries)

### Observations

All the code is commented, so all my steps towards the development of the model are explained there.

## Author

* [Matheus Julio Nascimento](https://www.linkedin.com/in/matheus-nascimento-88b8b0168/)
