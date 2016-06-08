# Weighted KNN for air pollution prediction
Two models are built
* Weighted KNN with feature vector = [time, longitude and latitude]
* Weighted KNN adding weather factors, with feature vector = [time, longitude, latitude, Temperature, Pressure, Humidity]

You can refer to `KNN.html` to see the building and results of the models.

## Data
The original data comes from Center for Geographic Analysis Dataverse of Harvard University, including hourly air pollution data(PM2.5, PM10, O3,NO2, SO2, CO) and weather data(Temperature, DewPoint, Pressure, Humidity, Wind) of 1372 monitor locations in China.

Download [here](http://aqi.cga.harvard.edu/china/about/).

**Reference**: `ChinaMap, 2014, "China AQI, Monthly Data Archive", http://dx.doi.org/10.7910/DVN/24826, Harvard Dataverse, V12`.


