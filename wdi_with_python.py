### Visit the website: https://pypi.org/project/wbdata/ to install wbdata library 
import wbdata
### Data sources 
wbdata.get_source()
### Indicators in the source 1
wbdata.get_indicator(source=1)
### Search countries by their ISO3C code
wbdata.search_countries("tun")
### Search for indicators
wbdata.search_indicators('gdp per capita')

### Extract some data with given dates. 
### import time library.
import time
from datetime import date
date.today()

### Extracting data for a given indicator
## Date range
data_date = (date(1960, 1, 1), date(2017, 1, 1))
data_date

### Extracting Data
### NY.GDP.PCAP.PP.KD.ZG        GDP per capita, PPP annual growth (%)
dd=wbdata.get_data(indicator='NY.GDP.PCAP.KD.ZG' , data_date=data_date,country='tun')
dd
### importing panda package
import pandas as pd
### Transforming dd as a data.frame 
dd=pd.DataFrame(dd)
dd.head()

### Exporting the data into a csv file
dd.to_csv("wdi_data.csv")

### Importing the data from the csv file
dd=pd.read_csv("wdi_data.csv")
dd.head()
dd.shape
dd.columns
dd.date
dd=dd[['date','value']]
dd=dd[1:51]
dd
### Creating date range object
rng = pd.date_range('1/1/1967', '1/1/2017', freq='y')

### Creating a ts object
ts = pd.Series(dd.value.values, index=rng)

### Visualization with matplotlib

from matplotlib import pyplot
pyplot.figure(figsize=(10.5, 5.5))
pyplot.plot(ts)
pyplot.grid()
pyplot.suptitle("Growth in GDP")
pyplot.title("Tunisia 1960-2017")
pyplot.xlabel("Year")
pyplot.ylabel("(%)")
pyplot.show()
