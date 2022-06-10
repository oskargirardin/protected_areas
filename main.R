# IMport libraries
library(readxl)



# Import raw data
setwd("C:/Users/Oskar/Documents/DSBA/Semester 3/Environmental Economics/Project/protected_areas")
WDPA_April_raw = read_excel("./WDPA.xlsx", sheet = "WDPA_April_raw")
GHG_OECD_raw = read_excel("./WDPA.xlsx", sheet = "GHG_OECD_raw")
PA_coverage_OWD_raw = read_excel("./WDPA.xlsx", sheet = "PA_coverage_OWD_raw")
GDP_growth_WorldBank_raw = read_excel("./WDPA.xlsx", sheet = "GDP_growth_WorldBank_raw")

data_analysis_2017 = PA_coverage_OWD_raw[PA_coverage_OWD_raw$Year == 2017,]
data_analysis_2017 = na.omit(data_analysis_2017)

co2_2017 = GHG_OECD_raw[(GHG_OECD_raw$TIME == 2017 & GHG_OECD_raw$SUBJECT == "CO2" & GHG_OECD_raw$MEASURE == "TONNE_CAP"),c(1,7)]

data_analysis_2017 = cbind(data_analysis_2017, rep(0,length(data_analysis_2017$Code)))
colnames(data_analysis_2017)[c(4,5)] = c("pa_coverage","co2_per_capita")

for (i in 1:length(data_analysis_2017$Code)){
  code = data_analysis_2017$Code[i]
  idx = match(code, co2_2017$LOCATION)
  data_analysis_2017[i,5] = co2_2017[idx,2]
}

data_analysis_2017 = cbind(data_analysis_2017, rep(0,length(data_analysis_2017$Code)))
colnames(data_analysis_2017)[6] = "gdp_growth"

for (i in 1:length(data_analysis_2017$Code)){
  code = data_analysis_2017$Code[i]
  idx = match(code, GDP_growth_WorldBank_raw$`World Development Indicators`)
  data_analysis_2017[i,6] = GDP_growth_WorldBank_raw[idx,62]
}

data_analysis_2017$pa_coverage = as.numeric(data_analysis_2017$pa_coverage)
reg_gdp = lm(gdp_growth ~ pa_coverage, data = data_analysis_2017)
reg_co2 = lm(co2_per_capita ~ pa_coverage, data = data_analysis_2017)

