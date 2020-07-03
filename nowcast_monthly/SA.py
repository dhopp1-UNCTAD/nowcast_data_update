import pandas as pd
from statsmodels.tsa.x13 import x13_arima_analysis

data = pd.read_excel("output_20200616_monthly.xlsx", sheet_name="wto data")

for country in ["Singapore", "Hong Kong, China", "Chinese Taipei", "Thailand", "Malaysia", "Viet Nam", "Romania"]:
    tmp = data.loc[data["Reporting Economy"] == country, ["date", "Value"]].set_index("date").sort_index()
    sa = x13_arima_analysis(tmp, x12path="/home/danhopp/x13as/x13as").seasadj
    if country == "Singapore":
        sas = pd.DataFrame({"date":sa.index, "country":country, "value":sa})
    else:
        sas = sas.append(pd.DataFrame({"date":sa.index, "country":country, "value":sa}))
    
sas.to_csv("wto_sa.csv", index=False)