from dateutil.relativedelta import *
import pandas as pd
import numpy as np
import statsmodels.api as sm

# custom imports
from gen_vintage import gen_target_data

### necessary files
catalog = pd.read_csv("../../../helper/catalog.csv")
data = pd.read_csv("../../../output/2020-07-01_database_tf.csv", parse_dates=["date"])
ys = ["x_world.sa", "x_vol_world2.sa", "x_servs_world.sa"]


### running DFM
start_date = "2002-01-01"
end_date = "2020-07-01"
target = "x_world.sa"

x = gen_target_data(data, catalog, target, start_date, end_date, interp_method="none", ragged_ends=True)
mod = sm.tsa.DynamicFactor(x, k_factors=1, factor_order=1, error_order=1)
initial_res = mod.fit(method='powell', disp=False)
res = mod.fit(initial_res.params, disp=False)
pred = res.predict().append(res.forecast(5))

# results
perf = pd.DataFrame(x[target][~pd.isna(x[target])]).merge(pred[target], left_index=True, right_index=True)
perf.columns = ["actual", "pred"]
mae = np.abs(perf.actual - perf.pred).mean()


### running model vintage
# # params
# start_date = "2002-01-01"
# global_end_date = "2020-06-01"
# target = "x_world.sa"
# model_num = "model_1"
    
# end_date = pd.to_datetime("2018-01-01")
# results = pd.DataFrame({"date": pd.date_range(end_date, global_end_date, freq="MS")})
# for col in ["actual", "+3", "+2", "+1", "0", "-1", "-2"]:
#     results[col] = np.nan

# while end_date <= pd.to_datetime(global_end_date):
#     print(end_date)
    
#     # model
#     x = gen_target_data(data, catalog, target, start_date, end_date, interp_method="none", ragged_ends=True)
#     mod = sm.tsa.DynamicFactor(x, k_factors=1, factor_order=1, error_order=1)
#     initial_res = mod.fit(method='powell', disp=False)
#     res = mod.fit(initial_res.params, disp=False)
#     res = mod.fit(disp=False)
#     pred = res.predict().append(res.forecast(5))
    
#     results.loc[results.date == (end_date + relativedelta(months = -1)), "+1"] = pred.loc[pred.index == (end_date + relativedelta(months = -1)), target].values[0]
#     results.loc[results.date == (end_date + relativedelta(months = -2)), "+2"] = pred.loc[pred.index == (end_date + relativedelta(months = -2)), target].values[0]
#     results.loc[results.date == (end_date + relativedelta(months = -3)), "+3"] = pred.loc[pred.index == (end_date + relativedelta(months = -3)), target].values[0]
#     results.loc[results.date == end_date, "0"] = pred.loc[pred.index == end_date, target].values[0]
#     results.loc[results.date == (end_date + relativedelta(months = +1)), "-1"] = pred.loc[pred.index == (end_date + relativedelta(months = +1)), target].values[0]
#     results.loc[results.date == (end_date + relativedelta(months = +2)), "-2"] = pred.loc[pred.index == (end_date + relativedelta(months = +2)), target].values[0]
    
#     end_date += relativedelta(months = +1)
    
# results = results.merge(data.loc[:, ["date", target]], how="left", on ="date")
# results.actual = results[target]
# results = results.drop([target], axis=1)

# results.to_csv(f"python_dfm_results/{model_num}.csv", index=False)