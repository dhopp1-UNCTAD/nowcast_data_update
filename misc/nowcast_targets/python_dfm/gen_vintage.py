import numpy as np
import pandas as pd

def offset_series(series, offset):
    tmp = series
    tmp[-offset:] = np.nan
    return tmp
    
def gen_vintage(data, catalog, start_date, end_date):
    tmp = data.loc[lambda x: (x.date >= start_date) & (x.date <= end_date), :]
    offsets = [
        catalog.loc[catalog.code == x,"publication_lag"].values[0]
            if len(catalog.loc[catalog.code == x,"publication_lag"]) > 0 
            else catalog.loc[catalog.code == x[:-3],"publication_lag"].values[0]
        for x in data.columns[1:]
    ]
    for i in range(1, len(tmp.columns)):
        tmp.iloc[:,i] = offset_series(tmp.iloc[:,i], offsets[i-1])
    return tmp

def interpolate(data, method):
    if method == "none":
        return data
    elif method == "linear":
        return data.interpolate()
    elif method == "mean":
        return data.fillna(data.mean())

def gen_target_data(data, catalog, target, start_date, end_date, interp_method="none", ragged_ends=True):
    vintage = gen_vintage(data, catalog, start_date, end_date)
    if target == "x_world.sa":
        catalog_col = "octave_value"
    elif target == "x_vol_world2.sa":
        catalog_col = "octave_volume"
    elif target == "x_servs_world.sa":
        catalog_col = "octave_services"
    tmp = vintage.loc[:, pd.unique(["date"] + catalog.loc[~pd.isna(catalog[catalog_col]), catalog_col].to_list())]
    tmp = interpolate(tmp, interp_method)
    if ragged_ends:
        tmp = gen_vintage(tmp, catalog, start_date, end_date).set_index("date").dropna(how="all")
    else:
        tmp = tmp.set_index("date").dropna(how="all")
    return tmp.loc[tmp.index > "2002-01-01",:]