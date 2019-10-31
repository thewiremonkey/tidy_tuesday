# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import pandas as pd
import numpy as np

sq = pd.read_csv("squirrels.csv")
print(sq)

sq_long = np.mean(sq['long'])
print(sq_long)