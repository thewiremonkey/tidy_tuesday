# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt


sq = pd.read_csv("squirrels.csv")
print(sq.head())

plt.plot(sq['long'],sq['lat'])


