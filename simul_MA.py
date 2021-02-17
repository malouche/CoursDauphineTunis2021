import statsmodels.api as sm

import numpy as np

np.random.seed(12345)


## X_t=Z_t+.65 Z_{t-1}

arparams = np.array([0])
maparams = np.array([.65])  ## X_t=Z_t+.65 Z_{t-1}
maparams
ma = np.r_[1, maparams]
ar = np.r_[1, -arparams] # add zero-lag and negate

arma_process = sm.tsa.ArmaProcess(ar, ma)
arma_process
arma_process.isstationary
arma_process.isinvertible



y = arma_process.generate_sample(250)
y


###   X_t=.45 X_{t-1}+Z_t un AR(1)


arparams = np.array([0.35])
maparams = np.array([0])

ma = np.r_[1, maparams]
ar = np.r_[1, -arparams] # add zero-lag and negate

arma_process = sm.tsa.ArmaProcess(ar, ma)
arma_process
arma_process.isstationary
arma_process.isinvertible

y = arma_process.generate_sample(250)
y
