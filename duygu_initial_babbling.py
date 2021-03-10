import pandas as pd
import numpy as np
import scipy as sp

import matplotlib.pyplot as plt
import seaborn as sns

import datetime
import scipy.fftpack

sns.set_style("whitegrid")

data = pd.read_csv('/Users/duygu/github/gretta-grettas/noaa_newark_data.csv')
data.head()

pol_data = pd.read_csv('/Users/duygu/github/gretta-grettas/ad_viz_plotval_data.csv')
pol_data.head()
pol_data['date'] = pd.to_datetime(pol_data['Date'])
pol_data = pol_data.sort_values(['date'])
pol_data['no2'] = pol_data['Daily Max 1-hour NO2 Concentration']
pol_data.no2.head()

df = pd.DataFrame(data)
df['date'] = pd.to_datetime(df[['year', 'mo', 'da']].astype(str).agg('-'.join, axis=1))
df = df.sort_values(['date'])

CUTOFFDATE = '2019-12-31'
train = df[df['date'] <= CUTOFFDATE]
test = df[df['date'] > CUTOFFDATE]

train.temp.head()
N = len(train.temp)
print(N)

plt.figure(figsize=(20, 3), dpi=70)
plt.plot(train.date, train.temp)
plt.xlabel('Date')
plt.ylabel('Mean temperature')
temp_fft = sp.fftpack.fft(train['temp'].astype(int)) # get the fft
temp_psd = np.abs(temp_fft) ** 2 # get the power spectral density
fftfreq = sp.fftpack.fftfreq(len(temp_psd), 1. / 365)
i = fftfreq > 0

pol_fft = sp.fftpack.fft(pol_data['no2'].astype(int))
pol_psd = np.abs(pol_fft) ** 2
pol_freq = sp.fftpack.fftfreq(len(pol_psd), 1./365)
j = pol_freq > 0

fig, ax = plt.subplots(1, 1, figsize=(8, 4))
ax.plot(fftfreq[i], 10 * np.log10(temp_psd[i]))
ax.set_xlim(0, 5)
ax.set_xlabel('Frequency (1/year)')
ax.set_ylabel('PSD (dB)')
temp_fft_bis = temp_fft.copy()
temp_fft_bis[np.abs(fftfreq) > 1.1] = 0

pol_fft_bis = pol_fft.copy()
pol_fft_bis[np.abs(pol_freq) > 1.1] = 0

pol_slow = np.real(sp.fftpack.ifft(pol_fft_bis))
temp_slow = np.real(sp.fftpack.ifft(temp_fft_bis))
fig,ax = plt.subplots(1,1,figsize=(20,3))
ax.plot(train.date, train.temp,color='blue')
ax.plot(test.date, test.temp, color='blue',alpha=.6)
ax.set_xlabel('Date', fontsize=14)
ax.set_ylabel('Mean temperature',color='blue',fontsize=14)
ax.plot_date(train.date, temp_slow, 'r-')
ax.set_xlim(datetime.date(2011, 1, 1),
        datetime.date(2021, 1, 1))
ax2 = ax.twinx()
ax2.plot(pol_data['date'], pol_data['Daily Max 1-hour NO2 Concentration'], color ='orange', alpha=.6)
ax2.plot(pol_data['date'],pol_slow,color = 'orange')
ax2.set_ylabel('Max NO2 Concentration (ppb)', color='orange', fontsize=14)
