#%%
import pandas as pd
import geopandas as gpd
import h3pandas
import h3
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np
pd.set_option('display.max_columns', None)
sns.set_theme(style="whitegrid")
# %%
path = '/Users/kylenessen/Dropbox/Programming/Rare plant literature question/GBIF Occurrence Download/0015616-210914110416597.csv'
columns = [
 'occurrenceStatus',
 'decimalLatitude',
 'decimalLongitude',
 'coordinateUncertaintyInMeters',
 'taxonKey']

hexList = list()

resolution = 5

chunksize = 10 ** 6
chunkCount = 0

with pd.read_csv(path,sep='\t',usecols=columns, chunksize=chunksize) as reader:
    for chunk in reader:
        chunk = chunk[chunk['occurrenceStatus'] == 'PRESENT']
        chunk = chunk[chunk['coordinateUncertaintyInMeters']<= 5000]
        chunk = chunk.rename({'decimalLongitude': 'lng', 'decimalLatitude': 'lat'}, axis=1)
        chunk = chunk.h3.geo_to_h3(resolution)
        chunk = chunk.reset_index()
        chunk = chunk[['h3_05','taxonKey']]
        hexList.append(chunk)
        chunkCount += 1
        print(chunkCount)

df = pd.concat(hexList)        # %%

df = df.groupby('taxonKey')['h3_05'].nunique()
df = df.reset_index()

#%%
def getArea(cells,resolution):
    area = h3.hex_area(resolution,'km^2')
    return cells * area
df['area'] = df.apply(lambda row : getArea(row['h3_05'],resolution), axis = 1)

# %%
names = pd.read_csv('/Users/kylenessen/Dropbox/Programming/Rare plant literature question/GBIF Occurrence Download/taxon_keys_all.csv')
# %%
df = df.merge(names,how='inner',left_on='taxonKey',right_on='usageKey')
# %%
lit = pd.read_csv('/Users/kylenessen/Dropbox/Programming/Rare plant literature question/Analysis Ready Results/CA-rareplant-data-20211013.csv')
# %%
df = df.merge(lit,how='inner',left_on='inputName',right_on='Scientific.Name')
# %%

columns = ['taxonKey',
 'area',
 'Scientific.Name',
 'Literature.Count',
 'Status',
 'Jepson.Link',
 'CRPR',
 'SRank',
 'CESA',
 'Species.Group',
 'NatureServe.Rounded.Global.Rank',
 'U.S..Endangered.Species.Act.Status',
 'Distribution',
 'View.on.NatureServe.Explorer',
 'Rating',
 'rarity',
 'native',
 'GenCategory']

df = df[columns]
#%%
def papers_per_km2(papers,area):
    return papers / area

df['score'] = df.apply(lambda row : papers_per_km2(row['Literature.Count'],row['area']), axis=1)

#%%
# Histogram of Papers per km^2 for plant categories
plt.figure(figsize=(9, 6))
ax = sns.histplot(df[df['score']>0],
x='score',
hue='GenCategory',
log_scale=(True,True),
multiple='stack'
)
ax.set(xlabel = 'Papers per km^2')
ax.legend_.set_title(None)


#%%

plt.figure(figsize=(9, 6))
ax = sns.barplot(x="GenCategory",
 y="score", 
 data=df,
 order = ['Rare','Neutral','Weeds'],
 **{'log':True}
 )
ax.set(xlabel = 'Plant Species Category', ylabel = 'Papers per km^2')

#%%
sns.set_style('white')
ax = sns.regplot(data=df,
x='area',
y='Literature.Count',
scatter_kws={'s':1}
)
ax.set_yscale('log')
ax.set_xscale('log')

#%%
plt.figure(figsize=(9, 6))

ax = sns.kdeplot(data=df[df['score']>0],
x='area',
y='Literature.Count',
log_scale=(True,True),
hue='GenCategory',
kind='kde',
fill=True,
**{"alpha":0.75}
)

ax.set(xlabel = 'Observed Extent (km^2)', 
ylabel = 'Count of Google Scholar Search Results',
title='Kernel Density Estimate for Plant Species')

ax.legend_.set_title(None)


#%%
plt.figure(figsize=(9, 6))

ax = sns.histplot(data=df[df['score']>0],
x='area',
y='Literature.Count',
log_scale=(True,True),
hue='GenCategory',
fill=True,
#legend=False,
#**{"alpha":0.75}
)

ax.set(xlabel = 'Observed Extent (km^2)', 
ylabel = 'Count of Google Scholar Search Results',
#title='Kernel Density Estimate for Plant Species'
)

#plt.legend(title='Category', loc='upper right', labels=['Neutral', 'Rare','Weeds'])

ax.legend_.set_title(None)


# %%

plt.figure(figsize=(9, 6))

ax = sns.scatterplot(data=df[df['score']>0],
x='area',
y='Literature.Count',
hue='GenCategory',
**{"alpha":0.5}
)

ax.set_yscale('log')
ax.set_xscale('log')
# %%
