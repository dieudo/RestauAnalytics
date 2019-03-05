#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Mar  4 21:24:11 2019

@author: dieudonneouedraogo
"""
import pandas as pd
import seaborn as sns
import numpy as np
import warnings
import matplotlib.pyplot as plt
warnings.filterwarnings('ignore')
#Load the data
my_data=pd.read_csv("DOHMH_New_York_City_Restaurant_Inspection_Results.csv")
my_data.shape
for i in my_data.columns:
    print(i, 'has type:',my_data[i].dtype,'and null values equal to:',my_data[i].isnull().sum())

my_data = my_data.dropna(subset=['ACTION','INSPECTION TYPE'], how='all')
my_data = my_data.reset_index(drop=True)
my_data.shape

code_map = {
    '02':'FOOD TEMPERATURE',
    '03':'FOOD SOURCE',
    '04':'PERSONAL HYGIENE/FOOD PROTECTION',
    '05':'FACILITY DESIGN',
    '06':'PERSONAL HYGIENE/FOOD PROTECTION',
    '08':'VERMIN/GARBAGE',
    '09':'FACILITY MAINTENANCE',
    '10':'FACILITY DESIGN',
    '07':'OTHER',
    '20':'LETTER GRADE CARD/OTHER POSTER ABSENT',
    '16':'MENU DESCRIPTION IMPROPER',
    '22':'OTHER',
    '15':'NO SMOKING POLICY',
    '18':'OTHER',
    'na':'NONE' 
}  

# Function that maps the P and Z grade to correct A, B or C grade
def grades_change(y):
    grade_list = ['A','B','C']
    if (y['GRADE'] in grade_list):
        return y['GRADE']
    elif (y['GRADE']=='P')| (y['GRADE']=='Z'):
        if (y['SCORE']>=0) & (y['SCORE']<=13):
            return 'A'
        elif (y['SCORE']>=14) & (y['SCORE']<=27):
            return 'B'
        else:
            return 'C'
        
        
my_data['GRADE'] = my_data.apply(grades_change,axis=1)
# After converting the grades to their correct class let's just check the final counts of them
my_data['GRADE'].value_counts()

my_data.info()

my_data['CAMIS'].nunique()

cus = []
cus_n = []
for i in my_data["CUISINE DESCRIPTION"].unique():
    cus.append(i)
    cus_n.append(my_data[my_data["CUISINE DESCRIPTION"]==i]["CAMIS"].nunique())
my_data_cus = { 'Cuisine Description':cus,'Number':cus_n}
my_data_cus = pd.DataFrame(my_data_cus)
my_data_cus.plot(x="Cuisine Description", y=["Number"], kind="bar",figsize=(22,8))
plt.title('Number of restaurants distribuition as per Cuisine type')
plt.ylabel('Number of Restaurants')

my_data.groupby(['BORO','CAMIS']).BORO.count().plot(kind='bar')
my_data2 = my_data.groupby(['BORO', 'GRADE'])['BORO'].count().unstack('GRADE').fillna(0)
my_data2[['A','B','C']].plot(kind='bar', stacked=True)
plt.ylabel('Number of Grades')
plt.show()

sns.set(rc={'figure.figsize':(10,8)})
sns.boxplot(x='BORO',y='SCORE',data=my_data)

my_data_closed = my_data[my_data['ACTION'].str.contains("Establishment Closed|Establishment re-closed")]
print ('Total Number of times Restaurants closed by the Health Inspectors are',my_data_closed['ACTION'].count())

my_data_cct = my_data_closed.groupby(['CUISINE DESCRIPTION'],as_index=False)['CAMIS'].count()
my_data_cct.rename(columns={'CAMIS': 'Count'}, inplace=True)
my_data_cct.sort_values(by='Count', ascending=False, inplace=True)
my_data_cct.reset_index(drop=True,inplace=True)
my_data_cct.plot.bar(x='CUISINE DESCRIPTION', y='Count',figsize=(22,8))

#Now let us check the distribution of Violation Types
df_v = my_data.groupby(['VIOLATION CODE'],as_index=False)['CAMIS'].count()
df_v.rename(columns={'CAMIS': 'Count'}, inplace=True)
df_v.sort_values(by='Count', ascending=False, inplace=True)
df_v.reset_index(drop=True,inplace=True)
df_v.head(20).plot.bar(x='VIOLATION CODE', y='Count')
plt.ylabel('Number of Violations')
plt.show()






