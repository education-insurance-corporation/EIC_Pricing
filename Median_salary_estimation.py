def Median_estimation(school):
    import pandas as pd
    import numpy as np
    import math
    from scipy.stats import lognorm
    from scipy.stats import norm
    import mysql.connector

    #School we are running
    for schools in school:
        school = schools

        #Import data
        #main_data = pd.read_csv('/Users/vigneshnatarajan/Desktop/New Python Code/data_cleaned/main_data1.csv')
        main_data = pd.read_csv('/Users/vigneshnatarajan/Desktop/New Python Code/data_cleaned/main_data2.csv')
        major_list = pd.read_csv('/Users/vigneshnatarajan/Desktop/New Python Code/data_cleaned/major_list1.csv')
        school_clustering = pd.read_csv('/Users/vigneshnatarajan/Desktop/New Python Code/data_cleaned/school_clustering1.csv')
        salary_growth = pd.read_csv('/Users/vigneshnatarajan/Desktop/New Python Code/data_cleaned/salary_growth1.csv')
        school_median_list = pd.read_csv('/Users/vigneshnatarajan/Desktop/New Python Code/school_median_list.csv')

        #Apply median and sigma calculation to main data
        def median (row):
                if row['median_salary'] != 0 :
                    return row['median_salary']
                elif row['salary_min'] != 0:
                    return np.exp((np.log(row['salary_min'])+np.log(row['salary_max']))/2)
                else:
                    return row['average_salary'] * .91

        def sigma_qt(row):
                if row['salary_min']!= 0:
                    sigma = (np.log(row['salary_max']) - np.log(row['salary_min']))/(norm.ppf(0.75, loc=0, scale=1) - norm.ppf(0.25, loc=0, scale=1))
                elif row['average_salary'] != 0 and row['median_salary'] != 0 and row['average_salary'] > row['median_salary']:
                    sigma = np.sqrt(2*(np.log(row['average_salary']) - np.log(row['median_salary'])))
                else:
                    sigma = 0
                return sigma

        main_data['median'] = main_data.apply(lambda row: median(row),axis=1)
        main_data['sigma'] = main_data.apply(lambda row: sigma_qt(row),axis=1)

        #Create sigma and salary growth data
        sigma = main_data[['major_category','sigma']]
        sigma = sigma.query('''sigma>0''')
        sigma = sigma.loc[sigma.major_category != 'all'].groupby('major_category')['sigma'].mean()
        sigma = sigma * [.434/sigma.mean()]

        growth = salary_growth[['major_category','growth_rate_2','growth_rate_3','growth_rate_4','growth_rate_5']]
        growth = growth.loc[growth['major_category'] != 'all']
        growth = growth.groupby('major_category')['growth_rate_2','growth_rate_3','growth_rate_4','growth_rate_5'].mean()
        theology_growth = pd.DataFrame({'major_category':'theology and religious vocations', 'growth_rate_2':growth.loc[growth.index =='philosophy and religious studies']['growth_rate_2'][0],
                                        'growth_rate_3':growth.loc[growth.index =='philosophy and religious studies']['growth_rate_3'][0],
                                        'growth_rate_4':growth.loc[growth.index =='philosophy and religious studies']['growth_rate_4'][0],
                                        'growth_rate_5':growth.loc[growth.index =='philosophy and religious studies']['growth_rate_5'][0]}, index=[0])

        theology_growth = theology_growth.set_index('major_category')
        growth = pd.concat([growth, theology_growth], axis=0)

        #Create Major title to Major Category mapping
        title_dict = dict(zip(major_list.major_title,major_list.majorCategoryID))
        cat_dict = dict(zip(major_list.majorCategoryID,major_list.major_category))
        major_id_title_dict = dict(zip(major_list.majorID,major_list.major_title))

        #Import completion data
        cnx = mysql.connector.connect(user='zenabi', password='hrbdrv092',
                                      host='old.eic.co',database='eic')
        cursor = cnx.cursor()
        cursor.execute("""SELECT * FROM eic.col_degree where degreeID = 3""")
        completion_data = cursor.fetchall()
        cursor.execute("""SELECT * FROM eic.col_major""")
        major_map_data = cursor.fetchall()
        cursor.execute("""SELECT col_college.colID, Institution_Name FROM eic.col_college inner join eic.gov_accred using (accredID) inner join eic.view_bachelor using (colID)""")
        college_map_data = cursor.fetchall()
        cursor.execute("""SELECT colID, enrolled_f FROM eic.det_admissions where gender = 'total' and enrolled_f > 0""")
        enrolled_data = cursor.fetchall()
        cursor.execute("""SELECT colID, yearly, comp_yr4, comp_yr6 FROM eic.col_rate where comp_yr6 > comp_yr4""")
        grad_data = cursor.fetchall()

        completion_data = pd.DataFrame(completion_data, columns=[1,2,3,4,5,6,7,8,9,10,11])
        major_map_data = pd.DataFrame(major_map_data, columns=[5,6,7,8])
        college_map_data = pd.DataFrame(college_map_data, columns=['colID','school_name'])
        enrolled_data = pd.DataFrame(enrolled_data, columns=['colID','enrolled_full'])
        grad_data = pd.DataFrame(grad_data, columns=['colID','year','grad_4','grad_6'])

        #Create enrolled column names and get rid of commas
        enrolled_data.enrolled_full = enrolled_data.enrolled_full.str.replace(",", "")
        enrolled_data.enrolled_full = enrolled_data.enrolled_full.astype(float)

        #Merge major and completion data and rename columns
        completion_major_data = pd.merge(completion_data[[3,4]],major_map_data[[5,6,7]],how='inner',
                                         left_on=3,right_on=5)
        completion_major_data = completion_major_data.drop([3,5], axis=1)
        completion_major_data.columns = ['completion','major','colID']

        grad_rate_colID = grad_data.groupby('colID').agg({'year':'max'})
        grad_data = pd.merge(grad_data,grad_rate_colID.reset_index(),how='inner',on=['colID','year'])
        school_completion = pd.merge(completion_major_data,grad_data[['colID','grad_4','grad_6']],how='inner',
                                    on='colID')
        school_completion.columns = ['completion','majorID','colID','grad_4','grad_6']
        school_completion.majorID = school_completion.majorID.map(major_id_title_dict)
        school_completion.majorID = school_completion.majorID.map(title_dict)
        school_completion.majorID = school_completion.majorID.map(cat_dict)

        colID_value_dict = dict(zip(college_map_data.school_name.str.lower(),college_map_data.colID))
        colID = colID_value_dict[school]

        #Subset data to match schools we have collegescorecard medians for
        school_median_list.Name = school_median_list.Name.str.lower()

        #School completion by school
        school_completion_by_school = school_completion.loc[school_completion.colID == colID]
        school_completion_by_school = school_completion_by_school.groupby('majorID').agg({'completion':'sum','grad_4':'mean','grad_6':'mean'})

        school_longx = school_clustering.loc[school_clustering['school_name'] == school, 'long_x']
        school_laty = school_clustering.loc[school_clustering['school_name'] == school, 'lat_y']
        school_clustering['distance'] = school_clustering.apply(lambda x: math.sqrt((school_longx-x['long_x'])**2 +
                                                                                    (school_laty-x['lat_y'])**2),axis=1)
        #Get schools based off school median range
        target_school_median = school_median_list.loc[school_median_list.Name == school, 'median_yr6']
        upper_range = (target_school_median * 1.2).values[0] #Can change to a fixed whole number instead of percentage
        lower_range = (target_school_median * .8).values[0] #Can change to a fixed whole number instead of percentage
        school_median_list = school_median_list.loc[(school_median_list.median_yr6 <= upper_range) & (school_median_list.median_yr6 >= lower_range)]

        #Subset main data based off the schools we get from school median range
        main_data = pd.merge(main_data,school_median_list[['Name']],left_on='school_name_matched',right_on='Name')
        main_data = pd.merge(main_data,school_clustering[['school_name','distance']],left_on='school_name_matched',right_on='school_name')
        main_data = main_data[['state','school_name_matched','major_category','median','source','distance']]
        main_data = main_data.loc[main_data.major_category != 'all'].groupby(['major_category','school_name_matched'])['median','distance'].mean()
        main_data = main_data.sort_values(by=['distance','median'],ascending=True).sort_index(level='major_category',sort_remaining=False)

        #Create Salary Estimation
        major_category_list = []
        mean_list = []
        schools = []
        for x in main_data.index.get_level_values('major_category').unique():
            #Get the school Median prediction by major
            major_category_list.append(x)
            false_median_value_list = []

            while True:
                above_median = []
                median_data = main_data.loc[main_data.index.get_level_values(0) == x]['median']
                median_data = median_data.loc[~median_data.isin(false_median_value_list)]
                median_data_initial = median_data[0:4]

                for y in median_data_initial:
                    if len(median_data_initial) <= 2:
                        above_median.append('True')
                    else:
                        median_data_mean = median_data_initial.loc[median_data_initial != y].mean()
                        if ((y < 1.5*median_data_mean) & (y > .5*median_data_mean)):
                            above_median.append('True')
                        else:
                            above_median.append('False')

                to_concat = [median_data_initial,pd.Series(above_median)]
                median_data2 = pd.DataFrame(pd.concat([i.reset_index(drop = True) for i in to_concat],axis=1))

                if median_data2[0].str.contains('False').any():
                    index_value = median_data2.loc[median_data2[0] == 'False'].last_valid_index()
                    false_median_value_list.append(median_data2.loc[median_data2.index == index_value]['median'].values[0])

                if len(median_data2[median_data2[0] == 'True']) == 4:
                    break
                elif len(median_data_initial) < 4:
                    break

            #Get similar school values
            similar_schools = pd.Series(median_data_initial.values,index=median_data_initial.index.get_level_values(1)).to_dict()
            schools.append(similar_schools)
            mean = median_data2['median'].mean()
            mean_list.append(mean)

        Output_estimate = pd.DataFrame({'Major_Category':major_category_list,'Year 1':mean_list,'Similar_schools':schools})
        Output_estimate = pd.merge(Output_estimate,growth,how='inner',left_on='Major_Category',right_index=True)
        Output_estimate = pd.merge(Output_estimate,sigma.to_frame(),how='inner',left_on='Major_Category',right_index=True)
        Output_estimate = pd.merge(Output_estimate,school_completion_by_school,how='inner',left_on='Major_Category',right_index=True)
        Output_estimate = Output_estimate.reset_index(drop='index')

        #Create csv format for R input
        if colID in list(enrolled_data.colID):
            student_size = enrolled_data.loc[enrolled_data.colID == colID]['enrolled_full'].values[0]
        else:
            student_size = 1000

        total_completion = Output_estimate.completion.sum()
        Output_estimate['Enrolled'] = round((Output_estimate.completion/total_completion)*student_size)
        Output_estimate['median_2019'] = ((Output_estimate['Year 1'])/1000).apply(np.floor).astype(int)*1000
        Output_estimate['grad_3rd'] = .15
        Output_estimate['grad_4th'] = (Output_estimate['grad_4']/Output_estimate['grad_6'])-.15
        Output_estimate['grad_5th'] = (2/3)*(1-(Output_estimate['grad_4']/Output_estimate['grad_6']))
        Output_estimate['grad_6th'] = (1/3)*(1-(Output_estimate['grad_4']/Output_estimate['grad_6']))
        if (math.floor(target_school_median.values[0]/1000)*1000) - 5000 > 35000:
            Output_estimate['Base_Wage'] = 35000/1.03
        elif (math.floor(target_school_median.values[0]/1000)*1000) - 5000 > 10000:
            Output_estimate['Base_Wage'] = ((math.floor(target_school_median.values[0]/1000)*1000) - 5000)/1.03
        else:
            Output_estimate['Base_Wage'] = 10000/1.03

        for x in range(4):
            Output_estimate.loc[len(Output_estimate)] = 0

        Output_estimate = Output_estimate.shift(periods=4)
        Output_estimate.iloc[3] = Output_estimate.columns
        Output_estimate.iloc[0,0] = school
        Output_estimate.iloc[1,0] = student_size
        Output_estimate.iloc[2,0] = 'Grad rate 4th year: {}, Grad rate 6th year: {}, School median: {}'.format(Output_estimate['grad_4'].values[4],Output_estimate['grad_6'].values[4], target_school_median.values[0])
        Median_estimation.Output_estimate1 = Output_estimate
        Median_estimation.Output_estimate_csvformat = Output_estimate[['Major_Category','completion','Enrolled','grad_6','Year 1','growth_rate_2','growth_rate_3','growth_rate_4','growth_rate_5','sigma','grad_3rd','grad_4th','grad_5th','grad_6th','Base_Wage','median_2019']]

        Output_file = '/Users/vigneshnatarajan/Desktop/North Dakota schools/'+school+'.csv'
        Output_test = '/Users/vigneshnatarajan/Desktop/North Dakota schools/'+school+'1'+'.csv'
        Median_estimation.Output_estimate_csvformat.to_csv(Output_file, index=False, header=False)
        Median_estimation.Output_estimate1.to_csv(Output_test,index=False, header=False)

Median_estimation(['university of memphis'])

#import pandas as pd

#school_clust = pd.read_csv('/Users/vigneshnatarajan/Desktop/New Python Code/data_cleaned/school_clustering1.csv')
#med_val = pd.read_csv('/Users/vigneshnatarajan/Desktop/New Python Code/school_median_list.csv')
#med_val.Name = med_val.Name.str.lower()

#matches = pd.merge(school_clust,med_val[['Name']],left_on='school_name',right_on='Name')
#matches = matches.drop_duplicates()
#matches.loc[matches.state == 'new mexico']['school_name'].unique()
    #east west university 18700
