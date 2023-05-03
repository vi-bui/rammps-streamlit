# Import packages
import rpy2.robjects as robjects # run r in python
from rpy2.robjects import pandas2ri # pull dataframe from r into python
import streamlit as st
import pandas as pd
import subprocess
from PIL import Image
import plotnine 
from plotnine import *
from plotnine.data import mtcars
import textwrap # import textwrap and define the custom function
import numpy as np
import matplotlib.pyplot as plt

# Configures the default settings of the page.
st.set_page_config(layout="wide")

# Write arguments to the app.
st.write("# RaMMPS Results 👋")

# Create the sidebar
# Add description to sidebar
st.sidebar.success("Select a page above to view different results.")

# Label size bar
with st.sidebar:
    st.subheader("Select country")
    
# Use caching to load the data quickly after running cleaning script
@st.cache(suppress_st_warning=True)
def load_data():
  pandas2ri.activate() 
  print(robjects.r)
  r = robjects.r
  r['source']('testscript.R') # R script which creates df to be used for plots
  with robjects.default_converter + pandas2ri.converter:
    age_sex_mw = robjects.conversion.get_conversion().rpy2py(robjects.r['age_sex_mw']) # pull age_sex_mw df from testscript.R into python
    age_sex_df = robjects.conversion.get_conversion().rpy2py(robjects.r['age_sex_df'])
  return age_sex_mw, age_sex_df # returns the pulled dfs

# Load cached data
age_sex_mw, age_sex_df  = load_data()

# Split df into two columns
col1, col2 = st.columns(2)

# process = subprocess.Popen(["Rscript", "plot.R"], stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
# result = process.communicate()
# image1 = Image.open('trial.png')
# image1 = image1.resize((300, 400))
# st.image(image1)
# st.caption('**Figure 1.** A simple scatter plot of *wt* as a function of *mpg* from the mtcars dataset.')


# process2 = subprocess.Popen(["Rscript", "testscript.R"], stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
# print(process2)
# result2 = process2.communicate()
# print(result2)
# image = Image.open('plot.png')
# # image = image.resize((300, 400))
# st.image(image)
# st.caption('**Figure 1.** A simple scatter plot of *wt* as a function of *mpg* from the mtcars dataset.')



# p = ggplot(mtcars, aes("wt", "mpg")) + geom_point()
# st.pyplot(ggplot.draw(p))

# Write header for dashboard
st.header("Response Rates")
# Description of dashboard
st.write(
        """ This page shows the results of the response outcomes in Burkina Faso, the Democratic Republic of the Congo and Malawi. """)

# First column
with col1:
 
  #################################
  ## Calls Plots
  #################################
  st.subheader('Call Outcomes')
  #read in data
  calls = pd.read_csv("data/calls.csv")

  country_list_calls = list(calls['group'].unique()) # Get list of unique countries
  country_calls = st.sidebar.selectbox(label = "Call Outcomes", options = country_list_calls) # create selectbox

  query_country = f"group=='{country_calls}'"
  countries_filtered = calls.query(query_country)

  group_order = ['COMP', 'INCO', 'REFU', 'DEFER', 'NNA/NR', 'NNU', 'Other Eligibility Unknown', 'Other Ineligible'] # order the x-axis bars

  # function for limiting characters on x-axis
  def wraping_func(text):
      return [textwrap.fill(wraped_text, 15) for wraped_text in text]

  # create plot
  calls_plot = (ggplot(countries_filtered, aes(x='Outcome2', y = 'percperoutcome', fill='Eligibility')) +
    geom_bar(stat="identity") +
    labs(x="",y="Percent of phone calls") +
    theme_bw() +
    scale_y_continuous(limits = [0, 80]) +
    scale_x_discrete( labels=wraping_func, limits=group_order)) # limits the character length of x-axis and order of bars
  st.pyplot(ggplot.draw(calls_plot))

  #################################
  ## CATI Plots
  #################################
  st.subheader('CATI Outcomes')
  #read in data
  catis = pd.read_csv("data/catis.csv")

  country_list_catis = list(catis['group'].unique()) # Get list of unique countries
  country_catis = st.sidebar.selectbox(label = "CATI Outcomes", options = country_list_catis) # create selectbox

  query_country_catis = f"group=='{country_catis}'"
  countries_filtered_catis = catis.query(query_country_catis)

  group_order_catis = ['COMP', 'PART', 'REFU', 'NNA/NR', 'NNU', 'DEFER', 'PEND', 'INEL', 'Other Eligibility Unknown'] # order the x-axis bars

  # create plot
  calls_plot = (ggplot(countries_filtered_catis, aes(x='Outcome.FINAL', y = 'percperoutcome', fill='Eligibility')) +
    geom_bar(stat="identity") +
    labs(x="",y="Percent of phone calls") +
    theme_bw() +
    scale_y_continuous(limits = [0, 80]) +
    scale_x_discrete( labels=wraping_func, limits=group_order_catis)) # limits the character length of x-axis and order of bars
  st.pyplot(ggplot.draw(calls_plot))

# Second column
with col2:
  ##################################
  ## Call attempt order plots
  ##################################
  st.subheader('Call Attempts')
  #read in data
  outcomes_rdd = pd.read_csv("data/outcomes_rdd.csv")

  country_list_attempt = list(outcomes_rdd['country'].unique()) # Get list of unique countries
  country_attempt = st.sidebar.selectbox(label = "Call Attemps", options = country_list_attempt) # create selectbox

  query_country_attempt= f"country=='{country_attempt}'"
  countries_filtered_catis = outcomes_rdd.query(query_country_attempt)

  ## Number of phone calls vs call attempt order
  # creating figure using matplotlib as require dual axes
  fig, ax = plt.subplots(figsize = (15,12))
  sec_axis = ax.twinx() # create secondary axis
  countries_filtered_catis.plot(x = 'call_num_grp', y = ['perccum'], kind = 'line', ax = ax, color='red', legend=False) # culumative percentage
  countries_filtered_catis.plot(x = 'call_num_grp', y= ['Calls'], kind = 'bar', ax = sec_axis,color='blue', legend=False) # calls 
  ax.set_ylabel('Cumulative percentage of phone #s\n with completed CATI', fontsize=25)
  ax.set_xlabel('Call attempt')
  sec_axis.set_ylabel('Number of Phone Calls')
  plt.rcParams['font.size'] = 25
  # ax.legend(['Cumulative percentage of phone #s\n with completed CATI'])
  # sec_axis.legend(['Number of Phone Calls'])
  legend_labels = ['Cumulative\n percentage', 'Calls']
  ax.figure.legend(labels=legend_labels, loc='center right', bbox_to_anchor=(1.25, 0.5))
  # fig.subplots_adjust(right=0.80)
  sec_axis.set_ylim([0, 50000])
  st.pyplot(fig)


  ##################################
  ## Response Rates tables
  ##################################
  st.subheader('Response Rates')
  # initialize data of lists
  resp_rates = {"DRC": ["11952", "135968", "11.4", "25.0", "72.73 (66.86 – 80.03)", "17.80 (16.36 – 19.58)", "51%"],
                "MW": ["6796", "74088", "10.6", "22.9", "52.42 (50.26 – 63.29)", "19.95 (19.13 – 24.09)", "80%"],
                "BF": ["8729", "52916", "6.1", "22.8", "39.69 (37.64 – 44.4)", "8.62 (8.17 – 9.64)", "66%"]}

  # create df
  resp_rates_df = pd.DataFrame(resp_rates, index= [
                                "CATIS Completed",
                                "Calls Placed",
                                "Call attempts per completed CATI",
                                "% of called numbers with a completed CATI",
                                "Response rate",
                                "Refusal rate ",
                                "Estimated percent eligible (e)"])

  st.dataframe(resp_rates_df)

  # #### Age and Sex plot
  # st.subheader('Age and Sex plot')
  # country_list = list(age_sex_df['country'].unique())
  # country = st.sidebar.selectbox(label = "Age and Sex plot", options = country_list)

  # query = f"country=='{country}'"
  # df_filtered = age_sex_df.query(query)
  # print(df_filtered)

  # age_sex_mw_plot = (ggplot(df_filtered, aes(x='Resp.Age.pyr', y = 'relfreq', fill='Resp.Sex')) +
  #   geom_bar(stat = "identity", position = "dodge2") + 
  #   scale_fill_manual(values = ['#D2042D','#0671B7']) +
  #   ylab('Relative Frequency of Respondents (%)') + xlab('Respondent Age') +
  #   scale_x_discrete(labels = ['15-19','20-29','30-39','40-49','50-59','60-64']) +
  #   theme_bw() +
  #   labs(fill='Respondent Sex') +
  #   coord_cartesian(ylim = [0,50])) 
  # st.pyplot(ggplot.draw(age_sex_mw_plot))
