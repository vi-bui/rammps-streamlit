import pandas as pd
import matplotlib.pyplot as plt

outcomes_rdd = pd.read_csv("data/outcomes_rdd.csv")

# ax = outcomes_rdd.plot(x='call_num_grp', y='perccum', color='blue', legend=False)
# sec_axis = ax.twinx()
# outcomes_rdd.plot(x='call_num_grp', y='Calls', ax=sec_axis, color='red', legend=False)
# ax.figure.legend()
# plt.show()


fig, ax = plt.subplots(figsize = (15,8))
sec_axis = ax.twinx()
outcomes_rdd.plot(x = 'call_num_grp', y = ['perccum'], kind = 'line', ax = ax, legend=False)
outcomes_rdd.plot(x = 'call_num_grp', y= ['Calls'], kind = 'bar', ax = sec_axis, legend=False)
plt.show()