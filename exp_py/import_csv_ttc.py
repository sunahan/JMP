import pandas as pd

#TTC files
file_path = r'C:\Users\kccsu\Desktop\SchoolChoice\Data\Python\ttc_open_blue.xlsx'
df = pd.read_excel(file_path) # creates a DataFrame 'df'
columns = df.columns[-6:] #  extracts the last six columns from the DataFrame "df" and assigns them to the variable "columns"
start_row = 1
end_row = 61

ttc_open_blue = pd.read_excel(file_path, skiprows=start_row-1, nrows=end_row-start_row, usecols = columns, dtype = str)
# The lambda function takes each value in the DataFrame, iterates over each character in the value, and creates a list of the characters. 
ttc_open_blue = ttc_open_blue.applymap(lambda x: [val for val in x for x in val])

# defines a lambda function named "transform_row"
# takes a row from the DataFrame and converts it into a nested list
# Each element in the list consists of a column name (converted to a string) and a list of values from that column
transform_row = lambda row: [[str(col), list(row[col])] for col in row.index[0:]]

# iterates over each row in the DataFrame "ttc_open_blue" using the iterrows() method
# For each row, it applies the "transform_row" lambda function defined earlier.
ttc_open_blue = [transform_row(row) for _, row in ttc_open_blue.iterrows()]

#print(ttc_open_blue)

