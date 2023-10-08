import pandas as pd

#DA files
file_path = r'C:\Users\kccsu\Desktop\SchoolChoice\Data\strategy_py\da_open_green.xlsx'
df = pd.read_excel(file_path)
column = ["7", "2", "3", "4", "5", "6"]  
start_row = 1
end_row = 61

da_open_green = pd.read_excel(file_path, skiprows=start_row-1, nrows=end_row-start_row, usecols = column, dtype = str) #cannot extract columns in the correct order
da_open_green = da_open_green[["7", "2", "3", "4", "5", "6"] ] # reorder based on the correct order
col_names = ["1", "2", "3", "4", "5", "6"] # rename columns
print(da_open_green)
da_open_green.columns = col_names

da_open_green = da_open_green.applymap(lambda x: [val for val in x for x in val])

transform_row = lambda row: [[str(col), list(row[col])] for col in row.index[0:]]
da_open_green = [transform_row(row) for _, row in da_open_green.iterrows()]

for i in da_open_green: # need to add counter '0' for each student
    for j in i:
        j.append(0)

print(da_open_green)


