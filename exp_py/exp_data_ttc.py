import pandas as pd

file_path = r'C:\Users\kccsu\Desktop\SchoolChoice\Data\strategy_py\ttc_open_blue.xlsx'
df = pd.read_excel(file_path) 
column = ["1", "2", "8", "4", "5", "6"] 
start_row = 1
end_row = 61

ttc_open_blue = pd.read_excel(file_path, skiprows=start_row-1, nrows=end_row-start_row, usecols = column, dtype = str) #cannot extract columns in the correct order
ttc_open_blue = ttc_open_blue[["1", "2", "8", "4", "5", "6"]] # reorder based on the correct order
col_names = ["1", "2", "3", "4", "5", "6"] # rename columns
ttc_open_blue.columns = col_names

ttc_open_blue = ttc_open_blue.applymap(lambda x: [val for val in x for x in val])

transform_row = lambda row: [[str(col), list(row[col])] for col in row.index[0:]]
ttc_open_blue = [transform_row(row) for _, row in ttc_open_blue.iterrows()]

'''#Blue
A_list = ['A', 2, 0, ['1','2','3','4','5','6'], ['1','2','3','4','5','6']] #[school code, seat capacities for open seats, seat capacities for reserved seats, [priority orders of open seats], [priority orders of reserved seats]]
B_list = ['B', 1, 1, ['3','5','6','1','2','4'], ['5','6','3','1','2','4']]
C_list = ['C', 1, 1, ['5','1','6','3','2','4'], ['5','6','1','3','2','4']]
'''

'''#Green
full_students = [['1', ['B','C','A']], ['2', ['B','C','A']], ['3', ['C','B','A']], ['4', ['B','C','A']], ['5', ['C','B','A']], ['6', ['C','B','A']]]
A_list = ['A', 2, 0, ['1','2','3','4','5','6'], ['1','2','3','4','5','6']]
B_list = ['B', 1, 1, ['3','1','2','4','5','6'], ['5','6','3','1','2','4']]
C_list = ['C', 1, 1, ['6','3','5','1','2','4'], ['6','5','3','1','2','4']]
'''

##############################################################################
#So far, schools and students have the format needed to run the TTC algorithm
#print(schools)                                                              
#print(full_students)
#print(A_list)
#print(student_code)                                                               
##############################################################################
def TTC(data_list):
  #Blue
  student_code = ['1','2','3','4','5','6']
  A_list = ['A', 2, 0, ['1','2','3','4','5','6'], ['1','2','3','4','5','6']]
  B_list = ['B', 1, 1, ['3','5','6','1','2','4'], ['5','6','3','1','2','4']]
  C_list = ['C', 1, 1, ['5','1','6','3','2','4'], ['5','6','1','3','2','4']]
  schools = [A_list, B_list, C_list]
  openfirst = "1" # Schools fill open seats first then reserved seats; "0" for filling reserved seats first then open seats

  #Index indicators used to find remaining seat capacity, priority list and candidate list based on the current Respect (O/R)
  A_seat_left = 0
  A_priority_order = 0
  B_seat_left = 0
  B_priority_order = 0
  C_seat_left = 0
  C_priority_order = 0

  #initialize index indicators for n schools based on openfirst and capacity
  if openfirst == "1":
    if A_list[1] > 0:
      A_seat_left = 1
      A_priority_order = 3
    else:
      A_seat_left = 2
      A_priority_order = 4
    if B_list[1] > 0:
      B_seat_left = 1
      B_priority_order = 3
    else:
      B_seat_left = 2
      B_priority_order = 4
    if C_list[1] > 0:
      C_seat_left = 1
      C_priority_order = 3
    else:
      C_seat_left = 2
      C_priority_order = 4
  elif openfirst == "0":
    if A_list[2] > 0:
      A_seat_left = 2
      A_priority_order = 4
    else:
      A_seat_left = 1
      A_priority_order = 3
    if B_list[2] > 0:
      B_seat_left = 2
      B_priority_order = 4
    else:
      B_seat_left = 1
      B_priority_order = 3
    if C_list[2] > 0:
      C_seat_left = 2
      C_priority_order = 4
    else:
      C_seat_left = 1
      C_priority_order = 3
  else:
    raise TypeError("Respect has to be either O or R")

  # At the beginning, all students are unassigned
  unassigned_students = data_list.copy()
  # test variable to check the collected cycles
  cycle_counter = 1
  cycle_len = 1
  final_assignment = []
  # We loop until there are no more unassigned students
  # Format [[id1, [preference list ranked from most preferred to least preferred]], [id2, [preference list]]...]
  # Format ["School", Openseat, Reserveseat, [Open priority list], [Reserve priority list]]
  while len(unassigned_students) > 0:
    cur_std_prf = []
    cur_sch_priority = []
    save_all_connections = []
    cycles_found = []
    #For each student, create list of student preferences by pairing and appending
    # [Student id, preferred school] to the cur_std_prf
    for std in unassigned_students:
      cur_std_prf.append([std[0], std[1][0]])
    #print(cur_std_prf)
    # For each school create list of school preferences by pairing and appending
    # [school id, preferred student] to the cur_sch_priority
    for sch in schools:
      if sch[0] == 'A':
        cur_sch_priority.append([sch[0], sch[A_priority_order][0]])
      elif sch[0] == 'B':
        cur_sch_priority.append([sch[0], sch[B_priority_order][0]])
      elif sch[0] == 'C':
        cur_sch_priority.append([sch[0], sch[C_priority_order][0]])
      else:
        raise TypeError("School has to be A, B or C")
    #print(cur_sch_priority)
    #We now pair each item of the created lists if student's preferred school in cur_std_prf
    #matches the school id in cur_sch_priority. Append found pair to save_all_connections
    #ex) [[Student id, **preferred school**], [**school id**, preferred student]]
    for i in cur_std_prf:
      for j in cur_sch_priority:
        if i[1] == j[0]:
          save_all_connections.append([i, j])
      #print(save_all_connections)
    #For the first round, we skip this one and work with cycles = 1. We come into this loop only if we cannot find a cycle of length 1 and add more cycles in below loop
    if cycle_len > 1:
      #tmp is incremented by 1 every loop at the end
      tmp = 1
      #while tmp is less than asked cycle length, add more nodes to at the end of each items in save_all_connections
      while tmp < cycle_len:
        tmp_std = []
        tmp_sch = []
        #for each connection saved in save_all_connections,
        #find the student that matches the preferred student at the end (first for loop) and store it in tmp_std
        #find the matching preferred school for tmp_std and save it to tmp_sch (second for loop)
        for i in save_all_connections:
          for j in cur_std_prf:
            if j[0] == i[-1][1]:
              tmp_std = j
          #print(tmp_std)
          for k in cur_sch_priority:
            if tmp_std[-1] == k[0]:
              tmp_sch = k
          #print(tmp_sch)
            #append tmp_std and tmp_sch to the end of the individual connection
            #index(x) 함수는 리스트에 x 값이 있으면 x의 인덱스 값을 리턴한다.
          save_all_connections[save_all_connections.index(i)].append(tmp_std)
          save_all_connections[save_all_connections.index(i)].append(tmp_sch)
          #print(save_all_connections)
        tmp = tmp + 1
      #print(save_all_connections)
  #for all connections collected, check the beginning and end. Matching beginning/end indicates a cycle.
    for k in save_all_connections:
      if k[0][0] == k[-1][-1]:
        cycles_found.append(k)

    #print(cycles_found)
    if cycles_found:
      remove_seats = []
      for c in cycles_found:
        #elem in cycle is each step in cycle, format is [1,A]
        for elem in c:
          temp_elem = elem.copy()
          if temp_elem[-1] not in remove_seats:
            #add school to remove_seats
            remove_seats.append(temp_elem[-1])
          #final_assignment is what we want in the end. It tells us where students are assigned to and what seat types they are getting; open for 'O' and reserved for 'R'
          if temp_elem[0] in student_code and temp_elem not in final_assignment:
            final_assignment.append(temp_elem)
            final_assignment.sort()
            if temp_elem[-1] == 'A' and A_list[A_seat_left] > 0 and A_seat_left == 1:
              final_assignment[final_assignment.index(temp_elem)].append('O')
            elif temp_elem[-1] == 'A' and A_list[A_seat_left] > 0 and A_seat_left == 2:
              final_assignment[final_assignment.index(temp_elem)].append('R')
            elif temp_elem[-1] == 'B' and B_list[B_seat_left] > 0 and B_seat_left == 1:
              final_assignment[final_assignment.index(temp_elem)].append('O')
            elif temp_elem[-1] == 'B' and B_list[B_seat_left] > 0 and B_seat_left == 2:
              final_assignment[final_assignment.index(temp_elem)].append('R')
            elif temp_elem[-1] == 'C' and C_list[C_seat_left] > 0 and C_seat_left == 1:
              final_assignment[final_assignment.index(temp_elem)].append('O')
            elif temp_elem[-1] == 'C' and C_list[C_seat_left] > 0 and C_seat_left == 2:
              final_assignment[final_assignment.index(temp_elem)].append('R')
        #print(final_assignment)
        #for each item in remove seats, if item is one of 1~6 (student)
        #remove that student from all school preference lists
      for item in remove_seats:
        if item in student_code: 
          for school in schools:
            if item in school[3]:
              school[3].remove(item)
            if item in school[4]:
              school[4].remove(item)
          #print(schools)
          for std in unassigned_students:
            if std[0] == item:
              unassigned_students.remove(std)
        #if school, reduce capacity or remove school if both capacity reaches 0
        #do this for each school
        elif item in ['A', 'B', 'C']:
          if item == 'A':
            #If there are remaining spots in current O/R, subtract 1
            if A_list[A_seat_left] > 0:
              A_list[A_seat_left] -= 1
              #If there are no more remaining spots after the subtraction in line 205, switch O/R R/O
              if A_list[A_seat_left] == 0 and A_seat_left == 1:  # no available seat at open seat
                A_seat_left = 2  # remaining seats are at reserve seats
                A_priority_order = 4  # reserve priority
              elif A_list[A_seat_left] == 0 and A_seat_left == 2:  # no available seat at reserve seat
                A_seat_left = 1  # remaining seats are at open seats
                A_priority_order = 3  # open priority
          #Edge case: if open/reserve seats were already run out, we spend the reserve/open seats
            elif A_list[1] > 0 or A_list[2] > 0:
              if A_seat_left == 1:
                A_seat_left = 2
                A_priority_order = 4
                A_list[A_seat_left] -= 1
              elif A_seat_left == 2:
                A_seat_left = 1
                A_priority_order = 3
                A_list[A_seat_left] -= 1
          #If there are no more remaining spots for both O/R after processing, remove school
            if A_list[1] == 0 and A_list[2] == 0:
                for s in unassigned_students:
                    if item in s[1]:
                        s[1].remove(item)
                schools.remove(A_list)
          elif item == 'B':
            if B_list[B_seat_left] > 0:
              B_list[B_seat_left] -= 1
              if B_list[B_seat_left] == 0 and B_seat_left == 1:
                B_seat_left = 2
                B_priority_order = 4
              elif B_list[B_seat_left] == 0 and B_seat_left == 2:
                B_seat_left = 1
                B_priority_order = 3
            elif B_list[1] > 0 or B_list[2] > 0:
              if B_seat_left == 1:
                B_seat_left = 2
                B_priority_order = 4
                B_list[B_seat_left] -= 1
              elif B_seat_left == 2:
                B_seat_left = 1
                B_priority_order = 3
                B_list[B_seat_left] -= 1
            if B_list[1] == 0 and B_list[2] == 0:
              for s in unassigned_students:
                if item in s[1]:
                  s[1].remove(item)
              schools.remove(B_list)
          elif item == 'C':
            if C_list[C_seat_left] > 0:
              C_list[C_seat_left] -= 1
              if C_list[C_seat_left] == 0 and C_seat_left == 1:
                C_seat_left = 2
                C_priority_order = 4
              elif C_list[C_seat_left] == 0 and C_seat_left == 2:
                C_seat_left = 1
                C_priority_order = 3
            elif C_list[1] > 0 or C_list[2] > 0:
              if C_seat_left == 1:
                C_seat_left = 2
                C_priority_order = 4
                C_list[C_seat_left] = C_list[C_seat_left] - 1
              elif C_seat_left == 2:
                C_seat_left = 1
                C_priority_order = 3
                C_list[C_seat_left] = C_list[C_seat_left] - 1
            if C_list[1] == 0 and C_list[2] == 0:
              for s in unassigned_students:
                if item in s[1]:
                  s[1].remove(item)
              schools.remove(C_list)
          else:
            raise TypeError("Item value has to be one of A B C")
        else:
          raise TypeError("Item value has to be one of 1 to 6 or A to C")
      cycle_len = 1
      cycle_counter += 1
    else:
      cycle_len += 1
      cycle_counter += 1
  return final_assignment

def runTTC():
  outcomes = []

  for big in ttc_open_blue:
    full_students = big
    outcome = TTC(full_students)
    # outcomes = [[['1','B','O'], ['2','A','O'], ... , ['6','C','O']], [[], [],..,[]], ..., [[], [],..,[]]]
    outcomes.append(outcome)

# Create a list to collect the rows of data
  data_rows = []

# Iterate over each nested list in outcomes
  for nested_list in outcomes:
    # Use a set to store unique tuples while preserving the order
    unique_tuples = set()
    unique_list = []

    # Iterate over each list in the nested list
    for lst in nested_list:
      # Convert the list to a tuple
      tpl = tuple(lst)
      # Check if the tuple is unique
      if tpl not in unique_tuples:
        # Add the tuple to the set and unique list
        unique_tuples.add(tpl)
        unique_list.append(lst)

    # Create a dictionary for the current nested list
    data = {}

    # create two separate lists, schools and seats, 
    # by extracting the second and third elements from each sublist in the unique_list
    schools = [lst[1] for lst in unique_list]
    seats = [lst[2] for lst in unique_list]

    # This for loop iterates over the schools and seats lists simultaneously using the zip() function
    for i, (school, seat) in enumerate(zip(schools, seats), start=1):
      data[f's_assigned_{i}'] = school
      data[f'seat_{i}'] = seat

    # Append the dictionary to the list of data rows
    data_rows.append(data)

  # Create the DataFrame from the list of data rows
  df = pd.DataFrame(data_rows)

  # includes only the columns where the column name does not start with the string 'Student'
  cols = [col for col in df.columns if not col.startswith('Student')]
  df = df[cols]

  #df = pd.DataFrame(outcomes)
  file_directory = r'C:\Users\kccsu\Desktop\SchoolChoice\Data\strategy_py'
  file_name = 'ttc_open_blue_3.xlsx'
  output_file = file_directory + '\\ttc_open_blue_3.xlsx'
  df.to_excel(output_file, index=False)
 
  #print(outcomes)

runTTC()

