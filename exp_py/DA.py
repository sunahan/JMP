import random

#Green
student_code = ['1','2','3','4','5','6']
full_students = [['1', ['B','C','A'], 0], ['2', ['B','C','A'], 0], ['3', ['C','B','A'], 0], ['4', ['B','C','A'], 0], ['5', ['C','B','A'], 0], ['6', ['C','B','A'], 0]]
A_list = ['A', 2, 0, ['1','2','3','4','5','6'], ['1','2','3','4','5','6'], [], [], []]
B_list = ['B', 1, 1, ['3','1','2','4','5','6'], ['5','6','3','1','2','4'], [], [], []]
C_list = ['C', 1, 1, ['6','3','5','1','2','4'], ['6','5','3','1','2','4'], [], [], []]
schools = [A_list, B_list, C_list]


def DA():
  openfirst = "0"
  #Index indicators
  seat_left = 0
  priority_order = 0
  temp_held_students = 0
  unassigned_students = full_students.copy()
  unwanted_students = []

  while len(unassigned_students) > 0:
    #Students apply to school and added to the school's application list at [7]
    #students' round number [2] gets incremented by 1
    applicants_a = []
    applicants_b = []
    applicants_c = []

    #Use round_counter as index for current matching school; see above format for student
    for st in unassigned_students:
      #st[2] 0 at start; our index is 0 at the beginning
      indx = st[2]
      #st[1][indx] finds the exact school code the student is currently applying at this round ex st[1][0] == 1st element in [A, C, B] == 'A'
      #if student wants to go to school A, add that student to applicant_a list; and same for the other schools
      if st[1][indx] == 'A':
        applicants_a.append(st)
      elif st[1][indx] == 'B':
        applicants_b.append(st)
      elif st[1][indx] == 'C':
        applicants_c.append(st)
      else:
        raise TypeError("School not properly selected; contact admin")
      #We need to increment the round counter after adding school so that on the next round we pick the next school on the list    
      st[2] = st[2] + 1
    #add those applicants to each school's [applicants at this round list], which is at index [7]
    A_list[7] = applicants_a.copy()
    B_list[7] = applicants_b.copy()
    C_list[7] = applicants_c.copy()

    #We empty the unassigned students, as all students should be applied to at least 1 school by now
    #Also, we need fresh list to save rejected students AFTER the selection process to do the while loop again line 85 
    unassigned_students = []
    rejected_students = []
    
    #Each school in schools now has list of applicants at index 7 [7]
    #For each school, we go through selection process to pick the highest ranked student according to the priority list
    for sch2 in schools:
      if openfirst == "1":
        seat_left = 1
        priority_order = 3
        temp_held_students = 5
      elif openfirst == "0":
        seat_left = 2
        priority_order = 4
        temp_held_students = 6
      else:
        raise TypeError("Respect has to be either O or R")

      #to hold sorted applicants according to the current priority list
      sorted_students = []
      #Total applicant list adds current applicants + Open applicants + Reserve applicants into one list
      total_applicants = sch2[7] + sch2[5] + sch2[6]

      #if the school has no seats for the current Respect setup, (O/R)
      #no need to sort the list as all will be rejected; sorted list == total applicant list
      #in this case, for loop below will be skipped, as sch2[priority_order] will be nothing (empty)
      if len(sch2[priority_order]) == 0 or sch2[seat_left] == 0:
        sorted_students = total_applicants.copy()
      else:
      #this naturally adds students to the sorted_std_list in the priority order
        for student_code in sch2[priority_order]:
          for applicant in total_applicants:
            if student_code == applicant[0]:
              sorted_students.append(applicant)
      
      # qualified students are top "number of spots available" in that school
      # sorted_students[:sch2[seat_left]] returns sorted_students chopped at the available seat n
      # Therefore those students can be added to the candidates at the current school, sch2[temp_held_students]
      qualified_students = sorted_students[:sch2[seat_left]].copy()
      sch2[temp_held_students] = qualified_students.copy()                
      #Remainder of the students, sorted_students[sch2[seat_left]:] are saved
      rejected_students = sorted_students[sch2[seat_left]:].copy()            

      #Before we release unwanted students into unassigned, we need to evaluate
      #if the school has remaining seat for the counterside R/O
      #For O->R, as we considered Open spots in the previous loop, we will now change indexes to indicate RESERVE
      #unwanted students will now be reviewed for the reserve spots, if Respect='O' and vise versa
      if openfirst == "1":
        seat_left = 2
        priority_order = 4
        temp_held_students = 6
      elif openfirst == "0":
        seat_left = 1
        priority_order = 3
        temp_held_students = 5

      total_applicants2 = rejected_students.copy()
      sorted_students2 = []
      rejected_students = []

      # When school does not have open spots for current setup, sorted lists are just total applicant list
      # As they will all be rejected regardless
      if len(sch2[priority_order]) == 0 or sch2[seat_left] == 0:
        sorted_students2 = total_applicants2.copy()

      # will return preference list of Reserve, if started with Open Respect and vise versa
      for student_code in sch2[priority_order]:
        for applicant2 in total_applicants2:
          if student_code == applicant2[0]:
            sorted_students2.append(applicant2)

      qualified_students = sorted_students2[:sch2[seat_left]]
      sch2[temp_held_students] = qualified_students.copy()
      rejected_students = sorted_students2[sch2[seat_left]:].copy()

      #Students in the rejected_students are the final rejections that needs to be re-interated through the loop
      #if student has spent all his round number (ustd[2] == 3) we take student out to the unwanted_students list (error case)
      #if student still has rounds remaining, add back to unassigned students
      for ustd in rejected_students:
        if ustd[2] == 3 and ustd not in unwanted_students:
          unwanted_students.append(ustd)                        
        if ustd[2] < 3 and ustd not in unassigned_students:
          unassigned_students.append(ustd)
      #Empty the current school's applicant list as new applicants needs to be assigned back at the top of the loop
      sch2[7] = []
    
  #if there is a finished student after this loop it means some students were not matched to school;
  #this is an error case; please report back
  if len(unwanted_students) > 0:
    raise TypeError("Unmatched student : " + str(unwanted_students) + ". Report back to admin")
  
  #After going through the loop, the school now has locked candidates for
  #Open spots at index [5] and Reserve spots at index[6]; see line 82
  #Go through those lists and find students to assign them to the school they were matched to
  final_assignment = []
  for sch in schools:
    if sch[5]: # for open priority order
      for std in sch[5]: # for std in open priority order
        if std not in final_assignment:
          final_assignment.append([std[0],sch[0],'O'])
    if sch[6]: # for reserve priority order
      for std2 in sch[6]:
        #for std2 in student_code:
        if std2 not in final_assignment:
          final_assignment.append([std2[0],sch[0],'R'])
    final_assignment.sort()   
  return final_assignment

outcome = DA()
print(outcome)   