def optimise_study(sample_data, unseen_species, consecutive_visits):
    ''' Evaluates the effect that consecutive visits and unseen species
    thresholds have on the accuracy of diversity estimates. Takes a list of 
    lists containing species observed on a sampling visit `sample_data` and 
    takes a minimum number of previously `unseen_species` that must be 
    observed before a visit is deemed productive and then the number of 
    unproductive `consecutive_visits` that must occur to trigger the stopping 
    rule. Returns the number of visits that occur before the study has stopped 
    and the proportion of the total bird species observed by the study at that 
    point, compared to if all sampling visits contained in `sample_data` had 
    been conducted. '''
    
    # list that stores unique species found in all visits
    unique_list = []
    
    consecutive_unproductive_visits = 0
    
    total_visits = len(sample_data)
    
    # track if study has stopped early or not
    study_stop = False
    
    # track the number of unseen species found in each visit to determine 
    # if a visit was productive or not
    for visit in range(total_visits): 
        new_species = 0
        for species in sample_data[visit]:
            if species not in unique_list:
                unique_list.append(species)
                new_species += 1
                    
        # determine when the study will stop and track how many visits and
        # unique species there were up until this point
        if new_species < unseen_species and visit > 0:
            consecutive_unproductive_visits += 1
        else: 
            consecutive_unproductive_visits = 0
        if (visit + 1 == total_visits and study_stop is False or 
            consecutive_unproductive_visits == consecutive_visits and 
            study_stop is False):
            visits_before_stop = visit + 1
            study_stop = True
            unique_list_at_stop = unique_list.copy()
    
    # calculate the proportion of total bird species observed at study stop
    # compared to if all visits had been conducted
    species_proportion = len(unique_list_at_stop) / len(unique_list)
 
    return visits_before_stop, species_proportion
    
    
    
    
    
    
    
   
            
   
        
                   
                   
        
        
        