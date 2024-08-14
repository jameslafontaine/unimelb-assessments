# DO NOT DELETE/EDIT THIS LINE OF CODE, AS IT IS USED TO PROVIDE ACCESS TO
# THE FUNCTIONS FROM Q1 AND Q2
from hidden import get_species_richness, get_species_evenness

def compare_diversity(observed_list, diversity_measure):
    ''' Ranks a set of habitats in a list of tuples containing independent
    observations of birds and their habitats `observed_list` by diversity 
    metric `diversity_measure` which will be either richness or evenness. 
    Returns a list of tuples consisting of the habitat name and the diversity 
    of that habitat according to `diversity_measure`. The list is sorted from 
    most diverse to least diverse habitat, and is sorted alphabetically in the 
    case of a tie. '''
    
    # sort tuples into lists based on which habitat the observation was made in
    habitat_dict = {}
    for tup in observed_list:
        if tup[1] not in habitat_dict:
            habitat_dict[tup[1]] = []
        habitat_dict[tup[1]].append(tup[0])
        
    # calculate the diversity of each habitat and add the habitat and 
    # diversity measured to a list `diversity_list` 
    diversity_list = []    
    if diversity_measure == "richness":
        for tup in list(habitat_dict.items()):
            diversity_list.append((tup[0], get_species_richness(tup[1])[0]))
    else:
        for tup in list(habitat_dict.items()):
            diversity_list.append((tup[0], get_species_evenness(tup[1])[0]))
            
    # swap elements in tuples of `diversity_list` to sort from highest to 
    # lowest diversity then alphabetically between habitats with same diversity
    for i in range(len(diversity_list)):
        habitat, diversity = diversity_list[i]
        diversity *= -1
        diversity_list[i] = diversity, habitat
    diversity_list = sorted(diversity_list)
    
    # swap elements in tuples of `diversity_list` back to required format
    for i in range(len(diversity_list)):
        diversity, habitat = diversity_list[i]
        diversity *= -1
        diversity_list[i] = habitat, diversity
  
    return diversity_list
        
        
    

