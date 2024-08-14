def get_species_evenness(observed_list=[]):
    ''' Calculates the species evenness of a list of independent observations of 
    birds `observed_list` as the inverse of Simpson's index and returns the 
    species evenness and a list of tuples containing each bird species and the 
    number of times they were observed, sorted alphabetically by species. '''
    
    # address the case in which the list of species is empty
    if not bool(observed_list):
        return (0, [])
    
    # stores unique species as keys and the number of observations as values
    species_dict = {}
    
    # record the amount of observations of each species
    for species in observed_list:
        if species not in species_dict:
            species_dict[species] = 0
        species_dict[species] += 1
        
    # calculate simpsons index for `observed_list` by summing the squares 
    # of proportions of species, then inverse of this value is species evenness
    simpsons_index = 0
    for observation in species_dict.values():
        simpsons_index += (observation / len(observed_list))**2
    inverse_simpsons_index = 1 / simpsons_index
    
    # return species evenness and list containing each bird species and 
    # the number of times they were observed sorted alphabetically by species
    return (inverse_simpsons_index, sorted(list(species_dict.items())))
    

        
    
    
 
    
    