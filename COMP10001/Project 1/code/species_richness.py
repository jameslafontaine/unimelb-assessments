def get_species_richness(observed_list=[]):
    ''' Calculates the species richness of a habitat as the number of different
    species observed in a list of independent observations of bird species 
    `observed_list`, and returns the species richness and an alphabetically 
    sorted list of the species that were observed. '''
    
    # list which stores all unique species found in `observed_list`
    unique_list = []
    
    # get all unique bird species from `observed_list` and store them in a list
    for species in observed_list:
        if species not in unique_list:
            unique_list.append(species)
    
    # return species richness and alphabetically sorted list of unique species
    return (len(unique_list), sorted(unique_list))
        
